#packages
library(tidyverse)
library(hms)

#data
plays <- read_csv('plays.csv')
plays$gameClock <- as_hms(plays$gameClock)
player_play <- read_csv('player_play.csv')
players <- read_csv('players.csv')
tracking <- sapply(c('tracking_week_1.csv', 'tracking_week_2.csv', 'tracking_week_3.csv', 'tracking_week_4.csv', 'tracking_week_5.csv', 'tracking_week_6.csv', 'tracking_week_7.csv', 'tracking_week_8.csv', 'tracking_week_9.csv'), FUN = read_csv, simplify = FALSE)
games <- read_csv('games.csv')

#select plays of interest
pass_plays <- plays %>% 
  filter(!(gameClock < as_hms('02:00:00') & quarter %in% c(2, 4))) %>%
  filter(isDropback == T) %>%
  filter(!(pff_passCoverage %in% c('Bracket', 'Goal Line', 'Red Zone', 'Prevent', 'Miscellaneous', NA)))

pass_player_play <- player_play %>% 
  inner_join(pass_plays[,c('gameId', 'playId', 'passResult', 'pff_passCoverage')], join_by('gameId', 'playId')) 

pass_tracking <- lapply(tracking, \(l) l %>% inner_join(pass_plays[,c('gameId',  'playId', 'passResult', 'pff_passCoverage')], join_by('gameId', 'playId')))

#get football locations
fb_snap_pos <- lapply(pass_tracking, \(x) x %>% 
  filter(is.na(nflId) & frameType == 'SNAP')) %>% 
  do.call('rbind',.) %>% 
  rename(fb.x = x, fb.y = y) %>% 
  mutate(fb.off.x = ifelse(playDirection == 'right', fb.x, 120 - fb.x)) %>%
  mutate(fb.off.y = ifelse(playDirection == 'left', fb.y, 53.3 - fb.y))

#re-orient all players to offense
pass_tracking <- lapply(pass_tracking, \(l) l %>% 
  mutate(off.x = ifelse(playDirection == 'right', x, 120 - x)) %>%
  mutate(off.y = ifelse(playDirection == 'left', y, 53.3 - y)) %>%
  left_join(.,fb_snap_pos[,c('gameId', 'playId', 'fb.off.x', 'fb.off.y')], join_by(gameId, playId)) %>%
  mutate(fb.or.x = off.x - fb.off.x) %>%
  mutate(fb.or.y = off.y - fb.off.y) %>%
  mutate(off.or.dir = ifelse(playDirection == "left", dir + 180, dir)) %>%
  mutate(off.or.dir = ifelse(off.or.dir > 360, off.or.dir - 360, off.or.dir)) %>%
  mutate(off.or.o = ifelse(playDirection == "left", o + 180, o)) %>%
  mutate(off.or.o = ifelse(off.or.o > 360, off.or.o - 360, off.or.o)))

#select only presnap defense players
pass_pp_def <- pass_player_play %>% 
  left_join(players[,c('nflId', 'position')]) %>% 
  filter(position %in% c('ILB', 'DT', 'CB', 'DE', 'SS', 'NT', 'FS', 'OLB', 'MLB', 'DB', 'LB'))

#gather only pre-snap tracking data
pt_presnapd <- lapply(pass_tracking, \(l) l %>% filter(frameType == 'BEFORE_SNAP'))

#summarize by last presnap frame
pt_presnapd_lf <- lapply(pt_presnapd, \(l) l %>%
  group_by(gameId, playId, nflId) %>% 
  summarise(last.frame = max(frameId)) %>% 
  left_join(., l, join_by(gameId, playId, nflId, last.frame == frameId))) %>% 
  do.call('rbind',.)

#sort out pass coverage players only, no prevent
pt_psd_lf_pc <- pt_presnapd_lf %>% 
  left_join(pass_pp_def[,c('gameId', 'playId', 'nflId', 'pff_defensiveCoverageAssignment', 'position')], join_by('gameId', 'playId', 'nflId')) %>% 
  filter(!is.na(pff_defensiveCoverageAssignment)) %>% 
  filter(pff_defensiveCoverageAssignment != 'PRE') %>% 
  filter(fb.or.x > 0) %>% 
  filter(fb.or.y > -27) %>% 
  filter(fb.or.y < 27) %>% 
  left_join(pass_plays[c('gameId', 'playId', 'yardsToGo', 'playAction')], join_by('gameId', 'playId'))

#classify deep players by expectation
pt_psd_lf_pc <- pt_psd_lf_pc %>% 
  filter(fb.off.x < 100) %>% 
  mutate(expectedDeep = case_when(
  (pff_passCoverage %in% c('Cover-1', 'Cover-1 Double') & pff_defensiveCoverageAssignment == 'DF') ~ T,
  (pff_passCoverage %in% c('Cover-2', '2-Man', 'Cover-6 Right') & pff_defensiveCoverageAssignment == '2R') ~ T,
  (pff_passCoverage %in% c('Cover-2', '2-Man', 'Cover 6-Left') & pff_defensiveCoverageAssignment == '2L') ~ T,
  (pff_passCoverage %in% c('Cover-3', 'Cover-3 Seam', 'Cover-3 Cloud Left', 'Cover-3 Cloud Right', 'Cover-3 Double Cloud') & pff_defensiveCoverageAssignment == '3M') ~ T,
  (pff_passCoverage %in% c('Quarters', 'Cover-6 Right') & pff_defensiveCoverageAssignment == '4IL') ~ T,
  (pff_passCoverage %in% c('Quarters', 'Cover 6-Left') & pff_defensiveCoverageAssignment == '4IR') ~ T,
  .default = F)) %>% 
  mutate(isDeep = case_when(
  (yardsToGo <= 10 & fb.or.x > 10) ~ T,
  ((yardsToGo > 10 &yardsToGo <= 15) & fb.or.x > 11) ~ T,
  (yardsToGo > 15 & fb.or.x > 12) ~ T,
  .default = F)) %>% 
  mutate(isDisguised = ifelse(isDeep != expectedDeep, T, F))

#check disguised players for expected behavior
disguised_players <- pt_psd_lf_pc %>% 
  filter(isDisguised == T) %>% 
  mutate(ps.frame = case_when(playAction == F ~ last.frame + 11, playAction == T ~ last.frame + 21))

disguised_players_list <- lapply(pass_tracking, \(x) inner_join(disguised_players, x[,c('gameId', 'playId', 'nflId', 'frameId', 'off.x', 'off.y')], join_by('gameId', 'playId', 'nflId', 'ps.frame' == 'frameId')))
disguised_players_ps <- do.call('rbind', disguised_players_list)

disguised_players_ps <- disguised_players_ps %>% 
  mutate(fb.or.x.ps = off.x.y - fb.off.x) %>% 
  mutate(fb.or.y.ps = off.y.y - fb.off.y) %>% 
  mutate(vert.dis = fb.or.x.ps - fb.or.x) %>% 
  filter(!(isDeep == T & vert.dis >= 0)) %>% 
  filter(!(isDeep == F & vert.dis <= 0))

pt_psd_lf_pc <- left_join(pt_psd_lf_pc, disguised_players_ps[,c('gameId', 'playId', 'nflId', 'isDisguised')], join_by('gameId', 'playId', 'nflId')) %>% 
  mutate(isDisguised = ifelse(is.na(isDisguised.y), F, T)) %>% 
  select(!c('isDisguised.x', 'isDisguised.y'))

#use disguised player values to assign 
disguised_plays <- pt_psd_lf_pc %>% 
  group_by(gameId, playId) %>% 
  summarize(playDisguised = any(isDisguised))

pass_plays_dis <- pass_plays %>% 
  mutate(mof = ifelse(pff_passCoverage %in% c('Cover-6 Right', 'Quarters', 'Cover 6-Left', '2-Man', 'Cover-0', 'Cover-2'), 'MOFO', 'MOFC')) %>% 
  mutate(highSafeties = case_when(
  pff_passCoverage %in% c('Cover-6 Right', 'Quarters', 'Cover 6-Left', '2-Man', 'Cover-2') ~ 2,
  pff_passCoverage == 'Cover-0' ~ 0,
  .default = 1)) %>% 
  inner_join(disguised_plays, join_by('gameId', 'playId'))

#if disguised==F check for appropriate one or two deepest players
deepest_dis <- pt_psd_lf_pc %>% 
  group_by(gameId, playId) %>% 
  summarise(deepest = max(fb.or.x)) %>% 
  left_join(., pt_psd_lf_pc, join_by(gameId, playId, deepest == fb.or.x), multiple = 'any') %>% 
  left_join(., pass_plays_dis[, c('gameId', 'playId', 'playDisguised')], join_by('gameId', 'playId')) %>% 
  filter(playDisguised == F)

#check for disguised plays with all shallow players
deepest_dis2 <- pt_psd_lf_pc %>% 
  group_by(gameId, playId) %>% 
  summarise(deepest = max(fb.or.x)) %>% 
  left_join(., pt_psd_lf_pc, join_by(gameId, playId, deepest == fb.or.x), multiple = 'any') %>% 
  left_join(., pass_plays_dis[,c('gameId', 'playId', 'playDisguised')], join_by('gameId', 'playId')) %>% 
  filter(playDisguised == T)

deepest_dis2_short <- deepest_dis2 %>% 
  filter(expectedDeep == T & isDeep == F & isDisguised == T) %>% 
  mutate(disguised2 = F)

#fix player assignments
deepest_dis <- deepest_dis %>% 
  mutate(disguised2 = case_when(
  pff_passCoverage %in% c('Cover-1', 'Cover-1 Double', 'Cover-3', 'Cover-3 Seam', 'Cover-3 Cloud Right', 'Cover-3 Cloud Left', 'Cover-3 Double Cloud') & pff_defensiveCoverageAssignment %in% c('3M', 'DF') ~ F,
  pff_passCoverage %in% c('2-Man', 'Cover-2') & pff_defensiveCoverageAssignment %in% c('2L', '2R') ~ F,
  pff_passCoverage == 'Cover-6 Right' & pff_defensiveCoverageAssignment %in% c('2R', '4IL') ~ F,
  pff_passCoverage == 'Cover 6-Left' & pff_defensiveCoverageAssignment %in% c('2L', '4IR') ~ F,
  pff_passCoverage == 'Quarters' & pff_defensiveCoverageAssignment %in% c('4IL', '4IR') ~ F,
  .default = T)) %>%
  rbind(., deepest_dis2_short)

pass_plays_dis <- left_join(pass_plays_dis, deepest_dis[,c('gameId', 'playId', 'disguised2')], join_by('gameId', 'playId')) %>% 
  mutate(disguised = ifelse(is.na(disguised2), T, disguised2)) %>% 
  select(!c('disguised2'))

