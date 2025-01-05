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
  mutate(fb.off.y = ifelse(playDirection == 'left', fb.y, 53.3 - fb.y)) %>%
  mutate(gameIdplayId = paste0(gameId, playId))

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
pt_presnap <- lapply(pass_tracking, \(l) l %>% filter(frameType == 'BEFORE_SNAP'))

#summarize by last presnap frame
pt_presnap_lf <- lapply(pt_presnap, \(l) l %>%
  group_by(gameId, playId, nflId) %>% 
  summarise(last.frame = max(frameId)) %>% 
  left_join(., l, join_by(gameId, playId, nflId, last.frame == frameId))) %>% 
  do.call('rbind',.)

#sort out pass coverage players only, no prevent
pt_psd_lf_pc <- pt_presnap_lf %>% 
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

#if disguised==F check for appropriate deepest player
deepest_dis <- pt_psd_lf_pc %>% 
  group_by(gameId, playId) %>% 
  summarise(deepest = max(fb.or.x)) %>% 
  left_join(., pt_psd_lf_pc, join_by(gameId, playId, deepest == fb.or.x), multiple = 'any') %>% 
  left_join(., pass_plays_dis[, c('gameId', 'playId', 'playDisguised')], join_by('gameId', 'playId')) %>% 
  filter(playDisguised == F)

deepest_dis2_nfl <- read_csv('deepest_dis2.csv')

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
  mutate(playDisguised = ifelse(is.na(disguised2), T, disguised2)) %>% 
  select(!c('disguised2'))

#add disguise assignments to all pass tracking data
pass_tracking_dis <- lapply(pass_tracking, \(l) inner_join(l, pass_plays_dis[,c('gameId', 'playId', 'mof', 'highSafeties', 'playDisguised', 'yardsToGo')], join_by('gameId', 'playId')) %>% left_join(., player_play[,c('gameId', 'playId', 'nflId', 'pff_defensiveCoverageAssignment')],join_by('gameId', 'playId', 'nflId')) %>% left_join(.,players[,c('nflId', 'position')], join_by('nflId')))

#defense only
pass_tracking_disd <- lapply(pass_tracking_dis, \(l) l %>% filter(position %in% c('ILB', 'DT', 'CB', 'DE', 'SS', 'NT', 'FS', 'OLB', 'MLB', 'DB', 'LB')))

#check for: PRE players, more than 11 players, assignments which don't match coverage, plays within 10 yards
prevent <- lapply(pass_tracking_disd, \(l) l %>% filter(pff_defensiveCoverageAssignment=='PRE')) #empty

player_counts <- lapply(pass_tracking_disd, \(l) l %>% group_by(gameId, playId)%>% summarize(playercount = n_distinct(nflId))) %>% do.call('rbind',.) #all 11s

football_checks <- lapply(pass_tracking_disd, \(l) l %>% filter(fb.off.x > 100)) #empty

#taken from PFF's definitions of coverages and assignments
mismatch <- lapply(pass_tracking_disd, \(l) l %>% mutate(mismatch = case_when(
  pff_defensiveCoverageAssignment %in% c('2L', '2R') & !(pff_passCoverage %in% c('2-Man', 'Cover-2', 'Cover 6-Left', 'Cover-6 Right')) ~ T,
  pff_defensiveCoverageAssignment %in% c('3M', '3L', '3R') & !(pff_passCoverage %in% c('Cover-3', 'Cover-3 Seam', 'Cover-3 Cloud Left', 'Cover-3 Cloud Right', 'Cover-3 Double Cloud')) ~ T,
  pff_defensiveCoverageAssignment %in% c('4OR', '4OL', '4IL', '4IR') & !(pff_passCoverage %in% c('Quarters', 'Cover-6 Right', 'Cover 6-Left')) ~ T,
  pff_defensiveCoverageAssignment %in% c('FR', 'FL') & !(pff_passCoverage %in% c('Cover-2', '2-Man', 'Cover-3 Cloud Left', 'Cover-3 Cloud Right', 'Cover-3 Double Cloud', 'Cover 6-Left', 'Cover-6 Right')) ~ T,
  pff_defensiveCoverageAssignment %in% c('CFR', 'CFL') & !(pff_passCoverage %in% c('Cover-3', 'Cover-3 Seam', 'Cover-3 Cloud Left', 'Cover-3 Cloud Right', 'Cover-3 Double Cloud', 'Quarters', 'Cover-6 Right', 'Cover 6-Left')) ~ T,
  pff_defensiveCoverageAssignment %in% c('DF') & !(pff_passCoverage %in% c('Cover-1')) ~ T,
  .default = F)) %>% 
  filter(mismatch == T)) %>% 
  do.call('rbind',.) %>% 
  group_by(gameId,playId) #%>% 

pass_plays_dis <- anti_join(pass_plays_dis, mismatch, join_by(gameId, playId))

pass_tracking_dis <- lapply(pass_tracking_dis, \(l) anti_join(l, mismatch, join_by(gameId, playId)))

#pass_tracking_disd <- lapply(pass_tracking_disd, \(l) anti_join(l, mismatch, join_by(gameId, playId)))

#reduce to last frame pre-snap and remove footballs
pt_dis_lfps <- lapply(pass_tracking_dis, \(l) l %>% filter(frameType=='BEFORE_SNAP') %>% group_by(gameId, playId, nflId) %>% summarise(last.frame=max(frameId)) %>% ungroup(.) %>% left_join(., l, join_by(gameId, playId, nflId, last.frame == frameId))) %>% do.call('rbind',.) %>% filter(!is.na(position))

#check for players out of bounds/across LoS w/1-yd buffer
outofbounds_side <- pt_dis_lfps %>% 
  filter(fb.or.y > (53.3 / 2) | fb.or.y < (-53.3 / 2)) %>% 
  group_by(gameId, playId) %>% 
  summarize()

outofbounds_los <- pt_dis_lfps %>% 
  filter(case_when(position %in% c('ILB', 'DT', 'CB', 'DE', 'SS', 'NT', 'FS', 'OLB', 'MLB', 'DB', 'LB') ~ fb.or.x < (-1), position %in% c('G', 'C', 'WR', 'T', 'QB', 'RB', 'TE', 'FB') ~ fb.or.x > 1)) %>% 
  group_by(gameId, playId) %>% 
  summarize()

pass_plays_dis <- anti_join(pass_plays_dis, outofbounds_los, join_by(gameId, playId)) %>% 
  anti_join(., outofbounds_side, join_by(gameId, playId)) %>% 
  mutate(gameIdplayId=paste0(gameId, playId))

#pass_tracking_dis <- lapply(pass_tracking_dis, \(l) anti_join(l, outofbounds_los, join_by(gameId, playId)) %>% anti_join(l, outofbounds_side, join_by(gameId,  playId)))

pt_dis_lfps <- anti_join(pt_dis_lfps, outofbounds_los, join_by(gameId, playId)) %>% 
  anti_join(., outofbounds_side, join_by(gameId, playId)) %>% 
  mutate(gameIdplayId=paste0(gameId, playId))

#assign numbers to clubs and positions
clubs <- as.data.frame(sort(unique(pass_plays_dis$possessionTeam)))
colnames(clubs) <- c('club')
clubs$clubNum <- seq(1, nrow(clubs), by=1)

positions <- as.data.frame(unique(players$position))
colnames(positions) <- c('position')
positions$positionNum <- seq(1, nrow(positions), 1)

#create structure for tensor
pt_for_tensor <- pt_dis_lfps %>% 
  left_join(positions) %>% 
  left_join(clubs) %>% 
  select(c('gameIdplayId', 'nflId', 'clubNum', 's', 'a', 'off.or.o', 'off.or.dir', 'fb.or.x', 'fb.or.y', 'yardsToGo', 'positionNum', 'highSafeties', 'playDisguised')) %>%
  mutate(dir.rad = pi*(off.or.dir / 180), o.rad = pi*(off.or.o / 180)) %>%
  mutate(dir.x = sin(dir.rad), dir.y = cos(dir.rad)) %>%
  mutate(o.x = sin(o.rad), o.y = cos(o.rad)) %>%
  mutate(a.x = dir.x*a, a.y = dir.y*a, s.x = dir.x*s, s.y = dir.y*s) %>%
  select(-c(s, a, off.or.dir, off.or.o, dir.rad, o.rad, dir.x, dir.y)) %>%
  left_join(fb_snap_pos[,c('fb.off.y', 'gameIdplayId')], join_by(gameIdplayId))

pt_for_tensord <- pt_for_tensor %>% 
  filter(positionNum %in% c(5, 6, 7, 8, 10, 11, 12, 15, 16, 18, 19)) %>% 
  mutate(l2g.or.x = fb.or.x - yardsToGo) %>% 
  select(-c('yardsToGo'))

pt_for_tensoro <- pt_for_tensor %>% 
  filter(positionNum %in% c(1, 2, 3, 4, 9, 13, 14, 17)) %>% 
  select(gameIdplayId, fb.or.x, fb.or.y)

pt_for_tensor_do <- left_join(pt_for_tensord, pt_for_tensoro, join_by('gameIdplayId')) %>% 
  mutate(x.dis.o = fb.or.x.x - fb.or.x.y, y.dis.o = fb.or.y.x - fb.or.y.y) %>% 
  select(-c('fb.or.x.y', 'fb.or.y.y')) %>% 
  rename(fb.or.x = fb.or.x.x, fb.or.y = fb.or.y.x)
