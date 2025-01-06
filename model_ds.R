library(tidyverse)
library(torch)
library(pROC)
library(probably)
library(scales)
library(sportyR)
library(gganimate)
library(nflplotR)

play_summary3 <- play_summary3 %>% 
  mutate(playDisguised = as.numeric(playDisguised) + 1)

pt_for_tensor3 <- left_join(pt_for_tensor_test, play_summary3[,c("gameIdplayId", "coverageRead")]) %>% 
  mutate(coverageRead = as.numeric(coverageRead)) %>%
  mutate(playDisguised = as.numeric(playDisguised) + 1)

n_plays3 <- n_distinct(pt_for_tensor3$gameIdplayId)
n_features3 <- 15
data_tensor3 <- torch_empty(n_plays3, 1, n_features3, 11, 11)
n_class3 <- 2

fill_row3 <- function(row) {
  
  # indices for putting in tensor
  i <- row$i # row
  f <- 1 # frame
  
  # play info for extracting from df
  playid <- row$gameIdplayId
  #frameid <- row$frame_id
  
  play_df <- pt_for_tensor3 %>%
    filter(gameIdplayId == playid) %>%
    select(-gameIdplayId)
  
  # how many defense and offense players are there on this play?
  defenders <- 11
  n_offense <- 11
  
  # get rid of columns
  play_df <- play_df %>% select(-c(nflId, playDisguised, highSafeties))
  
  # where the magic happens
  data_tensor3[i,f, , 1:defenders, 1:n_offense] <-
    torch_tensor(t(play_df))$view(c(-1, defenders, n_offense))
}

walk(.x=1:nrow(play_summary3), ~ {
  if (.x %% 250 == 0) {
    message(glue::glue("{.x} of {nrow(play_summary3)}"))
  }
  fill_row3(play_summary3 %>% dplyr::slice(.x))
})

label_tensor3 <- torch_tensor(play_summary3$playDisguised, dtype = torch_long())
data_tensor3 <- torch_squeeze(data_tensor3)

test_size3 <- 1000
set.seed(19)

# hold out
test_id3 <- sample(1:n_plays3, size = test_size3)
test_data3 <- data_tensor3[test_id3, ..]
test_label3 <- label_tensor3[test_id3]

# full training set
train_id3 <- setdiff(1:n_plays3, test_id3)
train_data3 <- data_tensor3[train_id3, ..]
train_label3 <- label_tensor3[train_id3]

# helper thing that is just 1, ..., length train data
all_train_idx3 <- 1:dim(train_data3)[1]

# create folds from the train indices
# stratified by label
folds3 <- splitTools::create_folds(
  y = as.integer(train_label3),
  k = 5,
  type = "stratified",
  invert = TRUE,
  seed = 19
)

augment_data3 <- function(df,
                          # stuff that will be multiplied by -1 (y.dis.o, o.y, a.y, s.y)
                          flip_indices = c(6, 8, 10, 14),
                          # raw y location
                          subtract_indices = c(3, 11)) {
  
  
  # indices of the elements that need to be flipped
  t <- torch_ones_like(df)
  t[, flip_indices[1], , ] <- -1
  t[, flip_indices[2], , ] <- -1
  t[, flip_indices[3], , ] <- -1
  t[, flip_indices[4], , ] <- -1
  # first fix: multiply by -1 where needed (stuff like speed in Y direction)
  flipped <- df * t
  
  # for flipping Y itself, need to do 160/3 - y
  t <- torch_zeros_like(df)
  t[, subtract_indices[1], , ] <- 160 / 3
  t[, subtract_indices[2], , ] <- 160 / 3
  
  # second fix: flip around y
  flipped[, subtract_indices[1], , ] <- t[, subtract_indices[1], , ] - flipped[, subtract_indices[1], , ]
  flipped[, subtract_indices[2], , ] <- t[, subtract_indices[2], , ] - flipped[, subtract_indices[2], , ]
  
  return(flipped)
}

tracking_dataset <- dataset(
  name = "tracking_dataset",
  initialize = function(x_tensor, y_tensor) {
    self$data_x <- x_tensor
    self$data_y <- y_tensor
  },
  .getitem = function(i) {
    list("x" = self$data_x[i, ], "y" = self$data_y[i])
  },
  .length = function() {
    self$data_y$size()[[1]]
  }
)

train_ds3 <- tracking_dataset(train_data3, train_label3)
train_dl3 <- train_ds3 %>% dataloader(batch_size = 64, shuffle = TRUE)

net3 <- nn_module(
  "Net",
  initialize = function() {
    self$conv_block_1 <- nn_sequential(
      nn_conv2d(
        # 1x1 convolution taking in 15 (n_features3) channels and outputting 128
        # before: batch * 15 * 11 * 11
        # after: batch * 128 * 11 * 11
        in_channels = n_features3,
        out_channels = 128,
        kernel_size = 1
      ),
      nn_relu(inplace = TRUE),
      nn_conv2d(
        in_channels = 128,
        out_channels = 160,
        kernel_size = 1
      ),
      nn_relu(inplace = TRUE),
      nn_conv2d(
        in_channels = 160,
        out_channels = 128,
        kernel_size = 1
      ),
      nn_relu(inplace = TRUE),
    )
    
    self$conv_block_2 <- nn_sequential(
      nn_batch_norm1d(128),
      nn_conv1d(
        in_channels = 128,
        out_channels = 160,
        kernel_size = 1
      ),
      nn_relu(inplace = TRUE),
      nn_batch_norm1d(160),
      nn_conv1d(
        in_channels = 160,
        out_channels = 96,
        kernel_size = 1
      ),
      nn_relu(inplace = TRUE),
      nn_batch_norm1d(96),
      nn_conv1d(
        in_channels = 96,
        out_channels = 96,
        kernel_size = 1
      ),
      nn_relu(inplace = TRUE),
      nn_batch_norm1d(96)
    )
    
    self$linear_block <- nn_sequential(
      nn_linear(96, 96),
      nn_relu(inplace = TRUE),
      nn_batch_norm1d(96),
      nn_linear(96, 256),
      nn_relu(inplace = TRUE),
      
      nn_batch_norm1d(256),
      nn_layer_norm(256),
      nn_dropout(p = 0.3),
      
      # n_class is how many distinct labels there are
      nn_linear(256, n_class3)
    )
  },
  forward = function(x) {
    
    # first conv layer
    # outputs batch * 128 * 11 * 11
    x <- self$conv_block_1(x)
    
    # first pool layer: average of mean and max pooling
    # the 11 is number of offensive players
    avg <- nn_avg_pool2d(kernel_size = c(1, 11))(x) %>%
      torch_squeeze(-1)
    max <- nn_max_pool2d(kernel_size = c(1, 11))(x) %>%
      torch_squeeze(-1)
    
    x <- 0.7 * avg + 0.3 * max
    
    # x is now batch * 128 * 11
    
    # second conv layer
    x <- self$conv_block_2(x)
    
    # second pool layer
    avg <- nn_avg_pool1d(kernel_size = 11)(x) %>%
      torch_squeeze(-1)
    max <- nn_max_pool1d(kernel_size = 11)(x) %>%
      torch_squeeze(-1)
    
    x <- 0.7 * avg + 0.3 * max
    
    # x is now batch * 96
    
    x <- self$linear_block(x)
    
    # x is now batch * # labels
    
    x
  }
)


set.seed(19)
torch_manual_seed(19)

accuracies3 <- torch_zeros(length(folds3))
best_epochs3 <- torch_zeros(length(folds3))

epochs <- 50

# start iteration over folds
for (fold in 1:length(folds3)) {
  cat(sprintf("\n------------- FOLD %d ---------", fold))
  
  model3 <- net3()
  optimizer <- optim_adam(model3$parameters, lr = 0.001)
  scheduler <- lr_step(optimizer, step_size = 1, 0.975)
  
  # extract train and validation sets
  val_i <- folds3[[fold]]
  train_i <- all_train_idx3[-val_i]
  
  .ds_train <- dataset_subset(train_ds3, train_i)
  .ds_val <- dataset_subset(train_ds3, val_i)
  
  set.seed(19)
  torch_manual_seed(19)
  
  .train_dl <- .ds_train %>%
    dataloader(batch_size = 64, shuffle = TRUE)
  .valid_dl <- .ds_val %>%
    dataloader(batch_size = 64, shuffle = TRUE)
  
  for (epoch in 1:epochs) {
    train_losses <- c()
    valid_losses <- c()
    valid_accuracies <- c()
    
    # train step: loop over batches
    model3$train()
    for (b in enumerate(.train_dl)) {
      # augment first
      b_augmented <- augment_data3(b$x)
      x <- torch_cat(list(b$x, b_augmented))
      # double the label list
      y <- torch_cat(list(b$y, b$y))
      optimizer$zero_grad()
      loss <- nnf_cross_entropy(model3(x), y)
      #loss$requires_grad = T
      loss$backward()
      optimizer$step()
      train_losses <- c(train_losses, loss$item())
    }
    
    # validation step: loop over batches
    model3$eval()
    for (b in enumerate(.valid_dl)) {
      output <- model3(b$x)
      #print(output)
      
      # augment
      valid_data_augmented <- augment_data3(b$x)
      output_augmented <- model3(valid_data_augmented)
      output <- (output + output_augmented) / 2
      
      valid_losses <- c(valid_losses, nnf_cross_entropy(output, b$y)$item())
      
      pred <- torch_max(output, dim = 2)[[2]]
      correct <- (pred == b$y)$sum()$item()
      valid_accuracies <- c(valid_accuracies, correct / length(b$y))
    }
    
    scheduler$step()
    
    if (epoch %% 10 == 0) {
      cat(sprintf("\nLoss at epoch %d: training: %1.4f, validation: %1.4f // validation accuracy %1.4f", epoch, mean(train_losses), mean(valid_losses), mean(valid_accuracies)))
      
    }
    
    if (mean(valid_accuracies) > as.numeric(accuracies3[fold])) {
      message(glue::glue("Fold {fold}: New best at epoch {epoch} ({round(mean(valid_accuracies), 3)}). Saving model"))
      
      torch_save(model3, glue::glue("best_model3_{fold}.pt"))
      
      # save new best loss
      accuracies3[fold] <- mean(valid_accuracies)
      best_epochs3[fold] <- epoch
    }
  }
}


# get the labels
labels3 <- test_label3 %>%
  as.matrix() %>%
  as_tibble() %>%
  set_names("label")

# load all the models
models3 <- map(1:length(folds3), ~ {
  torch_load(glue::glue("best_model3_{.x}.pt"))
})

# augment test data
test_data_augmented3 <- augment_data3(test_data3)

# initialize empty output
output3 <- torch_zeros(length(folds3), dim(test_data3)[1], n_class3)

# get augmented prediction for each fold
walk(1:length(folds3), ~ {
  output3[.x, ..] <- (models3[[.x]](test_data3) + models3[[.x]](test_data_augmented3)) / 2
})

output3_sm <- nnf_softmax(output3, dim=3)

# average prediction over folds
predictions3 <- (1 / length(folds3)) * torch_sum(output3_sm, 1)  %>%  as.matrix()

# join prediction to label
predictions3 <- predictions3 %>%
  as_tibble() %>%
  mutate(row = 1:n()) %>%
  bind_cols(labels3) %>%
  as_tibble() 

#ROC and AUC
roc3 <- roc(response = predictions3$label, predictor=predictions3$V2)
plot.roc(roc3)
View(coords(roc3, 'local maxima'))

predictions3 <- predictions3 %>%
  mutate(prediction = ifelse(V2 > 0.44198134, 2, 1)) %>%
  mutate(correct = ifelse(prediction == label, 1, 0)) %>%
  as_tibble() 

#accuracies
sum(predictions3$correct)/1000

predf <- predictions3 %>% filter(label == 1)
predt <- predictions3 %>% filter(label == 2)

sum(predf$correct)/nrow(predf)
sum(predt$correct)/nrow(predt)

#calibration
predictions3 %>% cal_plot_breaks(label, V1)

post_model_playsummary <- play_summary3[test_id3, -5] %>% 
  cbind(predictions3[,-c(3, 5)]) %>% 
  left_join(pass_plays_dis[ ,c(6, 15, 49, 54, 3, 7, 8, 22)])

### charts and such for kaggle

#animation
alldata2022092513679 <- pass_tracking_dis$tracking_week_3.csv %>% filter(gameId==2022092513, playId==679)

geom_football(league = 'nfl', rotation = 90, x_trans = 60, ylims = c(-53.3/2, 53.3/2), xlims = c(30, 65)) + 
  geom_hline(yintercept = 44.71, color='navy') + 
  geom_hline(yintercept = 44.71 + 9, color = 'gold') +
  geom_label(data = alldata2022092513679 %>% filter(frameId >= 47 & frameId <= 109), aes(x = fb.or.y, y = off.x, label = position, color = club, fill = club, fontface = 'bold')) +
  scale_color_nfl(type = "secondary") +
  scale_fill_nfl(type = 'primary') +
  transition_time(frameId)

#pre-snap db locations
plotting_defense_pc <- pt_dis_lfps %>%filter(position %in% c('CB','SS','FS','DB'))

#disguise rates by team
disguiseratesmof <- as.data.frame((prop.table(table(pass_plays_dis$defensiveTeam, pass_plays_dis$highSafeties, pass_plays_dis$playDisguised), margin = 1))) %>%
  filter(Var2 != 0, Var3 == T)

#mean and median epa by coverage type
pass_plays_dis_cg <- pass_plays_dis %>% 
  mutate(covgroup = case_when(
  pff_passCoverage %in% c('Cover-1', 'Cover-1 Double') ~ 'Cover-1',
  pff_passCoverage %in% c('Cover-3', 'Cover-3 Seam', 'Cover-3 Cloud Right', 'Cover-3 Cloud Left', 'Cover-3 Double Cloud') ~ 'Cover-3',
  pff_passCoverage %in% c('Cover-6 Right', 'Cover 6-Left') ~ 'Cover-6',
  .default = pff_passCoverage
))

undis_epa_cov <- pass_plays_dis_cg %>% 
  filter(playDisguised == F) %>% 
  group_by(covgroup) %>% 
  summarise(mean(expectedPointsAdded), median(expectedPointsAdded)) %>%
  rename('mean.undis.epa' = 'mean(expectedPointsAdded)', 'med.undis.epa' = 'median(expectedPointsAdded)')

dis_epa_cov <- pass_plays_dis_cg %>% filter(playDisguised == T) %>% 
  group_by(covgroup) %>% 
  summarise(mean(expectedPointsAdded), median(expectedPointsAdded)) %>% 
  rename('mean.dis.epa' = 'mean(expectedPointsAdded)', 'med.dis.epa' = 'median(expectedPointsAdded)')

epaplotcov <- left_join(undis_epa_cov, dis_epa_cov, join_by(covgroup)) %>% 
  mutate(delta.mean = mean.undis.epa - mean.dis.epa, delta.med = med.undis.epa - med.dis.epa)

#mean ds for all cov types
postmodel_ps_cg <- post_model_playsummary %>% 
  mutate(covgroup = case_when(
  pff_passCoverage %in% c('Cover-1', 'Cover-1 Double') ~ 'Cover-1',
  pff_passCoverage %in% c('Cover-3', 'Cover-3 Seam', 'Cover-3 Cloud Right', 'Cover-3 Cloud Left', 'Cover-3 Double Cloud') ~ 'Cover-3',
  pff_passCoverage %in% c('Cover-6 Right', 'Cover 6-Left') ~ 'Cover-6',
  .default = pff_passCoverage
))

cov_v2_dis <- postmodel_ps_cg %>% 
  filter(playDisguised == 2) %>% 
  group_by(covgroup) %>% 
  summarise(mean(V2)) %>%
  rename('mean.ds.dis' = 'mean(V2)')

cov_v2_undis <- postmodel_ps_cg %>% 
  filter(playDisguised == 1) %>% 
  group_by(covgroup) %>%
  summarise(mean(V2)) %>%
  rename('mean.ds.und' = 'mean(V2)')

covplotv2 <- full_join(cov_v2_dis, cov_v2_undis, join_by(covgroup)) %>% 
  mutate(delta.mean = mean.ds.dis - mean.ds.und)

perc <- label_percent(accuracy = 0.1)

covgrouptable <- covplotv2 %>% 
  mutate(mean.ds.dis.p = perc(mean.ds.dis), mean.ds.und.p = perc(mean.ds.und), Difference = round(delta.mean * 100, digits = 1)) %>% 
  select(covgroup, mean.ds.dis.p, mean.ds.und.p, Difference) %>% 
  rename('Coverage Type'='covgroup', 'Mean Disguise Score, Disguised'=mean.ds.dis.p, 'Mean Disguise Score, Undisguised'='mean.ds.und.p') %>% 
  arrange(desc(Difference))
