#packages
library(tidyverse)
library(torch)

#separate out undisguised plays to train coverage reader
pt_for_tensor_do_und <- pt_for_tensor_do %>% filter(playDisguised == F)
pt_for_tensor_do_dis <- pt_for_tensor_do %>% filter(playDisguised == T)

n_plays <- n_distinct(pt_for_tensor_do_und$gameIdplayId)
n_features <- 5
data_tensor <- torch_empty(n_plays, 1, n_features, 11, 11)
n_class <- 2

play_summary <- pt_for_tensor_do_und %>% select(gameIdplayId, highSafeties) %>%
  unique(.) %>% 
  mutate(i = 1:n_plays)

#create tensor from data
fill_row <- function(row) {
  
  # indices for putting in tensor
  i <- row$i # row
  f <- 1 # frame
  
  # play info for extracting from df
  playid <- row$gameIdplayId
  #frameid <- row$frame_id
  
  play_df <- pt_for_tensor_do_und %>%
    filter(gameIdplayId == playid) %>%
    select(-gameIdplayId)
  
  # how many defense and offense players are there on this play?
  defenders <- 11
  n_offense <- 11
  
  # get rid of non-location columns
  play_df <- play_df %>% select(c(fb.or.x, fb.or.y, l2g.or.x, x.dis.o, y.dis.o))
  
  # where the magic happens
  data_tensor[i,f, , 1:defenders, 1:n_offense] <-
    torch_tensor(t(play_df))$view(c(-1, defenders, n_offense))
}

walk(.x=1:nrow(play_summary), ~ {
  if (.x %% 250 == 0) {
    message(glue::glue("{.x} of {nrow(play_summary)}"))
  }
  fill_row(play_summary %>% dplyr::slice(.x))
})

label_tensor <- torch_tensor(play_summary$highSafeties, dtype = torch_long())
data_tensor <- torch_squeeze(data_tensor)

test_size <- 2645
set.seed(19)

# hold out
test_id <- sample(1:n_plays, size = test_size)

# full training set
train_id <- setdiff(1:n_plays, test_id)
train_data <- data_tensor[train_id, ..]
train_label <- label_tensor[train_id]

# helper thing that is just 1, ..., length train data
all_train_idx <- 1:dim(train_data)[1]

# create folds from the train indices
# stratified by label
folds <- splitTools::create_folds(
  y = as.integer(train_label),
  k = 5,
  type = "stratified",
  invert = TRUE,
  seed = 19
)



augment_data <- function(df,
                         # stuff that will be multiplied by -1 (y.dis.o)
                         flip_indices = c(5),
                         # raw y location
                         subtract_indices = c(2)) {
  
  
  # indices of the elements that need to be flipped
  t <- torch_ones_like(df)
  t[, flip_indices, , ] <- -1
  
  # first fix: multiply by -1 where needed (stuff like speed in Y direction)
  flipped <- df * t
  
  # for flipping Y itself, need to do 160/3 - y
  t <- torch_zeros_like(df)
  t[, subtract_indices, , ] <- 160 / 3
  
  # second fix: flip around y
  flipped[, subtract_indices, , ] <- t[, subtract_indices, , ] - flipped[, subtract_indices, , ]
  
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

train_ds <- tracking_dataset(train_data, train_label)
train_dl <- train_ds %>% dataloader(batch_size = 64, shuffle = TRUE)

net <- nn_module(
  "Net",
  initialize = function() {
    self$conv_block_1 <- nn_sequential(
      nn_conv2d(
        # 1x1 convolution taking in 11 (n_features) channels and outputting 128
        # before: batch * 11 * 11 * 11
        # after: batch * 128 * 11 * 11
        in_channels = n_features,
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
      
      # note: breaks on current kaggle version
      nn_batch_norm1d(256),
      nn_layer_norm(256),
      nn_dropout(p = 0.3),
      
      # n_class is how many distinct labels there are
      nn_linear(256, n_class)
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

accuracies <- torch_zeros(length(folds))
best_epochs <- torch_zeros(length(folds))

epochs <- 50

# start iteration over folds
for (fold in 1:length(folds)) {
  cat(sprintf("\n------------- FOLD %d ---------", fold))
  
  model <- net()
  optimizer <- optim_adam(model$parameters, lr = 0.001)
  scheduler <- lr_step(optimizer, step_size = 1, 0.975)
  
  # extract train and validation sets
  val_i <- folds[[fold]]
  train_i <- all_train_idx[-val_i]
  
  .ds_train <- dataset_subset(train_ds, train_i)
  .ds_val <- dataset_subset(train_ds, val_i)
  
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
    model$train()
    for (b in enumerate(.train_dl)) {
      # augment first
      b_augmented <- augment_data(b$x)
      x <- torch_cat(list(b$x, b_augmented))
      # double the label list
      y <- torch_cat(list(b$y, b$y))
      optimizer$zero_grad()
      loss <- nnf_cross_entropy(model(x), y)
      #loss$requires_grad = T
      loss$backward()
      optimizer$step()
      train_losses <- c(train_losses, loss$item())
    }
    
    # validation step: loop over batches
    model$eval()
    for (b in enumerate(.valid_dl)) {
      output <- model(b$x)
      #print(output)
      
      # augment
      valid_data_augmented <- augment_data(b$x)
      output_augmented <- model(valid_data_augmented)
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
    
    if (mean(valid_accuracies) > as.numeric(accuracies[fold])) {
      message(glue::glue("Fold {fold}: New best at epoch {epoch} ({round(mean(valid_accuracies), 3)}). Saving model"))
      
      torch_save(model, glue::glue("best_model_{fold}.pt"))
      
      # save new best loss
      accuracies[fold] <- mean(valid_accuracies)
      best_epochs[fold] <- epoch
    }
  }
}

play_summary_test <- play_summary[test_id,]

#select test set undisguised plays and combine with disguised plays
pt_for_tensor_test <- pt_for_tensor_do_und %>% 
  filter(gameIdplayId %in% play_summary_test$gameIdplayId) %>% 
  rbind(.,pt_for_tensor_do_dis)

n_plays2 <- n_distinct(pt_for_tensor_test$gameIdplayId)
data_tensor2 <- torch_empty(n_plays2, 1, n_features, 11, 11)

play_summary2 <- pt_for_tensor_test %>% 
  select(gameIdplayId, playDisguised, highSafeties) %>% 
  unique(.) %>% 
  mutate(i = 1:n_plays2)

fill_row2 <- function(row) {
  
  # indices for putting in tensor
  i <- row$i # row
  f <- 1 # frame
  
  # play info for extracting from df
  playid <- row$gameIdplayId
  #frameid <- row$frame_id
  
  play_df <- pt_for_tensor_test %>%
    filter(gameIdplayId == playid) %>%
    select(-gameIdplayId)
  
  # how many defense and offense players are there on this play?
  defenders <- 11
  n_offense <- 11
  
  # get rid of non-location columns
  play_df <- play_df %>% select(c(fb.or.x, fb.or.y, l2g.or.x, x.dis.o, y.dis.o))
  
  # where the magic happens
  data_tensor2[i,f, , 1:defenders, 1:n_offense] <-
    torch_tensor(t(play_df))$view(c(-1, defenders, n_offense))
}

walk(.x=1:nrow(play_summary2), ~ {
  if (.x %% 250 == 0) {
    message(glue::glue("{.x} of {nrow(play_summary2)}"))
  }
  fill_row2(play_summary2 %>% dplyr::slice(.x))
})

label_tensor2 <- torch_tensor(play_summary2$highSafeties, dtype = torch_long())
data_tensor2 <- torch_squeeze(data_tensor2)

# get the labels
labels <- label_tensor2 %>%
  as.matrix() %>%
  as_tibble() %>%
  set_names("label")

# load all the models
models <- map(1:length(folds), ~ {
  torch_load(glue::glue("best_model_{.x}.pt"))
})

# augment test data
test_data_augmented <- augment_data(data_tensor2)

# initialize empty output
output <- torch_zeros(length(folds), dim(data_tensor2)[1], n_class)

# get augmented prediction for each fold
walk(1:length(folds), ~ {
  output[.x, ..] <- (models[[.x]](data_tensor2) + models[[.x]](test_data_augmented)) / 2
})

# average prediction over folds
predictions <- (1 / length(folds)) * torch_sum(output, 1) %>%
  as.matrix()

# join prediction to label
predictions <- predictions %>%
  as_tibble() %>%
  mutate(row = 1:n()) %>%
  transform(prediction = max.col(predictions)) %>%
  bind_cols(labels) %>%
  mutate(correct = ifelse(prediction == label, 1, 0)) %>%
  as_tibble() %>%
  mutate(
    label = as.factor(label),
    prediction = as.factor(prediction)
  )

play_summary3 <- cbind(play_summary2, predictions[,c('correct', 'prediction')]) %>% rename(coverageRead = prediction)

#accuracies
ps3f <- play_summary3 %>% filter(playDisguised == F)
ps3t <- play_summary3 %>% filter(playDisguised == T)

sum(ps3t$correct)/nrow(ps3t)
sum(ps3f$correct)/nrow(ps3f)
