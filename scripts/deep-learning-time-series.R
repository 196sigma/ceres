## Reginald Edwards
## 26 June 2018
##
## Time series analysis via deep learning
rm(list=ls())
gc()

# Core Tidyverse
library(tidyverse)
library(glue)
library(forcats)

# Time Series
library(timetk)
library(tidyquant)
library(tibbletime)

# Visualization
library(cowplot)

# Preprocessing
library(recipes)

# Sampling / Accuracy
library(rsample)
library(yardstick) 

# Modeling
library(keras)
library(tfruns)

## Run the first time
#install_keras()

###############################################################################

## sunspot.month is a ts class (not tidy), so we’ll convert to a tidy data set 
## using the tk_tbl() function from timetk. We use this instead of as.tibble() 
## from tibble to automatically preserve the time series index as a zoo yearmon
## index. Last, we’ll convert the zoo index to date using lubridate::as_date() 
## (loaded with tidyquant) and then change to a tbl_time object to make time 
## series operations easier.


sun_spots <- datasets::sunspot.month
sun_spots <- tk_tbl(sun_spots)
sun_spots <- mutate(sun_spots, index = as_date(index))
sun_spots <- as_tbl_time(sun_spots, index = index)

###############################################################################
## EDA
###############################################################################

p1 <- ggplot(data = sun_spots, aes(index, value)) +
  geom_point(color = palette_light()[[1]], alpha = 0.5) +
  theme_tq() +
  labs(title = "From 1749 to 2013 (Full Data Set)")

p2 <- ggplot(data = filter_time(sun_spots, "start" ~ "1800"), 
             aes(index, value)) +
  geom_line(color = palette_light()[[1]], alpha = 0.5) +
  geom_point(color = palette_light()[[1]]) +
  geom_smooth(method = "loess", span = 0.2, se = FALSE) +
  theme_tq() +
  labs(title = "1749 to 1759 (Zoomed In To Show Changes over the Year)",
       caption = "datasets::sunspot.month")

p_title <- ggdraw() + draw_label("Sunspots", size = 18, fontface = "bold", 
  colour = palette_light()[[1]])

plot_grid(p_title, p1, p2, ncol = 1, rel_heights = c(0.1, 1, 1))


###############################################################################
## Time Series Cross-Validation
###############################################################################

## time dependencies on preceding samples must be preserved. 
## create a cross validation sampling plan by offsetting the window 
## used to select sequential sub-samples. 

## rsample package includes facitlities for backtesting on time series. 
## use the rolling_origin() function to create samples designed for time 
## series cross validation.

## The sampling plan we create uses 50 years (initial = 12 x 50 samples)
## for the training set and ten years (assess = 12 x 10) for the testing
## (validation) set. We select a skip span of about twenty years
## (skip = 12 x 20 - 1) to approximately evenly distribute the samples
## into 6 sets that span the entire 265 years of sunspots history.
## Last, we select cumulative = FALSE to allow the origin to shift
## which ensures that models on more recent data are not given an unfair
## advantage (more observations) over those operating on less recent data.
## The tibble return contains the rolling_origin_resamples.

periods_train <- 12*100
periods_test  <- 12*50
skip_span     <- 12*(22 - 1)

rolling_origin_resamples <- rsample::rolling_origin(sun_spots,
  initial = periods_train,
  assess = periods_test,
  cumulative = FALSE,
  skip = skip_span)


###############################################################################
## VISUALIZING THE BACKTESTING STRATEGY
###############################################################################

## We can visualize the resamples with two custom functions. 
## The first, plot_split(), plots one of the resampling splits using ggplot2. 
## Note that an expand_y_axis argument is added to expand the date range to 
## the full sun_spots dataset date range. This will become useful when we 
## visualize all plots together.

# Plotting function for a single split
plot_split <- function(split, expand_y_axis = TRUE, alpha = 1, size = 1, 
                       base_size = 14){
  
  # Manipulate data
  train_tbl <- training(split) %>%
    add_column(key = "training") 
  
  test_tbl  <- testing(split) %>%
    add_column(key = "testing") 
  
  data_manipulated <- bind_rows(train_tbl, test_tbl) %>%
    as_tbl_time(index = index) %>%
    mutate(key = fct_relevel(key, "training", "testing"))
  
  # Collect attributes
  train_time_summary <- train_tbl %>%
    tk_index() %>%
    tk_get_timeseries_summary()
  
  test_time_summary <- test_tbl %>%
    tk_index() %>%
    tk_get_timeseries_summary()
  
  # Visualize
  g <- data_manipulated %>%
    ggplot(aes(x = index, y = value, color = key)) +
    geom_line(size = size, alpha = alpha) +
    theme_tq(base_size = base_size) +
    scale_color_tq() +
    labs(
      title    = glue("Split: {split$id}"),
      subtitle = glue("{train_time_summary$start} to {test_time_summary$end}"),
      y = "", x = ""
    ) +
    theme(legend.position = "none") 
  
  if (expand_y_axis) {
    
    sun_spots_time_summary <- sun_spots %>% 
      tk_index() %>% 
      tk_get_timeseries_summary()
    
    g <- g +
      scale_x_date(limits = c(sun_spots_time_summary$start, 
                              sun_spots_time_summary$end))
  }
  
  return(g)
}


## The plot_split() function takes one split (in this case Slice01), 
## and returns a visual of the sampling strategy. We expand the axis to 
## the range for the full dataset using expand_y_axis = TRUE.
rolling_origin_resamples$splits[[1]] %>%
  plot_split(expand_y_axis = TRUE) +
  theme(legend.position = "bottom")

## The second function, plot_sampling_plan(), scales the plot_split() 
## function to all of the samples using purrr and cowplot.

# Plotting function that scales to all splits 
plot_sampling_plan <- function(sampling_tbl, expand_y_axis = TRUE, ncol = 3, 
  alpha = 1, size = 1, base_size = 14, title = "Sampling Plan"){
  
  # Map plot_split() to sampling_tbl
  sampling_tbl_with_plots <- sampling_tbl %>%
    mutate(gg_plots = map(splits, plot_split, 
                          expand_y_axis = expand_y_axis,
                          alpha = alpha, base_size = base_size))
  
  # Make plots with cowplot
  plot_list <- sampling_tbl_with_plots$gg_plots 
  
  p_temp <- plot_list[[1]] + theme(legend.position = "bottom")
  legend <- get_legend(p_temp)
  
  p_body  <- plot_grid(plotlist = plot_list, ncol = ncol)
  
  p_title <- ggdraw() + 
    draw_label(title, size = 14, fontface = "bold", colour = palette_light()[[1]])
  
  g <- plot_grid(p_title, p_body, legend, ncol = 1, rel_heights = c(0.05, 1, 0.05))
  
  return(g)
  
}


## We can now visualize the entire backtesting strategy with 
## plot_sampling_plan(). We can see how the sampling plan shifts the sampling 
## window with each progressive slice of the train/test splits.
rolling_origin_resamples %>%
  plot_sampling_plan(expand_y_axis = T, ncol = 3, alpha = 1, size = 1, base_size = 10, 
    title = "Backtesting Strategy: Rolling Origin Sampling Plan")


###############################################################################
## The LSTM model
###############################################################################

## To begin, we’ll develop an LSTM model on a single sample from the 
## backtesting strategy, namely, the most recent slice. We’ll then apply the 
## model to all samples to investigate modeling performance.

example_split    <- rolling_origin_resamples$splits[[6]]
example_split_id <- rolling_origin_resamples$id[[6]]

## visualize the split
plot_split(example_split, expand_y_axis = FALSE, size = 0.5) +
  theme(legend.position = "bottom") +
  ggtitle(glue("Split: {example_split_id}"))

## Training and Validation Sets
## To aid hyperparameter tuning, besides the training set we also need a 
## validation set. For example, we will use a callback, 
##callback_early_stopping, that stops training when no significant performance
## is seen on the validation set (what’s considered significant is up to you).
## We will dedicate 2 thirds of the analysis set to training, and 1 third to 
## validation.

df_trn <- analysis(example_split)[1:800, , drop = FALSE]
df_val <- analysis(example_split)[801:1200, , drop = FALSE]
df_tst <- assessment(example_split)

## First, let’s combine the training and testing data sets into a single data 
## set with a column key that specifies where they came from (either 
## “training” or “testing)”. 
## Note that the tbl_time object will need to have 
## the index respecified during the bind_rows() step

df <- bind_rows(df_trn %>% add_column(key = "training"),
  df_val %>% add_column(key = "validation"),
  df_tst %>% add_column(key = "testing")) %>%
  as_tbl_time(index = index)

## PREPROCESSING WITH RECIPES
## The LSTM algorithm will usually work better if the input data has been
## centered and scaled. We can conveniently accomplish this using the 
## recipes package. In addition to step_center and step_scale, we’re using 
## step_sqrt to reduce variance and remov outliers. 
## The actual transformations are executed when we bake the data according 
## to the recipe:

rec_obj <- recipe(value ~ ., df) %>%
  step_sqrt(value) %>%
  step_center(value) %>%
  step_scale(value) %>%
  prep()

df_processed_tbl <- bake(rec_obj, df)

## capture the original center and scale so we can invert the steps after 
## modeling

center_history <- rec_obj$steps[[2]]$means["value"]
scale_history  <- rec_obj$steps[[3]]$sds["value"]
c("center" = center_history, "scale" = scale_history)

## RESHAPING THE DATA
## Input has to be a 3-d array of size num_samples, num_timesteps, 
## num_features:
## -- num_samples is the number of observations in the set. This will get fed
##    to the model in portions of batch_size. 
## -- num_timesteps, is the length of the hidden state 
## -- num_features is the number of predictors we’re using. For univariate 
##    time series, this is 1.

## We want to produce predictions for 12 months so our LSTM should have
## a hidden state length of 12.
## These 12 time steps will then get wired to 12 linear predictor units 
## using a time_distributed() wrapper. 
## That wrapper’s task is to apply the same calculation 
## (i.e., the same weight matrix) to every state input it receives.

## As we’re forecasting several timesteps the target data needs to be 
## 3-dimensional. 
## Dimension 1 is the batch dimension, 
## dimension 2 is the number of timesteps (the forecasted ones)
## dimension 3 is the size of the wrapped layer. 
## The wrapped layer is a layer_dense() of a single unit, as we want exactly 
## one prediction per point in time.

## Create sliding windows of 12 steps of input, followed by 12 steps of 
## output each. 

n_timesteps <- 12
n_predictions <- n_timesteps
batch_size <- 10

# functions used
build_matrix <- function(tseries, overall_timesteps) {
  t(sapply(1:(length(tseries) - overall_timesteps + 1), function(x) 
    tseries[x:(x + overall_timesteps - 1)]))
}

reshape_X_3d <- function(X) {
  dim(X) <- c(dim(X)[1], dim(X)[2], 1)
  X
}

# extract values from data frame
train_vals <- df_processed_tbl %>%
  filter(key == "training") %>%
  select(value) %>%
  pull()
valid_vals <- df_processed_tbl %>%
  filter(key == "validation") %>%
  select(value) %>%
  pull()
test_vals <- df_processed_tbl %>%
  filter(key == "testing") %>%
  select(value) %>%
  pull()


# build the windowed matrices
train_matrix <-
  build_matrix(train_vals, n_timesteps + n_predictions)
valid_matrix <-
  build_matrix(valid_vals, n_timesteps + n_predictions)
test_matrix <- build_matrix(test_vals, n_timesteps + n_predictions)

# separate matrices into training and testing parts
# also, discard last batch if there are fewer than batch_size samples
# (a purely technical requirement)
X_train <- train_matrix[, 1:n_timesteps]
y_train <- train_matrix[, (n_timesteps + 1):(n_timesteps * 2)]
X_train <- X_train[1:(nrow(X_train) %/% batch_size * batch_size), ]
y_train <- y_train[1:(nrow(y_train) %/% batch_size * batch_size), ]

X_valid <- valid_matrix[, 1:n_timesteps]
y_valid <- valid_matrix[, (n_timesteps + 1):(n_timesteps * 2)]
X_valid <- X_valid[1:(nrow(X_valid) %/% batch_size * batch_size), ]
y_valid <- y_valid[1:(nrow(y_valid) %/% batch_size * batch_size), ]

X_test <- test_matrix[, 1:n_timesteps]
y_test <- test_matrix[, (n_timesteps + 1):(n_timesteps * 2)]
X_test <- X_test[1:(nrow(X_test) %/% batch_size * batch_size), ]
y_test <- y_test[1:(nrow(y_test) %/% batch_size * batch_size), ]
# add on the required third axis
X_train <- reshape_X_3d(X_train)
X_valid <- reshape_X_3d(X_valid)
X_test <- reshape_X_3d(X_test)

y_train <- reshape_X_3d(y_train)
y_valid <- reshape_X_3d(y_valid)
y_test <- reshape_X_3d(y_test)

## Building the LSTM Model
FLAGS <- flags(
  # There is a so-called "stateful LSTM" in Keras. 
  ## While LSTM is stateful per se,
  ## this adds a further tweak where the hidden states get initialized with 
  ## values 
  ## from the item at same position in the previous batch.
  ## This is helpful just under specific circumstances, or if you want to 
  ## create an
  ## "infinite stream" of states, in which case you'd use 1 as the batch size.
  flag_boolean("stateful", FALSE),
  
  ## Should we use several layers of LSTM?
  ## Again, just included for completeness, it did not yield any superior 
  ## performance on this task.
  ## This will actually stack exactly one additional layer of LSTM units.
  flag_boolean("stack_layers", FALSE),
  
  ## number of samples fed to the model in one go
  flag_integer("batch_size", 10),
  
  ## size of the hidden state, equals size of predictions
  flag_integer("n_timesteps", 12),
  
  ## how many epochs to train for
  flag_integer("n_epochs", 100),
  
  ## fraction of the units to drop for the linear transformation of the inputs
  flag_numeric("dropout", 0.2),
  
  ## fraction of units to drop for the linear transformation of the recurrent 
  ## state
  flag_numeric("recurrent_dropout", 0.2),
  
  ## loss function: better for this specific case than mean squared error
  flag_string("loss", "logcosh"),
  
  ## optimizer = stochastic gradient descent. 
  ## Seemed to work better than adam or rmsprop
  flag_string("optimizer_type", "sgd"),
  
  ## size of the LSTM layer
  flag_integer("n_units", 128),
  
  ## learning rate
  flag_numeric("lr", 0.003),
  
  ## momentum, an additional parameter to the SGD optimizer
  flag_numeric("momentum", 0.9),
  
  ## parameter to the early stopping callback
  flag_integer("patience", 10))

## the number of predictions we'll make equals the length of the hidden state
n_predictions <- FLAGS$n_timesteps

## how many features = predictors we have
n_features <- 1

## just in case we wanted to try different optimizers, we could add here
optimizer <- switch(FLAGS$optimizer_type, sgd = optimizer_sgd(lr = FLAGS$lr, 
  momentum = FLAGS$momentum))

## callbacks to be passed to the fit() function
## We just use one here: we may stop before n_epochs if the loss on the 
## validation set does not decrease (by a configurable amount, over a 
## configurable time)
callbacks <- list(callback_early_stopping(patience = FLAGS$patience))


###############################################################################
## “long version”, would allow you to test stacking several LSTMs or use a 
## stateful LSTM
###############################################################################

model <- keras_model_sequential()

model %>%
  layer_lstm(
    units = FLAGS$n_units,
    batch_input_shape  = c(FLAGS$batch_size, FLAGS$n_timesteps, n_features),
    dropout = FLAGS$dropout,
    recurrent_dropout = FLAGS$recurrent_dropout,
    return_sequences = TRUE,
    stateful = FLAGS$stateful
  )

if (FLAGS$stack_layers) {
  model %>%
    layer_lstm(
      units            = FLAGS$n_units,
      dropout = FLAGS$dropout,
      recurrent_dropout = FLAGS$recurrent_dropout,
      return_sequences = TRUE,
      stateful = FLAGS$stateful
    )
}
model %>% time_distributed(layer_dense(units = 1))

model %>%
  compile(
    loss = FLAGS$loss,
    optimizer = optimizer,
    metrics = list("mean_squared_error")
  )

if (!FLAGS$stateful) {
  model %>% fit(
    x          = X_train,
    y          = y_train,
    validation_data = list(X_valid, y_valid),
    batch_size = FLAGS$batch_size,
    epochs     = FLAGS$n_epochs,
    callbacks = callbacks
  )
  
} else {
  for (i in 1:FLAGS$n_epochs) {
    model %>% fit(
      x          = X_train,
      y          = y_train,
      validation_data = list(X_valid, y_valid),
      callbacks = callbacks,
      batch_size = FLAGS$batch_size,
      epochs     = 1,
      shuffle    = FALSE
    )
    model %>% reset_states()
  }
}

if (FLAGS$stateful) model %>% reset_states()


## Now let’s step through the simpler, yet better (or equally)
## performing configuration below.

## create the model
model <- keras_model_sequential()

## add layers
## we have just two, the LSTM and the time_distributed 
model %>%
  layer_lstm(
    units = FLAGS$n_units, 
    # the first layer in a model needs to know the shape of the input data
    batch_input_shape  = c(FLAGS$batch_size, FLAGS$n_timesteps, n_features),
    dropout = FLAGS$dropout,
    recurrent_dropout = FLAGS$recurrent_dropout,
    # by default, an LSTM just returns the final state
    return_sequences = TRUE
  ) %>% time_distributed(layer_dense(units = 1))

model %>%
  compile(loss = FLAGS$loss,
    optimizer = optimizer,
    # in addition to the loss, Keras will inform us about current MSE while training
    metrics = list("mean_squared_error"))

history <- model %>% fit(
  x          = X_train,
  y          = y_train,
  validation_data = list(X_valid, y_valid),
  batch_size = FLAGS$batch_size,
  epochs     = FLAGS$n_epochs,
  callbacks = callbacks)

plot(history, metrics = "loss")
