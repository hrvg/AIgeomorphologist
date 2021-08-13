devtools::load_all()
library(progressr)
handlers(global = TRUE)
handlers("progress")
library(tensorflow)
library(keras)

img_dir <- "/media/hguillon/datadrive/deep_learning_images_naip/" # output folder
deep_learning_images <- deep_learning_images_naip %>% na.omit()

labels <- deep_learning_images %>%
  dplyr::pull(.data$label) %>%
  gsub("CA-", "", .) %>%
  as.numeric()

class_names <- labels %>% unique() %>% sort()
class_names <- paste0("CA-", class_names)

nx <- ny <- 384
all_images <- get_deep_learning_images(deep_learning_images, nx, ny)

ind_na <- which(all_images %>% is.na)
labels <- labels[-ind_na]
if (length(ind_na) >= 1) {
  all_images <- rlist::list.remove(all_images, ind_na)
}
input_dim <- dim(all_images[[1]])
all_images <- abind::abind(all_images, along = 0)
dim(all_images) %>% print()

# stratified block holdout
split_ratio <- 0.8
block_cv <- deep_learning_images[-ind_na, ] %>%
  # tibble::rownames_to_column() %>%
  dplyr::mutate(rowname = seq(nrow(.))) %>%
  dplyr::group_by(.data$label) %>%
  dplyr::group_modify(~ {
    to_select <- .x$SiteID %>% unique()
    to_select <- to_select %>% sample(split_ratio * length(to_select))
    .x %>% dplyr::filter(.data$SiteID %in% to_select)
  })
train_ind <- block_cv$rowname %>% as.numeric()
test_ind <- seq_along(labels)[-train_ind]
table(labels[train_ind]) %>% print()

hg_data <- list(
  train = list(
    x = all_images[train_ind, , , ],
    y = labels[train_ind] - 1)
  ,
  test = list(
    x = all_images[test_ind, , , ],
    y = labels[test_ind] - 1)
  )
c(train_images, train_labels) %<-% hg_data$train
c(test_images, test_labels) %<-% hg_data$test

# create the base pre-trained model
datagen <- image_data_generator(
  horizontal_flip = TRUE,
  vertical_flip = TRUE,
  preprocessing_function = mobilenet_preprocess_input
)

batch_size <- 16
datagen %>% fit_image_data_generator(train_images)
training_image_flow <- flow_images_from_data(train_images, train_labels, datagen, batch_size = batch_size)
validation_image_flow <- flow_images_from_data(test_images, test_labels, datagen, batch_size = batch_size)

input_tensor <- layer_input(shape = dim(train_images) %>% tail(3))
base_model <- application_mobilenet(
  # input_shape = dim(train_images) %>% tail(3),
  weights = 'imagenet',
  include_top = FALSE,
  input_tensor = input_tensor,
  pooling = "avg"
  )

# add our custom layers
predictions <- base_model$output %>% 
  # layer_dense(units = 128, activation = 'relu') %>% 
  # layer_dense(units = 64, activation = 'relu') %>% 
  # layer_batch_normalization() %>%
  # layer_dense(units = 32, activation = 'relu') %>% 
  layer_dense(units = 10, activation = 'softmax')

# this is the model we will train
model <- keras_model(inputs = base_model$input, outputs = predictions)

# first: train only the top layers (which were randomly initialized)
# i.e. freeze all convolutional InceptionV3 layers
freeze_weights(base_model)

# Function for decaying the learning rate.
# You can define any decay function you need.
decay <- function(epoch, lr) {
  if (epoch < 30) 1e-2
    else if (epoch >= 30 && epoch < 60) 1e-3
      else 1e-5
}

callbacks <- list(
    callback_progbar_logger(),
    callback_learning_rate_scheduler(decay)
)

# compile the model (should be done *after* setting layers to non-trainable)
model %>% compile(
  optimizer = optimizer_sgd(
    lr = 0.01,
    momentum = 0.9,
    decay = 0.0001
    ), 
  loss = "sparse_categorical_crossentropy",
  metrics = c("accuracy")
)

model %>% summary() %>% print()

n_epochs <- 5000
history <- model %>% fit(
  training_image_flow,
  verbose = 1,
  epochs = n_epochs,
  steps_per_epoch = training_image_flow$n / training_image_flow$batch_size,
  validation_data = validation_image_flow,
  validation_steps = validation_image_flow$n / validation_image_flow$batch_size
)
