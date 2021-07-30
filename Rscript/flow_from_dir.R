devtools::load_all()
library(progressr)
handlers(global = TRUE)
handlers("progress")
library(keras)

img_dir <- "/media/hguillon/datadrive/deep_learning_images_naip/" # output folder

img_set <- flow_images_from_directory(
  img_dir, 
  generator = image_data_generator(
    validation_split = 0.20
    ),
  save_to_dir = NULL,
  target_size = c(512, 512)
  )

model <- keras_model_sequential() %>% 
  layer_conv_2d(filters = 32, kernel_size = c(3,3), activation = 'relu', 
                input_shape = c(512, 512, 3)) %>% 
  layer_conv_2d(filters = 32, kernel_size = c(3,3), activation = 'relu') %>% 
  layer_max_pooling_2d(pool_size = c(2,2)) %>% 
  layer_dropout(rate = 0.25) %>% 
  layer_conv_2d(filters = 64, kernel_size = c(3,3), activation = 'relu') %>% 
  layer_conv_2d(filters = 64, kernel_size = c(3,3), activation = 'relu') %>% 
  layer_max_pooling_2d(pool_size = c(2,2)) %>% 
  layer_dropout(rate = 0.25)

model %>%
  layer_flatten() %>% 
  layer_dense(units = 256, activation = 'relu') %>% 
  layer_dropout(rate = 0.25) %>% 
  layer_dense(units = 10, activation = 'softmax')

summary(model)

model %>% 
  compile(
  optimizer = "adam",
  loss = "sparse_categorical_crossentropy",
  metrics = "accuracy"
)

history <- model %>% 
  fit(
    img_set,
    epochs = 20,
    verbose = 1
  )