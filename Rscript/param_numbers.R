devtools::load_all()
library(keras)

apps <- list(
  application_densenet121 = application_densenet121,
  application_densenet169 = application_densenet169,
  application_densenet201 = application_densenet201,
  application_inception_resnet_v2 = application_inception_resnet_v2,
  application_inception_v3 = application_inception_v3,
  application_mobilenet = application_mobilenet,
  application_mobilenet_v2 = application_mobilenet_v2,
  application_nasnetlarge = application_nasnetlarge,
  application_nasnetmobile = application_nasnetmobile,
  application_resnet50 = application_resnet50,
  application_vgg16 = application_vgg16,
  application_vgg19 = application_vgg19,
  application_xception = application_xception
)

foreach(i = seq_along(apps)) %do% {
  fun <- apps[[i]]
  fun_name <- names(apps)[i]
  n <- fun(include_top = FALSE) %>% count_params()
  data.frame(application_name = fun_name, param_count = n)
}







