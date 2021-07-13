devtools::load_all()
library(sen2r)

search_results <- s2_list(
  tile = id_tiles, 
  time_interval = c(as.Date("2019-06-01"), as.Date("2020-10-01")),
  server = "gcloud",
  time_period = "full",
  level = "L2A",
  availability = "online"
  )
usethis::use_data(search_results, overwrite = TRUE)