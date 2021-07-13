devtools::load_all()

search_results <- search_results %>% as.data.frame()

search_stats <- search_results %>% 
  dplyr::group_by(id_tile) %>%
  dplyr::summarise(dplyr::across(dplyr::contains("clouds"), list(min = min, mean = mean))) %>%
  dplyr::summarise(clouds_min = min(clouds_min), clouds_mean = mean(clouds_mean))

clouds_stats <- data.frame(
    clouds_threshold = seq(ceiling(search_stats$clouds_min), ceiling(search_stats$clouds_mean), by = 0.1)
  ) %>% 
  dplyr::mutate(
    min_SAFE = sapply(clouds_threshold, function(clouds_threshold){
      search_results %>% 
        dplyr::group_by(id_tile) %>% 
        dplyr::group_map(~ {dplyr::filter(., clouds <= clouds_threshold) %>% nrow()}) %>%
        unlist() %>%
        min()
      }),
    sum_SAFE = sapply(clouds_threshold, function(clouds_threshold){
      search_results %>% 
        dplyr::group_by(id_tile) %>% 
        dplyr::group_map(~ {dplyr::filter(., clouds <= clouds_threshold) %>% nrow()}) %>%
        unlist() %>%
        sum()
      })
  )

sensing_limit <- 30
clouds_stats %>% dplyr::filter(min_SAFE > sensing_limit) %>% head(1)
clouds_threshold <- clouds_stats %>% dplyr::filter(min_SAFE > sensing_limit) %>% head(1) %>% dplyr::pull(clouds_threshold)
usethis::use_data(clouds_threshold, overwrite = TRUE)

selected_search_results <- search_results %>% dplyr::filter(clouds <= clouds_threshold)
usethis::use_data(selected_search_results, overwrite = TRUE)
