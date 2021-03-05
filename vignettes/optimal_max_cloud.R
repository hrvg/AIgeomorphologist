out_dir <- "out"
if(!dir.exists(out_dir)) dir.create(out_dir)

locations <- head(labelled_points)

id_tiles <- s2_list(
	spatial_extent = labelled_points, 
	time_period = "full",
	level = "L2A",
	availability = "online"
	) %>% 
	as.data.frame() %>% 
	dplyr::pull(id_tile) %>%
	unique()
saveRDS(id_tiles, file.path(out_dir, "id_tiles.Rds"))
id_tiles <- readRDS(file.path(out_dir, "id_tiles.Rds"))

search_results <- s2_list(
	tile = id_tiles, 
	time_interval = c(as.Date("2019-06-01"), as.Date("2020-10-01")),
	time_period = "full",
	level = "L2A",
	availability = "online"
	) 
saveRDS(search_results, file.path(out_dir, "search_results.Rds"))
search_results <- readRDS(file.path(out_dir, "search_results.Rds")) %>% as.data.frame()

search_stats <- search_results %>% dplyr::group_by(id_tile) %>%  dplyr::summarise(dplyr::across(dplyr::contains("clouds"), list(min = min, mean = mean))) %>% dplyr::summarise(clouds_min = min(clouds_min), clouds_mean = mean(clouds_mean))

cloud_threshold <- search_stats$clouds_mean

min_cloud_stats <- data.frame(
		cloud_threshold = seq(ceiling(search_stats$clouds_min), ceiling(search_stats$clouds_mean), by = 0.5)
	) %>% 
	dplyr::mutate(
		min_SAFE = sapply(cloud_threshold, function(cloud_threshold){
			search_results %>% 
				dplyr::group_by(id_tile) %>% 
				dplyr::group_map(~ {dplyr::filter(., clouds < cloud_threshold) %>% nrow()}) %>%
				unlist() %>%
				min()
			}),
		sum_SAFE = sapply(cloud_threshold, function(cloud_threshold){
			search_results %>% 
				dplyr::group_by(id_tile) %>% 
				dplyr::group_map(~ {dplyr::filter(., clouds < cloud_threshold) %>% nrow()}) %>%
				unlist() %>%
				sum()
			})
	) %>% 
	reshape2::melt(id.vars = c("cloud_threshold"))


p <- ggplot2::ggplot(min_cloud_stats, ggplot2::aes(x = cloud_threshold, y = value)) +
	ggplot2::geom_point() +
	ggplot2::facet_wrap(. ~ variable, scales = "free") +
	ggpubr::theme_pubclean()
plotly::ggplotly(p)
