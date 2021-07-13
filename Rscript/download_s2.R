devtools::load_all()
library(sen2r)

selected_search_results <- selected_search_results %>% as("safelist")

safe_dir <- "/media/hguillon/UCD_eFlow_BigData1/S2_data/SAFE/" # folder to store downloaded SAFE
s2_download(s2_prodlist = selected_search_results, outdir = safe_dir)