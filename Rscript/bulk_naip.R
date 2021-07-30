library(magrittr)
library(RSelenium)
library(rvest)
library(foreach)
library(doFuture)

url <- "https://prd-tnm.s3.amazonaws.com/index.html?prefix=StagedProducts/NAIP/ca_2016/"

rD <- rsDriver(port = 4445L, browser = "firefox")
remDr <- rD[["client"]]
remDr$open()
remDr$navigate(url)
source <- remDr$getPageSource()[[1]] %>%
  read_html() %>%
  html_nodes("a") %>%
  html_attr("href")
source <- source[grepl("ca_2016", source)]

all_urls <- lapply(source, function(url) {
  remDr$navigate(url)
  Sys.sleep(5)
  urls <- remDr$getPageSource()[[1]] %>%
    read_html() %>%
    html_nodes("a") %>%
    html_attr("href")
  urls <- urls[grepl("ca_2016", urls)]
  return(urls)
})

all_urls <- unlist(all_urls)
all_urls <- all_urls[grepl("https://", all_urls)]

naip_download <- data.frame(url = all_urls) %>%
  dplyr::mutate(remote_fname = sapply(url, basename))

out_dir <- "/media/hguillon/UCD_eFlow_BigData1/hguillon/research/data/california-rivers/USGS_data/aerial-photography"
local_fname <- list.files(out_dir, pattern = "jp2")

to_download <- naip_download %>%
  dplyr::filter(!.data$remote_fname %in% local_fname) %>%
  dplyr::mutate(out = file.path(out_dir, remote_fname))

pb_tiles <- progress::progress_bar$new(
  format = "[:bar] :current/:total tiles - :percent in :elapsed/:eta (:elapsedfull) \n",
  total = nrow(to_download),
  show_after = 0
  )
invisible(pb_tiles$tick(0))

res <- foreach(i = seq(nrow(to_download))) %do% {
  curl::curl_download(to_download$url[i], to_download$out[i])
  pb_tiles$tick()
  to_download$out[i]
}