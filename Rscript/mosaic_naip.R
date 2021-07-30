raster_paths <- list.files()



stars::st_mosaic(raster_paths, dst = "NAIP_mosaic_2016.tif")

