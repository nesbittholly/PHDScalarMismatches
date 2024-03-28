library(googledrive)

# download --------------------------------------------
options(  gargle_oauth_cache = ".secrets",  gargle_oauth_email = TRUE)
folder_url <- "https://drive.google.com/drive/u/0/folders/1Is6fZLSEZCPdNANy7tthCtveAy7woVL9" #should be copied from browser not "copy link" menu option
folder <- drive_get(as_id(folder_url))
gdrive_files <- drive_ls(folder)
dir.create(here::here("data/original"))

lapply(gdrive_files$id, function(x) drive_download(as_id(x),
                                                   path = paste0(here::here("data/original/"), gdrive_files[gdrive_files$id==x,]$name), overwrite = TRUE))
