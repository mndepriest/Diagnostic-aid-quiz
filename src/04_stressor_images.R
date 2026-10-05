# Move stressor images ####

# Aug 11 2026
# R 4.5.2

# Move example soybean disease images from Open Crop Manager/Images/stressor_info/Soybean/Disease so they can be used in the disease diagnostic tool



# # Set up #### 
# 
# # # Load packages and environment objects
# # source("src/Setup.R")
# 
# # disease image folder
# dis.fold <- "D:/OCM/abiotic_example_images/abiotic"
# new.fold <- "C:/Users/miran/Desktop/Disease Dashboard/www/diagnostic/soybean"
# 
# img.df <- data.frame(path = list.files(path = dis.fold,
#                                        pattern = ".jpg|.JPG",
#                                        full.names = TRUE,
#                                        recursive = TRUE),
#                      name = list.files(path = dis.fold,
#                                        pattern = ".jpg|.JPG",
#                                        full.names = FALSE,
#                                        recursive = TRUE))
# 
# # Get rid of anything containing thumbnail or 3-6
# img.df <- img.df %>%
#   mutate(name = gsub("\\/", " ", as.character(name))) %>%
#   filter(!grepl("[3-6]", name)) %>%
#   filter(!grepl("thumbnail", name)) %>%
#   mutate(new_path = paste0(new.fold, "/", name))
# 
# 
# for (i in 1:nrow(img.df)){
#   file.copy(from = img.df$path[i],
#             to = img.df$new_path[i])
# }


# str.imgs <- read.csv("C:/Users/miran/Desktop/Disease Dashboard/data/stressor images.csv")
# 
# # Filter for first pics
# str.imgs1 <- str.imgs[grepl("1", str.imgs$name),]
# str.imgs2 <- str.imgs[grepl("2", str.imgs$name),]
# 
# # Recombine
# str.imgs <- rbind(str.imgs1, str.imgs2)
# 
# str.imgs <- str.imgs %>%
#   arrange(folder, name)
# 
# str.imgs$file_name <- paste0(str.imgs$folder, " ", str.imgs$name, ".JPG")
# 
# str.imgs$full_path <- paste0(new.fold, "/", str.imgs$file_name)
# 
# 
# # # List the photos that were moved over 
# # 
# new.fold <- "www/diagnostic/soybean"
# # 
# # imgs <- data.frame(file = list.files(path = new.fold,
# #                                      pattern = ".JPG"))
# 
# str.imgs <- read.csv("C:/Users/miran/Desktop/Disease Dashboard/data/stressor images.csv")
# 
# # Filter for first pics
# str.imgs1 <- str.imgs[grepl("1", str.imgs$name),]
# colnames(str.imgs1) <- c("folder", "file1", "citation1")
# 
# # Filter for second pics
# str.imgs2 <- str.imgs[grepl("2", str.imgs$name),]
# colnames(str.imgs2) <- c("folder", "file2", "citation2")
# 
# str.imgs1 <- left_join(str.imgs1, str.imgs2, by = "folder")
# 
# 
# str.imgs1$file1 <- paste0(str.imgs1$folder, " ", str.imgs1$file1, ".JPG")
# str.imgs1$file2 <- paste0(str.imgs1$folder, " ", str.imgs1$file2, ".JPG")
# 
# write.csv(str.imgs1, "data/stressor images1.csv", row.names = FALSE)
# 
# 
# 
# # Put the underscores in all of the names ##### 
# 
# imgs <- data.frame(old_name = list.files(path = new.fold,
#                                          pattern = ".jpg|.JPG",
#                                          full.names = FALSE))
# 
# imgs <- imgs %>%
#   mutate(old_name = paste0("www/diagnostic/soybean/", old_name)) %>%
#   mutate(new_name = gsub(" ", "_", as.character(old_name)))
# 
# for (i in 1:nrow(imgs)){
#   file.copy(from = imgs$old_name[i],
#             to = imgs$new_name[i])
#   file.remove(imgs$old_name[i])
# }


folder <- "www/soybean"

old <- list.files(folder)

file.rename(
  file.path(folder, old),
  file.path(folder, tolower(old))
)

info$image1 <- tolower(info$image1)
info$image2 <- tolower(info$image2)

write.csv(
  info,
  "data/soybean_information.csv",
  row.names = FALSE
)
