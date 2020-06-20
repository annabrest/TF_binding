# To start run:
################
folder_names <- c("raw_data", "output_data", "scripts", "docs", "plots")
sapply(folder_names, dir.create)
rm(folder_names)
# raw_data
# data contains the raw data files used in the project. These files should not be altered and are ideally read-only.
# 
# docs
# docs contains any manuscripts or interim summaries produced with the project.
# 
# plots
# plots contains any plots, images, tables, or figures created and saved by your code. It should be possible to delete and regenerate this folder with the scripts in the project folder.
# 
# output_data
# output contains non-figure objects created by the scripts. For example, processed data or logs.
# 
# scripts
# scripts is a folder for any files you may want to source() in your scripts. 

#----------------
# Then copy pkgs.R into "scripts/" and run to download and update all packages

source("scripts/pkgs.R")