# Source all required packages and check if update is required

#########
# currentely not working!
#   check later
#########
pkgs <- c('here', 'tidyverse','RColorBrewer','gplots',
          'randomForest', 'broom', 'gridExtra',
          'rtracklayer', 'Biostrings', 'GenomicRanges',
          'ggpubr', 'BSgenome')

check <- sapply(pkgs,require,warn.conflicts = TRUE,character.only = TRUE)

pkgs.missing <- NULL
  
if (any(!check)) {
  pkgs.missing <- pkgs[!check]
  install.packages(pkgs.missing)
  check <- sapply(pkgs,require,warn.conflicts = TRUE,character.only = TRUE)
  pkgs.missing <- pkgs[!check]
  
  if (!requireNamespace("BiocManager", quietly = TRUE)) {
    install.packages("BiocManager")
  }
  BiocManager::install(pkgs.missing)
}

  check <- sapply(pkgs.missing,require,warn.conflicts = TRUE,character.only = TRUE)

message(paste(length(check), "missing packages"))

rm(pkgs, pkgs.missing, check )

# if (!requireNamespace("BiocManager", quietly = TRUE))
#     install.packages("BiocManager")
# 
# BiocManager::install("Biostrings")
# BiocManager::install("GenomicRanges")
# BiocManager::install("rtracklayer")


 
  # check <- sapply(pkgs,require,warn.conflicts = TRUE,character.only = TRUE)
  # if(any(!check)){
  #   pkgs.missing <- pkgs[!check]
  #   install.packages(pkgs.missing)
  #   pkgs.missing <- pkgs[!check]
  # }  
  # 
  # check <- sapply(pkgs.missing,require,warn.conflicts = TRUE,character.only = TRUE)
  # 