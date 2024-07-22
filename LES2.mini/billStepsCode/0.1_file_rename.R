# Set directory to where the full list of txt bill files
setwd("~/Dropbox/LES2/bill_text_raw/103_1/")


# Rename the files to be preprocessed
a <- list.files(path = ".")  
b <- gsub(".htm.txt", "", a)
file.rename(a, b)
