
# Set directory to where the full list of htm bill files
setwd("~/Dropbox/LES2/bill_text_raw/103_1/")


# Rename the files to be preprocessed
a <- list.files(path = ".")  
b <- toupper(a)
c <- gsub("BILLS-", "", b)
rm(b)
d <- gsub("(\\d+)", "\\1-", c)
e <- gsub("(\\D+)", "\\1-", d)
f <- gsub(".HTM-", ".htm", e)
rm(c,d,e)
file.rename(a, f)

