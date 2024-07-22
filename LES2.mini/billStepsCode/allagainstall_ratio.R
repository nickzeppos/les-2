#####################################################
# Script to pre-process, tokenize into n-grams, and
# calculate similarity for Congressional bills
# This does an all-by-all comparison 
#####################################################
# Mary Kroeger
# 6/28/2019
#####################################################


# Did this to save some space: 
gc(full = TRUE, verbose = TRUE)


# Libraries
library(stringr)
library(textreuse)
library(reshape2)
library(dplyr)

# Set the working directory
setwd("~/Dropbox/LES2/bill_text_clean_117/")

# Get list of files
myFiles <- list.files(path=".", pattern="*")
myFiles <- myFiles[grepl("-S-", myFiles)]

# Load the data 
# Convert into dataframe with all files
combined.data <- lapply(myFiles, readLines)
data <- data.frame(do.call(rbind, combined.data))
data$bill_id <- myFiles
names(data) <- c("text", "bill_id")

# Remove extra files
rm(combined.data, myFiles)

# Create a version variable (e.g., IH, IS, EH, ENR)
data$version <- as.character(lapply(strsplit(as.character(data$bill_id), "\\-"), "[", 4))

# Create a bill ID variable
data$congress <- as.character(lapply(strsplit(as.character(data$bill_id), "\\-"), "[", 1))
data$bill_prefix <- as.character(lapply(strsplit(as.character(data$bill_id), "\\-"), "[", 2))
data$bill_num <- as.character(lapply(strsplit(as.character(data$bill_id), "\\-"), "[", 3))

data$BillID <- paste(data$congress, data$bill_prefix, data$bill_num, sep="-")

# Create a dataset with how many versions each bill has 
# How many versions of the bill
versions <- aggregate(version ~ BillID, data, unique)
versions$numVersions <- as.numeric(lapply(versions$version, length))
names(versions) <- c("BillID", "versionList", "numVersions")

data1 <- merge(data, versions, by="BillID", all=T)

# Limit to those bills with more than one version #NO!
# CHANGED, NO 
data_multiple <- data1

# Order the dataset by section_index and bill_id
#data_multiple_order <- data_multiple[order(data_multiple$bill_id, data_multiple$section_index),]

# Combine the text within bill
#allText <- aggregate(text ~ bill_id, data_multiple_order, function(x) {str_c(x, collapse = " ")})


#########################################
# Clean up text
#########################################

# Split into Congresses

c117 <- data1[(data1$congress==117 & (data1$bill_prefix=="S")),]

rm(data, data_multiple, data1, versions)
#########################################
# 112
docs <- unlist(c117$text)
names(docs) <- c117$bill_id 

#########################################
# Tokenization
#########################################
# Tokenize into 3-grams
n3_corpus_117 <- textreuse::TextReuseCorpus(text=docs, tokenizer = textreuse::tokenize_ngrams, 
                                            n = 3, progress = T)

# Tokenize into 5-grams
#n5_corpus_112 <- textreuse::TextReuseCorpus(text=docs, tokenizer = textreuse::tokenize_ngrams, 
#n = 5, progress = T)

setwd("~/Dropbox/genderBillChange/data1/tokenizedData/")
save(n3_corpus_117, file="n3_corpus_117_S.RData")
#save(n5_corpus_112, file="n5_corpus_112.RData")


#########################################
# DIRECTIONAL
# Calculate all-by-all similarity 
#########################################
start.time <- Sys.time()

n3_comparisons_117_directional_s <- textreuse::pairwise_compare(n3_corpus_117, ratio_of_matches, 
                                                                progress = TRUE, directional = T)

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
# Making 63,258,162 comparisons.
rm(docs, c117, n3_comparisons_117,n3_corpus_117)

candidates <- pairwise_candidates(n3_comparisons_117_directional_s, directional=T)


candidates$billID_1 <- gsub('(.*)-\\w+', '\\1', candidates$a)
candidates$billID_2 <- gsub('(.*)-\\w+', '\\1', candidates$b)
candidates$sameBillDiffVersion <- ifelse(candidates$billID_1==candidates$billID_2, 1, 0)

candidates_n3_117_S <- candidates
save(candidates_n3_117_S, file="~/Dropbox/LES2/similarity_measure/candidates_n3_117_S.RData")

