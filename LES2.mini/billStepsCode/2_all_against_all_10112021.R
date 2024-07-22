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


save(n3_pairs_112_directional, file="~/Dropbox/LES2/similarity_measure/n3_pairs_112_directional_s.RData")


#########################################
# Turn into paired comparisons
#########################################

start.time <- Sys.time()
M2 <- n3_comparisons_117 %>%
  melt(.) %>%
  transmute(pair = paste(Var1, Var2, sep = ","),
            value = value)
n3_pairs_112 <- M2[!is.na(M2$value),]

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

n3_pairs_112$bill_id_1 <- as.character(lapply(strsplit(as.character(n3_pairs_112$pair), ","), "[", 1))
n3_pairs_112$bill_id_2 <- as.character(lapply(strsplit(as.character(n3_pairs_112$pair), ","), "[", 2))
n3_pairs_112$billID_1 <- gsub('(.*)-\\w+', '\\1', n3_pairs_112$bill_id_1)
n3_pairs_112$billID_2 <- gsub('(.*)-\\w+', '\\1', n3_pairs_112$bill_id_2)
n3_pairs_112$sameBillDiffVersion <- ifelse(n3_pairs_112$billID_1==n3_pairs_112$billID_2, 1, 0)

#save(n3_pairs_112, file="~/Dropbox/LES2/similarity_measure/n3_ALLpairs_112_10112021.RData")


load("~/Dropbox/LES2/similarity_measure/n3_ALLpairs_112_10112021.RData")
aggregate(value ~ sameBillDiffVersion, data = n3_pairs_112, FUN = mean)

different <- n3_pairs_112[n3_pairs_112$sameBillDiffVersion ==0,]
diffHigh <- different[different$value > .5,]

M2 <- n5_comparisons_112 %>%
  melt(.) %>%
  transmute(pair = paste(Var1, Var2, sep = ","),
            value = value)
n5_pairs_112 <- M2[!is.na(M2$value),]

n5_pairs_112$bill_id_1 <- as.character(lapply(strsplit(as.character(n5_pairs_112$pair), ","), "[", 1))
n5_pairs_112$bill_id_2 <- as.character(lapply(strsplit(as.character(n5_pairs_112$pair), ","), "[", 2))
n5_pairs_112$billID_1 <- gsub('(.*)-\\w+', '\\1', n5_pairs_112$bill_id_1)
n5_pairs_112$billID_2 <- gsub('(.*)-\\w+', '\\1', n5_pairs_112$bill_id_2)


rm(c117, data, data1, n3_corpus_117, docs)

# Limit to only within version comparisons
save(withinVersionSim, file="~/Dropbox/LES2/similarity_measure/withinVersionSim_112.RData")
withinVersionSim <- n5_pairs_112[n5_pairs_112$billID_1==n5_pairs_112$billID_2,]
withinVersionSim <- withinVersionSim[withinVersionSim$bill_id_1!=withinVersionSim$bill_id_2,]

# Plot density
library(ggplot2)

# Change density plot fill colors by groups
pdf("~/Dropbox/LES2/similarity_measure/summary_numberChanges/plot_sim.pdf")
ggplot(withinVersionSim, aes(x=value)) +
  geom_density() + xlab("Similarity Measure, 5-gram") +
  labs(title="Density Plot of Similarity Score\n for Bills with More than 1 version")
dev.off()

withinVersionSim$change_greater <- ifelse(withinVersionSim$value < .9, 1, 0)

a <- aggregate(change_greater ~ billID_1, withinVersionSim, sum)

xtable(table(versions$numVersions))


# EAH Question

# Load the within bill dataset
load("~/Dropbox/LES2/similarity_measure/withinVersionSim_112.RData")

EAH <- withinVersionSim[grepl("EAH", withinVersionSim$pair),]
EAH1 <- withinVersionSim[(withinVersionSim$billID_1 %in% EAH$billID_1),]
EAH1$version1 <- as.character(lapply(strsplit(as.character(EAH1$bill_id_1), "-"), "[", 4))
EAH1$version2 <- as.character(lapply(strsplit(as.character(EAH1$bill_id_2), "-"), "[", 4))

EAH2 <- EAH1[((EAH1$version1=="IH" & EAH1$version2=="EH")|(EAH1$version1=="EH"&EAH1$version2=="IH")),]

########
#########################################
# Turn into paired comparisons
#########################################

M2 <- n3_comparisons_117 %>%
  melt(.) %>%
  transmute(pair = paste(Var1, Var2, sep = ","),
            value = value)
n3_pairs_112 <- M2[!is.na(M2$value),]

n3_pairs_112$bill_id_1 <- as.character(lapply(strsplit(as.character(n3_pairs_112$pair), ","), "[", 1))
n3_pairs_112$bill_id_2 <- as.character(lapply(strsplit(as.character(n3_pairs_112$pair), ","), "[", 2))
n3_pairs_112$billID_1 <- gsub('(.*)-\\w+', '\\1', n3_pairs_112$bill_id_1)
n3_pairs_112$billID_2 <- gsub('(.*)-\\w+', '\\1', n3_pairs_112$bill_id_2)


## Load the amendment data
amendments <- read.csv("~/Dropbox/LES2/amendment_data/Cleaned/amdt_metadata_no_action_112th.csv")
amendments_pass <- amendments[amendments$admt_status=="pass",]
amendments_pass$BillID <- paste(amendments_pass$amended_bill_cong, toupper(amendments_pass$amended_bill_type), amendments_pass$amended_bill_num, sep="-")
amendments_pass$numAmendments <- 1
num_amendments_pass <- aggregate(numAmendments ~ BillID, amendments_pass, sum)

versions1 <- merge(versions, num_amendments_pass, by="BillID", all.x=T)
versions1$numAmendments[is.na(versions1$numAmendments)] <- 0

library(xtable)
versions1$numAmendments1 <- ifelse(versions1$numAmendments < 10, versions1$numAmendments, 10)
xtable(table(versions1$numAmendments1, versions1$numVersions))

odd <- versions1[versions1$numAmendments > 1 & versions1$numVersions==1,]

