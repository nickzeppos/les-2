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
setwd("~/Dropbox/LES2/bill_text_clean_116/")

# Get list of files
myFiles <- list.files(path=".", pattern="*")
myFiles <- myFiles[grepl("(-HR-|-S-)", myFiles)]

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

data <- data[(data$version=="ENR"|data$version=="IS"|data$version=="IH"),]

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

c116 <- data1[(data1$congress==116 & (data1$bill_prefix=="HR"|data1$bill_prefix=="S")),]

rm(data, data_multiple, data1, versions)
#########################################
docs <- unlist(c116$text)
names(docs) <- c116$bill_id 

#########################################
# Tokenization
#########################################
# Tokenize into 5-grams
n5_corpus_116 <- textreuse::TextReuseCorpus(text=docs, tokenizer = textreuse::tokenize_ngrams, 
                                            n = 5, progress = T)




#########################################
# DIRECTIONAL
# Calculate all-by-all similarity 
#########################################


#########################################
#started 9:32am on 12/15
start.time <- Sys.time()

n5_comparisons_116_directional_hr <- textreuse::pairwise_compare(n5_corpus_116, ratio_of_matches, 
                                                                 progress = TRUE, directional = T)

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

#Time difference of 4.059085 hours

save(n5_comparisons_116_directional_hr, file="~/Dropbox/LES2.mini/similarity/n5_comparisons_116_directional_hr_s.RData")
load("~/Dropbox/LES2.mini/similarity/n5_comparisons_116_directional_hr_s.RData")

#KELSEY CODE
names.data <- rownames(n5_comparisons_116_directional_hr)

step <- apply(as.matrix(names.data), 1, 
              FUN=function(x){strsplit(x, split="-")[[1]][4]})
table(step)


# We only end up comparing ENR -> corresponding introduced version, i.e. r_of_m(enr, ih), r_of_m(enr, is)
by_type <- list('ih' = which(step == "IH"),
                'is' = which(step == "IS"),
                'enr' = which(step == "ENR"))

ih_is <- n5_comparisons_116_directional_hr[by_type$ih, by_type$is]
ih_is_melt <- melt(ih_is)
##
is_ih <- n5_comparisons_116_directional_hr[by_type$is, by_type$ih]
is_ih_melt <- melt(is_ih)

##
enr_ih <- n5_comparisons_116_directional_hr[by_type$enr, by_type$ih]
enr_ih_melt <- melt(enr_ih)

##
enr_is <- n5_comparisons_116_directional_hr[by_type$enr, by_type$is]
enr_is_melt <- melt(enr_is)

candidates <- rbind(ih_is_melt,is_ih_melt,enr_ih_melt,enr_is_melt)
rm(ih_is_melt, ih_is,n5_comparisons_116_directional_hr)
candidatesHigh <- candidates[candidates$value>.5,]

candidatesHigh$billID_1 <- gsub('(.*)-\\w+', '\\1', candidatesHigh$Var1)
candidatesHigh$billID_2 <- gsub('(.*)-\\w+', '\\1', candidatesHigh$Var2)
candidatesHigh$sameBillDiffVersion <- ifelse(candidatesHigh$billID_1==candidatesHigh$billID_2, 1, 0)

candidatesHigh <- candidatesHigh[candidatesHigh$sameBillDiffVersion==0,]

candidatesHigh$type_a <- str_split_fixed(candidatesHigh$Var1, "-",4)[,4]
candidatesHigh$type_b <- str_split_fixed(candidatesHigh$Var2, "-",4)[,4]

candidatesHigh_enr <- candidatesHigh[candidatesHigh$type_a=="ENR",]
hist(candidatesHigh_enr$value, plot=T)

candidates_n5_116_HR_S <- candidatesHigh_enr
save(candidates_n5_116_HR_S, file="~/Dropbox/LES2/similarity_measure/candidates_n5_116_HR_S_combo.RData")
write.csv(candidates_n5_116_HR_S, file="~/Dropbox/LES2/similarity_measure/candidates_n5_116_HR_S_combo.csv")


# Ask A&C if they need original bill Var1 & 2 cols rather than derived key vals
# I don't know what X is, but it can't be too important. Mary is going to look into it and will tell me if it's important.

# | index 
# | Var2 (billKey) 
# | Var1 (billKey) 
# | X (?) 
# | value (similarity score) 
# | billID_1 
# | billID_2 
# | sameBillDiffVersion 
# | type_a (bill version) 
# | type_b (billVersion) 
# | numWords_bill1 
# | numWords_bill2





