library(stringr)

# utils
# convert 112-HR-1249-ENR to 112-HR-1249
get_bill_key <- function(bill_id) {
    parts <- strsplit(bill_id, "-")[[1]]
    return(paste(parts[1], parts[2], parts[3], sep = "-"))
}

# Given value from a or b, get the word count
make_path <- function(value) {
    return(paste0("cleaned_bill_text/112/", value, ".txt"))
}
get_word_count <- function(path) {
    text <- readLines(path)
    count <- str_count(text)
    return(count)
}
# Given a string like 112-HR-1249-ENR, return BILLS-112hr1249enr
bill_id_to_path <- function(bill_id) {
    parts <- strsplit(bill_id, "-")[[1]]
    relative <- paste0("BILLS-", tolower(paste(parts[1], parts[2], parts[3], parts[4], sep = "")))
    return(paste0("data/cleaned_bill_text/112/", relative, ".txt"))
}


load("pairwise_comparisons/112combined_comparisons.RData")

a <- combined_comparisons[[1]]
b <- combined_comparisons[[2]]
value <- combined_comparisons[[3]]

df <- data.frame(a, b, value, stringsAsFactors = FALSE)

bill_key_a <- sapply(df$a, get_bill_key)
bill_key_b <- sapply(df$b, get_bill_key)

df$bill_key_a <- bill_key_a
df$bill_key_b <- bill_key_b

word_count_a <- sapply(df$a, bill_id_to_path) %>% sapply(get_word_count)
word_count_b <- sapply(df$b, bill_id_to_path) %>% sapply(get_word_count)

df$word_count_a <- word_count_a
df$word_count_b <- word_count_b

# Filter out rows where df$bill_key_a == df$bill_key_b
df <- df[df$bill_key_a != df$bill_key_b, ]

# df to csv
write.csv(df, "pairwise_comparisons/112/combined_comparisons.csv", row.names = FALSE)
