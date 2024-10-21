# libs
suppressPackageStartupMessages({
    library(stringr)
    library(textreuse)
    library(reshape2)
    library(dplyr)
})

# consts
congress <- 113
root_path <- getwd()
cache_path <- paste0(root_path, "/data")
cleaned_bill_text_path <- paste0(cache_path, "/cleaned_bill_text/", congress)
output_path <- paste0(root_path, "/pairwise_comparisons/", congress)

# Create output directory if it doesn't exist
if (!dir.exists(output_path)) {
    dir.create(output_path)
}

# utils
path_to_bill_id <- function(path) {
    path_no_suffix <- gsub(".txt", "", path)
    split <- strsplit(path_no_suffix, "-")[[1]][2]
    parts <- str_match(split, "(\\d+)(\\D+)(\\d+)(\\D+)")
    congress_part <- parts[1, 2]
    bill_type_part <- parts[1, 3]
    bill_number_part <- parts[1, 4]
    bill_version_part <- parts[1, 5]
    concat <- paste(congress_part, bill_type_part, bill_number_part, bill_version_part, sep = "-")
    return(toupper(concat))
}

# read in files
paths <- list.files(cleaned_bill_text_path, full.names = TRUE, recursive = TRUE)
bills <- lapply(paths, readLines)

# Create a data frame with the text and bill_id
df <- data.frame(
    text = sapply(bills, paste, collapse = "\n"),
    bill_id = unname(sapply(list.files(cleaned_bill_text_path), path_to_bill_id)),
    stringsAsFactors = FALSE
)

# Create text vector
text_vector <- df$text
names(text_vector) <- df$bill_id

# Sample 100 to pilot approach

# Separate ENR bills from IH and IS bills
enr_bills <- text_vector[grepl("-ENR$", names(text_vector))]
ih_is_bills <- text_vector[grepl("-(IH|IS)$", names(text_vector))]

print(length(enr_bills))
print(length(ih_is_bills))
# Create separate corpora
enr_corpus <- textreuse::TextReuseCorpus(text = enr_bills, tokenizer = textreuse::tokenize_ngrams, n = 5, progress = TRUE)
ih_is_corpus <- textreuse::TextReuseCorpus(text = ih_is_bills, tokenizer = textreuse::tokenize_ngrams, n = 5, progress = TRUE)

warnings()

# Perform pairwise comparisons and save each result as a separate file
for (enr_id in names(enr_corpus)) {
    print(enr_id)
    comparisons <- list()
    for (ih_is_id in names(ih_is_corpus)) {
        # Extract bill type and number
        enr_parts <- strsplit(enr_id, "-")[[1]]
        ih_is_parts <- strsplit(ih_is_id, "-")[[1]]
        #
        # Skip comparison if bill type and number are the same
        if (enr_parts[2] == ih_is_parts[2] && enr_parts[3] == ih_is_parts[3]) {
            next
        }

        # Perform comparison
        comparison <- textreuse::ratio_of_matches(enr_corpus[[enr_id]], ih_is_corpus[[ih_is_id]])
        comparisons[[ih_is_id]] <- comparison
    }

    # Save the comparisons for the current ENR bill
    comparison_file <- paste0(output_path, "/", enr_id, "_comparisons.RData")
    print(paste("Saving comparisons for", enr_id, "to", comparison_file))
    save(comparisons, file = comparison_file)
}
