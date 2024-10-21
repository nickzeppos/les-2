suppressPackageStartupMessages({
    library(dplyr)
    library(rJava)
    library(quanteda)
    library(qdap)
})

# imports
source("00-functions.R")


# consts
congress <- 113
root_path <- getwd()
cache_path <- paste0(root_path, "/data")
bill_text_path <- paste0(cache_path, "/bill_text/")
cleaned_bill_text_path <- paste0(cache_path, "/cleaned_bill_text/")

# - path to where the raw bills are located
raw_path <- paste0(bill_text_path, congress)

# - path where to place the pre-processed bills
clean_path <- paste0(cleaned_bill_text_path, congress)

# print consts
print(paste("Raw path for", congress, ":", raw_path))
print(paste("Clean path for", congress, ":", clean_path))

# - top procedural statement all bills have and that we want to remove and not
#   to take into account when comparing bill versions
top_statement <- "be it enacted by the senate and house of representatives of the united states of america in congress assembled"
top_statement_no_punct <- qdap::clean(get_bill_core_text(top_statement))
top_statement_no_num <- gsub("[0-9]", "", top_statement_no_punct)
top_statement_unigrams <- paste0(
    get_unigrams(top_statement_no_num, stopw_list = stopw),
    collapse = " "
)


# - small procedural tokens to remove. These appear sometimes at the end of the
#   bills and we don't want to take them into account neither when comparing
#   bill versions.
tokens_bottom <- c(
    "lt gt", " lt ", " gt ", "hr eh", "hr ih", "hr sc",
    "hr enr", "hr rh", "hr rs", "hr eas", "hr es", "hr is",
    "hr as"
)


bills_list <- list.files(raw_path, recursive = T, full.names = T)

for (bill in bills_list) {
    # Debugging: Print the bill path
    print(paste("Processing bill:", bill))

    # Check if the file exists and can be read
    if (!file.exists(bill)) {
        print(paste("Error: File does not exist:", bill))
        next
    }

    bill_readLines <- readLines(bill)
    doc <- paste(qdap::clean(bill_readLines), collapse = "\n")

    print(substr(doc, 1, 50))
    out <- tryCatch(
        {
            # - removing amendments/edits to the text. The text files provided by
            #   congress.gov sometimes show delitions made to the text of the bill when
            #   drafting the new bill version (e.g. <DELETED> .... </DELETED>). In this
            #   step we get rid of the <DELETED> marks as well as the text in between
            text <- rm_amendments(doc)


            sections <- get_section_titles(text) %>%
                mutate(
                    findings = grepl("FINDINGS", title),
                    definitions = grepl("DEFINITIONS", title),
                    auth = grepl("AUTHORIZATION OF APPROPRIATIONS", title),
                    content = grepl("TABLE OF CONTENT", title),
                    to_remove = ifelse(findings == TRUE |
                        definitions == TRUE |
                        auth == TRUE |
                        content == TRUE, 1, 0)
                )
            orig_text <- text
            sec_i_to_remove <- which(sections$to_remove == 1)
            for (i in sec_i_to_remove) {
                start <- sections$start_i[i]
                end <- sections$end_i[i]
                txt_to_rm <- substring(orig_text, start, end)
                text <- gsub(txt_to_rm, "", text, fixed = TRUE)
            }
            remaining_sections <- get_section_titles(text)

            orig_text2 <- text
            for (i in 1:nrow(remaining_sections)) {
                start <- remaining_sections$start_i[i]
                end <- remaining_sections$title_end_i[i]
                if (start > 0) {
                    txt_to_rm <- substring(orig_text2, start, end)
                    text <- gsub(txt_to_rm, "", text, fixed = TRUE)
                }
            }
            # - remove the bill's procedural header and tail. We also transform all
            #   text to lower case and remove punctuation, stopwords, and other
            #   common procedural tokens/words
            processed_txt_no_punct <- qdap::clean(get_bill_core_text(text))

            # - remove numbers
            processed_txt_no_num <- gsub("[0-9]", "", processed_txt_no_punct)
            processed_unigrams <- paste0(
                get_unigrams(processed_txt_no_num, stopw_list = stopw),
                collapse = " "
            )
            # - remove the procedural tokens that often appear at the end of a bill
            for (t in tokens_bottom) {
                processed_unigrams <- gsub(t, "", processed_unigrams)
            }
            # - get rid of line break markers if they are still there
            doc_clean1 <- qdap::clean(gsub("\n", " ", processed_unigrams))


            # - get rid of a common procedural statments that often appears at the
            #   top of the bill
            doc_clean2 <- gsub(top_statement_unigrams, "", doc_clean1)


            # - exporting the clean version of the text of this bill version
            cleaned <- doc_clean2


            write.table(
                cleaned,
                file = paste0(clean_path, "/", basename(bill)),
                row.names = FALSE, col.names = FALSE, quote = FALSE
            )
        },
        error = function(e) {
            message(paste("Error with bill:", bill, ":", e$message))
        }
    )
}
