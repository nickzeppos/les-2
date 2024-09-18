# libs
library(dplyr)
library(qdap)
suppressPackageStartupMessages({
    library(purrr)
})

# imports
hitchhikers_fn <- new.env()
sys.source("hitchhikers_fn.r", envir = hitchhikers_fn, keep.source = FALSE, keep.parse.data = FALSE)

# utils
apply_gsub <- function(text, tokens, replace = "") {
    sapply(tokens, function(token) {
        text <<- gsub(token, replace, text, fixed = TRUE)
    })

    return(text)
}

# consts
root_path <- getwd()
cache_path <- paste0(root_path, "/data")
bill_text_path <- paste0(cache_path, "/bill_text/")
cleaned_bill_text_path <- paste0(cache_path, "/cleaned_bill_text/")
tokens_bottom <- c(
    "lt gt", " lt ", " gt ", "hr eh", "hr ih", "hr sc",
    "hr enr", "hr rh", "hr rs", "hr eas", "hr es", "hr is",
    "hr as"
)
top_statement <- "be it enacted by the senate and house of representatives of the united states of america in congress assembled"
top_statement_unigrams <- top_statement %>%
    hitchhikers_fn$get_bill_core_text() %>%
    qdap::clean() %>%
    gsub("[0-9]", "", .) %>%
    hitchhikers_fn$get_unigrams(stopw_list = hitchhikers_fn$stopw) %>%
    paste0(collapse = " ")


# refactor of cleaning protocol
clean_file <- function(file_path) {
    text <- file_path %>%
        # read
        readLines() %>%
        # clean
        qdap::clean() %>%
        # collapse
        paste(collapse = "\\n") %>%
        # remove amendments
        hitchhikers_fn$rm_amendments()

    cleaned <-
        # get section titles
        hitchhikers_fn$get_section_titles(text) %>%
        #
        mutate(
            findings = grepl("FINDINGS", title),
            definitions = grepl("DEFINITIONS", title),
            auth = grepl("AUTHORIZATION OF APPROPRIATIONS", title),
            content = grepl("TABLE OF CONTENT", title),
            to_remove = ifelse(
                findings == TRUE |
                    definitions == TRUE |
                    auth == TRUE |
                    content == TRUE, 1, 0
            )
        ) %>%
        # Add two new columns that have appropriate start and end indices according to to_remove
        mutate(
            start = start_i,
            end = ifelse(to_remove == 1, end_i, title_end_i)
        ) %>%
        # Use indices to extract sections of text we want to remove
        mutate(chunk = mapply(function(start, end) substring(text, start, end), start, end)) %>%
        # pull out chunk column
        pull(chunk) %>%
        # remove chunks from text
        apply_gsub(text, .) %>%
        # prepare text again? idk why this is happening, but just recreating
        hitchhikers_fn$get_bill_core_text() %>%
        qdap::clean() %>%
        gsub("[0-9]", "", .) %>%
        hitchhikers_fn$get_unigrams(stopw_list = hitchhikers_fn$stopw) %>%
        paste0(collapse = " ") %>%
        # remove more tokens
        apply_gsub(tokens_bottom) %>%
        apply_gsub(top_statement_unigrams) %>%
        qdap::clean()
    return(cleaned)
}



# main
clean_congress <- function(congress) {
    # get all cache/bill_text/congress/ files
    bill_text_files <- list.files(paste0(bill_text_path, congress))

    # get all cache/cleaned_bill_text/congress/ files
    cleaned_bill_text_files <- list.files(paste0(cleaned_bill_text_path, congress))

    diff_files <- setdiff(bill_text_files, cleaned_bill_text_files)
    print(length((diff_files)))

    for (file in diff_files) {
        print(file)
        file_path <- paste0(bill_text_path, congress, "/", file)
        cleaned <- clean_file(file_path)
        write(cleaned, paste0(cleaned_bill_text_path, congress, "/", file))
        cat(file, "\n", file = paste0(cleaned_bill_text_path, congress, "/log.txt"), append = TRUE)
    }

    # write key to output log
    return("Done")
}
