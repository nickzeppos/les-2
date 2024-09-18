# Replicate Casas, Denny, Wilkerson data processing
# From AJPS replication file: `07-supporting-info-A.R'
# File started: 1/27/2021
# File edited: 1/27/2021

sink(file = "~/Desktop/clean.txt")

# - load the packages
# install.packages('rJava', dependencies = TRUE)
library(dplyr)
library(rJava)
library(quanteda)
library(qdap)

source("~/Dropbox/LES2/CasasDennyWilkersonReplication/hitchhiker_bills_dataverse/code/00-functions.R")

# CONSTANTS & PATHS
# ===============================================================================
# - path to where the raw bills are located
raw_path <- "~/Dropbox/LES2/bill_text_raw/103_1"

# - path where to place the pre-processed bills
clean_path <- "~/Dropbox/LES2/bill_text_clean_103/"

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

# DATA
# ===============================================================================
# - list of bills to be pre-processed (2 example bills in the ./data/bills/raw/
#   directory --> 103-HR-1-IH and 103-HR-2-RH)
# MARY: have to change the format of the text files
bills_list <- list.files(raw_path, recursive = T)
# bills_list <- bills_list[2765:length(bills_list)]
# Get list of bill types

billListData <- data.frame(bills_list)
billListData$bill_version <- as.character(lapply(strsplit(as.character(billListData$bills_list), "-"), "[", 4))
table(billListData$bill_version)


# MAIN
# ===============================================================================
# - iterate through and clean each of the bill versions. We use in here the
#   following functions from the "./code/00-functions.R" file. Check those
#   functions and the Supporting Information A of the article for more details
#   on the pre-processing steps.
counter <- 0
total <- length(bills_list)
for (bill_name in bills_list) {
  # - update counter and report progress
  counter <- counter + 1
  print(paste0("Bill version [", counter, "/", total, "]"))

  # - check the version of the bill. We are not interested in comparing some
  #   bill versions because they are simply procedural copies of preceding
  #   bill version, so we would be overcounting otherwise.
  bill_version <- gsub(".txt", "", strsplit(bill_name, split = "-")[[1]][4])

  # - proceed with the text pre-processing if this is a bill version of interest
  if (bill_version %in%
    c("ENR", "IH", "EH", "RH", "EAS", "RS", "EAH", "ES", "IS", "AS")) {
    # - read the text file for this bill version
    bill_raw_path <- paste(raw_path, bill_name, sep = "/") # Changed this line
    doc <- suppressWarnings(paste(qdap::clean(readLines(bill_raw_path)),
      collapse = "\n"
    ))

    # - we wrap the text pre-processing steps into a tryCatch loop, in case there
    #   is an error when pre-processing a bill version that needs to be debugged.
    out <- tryCatch(
      {
        # - removing amendments/edits to the text. The text files provided by
        #   congress.gov sometimes show delitions made to the text of the bill when
        #   drafting the new bill version (e.g. <DELETED> .... </DELETED>). In this
        #   step we get rid of the <DELETED> marks as well as the text in between
        text <- rm_amendments(doc)

        # - special extra minor pre-processing step for Joint Resolutions
        if (grepl("HJRES", bill_version) | grepl("SJRES", bill_version)) {
          start_i <- as.numeric(regexec("JOINT RESOLUTION", text)) + 17
          text <- substring(text, start_i, nchar(text))
        }

        # - pulling the section titles, the start and end index of each of them,
        #   and whether we want to remove: we remove the Findings, Definitions,
        #   Appropriations, and Table of Content sections. These are procedural
        #   and should not be compared because we are interested in whether
        #   the full substantive text of a given bill is inserted into another
        #   one.
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

        # - removing the procedural sections.
        orig_text <- text
        sec_i_to_remove <- which(sections$to_remove == 1)
        for (i in sec_i_to_remove) {
          start <- sections$start_i[i]
          end <- sections$end_i[i]
          txt_to_rm <- substring(orig_text, start, end)
          text <- gsub(txt_to_rm, "", text, fixed = TRUE)
        }
        remaining_sections <- get_section_titles(text)

        # - remove the titles of the sections: sometimes the full content of a
        #   section is inserted into another bill but the Title changes; that's
        #   why we want to ignore section titles when comparing bill similarity.
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
        path_to <- paste0(clean_path, bill_name)
        write.table(doc_clean2,
          file = path_to,
          row.names = FALSE, col.names = FALSE, quote = FALSE
        )
      },
      error = function(e) "error"
    )
    if (!is.null(out)) {
      if (out == "error") {
        print(paste0("There was an issue with ", bill_name))
      }
    }
  }
}



sink()
