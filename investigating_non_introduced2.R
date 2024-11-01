# first script identified urls to fetch, and wrote them in cache/non_introducted_urls.txt
# second script fill fetch and save bill text at url

# libs
library(httr2)
library(jsonlite)
library(stringr)
library(lubridate)
library(dotenv)
suppressPackageStartupMessages({
    library(xml2)
    library(purrr)
    library(dplyr)
    library(rvest)
    library(qdap)
})

# source
source("utils.R")
source("00-functions.R")
# env
dotenv::load_dot_env()

# consts
bill_types <- c("s", "hr")
base_url <- "https://www.govinfo.gov"
root_path <- getwd()
cache_path <- paste0(root_path, "/data")
config_path <- paste0(root_path, "/config.json")
sitemaps_path <- paste0(cache_path, "/govinfo_sitemaps")
url_file_path <- paste0(cache_path, "/non_introduced_urls.txt")

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




# funcs
congresses_in_urls <- function(urls) {
    unique_urls <- urls %>%
        sapply(function(url) {
            parse_sitemap_bill_url(url)$congress
        }) %>%
        unname() %>%
        unique() %>%
        unlist()
    return(unique_urls)
}

init_cleaned_dir <- function(congress) {
    dir_path <- paste0(cache_path, "/cleaned_bill_text/", congress)
    if (!dir.exists(dir_path)) {
        dir.create(dir_path, recursive = TRUE)
    }
}



# main workflows

fetcher <- function(urls_to_fetch) {
    for (i in seq_along(urls_to_fetch)) {
        url <- urls_to_fetch[i]
        parsed_url <- parse_sitemap_bill_url(url)
        # skip all but 117 congress
        int_cong <- as.integer(parsed_url$congress)
        if (int_cong != 117) {
            print(paste("Skipping", parsed_url$congress))
            next
        }

        print(url)


        bill_key <- paste0(parsed_url$congress, parsed_url$bill_type, parsed_url$bill_number, parsed_url$bill_version)
        file_path <- paste0(cache_path, "/bill_text/", parsed_url$congress, "/BILLS-", bill_key, ".txt")
        curl_html2text_safe(url, file_path)
        Sys.sleep(0.01)
        if (i %% 50 == 0) {
            print(paste("Processed", i, "of", length(urls_to_fetch)))
            print(paste("Sleeping for 1s"))
            Sys.sleep(1)
        }
    }
}

clean_urls <- function(urls_to_clean) {
    for (i in seq_along(urls_to_clean)) {
        url <- urls_to_clean[i]

        parsed_url <- parse_sitemap_bill_url(url)

        int_cong <- as.integer(parsed_url$congress)

        if (int_cong != 117) {
            print(paste("Skipping", parsed_url$congress))
            next
        }

        bill_key <- paste0(parsed_url$congress, parsed_url$bill_type, parsed_url$bill_number, parsed_url$bill_version)
        file_path <- paste0(cache_path, "/bill_text/", parsed_url$congress, "/BILLS-", bill_key, ".txt")
        cleaned_file_path <- paste0(cache_path, "/cleaned_bill_text/", parsed_url$congress, "/BILLS-", bill_key, ".txt")
        print(paste("Cleaning", file_path))
        print(paste("Writing to", cleaned_file_path))
        bill_text <- readLines(file_path)
        doc <- paste(qdap::clean(bill_text), collapse = "\n")
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
            file = cleaned_file_path,
            row.names = FALSE, col.names = FALSE, quote = FALSE
        )
    }
}

clean_congresses <- function(congresses) {
    for (congress in congresses) {
        if (congress != 117) {
            print(paste("Skipping", congress))
            next
        }
        raw_bill_dir <- paste0(cache_path, "/bill_text/", congress)
        cleaned_bill_dir <- paste0(cache_path, "/cleaned_bill_text/", congress)

        bill_versions <- c("enr")

        raw_bill_paths <- list.files(raw_bill_dir, full.names = FALSE)

        for (bill_path in raw_bill_paths) {
            bill_key <- path_to_bill_key(bill_path)
            parsed_bill_key <- parse_bill_key(bill_key)

            # skip if bill version not in bill versions
            if (!parsed_bill_key$bill_version %in% bill_versions) {
                print(paste("Skipping", bill_key))
                next
            }

            # If it's a bill version we wnat to clean, proceed

            # build out path
            raw_file_path <- bill_key_to_path(bill_key, "raw")
            cleaned_file_path <- bill_key_to_path(bill_key, "clean")
            print(cleaned_file_path)


            # cleaning procedure
            bill_text <- readLines(raw_file_path)
            doc <- paste(qdap::clean(bill_text), collapse = "\n")
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
                file = cleaned_file_path,
                row.names = FALSE, col.names = FALSE, quote = FALSE
            )
        }
    }
}


comparer <- function(urls, congresses) {
    # For congress
    for (congress in congresses) {
        if (congress != 117) {
            print(paste("Skipping", congress))
            next
        }
        paths <- list.files(paste0(cache_path, "/cleaned_bill_text/", congress), full.names = TRUE, recursive = TRUE)
        bills <- lapply(paths, readLines)
        df <- data.frame(
            text = sapply(bills, paste, collapse = "\n"),
            bill_id = unname(sapply(list.files(paste0(cache_path, "/cleaned_bill_text/", congress)), path_to_bill_id)),
            stringsAsFactors = FALSE
        )
        text_vector <- df$text
        names(text_vector) <- df$bill_id


        non_introduced_ids <- urls %>%
            map(url_to_bill_key) %>%
            map(parse_bill_key) %>%
            keep(function(x) {
                x$congress == congress
            }) %>%
            keep(~ !is.null(.x)) %>%
            map(~ encode_bill_key(.$congress, .$bill_type, .$bill_number, .$bill_version)) %>%
            map(bill_key_to_bill_id) %>%
            list_c()

        # separate enr bills and non-intrdocued bills
        enr_bills <- text_vector[grepl("-ENR$", names(text_vector))]
        non_introduced_bills <- text_vector[non_introduced_ids]

        # generate corpora
        print(paste("ENR bills:", length(enr_bills)))
        print(paste("Non-introduced bills:", length(non_introduced_bills)))
        print("Generating corpora")
        gen_start <- Sys.time()
        enr_corpus <- textreuse::TextReuseCorpus(text = enr_bills, tokenizer = textreuse::tokenize_ngrams, n = 5, progress = TRUE)
        non_introduced_corpus <- textreuse::TextReuseCorpus(text = non_introduced_bills, tokenizer = textreuse::tokenize_ngrams, n = 5, progress = TRUE)
        gen_end <- Sys.time()
        print(paste("Corpora generated in", as.numeric(gen_end - gen_start), "seconds"))
        print("Warnings--- ")
        warnings()
        for (enr_id in names(enr_corpus)) {
            print(paste("Comparing", enr_id))
            comparisons <- list()
            for (non_introduced_id in names(non_introduced_corpus)) {
                enr_parts <- strsplit(enr_id, "-")[[1]]
                non_introduced_parts <- strsplit(non_introduced_id, "-")[[1]]
                # if bill type and bill number are same, skip
                if (enr_parts[2] == non_introduced_parts[2] && enr_parts[3] == non_introduced_parts[3]) {
                    next
                }
                # else, compare
                comparison <- textreuse::ratio_of_matches(enr_corpus[[enr_id]], non_introduced_corpus[[non_introduced_id]])
                comparisons[[non_introduced_id]] <- comparison
            }
            # save comparisons
            output_path <- paste0(root_path, "/pairwise_comparisons/non_introduced/", congress)
            if (!dir.exists(output_path)) {
                dir.create(output_path)
            }
            comparison_file <- paste0(output_path, "/", enr_id, "_comparisons.RData")
            print(paste("Saving comparisons for", enr_id, "to", comparison_file))
            save(comparisons, file = comparison_file)
        }
    }
}

analyze <- function(congresses) {
    for (congress in congresses) {
        if (congress != 117) {
            print(paste("Skipping", congress))
            next
        }
        print(paste("Analyzing congress", congress))

        pwc_path <- paste0(getwd(), "/pairwise_comparisons/non_introduced/", congress)
        fl <- list.files(pwc_path, full.names = TRUE)
        all_comparisons <- list()
        for (f in fl) {
            load(f)
            print(f)
            comparisons_df <- do.call(rbind, lapply(names(comparisons), function(y) {
                data.frame(enr_id = sub("_comparisons.RData", "", basename(f)), ih_is_id = y, value = comparisons[[y]])
            }))
            print(comparisons_df)
            filtered <- comparisons_df %>% filter(value > 0.5)
            all_comparisons[[f]] <- filtered
        }

        combined_comparisons <- do.call(rbind, all_comparisons)
        save(combined_comparisons, file = paste0(pwc_path, "combined_comparisons.RData"))
    }
}

fix_up <- function(congresses) {
    for (congress in congresses) {
        if (congress != 117) {
            print(paste("Skipping", congress))
            next
        }
        pwc_path <- paste0(getwd(), "/pairwise_comparisons/non_introduced/", congress)
        file_path <- paste0(pwc_path, "combined_comparisons.RData")
        load(file_path)
        a <- combined_comparisons[[1]]
        b <- combined_comparisons[[2]]
        value <- combined_comparisons[[3]]

        df <- data.frame(a, b, value, stringsAsFactors = FALSE)

        bill_paths_a <- df$a %>%
            map(bill_id_to_bill_key) %>%
            map(function(x) bill_key_to_path(x, "clean")) %>%
            list_c()
        bill_paths_b <- df$b %>%
            map(bill_id_to_bill_key) %>%
            map(function(x) bill_key_to_path(x, "clean")) %>%
            list_c()

        word_count_a <- bill_paths_a %>%
            map(get_word_count) %>%
            unlist() %>%
            unname()
        word_count_b <- bill_paths_b %>%
            map(get_word_count) %>%
            unlist() %>%
            unname()

        df$word_count_a <- word_count_a
        df$word_count_b <- word_count_b

        df$bill_key_a <- df$a %>%
            map(bill_id_to_bill_key) %>%
            list_c()

        df$bill_key_b <- df$b %>%
            map(bill_id_to_bill_key) %>%
            list_c()


        # Filter out rows where df$bill_key_a == df$bill_key_b
        df <- df[df$bill_key_a != df$bill_key_b, ]

        # df to csv
        write_path <- paste0(pwc_path, "/combined_comparisons.csv")
        write.csv(df, write_path, row.names = FALSE)
    }
}
# main
main <- function() {
    urls <- readLines(url_file_path)
    congresses <- congresses_in_urls(urls)

    congresses %>%
        map(init_cleaned_dir)

    # fetcher(urls)
    clean_urls(urls)
    clean_congresses(congresses)
    comparer(urls, congresses)
    analyze(congresses)
    fix_up(congresses)
}

main()
