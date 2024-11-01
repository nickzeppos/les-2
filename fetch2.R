# Specify congresses, bill types, and bill versions to fetch and cache
# Depends on config.json


# libs
library(httr2)
library(jsonlite)
library(stringr)
suppressPackageStartupMessages({
    library(xml2)
    library(purrr)
})

# source
source("utils.R")


# consts
base_url <- "https://www.govinfo.gov"
root_path <- getwd()
cache_path <- paste0(root_path, "/data")
config_path <- paste0(root_path, "/config.json")
sitemaps_path <- paste0(cache_path, "/govinfo_sitemaps")

# args
bill_types <- c("s", "hr")
bill_versions <- c("enr")
congresses <- c("108", "109", "110", "111", "112", "113", "114", "115", "116", "117")

# load in config
config <- fromJSON(config_path)

# funcs
get_urls_for_congress <- function(congress) {
    congress_config <- config[[congress]]
    years <- congress_config$years
    paths <- years %>%
        sapply(function(year) {
            paste0("BILLS_", year, "_sitemap.xml")
        }) %>%
        sapply(function(suffix) {
            return(paste0(sitemaps_path, "/", suffix))
        }) %>%
        unname() %>%
        unlist()

    urls <- paths %>%
        sapply(function(path) {
            xml <- read_xml(path)
            urls <- get_urls_from_sitemap(xml)
            return(urls)
        }) %>%
        unname() %>%
        unlist()

    filtered_urls <- urls %>%
        sapply(function(url) {
            return(filter_bill_urls(url,
                congresses_to_keep = congresses,
                bill_types_to_keep = bill_types,
                bill_versions_to_keep = bill_versions
            ))
        }) %>%
        unname() %>%
        urls[.]
    return(filtered_urls)
}

init_dir_for_congress <- function(congress) {
    dir_path <- paste0(cache_path, "/bill_text/", congress)
    if (!dir.exists(dir_path)) {
        dir.create(dir_path, recursive = TRUE)
    }
}



# main
main <- function() {
    # init dirs
    congresses %>%
        map(init_dir_for_congress)

    urls <- congresses %>%
        map(get_urls_for_congress) %>%
        unname() %>%
        unlist()

    processed_urls <-
        # on congresses
        congresses %>%
        # for each congress, list .txt files in bill_text_dir
        sapply(function(congress) {
            bill_text_path <- paste0(cache_path, "/bill_text/", congress)
            return(list.files(bill_text_path, pattern = ".txt"))
        }, USE.NAMES = FALSE) %>%
        # convert to url
        sapply(function(file) {
            paste0(base_url, "/app/details/", file)
        }, USE.NAMES = FALSE) %>%
        unlist() %>%
        str_remove(".txt")
    remaining <- setdiff(urls, processed_urls)


    if (length(remaining) == 0) {
        print("No remaining URLs to process")
        return()
    } else {
        print(paste("Remaining URLs to process:", length(remaining)))
    }
    for (i in seq_along(remaining)) {
        details_url <- remaining[i]
        parsed_url <- parse_sitemap_bill_url(details_url)
        bill_key <- paste0(parsed_url$congress, parsed_url$bill_type, parsed_url$bill_number, parsed_url$bill_version)
        url <- paste0(base_url, "/content/pkg/BILLS-", bill_key, "/html/BILLS-", bill_key, ".htm")
        print(url)
        file_path <- paste0(cache_path, "/", "bill_text", "/", parsed_url$congress, "/BILLS-", bill_key, ".txt")
        curl_html2text_safe(url, file_path)
        Sys.sleep(0.01)
        if (i %% 50 == 0) {
            print(paste("Processed", i, "of", length(remaining)))
            print(paste("Sleeping for 1s"))
            Sys.sleep(1)
        }
    }
}

main()
