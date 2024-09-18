# Fetches bill text from govinfo.gov via sitemaps.
# Depends on two local resources:
# (1) ./config.json
# (2) ./data/govinfo_sitemaps/BILLS_index.xml

# libs
library(httr2)
library(jsonlite)
library(stringr)

suppressPackageStartupMessages({
    library(xml2)
    library(purrr)
})
# setwd above src
setwd(dirname(getwd()))


# consts
bill_types <- c("s", "hr")
bill_versions <- c("ih", "is", "enr")
congress <- "117"
base_url <- "https://www.govinfo.gov"
root_path <- getwd()
cache_path <- paste0(root_path, "/data")
config_path <- paste0(root_path, "/config.json")
sitemaps_path <- paste0(cache_path, "/govinfo_sitemaps")
bill_text_path <- paste0(cache_path, "/bill_text/", congress)
log_path <- paste0(bill_text_path, "/log.txt")


# utils
extract_urls <- function(xml_data) {
    ns <- xml_ns(xml_data)
    xml_find_all(xml_data, ".//d1:loc", ns) %>%
        xml_text()
}

fetch_and_save_sitemap <- function(url) {
    req <- request(url)
    res <- req_perform(req)
    xml <- resp_body_xml(res)
    path <- paste0(sitemaps_path, "/", basename(url))
    write_xml(xml, path)
    return(xml)
}

filter_urls <- function(url) {
    split <- strsplit(url, "-")[[1]][2]
    parts <- str_match(split, "(\\d+)(\\D+)(\\d+)(\\D+)")

    congress_part <- parts[1, 2]
    bill_type <- parts[1, 3]
    bill_version_part <- parts[1, 5]

    # if congress == congress and bill_version_part %in% bill_versions, True, else False
    if (congress_part == congress && bill_version_part %in% bill_versions && bill_type %in% bill_types) {
        return(TRUE)
    }
    return(FALSE)
}

curl_html2text <- function(url, path) {
    cmd <- paste("bash -c", shQuote(paste("curl --max-time 10", url, "| html2text >", shQuote(path))))
    system(cmd)
}

curl_html2text_safe <- function(url, path) {
    tryCatch(
        {
            curl_html2text(url, path)
        },
        error = function(e) {
            message(paste("Error with URL:", url, ":", e$message))
        }
    )
}

# get config obj corresponding to congress
config <- readLines(config_path) %>%
    fromJSON() %>%
    .[[congress]]

# make sitemap urls for years in config
site_map_urls <-
    # grab years from config
    config$years %>%
    # filter years we have sitemaps for
    sapply(function(year) {
        path <- paste0(sitemaps_path, "/BILLS_", year, ".xml")
        if (file.exists(path)) {
            return(NULL)
        } else {
            return(year)
        }
    }) %>%
    # filter nulls
    unlist() %>%
    # make urls with remaining years
    sapply(function(year) {
        paste0("BILLS_", year, "_sitemap.xml")
    }) %>%
    sapply(function(suffix) {
        paste0(base_url, "/sitemap/", suffix)
    }) %>%
    # unname vec
    unname()

xmls <- map(site_map_urls, fetch_and_save_sitemap)
all_urls <- map(xmls, extract_urls) %>%
    # list_c is flatten func
    list_c()

filtered_urls <- map(all_urls, filter_urls) %>%
    list_c() %>%
    all_urls[.]

# diff filtered by log
processed <- readLines(log_path)
remaining <- setdiff(filtered_urls, processed)

# fetch and save unprocessed portion
# sleep 0.1s between requests
# sleep 2s every 100 requests
for (i in seq_along(remaining)) {
    r <- remaining[i]
    bill_key <- strsplit(r, "/")[[1]][6]
    url <- paste0(base_url, "/content/pkg/", bill_key, "/html/", bill_key, ".htm")
    file_path <- paste0(bill_text_path, "/", bill_key, ".txt")
    curl_html2text_safe(url, file_path)
    cat(url, "\n", file = log_path, append = TRUE)
    Sys.sleep(0.1)
    if (i %% 100 == 0) {
        Sys.sleep(2)
    }
}
