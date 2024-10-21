# ensure all sitemaps are present for congresses

# libs
library(httr2)
library(jsonlite)
library(stringr)

suppressPackageStartupMessages({
    library(xml2)
    library(purrr)
})


# consts
bill_types <- c("s", "hr")
bill_versions <- c("ih", "is", "enr")
congress <- "113"
base_url <- "https://www.govinfo.gov"
root_path <- getwd()
cache_path <- paste0(root_path, "/data")
config_path <- paste0(root_path, "/config.json")
sitemaps_path <- paste0(cache_path, "/govinfo_sitemaps")
bill_text_path <- paste0(cache_path, "/bill_text/", congress)
