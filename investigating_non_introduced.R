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
})

# source
source("utils.R")

# env

# consts
first_congress <- 113
last_congress <- 117
congresses <- seq.int(first_congress, last_congress)

bill_types <- c("hr", "s")
base_url <- "https://www.govinfo.gov"
root_path <- getwd()
cache_path <- paste0(root_path, "/data")
config_path <- paste0(root_path, "/config.json")
sitemaps_path <- paste0(cache_path, "/govinfo_sitemaps")
url_file_path <- paste0(cache_path, "/non_introduced_urls.txt")


# funcs
range_to_vector <- function(bill_range) {
    return(seq.int(bill_range[1], bill_range[2]))
}

year_to_sitemap_path <- function(year) {
    return(paste0(sitemaps_path, "/BILLS_", year, "_sitemap.xml"))
}


# main
# load config
config <- fromJSON(config_path)

# for each congress in congresses
for (congress in congresses) {
    # get the corresponding config object
    congress_config <- config[[toString(congress)]]

    # compute a df of filtered bill urls where congress is congress.
    # columns are congress, bill_type, bill_number, bill_version, url
    bill_urls <-
        # get years from config
        congress_config$years %>%
        # get sitemap paths from years
        map(year_to_sitemap_path) %>%
        # read paths
        map(read_xml) %>%
        # get urls from sitemaps
        map(get_urls_from_sitemap) %>%
        # flatten
        list_c() %>%
        # filter and keep where url is related to congress of interest
        map(., ~ keep(.x, .p = filter_bill_urls(.x, congress))) %>%
        # flatten
        list_c() %>%
        # parse urls into meainingful parts, retain url
        map(parse_sitemap_bill_url) %>%
        # bind rows
        bind_rows()

    # Get unique bill keys of bills that don't have a corresponding introduced version
    related_urls <- bill_urls %>%
        group_by(bill_number, bill_type) %>%
        # filter where there is no introduced version
        filter(
            (bill_type == "hr" & !("ih" %in% bill_version)) |
                (bill_type == "s" & !("is" %in% bill_version))
        ) %>%
        # for each unique congress + bill_type + bill_number, get the first url
        group_by(congress, bill_type, bill_number) %>%
        # slice the first entry of each group
        slice_head() %>%
        # select just urls
        select(url) %>%
        # append /related to each url
        mutate(url = paste0(url, "/related")) %>%
        # lift urls to a vector
        pull()

    print(paste("Found", length(related_urls), "to fetch."))
    average_time <- 0
    # for each related url, fetch the page
    for (index in seq_along(related_urls)) {
        url <- related_urls[index]
        print(url)
        # If index is mutliple of 10, log a start time to compute a running average runtime
        if (index %% 10 == 0) {
            start_time <- Sys.time()
        }


        parsed_url <- parse_sitemap_bill_url(url)

        # Build url, fetch, rest 1 second
        api_url <- build_api_url("bill", congress, parsed_url$bill_type, parsed_url$bill_number, "text")
        json <- fetch_congress_api(api_url)
        Sys.sleep(1.0)

        text_versions <- json$textVersions %>%
            keep(~ !is.null(.x$date)) %>%
            keep(~ length(.x$formats) > 0)

        # Convert dates to POSIXct objects and find the entry with the earliest date
        earliest_entry <- text_versions %>%
            map(~ {
                .x$date <- ymd_hms(.x$date)
                .x
            }) %>%
            {
                .[which.min(map_dbl(., ~ .x$date))]
            }
        earliest_url <- earliest_entry[[1]]$formats[[1]]$url

        # add to end of file
        write(earliest_url, file = url_file_path, append = TRUE)

        # If index is mutliple of 10, log a end time to compute a running average runtime
        if (index %% 10 == 0) {
            end_time <- Sys.time()
            average_time <- (average_time * (index - 10) + as.numeric(end_time - start_time)) / index

            # Print average time, estimated time left, and current index
            print(paste("Average time:", average_time, "Estimated time left:", average_time * (length(related_urls) - index), "Index:", index))
        }
    }
}
