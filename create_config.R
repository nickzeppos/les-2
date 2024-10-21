# Creates or modifies config.json to contain necessary information for env vars FIRST_CONGRESS and CURRENT_CONGRESS

# libs
library(jsonlite)
library(dotenv)
library(httr2)
suppressPackageStartupMessages({
    library(xml2)
    library(purrr)
})

# source
source("utils.R")

# env
dotenv::load_dot_env(file = ".env")
first_congress <- as.integer(Sys.getenv("FIRST_CONGRESS"))
current_congress <- as.integer(Sys.getenv("CURRENT_CONGRESS"))

# consts
govinfo_base_url <- "https://www.govinfo.gov"
root_path <- getwd()
cache_path <- paste0(root_path, "/data")
config_path <- paste0(root_path, "/config.json")
sitemaps_path <- paste0(cache_path, "/govinfo_sitemaps")
CONGRESSES <- seq.int(first_congress, current_congress)

# funcs
ensure_sitemaps <- function() {
    # ensure index file
    sitemap_index_path <- paste0(sitemaps_path, "/BILLS_index.xml")
    if (!file.exists(sitemap_index_path)) {
        print("No sitemap index file found. Fetching...")
        url <- paste0(govinfo_base_url, "/sitemap/BILLS_index.xml")
        res <- fetch(url)
        xml <- resp_body_xml(res)
        write_xml(xml, sitemap_index_path)
    }

    sitemap_index_xml <- read_xml(sitemap_index_path)
    sitemap_urls <- extract_text_from_xml(sitemap_index_xml, ".//d1:loc")

    # empty to accumulate because I'm using this function to return as well
    sitemap_file_paths <- list()
    # ensure all sitemaps
    for (url in sitemap_urls) {
        sitemap_path <- paste0(sitemaps_path, "/", basename(url))
        if (!file.exists(sitemap_path)) {
            print(paste("No sitemap file found for", basename(url), ". Fetching..."))
            res <- fetch(url)
            xml <- resp_body_xml(res)
            write_xml(xml, sitemap_path)
        }
        # accumulate
        sitemap_file_paths <- c(unlist(sitemap_file_paths), sitemap_path)
    }
    return(sitemap_file_paths)
}

initialize_config_json <- function(congresses) {
    # empty json
    json <- list()

    for (congress in congresses) {
        json[[as.character(congress)]] <- list(
            congress = unbox(congress),
            hr = list(),
            hr_count = unbox(0),
            s = list(),
            s_count = unbox(0),
            hjres = list(),
            hjres_count = unbox(0),
            sjres = list(),
            sjres_count = unbox(0),
            hconres = list(),
            hconres_count = unbox(0),
            sconres = list(),
            sconres_count = unbox(0),
            years = list()
        )
    }
    return(json)
}

filter_url <- function(url) {
    parsed_url <- parse_sitemap_bill_url(url)
    congress <- parsed_url$congress

    if (congress %in% CONGRESSES) {
        return(TRUE)
    }
    return(FALSE)
}


# sitemaps
print("Ensuring sitemaps...")
sitemap_file_paths <- ensure_sitemaps()

# config
print("Creating config...")
json <- initialize_config_json(CONGRESSES)
for (sitemap_file_path in sitemap_file_paths) {
    # print filename
    print(basename(sitemap_file_path))

    # read file
    sitemap_xml <- read_xml(sitemap_file_path)

    # get year from file name
    year <- as.integer(str_match(basename(sitemap_file_path), "(\\d+)")[1, 2])

    # get urls
    all_urls <- extract_text_from_xml(sitemap_xml, ".//d1:loc")

    # filter urls to include only those we care about (i.e., those which have congress in congresses)
    filtered_urls <- map(all_urls, filter_url) %>%
        list_c() %>%
        all_urls[.]



    # for url in url
    for (url in filtered_urls) {
        parsed_url <- parse_sitemap_bill_url(url)
        congress <- parsed_url$congress
        bill_type <- parsed_url$bill_type
        bill_number <- parsed_url$bill_number

        # TODO: Filter urls up front so this isn't necessary.
        # if congress is not in config, warn and skip
        # if (!(as.character(congress) %in% names(json))) {
        #     next
        # }

        # print before update
        print(url)

        # update config
        # Add year if we haven't yet
        if (!(year %in% json[[as.character(congress)]]$years)) {
            json[[as.character(congress)]]$years <- c(json[[as.character(congress)]]$years, year)
        }

        # See if bill number exists in the array corresponding to the bill type
        if (!(bill_number %in% json[[as.character(congress)]][[bill_type]])) {
            json[[as.character(congress)]][[bill_type]] <- c(json[[as.character(congress)]][[bill_type]], bill_number)
        }
    }
}

# compute count values on json
for (congress in names(json)) {
    for (bill_type in c("hr", "s", "hjres", "sjres", "hconres", "sconres")) {
        json[[congress]][[paste0(bill_type, "_count")]] <- unbox(length(json[[congress]][[bill_type]]))
    }
}

# convert values to list of start stops,
# i.e., given a vector [1,2,3,5,7,8,10], return [(1,3), (5,5), (7,8), (10,10)]
for (congress in names(json)) {
    for (bill_type in c("hr", "s", "hres", "sres", "hjres", "sjres", "hconres", "sconres")) {
        json[[congress]][[bill_type]] <- json[[congress]][[bill_type]] %>%
            sort() %>%
            split(cumsum(c(1, diff(.) != 1))) %>%
            map(~ c(min(.), max(.)))
    }
}





# write to file
write_json(json, config_path, pretty = TRUE)
