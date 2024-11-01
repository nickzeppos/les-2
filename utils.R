# libs
library(httr2)
library(stringr)
library(dotenv)

# source
source("00-functions.R")


# consts
sitemap_url_xpath <- ".//d1:loc"
govinfo_base_url <- "https://www.govinfo.gov"
govinfo_bill_text_base_url <- "https://www.govinfo.gov/content/pkg"
cdg_api_base_url <- "https://api.congress.gov/v3"
root_path <- getwd()
cache_path <- paste0(root_path, "/data")
bill_text_path <- paste0(cache_path, "/bill_text")
cleaned_bill_text_path <- paste0(cache_path, "/cleaned_bill_text")


# env
load_dot_env()
cdg_api_key <- Sys.getenv("CONGRESS_DOT_GOV_API_KEY")

# methods
curl_html2text <- function(url, path) {
    cmd <- paste("bash -c", shQuote(paste("curl -s -L --max-time 10", url, "| html2text >", shQuote(path))))
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

fetch <- function(url) {
    req <- request(url)
    res <- req_perform(req)
    return(res)
}

build_api_url <- function(route, congress, bill_type, bill_number, endpoint) {
    args <- c(cdg_api_base_url, route, congress, bill_type, bill_number, endpoint)
    url <- paste(args, collapse = "/")
}

fetch_congress_api <- function(url) {
    req <- request(url) %>%
        req_url_query(format = "json", api_key = cdg_api_key)
    res <- req_perform(req)
    json <- resp_body_json(res)
    return(json)
}

rvest_fetch <- function(url) {
    return(read_html(url))
}


extract_text_from_xml <- function(xml_data, xpath) {
    namespace <- xml_ns(xml_data)
    xml_find_all(xml_data, xpath, namespace) %>%
        xml_text()
}


get_urls_from_sitemap <- function(sitemap_xml) {
    return(extract_text_from_xml(sitemap_xml, sitemap_url_xpath))
}

parse_sitemap_bill_url <- function(url) {
    split <- strsplit(url, "-")[[1]][2]
    parts <- str_match(split, "(\\d+)(\\D+)(\\d+)(\\D+)")

    congress_part <- parts[1, 2]
    bill_type <- parts[1, 3]
    bill_number_part <- parts[1, 4]

    # if bill_version_part contains a suffix (e.g., .html, .txt)
    # strsplit on '.' and take the first part
    bill_version_part <- strsplit(parts[1, 5], "\\.")[[1]][1]

    return(list(
        congress = as.integer(congress_part),
        bill_type = bill_type,
        bill_number = as.integer(bill_number_part),
        bill_version = bill_version_part,
        url = url
    ))
}

filter_bill_urls <- function(url,
                             congresses_to_keep,
                             bill_types_to_keep = NULL,
                             bill_numbers_to_keep = NULL,
                             bill_versions_to_keep = NULL) {
    # Split the url, get the relevant parts
    split <- strsplit(url, "-")[[1]][2]
    parts <- str_match(split, "(\\d+)(\\D+)(\\d+)(\\D+)")
    congress_part <- parts[1, 2]
    bill_type_part <- parts[1, 3]
    bill_number_part <- parts[1, 4]
    bill_version_part <- parts[1, 5]

    # Check if the parts match the conditions specified
    congress_match <- congress_part %in% congresses_to_keep
    # Possibly NULL conditions
    bill_type_match <- ifelse(is.null(bill_types_to_keep), TRUE, bill_type_part %in% bill_types_to_keep)
    bill_number_match <- ifelse(is.null(bill_numbers_to_keep), TRUE, as.integer(bill_number_part) %in% bill_numbers_to_keep)
    bill_version_match <- ifelse(is.null(bill_versions_to_keep), TRUE, bill_version_part %in% bill_versions_to_keep)


    # return true if all specified conditions match
    return(congress_match & bill_type_match & bill_number_match & bill_version_match)
}

# data type encoder/decoders
args_to_path <- function(...) {
    # given args, collapse them on / (i.e. a file path)
    args <- list(...)
    args <- lapply(args, as.character)

    # E.g., ["116", "hr"] -> "116/hr"
    return(paste(args, collapse = "/"))
}

bill_key_to_file_name <- function(bill_key) {
    # given a bill_key: str
    # return corresponding file name: str

    # E.g., "116hr1ih" -> "BILLS-116hr1ih.txt"
    return(paste0("BILLS-", bill_key, ".txt"))
}

bill_key_to_endpoint <- function(bill_key) {
    # given a bill_key: str
    # return corresponding endpoint: str

    # E.g., "116hr1ih" -> "BILLS-116hr1ih/html/BILLS-116hr1ih.htm"
    endpoint <- paste0("BILLS-", bill_key, "/html/BILLS-", bill_key, ".htm")
    return(endpoint)
}

path_to_bill_key <- function(path) {
    # given a path: str
    # return corresponding bill_key: str

    # E.g.,
    # "/.././BILLS-116hrih.txt" -> "116hr1ih"
    split <- strsplit(path, "-")[[1]][2]
    parts <- str_match(split, "(\\d+)(\\D+)(\\d+)(\\D+)")

    congress_part <- parts[1, 2]
    bill_type_part <- parts[1, 3]
    bill_number_part <- parts[1, 4]
    # if bill_version_part contains a suffix (e.g., .html, .txt)
    # strsplit on '.' and take the first part
    bill_version_part <- strsplit(parts[1, 5], "\\.")[[1]][1]

    bill_key <- paste0(congress_part, bill_type_part, bill_number_part, bill_version_part)
    return(bill_key)
}

url_to_bill_key <- function(url) {
    # given a govinfo bill text url: str
    # return corresponding bill_key: str

    # E.g., "https://www.govinfo.gov/content/pkg/BILLS-116hr1ih/html/BILLS-116hr1ih.htm" -> "116hr1ih"
    split <- strsplit(url, "-")[[1]][2]
    parts <- str_match(split, "(\\d+)(\\D+)(\\d+)(\\D+)")
    congress_part <- parts[1, 2]
    bill_type <- parts[1, 3]
    bill_number_part <- parts[1, 4]

    # if bill_version_part contains a suffix (e.g., .html, .txt)
    # strsplit on '.' and take the first part
    bill_version_part <- strsplit(parts[1, 5], "\\.")[[1]][1]

    bill_key <- paste0(congress_part, bill_type, bill_number_part, bill_version_part)
    return(bill_key)
}

bill_key_to_path <- function(bill_key, path_type) {
    # given a bill_key: str
    # return corresponding path: str

    # E.g., "116hr1ih", "raw" -> "/data/bill_text/116/BILLS-116hr1ih.txt"

    # assert path_type is either "raw" or "clean"
    if (!path_type %in% c("raw", "clean")) {
        stop("Invalid path_type. Must be either 'raw' or 'clean'.")
    }

    file_name <- bill_key_to_file_name(bill_key)
    parts <- str_match(bill_key, "(\\d+)(\\D+)(\\d+)(\\D+)")

    congress_part <- parts[1, 2]

    dir_path <- ifelse(path_type == "raw", bill_text_path, cleaned_bill_text_path)
    dir_path <- args_to_path(dir_path, congress_part)
    file_path <- args_to_path(dir_path, file_name)
    return(file_path)
}

bill_key_to_url <- function(bill_key) {
    # given a bill_key: str
    # return corresponding govinfo bill text url: str

    # E.g., "116hr1ih" -> "https://www.govinfo.gov/content/pkg/BILLS-116hr1ih/html/BILLS-116hr1ih.htm"
    endpoint <- bill_key_to_endpoint(bill_key)
    url <- make_path(govinfo_bill_text_base_url, endpoint)
    return(url)
}

parse_bill_key <- function(bill_key) {
    # given a bill_key: str
    # return list of parts: [congress, bill_type, bill_number, bill_version]

    # E.g., "116hr1ih" -> [116, "hr", 1, "ih"]
    parts <- str_match(bill_key, "(\\d+)(\\D+)(\\d+)(\\D+)")
    congress <- as.integer(parts[1, 2])
    bill_type <- parts[1, 3]
    bill_number <- as.integer(parts[1, 4])
    bill_version <- parts[1, 5]
    return(list(
        congress = congress,
        bill_type = bill_type,
        bill_number = bill_number,
        bill_version = bill_version
    ))
}

encode_bill_key <- function(congress, bill_type, bill_number, bill_version) {
    # given parsed bill key parts: [congress, bill_type, bill_number, bill_version]
    # return bill_key: str

    # E.g., 116, "hr", 1, "ih" -> "116hr1ih"
    return(paste0(congress, bill_type, bill_number, bill_version))
}

get_word_count <- function(path) {
    # given a path: str
    # return word count: int

    # E.g., "/.././BILLS-116hrih.txt" -> 1234
    text <- readLines(path)
    count <- str_count(text)
    return(count)
}

# bill ids are something for outputs to academics, not an internal identifier
# so only used at the very end of a workflow, i.e. to create a useful column in a .csv output, etc.

# core encoder and decoder for id <-> key
bill_key_to_bill_id <- function(bill_key) {
    # given a bill_key: str
    # return corresponding bill_id: str

    # E.g., "116hr1ih" -> "116-HR-1-IH"
    parts <- str_match(bill_key, "(\\d+)(\\D+)(\\d+)(\\D+)")
    congress_part <- parts[1, 2]
    bill_type_part <- parts[1, 3]
    bill_number_part <- parts[1, 4]
    bill_version_part <- parts[1, 5]
    concat <- paste(congress_part, bill_type_part, bill_number_part, bill_version_part, sep = "-")
    return(toupper(concat))
}

bill_id_to_bill_key <- function(bill_id) {
    # given a bill_id: str
    # return corresponding bill_key: str

    # E.g., "116-HR-1-IH" -> "116hr1ih"
    parts <- strsplit(bill_id, "-")[[1]]
    concat <- paste(parts[1], parts[2], parts[3], parts[4], sep = "")
    return(tolower(concat))
}



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

# just get the core bill id, e.g. 116-HR-1, drop the version
get_core_bill_id <- function(bill_id) {
    parts <- strsplit(bill_id, "-")[[1]]
    return(paste(parts[1], parts[2], parts[3], sep = "-"))
}
