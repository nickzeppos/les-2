library(httr2)
library(stringr)

fetch <- function(url) {
    req <- request(url)
    res <- req_perform(req)
    return(res)
}


extract_text_from_xml <- function(xml_data, xpath) {
    namespace <- xml_ns(xml_data)
    xml_find_all(xml_data, xpath, namespace) %>%
        xml_text()
}

parse_sitemap_bill_url <- function(url) {
    split <- strsplit(url, "-")[[1]][2]
    parts <- str_match(split, "(\\d+)(\\D+)(\\d+)(\\D+)")

    congress_part <- parts[1, 2]
    bill_type <- parts[1, 3]
    bill_number_part <- parts[1, 4]
    bill_version_part <- parts[1, 5]

    return(list(
        congress = as.integer(congress_part),
        bill_type = bill_type,
        bill_number = as.integer(bill_number_part),
        bill_version = bill_version_part
    ))
}
