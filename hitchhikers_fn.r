# Bringing together some functions from a few different sources for our R reimplimentation


# deps
library(stopwords)
library(qdap)


# Hitchhikers code with qdap::clean replaced with local implementation

# ===============================================================================
# Function from the congressReuse package to clean the top and bottom
#   procedural stuff from the bills text
get_bill_core_text <- function(bill_full_text) {
  # sec1_ind <- as.numeric(regexec("SECTION 1.", bill_full_text))
  lower_text <- tolower(bill_full_text)
  front_cut <- as.numeric(regexec("be it enacted by", lower_text))
  # if (grepl("AN ACT", bill_full_text)) {
  #  front_cut <- as.numeric(regexec("AN ACT[[:space:]]+", bill_full_text)) +
  #    attr(regexec("AN ACT[[:space:]]+", bill_full_text)[[1]], "match.length")
  # } else {
  #  front_cut <- as.numeric(regexec("A BILL", bill_full_text)) +
  #    attr(regexec("A BILL[[:space:]]+", bill_full_text)[[1]], "match.length")
  # }
  bill_no_front <- substring(bill_full_text, front_cut, nchar(bill_full_text))
  end_cut_marker <- names(which(sapply(
    bill_endings,
    function(x) grepl(x, bill_no_front)
  ) == TRUE))
  if (length(end_cut_marker) == 0) {
    end_cut <- nchar(bill_no_front)
  } else {
    if (length(end_cut_marker > 1)) {
      matches <- sapply(end_cut_marker, function(x) regexec(x, bill_no_front))
      matches_index <- as.numeric(matches)
      end_cut <- min(matches_index) - 1
    } else {
      end_cut <- as.numeric(regexec(end_cut_marker[1], bill_no_front)) - 1
    }
  }
  bill_core_text <- substring(bill_no_front, 1, end_cut)
  bill_core_text <- gsub("\n{2,}", "\n", bill_core_text)
  bill_core_text <- gsub("\n\\s+", "\n", bill_core_text)
  # bill_core_text <- gsub("([\\()])|[[:punct:]]", "\\1", bill_core_text)
  bill_core_text <- qdap::clean(gsub("[[:punct:]]", " ", bill_core_text))
  bill_core_text <- tolower(bill_core_text)
  return(bill_core_text)
}

bill_endings <- c(
  "<all>", "(?:Union)? Calendar No.", "Passed the [House|Senate]",
  "Speaker of the House of Representatives[.]", "Attest:"
)
months <- c(
  "January", "February", "March", "April", "May", "June", "July",
  "August", "September", "November", "December"
)


# ===============================================================================
get_unigrams <- function(string, rm_stopw = TRUE, stopw_list = NULL,
                         rm_num = TRUE, rm_punct = TRUE, rm_shorter_than = 1,
                         unique = FALSE) {
  # This function takes a string and returns a vector of unigrams.
  if (rm_punct) {
    string <- gsub("[[:punct:]]", "", string)
  }
  if (rm_num) {
    string <- gsub("[0-9]", "", string)
  }
  unigrams <- strsplit(string, split = " ")[[1]]
  if (rm_stopw) {
    unigrams <- unigrams[which(!(unigrams %in% stopw_list))]
  }
  unigrams <- unigrams[which(nchar(unigrams) > 1)]
  if (unique) {
    return(unique(unigrams))
  } else {
    return(unigrams)
  }
}

# ===============================================================================
# Pulling sections from raw bill text
get_section_titles <- function(text) {
  first_mark <- gregexpr(
    "SECTION 1\\. (([A-Z]* ){1,10}|([A-Z]*. ){1,10})", text
  )
  if (as.numeric(first_mark[[1]][1]) < 0) {
    first_mark <- gregexpr(
      "Section 1\\. [A-Z]", text
    )
  }
  marks <- gregexpr("SEC\\. [0-9]{1,}\\. (([A-Z]* ){1,10}|([A-Z]*. ){1,10})", text)
  if (as.numeric(marks[[1]][1]) < 0) {
    marks <- gregexpr("Sec\\. [0-9]{1,}\\. [A-Z]", text)
  }
  if (as.numeric(marks[[1]][1]) != -1) {
    sec_title_start <- c(as.numeric(first_mark[[1]])[1], as.numeric(marks[[1]]))
    sec_title_len <- c(
      attr(first_mark[[1]], "match.length")[1],
      attr(marks[[1]], "match.length")
    )
  } else {
    sec_title_start <- as.numeric(first_mark[[1]])
    sec_title_len <- attr(first_mark[[1]], "match.length")
  }
  sec_title_end <- sec_title_start + sec_title_len
  sec_titles <- as.character(sapply(1:length(sec_title_start), function(x) {
    substring(text, sec_title_start[x], sec_title_end[x])
  }))
  if (length(sec_title_start) > 1) {
    sec_ends <- sec_title_start[2] - 1
    j <- 2
    while (sec_ends < sec_title_start[1]) {
      j <- j + 1
      sec_ends <- sec_title_start[j] - 1
    }
    for (i in 2:length(sec_title_start)) {
      if (length(sec_title_start) > i) {
        new_sec_ends <- sec_title_start[i + 1] - 1
        z <- i + 1
        while (new_sec_ends < sec_title_start[i]) {
          z <- z + 1
          new_sec_ends <- sec_title_start[z] - 1
        }
        sec_ends <- c(sec_ends, new_sec_ends)
      } else {
        sec_ends <- c(sec_ends, nchar(text))
      }
    }
  } else {
    sec_ends <- nchar(text)
  }
  output <- data.frame(
    title = sec_titles,
    start_i = sec_title_start,
    end_i = sec_ends,
    title_end_i = (sec_title_end - 3),
    pre_sec = substring(text, sec_title_start - 2, sec_title_start - 1)
  )
  output$delete <- ifelse(grepl("``", output$pre_sec), 1, 0)
  output <- output[which(output$delete == 0), ]
  return(output)
}


# ===============================================================================
rm_amendments <- function(text) {
  bill_core_text <- text
  if (grepl("&lt;DELETED&gt;", bill_core_text)) {
    opening_deleted_indeces <- gregexpr("&lt;DELETED&gt;", bill_core_text)
    opening_deleted_length <- attr(opening_deleted_indeces[[1]], "match.length")[1]
    opening_deleted_indeces <- as.numeric(opening_deleted_indeces[[1]])
    closing_deleted_indeces <- gregexpr("&lt;/DELETED&gt;", bill_core_text)
    closing_deleted_length <- attr(closing_deleted_indeces[[1]], "match.length")[1]
    closing_deleted_indeces <- as.numeric(closing_deleted_indeces[[1]])
    # Sometimes more opening <DELETED> than closing ones
    # When that it's the case, we add closing ones right before the following
    #   opening.
    if (!(length(opening_deleted_indeces) == length(closing_deleted_indeces))) {
      for (i in 1:(length(opening_deleted_indeces) - 1)) {
        open_ind <- opening_deleted_indeces[i]
        next_open_ind <- opening_deleted_indeces[i + 1]
        closest_close_ind <- closing_deleted_indeces[i]
        if (next_open_ind < closest_close_ind) {
          new_closing_ind <- next_open_ind - 1
          # Inserting an extra closing </deleted> index
          closing_first_part <- closing_deleted_indeces[0:(i - 1)]
          closing_second_part <- closing_deleted_indeces[i:length(closing_deleted_indeces)]
          closing_deleted_indeces <- c(
            closing_first_part, new_closing_ind,
            closing_second_part
          )
        }
      }
    }
    # I find one case (111-S-1733) for which after running the previous loop there
    #   are still more openings <deleted> than closing ones. I checked and it
    #   is because the last opening <deleted> is at the end of the bill and it
    #   doesn't have a closing one, implying that they want to delete until the
    #   end of the bill. I write code below to do that for these cases.
    if (length(opening_deleted_indeces) > length(closing_deleted_indeces)) {
      closing_deleted_indeces <- c(
        closing_deleted_indeces,
        nchar(bill_core_text)
      )
    }
    # If still not same number of opening <deleted> than closing </deleted>,
    #   that means that there are more </deleted>. I checked manually (e.g. bills
    #   111-S-1017-RS and 111-S-1635-RS) and they actually want to delete the
    #   whole thing in between when there are two consecutive </deleted>.
    if (length(opening_deleted_indeces) < length(closing_deleted_indeces)) {
      for (i in 2:(length(closing_deleted_indeces) - 1)) {
        closing_ind <- closing_deleted_indeces[i]
        previous_closing_ind <- closing_deleted_indeces[i - 1]
        closest_opening_ind <- opening_deleted_indeces[i]
        if (closing_ind < closest_opening_ind) {
          new_opening_ind <- previous_closing_ind + 16
          # Inserting an extra opening <deleted> index
          opening_first_part <- opening_deleted_indeces[0:(i - 1)]
          opening_second_part <- opening_deleted_indeces[i:length(opening_deleted_indeces)]
          opening_deleted_indeces <- c(
            opening_first_part, new_opening_ind,
            opening_second_part
          )
        }
      }
    }
    # If still more closings than openings (e.g. 111-S-3903-RS), adding a
    #   an extra opening right after the second from the last closing
    if (length(opening_deleted_indeces) < length(closing_deleted_indeces)) {
      y <- closing_deleted_indeces[length(closing_deleted_indeces) - 1]
      opening_deleted_indeces <- c(opening_deleted_indeces, y + 16)
    }
    # If the number of opening and closing <deleted> are still different, just
    #   get rid of everything before the last <deleted>
    if (length(opening_deleted_indeces) == length(closing_deleted_indeces)) {
      deleting <- data.frame(
        open = opening_deleted_indeces,
        close = closing_deleted_indeces
      )
      clean_text <- substring(bill_core_text, 1, opening_deleted_indeces[1] - 1)
      for (i in 1:nrow(deleting)) {
        closing_ind <- deleting$close[i]
        if (i < nrow(deleting)) {
          next_opening <- deleting$open[i + 1] - 1
          text_to_add <- substring(bill_core_text, closing_ind, next_opening)
          text_to_add <- gsub("&lt;/DELETED&gt;", "", text_to_add)
          text_to_add <- gsub("&lt;DELETED&gt;", "", text_to_add)
        } else {
          text_to_add <- substring(bill_core_text, closing_ind, nchar(bill_core_text))
          text_to_add <- gsub("&lt;/DELETED&gt;", "", text_to_add)
          text_to_add <- gsub("&lt;DELETED&gt;", "", text_to_add)
        }
        if (!is.na(text_to_add)) {
          clean_text <- paste(clean_text, text_to_add)
        }
      }
    } else {
      last_del_tag <- max(c(opening_deleted_indeces, closing_deleted_indeces))
      clean_text <- substring(bill_core_text, last_del_tag, nchar(bill_core_text))
      clean_text <- gsub("&lt;/DELETED&gt;", "", clean_text)
      clean_text <- gsub("&lt;DELETED&gt;", "", clean_text)
    }

    # Now removing the stuff/text between <deleted></deleted> tags
    # initializing the new clean text with the beginning of the bill

    bill_core_text <- clean_text
  }
  bill_core_text <- gsub("&lt;all&gt;", "", bill_core_text)
  return(bill_core_text)
}

# ===============================================================================
# LIST OF STOPWORDS:
#   + The default stopwords in the 'quanteda' pacakge
#   + Some extra stopwords: out of the 50 most frequent words in a random
#       sample of 500 bills, the words we thought to be irrelevant. See the full
#       list of 50 and which ones we considered to be stopwords (== YES).
#     (you can find the file with the top 100 in the data directory:
#      "./data/top_100_features_in_random_500bills.csv")


# 1            shall     33975 YES
# 2          section     29942 YES
# 3              act     23032 YES
# 4        secretary     17741 YES
# 5       subsection     12674 YES
# 6              may     12288 YES
# 7              sec     11481 YES
# 8           states     11350 YES
# 9         provided     11092 YES
# 10           state     10244 NO
# 11       available     10208 NO
# 12          united     10098 NO
# 13            year      9684 NO
# 14         program      9651 NO
# 15       paragraph      9502 YES
# 16           title      9333 YES
# 17         federal      9131 NO
# 18          health      8737 NO
# 19           funds      8503 NO
# 20         general      8225 NO
# 21       including      8049 NO
# 22              ii      7311 YES
# 23        national      7268 NO
# 24          public      7149 NO
# 25         amended      6490 YES
# 26            made      6417 YES
# 27        services      6355 NO
# 28            date      6074 YES
# 29       inserting      5929 YES
# 30            term      5920 NO
# 31          fiscal      5879 NO
# 32     information      5565 NO
# 33      assistance      5542 NO
# 34             law      5532 YES
# 35       following      5352 NO
# 36          agency      5244 YES
# 37         service      5235 NO
# 38        striking      5038 YES
# 39          amount      5012 YES
# 40      activities      4905 YES
# 41        programs      4734 YES
# 42      authorized      4684 YES
# 43       described      4681 NO
# 44             use      4679 NO
# 45          report      4645 YES
# 46    subparagraph      4547 YES
# 47        purposes      4320 NO
# 48            plan      4177 NO
# 49        security      4156 NO
# 50         provide      4041 NO

basic_stopw <- stopwords::stopwords("english")

extra_stopw <- c(
  "shall", "section", "act", "secretary", "subsection", "may",
  "sec", "states", "provided", "paragraph", "title", "ii",
  "amended", "made", "date", "insterting", "law", "agency",
  "striking", "amount", "activities", "programs", "authorized",
  "report", "subparagraph"
)

stopw <- c(basic_stopw, extra_stopw)
