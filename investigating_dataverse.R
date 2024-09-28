library(diffobj)
library(textreuse)
options(warn = -1)

# utils
compare_and_print <- function(obj1, obj2, style = diffobj::StyleAnsi()) {
    diff <- diffobj::diffChr(obj1, obj2, style = style)
    diff_summary <- summary(diff)
    all_eq <- slot(diff_summary, "all.eq")
    if (length(all_eq) == 0) {
        cat("No visible differences detected.\n")
    } else {
        print(diff)
    }
}

#
from_dropbox_enr_1319 <- readLines("data/117_testing/117-HR-1319-ENR")
my_enr_1319 <- readLines("data/cleaned_bill_text/117/BILLS-117hr1319enr.txt")
from_dropbox_ih_1609 <- readLines("data/117_testing/117-HR-1609-IH")
my_ih_1609 <- readLines("data/cleaned_bill_text/117/BILLS-117hr1609ih.txt")
from_dropbox_ih_1668 <- readLines("data/117_testing/117-HR-1668-IH")
my_ih_1668 <- readLines("data/cleaned_bill_text/117/BILLS-117hr1668ih.txt")
from_dropbox_ih_1682 <- readLines("data/117_testing/117-HR-1682-IH")
my_ih_1682 <- readLines("data/cleaned_bill_text/117/BILLS-117hr1682ih.txt")

# five grams
from_dropbox_enr_1319_n5 <- textreuse::tokenize_ngrams(from_dropbox_enr_1319, n = 5)
my_enr_1319_n5 <- textreuse::tokenize_ngrams(my_enr_1319, n = 5)
from_dropbox_ih_1609_n5 <- textreuse::tokenize_ngrams(from_dropbox_ih_1609, n = 5)
my_ih_1609_n5 <- textreuse::tokenize_ngrams(my_ih_1609, n = 5)
from_dropbox_ih_1668_n5 <- textreuse::tokenize_ngrams(from_dropbox_ih_1668, n = 5)
my_ih_1668_n5 <- textreuse::tokenize_ngrams(my_ih_1668, n = 5)
from_dropbox_ih_1682_n5 <- textreuse::tokenize_ngrams(from_dropbox_ih_1682, n = 5)
my_ih_1682_n5 <- textreuse::tokenize_ngrams(my_ih_1682, n = 5)


# compare
textreuse::ratio_of_matches(from_dropbox_enr_1319_n5, from_dropbox_ih_1609_n5)
textreuse::ratio_of_matches(from_dropbox_enr_1319_n5, my_ih_1609_n5)
textreuse::ratio_of_matches(my_enr_1319_n5, from_dropbox_ih_1609_n5)
textreuse::ratio_of_matches(my_enr_1319_n5, my_ih_1609_n5)

textreuse::ratio_of_matches(from_dropbox_enr_1319_n5, from_dropbox_ih_1682_n5)
textreuse::ratio_of_matches(from_dropbox_enr_1319_n5, my_ih_1682_n5)
textreuse::ratio_of_matches(my_enr_1319_n5, from_dropbox_ih_1682_n5)
textreuse::ratio_of_matches(my_enr_1319_n5, my_ih_1682_n5)
