# Reimplimenting Mary's bill text treatment for LES 2.0.
# The treatment is spread across a few files.
# I have placed copies of the two main files in the old_code folder.
# I'm going to reimpliment the full process in this file.


# Basic outline is something like this:
# 1. FETCH: Given {congress_number}, fetch from govinfo.gov
# 2. CONVERT: Convert bill text html -> txt with html2text
# 3. CLEAN: Clean text per Casas, Denny, Wilkerson
# 4. COMPARE: For every ENR, IH, IS, tokenize into trigrams, compute r_o_m

# libs
library(textreuse)
library(diffobj)
suppressPackageStartupMessages({
    library(dplyr)
    library(purrr)
})

# imports
clean_fn <- new.env()
sys.source("clean.r", envir = clean_fn, keep.source = FALSE, keep.parse.data = FALSE)

# # consts
congress <- 117
# # clean
out <- clean_fn$clean_congress(congress)

print(out)

# testing
