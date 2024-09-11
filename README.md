**STEPS**  
1. [Fetch bill data](#fetch-bill-data)
2. [Copy `.htm` files over](#copy-htm-files-over)
3. [Rename files](#rename-files)
4. [Preprocess](#preprocess)
5. [Process](#process)
6. [Questions](#questions)
### Fetch bill data
- Looks like this was done, no more code around. Raw text stored in Dropbox:
```
LES2/
└── bill_text_raw/ 
    └──{congressNumber}/ 
        └──{billType}/ 
            └──{billNumber}/ 
                └──text-versions/ 
                    └──{billVersion}/ 
                        └──BILLS-{*}/ 
                            └──html/
                                └──*.htm


## looks a lot like how govinfo organizes link service collections
```
### Copy `.htm` files over
**Special case for the 112th where we have to run `rename112.sh`**
- Copy text files over to new dirs: `{congressNumber}/ -> {congressNumber}_1/`

### Rename files
- 2 R scripts: `0.1_file_rename.R` `0_file_rename.R` 
- 1 bash command using `html2text`
- in order, something like: `0_ > html2text > 0.1_`
- `0_` does ~ this:
    1. list the files
    2. push to uppercase
    3. remove "BILLS-" prefix
    4. introduce hyphens to separate args (e.g., "100HR1IH" `->` "100-HR-1-IH")
    5. push file extension to lowercase
    6. rename files with new names  
- `html2text`:  
    ```sh
    for file in *.htm; do html2text "$file" > "$file.txt"; done
    ```

- `0.1_` does ~ this:  
    1. drop the `.htm` files
### Preprocess
- script name - `1_cleaning.R`
- Is the preprocessing treatment from Casas, Denny, Wilkerson Legislative Hitchhikers paper?
- Does ~ this:
    1. Read in
    2. Only care about certain bill versions
    ```R
    if (bill_version %in% 
      c("ENR", "IH", "EH", "RH", "EAS", 
        "RS", "EAH", "ES", "IS", "AS"))
    ```
    3. Remove amending/editing text, generally demarcated by \<DELETED> tags
    ```R
    text <- rm_amendments(doc) # rm_amendments() from wilkerson replication repo
    ```
    4. Remove procedural titles
    ```R
    sections <- get_section_titles(text) # get_section_titles from wilkerson replication repo
        %>% mutate(...) # findings, definitions, auth of approps, toc
    
    # slice sections out
    orig_text <- text
    sec_i_to_remove <- which(sections$to_remove == 1)
    for (i in sec_i_to_remove) {
        start <- sections$start_i[i]
        end <- sections$end_i[i]
        txt_to_rm <- substring(orig_text, start, end)
        text <- gsub(txt_to_rm, "", text, fixed = TRUE)
    }
    remaining_sections <- get_section_titles(text)
    ```
    5. Remove titles of sections
    ```R
    orig_text2 <- text
    for (i in 1:nrow(remaining_sections)) {
        start <- remaining_sections$start_i[i]
        end <- remaining_sections$title_end_i[i]
        if (start > 0) {
        txt_to_rm <- substring(orig_text2, start, end)
        text <- gsub(txt_to_rm, "", text, fixed = TRUE)
        }
    }
    ```
    5. Do some text cleaning. Stop words, lowercase, line breaks, numbers, etc. Also remove procedural head and tail
    ```R
    # drop punctuation
    processed_txt_no_punct <- qdap::clean(get_bill_core_text(text))
    # drop numbers
    processed_txt_no_num <- gsub("[0-9]", "", processed_txt_no_punct)
    # get unigrams, drop stop words
    processed_unigrams <- paste0(
        # get_unigrams from wilkerson replication repo
        get_unigrams(processed_txt_no_num, stopw_list = stopw), 
        collapse = "")
    # drop procedural tokens from tail
    for (t in tokens_bottom) { # tokens_bottom defined earlier
        processed_unigrams <- gsub(t, "", processed_unigrams)
    }
    # drop line breaks
    doc_clean1 <- qdap::clean(gsub("\n", " ", processed_unigrams))
    # drop procedural header statement
    doc_clean2 <- gsub(top_statement_unigrams, "", doc_clean1)
    ```
- Wilkerson replication function file, in Dropbox -  `LES2/Casas.../hitch.../code/00-functions.R`

### Process
- script name - `2_all_against_all_10112021.R`
- Does ~ this:
    1. Read in data cleaned in preprocessing
    2. Tokenize into trigrams
    ```R
    n3_corpus_117 <- textreuse::TextReuseCorpus(
        text=docs, 
        tokenizer=textreuse::tokenize_ngrams, 
        n = 3, 
        progress = T)
    ```
    3. Generate pairwise similarity score matrix. $M$ square matrix, $M = corpus.length$
    ```R
    n3_comparisons_117_directional_s <- textreuse::pairwise_compare(
        n3_corpus_117, # corpus
        ratio_of_matches, # compare fn 
        progress = T # show progress bar in terminal
        directional = T) # do bidirectional comparison, because our compare fn is not commutative (r_of_m(a,b) != r_of_m(b,a))

    # more on ratio_of_matches, from textreuse docs: finds the ratio between the number of items in b that are also in a and the total number of items in b.Note that this similarity measure is directional: it measures how much b borrows from a, but says  nothing about how much of a borrows from b.
    # https://cran.r-project.org/web/packages/textreuse/textreuse.pdf, 17.
    ```
    4. Get candidates from all-to-all matrix. $(M^2 - M)\times3$ comparisons
    ```R
    candidates <- pairwise_condidates(
        n3_comparisons_117_directional_s, # similarity matrix
        directional = T) # same as usage in pairwise_compare
    ```
    5. Not sure after this. Maybe a few different threads being followed?

### Questions
- What is the rest of the code in `2_all_against_all_10112021.R` for?  

- What's the intended output?
- What's specifically wrong with the 112th? Is it just the naming convention thing that `rename112.sh` fixes?
    - The current data is a rescrape of the 112th, and is still having problems.
    -  