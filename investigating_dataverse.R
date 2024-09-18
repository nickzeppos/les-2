library(diffobj)
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

# I generated cleaned bill text files with 07-supporting-info.A.R
# Am now going to comp the cleaned bill text files with the example cleans provided in the replication repo
ex_clean_103hr1ih <- readLines("replication/from_dataverse/CLEAN-103-HR-1-IH.txt")
ex_clean_103hr2rh <- readLines("replication/from_dataverse/CLEAN-103-HR-2-RH.txt")

my_clean_103hr1ih <- readLines("replication/clean/103-HR-1-IH.txt")
my_clean_103hr2rh <- readLines("replication/clean/103-HR-2-RH.txt")

# Let's take a look at the bill they use in their appendix, though: 111-HR-408-IH.
# First 75 words of the appendix:
from_appendix <- "cited southern nevada limited transition area conveyance notwithstanding federal land policy management et seq request city without consideration subject valid existing rights convey city right interest united transition area use land nonresidential development general conveyance city city sell lease otherwise convey portion portions transition area purposes nonresidential development method sale general sale lease conveyance land competitive bidding process fair market value land sold leased otherwise conveyed less fair market value compliance charter except paragraphs city"

# Now let's load in and slice the first 497 from the bill cleaned by 07-supporting-info-A.R
my_clean_111hr408ih <- readLines("replication/clean/111-HR-408-IH.txt")
from_me <- paste(my_clean_111hr408ih[1], collapse = " ")
words <- unlist(strsplit(from_me, "\\s+"))
sliced <- paste(words[1:100], collapse = " ")


print("Comparing 103-HR-1-IH")
compare_and_print(ex_clean_103hr1ih, my_clean_103hr1ih)
print("Comparing 103-HR-2-RH")
compare_and_print(ex_clean_103hr2rh, my_clean_103hr2rh)
print("Comparing 111-HR-408-IH")
compare_and_print(from_appendix, sliced)
