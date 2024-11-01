library(dplyr)
library(reshape2)

# consts
congress <- 108
pwc_path <- paste0(getwd(), "/pairwise_comparisons/non_introduced/", congress)
fl <- list.files(pwc_path, full.names = TRUE)

# Initialize an empty list to store all comparisons
all_comparisons <- list()

# Initialize a data frame to store timing results
timing_results <- data.frame(file = character(), load_time = numeric(), convert_time = numeric(), filter_time = numeric(), stringsAsFactors = FALSE)

for (f in fl) {
    print(f)
    # Measure time to load the .RData file
    load_time <- system.time({
        load(f)
    })[3]
    print(paste("Load time:", load_time))

    # Measure time to convert the named list to a data frame
    convert_time <- system.time({
        comparisons_df <- do.call(rbind, lapply(names(comparisons), function(y) {
            data.frame(enr_id = sub("_comparisons.RData", "", basename(f)), ih_is_id = y, value = comparisons[[y]])
        }))
    })[3]

    print(paste("Convert time:", convert_time))

    # Measure time to filter the data frame
    filter_time <- system.time({
        filtered <- comparisons_df %>% filter(value > 0.5)
    })[3]
    print(paste("Filter time:", filter_time))

    # Append the comparisons data frame to the all_comparisons list
    all_comparisons[[f]] <- filtered

    # Store timing results
    timing_results <- rbind(timing_results, data.frame(file = f, load_time = load_time, convert_time = convert_time, filter_time = filter_time))
}

# Combine all comparisons into a single data frame
combined_comparisons <- do.call(rbind, all_comparisons)
save(combined_comparisons, file = paste0(pwc_path, "combined_comparisons.RData"))
# Print timing results
print(timing_results)
