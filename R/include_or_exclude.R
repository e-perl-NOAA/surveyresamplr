# Generate resampled datasets with specified sample size proportions
include_or_exclude <- function(df, proportions, replicate_num = 1) {
  # Set random seed for reproducibility
  set.seed(123)
  
  # Initialize list to store results
  all_samples <- list()
  
  # For each proportion, create replicate_num samples
  for (prop in proportions) {
    for (rep in 1:replicate_num) {
      # Generate random assignment (1 = include, 0 = exclude)
      temp_df <- df
      temp_df$RandomAssignment <- rbinom(nrow(df), 1, prob = prop)
      
      # Add to results list with name indicating proportion and replicate
      name <- paste0(prop, "_", rep)
      all_samples[[name]] <- temp_df
    }
  }
  
  all_samples
}