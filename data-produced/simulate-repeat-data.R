library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)

set.seed(123)  # For reproducibility

# 1. Create accounts and nonprofits
n_accounts <- 5000
n_nonprofits <- 500

accounts <- sprintf("A%05d", 1:n_accounts)
nonprofits <- sprintf("N%04d", 1:n_nonprofits)

# 2. Helper function to simulate grant dates
simulate_grant_dates <- function(start_date, end_date, n, pattern = "random") {
  if (pattern == "random") {
    return(sort(sample(seq.Date(start_date, end_date, by = "day"), size = n, replace = FALSE)))
  } else if (pattern == "monthly") {
    start <- sample(seq.Date(start_date, end_date - months(1), by = "day"), 1)
    dates <- seq.Date(start, end_date, by = "month")
    return(dates[1:min(length(dates), n)])
  } else if (pattern == "yearly") {
    start <- sample(seq.Date(start_date, end_date - years(1), by = "day"), 1)
    dates <- seq.Date(start, end_date, by = "year")
    return(dates[1:min(length(dates), n)])
  }
}

# 3. Simulate grantmaking
grantmaking_records <- list()

for (account_id in accounts) {
  n_choices <- sample(0:10, 1)
  chosen_nonprofits <- sample(nonprofits, n_choices)
  
  for (nonprofit_id in chosen_nonprofits) {
    pattern_type <- sample(c("one_time", "random_multi", "monthly", "yearly"),
                           1, prob = c(0.4, 0.3, 0.2, 0.1))
    
    start_date <- as.Date("2015-01-01")
    end_date <- as.Date("2025-01-01")
    
    # Generate dates based on pattern
    if (pattern_type == "one_time") {
      dates <- simulate_grant_dates(start_date, end_date, 1, "random")
    } else if (pattern_type == "random_multi") {
      n_times <- sample(2:30, 1)
      dates <- simulate_grant_dates(start_date, end_date, n_times, "random")
    } else if (pattern_type == "monthly") {
      n_times <- sample(3:24, 1)
      dates <- simulate_grant_dates(start_date, end_date, n_times, "monthly")
    } else if (pattern_type == "yearly") {
      n_times <- sample(2:10, 1)
      dates <- simulate_grant_dates(start_date, end_date, n_times, "yearly")
    }
    
    # Generate grants
    base_grant <- runif(1, 5, 100)
    if (pattern_type %in% c("monthly", "yearly")) {
      grant_vec <- rep(base_grant, length(dates))
    } else {
      grant_vec <- round(rnorm(length(dates), mean = base_grant, sd = base_grant * 0.1), 2)
      grant_vec <- pmax(grant_vec, 1)  # no grant below $1
    }
    
    # Combine into a data frame
    df <- data.frame(
      account_id = account_id,
      nonprofit_id = nonprofit_id,
      grant_date = dates,
      grant_amount = grant_vec,
      stringsAsFactors = FALSE
    )
    
    grantmaking_records[[length(grantmaking_records) + 1]] <- df
  }
}

# 4. Combine all into one dataset
grantmaking_data <- bind_rows(grantmaking_records)

# 5. Preview
print(head(grantmaking_data))
