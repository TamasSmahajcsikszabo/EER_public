library(tidyverse)

get_ICC <- function(lmObject, round = TRUE) {
  # expects an lme() object as parameter
  varCorObject <- VarCorr(lmObject)
  interceptVar <- as.numeric(varCorObject[1])
  residualVar <- as.numeric(varCorObject[2])
  ICC <- interceptVar / (interceptVar + residualVar)
  
  if (round) {
    round(ICC, 3)
  } else {
    ICC
  }
}

get_missing_date <- function(start_date, end_date, only_missing = FALSE) {
  date_diff <- abs(as.numeric(end_date - start_date)) - 1

  dates <- c(start_date)

  for (i in seq(1, date_diff)) {
    dates[i + 1] <- dates[i] + 1
  }
  dates <- c(dates, end_date)

  if (only_missing) {
    dates[c(-1, -length(dates))]
  } else {
    dates
  }
}

get_status_check <- function(dataset, user_id, min_date = NA, max_date = NA, periods = "") {
  user_data <- dataset[dataset$id == user_id, ]
  date_summary <- summary(user_data$date)
  if (is.na(min_date)) {
    min_date <- lubridate::as_date(as.numeric(date_summary["Min."]))
  }

  if (is.na(max_date)) {
    max_date <- lubridate::as_date(as.numeric(date_summary["Max."]))
  }

  full_date_range <- get_missing_date(min_date, max_date)
  if (periods == "") {
    periods <- c("morning", "evening")
  }
  profile <- tibble()
  for (date in full_date_range) {
    for (p in periods) {
      actual_row <- tibble(
        "id" = user_id,
        "date" = lubridate::as_date(date),
        "period" = p
      )
      profile <- bind_rows(profile, actual_row)
    }
  }
  profile
}

insert_missing_obs <- function(dataset, filled = c("id"), min_date = NA, max_date = NA) {
  min_date <- lubridate::as_date(min_date)
  max_date <- lubridate::as_date(max_date)
  result <- dataset %>%
    mutate(
      start_hour = lubridate::hour(start_date),
      date = lubridate::as_date(start_date)
    ) %>%
    mutate(period = if_else(start_hour %in% seq(7, 12, 1), "morning",
      if_else(start_hour %in% c(20, 21, 22, 23, 0, 1, 2), "evening", "invalid")
    )) %>%
    filter(period != "invalid") %>%
    group_by(id) %>%
    mutate(lag_morning_evening = lag(period)) %>%
    mutate(status_check = period == lag_morning_evening) %>%
    mutate(date = if_else(start_hour %in% c(seq(7, 12, 1), 20, 21, 22, 23), date, if_else(start_hour %in% c(0, 1, 2), date - 1, date)))

  user_ids <- as.character(unique(dataset$id))
  modified_result <- tibble()

  for (user in user_ids) {
    user_profile <- get_status_check(result, user, min_date = min_date, max_date = max_date)
    user_data <- result %>%
      filter(id == user) %>%
      mutate(id = as.character(id))

    user_modified_data <- user_profile %>%
      left_join(user_data)
    modified_result <- bind_rows(modified_result, user_modified_data)
  }

  modified_result <- modified_result %>%
    dplyr::select(-lag_morning_evening, -status_check)

  # fill in missing columns
  results <- tibble()
  for (user in user_ids) {
    user_data <- modified_result %>%
      filter(id == user)

    for (variable in filled) {
      user_value <- unique(user_data[, names(user_data) == variable][[1]])
      if (sum(!is.na(user_value)) >= 1) {
        user_value <- user_value[!is.na(user_value)]
      }
      user_data[, names(user_data) == variable][is.na(user_data[, names(user_data) == variable])] <- user_value
    }
    results <- bind_rows(results, user_data)
  }

  results <- results %>%
    group_by(id) %>%
    mutate(
      obs = row_number(),
      sampling_id = obs %/% 2,
      day = if_else(is.na(lead(sampling_id)), max(lead(sampling_id), na.rm = TRUE), lead(sampling_id))
    ) %>%
    ungroup() %>%
    dplyr::select(-sampling_id)

  results
}


################# APPENDIX #########################
obs_create <- function(dataset, user_id = "400") {
  actual_df <- dataset[dataset$id == user_id, ]
  user_status_check <- actual_df$status_check
  user_status_check[is.na(user_status_check)] <- FALSE

  obs <- c()

  for (n in seq_along(user_status_check)) {
    if (n == 1) {
      obs[n] <- 1
    } else if (!user_status_check[n]) {
      obs[n] <- obs[n - 1] + 1
    } else if (user_status_check[n]) {
      obs[n] <- obs[n - 1] + 2
    }
  }

  actual_df$obs <- obs
  actual_df
}
# test <- baseline[baseline$id == "202", ]
# test <- obs_create(test, "202")
