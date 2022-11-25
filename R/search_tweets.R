source("~/Masterarbeit/R/packages.R")

#' Collects tweets over the span of a single day. If the rate limit is reached,
#' wait and try again after 15 minutes.
#' 
#' @param day Character string, Date, POSIXct or POSIXlt for which tweets should
#' be gathered.
#' @param ... Further arguments passed to `v2_search_tweets`.
collect_tweets <- function(query, day, ...) {
  if (is.na(day)) {
    cli_abort("All tweets from available days have been collected.")
  }
  
  # Get 00:00:00 of input day
  start <- as_datetime(day) %>%
    floor_date(unit = "days")
  
  # Get 23:59:59 of input day
  end <- start + 86400 - 1
  
  # Get numeric of datetimes to calculate with
  start_num <- as.numeric(start)
  end_num <- as.numeric(end) -  start_num
  
  # Format dates for all uses
  start_iso <- format(start, format = "%Y-%m-%dT%H:%M:%SZ")
  end_iso <- format(end, format = "%Y-%m-%dT%H:%M:%SZ")
  ymd <- format(start, format = "%Y-%m-%d")
  ymd_short <- format(start, format = "%y%m%d")

  # Prepare iterators and output list
  newest_id <- NULL
  contents <- list()
  time_set <- 0
  i <- j <- 1

  cli::cli_alert_info("Collecting tweets for {ymd}.")
  
  # Initialize progress bar
  cli_progress_bar(
    status = "Searching tweets",
    format = "{pb_status} {pb_bar} {pb_percent}, {time_display} | ETA: {pb_eta}",
    total = end_num
  )
  
  # Iterate through every time period of the input day
  # The number of iterations and the size of each time period is determined by
  # the tweet volume over time. 
  repeat {
    # Query Twitter API
    res <- v2_search_tweets(
      query = query,
      end_time = end_iso,
      start_time = start_iso,
      ...
    )
    
    # Parse response
    contents[[i]] <- resp_body_json(res, simplifyVector = TRUE, flatten = TRUE)
    
    # Check if results were returned. If no results are returned this means that
    # the loop is complete and all contents of a day are extracted.
    no_result <- identical(contents[[i]]$meta$result_count, 0L)

    if (res$status_code != 200L || no_result) {
      message <- contents[[i]]$errors$message
      title <- contents[[i]]$title
      err_time <- str_extract(message, "[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}Z")
      
      # If HTTP error "Too Many Request", just wait for 15 minutes and try again
      if (res$status_code == 429L) {
        cli_progress_update(status = "Waiting out rate limit")
        Sys.sleep(900)
        next
      }

      # If no results are output or if an HTTP error is returned that repeats
      # the same datetime twice, complete the function call
      if ((!is.null(message) && (!is.na(err_time) && str_count(message, err_time) == 2)) || no_result) {
        saveRDS(contents, sprintf("data/tweets/%s.rds", ymd_short))
        cli_alert_success("All tweets for {ymd} were collected.")
        return(contents)
      }
      
      # If an unknown error occurs, throw an error
      cli_inform(c(
        "!" = "Function exited due to HTTP error:",
        "x" = "{res$status_code} {title}: {message}"
      ))
      return(contents)
    }
    
    # Time that is displayed next to the progress bar
    time_display <- contents[[i]]$data$created_at %>%
      min() %>%
      as_datetime() %>%
      format("%H:%M:%S")
    
    # Set current progress bar value by taking the lowest time value included
    # in the output and turning it into a numeric
    time_set <- contents[[i]]$data$created_at %>%
      min() %>%
      as_datetime() %>%
      as.numeric() %>%
      subtract(start_num) %>%
      subtract(end_num, .)
    
    # Update end_time with each iteration where end_time is equal to the lowest
    # time value included in the output of the current iteration
    end_iso <- contents[[i]]$data$created_at %>%
      min() %>%
      as_datetime() %>%
      format("%Y-%m-%dT%H:%M:%SZ")

    pb <- cli_progress_update(set = time_set, status = "Searching tweets")
    i <- i + 1
  }
  
  cli_progress_done()
}


v2_search_tweets <- function(
  query,
  end_time = NULL,
  start_time = NULL,
  expansions = list(),
  media.fields = list(),
  place.fields = list(),
  poll.fields = list(),
  tweet.fields = list(),
  user.fields = list(),
  max_results = 100L,
  next_token = NULL,
  since_id = NULL,
  until_id = NULL,
  sort_order = c("recency", "relevancy")
) {
  bearer_token <- Sys.getenv("TWIT_BEARER")
  
  end_time <- format(end_time, format = "%Y-%m-%dT%T")
  start_time <- format(start_time, format = "%Y-%m-%dT%T")
  
  sort_order <- match.arg(sort_order)
  
  params <- list(
    query = query,
    end_time = end_time,
    expansions = paste(expansions, collapse = ","),
    max_results = max_results,
    media.fields = paste(media.fields, collapse = ","),
    next_token = next_token,
    place.fields = paste(place.fields, collapse = ","),
    poll.fields = paste(poll.fields, collapse = ","),
    since_id = since_id,
    sort_order = sort_order,
    start_time = start_time,
    tweet.fields = paste(tweet.fields, collapse = ","),
    until_id = until_id,
    user.fields = paste(user.fields, collapse = ",")
  )
  params <- params[!sapply(params, is.null)]

  request("https://api.twitter.com/2/tweets/search/recent") %>%
    req_method("GET") %>%
    req_auth_bearer_token(bearer_token) %>%
    list() %>%
    c(.req = ., params) %>%
    do.call(req_url_query, .) %>%
    req_error(is_error = function(res) FALSE) %>%
    req_perform()
}
