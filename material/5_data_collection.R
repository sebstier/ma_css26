#' course: "Computational Social Science and Digital Behavioral Data, University of Mannheim"
#' title: "Data collection from APIs"
#' author: "Sebastian Stier"
#' institute: University of Mannheim & GESIS
library(tidyverse)


# Tumblr ----

# Set up .Renviron with key and secret
usethis::edit_r_environ() # Define environment variable in the following format: RTUMBLR_TOKEN=consumer_key;consumer_secret


# Line 13 to 90 written by Claude AI
# Default delay of 3.6s = 1000 calls/hour max
get_all_posts_tag <- function(tag, max_posts = Inf, delay = 3.6) {
  all_posts <- list()
  before_ts <- NULL
  total <- 0
  calls_this_hour <- 0
  hour_start <- Sys.time()
  calls_today <- 0
  day_start <- Sys.time()
  
  repeat {
    # --- Rate limit management ---
    now <- Sys.time()
    
    # Reset hourly counter if an hour has passed
    if (as.numeric(difftime(now, hour_start, units = "secs")) >= 3600) {
      calls_this_hour <- 0
      hour_start <- now
    }
    
    # Reset daily counter if a day has passed
    if (as.numeric(difftime(now, day_start, units = "secs")) >= 86400) {
      calls_today <- 0
      day_start <- now
    }
    
    # Pause if approaching hourly limit (leave small buffer of 10 calls)
    if (calls_this_hour >= 990) {
      wait_secs <- 3600 - as.numeric(difftime(now, hour_start, units = "secs"))
      message(sprintf("Approaching hourly limit. Waiting %.0f seconds until reset...", wait_secs))
      Sys.sleep(max(wait_secs, 0) + 1)
      calls_this_hour <- 0
      hour_start <- Sys.time()
    }
    
    # Stop if daily limit reached
    if (calls_today >= 5000) {
      message("Daily rate limit of 5000 calls reached. Stopping.")
      break
    }
    
    # --- API call ---
    batch <- tryCatch(
      if (is.null(before_ts)) {
        Rtumblr::get_posts_tag(tag = "transgender", limit = 20)
      } else {
        Rtumblr::get_posts_tag(tag = "transgender", limit = 20, before = before_ts)
      },
      error = function(e) { message("API error: ", e$message); NULL }
    )
    
    calls_this_hour <- calls_this_hour + 1
    calls_today     <- calls_today + 1
    
    # --- Process batch ---
    batch_df <- dplyr::bind_rows(batch)
    
    if (is.null(batch_df) || nrow(batch_df) == 0) {
      message("No more posts found.")
      break
    }
    
    all_posts <- append(all_posts, list(batch_df))
    total <- total + nrow(batch_df)
    message(sprintf(
      "Fetched %d posts | API calls: %d/hour, %d/day",
      total, calls_this_hour, calls_today
    ))
    
    if (total >= max_posts) break
    
    before_ts <- min(batch_df$timestamp, na.rm = TRUE)
    
    Sys.sleep(delay)
  }
  
  dplyr::bind_rows(all_posts)
}

# Fetch posts and create dataset
tumblr_test <- get_all_posts_tag(tag = "transgender", max = 1000)
save(tumblr_test, file = "tumblr_test.RData") # Save data in project


# Bluesky ----

library(bskyr)
# Create a Bluesky account
# Then create an app password: https://bsky.app/settings/app-passwords
# Then verify once and store the ~/.Renviron file 
#set_bluesky_user()
#set_bluesky_pass()
bs_auth(user = get_bluesky_user(), pass = get_bluesky_pass(), save_auth = TRUE)
profile <- bs_get_profile('nytimes.com')
feed <- bs_get_author_feed('nytimes.com', limit = 10)
followers <- bs_get_followers('nytimes.com', limit = 10)


# YouTube ----

library(tuber)
# Get your API verification here: https://developers.google.com/youtube/v3/getting-started
# More precisely get an API key here: https://console.cloud.google.com/apis/credentials

#client_id <- "YOUR-CLIENT-ID"
#client_secret <- "YOUR-CLIENT-SECRET"

yt_oauth(
  app_id = client_id,
  app_secret = client_secret
)
# After you were offline, you have to re-authenticate in a browser using the following command
# unlink(".httr-oauth")
# then run the chunk yt_oauth... above again

# Check out the functions in 
#tuber::
get_stats(video_id = "24UV7imfIRk")

get_video_details(video_id = "24UV7imfIRk")

df_yt <- get_comment_threads(
  filter = list(video_id = "24UV7imfIRk"),
  max_results = 20
)
