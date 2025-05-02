
# STUDY ONE, Candidate Change Posturing ----------------------------------------------------------------------

# scraping the American Presidential Project
# for campaign documents 

# libraries -----------------------------------------------------------------------

library(tidyverse) # data manipulation
library(here) # working paths
library(tictoc) # stopwatch functionality
library(httr) # access sites 
library(rvest) # harvest (scrape) data 
library(xml2) # xml data manipulation


# working directory
here::i_am("code/1_1_scraping_APP.R")



# 1.1 PREP Automated URL scraping ------------------------------------------------------


# URL of the speeches index page (Modify this to match the right page!)
base_url <- "https://www.presidency.ucsb.edu/documents/app-categories/elections-and-transitions/campaign-documents?items_per_page=60" 

page_num <- 1  # Start at page 1

# maximum number of pages to scrape
max_pages <- 370  # set the limit here 
# need to go to page 361 at least
# page 361 is the first one with 2000


# Initialize an empty vector to store all the scraped URLs
all_links <- c()


# 1.1 Collect URLS to be scraped ------------------------------------------------------

# Loop to scrape each page until max_pages
while(page_num <= max_pages) {
  # Construct the URL for the current page
  url <- paste0(base_url, "&page=",page_num)
  
  # Fetch the webpage
  page <- GET(url)
  
  # Parse the HTML
  html_content <- read_html(content(page, "text"))
  

  # Extract all speech URLs
  speech_links <- html_content %>%
    html_elements(xpath = "//div[contains(@class, 'view-content')]//div[contains(@class, 'field-title')]//a") %>%
    html_attr("href")
  
  # Store the links
  all_links <- c(all_links, speech_links)
  
  # Print status
  cat("Scraped index page", page_num, "for links", "\n")
  
  # Increment the page number
  page_num <- page_num + 1
  
  # Sleep to be polite
  Sys.sleep(1)  # Sleep for 1 seconds between requests
}

# make sure we only save unique ones
all_links <- unique(all_links)
all_links_df <- as.data.frame(all_links)

# write to csv
all_links_df %>%
  write_csv(here("data/APP_UCSB/scraped_links_campaigndocs.csv"))






# 2.1 Function for page scraping using URLS  -------------------------------------------------


# Function to extract text using XPath
extract_text <- function(html_content, xpath) {
  elements <- html_content %>%
    html_elements(xpath = xpath) %>% # basic function that we will feed with values
    html_text(trim = TRUE) # trim for good measure
  
  if (length(elements) > 0) {
    return(elements[1])  # Extract first valid result
  } else {
    return(NA)  # Handle missing data
  }
}




# 2.2 PREP variables and running objects for scraping ---------------------------------------------------------

# Set batch size for saving
save_interval <- 50
today_date <- date()

# import all URLs that we've scraped from index page
all_links_df <- read_csv(here("data/APP_UCSB/scraped_links_campaigndocs.csv"))
all_links <- all_links_df$all_links # convert to list


# load progress from previous scrape efforts
# (total was about 21 hours of scraping, so we had to save progress frequently)
output_file <- here("data/APP_UCSB/scraped_campaigndocs.rds")

# simple logic for keeping track of URLs that we have scraped already
# else, scrape
if (file.exists(output_file)) {
  speech_data <- readRDS(output_file)  # Load existing progress
  scraped_urls <- speech_data$url      # Track scraped URLs
  scraped_urls <- str_remove_all(scraped_urls, "https://www.presidency.ucsb.edu")
  
} else {
  # initialize df to hold our values of interest
  speech_data <- data.frame(
    url = character(),
    date_scraped = character(),
    speaker = character(),
    date = character(),
    title = character(),
    citation = character(),
    body = character(),
    nchars = integer(),
    stringsAsFactors = FALSE
  )
  scraped_urls <- c()  # Start with empty list
}

# figures out
# which still need to be scraped
urls_to_scrape <- setdiff(all_links, scraped_urls)  
                                                  
x <- nrow(speech_data) + 1  # Resume from last scraped entry




# 2.3 BEGIN actual scraping of pages -------------------------------------------

tictoc::tic() # timing

for (url in urls_to_scrape){
  # Define the full URL
  website_stub <- "https://www.presidency.ucsb.edu"
  url <- paste0(website_stub, url)
  
  cat("Fetching page:", x, "\n") # report progress
  cat(url, "\n\n")
  
  # Fetch the webpage
  page <- GET(url)
  
  # Parse the HTML content
  html_content <- read_html(content(page, "text"))
  
  # Define XPaths
  # extract metadata and actual text (body)
  xpaths <- list(
    speaker = "//div[contains(@class, 'field-title')]",
    date = "//div[contains(@class, 'field-docs-start-date-time')]",
    title = "//div[contains(@class, 'field-ds-doc-title')]",
    citation = "//div[contains(@class, 'field-prez-document-citation')]",
    body = "//div[contains(@class, 'field-docs-content')]" # body contains the actual text
  )
  
  # Create data row from xpath results
  data_row <- data.frame(
    url = url,
    date_scraped = today_date,
    speaker = extract_text(html_content, xpaths$speaker),
    date = extract_text(html_content, xpaths$date),
    title = extract_text(html_content, xpaths$title),
    citation = extract_text(html_content, xpaths$citation),
    body = extract_text(html_content, xpaths$body),
    stringsAsFactors = FALSE
  )
  
  data_row <- data_row %>% 
    mutate(nchars = nchar(body)) # for future calculation of percentages
  
  # Append row to dataframe
  speech_data <- rbind(speech_data, data_row)
  
  # Save every `save_interval` pages
  if (x %% save_interval == 0) {
    
    speech_data <- speech_data %>%
      mutate(body = str_replace_all(body, "[\r\n]", " ")) # delete some annoying characters
    
    saveRDS(speech_data, file = output_file) # update progress
    cat("---Progress saved at", x, "pages\n")
  }
  
  # increase counter
  x <- x + 1  
  Sys.sleep(1)  # Prevent rate limiting
}


tictoc::toc() # end timer

# saving data -----------------------------------------------------------------

# save our scraped webpages and text
speech_data %>%
  saveRDS(file = here("data/APP_UCSB/scraped_campaigndocs.rds"))

# for excel ( since excel has a max number of characters that can exist in one cell)
# aka we have to truncate texts, which is annoying
# but we dont use csv later on, this is just for visual scanning by us


# speech_data_truncated <- speech_data %>%
#   #mutate(body = str_replace_all(body, "[:punct:]", " ")) %>%
#   mutate(body_1 = str_sub(body, 1, 30000),  # First 30,000 characters
#          body_2 = str_sub(body, 30001, 60000),
#          body_3 = str_sub(body, 60001, 90000),
#          body_4 = str_sub(body, 90001, nchar(body))) %>%
#   select(-body) %>%
#   write_csv(here("data/APP_UCSB/scraped_campaigndocs_truncated.csv"), quote = "all")


cat("Final dataset saved \n")

