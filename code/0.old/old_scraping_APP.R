
# manual scarping, not automated ----------------------------------------------------------------------

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
here::i_am("code/1_scraping_APP.R")







# attempt 1, just one page -----------------------------------------------------------


xpath_speaker <- "//div[contains(@class, 'field-title')]" # name of speaker (returns two items, one of which is meta; kill it in future scrape)
xpath_date <- "//div[contains(@class, 'field-docs-start-date-time')]" # date (is a string, maybe u will want to extract the formatted version instead)
xpath_title <- "//div[contains(@class, 'field-ds-doc-title')]" # title of speech (like type of speech and city and state)
xpath_citation <- "//div[contains(@class, 'field-prez-document-citation')]" # academic citation
xpath_body <- "//div[contains(@class, 'field-docs-content')]" # full text of speech (including audience interactions etc.)




# Define the URL
url <- "https://www.presidency.ucsb.edu/documents/remarks-the-vice-president-political-event-west-allis-wisconsin"

url <- "https://www.presidency.ucsb.edu/documents/remarks-the-vice-president-campaign-event-flint-michigan"
# Fetch the webpage
page <- GET(url)

# Parse the HTML content
html_content <- read_html(content(page, "text"))

# Define XPaths
xpaths <- list(
  speaker = "//div[contains(@class, 'field-title')]",
  date = "//div[contains(@class, 'field-docs-start-date-time')]",
  title = "//div[contains(@class, 'field-ds-doc-title')]",
  citation = "//div[contains(@class, 'field-prez-document-citation')]",
  body = "//div[contains(@class, 'field-docs-content')]"
)

# Function to extract text using XPath
extract_text <- function(xpath) {
  elements <- html_content %>%
    html_elements(xpath = xpath) %>%
    html_text(trim = TRUE)
  
  if (length(elements) > 0) {
    return(elements[1])  # Extract first valid result
  } else {
    return(NA)  # Handle missing data gracefully
  }
}

# Extract data and store in a data frame
temp <- data.frame(
  speaker = extract_text(xpaths$speaker),  # Removing unwanted metadata
  date = extract_text(xpaths$date),  
  title = extract_text(xpaths$title),
  citation = extract_text(xpaths$citation),
  body = extract_text(xpaths$body),
  stringsAsFactors = FALSE  # Prevents automatic factor conversion
)

# Print the structured data
print(speech_data)



# attempt 2, non-elegant batch scrape -------------------------------------------------


# Function to extract text using XPath
extract_text <- function(html_content, xpath) {
  elements <- html_content %>%
    html_elements(xpath = xpath) %>%
    html_text(trim = TRUE)
  
  if (length(elements) > 0) {
    return(elements[1])  # Extract first valid result
  } else {
    return(NA)  # Handle missing data
  }
}


# Initialize an empty data frame
speech_data <- data.frame(
  url = character(),
  speaker = character(),
  date = character(),
  title = character(),
  citation = character(),
  body = character(),
  stringsAsFactors = FALSE
)

urls <- c(
  "https://www.presidency.ucsb.edu/documents/remarks-the-vice-president-political-event-west-allis-wisconsin",
  "https://www.presidency.ucsb.edu/documents/remarks-the-vice-president-campaign-event-atlanta-georgia"
)

for (url in urls){
  # Define the URL
  url <- url
  
  cat("fetching page: ", url, "\n")
  
  # Fetch the webpage
  page <- GET(url)
  
  # Parse the HTML content
  html_content <- read_html(content(page, "text"))
  
  # Define XPaths
  xpaths <- list(
    speaker = "//div[contains(@class, 'field-title')]",
    date = "//div[contains(@class, 'field-docs-start-date-time')]",
    title = "//div[contains(@class, 'field-ds-doc-title')]",
    citation = "//div[contains(@class, 'field-prez-document-citation')]",
    body = "//div[contains(@class, 'field-docs-content')]"
  )
  
  # create data row from xpath results
  data_row <- data.frame(
    url = url,
    speaker = extract_text(html_content, xpaths$speaker),
    date = extract_text(html_content, xpaths$date),
    title = extract_text(html_content, xpaths$title),
    citation = extract_text(html_content, xpaths$citation),
    body = extract_text(html_content, xpaths$body),
    stringsAsFactors = FALSE
  )
  
  # append row to running df
  speech_data <- rbind(speech_data, data_row)
  
  # system messages and sleep
  cat("row appended, sleeping...","\n")
  Sys.sleep(2)
  
}


