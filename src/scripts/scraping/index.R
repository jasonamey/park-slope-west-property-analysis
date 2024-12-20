# Code extracts all the links to the .zip files from the NYC tax assessment archive data
# HTML was copy/pasted and saved locally as page is generated with javascript

library(httr2)
library(rvest)

html_file_path <- "final_project/src/scripts/scraping/dept_of_finance_tax_assessment_archives.html"

base_url <- "https://www.nyc.gov"

webpage <- read_html(html_file_path)
# Extract all <a> tags
links <- webpage |>
  html_elements("a") |>
  html_attr("href")

# Append the base URL for relative links
full_urls <- ifelse(grepl("^/", links), paste0(base_url, links), links)

# Filter for .zip files and for those related to /tar/tc or 2023/2024
full_urls <- full_urls[grepl("\\.zip$", full_urls)] # Only .zip files
full_urls <- full_urls[grepl("tar/tc|2023|2024", full_urls)] # Match files for 2023 and 2024

# Create a data frame to store the results
url_data <- data.frame(
  full_urls
)

# Define the output file path and save the data
output_csv_path <- "final_project/data/tax_class_urls_t.csv"
write.csv(url_data, file = output_csv_path, row.names = FALSE)
