# Load necessary packages
library(rvest)
library(dplyr)

# Define school IDs
school_ids <- c(
  "Augustana IL Vikings" = "40",
  "Carthage Firebirds" = "121",
  "North Central Cardinals" = "492",
  "Millikin Big Blue" = "424",
  "Illinois Wesleyan Titans" = "300",
  "North Park Vikings" = "496",
  "Carroll University Pioneers" = "120",
  "Wheaton Thunder" = "778",
  "Elmhurst BlueJays" = "212",
  "WashU Bears" = "755"
)

# Function to fetch team data by school ID or name
fetch_team_data_by_id_or_name <- function(input, year_ids) {
  # Initialize list to store data
  all_tables_data <- list()
  
  # Determine if input is school ID or name
  if (is.numeric(input)) {
    school_id <- as.character(input)
  } else if (is.character(input) && input %in% names(school_ids)) {
    school_id <- school_ids[[input]]
  } else {
    stop("Invalid input. Please provide a valid school ID or name.")
  }
  
  # Loop through each year ID
  for (year_id in year_ids) {
    url <- paste0("https://stats.ncaa.org/team/", school_id, "/stats?id=16460&year_stat_category_id=", year_id)
    page <- tryCatch({
      read_html(url)
    }, error = function(e) return(NULL))
    
    if (!is.null(page)) {
      tables <- page %>%
        html_nodes("table") %>%
        html_table(fill = TRUE)
      if (length(tables) > 0) {
        all_tables_data[[as.character(year_id)]] <- tables
      } else {
        all_tables_data[[as.character(year_id)]] <- "No data found for this year."
      }
    } else {
      all_tables_data[[as.character(year_id)]] <- "Failed to load data for this year."
    }
  }
  
  return(list(SchoolName = input, Data = all_tables_data))
}

# User interaction
input <- readline(prompt="Enter school name or ID: ")
year_ids_input <- readline(prompt="Enter year IDs range (e.g., 15040:15059): ")
year_ids <- as.numeric(unlist(strsplit(year_ids_input, ":")))

school_data <- fetch_team_data_by_id_or_name(input, year_ids)

print(school_data$SchoolName)
for (year in names(school_data$Data)) {
  print(paste("Year:", year))
  print(school_data$Data[[year]])
}
