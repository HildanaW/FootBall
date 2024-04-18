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
conference_ids <- list(
  "ASC" = "14825",
  "American Rivers" = "864",
  "CCC Football" = "889",
  "CCIW" = "836",
  "Centennial" = "9683",
  "DIII Independent" = "99020",
  "Eastern Collegiate Football Conference" = "30122",
  "Empire 8" = "863",
  "HCAC" = "9174",
  "Landmark" = "30101",
  "Liberty League" = "24197",
  "MASCAC" = "870",
  "MIAC" = "882",
  "MWC" = "23119",
  "Michigan Intercol. Ath. Assn." = "873",
  "Middle Atlantic" = "877",
  "NACC" = "30097",
  "NCAC" = "897",
  "NESCAC" = "12817",
  "NEWMAC" = "890",
  "NJAC" = "891",
  "NWC" = "25300",
  "OAC" = "901",
  "ODAC" = "903",
  "PAC" = "907",
  "SAA" = "30169",
  "SCIAC" = "910",
  "UMAC" = "30115",
  "USA South" = "842",
  "WIAC" = "1473"
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
  if (is.numeric(Conference_id)) {
    Conference_id <- as.character(Conference_id)
  } else if (is.character(inputConference) && inputConference %in% names(Conference_ids)) {
    Conference_id <- Conference_ids[[inputConference]]
  } else {
    stop("Invalid input. Please provide a valid Conference Name/ ID.")
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
  # Add conference ID to URL if provided
  if (!is.null(conference_id)) {
    url <- paste0(url, "&conference_id=", conference_id)
  }
  
  
  return(list(SchoolName = input, Data = all_tables_data))
}

# User interaction
input <- readline(prompt="Enter school name or ID: ")
year_ids_input <- readline(prompt="Enter year IDs range (e.g., 15040:15059): ")
year_ids <- as.numeric(unlist(strsplit(year_ids_input, ":")))
Conference_id <- readline(prompt="Enter conference ID (optional, press Enter to skip): ")



school_data <- fetch_team_data_by_id_or_name(input, year_ids, Conference_id)
print(school_data$SchoolName)
for (year in names(school_data$Data)) {
  print(paste("Year:", year))
  print(school_data$Data[[year]])
}
