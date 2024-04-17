# Load necessary packages
library(rvest)
library(dplyr)
rm(list = ls())



#for each team

url_base <- "https://stats.ncaa.org/team/40/stats?id=16460&year_stat_category_id="
year_id = 15040:15059
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

all_tables_data <- list()

fetch_team_data_by_id_or_name <- function(input, year_ids) {
  if (is.numeric(input)) {
    school_id <- as.character(input)
    school_name <- names(school_ids)[school_ids == school_id]
    if (length(school_name) == 0) {
      stop("No school found with the given ID.")
    }
  } else {
    school_id <- school_ids[input]
    school_name <- input
    if (is.null(school_id)) {
      stop("No school found with the given name.")
    }
  }
  
  all_tables_data <- list()
  
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
  
  return(list(SchoolName = school_name, Data = all_tables_data))
}

# User interaction
input <- readline(prompt="Enter school name or ID: ")


# Fetch data
school_data <- fetch_team_data_by_id_or_name(input, year_ids)

# Output data
print(school_data$SchoolName)
for (year in names(school_data$Data)) {
  print(paste("Year:", year))
  print(school_data$Data[[year]])
}

###### Cod below is from version 1


# Initialize an empty list to store table data
all_tables_data <- list()

# Loop through each year_stat_category_id
for (year_stat_category_id in year_stat_category_ids) {
  # Construct URL with specific year_stat_category_id
  url <- paste0(url_base, year_stat_category_id)
  
  # Read HTML content of the webpage
  webpage <- read_html(url)
  
  # Scrape table data
  tables <- webpage %>%
    html_nodes("table") %>%
    html_table(fill = TRUE)
  
  # Process table data to handle blank values
  for (i in 1:length(tables)) {
    for (j in 1:length(tables[[i]])) {
      tables[[i]][[j]][tables[[i]][[j]] == ""] <- NA
    }
  }
  
  # Store table data in the list
  all_tables_data[[as.character(year_stat_category_id)]] <- tables
}

# Naming the data frames, converting them, and removing the first 21 columns
for (i in 1:length(all_tables_data)) {
  # Convert to dataframe
  df <- as.data.frame(all_tables_data[[i]])
  
  # Remove the first 21 columns if there are more than 21 columns
  if (ncol(df) > 21) {
    df <- df[, -(1:21)]
  }
  
  # Assign the dataframe to a name
  assign(paste0("table_", i), df)
}




# URL of the roster page
url_2 <- "https://stats.ncaa.org/team/40/roster/16460"

# Read HTML content of the webpage
webpage <- read_html(url_2)

# Scrape roster table data
roster_table <- webpage %>%
  html_nodes("table") %>%
  html_table(fill = TRUE)

# Extract the roster dataframe (assuming it's the first table)
roster_df <- as.data.frame(roster_table[[1]])

# Remove rows containing only "Roster"
roster_df <- roster_df[rowSums(roster_df == "Roster") != ncol(roster_df), ]

# Remove "Roster" from column names
colnames(roster_df) <- gsub("Roster\\.", "", colnames(roster_df))

# Set column names to values from the first row
colnames(roster_df) <- roster_df[1, ]

# Remove the first row
roster_df <- roster_df[-1, ]











#delete below when all data is scraped
#iterations to clean output from above code below

url_base <- "https://stats.ncaa.org/team/40/stats?id=16460&year_stat_category_id="

# List of year_stat_category_id values
year_stat_category_ids <- 15040:15058

# Initialize an empty list to store table data
all_tables_data <- list()

# Loop through each year_stat_category_id
for (year_stat_category_id in year_stat_category_ids) {
  # Construct URL with specific year_stat_category_id
  url <- paste0(url_base, year_stat_category_id)
  
  # Read HTML content of the webpage
  webpage <- read_html(url)
  
  # Scrape table data
  tables <- webpage %>%
    html_nodes("table") %>%
    html_table(fill = TRUE)
  
  # Process table data to handle blank values
  for (i in 1:length(tables)) {
    for (j in 1:length(tables[[i]])) {
      tables[[i]][[j]][tables[[i]][[j]] == ""] <- NA
    }
  }
  
  # Naming the data frames and converting them
  for (i in 1:length(tables)) {
    # Convert table to data frame
    table_df <- as.data.frame(tables[[i]])
    
    # Remove the first 21 columns if there are more than 21 columns
    if (ncol(table_df) > 21) {
      table_df <- table_df[, -(1:21)]
    }
    
    # Store data frame in the list
    all_tables_data[[paste0("table_", year_stat_category_id, "_", i)]] <- table_df
    
    # Naming the data frames and converting them
    for (i in 1:length(all_tables_data)) {
      assign(paste0("table_", year_stat_category_id, "_", i), as.data.frame(all_tables_data[[i]]))
    }
    
    }
}


url_base <- "https://stats.ncaa.org/team/40/stats?id=16460&year_stat_category_id="
year_stat_category_ids <- 15040:15058
all_tables_data <- list()

for (year_stat_category_id in year_stat_category_ids) {
  url <- paste0(url_base, year_stat_category_id)
  webpage <- read_html(url)
  
  tables <- webpage %>%
    html_nodes("table") %>%
    html_table(fill = TRUE)
  
  # Remove the first 21 columns if they exist
  if (ncol(combined_table) > 21) {
    combined_table <- combined_table[, -(1:21)]
  }
  
  # Convert to dataframe and store in all_tables_data if it has at least 85 rows
  if (nrow(combined_table) >= 85) {
    all_tables_data[[as.character(year_stat_category_id)]] <- as.data.frame(combined_table)
  }
}

