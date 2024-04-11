# Load necessary packages
library(rvest)

rm(list = ls())






# Keeps getting the following error code: 
  # Error in (function (..., row.names = NULL, check.rows = FALSE, check.names = TRUE,  : 
  # arguments imply differing number of rows: 1, 37, 36, 25, 24

# This code is for the Game by Game section of Augie football stats
  # Come back and resolve errors if this data is deemed necessary



# # URL of the webpage
# url_base_2 <- "https://stats.ncaa.org/player/index?id=16460&org_id=40&stats_player_seq=-100&year_stat_category_id="
# 
# # List of year_stat_category_id values
# year_stat_category_ids_2 <- 15040:15058
# 
# # Initialize an empty list to store table data
# all_tables_data_2 <- list()
# 
# # Loop through each year_stat_category_id
# for (year_stat_category_id_2 in year_stat_category_ids_2) {
#   # Construct URL with specific year_stat_category_id
#   url_4 <- paste0(url_base_2, year_stat_category_id_2)
#   
#   # Read HTML content of the webpage
#   webpage <- read_html(url_4)
#   
#   # Scrape table data
#   tables_2 <- webpage %>%
#     html_nodes("table") %>%
#     html_table(fill = TRUE)
#   
#   # Process table data to handle blank values
#   for (i in 1:length(tables_2)) {
#     for (j in 1:length(tables_2[[i]])) {
#       tables_2[[i]][[j]][tables_2[[i]][[j]] == ""] <- NA
#     }
#   }
#   
#   # Store table data in the list
#   all_tables_data_2[[as.character(year_stat_category_id_2)]] <- tables_2
# }
# 
# # Naming the data frames, converting them, and removing the first 21 columns
# for (i in 1:length(all_tables_data_2)) {
#   # Convert to dataframe
#   # Extract the final 11 observations from each data frame
#   df_2 <- tail(all_tables_data_2[[i]], 11)
#   
#   # Convert to dataframe
#   df_2 <- as.data.frame(df_2)
#   
#   # Assign the dataframe to a name
#   assign(paste0("grid_", i), df_2)
# }








url_3 <- "https://stats.ncaa.org/teams/558188"

# Read HTML content of the webpage
webpage <- read_html(url_3)

# Scrape data from each table
tables <- webpage %>%
  html_nodes("table") %>%
  html_table(fill = TRUE)

# Process and display data from each table
for (i in 1:length(tables)) {
  # Extract dataframe from the ith table
  df <- as.data.frame(tables[[i]])
  assign(paste0("chart_", i), df)
}

# Remove the first and second rows
chart_3 <- chart_3[-c(1), ]
chart_4 <- chart_4[-c(1), ]

# Set column names of chart_3 and chart_4 using row 1
colnames(chart_3) <- unlist(chart_3[1, ])
colnames(chart_4) <- unlist(chart_4[1, ])

# Remove the first row
team_stats <- chart_3[-1, ]
individual_stat_leaders <- chart_4[-1, ]

schedule_results <- chart_2[c(TRUE, FALSE), ]




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