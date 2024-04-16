# Load necessary packages
library(rvest)
library(httr)

rm(list = ls())

roster_website_url <- "https://athletics.augustana.edu/sports/football/roster?view=2"

# Read HTML content of the webpage
webpage <- read_html(roster_website_url)

# Extract roster table
roster_tab <- webpage %>%
  html_nodes("table") %>%
  html_table(fill = TRUE)

# Convert from lists to data frames
df_roster <- as.data.frame(roster_tab[[3]])
coach_df <- as.data.frame(roster_tab[[4]])
#coach_df without NA columns for images
subset_df <- coach_df[, c(2, 3)]


# # URL of the player's profile page
# url <- "https://athletics.augustana.edu/sports/football/roster/john-bolton/10758"
# 
# # Read HTML content of the player's profile page
# webpage <- read_html(url)
# 
# # Extract the image URL using XPath selector
# image_url <- webpage %>%
#   html_node(xpath = "//img[contains(@data-airgap-id, '45')]") %>%
#   html_attr("src")
# 
# # Print the image URL
# print(image_url)

#Matt said pictures of all the players in the roster would be nice to have
#I could not figure out how to do this








schedule_results_url <- "https://stats.ncaa.org/teams/558188"

# Read HTML content of the webpage
webpage <- read_html(schedule_results_url)

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



# URL of the roster page
basic_roster_url <- "https://stats.ncaa.org/team/40/roster/16460"

# Read HTML content of the webpage
webpage <- read_html(basic_roster_url)

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



team_stats_url_base <- "https://stats.ncaa.org/team/40/stats?id=16460&year_stat_category_id="

# List of year_stat_category_id values
year_stat_category_ids <- 15040:15058

# Initialize an empty list to store table data
all_tables_data <- list()

# Loop through each year_stat_category_id
for (year_stat_category_id in year_stat_category_ids) {
  # Construct URL with specific year_stat_category_id
  team_stats_url <- paste0(team_stats_url_base, year_stat_category_id)
  
  # Read HTML content of the webpage
  webpage <- read_html(team_stats_url)
  
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



game_by_game_url_base <- "https://stats.ncaa.org/player/index?id=16460&org_id=40&stats_player_seq=-100&year_stat_category_id="

# List of year_stat_category_id values
year_stat_category_ids <- 15040:15057

# Initialize an empty list to store table data
all_tables_data <- list()

# Loop through each year_stat_category_id
for (year_stat_category_id in year_stat_category_ids) {
  # Construct URL with specific year_stat_category_id
  game_by_game_url <- paste0(game_by_game_url_base, year_stat_category_id)
  
  # Read HTML content of the webpage
  webpage <- read_html(game_by_game_url)
  
  # Scrape table data
  tables <- webpage %>%
    html_nodes("table") %>%
    html_table(fill = TRUE)
  
  # Store table data in the list
  all_tables_data[[as.character(year_stat_category_id)]] <- tables
}

# Initialize an empty list to store the desired tables
desired_tables <- list()

# Loop through each URL variant
for (year_stat_category_id in names(all_tables_data)) {
  # Extract tables for the current year_stat_category_id
  tables <- all_tables_data[[year_stat_category_id]]
  
  # Extract the fifth table (assuming it's always the fifth table)
  table_5 <- tables[[5]]
  
  # Store the table in the desired_tables list
  desired_tables[[year_stat_category_id]] <- table_5
}

# Loop through each element (tables) in desired_tables
for (year_stat_category_id in names(desired_tables)) {
  # Extract the desired table for the current year_stat_category_id
  table <- desired_tables[[year_stat_category_id]]
  
  # Remove the first row
  table <- table[-1, ]
  
  # Extract the second row as column names
  col_names <- as.character(table[1, ])
  
  # Assign column names to the table
  colnames(table) <- col_names
  
  # Remove the second row (now column names)
  table <- table[-1, ]
  
  # Convert the table to a data frame
  df <- as.data.frame(table)
  
  # Assign the data frame to a variable
  assign(paste0("table_", year_stat_category_id), df)
}



# update url according to year and team
national_ranking_summary_url <- paste0("https://stats.ncaa.org/rankings/ranking_summary?academic_year=", 2024, "&division=", 3, "&org_id=", 40, "&sport_code=MFB")

# Read HTML content of the webpage
webpage <- read_html(national_ranking_summary_url)

# Scrape the third table from the webpage
tables <- html_nodes(webpage, "table")
natl_table <- html_table(tables[[3]], fill = TRUE)
natl_table_2 <- html_table(tables[[4]], fill = TRUE)

# update url according to year and team
conference_ranking_summary_url <- paste0("https://stats.ncaa.org/rankings/ranking_summary?academic_year=", 2024, "&division=", 3, "&conf_id=", 836, "&sport_code=MFB")

# Read HTML content of the webpage
webpage <- read_html(conference_ranking_summary_url)

# Scrape the third table from the webpage
tables <- html_nodes(webpage, "table")
conf_table <- html_table(tables[[2]], fill = TRUE)
conf_table_2 <- html_table(tables[[3]], fill = TRUE)

#two tables above need to be fixed before being loaded in 
#want team, national rank, and value split out by row
#should have 470 observations instead of 47




























