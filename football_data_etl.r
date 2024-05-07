# Load necessary packages
library(rvest)
library(httr)
library(tidyr)
library(dplyr)
library(stringr)
library(openxlsx)
library(data.table)

rm(list = ls())

# set your working directory to where you want the data tables to be loaded into
# located in the settings gear

# MUST READ
# Since this code does not accept/require inputs, the urls will need to be manually updated
# Any 16460 value in a url should be replaced with 16680 for 2024-25
# The year value in the national_ranking_summary_url will need to be changed from 2024 to 2025
# All url objects are preceded by a 3 line break

# This code can be ran all at one time



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
coach_df <- coach_df[, c(2, 3)]

# Load existing Excel file
wb <- loadWorkbook("AugieData.xlsx")

# Clear all data in the sheet - run the code below if writing to same the workbook as previous; otherwise comment (#) the line below
removeWorksheet(wb, "Roster")
addWorksheet(wb, "Roster")

# Write the data frames to the Excel file and assign columns for iterability
writeData(wb, sheet = "Roster", x = df_roster, startRow = 1, startCol = 1)
writeData(wb, sheet = "Roster", x = coach_df, startRow = 1, startCol = 11)

# Save the modified Excel file
saveWorkbook(wb, "AugieData.xlsx", overwrite = TRUE)



# URL for first page of team on ncaa stats site
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

# Clear all data in the sheet - run the code below if writing to the same workbook as previous; otherwise comment (#) the line below
removeWorksheet(wb, "ScheduleResults")
addWorksheet(wb, "ScheduleResults")

writeData(wb, sheet = "ScheduleResults", x = schedule_results, startRow = 1, startCol = 1)
writeData(wb, sheet = "ScheduleResults", x = team_stats, startRow = 1, startCol = 6)
writeData(wb, sheet = "ScheduleResults", x = individual_stat_leaders, startRow = 1, startCol = 10)

# Save the modified Excel file
saveWorkbook(wb, "AugieData.xlsx", overwrite = TRUE)



# URL of the roster page
basic_roster_url <- "https://stats.ncaa.org/team/40/roster/16460"

# Read HTML content of the webpage
webpage <- read_html(basic_roster_url)

# Scrape roster table data
roster_table <- webpage %>%
  html_nodes("table") %>%
  html_table(fill = TRUE)

# Extract the roster data frame (assuming it's the first table)
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
  
  df[[2]] <- as.numeric(df[[2]])
  
  # Assign the dataframe to a name
  assign(paste0("table_", i), df)
}

# Rename the data frames for ease of loading into Excel
rushing_team_stats <- table_1
first_downs_team_stats <- table_2
passing_team_stats <- table_3
receiving_team_stats <- table_4
total_offense_team_stats <- table_5
all_purpose_yards_team_stats <- table_6
scoring_team_stats <- table_7
sacks_team_stats <- table_8
tackles_team_stats <- table_9
passes_defended_team_stats <- table_10
fumbles_team_stats <- table_11
kicking_team_stats <- table_12
punting_team_stats <- table_13
punt_returns_team_stats <- table_14
ko_and_ko_return_team_stats <- table_15
redzone_team_stats <- table_16
defense_team_stats <- table_17
turnover_margin_team_stats <- table_18
participation_team_stats <- table_19

# Replace NA values with blanks in each data frame to prevent errors when loading data into excel
rushing_team_stats <- rushing_team_stats %>%
  mutate_all(~ ifelse(is.na(.), "", .))
first_downs_team_stats <- first_downs_team_stats %>%
  mutate_all(~ ifelse(is.na(.), "", .))
passing_team_stats <- passing_team_stats %>%
  mutate_all(~ ifelse(is.na(.), "", .))
receiving_team_stats <- receiving_team_stats %>%
  mutate_all(~ ifelse(is.na(.), "", .))
total_offense_team_stats <- total_offense_team_stats %>%
  mutate_all(~ ifelse(is.na(.), "", .))
all_purpose_yards_team_stats <- all_purpose_yards_team_stats %>%
  mutate_all(~ ifelse(is.na(.), "", .))
scoring_team_stats <- scoring_team_stats %>%
  mutate_all(~ ifelse(is.na(.), "", .))
sacks_team_stats <- sacks_team_stats %>%
  mutate_all(~ ifelse(is.na(.), "", .))
tackles_team_stats <- tackles_team_stats %>%
  mutate_all(~ ifelse(is.na(.), "", .))
passes_defended_team_stats <- passes_defended_team_stats %>%
  mutate_all(~ ifelse(is.na(.), "", .))
fumbles_team_stats <- fumbles_team_stats %>%
  mutate_all(~ ifelse(is.na(.), "", .))
kicking_team_stats <- kicking_team_stats %>%
  mutate_all(~ ifelse(is.na(.), "", .))
punting_team_stats <- punting_team_stats %>%
  mutate_all(~ ifelse(is.na(.), "", .))
punt_returns_team_stats <- punt_returns_team_stats %>%
  mutate_all(~ ifelse(is.na(.), "", .))
ko_and_ko_return_team_stats <- ko_and_ko_return_team_stats %>%
  mutate_all(~ ifelse(is.na(.), "", .))
redzone_team_stats <- redzone_team_stats %>%
  mutate_all(~ ifelse(is.na(.), "", .))
defense_team_stats <- defense_team_stats %>%
  mutate_all(~ ifelse(is.na(.), "", .))
turnover_margin_team_stats <- turnover_margin_team_stats %>%
  mutate_all(~ ifelse(is.na(.), "", .))
participation_team_stats <- participation_team_stats %>%
  mutate_all(~ ifelse(is.na(.), "", .))

# Define a function to convert values to numeric and remove commas
convert_to_numeric <- function(x) {
  as.numeric(gsub(",", "", ifelse(is.na(x), "", x)))
}

# Apply the function above to team_stats data frames to ensure Excel lookup formulas work properly
columns_to_convert <- c(2, 7:ncol(rushing_team_stats))
rushing_team_stats[columns_to_convert] <- lapply(rushing_team_stats[columns_to_convert], convert_to_numeric)
columns_to_convert <- c(2, 7:ncol(first_downs_team_stats))
first_downs_team_stats[columns_to_convert] <- lapply(first_downs_team_stats[columns_to_convert], convert_to_numeric)
columns_to_convert <- c(2, 7:ncol(passing_team_stats))
passing_team_stats[columns_to_convert] <- lapply(passing_team_stats[columns_to_convert], convert_to_numeric)
columns_to_convert <- c(2, 7:ncol(receiving_team_stats))
receiving_team_stats[columns_to_convert] <- lapply(receiving_team_stats[columns_to_convert], convert_to_numeric)
columns_to_convert <- c(2, 7:ncol(total_offense_team_stats))
total_offense_team_stats[columns_to_convert] <- lapply(total_offense_team_stats[columns_to_convert], convert_to_numeric)
columns_to_convert <- c(2, 7:ncol(all_purpose_yards_team_stats))
all_purpose_yards_team_stats[columns_to_convert] <- lapply(all_purpose_yards_team_stats[columns_to_convert], convert_to_numeric)
columns_to_convert <- c(2, 7:ncol(scoring_team_stats))
scoring_team_stats[columns_to_convert] <- lapply(scoring_team_stats[columns_to_convert], convert_to_numeric)
columns_to_convert <- c(2, 7:ncol(sacks_team_stats))
sacks_team_stats[columns_to_convert] <- lapply(sacks_team_stats[columns_to_convert], convert_to_numeric)
columns_to_convert <- c(2, 7:ncol(tackles_team_stats))
tackles_team_stats[columns_to_convert] <- lapply(tackles_team_stats[columns_to_convert], convert_to_numeric)
columns_to_convert <- c(2, 7:ncol(passes_defended_team_stats))
passes_defended_team_stats[columns_to_convert] <- lapply(passes_defended_team_stats[columns_to_convert], convert_to_numeric)
columns_to_convert <- c(2, 7:ncol(fumbles_team_stats))
fumbles_team_stats[columns_to_convert] <- lapply(fumbles_team_stats[columns_to_convert], convert_to_numeric)
columns_to_convert <- c(2, 7:ncol(kicking_team_stats))
kicking_team_stats[columns_to_convert] <- lapply(kicking_team_stats[columns_to_convert], convert_to_numeric)
columns_to_convert <- c(2, 7:ncol(punting_team_stats))
punting_team_stats[columns_to_convert] <- lapply(punting_team_stats[columns_to_convert], convert_to_numeric)
columns_to_convert <- c(2, 7:ncol(punt_returns_team_stats))
punt_returns_team_stats[columns_to_convert] <- lapply(punt_returns_team_stats[columns_to_convert], convert_to_numeric)
columns_to_convert <- c(2, 7:ncol(ko_and_ko_return_team_stats))
ko_and_ko_return_team_stats[columns_to_convert] <- lapply(ko_and_ko_return_team_stats[columns_to_convert], convert_to_numeric)
columns_to_convert <- c(2, 7:ncol(redzone_team_stats))
redzone_team_stats[columns_to_convert] <- lapply(redzone_team_stats[columns_to_convert], convert_to_numeric)
columns_to_convert <- c(2, 7:ncol(defense_team_stats))
defense_team_stats[columns_to_convert] <- lapply(defense_team_stats[columns_to_convert], convert_to_numeric)
columns_to_convert <- c(2, 7:ncol(turnover_margin_team_stats))
turnover_margin_team_stats[columns_to_convert] <- lapply(turnover_margin_team_stats[columns_to_convert], convert_to_numeric)
columns_to_convert <- c(2, 7:ncol(participation_team_stats))
participation_team_stats[columns_to_convert] <- lapply(participation_team_stats[columns_to_convert], convert_to_numeric)

# Clear all data in the sheet - run the code below if writing to same the workbook as previous; otherwise comment (#) the line below
removeWorksheet(wb, "TeamStats")
addWorksheet(wb, "TeamStats")

writeData(wb, sheet = "TeamStats", x = rushing_team_stats, startRow = 1, startCol = 1)
writeData(wb, sheet = "TeamStats", x = first_downs_team_stats, startRow = 1, startCol = 19)
writeData(wb, sheet = "TeamStats", x = passing_team_stats, startRow = 1, startCol = 32)
writeData(wb, sheet = "TeamStats", x = receiving_team_stats, startRow = 1, startCol = 52)
writeData(wb, sheet = "TeamStats", x = total_offense_team_stats, startRow = 1, startCol = 69)
writeData(wb, sheet = "TeamStats", x = all_purpose_yards_team_stats, startRow = 1, startCol = 96)
writeData(wb, sheet = "TeamStats", x = scoring_team_stats, startRow = 1, startCol = 115)
writeData(wb, sheet = "TeamStats", x = sacks_team_stats, startRow = 1, startCol = 139)
writeData(wb, sheet = "TeamStats", x = tackles_team_stats, startRow = 1, startCol = 153)
writeData(wb, sheet = "TeamStats", x = passes_defended_team_stats, startRow = 1, startCol = 169)
writeData(wb, sheet = "TeamStats", x = fumbles_team_stats, startRow = 1, startCol = 183)
writeData(wb, sheet = "TeamStats", x = kicking_team_stats, startRow = 1, startCol = 197)
writeData(wb, sheet = "TeamStats", x = punting_team_stats, startRow = 1, startCol = 223)
writeData(wb, sheet = "TeamStats", x = punt_returns_team_stats, startRow = 1, startCol = 239)
writeData(wb, sheet = "TeamStats", x = ko_and_ko_return_team_stats, startRow = 1, startCol = 252)
writeData(wb, sheet = "TeamStats", x = redzone_team_stats, startRow = 1, startCol = 269)
writeData(wb, sheet = "TeamStats", x = defense_team_stats, startRow = 1, startCol = 291)
writeData(wb, sheet = "TeamStats", x = turnover_margin_team_stats, startRow = 1, startCol = 304)
writeData(wb, sheet = "TeamStats", x = participation_team_stats, startRow = 1, startCol = 317)

# Save the modified Excel file
saveWorkbook(wb, "AugieData.xlsx", overwrite = TRUE)



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

# Rename the data frames
rushing_game_stats <- table_15040
first_downs_game_stats <- table_15041
passing_game_stats <- table_15042
receiving_game_stats <- table_15043
total_offense_game_stats <- table_15044
all_purpose_yards_game_stats <- table_15045
scoring_game_stats <- table_15046
sacks_game_stats <- table_15047
tackles_game_stats <- table_15048
passes_defended_game_stats <- table_15049
fumbles_game_stats <- table_15050
kicking_game_stats <- table_15051
punting_game_stats <- table_15052
punt_returns_game_stats <- table_15053
ko_and_ko_return_game_stats <- table_15054
redzone_game_stats <- table_15055
defense_game_stats <- table_15056
turnover_margin_game_stats <- table_15057

# Removing rows with NA values that were disrupting loading process
rushing_game_stats <- rushing_game_stats %>%
  mutate_all(~ ifelse(is.na(.), "", .))
first_downs_game_stats <- first_downs_game_stats %>%
  mutate_all(~ ifelse(is.na(.), "", .))
passing_game_stats <- passing_game_stats %>%
  mutate_all(~ ifelse(is.na(.), "", .))
receiving_game_stats <- receiving_game_stats %>%
  mutate_all(~ ifelse(is.na(.), "", .))
total_offense_game_stats <- total_offense_game_stats %>%
  mutate_all(~ ifelse(is.na(.), "", .))
all_purpose_yards_game_stats <- all_purpose_yards_game_stats %>%
  mutate_all(~ ifelse(is.na(.), "", .))
scoring_game_stats <- scoring_game_stats[, -ncol(scoring_game_stats)]
scoring_game_stats <- scoring_game_stats %>%
  mutate_all(~ ifelse(is.na(.), "", .))
sacks_game_stats <- sacks_game_stats %>%
  mutate_all(~ ifelse(is.na(.), "", .))
tackles_game_stats <- tackles_game_stats %>%
  mutate_all(~ ifelse(is.na(.), "", .))
passes_defended_game_stats <- passes_defended_game_stats %>%
  mutate_all(~ ifelse(is.na(.), "", .))
fumbles_game_stats <- fumbles_game_stats %>%
  mutate_all(~ ifelse(is.na(.), "", .))
kicking_game_stats <- kicking_game_stats[, 1:8]
kicking_game_stats <- kicking_game_stats %>%
  mutate_all(~ ifelse(is.na(.), "", .))
punting_game_stats <- punting_game_stats %>%
  mutate_all(~ ifelse(is.na(.), "", .))
punt_returns_game_stats <- punt_returns_game_stats %>%
  mutate_all(~ ifelse(is.na(.), "", .))
ko_and_ko_return_game_stats <- ko_and_ko_return_game_stats %>%
  mutate_all(~ ifelse(is.na(.), "", .))
redzone_game_stats <- redzone_game_stats %>%
  mutate_all(~ ifelse(is.na(.), "", .))
defense_game_stats <- defense_game_stats %>%
  mutate_all(~ ifelse(is.na(.), "", .))
turnover_margin_game_stats <- turnover_margin_game_stats %>%
  mutate_all(~ ifelse(is.na(.), "", .))


# Clear all data in the sheet - run the code below if writing to same the workbook as previous; otherwise comment (#) the line below
removeWorksheet(wb, "GameStats")
addWorksheet(wb, "GameStats")

writeData(wb, sheet = "GameStats", x = rushing_game_stats, startRow = 1, startCol = 1)
writeData(wb, sheet = "GameStats", x = first_downs_game_stats, startRow = 1, startCol = 14)
writeData(wb, sheet = "GameStats", x = passing_game_stats, startRow = 1, startCol = 22)
writeData(wb, sheet = "GameStats", x = receiving_game_stats, startRow = 1, startCol = 37)
writeData(wb, sheet = "GameStats", x = total_offense_game_stats, startRow = 1, startCol = 49)
writeData(wb, sheet = "GameStats", x = all_purpose_yards_game_stats, startRow = 1, startCol = 71)
writeData(wb, sheet = "GameStats", x = scoring_game_stats, startRow = 1, startCol = 85)
writeData(wb, sheet = "GameStats", x = sacks_game_stats, startRow = 1, startCol = 103)
writeData(wb, sheet = "GameStats", x = tackles_game_stats, startRow = 1, startCol = 112)
writeData(wb, sheet = "GameStats", x = passes_defended_game_stats, startRow = 1, startCol = 123)
writeData(wb, sheet = "GameStats", x = fumbles_game_stats, startRow = 1, startCol = 132)
writeData(wb, sheet = "GameStats", x = kicking_game_stats, startRow = 1, startCol = 141)
writeData(wb, sheet = "GameStats", x = punting_game_stats, startRow = 1, startCol = 150)
writeData(wb, sheet = "GameStats", x = punt_returns_game_stats, startRow = 1, startCol = 162)
writeData(wb, sheet = "GameStats", x = ko_and_ko_return_game_stats, startRow = 1, startCol = 170)
writeData(wb, sheet = "GameStats", x = redzone_game_stats, startRow = 1, startCol = 183)
writeData(wb, sheet = "GameStats", x = defense_game_stats, startRow = 1, startCol = 200)
writeData(wb, sheet = "GameStats", x = turnover_margin_game_stats, startRow = 1, startCol = 208)

# Save the modified Excel file
saveWorkbook(wb, "AugieData.xlsx", overwrite = TRUE)



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

# Rename the sixth column to "LeaderValue"
colnames(conf_table)[6] <- "LeaderValue"

# Create an empty data frame to store the transformed data
transformed_df <- data.frame(matrix(ncol = ncol(conf_table), nrow = nrow(conf_table) * 10), stringsAsFactors = FALSE)
colnames(transformed_df) <- colnames(conf_table)

# Duplicate all columns
for (col in colnames(conf_table)[-6]) {
  transformed_df[[col]] <- rep(conf_table[[col]], each = 10)
}

# Store LeaderValue column separately
leader_values <- rep(conf_table$LeaderValue, each = 10)

# Split the team names in column 2 and create new rows
team_list <- strsplit(conf_table$Team, "\n")

for (i in 1:nrow(conf_table)) {
  team_names <- team_list[[i]]
  team_count <- length(team_names)
  if (team_count > 10) {
    team_names <- team_names[1:10]  # Limit to first 10 elements
  } else if (team_count < 10) {
    team_names <- c(team_names, rep("", 10 - team_count))  # Fill with empty strings if less than 10
  }
  transformed_df[((i - 1) * 10 + 1):((i - 1) * 10 + 10), 2] <- trimws(team_names)
}

# Split the national ranks in column 3 and create new rows
national_rank_list <- strsplit(conf_table$NationalRank, "\n")

for (i in 1:nrow(conf_table)) {
  national_ranks <- national_rank_list[[i]]
  national_rank_count <- length(national_ranks)
  if (national_rank_count > 10) {
    national_ranks <- national_ranks[1:10]  # Limit to first 10 elements
  } else if (national_rank_count < 10) {
    national_ranks <- c(national_ranks, rep("", 10 - national_rank_count))  # Fill with empty strings if less than 10
  }
  transformed_df[((i - 1) * 10 + 1):((i - 1) * 10 + 10), 3] <- trimws(national_ranks)
}

# Split the values in column 4 and create new rows
value_list <- strsplit(conf_table$Value, "\n")

for (i in 1:nrow(conf_table)) {
  values <- value_list[[i]]
  value_count <- length(values)
  if (value_count > 10) {
    values <- values[1:10]  # Limit to first 10 elements
  } else if (value_count < 10) {
    values <- c(values, rep("", 10 - value_count))  # Fill with empty strings if less than 10
  }
  transformed_df[((i - 1) * 10 + 1):((i - 1) * 10 + 10), 4] <- trimws(values)
}

# Split the values in column 5 and only store the first element
national_leader_list <- lapply(strsplit(conf_table$'National Leader', "\n"), function(x) x[1])

# Create new rows
for (i in 1:nrow(conf_table)) {
  values <- national_leader_list[[i]]
  first_value <- values[1]  # Extract the first value
  value_count <- length(values)
  
  if (value_count < 10) {
    values <- c(rep(first_value, 10 - value_count), values)  # Fill with first value if less than 10
  } else {
    values <- values[1:10]  # Limit to first 10 elements
  }
  transformed_df[((i - 1) * 10 + 1):((i - 1) * 10 + 10), 5] <- trimws(values)
}

# Append LeaderValue column
transformed_df$LeaderValue <- trimws(leader_values)

# Revert the naming of the dataframe
conf_table <- transformed_df

# Rename the sixth column to "LeaderValue"
colnames(conf_table_2)[6] <- "LeaderValue"

# Create an empty data frame to store the transformed data
transformed_df_2 <- data.frame(matrix(ncol = ncol(conf_table_2), nrow = nrow(conf_table_2) * 10), stringsAsFactors = FALSE)
colnames(transformed_df_2) <- colnames(conf_table_2)

# Duplicate all columns
for (col in colnames(conf_table_2)[-6]) {
  transformed_df_2[[col]] <- rep(conf_table_2[[col]], each = 10)
}

# Store LeaderValue column separately
leader_values_2 <- rep(conf_table_2$LeaderValue, each = 10)

# Split the player names in column 2 and create new rows
player_list_2 <- strsplit(conf_table_2$Player, "\n")

for (i in 1:nrow(conf_table_2)) {
  player_names <- player_list_2[[i]]
  player_count <- length(player_names)
  if (player_count > 10) {
    player_names <- player_names[1:10]  # Limit to first 10 elements
  } else if (player_count < 10) {
    player_names <- c(player_names, rep("", 10 - player_count))  # Fill with empty strings if less than 10
  }
  transformed_df_2[((i - 1) * 10 + 1):((i - 1) * 10 + 10), 2] <- trimws(player_names)
}

# Split the national ranks in column 3 and create new rows
national_rank_list_2 <- strsplit(conf_table_2$NationalRank, "\n")

for (i in 1:nrow(conf_table_2)) {
  national_ranks <- national_rank_list_2[[i]]
  national_rank_count <- length(national_ranks)
  if (national_rank_count > 10) {
    national_ranks <- national_ranks[1:10]  # Limit to first 10 elements
  } else if (national_rank_count < 10) {
    national_ranks <- c(national_ranks, rep("", 10 - national_rank_count))  # Fill with empty strings if less than 10
  }
  transformed_df_2[((i - 1) * 10 + 1):((i - 1) * 10 + 10), 3] <- trimws(national_ranks)
}

# Split the values in column 4 and create new rows
value_list_2 <- strsplit(conf_table_2$Value, "\n")

for (i in 1:nrow(conf_table_2)) {
  values <- value_list_2[[i]]
  value_count <- length(values)
  if (value_count > 10) {
    values <- values[1:10]  # Limit to first 10 elements
  } else if (value_count < 10) {
    values <- c(values, rep("", 10 - value_count))  # Fill with empty strings if less than 10
  }
  transformed_df_2[((i - 1) * 10 + 1):((i - 1) * 10 + 10), 4] <- trimws(values)
}

# Split the values in column 5 and only store the first element
national_leader_list_2 <- lapply(strsplit(conf_table_2$'National Leader', "\n"), function(x) x[1])

# Create new rows
for (i in 1:nrow(conf_table_2)) {
  values <- national_leader_list_2[[i]]
  first_value <- values[1]  # Extract the first value
  value_count <- length(values)
  
  if (value_count < 10) {
    values <- c(rep(first_value, 10 - value_count), values)  # Fill with first value if less than 10
  } else {
    values <- values[1:10]  # Limit to first 10 elements
  }
  transformed_df_2[((i - 1) * 10 + 1):((i - 1) * 10 + 10), 5] <- trimws(values)
}

# Append LeaderValue column
transformed_df_2$LeaderValue <- trimws(leader_values_2)

# Revert the naming of the dataframe
conf_table_2 <- transformed_df_2

# Clear all data in the "Rankings" sheet - run the code below if writing to same the workbook as previous; otherwise comment (#) the line below
removeWorksheet(wb, "Rankings")
addWorksheet(wb, "Rankings")

writeData(wb, sheet = "Rankings", x = natl_table, startRow = 1, startCol = 1)
writeData(wb, sheet = "Rankings", x = natl_table_2, startRow = 1, startCol = 10)
writeData(wb, sheet = "Rankings", x = conf_table, startRow = 1, startCol = 20)
writeData(wb, sheet = "Rankings", x = conf_table_2, startRow = 1, startCol = 27)

# Save the modified Excel file
saveWorkbook(wb, "AugieData.xlsx", overwrite = TRUE)




























