# Load necessary packages
library(rvest)
library(httr)
library(tidyr)
library(dplyr)
library(stringr)
library(openxlsx)

rm(list = ls())

# set your working directory to the file path where you want the data tables to be loaded into
# located in the settings gear

# MUST READ
# For the lines below (through team_id_input), you need to run them one at a time and enter the desired values for each in the console area below
# Make sure you click in the console interface before typing
# Once you have entered your value and hit enter, these values will be stored as objects in the environment area (top right window)
# For context on what values to enter, see comments above and below (text following the #'s) the next seven lines of code
# The rest of the script begins on line 120 - can run the whole script from there on



# For 2024-25 season, enter 2025
year_input <- readline("Enter year: ")

# Value will almost always be 3
division_input <- readline("Enter division number: ")

# This is included so that the conference ranking tables populate correctly if Augustana plays a team outside of the CCIW
# Tables may not work properly if there are not 10 teams in the opposing team's conference
# In that case, try replacing all 10 values in relevant code with the proper number of teams in a given conference
conference_input <- readline("Enter conference code: ")
# conference codes
# "ASC" = "14825",
# "American Rivers" = "864",
# "CCC Football" = "889",
# "CCIW" = "836",
# "Centennial" = "9683",
# "DIII Independent" = "99020",
# "Eastern Collegiate Football Conference" = "30122",
# "Empire 8" = "863",
# "HCAC" = "9174",
# "Landmark" = "30101",
# "Liberty League" = "24197",
# "MASCAC" = "870",
# "MIAC" = "882",
# "MWC" = "23119",
# "Michigan Intercol. Ath. Assn." = "873",
# "Middle Atlantic" = "877",
# "NACC" = "30097",
# "NCAC" = "897",
# "NESCAC" = "12817",
# "NEWMAC" = "890",
# "NJAC" = "891",
# "NWC" = "25300",
# "OAC" = "901",
# "ODAC" = "903",
# "PAC" = "907",
# "SAA" = "30169",
# "SCIAC" = "910",
# "UMAC" = "30115",
# "USA South" = "842",
# "WIAC" = "1473"

# Prompt the user to enter input
roster_website_url <- readline("Enter team's roster url: ")
# conference roster urls
# "Augustana IL Vikings" = https://athletics.augustana.edu/sports/football/roster
# "Carthage Firebirds" = https://athletics.carthage.edu/sports/football/roster
# "North Central Cardinals" = https://northcentralcardinals.com/sports/football/roster
# "Millikin Big Blue" = https://northcentralcardinals.com/sports/football/roster
# "Illinois Wesleyan Titans" = https://www.iwusports.com/sports/football/roster
# "North Park Vikings" = https://athletics.northpark.edu/sports/football/roster
# "Carroll University Pioneers" = https://gopios.com/sports/football/roster
# "Wheaton Thunder" = https://athletics.wheaton.edu/sports/football/roster
# "Elmhurst BlueJays" = https://elmhurstbluejays.com/sports/football/roster
# "WashU Bears" = https://washubears.com/sports/football/roster

# Prompt the user to enter input
year_team_classifier_input <- readline("Enter year/team classifier: ")
# conference codes for 2024-25;2023-24
# "Augustana IL Vikings" = 589539; 558188
# "Carthage Firebirds" = 589558; 558207
# "North Central Cardinals" = 589404; 558052
# "Millikin Big Blue" = 589392; 558040
# "Illinois Wesleyan Titans" = 589368; 558016
# "North Park Vikings" = 589405; 558053
# "Carroll University Pioneers" = 589557; 558206
# "Wheaton Thunder" = 589459; 558105
# "Elmhurst BlueJays" = 589580; 558229
# "WashU Bears" = 589453; 558099

# Prompt the user to enter input
sport_year_input <- readline("Enter the sport/year ID code: ")
# codes for current and previous years
# 2024-25: 16680
# 2023-24: 16460
# 2022-23: 16220
# 2021-22: 15821

# Prompt the user to enter input
team_id_input <- readline("Enter the team ID code: ")
# team id codes
# "Augustana IL Vikings" = 40
# "Carthage Firebirds" = 121
# "North Central Cardinals" = 492
# "Millikin Big Blue" = 424
# "Illinois Wesleyan Titans" = 300
# "North Park Vikings" = 496
# "Carroll University Pioneers" = 120
# "Wheaton Thunder" = 778
# "Elmhurst BlueJays" = 212
# "WashU Bears" = 755



# actual code starts here - can be ran all at once unlike code above

# Read HTML content of the webpage
webpage <- read_html(roster_website_url)

# Extract roster table
roster_tab <- webpage %>%
  html_nodes("table") %>%
  html_table(fill = TRUE)

# Convert from lists to data frames
df_roster <- as.data.frame(roster_tab[[3]])
opp_df_roster <- df_roster[, colSums(is.na(df_roster)) == 0]

coach_df <- as.data.frame(roster_tab[[4]])
#coach_df without NA columns for images
opp_coach_df <- coach_df[, c(2, 3)]

# Load existing Excel file
wb <- loadWorkbook("Football Spotterboards - Copy.xlsx")

# Clear all data in the sheet - run the code below if writing to same the workbook as previous; otherwise comment (#) the line below
removeWorksheet(wb, "OppRoster")
addWorksheet(wb, "OppRoster")

# Write the first dataframe to the Excel file starting at row 1, column 1
writeData(wb, sheet = "OppRoster", x = opp_df_roster, startRow = 1, startCol = 1)
writeData(wb, sheet = "OppRoster", x = opp_coach_df, startRow = 1, startCol = 11)
 
# Save the modified Excel file
saveWorkbook(wb, "Football Spotterboards - Copy.xlsx", overwrite = TRUE)


# URL for first page of any team on ncaa stats site
schedule_results_url_base <- "https://stats.ncaa.org/teams/"

# Construct the URL
schedule_results_url <- paste0(schedule_results_url_base, year_team_classifier_input)

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
opp_team_stats <- chart_3[-1, ]
opp_individual_stat_leaders <- chart_4[-1, ]

opp_schedule_results <- chart_2[c(TRUE, FALSE), ]

# Clear all data in the sheet - run the code below if writing to same the workbook as previous; otherwise comment (#) the line below
removeWorksheet(wb, "OppScheduleResults")
addWorksheet(wb, "OppScheduleResults")

writeData(wb, sheet = "OppScheduleResults", x = opp_schedule_results, startRow = 1, startCol = 1)
writeData(wb, sheet = "OppScheduleResults", x = opp_team_stats, startRow = 1, startCol = 6)
writeData(wb, sheet = "OppScheduleResults", x = opp_individual_stat_leaders, startRow = 1, startCol = 10)

# Save the modified Excel file
saveWorkbook(wb, "Football Spotterboards - Copy.xlsx", overwrite = TRUE)



# URL of the roster page
basic_roster_url <- paste0("https://stats.ncaa.org/team/", team_id_input, "/roster/", sport_year_input)

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


# URL of the team stats page
team_stats_url_base <- paste0("https://stats.ncaa.org/team/", team_id_input, "/stats?id=", sport_year_input, "&year_stat_category_id=")

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

# Rename the data frames
opp_rushing_team_stats <- table_1
opp_first_downs_team_stats <- table_2
opp_passing_team_stats <- table_3
opp_receiving_team_stats <- table_4
opp_total_offense_team_stats <- table_5
opp_all_purpose_yards_team_stats <- table_6
opp_scoring_team_stats <- table_7
opp_sacks_team_stats <- table_8
opp_tackles_team_stats <- table_9
opp_passes_defended_team_stats <- table_10
opp_fumbles_team_stats <- table_11
opp_kicking_team_stats <- table_12
opp_punting_team_stats <- table_13
opp_punt_returns_team_stats <- table_14
opp_ko_and_ko_return_team_stats <- table_15
opp_redzone_team_stats <- table_16
opp_defense_team_stats <- table_17
opp_turnover_margin_team_stats <- table_18
opp_participation_team_stats <- table_19

# Replace NA values with blanks in each dataframe to prevent errors when loading data into excel
opp_rushing_team_stats <- opp_rushing_team_stats %>%
  mutate_all(~ ifelse(is.na(.), "", .))
opp_first_downs_team_stats <- opp_first_downs_team_stats %>%
  mutate_all(~ ifelse(is.na(.), "", .))
opp_passing_team_stats <- opp_passing_team_stats %>%
  mutate_all(~ ifelse(is.na(.), "", .))
opp_receiving_team_stats <- opp_receiving_team_stats %>%
  mutate_all(~ ifelse(is.na(.), "", .))
opp_total_offense_team_stats <- opp_total_offense_team_stats %>%
  mutate_all(~ ifelse(is.na(.), "", .))
opp_all_purpose_yards_team_stats <- opp_all_purpose_yards_team_stats %>%
  mutate_all(~ ifelse(is.na(.), "", .))
opp_scoring_team_stats <- opp_scoring_team_stats %>%
  mutate_all(~ ifelse(is.na(.), "", .))
opp_sacks_team_stats <- opp_sacks_team_stats %>%
  mutate_all(~ ifelse(is.na(.), "", .))
opp_tackles_team_stats <- opp_tackles_team_stats %>%
  mutate_all(~ ifelse(is.na(.), "", .))
opp_passes_defended_team_stats <- opp_passes_defended_team_stats %>%
  mutate_all(~ ifelse(is.na(.), "", .))
opp_fumbles_team_stats <- opp_fumbles_team_stats %>%
  mutate_all(~ ifelse(is.na(.), "", .))
opp_kicking_team_stats <- opp_kicking_team_stats %>%
  mutate_all(~ ifelse(is.na(.), "", .))
opp_punting_team_stats <- opp_punting_team_stats %>%
  mutate_all(~ ifelse(is.na(.), "", .))
opp_punt_returns_team_stats <- opp_punt_returns_team_stats %>%
  mutate_all(~ ifelse(is.na(.), "", .))
opp_ko_and_ko_return_team_stats <- opp_ko_and_ko_return_team_stats %>%
  mutate_all(~ ifelse(is.na(.), "", .))
opp_redzone_team_stats <- opp_redzone_team_stats %>%
  mutate_all(~ ifelse(is.na(.), "", .))
opp_defense_team_stats <- opp_defense_team_stats %>%
  mutate_all(~ ifelse(is.na(.), "", .))
opp_turnover_margin_team_stats <- opp_turnover_margin_team_stats %>%
  mutate_all(~ ifelse(is.na(.), "", .))
opp_participation_team_stats <- opp_participation_team_stats %>%
  mutate_all(~ ifelse(is.na(.), "", .))

# Clear all data in the sheet - run the code below if writing to same the workbook as previous; otherwise comment (#) the line below
removeWorksheet(wb, "OppTeamStats")
addWorksheet(wb, "OppTeamStats")

writeData(wb, sheet = "OppTeamStats", x = opp_rushing_team_stats, startRow = 1, startCol = 1)
writeData(wb, sheet = "OppTeamStats", x = opp_first_downs_team_stats, startRow = 1, startCol = 19)
writeData(wb, sheet = "OppTeamStats", x = opp_passing_team_stats, startRow = 1, startCol = 32)
writeData(wb, sheet = "OppTeamStats", x = opp_receiving_team_stats, startRow = 1, startCol = 52)
writeData(wb, sheet = "OppTeamStats", x = opp_total_offense_team_stats, startRow = 1, startCol = 69)
writeData(wb, sheet = "OppTeamStats", x = opp_all_purpose_yards_team_stats, startRow = 1, startCol = 96)
writeData(wb, sheet = "OppTeamStats", x = opp_scoring_team_stats, startRow = 1, startCol = 115)
writeData(wb, sheet = "OppTeamStats", x = opp_sacks_team_stats, startRow = 1, startCol = 139)
writeData(wb, sheet = "OppTeamStats", x = opp_tackles_team_stats, startRow = 1, startCol = 153)
writeData(wb, sheet = "OppTeamStats", x = opp_passes_defended_team_stats, startRow = 1, startCol = 169)
writeData(wb, sheet = "OppTeamStats", x = opp_fumbles_team_stats, startRow = 1, startCol = 183)
writeData(wb, sheet = "OppTeamStats", x = opp_kicking_team_stats, startRow = 1, startCol = 197)
writeData(wb, sheet = "OppTeamStats", x = opp_punting_team_stats, startRow = 1, startCol = 223)
writeData(wb, sheet = "OppTeamStats", x = opp_punt_returns_team_stats, startRow = 1, startCol = 239)
writeData(wb, sheet = "OppTeamStats", x = opp_ko_and_ko_return_team_stats, startRow = 1, startCol = 252)
writeData(wb, sheet = "OppTeamStats", x = opp_redzone_team_stats, startRow = 1, startCol = 269)
writeData(wb, sheet = "OppTeamStats", x = opp_defense_team_stats, startRow = 1, startCol = 291)
writeData(wb, sheet = "OppTeamStats", x = opp_turnover_margin_team_stats, startRow = 1, startCol = 304)
writeData(wb, sheet = "OppTeamStats", x = opp_participation_team_stats, startRow = 1, startCol = 317)

# Save the modified Excel file
saveWorkbook(wb, "Football Spotterboards - Copy.xlsx", overwrite = TRUE)


# URL of the game by game page
game_by_game_url_base <- paste0("https://stats.ncaa.org/player/index?id=", sport_year_input, "&org_id=", team_id_input, "&stats_player_seq=-100&year_stat_category_id=")

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
opp_rushing_game_stats <- table_15040
opp_first_downs_game_stats <- table_15041
opp_passing_game_stats <- table_15042
opp_receiving_game_stats <- table_15043
opp_total_offense_game_stats <- table_15044
opp_all_purpose_yards_game_stats <- table_15045
opp_scoring_game_stats <- table_15046
opp_sacks_game_stats <- table_15047
opp_tackles_game_stats <- table_15048
opp_passes_defended_game_stats <- table_15049
opp_fumbles_game_stats <- table_15050
opp_kicking_game_stats <- table_15051
opp_punting_game_stats <- table_15052
opp_punt_returns_game_stats <- table_15053
opp_ko_and_ko_return_game_stats <- table_15054
opp_redzone_game_stats <- table_15055
opp_defense_game_stats <- table_15056
opp_turnover_margin_game_stats <- table_15057

# Removing rows with NA values that were disrupting loading process
opp_rushing_game_stats <- opp_rushing_game_stats %>%
  mutate_all(~ ifelse(is.na(.), "", .))
opp_first_downs_game_stats <- opp_first_downs_game_stats %>%
  mutate_all(~ ifelse(is.na(.), "", .))
opp_passing_game_stats <- opp_passing_game_stats %>%
  mutate_all(~ ifelse(is.na(.), "", .))
opp_receiving_game_stats <- opp_receiving_game_stats %>%
  mutate_all(~ ifelse(is.na(.), "", .))
opp_total_offense_game_stats <- opp_total_offense_game_stats %>%
  mutate_all(~ ifelse(is.na(.), "", .))
opp_all_purpose_yards_game_stats <- opp_all_purpose_yards_game_stats %>%
  mutate_all(~ ifelse(is.na(.), "", .))
opp_scoring_game_stats <- opp_scoring_game_stats[, -ncol(opp_scoring_game_stats)]
opp_scoring_game_stats <- opp_scoring_game_stats %>%
  mutate_all(~ ifelse(is.na(.), "", .))
opp_sacks_game_stats <- opp_sacks_game_stats %>%
  mutate_all(~ ifelse(is.na(.), "", .))
opp_tackles_game_stats <- opp_tackles_game_stats %>%
  mutate_all(~ ifelse(is.na(.), "", .))
opp_passes_defended_game_stats <- opp_passes_defended_game_stats %>%
  mutate_all(~ ifelse(is.na(.), "", .))
opp_fumbles_game_stats <- opp_fumbles_game_stats %>%
  mutate_all(~ ifelse(is.na(.), "", .))
opp_kicking_game_stats <- opp_kicking_game_stats[, 1:8]
opp_kicking_game_stats <- opp_kicking_game_stats %>%
  mutate_all(~ ifelse(is.na(.), "", .))
opp_punting_game_stats <- opp_punting_game_stats %>%
  mutate_all(~ ifelse(is.na(.), "", .))
opp_punt_returns_game_stats <- opp_punt_returns_game_stats %>%
  mutate_all(~ ifelse(is.na(.), "", .))
opp_ko_and_ko_return_game_stats <- opp_ko_and_ko_return_game_stats %>%
  mutate_all(~ ifelse(is.na(.), "", .))
opp_redzone_game_stats <- opp_redzone_game_stats %>%
  mutate_all(~ ifelse(is.na(.), "", .))
opp_defense_game_stats <- opp_defense_game_stats %>%
  mutate_all(~ ifelse(is.na(.), "", .))
opp_turnover_margin_game_stats <- opp_turnover_margin_game_stats %>%
  mutate_all(~ ifelse(is.na(.), "", .))


# Clear all data in the sheet - run the code below if writing to same the workbook as previous; otherwise comment (#) the line below
removeWorksheet(wb, "OppGameStats")
addWorksheet(wb, "OppGameStats")

writeData(wb, sheet = "OppGameStats", x = opp_rushing_game_stats, startRow = 1, startCol = 1)
writeData(wb, sheet = "OppGameStats", x = opp_first_downs_game_stats, startRow = 1, startCol = 14)
writeData(wb, sheet = "OppGameStats", x = opp_passing_game_stats, startRow = 1, startCol = 22)
writeData(wb, sheet = "OppGameStats", x = opp_receiving_game_stats, startRow = 1, startCol = 37)
writeData(wb, sheet = "OppGameStats", x = opp_total_offense_game_stats, startRow = 1, startCol = 49)
writeData(wb, sheet = "OppGameStats", x = opp_all_purpose_yards_game_stats, startRow = 1, startCol = 71)
writeData(wb, sheet = "OppGameStats", x = opp_scoring_game_stats, startRow = 1, startCol = 85)
writeData(wb, sheet = "OppGameStats", x = opp_sacks_game_stats, startRow = 1, startCol = 103)
writeData(wb, sheet = "OppGameStats", x = opp_tackles_game_stats, startRow = 1, startCol = 112)
writeData(wb, sheet = "OppGameStats", x = opp_passes_defended_game_stats, startRow = 1, startCol = 123)
writeData(wb, sheet = "OppGameStats", x = opp_fumbles_game_stats, startRow = 1, startCol = 132)
writeData(wb, sheet = "OppGameStats", x = opp_kicking_game_stats, startRow = 1, startCol = 141)
writeData(wb, sheet = "OppGameStats", x = opp_punting_game_stats, startRow = 1, startCol = 150)
writeData(wb, sheet = "OppGameStats", x = opp_punt_returns_game_stats, startRow = 1, startCol = 162)
writeData(wb, sheet = "OppGameStats", x = opp_ko_and_ko_return_game_stats, startRow = 1, startCol = 170)
writeData(wb, sheet = "OppGameStats", x = opp_redzone_game_stats, startRow = 1, startCol = 183)
writeData(wb, sheet = "OppGameStats", x = opp_defense_game_stats, startRow = 1, startCol = 200)
writeData(wb, sheet = "OppGameStats", x = opp_turnover_margin_game_stats, startRow = 1, startCol = 208)

# Save the modified Excel file
saveWorkbook(wb, "Football Spotterboards - Copy.xlsx", overwrite = TRUE)



# update url according to year and team
national_ranking_summary_url <- paste0("https://stats.ncaa.org/rankings/ranking_summary?academic_year=", year_input, "&division=", division_input, "&org_id=", team_id_input, "&sport_code=MFB")

# Read HTML content of the webpage
webpage <- read_html(national_ranking_summary_url)

# Scrape the third table from the webpage
tables <- html_nodes(webpage, "table")
opp_natl_table <- html_table(tables[[3]], fill = TRUE)
opp_natl_table_2 <- html_table(tables[[4]], fill = TRUE)

# update url according to year and team
conference_ranking_summary_url <- paste0("https://stats.ncaa.org/rankings/ranking_summary?academic_year=", year_input, "&division=", 3, "&conf_id=", conference_input, "&sport_code=MFB")

# Read HTML content of the webpage
webpage <- read_html(conference_ranking_summary_url)

# Scrape the third table from the webpage
tables <- html_nodes(webpage, "table")
opp_conf_table <- html_table(tables[[2]], fill = TRUE)
opp_conf_table_2 <- html_table(tables[[3]], fill = TRUE)

# Rename the sixth column to "LeaderValue"
colnames(opp_conf_table)[6] <- "LeaderValue"

# Create an empty data frame to store the transformed data
transformed_df <- data.frame(matrix(ncol = ncol(opp_conf_table), nrow = nrow(opp_conf_table) * 10), stringsAsFactors = FALSE)
colnames(transformed_df) <- colnames(opp_conf_table)

# Duplicate all columns
for (col in colnames(opp_conf_table)[-6]) {
  transformed_df[[col]] <- rep(opp_conf_table[[col]], each = 10)
}

# Store LeaderValue column separately
leader_values <- rep(opp_conf_table$LeaderValue, each = 10)

# Split the team names in column 2 and create new rows
team_list <- strsplit(opp_conf_table$Team, "\n")

for (i in 1:nrow(opp_conf_table)) {
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
national_rank_list <- strsplit(opp_conf_table$NationalRank, "\n")

for (i in 1:nrow(opp_conf_table)) {
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
value_list <- strsplit(opp_conf_table$Value, "\n")

for (i in 1:nrow(opp_conf_table)) {
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
national_leader_list <- lapply(strsplit(opp_conf_table$'National Leader', "\n"), function(x) x[1])

# Create new rows
for (i in 1:nrow(opp_conf_table)) {
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
opp_conf_table <- transformed_df



# Rename the sixth column to "LeaderValue"
colnames(opp_conf_table_2)[6] <- "LeaderValue"

# Create an empty data frame to store the transformed data
transformed_df_2 <- data.frame(matrix(ncol = ncol(opp_conf_table_2), nrow = nrow(opp_conf_table_2) * 10), stringsAsFactors = FALSE)
colnames(transformed_df_2) <- colnames(opp_conf_table_2)

# Duplicate all columns
for (col in colnames(opp_conf_table_2)[-6]) {
  transformed_df_2[[col]] <- rep(opp_conf_table_2[[col]], each = 10)
}

# Store LeaderValue column separately
leader_values_2 <- rep(opp_conf_table_2$LeaderValue, each = 10)

# Split the player names in column 2 and create new rows
player_list_2 <- strsplit(opp_conf_table_2$Player, "\n")

for (i in 1:nrow(opp_conf_table_2)) {
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
national_rank_list_2 <- strsplit(opp_conf_table_2$NationalRank, "\n")

for (i in 1:nrow(opp_conf_table_2)) {
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
value_list_2 <- strsplit(opp_conf_table_2$Value, "\n")

for (i in 1:nrow(opp_conf_table_2)) {
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
national_leader_list_2 <- lapply(strsplit(opp_conf_table_2$'National Leader', "\n"), function(x) x[1])

# Create new rows
for (i in 1:nrow(opp_conf_table_2)) {
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
opp_conf_table_2 <- transformed_df_2


# Clear all data in the "OppRankings" sheet - run the code below if writing to same the workbook as previous; otherwise comment (#) the line below
removeWorksheet(wb, "OppRankings")
addWorksheet(wb, "OppRankings")

writeData(wb, sheet = "OppRankings", x = opp_natl_table, startRow = 1, startCol = 1)
writeData(wb, sheet = "OppRankings", x = opp_natl_table_2, startRow = 1, startCol = 10)
writeData(wb, sheet = "OppRankings", x = opp_conf_table, startRow = 1, startCol = 20)
writeData(wb, sheet = "OppRankings", x = opp_conf_table_2, startRow = 1, startCol = 27)

# Save the modified Excel file
saveWorkbook(wb, "Football Spotterboards - Copy.xlsx", overwrite = TRUE)