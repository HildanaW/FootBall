library(rvest)
library(stringr)
library(dplyr)

rm(list = ls())

# This script is responsible for bringing in all of the player stats from the previous game played to be used in "L" section of player breakdowns

# Define function to scrape player data
scrape_player_data <- function(player_id, category_id) {
  url <- paste0("https://stats.ncaa.org/player/index?id=16460&org_id=40&stats_player_seq=", player_id, "&year_stat_category_id=", category_id)
  webpage <- read_html(url)
  # Extract the table or any other desired elements
  table <- html_table(html_nodes(webpage, "table"))[[5]]  # Assuming the fifth table on the page
  return(table)
}

# Define player IDs and category IDs
# Could not figure out how to create list based on scraped data - only works for Augie and not an iterable solution
# Will need to inspect element of the html code for next year's roster to update the values below
# chatgpt can do this for you if you provide the code and make the request
player_ids <- c(
  2882558,  # Alvarado, Michael
  2732033,  # Amiano, Joe
  2882537,  # Banks, AJ
  2584287,  # Baron, Mark
  2522693,  # Bhardwaj, Cole
  2598947,  # Bolton, John
  2598955,  # Breeden, Johnny
  2882564,  # Brosnan, Mikey
  2882547,  # Brown, Nick
  2598965,  # Brown, Owen
  2732109,  # Buol, Tanner
  2732157,  # Burton, Dylan
  2888150,  # Byrne, Collin
  2732162,  # Carone, Tristan
  2732193,  # Castle, Max
  2598974,  # Certa, Jack
  2638777,  # Clem, Joren
  2882535,  # Clevenger, Anthony
  2732213,  # Coss, Jake
  2599012,  # Crawley, Liam
  2887315,  # Dettloff, Dylan
  2599032,  # DiGioia, Mike
  2882531,  # Estrada, Derek
  2791080,  # Gallagher, Adam
  2732557,  # Glendenning, Adam
  2495534,  # Gorken, Ian
  2888099,  # Grannis, Peyton
  2888055,  # Gray, Matt
  2888162,  # Gray, Zach
  2495541,  # Grimes, Jason
  2732246,  # Gustafsson, Ryan
  2599229,  # Hall, Thomas
  2882518,  # Hanson, Tez
  2224822,  # Harper, Nick
  2732248,  # Hartman, Ben
  2888140,  # Henry, Nate
  2732256,  # Hogan, Nick
  2732259,  # Holcomb, Brett
  2732262,  # Hopping, Parker
  2887310,  # Houchin, Braden
  2732265,  # Hulett, Nolin
  2495604,  # Hunkins, Rukkus
  2888025,  # Hunt, Dino
  2224802,  # Inserra, Bobby
  2495694,  # Johnson, Cain
  2599301,  # Keany, Hugh
  2495621,  # Kessler, Matthew
  2888051,  # Klein, TJ
  2732293,  # Landers, John
  2887301,  # Lim, Josh
  2495613,  # Lopez, Aaron
  2732344,  # Ludlum, Ben
  2888015,  # Lyons, Jack
  2483898,  # Macdonald, Brayden
  2732346,  # Macdonald, Tanner
  2732298,  # Malloy, Ian
  2732357,  # Marcelino, Ricardo
  2224880,  # Maroon, Sidney
  2882560,  # Marsh, Cooper
  2732362,  # May, Colin
  2732364,  # McDonough, Joey
  2882559,  # McShaw, Kaden
  2888122,  # Michaels, Zach
  2732368,  # Miller, Jake
  2599291,  # Mistak, Arik
  2888139,  # Murdock, Blayden
  2888060,  # Nieto, Teke
  2599327,  # North, Jakob
  2934898,  # Novak, Ryan
  2732309,  # O'Boyle, Conor
  2495629,  # Ogarek, Alec
  2732392,  # Olson, Jack
  2732380,  # Oregon, Darren
  2888078,  # Ott, Zach
  2882611,  # Peterson, Liam
  2888159,  # Putman, Trevor
  2599374,  # Rivelli, Tyler
  2888061,  # Rodriguez, Antonio
  2225029,  # Romano, Cole
  2888142,  # Schlanser, Sean
  2599382,  # Sheehan, Zac
  2882546,  # Simon, Nate
  2224925,  # Skold, Daniel
  2732406,  # Smith, Breyden
  2882555,  # Spillane, Tim
  2732416,  # Splitt, Toby
  2887289,  # Sprecher, Tanner
  2888146,  # Stapleton, Miles
  2887317,  # Stapleton, Peter
  2732558,  # Stitely, Hunter
  2599385,  # Straight, Dakota
  2257842,  # Swaney, Tim
  2257827,  # Tatum, Chase
  2888072,  # Toland, Charles
  2888161,  # Toole, Ryan
  2495618,  # Uhlmann, Jacob
  2599393,  # Unyi, Bill
  2599422,  # Vaynerman, David
  2495658,  # Vesey, Jordan
  2888147,  # Vrabec, Ethan
  2732533,  # Wells, Magnus
  2732536,  # Williams, Ian
  2732537,  # Williams, Zion
  2599446,  # Willis, Luke
  2888018,  # Wissel, Jason
  2887305,  # Witkowski, Danny
  2599462,  # Woodrey, Josh
  2732538,  # Worrels, Ronde
  2888090   # Zitkus, Jimmy
)
jersey_numbers <- c(
  11,   # Alvarado, Michael
  81,   # Amiano, Joe
  5,    # Banks, AJ
  27,   # Baron, Mark
  12,   # Bhardwaj, Cole
  1,    # Bolton, John
  26,   # Breeden, Johnny
  16,   # Brosnan, Mikey
  8,    # Brown, Nick
  54,   # Brown, Owen
  62,   # Buol, Tanner
  60,   # Burton, Dylan
  95,   # Byrne, Collin
  47,   # Carone, Tristan
  72,   # Castle, Max
  35,   # Certa, Jack
  56,   # Clem, Joren
  11,   # Clevenger, Anthony
  20,   # Coss, Jake
  10,   # Crawley, Liam
  41,   # Dettloff, Dylan
  25,   # DiGioia, Mike
  2,    # Estrada, Derek
  83,   # Gallagher, Adam
  65,   # Glendenning, Adam
  5,    # Gorken, Ian
  74,   # Grannis, Peyton
  58,   # Gray, Matt
  99,   # Gray, Zach
  4,    # Grimes, Jason
  90,   # Gustafsson, Ryan
  15,   # Hall, Thomas
  1,    # Hanson, Tez
  19,   # Harper, Nick
  76,   # Hartman, Ben
  82,   # Henry, Nate
  91,   # Hogan, Nick
  31,   # Holcomb, Brett
  40,   # Hopping, Parker
  40,   # Houchin, Braden
  89,   # Hulett, Nolin
  39,   # Hunkins, Rukkus
  49,   # Hunt, Dino
  9,    # Inserra, Bobby
  77,   # Johnson, Cain
  94,   # Keany, Hugh
  63,   # Kessler, Matthew
  57,   # Klein, TJ
  30,   # Landers, John
  32,   # Lim, Josh
  53,   # Lopez, Aaron
  21,   # Ludlum, Ben
  45,   # Lyons, Jack
  66,   # Macdonald, Brayden
  36,   # Macdonald, Tanner
  57,   # Malloy, Ian
  67,   # Marcelino, Ricardo
  42,   # Maroon, Sidney
  13,   # Marsh, Cooper
  68,   # May, Colin
  75,   # McDonough, Joey
  18,   # McShaw, Kaden
  79,   # Michaels, Zach
  80,   # Miller, Jake
  23,   # Mistak, Arik
  81,   # Murdock, Blayden
  59,   # Nieto, Teke
  71,   # North, Jakob
  55,   # Novak, Ryan
  87,   # O'Boyle, Conor
  64,   # Ogarek, Alec
  28,   # Olson, Jack
  22,   # Oregon, Darren
  61,   # Ott, Zach
  20,   # Peterson, Liam
  97,   # Putman, Trevor
  34,   # Rivelli, Tyler
  59,   # Rodriguez, Antonio
  92,   # Romano, Cole
  84,   # Schlanser, Sean
  78,   # Sheehan, Zac
  7,    # Simon, Nate
  52,   # Skold, Daniel
  3,    # Smith, Breyden
  9,    # Spillane, Tim
  24,   # Splitt, Toby
  31,   # Sprecher, Tanner
  86,   # Stapleton, Miles
  43,   # Stapleton, Peter
  93,   # Stitely, Hunter
  33,   # Straight, Dakota
  50,   # Swaney, Tim
  8,    # Tatum, Chase
  60,   # Toland, Charles
  98,   # Toole, Ryan
  51,   # Uhlmann, Jacob
  85,   # Unyi, Bill
  6,    # Vaynerman, David
  2,    # Vesey, Jordan
  88,   # Vrabec, Ethan
  96,   # Wells, Magnus
  73,   # Williams, Zion
  17,   # William, Ian
  55,   # Willis, Luke
  46,   # Wissel, Jason
  38,   # Witkowski, Danny
  69,   # Woodrey, Josh
  44,   # Worrels, Ronde
  70    # Zitkus, Jimmy
)
category_ids <- 15040:15057 

# Initialize an empty list to store table data
all_tables_data <- list()

# Loop through each player ID
for (player_id in player_ids) {
  # Initialize an empty list to store table data for this player
  player_tables <- list()
  
  # Loop through each year_stat_category_id and scrape table data
  for (cat_id in category_ids) {
    table <- scrape_player_data(player_id, cat_id)
    player_tables[[as.character(cat_id)]] <- table
  }
  
  # Store the player tables in the all_tables_data list
  all_tables_data[[as.character(player_id)]] <- player_tables
}

# Create a named vector
jersey_mapping <- setNames(jersey_numbers, player_ids)

# Initialize an empty list to store the desired tables
desired_tables <- list()

# Loop through each player's tables
for (player_id in names(all_tables_data)) {
  # Loop through each year_stat_category_id
  for (cat_id in names(all_tables_data[[player_id]])) {
    # Extract the desired table (fifth table)
    table <- all_tables_data[[player_id]][[cat_id]]
    
    # Keep only the first and last row
    table <- rbind(table[2, ], table[nrow(table), ])
    
    # Extract the second row as column names
    col_names <- as.character(table[1, ])
    
    # Assign column names to the table
    colnames(table) <- col_names
    
    # Remove the second row (now column names)
    table <- table[-1, ]
    
    # Convert the table to a data frame
    df <- as.data.frame(table)
    
    # Check if any data is available in columns 5 through the end
    if (any(sapply(df[1, 5:ncol(df)], function(x) !is.na(x) && x != ""))) {
      # Remove columns with NA values
      df <- df[, colSums(is.na(df)) == 0]
      
      # Store the data frame in the desired_tables list
      desired_tables[[paste0(player_id, "_", cat_id)]] <- df
      
      # Extract player ID from the name
      player_id_extracted <- str_extract(player_id, "\\d+")
      
      # Find the index of the player_id_extracted in player_ids
      index <- match(as.numeric(player_id_extracted), player_ids)
      
      # If player ID is found, extract the corresponding jersey number
      if (!is.na(index)) {
        jersey_number <- jersey_numbers[index]
      } else {
        # If player ID is not found, assign NA to jersey number
        jersey_number <- ""
      }
      
      # Create a new data frame with "Jersey" as the first column
      new_df <- data.frame(Jersey = jersey_number, df)
      
      # Transpose the df
      new_df <- t(new_df)
      new_df <- as.data.frame(new_df)
      
      # Update the desired_tables list with the new data frame
      desired_tables[[paste0(player_id, "_", cat_id)]] <- new_df
    }
  }
}

# Loop through each element (tables) in desired_tables
for (cat_id in names(desired_tables)) {
  # Assign the data frame to a variable with a specific name (e.g., table_15040, table_15041, ...)
  assign(paste0("table_", cat_id), desired_tables[[cat_id]])
}

# Create a new workbook
wb <- createWorkbook()

# Add a worksheet to the workbook
addWorksheet(wb, "PlayerData3")

# Initialize a variable to keep track of the column number
col_number <- 1

# Loop through each element (tables) in desired_tables
for (cat_id in names(desired_tables)) {
  # Get the data frame
  df <- desired_tables[[cat_id]]
  
  
  # Write the modified data frame to the worksheet
  writeData(wb, "PlayerData3", df, startCol = col_number, startRow = 1)
  
  # Update the column number for the next data frame
  col_number <- col_number + ncol(df) + 1  # Add 1 for the empty column
}

# Save the workbook
saveWorkbook(wb, "PlayerData3.xlsx")



