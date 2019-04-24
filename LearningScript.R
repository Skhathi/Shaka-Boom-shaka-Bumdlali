library(pacman) # Check https://github.com/trinker/pacman for pacman package use
p_load(readxl)
p_load(tidyverse)
p_load(dplyr)


## Following code produces df df_skrrr1 for a specific user to make framework for
##----
# Automate this in a loop for all users
df_skrrr1 <- setNames(data.frame(matrix(ncol = 7, nrow = 0)), 
                     c("GameID",
                       "Date",
                       "Time",
                       "Category",
                       "Concept",
                       "Diff",
                       "Correct"))
df_skrrr2 <- setNames(data.frame(matrix(ncol = 7, nrow = 0)), 
                      c("GameID",
                        "Date",
                        "Time",
                        "Category",
                        "Concept",
                        "Diff",
                        "Correct"))
df_skrrr3 <- setNames(data.frame(matrix(ncol = 7, nrow = 0)), 
                      c("GameID",
                        "Date",
                        "Time",
                        "Category",
                        "Concept",
                        "Diff",
                        "Correct"))


# The loop for users will be over all user IDs. This is just a test for 3 English users

path_df <- "/Users/skhathi/Documents/DataAnalysis/Ibumdlali/"
df_gameDataSpecUser1 <- as.data.frame(read_csv(paste0(path_df, "AllGamesForSpecificUser1.csv", collapse = NULL)))
df_gameDataSpecUser2 <- as.data.frame(read_csv(paste0(path_df, "AllGamesForSpecificUser2.csv", collapse = NULL)))
df_gameDataSpecUser3 <- as.data.frame(read_csv(paste0(path_df, "AllGamesForSpecificUser3.csv", collapse = NULL)))


# For the sake of this analysis, we will look for the categories in the questions sheets. 
# Load every table with questions and assign categories based on the tables and not on the category from the specific user
# Next is just loading those tables indie of AnalysisScript.R

eng_path <- paste0(path_df, "English/", collapse = NULL)
zul_path <- paste0(path_df, "Zulu/", collapse = NULL)

eng_files <- list.files(eng_path, all.files = FALSE, full.names = FALSE)
zul_files <- list.files(zul_path, all.files = FALSE, full.names = FALSE)

eng_df_names <- vector()
for (i in 1:length(eng_files)) {
  df <- as.data.frame(read_csv2(paste0(eng_path, eng_files[i], collapse = NULL)))
  name_of_df <- gsub("\\-.*", "", eng_files[i])
  assign(name_of_df,df)
  rm(df)
  eng_df_names <- c(eng_df_names, name_of_df) # For different kinds of string regex manipulation and replacement see 
  # https://stevencarlislewalker.wordpress.com/2013/02/13/remove-or-replace-everything-before-or-after-a-specified-character-in-r-strings/
}
  
# zul_df_names <- vector()
# for (i in 1:length(zul_files)) {
#   df <- as.data.frame(read_csv2(paste0(zul_path, zul_files[i], collapse = NULL)))
#   name_of_df <- gsub("\\-.*", "", zul_files[i])
#   assign(name_of_df,df)
#   rm(df)
#   zul_df_names <- c(zul_df_names, name_of_df) # For different kinds of string regex manipulation and replacement see 
#   # https://stevencarlislewalker.wordpress.com/2013/02/13/remove-or-replace-everything-before-or-after-a-specified-character-in-r-strings/
# }
  
# Now to build a search for every category 

# Empty df to fill in following loop
df_catAndConc <- setNames(data.frame(matrix(ncol = 2, nrow = 0)), 
                          c("Concept",
                            "Category"))
# df_catAndConcZul <- setNames(data.frame(matrix(ncol = 2, nrow = 0)), 
#                           c("Concept",
#                             "Category"))


# Actually making vectors with concept as name and categories as entries (in this loop we will read in category names)
for (i in 1:length(eng_df_names)){
  df_gotten <- get(eng_df_names[i])
  vec_gotten <- df_gotten[1]
  
  vec_withCatName <- unique(vec_gotten)
  assign(paste0("search_", eng_df_names[i], collapse = NULL),vec_withCatName)
  
  rm(vec_gotten)
  rm(df_gotten)
  
  for (j in 1:length(vec_withCatName$Concept)){
    
  addition <- c(vec_withCatName$Concept[j], eng_df_names[i])
  df_catAndConc[(nrow(df_catAndConc)+1), ] <-addition
    
  }
  
}

# for (i in 1:length(zul_df_names)){
#   df_gotten <- get(zul_df_names[i])
#   vec_gotten <- df_gotten[1]
#   
#   vec_withCatName <- unique(vec_gotten)
#   assign(paste0("search_", zul_df_names[i], collapse = NULL),vec_withCatName)
#   
#   rm(vec_gotten)
#   rm(df_gotten)
#   
#   for (j in 1:length(vec_withCatName$Concept)){
#     
#     addition <- c(vec_withCatName$Concept[j], zul_df_names[i])
#     df_catAndConcZul[(nrow(df_catAndConcZul)+1), ] <-addition
#     
#   }
#   
# }
  

for (j in 1:nrow(df_gameDataSpecUser1)){
  # Searching for what concept is from which category and then putting it into the right entry into the 
  ind <- which(df_catAndConc$Concept == df_gameDataSpecUser1$Concept[j])

  # next is a vector of the entries I need to append
  addition <- c(df_gameDataSpecUser1$GameId[j], 
                gsub("\\ .*", "",df_gameDataSpecUser1$DatePlayed[j]), # does some freaky shit when using as.Date so just remove anything following date
                sub('\\..*', '', (gsub(paste(as.Date(df_gameDataSpecUser1$DatePlayed[j]), "", collapse = NULL), "", df_gameDataSpecUser1$DatePlayed[j]))), # this has the tiime but I'm not thinking about it too much. I'll sort when I do
                df_catAndConc$Category[ind],  
                df_gameDataSpecUser1$Concept[j], 
                df_gameDataSpecUser1$Difficulty[j], 
                df_gameDataSpecUser1$Correct[j] 
  )
  
  df_skrrr1[nrow(df_skrrr1)+1, ] <- addition # finally, addition of row works
  # Introducing df_skrrr1
  if (j == nrow(df_gameDataSpecUser1)){
    df_skrrr1$Diff <- as.integer(df_skrrr1$Diff)
    df_skrrr1 <- df_skrrr1[order(df_skrrr1$Date, df_skrrr1$Time),]
  }
}

for (j in 1:nrow(df_gameDataSpecUser2)){
  # Searching for what concept is from which category and then putting it into the right entry into the 
  ind <- which(df_catAndConc$Concept == df_gameDataSpecUser2$Concept[j])
  
  # next is a vector of the entries I need to append
  addition <- c(df_gameDataSpecUser2$GameId[j], 
                gsub("\\ .*", "",df_gameDataSpecUser2$DatePlayed[j]), # does some freaky shit when using as.Date so just remove anything following date
                sub('\\..*', '', (gsub(paste(as.Date(df_gameDataSpecUser2$DatePlayed[j]), "", collapse = NULL), "", df_gameDataSpecUser2$DatePlayed[j]))), # this has the tiime but I'm not thinking about it too much. I'll sort when I do
                df_catAndConc$Category[ind],  
                df_gameDataSpecUser2$Concept[j], 
                df_gameDataSpecUser2$Difficulty[j], 
                df_gameDataSpecUser2$Correct[j] 
  )
  
  df_skrrr2[nrow(df_skrrr2)+1, ] <- addition # finally, addition of row works
  # Introducing df_skrrr2
  if (j == nrow(df_gameDataSpecUser2)){
    df_skrrr2$Diff <- as.integer(df_skrrr2$Diff)
    df_skrrr2 <- df_skrrr2[order(df_skrrr2$Date, df_skrrr2$Time),]
  }
}

for (j in 1:nrow(df_gameDataSpecUser3)){
  # Searching for what concept is from which category and then putting it into the right entry into the 
  ind <- which(df_catAndConc$Concept == df_gameDataSpecUser3$Concept[j])
  
  # next is a vector of the entries I need to append
  addition <- c(df_gameDataSpecUser3$GameId[j], 
                gsub("\\ .*", "",df_gameDataSpecUser3$DatePlayed[j]), # does some freaky shit when using as.Date so just remove anything following date
                sub('\\..*', '', (gsub(paste(as.Date(df_gameDataSpecUser3$DatePlayed[j]), "", collapse = NULL), "", df_gameDataSpecUser3$DatePlayed[j]))), # this has the tiime but I'm not thinking about it too much. I'll sort when I do
                df_catAndConc$Category[ind],  
                df_gameDataSpecUser3$Concept[j], 
                df_gameDataSpecUser3$Difficulty[j], 
                df_gameDataSpecUser3$Correct[j] 
  )
  
  df_skrrr3[nrow(df_skrrr3)+1, ] <- addition # finally, addition of row works
  # Introducing df_skrrr3
  if (j == nrow(df_gameDataSpecUser3)){
    df_skrrr3$Diff <- as.integer(df_skrrr3$Diff)
    df_skrrr3 <- df_skrrr3[order(df_skrrr3$Date, df_skrrr3$Time),]
  }
}


## Now to actually build learning framework
##---- 

path_user <- "/Users/skhathi/Documents/DataAnalysis/Ibumdlali/"
df_allUserInfo <- as.data.frame(read_csv(paste0(path_user, "AllUserInfo.csv", collapse = NULL))) # Reads incorrectly with read_csv2, but correct with ..._csv. What's the difference
vec_allUserIDs <- as.vector(df_allUsers$Id)

# Slicing dimension/criteria (cuts)
by_bank = FALSE
by_network = FALSE
by_prepaid = FALSE
by_age = FALSE
by_geo = FALSE
by_edu = FALSE

# Dividing time range into analysable chunks
# First we do this for 3 users and then generalize approach for all users in specific case (can be by group or we can do all and group by analysis)
list_uID <- list(df_skrrr1, df_skrrr2, df_skrrr3)

for (i in 1:length(list_uID)){
  
}
  
  
  