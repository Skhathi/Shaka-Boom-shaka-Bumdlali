library(pacman) # Check https://github.com/trinker/pacman for pacman package use
p_load(readxl)
p_load(tidyverse)
p_load(dplyr)
source("AnalysisScript.R")


## Following code produces df df_skrrr1,2,3 for specific users to make framework for
# Automate this in a loop for all users (these will be changed to be the user IDs for automation)
##----
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


# For different kinds of string regex manipulation and replacement see 
# https://stevencarlislewalker.wordpress.com/2013/02/13/remove-or-replace-everything-before-or-after-a-specified-character-in-r-strings/
eng_df_names <- vector()
for (i in 1:length(eng_files)) {
  df <- as.data.frame(read_csv2(paste0(eng_path, eng_files[i], collapse = NULL)))
  name_of_df <- gsub("\\-.*", "", eng_files[i])
  assign(name_of_df,df)
  rm(df)
  eng_df_names <- c(eng_df_names, name_of_df) 
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
  
# df_skrrr1 has user ID ad6d2bd6-3f29-4261-9025-01642c0e6f07
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

# df_skrrr2 has user ID eec99158-8dfb-4e50-9de2-e590930e09b6
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

# df_skrrr3 has user ID c702d848-2c04-4619-8a81-c5e18a7e6592
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
## ---- 

path_user <- "/Users/skhathi/Documents/DataAnalysis/Ibumdlali/"
df_allUserInfo <- as.data.frame(read_csv(paste0(path_user, "AllUserInfo.csv", collapse = NULL))) # Reads incorrectly with read_csv2, but correct with ..._csv. What's the difference


# Following two users have many games played but aren't in 1st 100 so appending them manually
vec_addedUser2Info <- c("0.060168305322567589", "1", "64.630144585965041", NA, 
                        "2019-04-22 08:17:37.207 UTC", NA, "2f7e195d-8ddf-4b78-9ced-13f0a95dacc1", 
                        "2019-04-13 07:42:31.633 UTC", "kieran.ewers@mail.com", "4", 
                        "e0NMogwvPYQ:APA91bHR23B1_5Q3LXtJfVMuKGNQVBTb5KcwRVGgfLr9O5dn8HQEkV07rJdW8K4oKxgpG7JoNfT09dS1G5lgh9_Q4gONASZVv5Ma-qjoRi8GMD4IAYB8mYEVXa_lTZPxQ7QvnWsQrNN2", 
                        "1", "0", "1", "eec99158-8dfb-4e50-9de2-e590930e09b6", "Ewers", "4284.5818491925638", "Kieran", NA, "https://graph.facebook.com/1977030022423152/picture?type=large",
                        "TmV0d29ya05vZGU6MTA=", "0713916075", "Standard Bank", "Tertiary (diploma, degree, etc.)", "25 - 34", "1977030022423152", "279f7fe4-1543-4693-9272-f4f5a606363f"
) # for the df df_allUserInfo

vec_addedUser3Info <- c("0.060405492251382031", "1", "65.063374050817018", NA, 
                        "2019-04-24 08:24:59.480 UTC", NA, "2f7e195d-8ddf-4b78-9ced-13f0a95dacc1", 
                        "2019-04-11 12:41:27.445 UTC", "fathimadbn@gmail.com", "12", 
                        "dP4qlLHyXrU:APA91bGe36sEsIWuAfFTMdBF9qfPrQCruS48OrDo_vFQLRPY6rrYBu6BXIUuoLwLHAGff_A-2I7O4JZcJ7RjqMhCwT8GYz5kZHcDnnmiSCIE6rWhSijk37p4cxcxBE6eJrJ6j6yI3kX_", 
                        "0", "0", "1", "c702d848-2c04-4619-8a81-c5e18a7e6592", "Ebrahim", "4236.7744010739643", "Fatima", NA, "https://graph.facebook.com/1164627037047797/picture?type=large",
                        "TmV0d29ya05vZGU6MTI=", "0740597866", "F N B", "Matric", "25 - 34", "1164627037047797", NA
) # for the df df_allUserInfo
df_allUserInfo <- rbind(df_allUserInfo, vec_addedUser2Info, vec_addedUser3Info)


vec_allUserIDs <- as.vector(df_allUserInfo$Id) # Still only up to 1002


# Choose your slicing dimensions. 
# Note, no analysis if the criteria above is marked FALSE

# Keys for analysis and slicing and stuff
#----  
lang <- c("Zulu", "English")
banks <- c(NA, "F N B", "Capitec", "Absa", "Standard Bank", "Nedbank", "Other", "No bank account", "African Bank")
networks <- c(NA, "TmV0d29ya05vZGU6MTM=", "TmV0d29ya05vZGU6MTA=", "TmV0d29ya05vZGU6MTE=", "TmV0d29ya05vZGU6MTI=")
prepaids <- c(1, 0)
ages <- c(NA, "18 - 24", "25 - 34", "35 - 44", "45 - 54", "55 - 64", "65+") # check if there's a space between the 65 and the +
# geos <- c()
edus <- c(NA, "Matric", "High school", "Tertiary (diploma, degree, etc.)", "No schooling", "Primary school")

# Sleutels 
df_networkKey <- data.frame("ID" = c("TmV0d29ya05vZGU6MTI=", "TmV0d29ya05vZGU6MTM=", "TmV0d29ya05vZGU6MTA=", "TmV0d29ya05vZGU6MTE="),
                            "Network" = c("Cell C", "Telkom Mobile", "Vodacom", "MTN")) # We made a key table for networks
df_langKey <- data.frame("LanguagePreferenceID" = c("2f7e195d-8ddf-4b78-9ced-13f0a95dacc1", "c1d6424b-b692-446c-9d3d-d0c4ed1cbb63"),
                         "LanguageKey" = c(0, 1),
                         "Language" = c("English", "Zulu"))

# Analysis rigor
# Slicing dimension/criteria (cuts)
by_lang = TRUE
cut_byLang = c("English")

by_bank = TRUE
cut_byBank = c("Capitec", "Standard Bank") # third person is mized. Soz nigga

by_network = TRUE
cut_byNet = c("Cell C", "Vodacom") # Indices 1 & 3 in key table df_networKey

by_prepaid = FALSE # Testing that the filtering works when you false it
cut_byPrep = c("0")

by_age = TRUE
cut_byAge = c("18 - 24", "25 - 34", "35 - 44", "45 - 54", "55 - 64")

# by_geo = FALSE
# cut_byGeo = c() 

by_edu = FALSE
cut_byEdu = c()


# Reassign df names for single specific users to their user ID for group analysis
#----
assign("ad6d2bd6-3f29-4261-9025-01642c0e6f07" ,df_skrrr1)
assign("eec99158-8dfb-4e50-9de2-e590930e09b6" ,df_skrrr2)
assign("c702d848-2c04-4619-8a81-c5e18a7e6592" ,df_skrrr3)

vec_allUserIDs <- c(vec_allUserIDs, "eec99158-8dfb-4e50-9de2-e590930e09b6", "c702d848-2c04-4619-8a81-c5e18a7e6592")
vec_userIDsPlayed <- c(vec_userIDsPlayed, "eec99158-8dfb-4e50-9de2-e590930e09b6", "c702d848-2c04-4619-8a81-c5e18a7e6592")
vec_userIDsPostCut <- vec_userIDsPlayed
#rm(df_skrrr1, df_skrrr2, df_skrrr3)


# Dividing time range into analysable chunks
# First we do this for 3 users and then generalize approach for all users in specific case (can be by group or we can do all and group by analysis)
# Idea is import the data (dfs) made in Analysis script in a loop for analysis. Change the lines below to import properly 
list_uID <- vector("list", length = length(vec_userIDsPlayed))
for (k in seq_along(list_uID)){
  list_uID[[k]] <- get(vec_userIDsPlayed[k]) # Make list_uID list of all users by df name being ID and dfs being user df tingz as below
}

## Cut flow time
##---- 
# Cut by Language 
if (by_lang == TRUE){
  
  for (i in length(list_uID):1){
    if (length(cut_byLang) == 2){
      # Nothin changes because we are looking at both languages if the length is 2 (Eng + Zul)
    } else if (length(cut_byLang) == 1){
      if (by_lang == "English"){
        num_ID <- vec_userIDsPlayed[i]
        index <- which(df_allUserInfo$Id == num_ID)
        if (df_allUserInfo$LanguagePreferenceId[index] != "2f7e195d-8ddf-4b78-9ced-13f0a95dacc1"){
          list_uID[[i]] <- NULL
          vec_userIDsPostCut = vec_userIDsPostCut[vec_userIDsPostCut != vec_userIDsPostCut[i]]
        }
      } else if (by_lang == "Zulu"){
        num_ID <- vec_userIDsPlayed[i]
        index <- which(df_allUserInfo$Id == num_ID)
        if (df_allUserInfo$LanguagePreferenceId[index] != "c702d848-2c04-4619-8a81-c5e18a7e6592"){
          list_uID[[i]] <- NULL
          vec_userIDsPostCut = vec_userIDsPostCut[vec_userIDsPostCut != vec_userIDsPostCut[i]]
        }
      }
    }
  }
  
}
# end of Cut by Language

# Cut by Bank
if (by_bank == TRUE){
  
  for (i in length(list_uID):1){
    num_ID <- vec_userIDsPlayed[i]
    index <- which(df_allUserInfo$Id == num_ID)

      ind_equi <- which(df_allUserInfo$Bank[index] != cut_byBank)
      if (length(cut_byBank) == length(ind_equi)){
        list_uID[[i]] <- NULL
        vec_userIDsPostCut = vec_userIDsPostCut[vec_userIDsPostCut != vec_userIDsPostCut[i]]
      }

    
  }
  
}
# end of Cut by Bank

# Cut by Network
if (by_network == TRUE){
  
  for (i in length(list_uID):1){
    for (j in seq_along(cut_byNet)){
      num_ID <- vec_userIDsPlayed[i]
      index <- which(df_allUserInfo$Id == num_ID)
      
    }
  }
  
}
# end of Cut by Network

# Cut by Prepaid
if (by_prepaid == TRUE){
  for (i in length(list_uID):1){
    for (j in seq_along(cut_byPrep)){
      num_ID <- vec_userIDsPlayed[i]
      index <- which(df_allUserInfo$Id == num_ID)
      
    }
  }
  
}
# end of Cut by Prepaid

# Cut by Age
if (by_age == TRUE){
  
  for (i in length(list_uID):1){
    for (j in seq_along(cut_byAge)){
      num_ID <- vec_userIDsPlayed[i]
      index <- which(df_allUserInfo$Id == num_ID)
      
    }
  }
  
}
# end of Cut by Age

# Cut by Education
if (by_edu == TRUE){
  
  for (i in length(list_uID):1){
    for (j in seq_along(cut_byEdu)){
      num_ID <- vec_userIDsPlayed[i]
      index <- which(df_allUserInfo$Id == num_ID)
      
    }
  }
  
}
# end of Cut by Education

  
  