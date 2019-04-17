library(pacman) # Check https://github.com/trinker/pacman for pacman package use
p_load(readxl)
p_load(tidyverse)
p_load(dplyr)


# Let's make a script to learn for one human in question then move it to group analyses (Yaaas stats)

df_skrrr <- setNames(data.frame(matrix(ncol = 7, nrow = 0)), 
                     c("GameID",
                       "Date",
                       "Time",
                       "Category",
                       "Concept",
                       "Diff",
                       "Correct"))


path_df <- "/Users/skhathi/Documents/DataAnalysis/Ibumdlali/"
df_gameDataSpecUser <- as.data.frame(read_csv(paste0(path_df, "AllGamesForSpecificUser.csv", collapse = NULL)))

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




for (j in 1:nrow(df_gameDataSpecUser)){
  
  # next is a vector of the entries I need to append
  addition <- c(df_gameDataSpecUser$GameId[j], 
                gsub("\\ .*", "",df_gameDataSpecUser$DatePlayed[j]), # does some freaky shit when using as.Date so just remove anything following date
                df_gameDataSpecUser$DatePlayed[j], # this has the tiime but I'm not thinking about it too much. I'll sort when I do
                df_gameDataSpecUser$Category[j], 
                df_gameDataSpecUser$Concept[j], 
                df_gameDataSpecUser$Difficulty[j], 
                df_gameDataSpecUser$Correct[j] 
  )
  
  
  df_skrrr[nrow(df_skrrr)+1, ] <- addition # finally, addition of row works

}

