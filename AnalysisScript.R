library(pacman) # Check https://github.com/trinker/pacman for pacman package use
p_load(readxl)
p_load(tidyverse)
p_load(dplyr)

# Do you want to make histograms? Make TRUE for yes and FALSE for no
make_hists <- FALSE


csv_dir <- "/Users/skhathi/Documents/DataAnalysis/SampleCSVs/"
dir_files <- list.files('/Users/skhathi/Documents/DataAnalysis/SampleCSVs/', all.files = FALSE, full.names = FALSE)
df_names <- vector()

# Reading the csv files in my csv dir. and saving dataframes as the names of files w/out ext (.csv)
# No use yet but it shall be important for analysis
for (i in 1:length(dir_files)) {
  df <- as.data.frame(read.csv(paste0(csv_dir, dir_files[i], collapse = NULL)))
  name_of_df <- gsub('.csv', '', dir_files[i])
  assign(name_of_df,df)
  rm(df)
  df_names <- c(df_names, name_of_df)
}


# Path name to where the Ib'umdlali csv files with questions is
path_df <- "/Users/skhathi/Documents/DataAnalysis/Ibumdlali/"
eng_path <- paste0(path_df, "English/", collapse = NULL)
zul_path <- paste0(path_df, "Zulu/", collapse = NULL)

# Files present in the English and Zulu directories
eng_files <- list.files(eng_path, all.files = FALSE, full.names = FALSE)
zul_files <- list.files(zul_path, all.files = FALSE, full.names = FALSE)

# Making dfs and histograms for English questions
eng_df_names <- vector()
for (i in 1:length(eng_files)) {
  df <- as.data.frame(read_csv2(paste0(eng_path, eng_files[i], collapse = NULL)))
  name_of_df <- gsub("\\-.*", "", eng_files[i])
  assign(name_of_df,df)
  
  if (make_hists == TRUE){
    diff_scores <- df %>% pull(`Difficulty rating (</= 20)`)
    png(filename = paste0(name_of_df, " hist.png", collapse = NULL))
    h <- hist(diff_scores, 
              xlab = "Difficulty rating", 
              ylab = "Difficulty frequency", 
              col = "grey", 
              border = "black", 
              breaks = 19, 
              main = paste0("Distribution of \n", name_of_df, collapse = NULL))
    xfit <- seq()
    
    
    dev.off()
    
    rm(df)
    eng_df_names <- c(eng_df_names, name_of_df)
  }
}

# Making dfs for Zulu questions
zul_df_names <- vector()
for (i in 1:length(zul_files)) {
  df <- as.data.frame(read_csv2(paste0(zul_path, zul_files[i], collapse = NULL)))
  name_of_df <- gsub("\\-.*", "", zul_files[i])
  assign(name_of_df,df)
  rm(df)
  zul_df_names <- c(zul_df_names, name_of_df) # For different kinds of string regex manipulation and replacement see 
  # https://stevencarlislewalker.wordpress.com/2013/02/13/remove-or-replace-everything-before-or-after-a-specified-character-in-r-strings/
  
}



# We do need to correct the names of the sheets in spreadsheets for consistency
# Also, downloading as xlsx shortens the names of sheets, why?
# Names of categories (should be a way to read them first and then save each name as string in vector)


# Histograms for the categories (just English) without different colours
#for (i in 1:length(eng_df_names)){
#v <- `Credit basics-20March2019-ENG` %>% pull(`Difficulty rating (</= 20)`)
#png(filename = "histogram.png")
#hist(v, xlab = "Difficulty rating", col = "red", border = "yellow", breaks = 19)
# Adjust number of breaks 


#dev.off()
#}


# Now for one user's analysis (using UserData.csv)
path_user <- "/Users/skhathi/Documents/DataAnalysis/Ibumdlali/"
df_allUsers <- as.data.frame(read_csv(paste0(path_user, "UserDataTable.csv", collapse = NULL))) # Reads incorrectly with read_csv2, but correct with ..._csv. What's the difference
df_gameData <- as.data.frame(read_csv(paste0(path_user, "groupedGameData.csv", collapse = NULL)))
vec_allUserIDs <- as.vector(df_allUsers$Id)


if ((anyDuplicated(vec_allUserIDs) == 0) == TRUE){
  print("All user IDs are unique")
} else {
  y <- anyDuplicated(vec_allUserIDs)
  print(paste("Not all user IDs are unique
              Please check users table on BigQuery for problem on line", y))
}

# Now to start populating one person's Ib'umdlali table to the max
for (i in 1:length(vec_allUserIDs)){ # change the 3 to length(vec_allUserIDs)
  if (exists(vec_allUserIDs[i])){ 
    # Nothing should happen if the df exists (we will append info to all dfs when we have all users extant in df)
  } else { # Make new df with user ID as the name if the df doesn't exist
    df_skrrr <- setNames(data.frame(matrix(ncol = 7, nrow = 0)), 
                         c("GameID",
                           "Date",
                           "Time",
                           "Category",
                           "Concept",
                           "Diff",
                           "Correct"))
    
    df_userDataEach <- vec_allUserIDs[i]
    assign(df_userDataEach,df_skrrr)
    rm(df_skrrr)
  }
  
  ind <- which(df_gameData$UserId == vec_allUserIDs[i]) # Index for the corresponding User IDs. 
  
  if (length(ind) > 0){
    for (j in 1:length(ind)){
      
      df_userDataEach <- vec_allUserIDs[i]
      df_userInQuestion <- get(df_userDataEach) # Get the df according to the variable name
      
      # next is a vector of the entries I need to append
      addition <- c(df_gameData$GameId[ind[j]], 
                    gsub("\\ .*", "",df_gameData$DatePlayed[ind[j]]), # does some freaky shit when using as.Date so just remove anything following date
                    sub('\\..*', '', (gsub(paste(as.Date(df_gameData$DatePlayed[ind[j]]), "", collapse = NULL), "", df_gameData$DatePlayed[ind[j]]))), # this has the tiime but I'm not thinking about it too much. I'll sort when I do
                    df_gameData$Category[ind[j]], 
                    df_gameData$Concept[ind[j]], 
                    df_gameData$Difficulty[ind[j]], 
                    df_gameData$Correct[ind[j]] 
      )
      
      df_userInQuestion[nrow(df_userInQuestion)+1, ] <- addition # finally, addition of row works
      df_userInQuestion$Diff <- as.integer(df_userInQuestion$Diff)
      
      df_nameSake <- vec_allUserIDs[i]
      assign(df_nameSake,df_userInQuestion)
      rm(df_userInQuestion)
      #df_nameSake <- df_nameSake[order(df_nameSake[2], df_nameSake[3]),]
    }
  } else {
    # Nothing is happening 
  }  
  
  
}





