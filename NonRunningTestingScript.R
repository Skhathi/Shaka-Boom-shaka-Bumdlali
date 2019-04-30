
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
    
    if (df_allUserInfo$Bank[index] %in% cut_byBank == FALSE){
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
  