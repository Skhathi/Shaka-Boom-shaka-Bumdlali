library(pacman) # Check https://github.com/trinker/pacman for pacman package use
p_load(readxl)
p_load(tidyverse)
p_load(dplyr)

# Doesn't work on it's own. Simply to copy paste stuff to test on the actual script. Lot's of random code to follow. 

# 17 April 2019 test on Game ID in actual script. ID's are unique for the 1000 users (no repeats and they're)...
# supposed to reapeat about 4 times because each game has 4 rounds. This is to fix that. Confirm fix when complete

df_filteredUserID <- df_gameData %>% filter(UserId == "ad6d2bd6-3f29-4261-9025-01642c0e6f07")
View(df_filteredUserID)

# Put a break on this, technical error thing and haven't started working on actual learning. 
# Cobus will help you with this, and it may not even be a problem because it may be sequelized at the end.