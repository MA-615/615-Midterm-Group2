pacman::p_load('dplyr', 'tidyverse')

data_join <- read_csv("data_join.csv")

data_5_column <- data_join[,c(1,2,8,11,15)]

write.csv(data_5_column, "data_5_column.csv",row.names=F)
##only select 5 columns 
##year, state, pesticide, carcinogen, bee toxin