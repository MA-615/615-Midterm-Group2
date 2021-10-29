pacman::p_load('dplyr', 'tidyverse')

Pesticides <- read_csv("Pesticides.csv")

##I am sorry not use your data cause 5 data is too small. 
##I use the raw data and remove blank lines.
Pesti2 <- Pesticides %>% filter(!is.na(Pesticide))

##This is the data from Strawberry_wrangle_clean
data2 <- read_csv("cleanstr.csv")

data2<-data2[,-1]

##Rename to make two data frame has the same line title "Pesticide".
data2<- rename(data2,Pesticide=12)

##Change this table text to uppercase for easy matching.
Pesti2$Pesticide<-toupper(Pesti2$Pesticide)

##inner join two data tables
data_join<-inner_join(data2,Pesti2)

##write and save the merge data
write.csv(data_join, "data_join.csv")

##break up by state
california <- data_join %>% filter(State == "CALIFORNIA")
