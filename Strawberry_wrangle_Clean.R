
# Clean Version for Strawberry data wrangle

library(tidyverse)
library(magrittr)

# Orginal data 
data <- read_csv("Strawberries.csv")

#####################################################################
# Drop unuseful data
drop_na_info <- function(df){
  cnames = colnames(data)
  T = NULL
  for(i in 1:ncol(df)){T <- c(T, nrow(unique(df[i])))}
  drop_cols <- cnames[which(T == 1)]
  return(select(df, !all_of(drop_cols)))
}

drop_na_info(data)
data_1 <- drop_na_info(data)

#####################################################################
# Separate Data Item into 4 column
data_1 %<>% separate(col = 'Data Item', into = c("Strawberries", "Items", "Discription", "Units"),sep = ",",fill = "right")

distinct(data_1, Strawberries)

distinct(data_1, Items)

distinct(data_1, Discription)

distinct(data_1, Units)

#####################################################################
# Separate the domain
data_1 %<>%  separate(col=Domain, into = c("Status", "Type" ), sep = ",", fill = "right")

distinct(data_1, Status)

distinct(data_1, Type)

data_1 %<>% separate(col = `Domain Category`, into = c("Chemicals Name", "Number"), sep = "=", fill = "right")

data_1$`Chemicals Name`<-str_replace(data_1$`Chemicals Name`,"\\)","")

data_1$Number<-str_replace(data_1$Number,"\\)","")

data_1 %<>% separate(col = `Chemicals Name`, into = c("unuse", "Chemicals Names"), sep = "\\(", fill = "left")

data_1<-subset(data_1, select = -unuse )

#####################################################################
##Change the character type to a number
data_1$Number<-as.numeric(data_1$Number)

#Change ‘Program’ into binary variable： 1-CENSUS, 0-SURVEY
data_1$Program <- ifelse(data_1$Program == 'CENSUS', 1, 0)
drop_na_info(data_1)

#####################################################################
# Final outcome
view(data_1)
