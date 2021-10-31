library(tidyverse)
library(magrittr)
library(dplyr)

# Original data input
strawb <- read_csv("data/Strawberries.csv")
pesti <- read_csv("data/Pesticides.csv")

#####################################################################
# data cleaning about strwb
## drop columns which have only one value
drop_na_info <- function(df){
 # df <- strawb
  cnames = colnames(df)
  T = NULL
  for(i in 1:ncol(df)){T <- c(T, nrow(unique(df[i])))}
  drop_cols <- cnames[which(T == 1)]
  return(dplyr::select(df, !all_of(drop_cols)))
}

strawb1 <- drop_na_info(strawb)

## Separate Data Item into 4 column
strawb1 %<>% separate(col = 'Data Item', into = c("Strawberries", "Items", "Discription", "Units"),sep = ",",fill = "right")

distinct(strawb1, Strawberries)

distinct(strawb1, Items)

distinct(strawb1, Discription)

distinct(strawb1, Units)

distinct(strawb1,`Domain Category`)
# Separate the domain
strawb1 %<>% separate(col = Domain, into = c("dname", "type"), sep = ",", fill = "right")

distinct(strawb1, dname)

distinct(strawb1, type)

#strawb1 %<>% select(-Details)

# Separate Domain Category
strawb1 %<>% 
  mutate(Chemicals = `Domain Category`) %>%
  relocate(Chemicals, .after = `Domain Category`)

strawb1 %<>% 
  separate(Chemicals, into =c('Title', 'Details'), sep = ":", fill = "right")

distinct(strawb1, Details)

strawb1$Details <- str_replace(strawb1$Details, "\\(", "")
strawb1$Details <- str_replace(strawb1$Details, "\\)", "")

strawb1 %<>% 
  separate(Details, into = c('Chemical Name', "Number"), sep = "=", fill = "right")

distinct(strawb1, `Chemical Name`)
distinct(strawb1, Number)

# Delete empty space
strawb1$`Chemical Name` <- str_trim(strawb1$`Chemical Name`)

drops <- c("Strawberries", "Domain Category")
strawb1 <- strawb1[ , !(names(strawb1) %in% drops)]

# Pesticides 
pesti1 <- pesti %>% rename('Chemical Name' = Pesticide )

pesti1 %<>% filter(!is.na(pesti1))

pesti1$`Chemical Name` <- toupper(pesti1$`Chemical Name`)

# Define Human Toxins level
# high toxic for human: carcinogen = known or Neurotoxins/`Developmental or Reproductive Toxins` = present
# moderate toxic for human: carcinogen = probable/possible and `Hormone Disruptor` = suspect
# slight toxic for human: carcinogen= possible/possible or `Hormone Disruptor` = suspect, only one happens
pesti1 <- pesti1 %>% mutate('Human Toxins' = case_when(
  pesti1$Carcinogen == "known" | pesti1$Neurotoxins== "present" | pesti1$`Developmental or Reproductive Toxins`== "present" ~ "high" ,
  pesti1$Carcinogen == "possible" & pesti1$`Hormone Disruptor`=="suspected" & is.na(pesti1$Neurotoxins) & is.na(pesti1$`Developmental or Reproductive Toxins`)~ "moderate",
  pesti1$Carcinogen == "possible" & is.na(pesti1$`Hormone Disruptor`) & is.na(pesti1$Neurotoxins) & is.na(pesti1$`Developmental or Reproductive Toxins`)~ "slight",
  pesti1$Carcinogen == "probable" & is.na(pesti1$`Hormone Disruptor`) & is.na(pesti1$Neurotoxins) & is.na(pesti1$`Developmental or Reproductive Toxins`)~ "slight",
  is.na(pesti1$Carcinogen) & pesti1$`Hormone Disruptor`=="suspected" & is.na(pesti1$Neurotoxins) & is.na(pesti1$`Developmental or Reproductive Toxins`)~"slight"
))
                                      
# Also can use for loop? 

#####################################################################
# Combine two dataset
strwbPesti <- left_join(strawb1, pesti1,by="Chemical Name")

# Write the dataset into csv.
# write_csv(join, "final.csv")



