library(tidyverse)
library(magrittr)

# Original data input
data1 <- read_csv("data/Strawberries.csv")
data2 <- read_csv("data/Pesticides.csv")

#####################################################################
# data cleaning
drop_na_info <- function(df){
  # df <- data1
  cnames = colnames(df)
  T = NULL
  for(i in 1:ncol(df)){T <- c(T, nrow(unique(df[i])))}
  drop_cols <- cnames[which(T == 1)]
  return(select(df, !all_of(drop_cols)))
}

data1.1 <- drop_na_info(data1)

# Separate Data Item into 4 column
data1.1 %<>% separate(col = 'Data Item', into = c("Strawberries", "Items", "Discription", "Units"),sep = ",",fill = "right")

distinct(data1.1, Strawberries)

distinct(data1.1, Items)

distinct(data1.1, Discription)

distinct(data1.1, Units)


# Separate the domain
data1.1 %<>% separate(col = Domain, into = c("Title", "Details"), sep = ":", fill = "right")

distinct(data1.1, Title)

distinct(data1.1, Details)

data1.1 %<>% select(-Details)

# Separate Domain Category
data1.1 %<>% 
  mutate(Chemicals = `Domain Category`) %>%
  relocate(Chemicals, .after = `Domain Category`)

data1.1 %<>% 
  separate(Chemicals, into =c('Title', 'Details'), sep = ":", fill = "right")

data1.1$Details <- str_replace(data1.1$Details, "\\(", "")
data1.1$Details <- str_replace(data1.1$Details, "\\)", "")

data1.1 %<>% 
  separate(Details, into = c('Chemical Name', "Number"), sep = "=", fill = "right")

distinct(data1.1, `Chemical Name`)
distinct(data1.1, Number)

# Delete empty space
data1.1$`Chemical Name` <- str_trim(data1.1$`Chemical Name`)

data1.1 %<>% select(-c(Strawberries, `Domain Category`))
data1.1 <- drop_na_info(data1.1)


# Pesticides 
data2.1 <- data2 %>% rename('Chemical Name' = Pesticide )

data2.1 %<>% filter(!is.na(data2.1))

data2.1$`Chemical Name` <- toupper(data2.1$`Chemical Name`)

data2.1 <- data2.1 %>% mutate('Human Toxins' = NA)

# Define Human Toxins level
# high toxic for human: carcinogen = known
# moderate toxic for human: carcinogen = probable
# slight toxic for human: carcinogen= possible/NA
data2.1$`Human Toxins` <- ifelse(data2.1$Carcinogen == "known", "high", 
                                 ifelse(data2.1$Carcinogen == "possible", "slight", 
                                        ifelse(data2.1$Carcinogen == "possible", "slight", NA)))
# Also can use for loop? (did not success)

#####################################################################
# Combine two dataset
join <- full_join(data1.1, data2.1)

# Write the dataset into csv.
# write_csv(join, "final.csv")



