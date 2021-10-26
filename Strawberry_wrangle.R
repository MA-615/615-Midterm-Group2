# Strawberry Wrangle

library(tidyverse)
library(magrittr)

data <- read_csv("Strawberries.csv")


# data_1$Program <- ifelse(data_1$Program == 'CENSUS', 1, 0) #Change Program into binary variable 1-CENSUS, 0-SURVEY

# Drop no useful data
drop_no_info_cols <- function(df){
  cnames = colnames(data)
  T = NULL
  for(i in 1:ncol(df)){T <- c(T, nrow(unique(df[i])))}
  drop_cols <- cnames[which(T == 1)]
  return(select(df, !all_of(drop_cols)))
}

drop_no_info_cols(data)
data_1 <- drop_no_info_cols(data)

# Seperate Data Item into 4 column
data_1 %<>% separate(col = 'Data Item',
                    into = c("Strawberries", "Items", "Discription", "Units"),sep = ",",fill = "right")

distinct(data_1, Strawberries)

distinct(data_1, Items)

distinct(data_1, Discription)

distinct(data_1, Units)

View(data_1)

# Seperate the domain
data_1 %<>%  separate(col=Domain,
                      into = c("Status", "Type" ), 
                      sep = ",", 
                      fill = "right")
distinct(data_1, Status)
distinct(data_1, Type)


# make a copy of Domain Categoy

data_1 %<>% 
  mutate(Chemicals = 'Domain Category') %>% 
  relocate(Chemicals, .after = 'Domain Category') 


bb <- data_1$Chemicals %>% str_detect("CHEM")

sum(bb)

## index 
ind_C <- (!bb)*(1:dim(data_1)[1])

## 
r1 <- ind_C[ind_C > 0]

## set entries in Chemicals column to " " if they don't start with CHEM

data_1$Chemicals[r1] <- " "

# Seperate chemical 
data_1 %<>% separate(col = Chemicals,
                     into = c("title", "details"),
                     sep = ":",
                     fill = "right")

data_1 %<>% mutate(details = str_extract(str_trim(details) ,"[^(].*[^)]") )

data_1 %<>% mutate(type = str_trim(Type))

distinct(data_1, details)

distinct(data_1, type)

data_1 <- drop_no_info_cols(data_1)



# NA number checking 
# data_1_chem <- data_1 %>% filter((type=="FUNGICIDE")|
#                                    (type=="HERBICIDE")|
#                                    (type=="INSECTICIDE"))
# 
# data_1_other <- data_1 %>% filter(type=="OTHER")
# 
# data_1_herb <- data_1 %>% filter(type=="HERBICIDE")
# 
# data_1_na <- data_1 %>% filter(is.na(type)==TRUE)

# Seperate chemical data 
# distinct(data_1_herb, details)
# 
# distinct(data_1_chem, details)
# 
# distinct(data_1_other, details)
# 
# distinct(data_1_na, details)

# State data
# data_1_chem <- drop_no_info_cols(data_1_chem)
# 
# data_1_other <- drop_no_info_cols(data_1_other)
# 
# chem_list <- distinct(data_1_chem, details)
# 
# chem_list <- rbind2(chem_list,distinct(data_1_other, details))
# 
# distinct(data_1, State)
