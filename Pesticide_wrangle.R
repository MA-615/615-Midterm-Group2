#Pesticide Wrangle

pacman::p_load('dplyr', 'tidyverse')

Pesticides <- read_csv("Pesticides.csv")

# carcinogen = known or possible or probable
# hormone disruptor = suspected
# bee toxins = high

#Tidy table
Pesti <- Pesticides %>%
  filter(Carcinogen == "known" | Carcinogen == "possible" | 
           Carcinogen == "probable") %>%
  filter(`Hormone Disruptor` =="suspected") %>%
  filter(`Bee Toxins`=="high")

Pesti <- Pesti %>% rename('Chemical Name' = Pesticide )
