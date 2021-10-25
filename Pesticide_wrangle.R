#Pesticide Wrangle
library(dplyr)
library(tidyverse)
Pesticides <- read_csv("Pesticides.csv")

#carcinogen = known or possible or probable
## hormone disruptor = suspected
## neurotoxins = present or NA
## developmental or reproductive toxins = present
## bee toxins = slight or high or moderate
Pesti <- Pesticides %>%
  filter(Carcinogen == "known" | Carcinogen == "possible" | Carcinogen == "probable") %>%
  filter(`Hormone Disruptor` =="suspected") %>%
  filter(`Bee Toxins`=="high")
