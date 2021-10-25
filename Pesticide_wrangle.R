#Pesticide Wrangle
library(dplyr)
library(tidyverse)
Pesticides <- read_csv("Pesticides.csv")

#carcinogen = known or possible or probable
## hormone disruptor = suspected
## neurotoxins = present or NA
## developmental or reproductive toxins = present
## bee toxins = slight or high or moderate
pesti1 <- Pesticides %>%
  filter(Carcinogen == "known" | Carcinogen == "possible" | Carcinogen == "probable")
pesti2 <- pesti1 %>%
  filter(`Hormone Disruptor` =="suspected" & `Developmental or Reproductive Toxins`=="present")
