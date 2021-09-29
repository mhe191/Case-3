library(tidyverse)
library(data.table)
library(cowplot)
library(corrr)
library(readxl)
library(httr)

co2 <- fread("https://github.com/owid/co2-data/raw/master/owid-co2-data.csv")
codebook <- fread("https://github.com/owid/co2-data/raw/master/owid-co2-codebook.csv")

with(co2, table(country))
