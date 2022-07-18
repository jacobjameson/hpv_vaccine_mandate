################################################################################
# Purpose:      Get hpv vaccine and population data from Marketscan
# Author:       Jacob Jameson
#
# Last Updated: 7/16/2022
################################################################################

library(tidyverse)
library(foreign)
library(haven)

marketscan <- ("X:/MarketScan/ccaeo")
setwd("X:/staff/jjameson/HPV Vaccine Access/data")

vars.keep <- c('enrolid', 'egeoloc', 'age', 'sex', 'year')

region.a <- c('04', '05', '06', '07', '08', '09', '12')

region.b <- c('32', '31', '35', '13', '38', '39', '11')

for (x in c('061', '071','081', '091', '101', '111', '121', '131',
            '141', '151', '161', '171', '181')){
  
  filename <- paste0(x, '.sas7bdat')
  
  data <- read_sas(paste0(marketscan, filename)) %>%
    filter(RX == "1", EIDFLAG == "1", PROCTYP=="1", 
           (PROC1 == "90649" | PROC1 == "90650" ), 
           (EGEOLOC %in% region.a | EGEOLOC %in% region.b),
           AGE <= 20, AGE >= 9) %>%
    select(vars.keep)
  
  names(data) <- tolower(names(data))
  
  name <- paste0('hpv', x)
  
  write.csv(data, name, row.names = FALSE)
  
  rm(data)
  
  data <- read_sas(paste0(marketscan, filename)) %>%
    filter(AGE <= 20, AGE >= 9, (EGEOLOC %in% region.a | EGEOLOC %in% region.b)) %>%
    select(vars.keep)
  
  names(data) <- tolower(names(data))
  
  data <- data %>% filter(duplicated(enrolid) == FALSE) %>%
    mutate(male = ifelse(sex == "1", 1, 0),
           female = ifelse(sex == "2", 1, 0))
  
  data <- data %>%
    group_by(egeoloc, age) %>%
    summarize(male = sum(male), female = sum(female), year = mean(year))
  
  name <- paste0('counts_', x)
  
  write.csv(data, name, row.names = FALSE)
  
}
