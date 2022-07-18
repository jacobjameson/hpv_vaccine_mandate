################################################################################
# Purpose: Construct Analytical Dataset (HPV Vaccine Study)
# Author: Jacob Jameson
#
# Last Updated: 7/15/2022
################################################################################

library(tidyverse)
library(foreign)
library(gtsummary)
library(haven)


setwd("X:/staff/jjameson/HPV Vaccine Access/data")


# Variables used for analysis
vars.keep <- c('enrolid', 'egeoloc', 'age', 'sex', 'year')


# Names of the datafiles
hpv.years <- c('hpv062', 'hpv071', 'hpv081', 'hpv091', 'hpv101',
               'hpv111', 'hpv121', 'hpv131', 'hpv141',
               'hpv151', 'hpv161', 'hpv171', 'hpv181')

counts.years <- c('counts_071', 'counts_071', 'counts_081', 'counts_091', 
                  'counts_101','counts_111', 'counts_121', 'counts_131', 
                  'counts_141', 'counts_151', 'counts_161', 'counts_171', 
                  'counts_181')

region.a <- c('04', '05', '06', '07', '08', '09', '12')

region.b <- c('32', '31', '35', '13', '38', '39', '11')


################################################################################
################################################################################

# Construct dataframe of all years of HPV vaccination (ages 9-20)
hpv.df <- data.frame()
for (x in hpv.years){
  
  filename <-  paste0(x,'.dta')
  
  print(paste0('loading ', filename))
  df.temp <- read_dta(filename)[vars.keep]
  df.temp <- df.temp %>%
    filter(age <= 20, age >= 9, 
           (egeoloc %in% region.a | egeoloc %in% region.b))
  
  print(paste0('Appending ', filename))
  hpv.df <- rbind(hpv.df, df.temp)
  
  rm(df.temp)
  }

# Only keep vaccine initiation in each year 
hpv.df <- hpv.df %>%
  group_by(year) %>%
  filter(duplicated(enrolid) == FALSE) %>% 
  ungroup()


# Determine sex and number of people in each (state, year) that are
# re-occurring in a future year. This would mean that they would not 
# initiate vaccination in the future because they already have in prior year
hpv.df <- hpv.df %>% 
  mutate(prior.vaccinated = ifelse(duplicated(enrolid) == T, 1, 0))


################################################################################
################################################################################

# Construct dataframe for the number of unique counts of each sex in each state
count.df <- data.frame()
for (x in counts.years){
  
  filename <-  paste0(x,'.dta')
  
  print(paste0('loading ', filename))
  df.temp <- read_dta(filename)

  print(paste0('Appending ', filename))
  count.df <- rbind(count.df, df.temp)
  
  rm(df.temp)
}


################################################################################
################################################################################

# Subtract the patients that appear in multiple years in the HPV dataset from
# the counts dataset. This way each years' population only refers to those that
# were not vaccinated in a previous wave.

pts.subtract <- hpv.df %>%
  filter(prior.vaccinated == 1) %>%
  mutate(male = ifelse(sex == 1, 1, 0),
         female = ifelse(sex == 2, 1, 0)) %>% 
  select(egeoloc, age, male, female, year) %>%
  group_by(year, age, egeoloc) %>%
  summarise(male_neg = sum(male)*-1, female_neg = sum(female)*-1) %>% 
  ungroup()


# Get the final population numbers while omitting prior vaccinated pts
final.pop <- merge(count.df, pts.subtract, 
                   by = c('egeoloc', 'age', 'year'), all=TRUE)

final.pop$male_neg = ifelse(is.na(final.pop$male_neg), 
                            0, final.pop$male_neg)

final.pop$female_neg = ifelse(is.na(final.pop$female_neg),
                              0, final.pop$female_neg)

final.pop <- final.pop %>%
  mutate(male = male + male_neg, female = female + female_neg) %>%
  select(egeoloc, age, male, female, year)


RI.ref <- c('04', '05', '06', '07', '09', '12')
VA.ref <- c('32', '35', '13', '39', '11')
DC.ref <- c('32', '35', '13', '39', '11')

final.pop %>%
  filter(egeoloc %in% RI.ref) %>%
  group_by(year, age)
  

# Create the final dataset for analysis
pts.vaccinated <- hpv.df %>%
  filter(prior.vaccinated == 0) %>%
  mutate(male = ifelse(sex == 1, 1, 0),
         female = ifelse(sex == 2, 1, 0)) %>% 
  select(egeoloc, age, male, female, year) %>%
  group_by(year, age, egeoloc) %>%
  summarise(male_vax = sum(male), female_vax = sum(female)) %>% 
  ungroup()


final.df <- merge(final.pop, pts.vaccinated, 
                  by = c('egeoloc', 'age', 'year'), all=TRUE)

final.df$male_vax = ifelse(is.na(final.df$male_vax), 
                            0, final.df$male_vax)

final.df$female_vax = ifelse(is.na(final.df$female_vax),
                              0, final.df$female_vax)

final.df <- final.df %>%
  mutate(age_group = case_when(age == 9 | age == 10 ~ "9-10",
                               age >= 11 & age <= 14 ~ "11-14",
                               age >= 15 ~ "15-18")) %>%
  group_by(egeoloc, age_group, year) %>% 
  summarise(male_vax = sum(male_vax), female_vax = sum(female_vax),
            male = sum(male), female = sum(female)) %>% ungroup() %>%
  mutate(prop.vax = (male_vax + female_vax)/(male + female),
         prop.vax.male = male_vax/male,
         prop.vax.female = female_vax/female)


final.va.ref <- final.df %>%
  filter(egeoloc %in% VA.ref) %>% group_by(year, age_group) %>%
  summarise(year = mean(year), prop.vax = mean(prop.vax), 
            prop.vax.male = mean(prop.vax.male), 
            prop.vax.female = mean(prop.vax.female),
            egeoloc = 'VA Ref')

final.ri.ref <- final.df %>%
  filter(egeoloc %in% RI.ref) %>% group_by(year, age_group) %>%
  summarise(year = mean(year), prop.vax = mean(prop.vax), 
            prop.vax.male = mean(prop.vax.male), 
            prop.vax.female = mean(prop.vax.female),
            egeoloc = 'RI Ref')


final.df.2 <- final.df %>%
  filter(egeoloc %in% c('08', '31', '38'))


final.df <- rbind(final.va.ref, final.df.2)
final.df <- rbind(final.df, final.ri.ref)


write.csv(final.df, 'Final_data.csv', row.names = F)

