# ============================
# project: analysis of daily
# tasks park cleaning records
# date start: 02/22/2020
# author: IS
# ============================

# libs
libs <- c("dplyr", "ggplot2", "tidyr", "stringr", "pastecs")

# install and load
install.packages(libs)
lapply(libs, library, character.only = T)

# imports
path <- "/Users/ishashah/Documents/Cover Letters and Resumes/Data Journalism Intern Test"
setwd(path)

# import parksleaning file
parks <-  read.csv(file = "Daily_Tasks_Park_Cleaning_Records.csv", stringsAsFactors = FALSE)

# exploration
head(parks)
# stat.desc(parks, basic=TRUE, desc=TRUE, norm=FALSE, p=0.95)

names(parks)

# keep 2019 only in one school, collapse other years
parks <- parks %>% 
  select("gispropnum", "omppropid", "gisobjid", "district", "sector", "sector_name", "sector_desc",
         "activity", "animal_waste", "broken_glass", "dumping", "graffiti", "medical_waste",
         "npop", "nhours", "ncrew", "nnpw", "nhours", "fiscal_qtr", "date_worked") %>%
  mutate(year = as.numeric(substr(fiscal_qtr, 1, 4)),
         qtr = as.numeric(substr(fiscal_qtr, 6, 6)))

park19 <- parks %>% filter(year == 2019)
parksum <- parks %>% group_by(year, )

# how much time on average is spent cleaning each park?

# adjusting for the area of the park
# does it differ by borough?
# which park has the most animal waste?
# requires the greatest average crew number?
# what are the tasks that happen more in certain more parks than others
## e.g., broken glass, confetti, medical waste, dumping
# when a clean is off-route, what time of day does it usually happen?
# what is the distribution of times that cleans usually happen?
# number of vehicles per crew - are some boroughs more or less well staffed than others?
# number of crew associated with each activity, both parks and non parks

