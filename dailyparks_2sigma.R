# ============================
# project: analysis of daily
# tasks park cleaning records
# date start: 02/22/2020
# author: IS
# ============================

# libs
libs <- c("dplyr", "ggplot2", "tidyr", "stringr")

# install and load
install.packages(libs)
lapply(libs, library, character.only = T)

# imports
# path <- "/Users/ishashah/Documents/Cover Letters and Resumes/Data Journalism Intern Test"
path <- "C:/Users/is2404/Downloads"
setwd(path)

# import parks cleaning file
parks <-  read.csv(file = "Daily_Tasks_Park_Cleaning_Records.csv", 
                   stringsAsFactors = FALSE)

# ==============
# cleaning
# ==============

# exploration
head(parks)
names(parks)

# basic cleaning - drop unneeded variables, extract year and borough
parks <- parks %>% 
  select("gispropnum", "omppropid", "gisobjid", "district", "sector", 
         "sector_name", "sector_desc", "activity", "animal_waste", 
         "broken_glass", "dumping", "graffiti", "medical_waste",
         "nhours", "ncrew", "nnpw", "nhours", "fiscal_qtr", 
         "date_worked") %>%
  mutate(year = as.numeric(substr(date_worked, 
                                  nchar(date_worked) - 3, 
                                  nchar(date_worked))),
         borough = substr(sector_name, 1, 
                          regexpr(" Sector", sector_name) - 1),
         ntothours = nhours * (ncrew + nnpw)) %>%
  mutate_at(vars("animal_waste", "broken_glass", "dumping", "graffiti", 
                 "medical_waste"), function(x){as.numeric(x == "Yes")})

summary(parks, na.rm = TRUE)
# looks like some of the key geographic variables have NAs - but only for about 
# 1.5k out of 4.8M total
# investigate
head(parks[is.na(parks$ntothours),])
summary(parks[is.na(parks$ntothours),])
# doesn't look like there is anything significantly different about these 
# records, except that most also don't have a GIS object ID - 
# since we are focusing on geospatial patterns and crew, seems okay to drop these
parks <- parks %>% filter(!is.na(ntothours))

# we also want to make sure we have spatial data for each of these
table(parks$borough)
sum(is.na(parks$gisobjid))
# since only about 1.7k do not have a GIS object ID, let's drop those
parks <- parks[!is.na(parks$gisobjid),]
table(parks$borough)
head(parks[parks$borough == "",])
# there are still a small number of observations that don't have a borough
parks <- parks[parks$borough != "",]

# there also seems to be a few different types of activities - work, lunch, etc 
table(parks$activity)
# looks like about 3.8M out of 4.8M total are work, but that the rest are other 
# let's limit to work - we do care about parks employees' breaks!
# but maybe in another analysis where it is more relevant
parks <- parks[parks$activity == "Work",]

# now, turn attention to outliers
summary(parks[is.na(parks$ntothours),])

# see how many records there are with more than 8 nhours and 10 ncrew
count(parks[parks$nhours > 8 & parks$ncrew > 10,])
# 1553!

# see which sectors these are in
table(filter(parks, ncrew > 10 & nhours > 8) %>% select(gispropnum))
# from a quick glance, these seem to make sense - these codes are for
# union square park, bryant park, etc - parks that have a lot of foot traffic

# look at outliers
boxplot(parks$ncrew)
boxplot(parks$nhours)
boxplot(parks$ntothours)

# ok, based on a glance, choose cutoffs for outliers
parks <- parks %>%
  filter(ncrew < 20 & nhours < 10 & ntothours < 200)
# did not use


# create separate, more easily navigable datasets for exploration

# one that contains only 2019
parks_19 <- parks %>% filter(year == 2019) 

# one that contains all years, but summarized by park and year
parks_agg <- parks %>% 
  group_by(year, gispropnum, gisobjid, omppropid, sector, district, sector_name,
           sector_desc, activity, borough) %>%
  mutate(recs = 1) %>%
  summarize_at(c("animal_waste", "broken_glass", "dumping", "graffiti", 
                 "medical_waste", "ntothours", "nhours", "ncrew", "nnpw", 
                 "recs"),
               sum, na.rm = TRUE) %>%
  ungroup() %>%
  # create average number of crew, hours worked for a given park/year combination
  mutate(avg_ncrew = ncrew / recs,
         avg_nhours = nhours / recs, # this is hours per person
         avg_nnpw = nnpw / recs,
         avg_ntothours = ntothours / recs) # this is total person-hours

# attach some sense of how big these parks are 
# found some additional data on opendata nyc on area of these parks
# setwd("C:/Users/is2404/Downloads/data-journalism-master/data-journalism-master")
pdesc <- read.csv(file = "OpenData_ParksProperties.csv", 
                  stringsAsFactors = FALSE)
names(pdesc)

# drop some unnecessary variables from the descriptions dataset, then merge
pdesc <- select(pdesc, -c("PERMITDIST", "PERMITPARE", "COUNCILDIS", "PRECINCT",
                         "PERMIT", "SIGNNAME", "NYS_ASSEMB", "NYS_SENATE", 
                         "US_CONGRES"))
names(pdesc)

# merge

parks_geo <- left_join(parks_agg, pdesc, by = c("gispropnum" = "GISPROPNUM"))
sum(is.na(parks_geo$OBJECTID))

head(filter(parks_geo, is.na(GISPROPNUM)))
# seems that about 12,700 records do not merge on - this is too many to proceed
# will go on with just the information we have from the original dataset

parks_19 <- left_join(parks_19, pdesc, by = c("gispropnum" = "GISPROPNUM"))
sum(is.na(parks_19$OBJECTID))
# about 20% of dataset did not merge on
# also too many to proceed with geo information, will use only original for now


# ==============
# exploration
# ==============

# how many person-hours did crew spend maintaining parks in 2019?
parks_19 %>% summarize(tothours = sum(ntothours))

# which parks required the most time?
parks_temp <- parks_19 %>% 
  ungroup() %>% group_by(gispropnum, NAME311) %>% 
  summarize(tothours = sum(ntothours)) %>% 
  arrange(desc(tothours))
parks_temp <- parks_temp[1:10,]

parks_temp$NAME311 <- factor(parks_temp$NAME311, levels = parks_temp$NAME311[order(desc(parks_temp$tothours))])

ggplot(parks_temp, aes(x = NAME311, y = tothours)) +
  geom_col(fill = "forestgreen") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Park name", y = "Total person-hours", 
       title = "Top 10 parks by person-hours of cleaning, 2019")
  
saveRDS(parks_19, file = "parks_19.rds")

saveRDS(parks_agg, file = "parks_agg.rds")

saveRDS(parks, file = "parks.rds")


# repeat for hours per person rather than total person-hours
parks_temp <- parks_19 %>% 
  ungroup() %>% group_by(gispropnum, NAME311) %>% 
  summarize(tot_nhours = sum(nhours),
            recs = n()) %>% 
  mutate(avg_nhours_visit = tot_nhours / recs) %>%
  arrange(desc(avg_nhours_visit))
parks_temp <- parks_temp[1:10,]

parks_temp$NAME311 <- factor(parks_temp$NAME311, levels = parks_temp$NAME311[order(desc(parks_temp$avg_nhours))])

ggplot(parks_temp, aes(x = NAME311, y = avg_nhours)) +
  geom_col(fill = "forestgreen") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Park name", y = "Total person-hours", 
       title = "Top 10 parks by person-hours of cleaning, 2019")

# first, questions about time series
# what is the average vsit length in each year, and has this been changing?
park_temp <- parks_geo %>%
  ungroup() %>% group_by(year) %>%
  summarize_at(vars(starts_with("avg_")), mean, na.rm = TRUE)

# pivot to long
# which park has the greatest time to clean?
# what factors contribute the most to park maintenance time?

# how much time on average is spent cleaning each park?
p <- ggplot(park_temp, aes(x = year)) +
  geom_line(aes(y = avg_nhours)) +
  geom_line(aes(y = avg_ncrew))
p


p <- ggplot(parks_geo, aes(x = borough, y = nhours)) +
  geom_point()
p

p <- ggplot(parks_geo, aes(x = borough, y = ncrew)) +
  geom_point()
p


p <- ggplot(parks_geo, aes(x = ACRES, y = npop)) +
  geom_point()
p

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

