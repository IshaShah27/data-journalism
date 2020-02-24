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

# keep 2019 only in one dataset, collapse other years
parks <- parks %>% 
  select("gispropnum", "omppropid", "gisobjid", "district", "sector", 
         "sector_name", "sector_desc", "activity", "animal_waste", 
         "broken_glass", "dumping", "graffiti", "medical_waste",
         "npop", "nhours", "ncrew", "nnpw", "nhours", "fiscal_qtr", 
         "date_worked") %>%
  mutate(year = as.numeric(substr(date_worked, 
                                  nchar(date_worked) - 3, 
                                  nchar(date_worked))),
         borough = substr(sector_name, 1, 
                          regexpr(" Sector", sector_name) - 1))

# explore data
summary(parks)
# looks like some of the key variables have NAs - but only for about 
# 1.5k out of 4.8M total
# investigate
head(parks[is.na(parks$npop),])
summary(parks[is.na(parks$npop),])
# doesn't look like there is anything significantly different about these 
# records, except that most also don't have a GIS object ID - 
# since we are focusing on geospatial patterns and crew, seems okay to drop these
parks <- parks %>% filter(!is.na(ncrew))

# there also seems to be a few different types of activities - work, lunch, etc 
table(parks$activity)
# looks like about 3.8M out of 4.8M total are work, but that the rest are other 
# let's limit to work - we do care about parks employees' breaks and mobilization,
# but maybe in another analysis where it is more relevant
parks <- parks[parks$activity == "Work",]

# we also want to make sure we have spatial data for each of these
table(parks$borough)
sum(is.na(parks$gisobjid))
# since only about 1.7k do not have a GIS object ID, let's drop those
parks <- parks[!is.na(parks$gisobjid),]
table(parks$borough)
head(parks[parks$borough == "",])
# there are still a small number of observations that don't have a borough
parks <- parks[parks$borough != "",]

# reformatting binary variables from Yes/No to 0/1 to make it easier to sum
parks <- mutate_at(parks, 
                   vars("animal_waste", "broken_glass", "dumping", "graffiti", 
                        "medical_waste"), 
                   function(x){as.numeric(x == "Yes")})

# create separate, more easily navigable datasets for exploration
# one that contains only 2019
parks_19 <- parks %>% filter(year == 2019) 

# one that contains all years, but summarized by park and year
parks_agg <- parks %>% 
  group_by(year, gispropnum, gisobjid, omppropid, sector, district, sector_name,
           sector_desc, activity, borough) %>%
  mutate(recs = 1) %>%
  summarize_at(c("animal_waste", "broken_glass", "dumping", "graffiti", 
                    "medical_waste", "npop", "nhours", "ncrew", "nnpw", "recs"),
               sum, na.rm = TRUE)

# attach some sense of how big these parks are and how trafficked they are
# found some additional data on opendata nyc on area of these parks
setwd("C:/Users/is2404/Downloads/data-journalism-master/data-journalism-master")
pdesc <- read.csv(file = "OpenData_ParksProperties.csv", 
                  stringsAsFactors = FALSE)
names(pdesc)

# drop some unnecessary variables from the descriptions dataset, then merge
pdesc <- select(pdesc, -c("PERMITDIST", "PERMITPARE", "COUNCILDIS", "PRECINCT",
                         "PERMIT", "SIGNNAME", "NYS_ASSEMB", "NYS_SENATE", 
                         "US_CONGRES"))
names(pdesc)

# merge
parks_geo <- left_join(parks_agg, pdesc, by = c("gispropnum" = "GISPROPNUM",
                                                "omppropid" = "OMPPROPID",
                                                "gisobjid" = "GISOBJID"))
head(parks_geo)
sum(is.na(parks_geo$OBJECTID))

parks_geo <- left_join(parks_agg, pdesc, by = c("gispropnum" = "GISPROPNUM"))
sum(is.na(parks_geo$OBJECTID))

head(filter(parks_geo, is.na(OBJECTID)))
# seems that about 12,767 records do not merge on - this is too many to proceed
# will go on with just the information we have

# create average amount of time, crew members, etc. per visit
parks_geo <- parks_geo %>%
  mutate(avg_ncrew = ncrew / recs,
         avg_nhours = nhours / recs,
         avg_nnpw = nnpw / recs)


# ==============
# exploration
# ==============

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

