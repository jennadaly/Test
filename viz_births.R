library(dplyr)
#Setup environment
sub_folders <- list.files()
raw_data_location <- grep("raw", sub_folders, value=T)
data_location <- grep("data$", sub_folders, value=T)
path_to_data <-  (paste0(getwd(), "/", data_location))

#Create availability table to Demographics Data
##########################################################################################################################################

#Bring in individual aggregations
demo_1 <- read.csv(paste0(path_to_data, "/", "maternal_characteristics_demographics_1-Year.csv"), stringsAsFactors = F, header=T)
demo_3 <- read.csv(paste0(path_to_data, "/", "maternal_characteristics_demographics_3-Year.csv"), stringsAsFactors = F, header=T)
demo_5 <- read.csv(paste0(path_to_data, "/", "maternal_characteristics_demographics_5-Year.csv"), stringsAsFactors = F, header=T)

#Combine all aggregations
demo_all <- rbind(demo_1, demo_3, demo_5)

#Isolate to latest years and numbers only (no percent)
years <- c("2014", "2012-2014", "2010-2014")
demo_select <- demo_all[demo_all$Measure.Type == "Number" & demo_all$Year %in% years,]
demo_select$Year <- factor(demo_select$Year, levels = c("2014", "2012-2014", "2010-2014"))

#Create column that checks to see if data is available for a given year
demo_select2 <- demo_select %>% 
  select(-c(Measure.Type, FIPS)) %>% 
  arrange(Town, Birth.Weight, Gestational.Age, Mother.s.Age, Mother.s.Race.Ethnicity) %>% 
  group_by(Town, Birth.Weight, Gestational.Age, Mother.s.Age, Mother.s.Race.Ethnicity, Year) %>% 
  mutate(value_avail = ifelse((Value > 0), 1, 0)) 

#Based on availability column, finds groups that have data vs groups with no data
demo_select3 <- demo_select2 %>% 
  group_by(`Town`, Birth.Weight, Gestational.Age, Mother.s.Age, Mother.s.Race.Ethnicity) %>% 
  mutate(max_value_avail = max(value_avail))

#Conditionally assigns year_avail based on comparison of availabilty column and max column
demo_select4 <- demo_select3 %>% 
  mutate(year_avail = ifelse((value_avail == 0 & max_value_avail == 0), "None", NA), 
         year_avail = ifelse((value_avail == 0 & max_value_avail == 1), NA, NA), 
         year_avail = ifelse((value_avail == 1 & max_value_avail == 1), Year, NA))

#Assigns all NAs to 4 (so they may be sorted)
demo_select4[is.na(demo_select4)] <- 4

#Create column that reflects min year in which data is available
demo_select5 <- demo_select4 %>% 
  group_by(`Town`, Birth.Weight, Gestational.Age, Mother.s.Age, Mother.s.Race.Ethnicity) %>% 
  mutate(min_year = min(year_avail))

#Set up for assignments
demo_select5 <- as.data.frame(demo_select5)

#Assign year available based on value of min year column
demo_select6 <- demo_select5 %>% 
  mutate(Earliest.Data.Available = ifelse(min_year  == 1, "2014",
                    ifelse(min_year== 2, "2012-2014",
                    ifelse(min_year == 3, "2010-2014",
                    ifelse(min_year == 4, NA, NA)))))

#Remove extra columns
demo_available <- demo_select6 %>% 
  select(-c(year_avail, Year, Variable, Value, value_avail, max_value_avail, year_avail, min_year))

#Remove duplicates (isolate latest year available)
demo_available <- demo_available[!duplicated(demo_available), ]

#Create availability table to Behaviors Data
##########################################################################################################################################
#Bring in individual aggregations
behav_1 <- read.csv(paste0(path_to_data, "/", "maternal_characteristics_behavior_1-Year.csv"), stringsAsFactors = F, header=T)
behav_3 <- read.csv(paste0(path_to_data, "/", "maternal_characteristics_behavior_3-Year.csv"), stringsAsFactors = F, header=T)
behav_5 <- read.csv(paste0(path_to_data, "/", "maternal_characteristics_behavior_5-Year.csv"), stringsAsFactors = F, header=T)

#Combine all aggregations
behav_all <- rbind(behav_1, behav_3, behav_5)

#Isolate to latest years and numbers only (no percent)
years <- c("2014", "2012-2014", "2010-2014")
behav_select <- behav_all[behav_all$Measure.Type == "Number" & behav_all$Year %in% years,]
behav_select$Year <- factor(behav_select$Year, levels = c("2014", "2012-2014", "2010-2014"))

#Create column that checks to see if data is available for a given year
behav_select2 <- behav_select %>% 
  select(-c(Measure.Type, FIPS)) %>% 
  arrange(Town, Birth.Weight, Gestational.Age, Initiation.of.Prenatal.Care, Smoking.During.Pregnancy) %>% 
  group_by(Town, Birth.Weight, Gestational.Age, Initiation.of.Prenatal.Care, Smoking.During.Pregnancy, Year) %>% 
  mutate(value_avail = ifelse((Value > 0), 1, 0)) 

#Based on availability column, finds groups that have data vs groups with no data
behav_select3 <- behav_select2 %>% 
  group_by(`Town`, Birth.Weight, Gestational.Age, Initiation.of.Prenatal.Care, Smoking.During.Pregnancy) %>% 
  mutate(max_value_avail = max(value_avail))

#Conditionally assigns year_avail based on comparison of availabilty column and max column
behav_select4 <- behav_select3 %>% 
  mutate(year_avail = ifelse((value_avail == 0 & max_value_avail == 0), "None", NA), 
         year_avail = ifelse((value_avail == 0 & max_value_avail == 1), NA, NA), 
         year_avail = ifelse((value_avail == 1 & max_value_avail == 1), Year, NA))

#Assigns all NAs to 4 (so they may be sorted)
behav_select4[is.na(behav_select4)] <- 4

#Create column that reflects min year in which data is available
behav_select5 <- behav_select4 %>% 
  group_by(`Town`, Birth.Weight, Gestational.Age, Initiation.of.Prenatal.Care, Smoking.During.Pregnancy) %>% 
  mutate(min_year = min(year_avail))

#Set up for assignments
behav_select5 <- as.data.frame(behav_select5)

#Assign year available based on value of min year column
behav_select6 <- behav_select5 %>% 
  mutate(Earliest.Data.Available = ifelse(min_year  == 1, "2014",
                                   ifelse(min_year== 2, "2012-2014",
                                   ifelse(min_year == 3, "2010-2014",
                                   ifelse(min_year == 4, NA, NA)))))

#Remove extra columns
behav_available <- behav_select6 %>% 
  select(-c(year_avail, Year, Variable, Value, value_avail, max_value_avail, year_avail, min_year))

#Remove duplicates (isolate latest year available)
behav_available <- behav_available[!duplicated(behav_available), ]

#Create availability table to Socioeconomic Data
##########################################################################################################################################
#Bring in individual aggregations
socio_1 <- read.csv(paste0(path_to_data, "/", "maternal_characteristics_background_1-Year.csv"), stringsAsFactors = F, header=T)
socio_3 <- read.csv(paste0(path_to_data, "/", "maternal_characteristics_background_3-Year.csv"), stringsAsFactors = F, header=T)
socio_5 <- read.csv(paste0(path_to_data, "/", "maternal_characteristics_background_5-Year.csv"), stringsAsFactors = F, header=T)

#Combine all aggregations
socio_all <- rbind(socio_1, socio_3, socio_5)

#Isolate to latest years and numbers only (no percent)
years <- c("2014", "2012-2014", "2010-2014")
socio_select <- socio_all[socio_all$Measure.Type == "Number" & socio_all$Year %in% years,]
socio_select$Year <- factor(socio_select$Year, levels = c("2014", "2012-2014", "2010-2014"))

#Create column that checks to see if data is available for a given year
socio_select2 <- socio_select %>% 
  select(-c(Measure.Type, FIPS)) %>% 
  arrange(Town, Birth.Weight, Gestational.Age, Mother.s.Education, Mother.s.Marital.Status) %>% 
  group_by(Town, Birth.Weight, Gestational.Age, Mother.s.Education, Mother.s.Marital.Status, Year) %>% 
  mutate(value_avail = ifelse((Value > 0), 1, 0)) 

#Based on availability column, finds groups that have data vs groups with no data
socio_select3 <- socio_select2 %>% 
  group_by(`Town`, Birth.Weight, Gestational.Age, Mother.s.Education, Mother.s.Marital.Status) %>% 
  mutate(max_value_avail = max(value_avail))

#Conditionally assigns year_avail based on comparison of availabilty column and max column
socio_select4 <- socio_select3 %>% 
  mutate(year_avail = ifelse((value_avail == 0 & max_value_avail == 0), "None", NA), 
         year_avail = ifelse((value_avail == 0 & max_value_avail == 1), NA, NA), 
         year_avail = ifelse((value_avail == 1 & max_value_avail == 1), Year, NA))

#Assigns all NAs to 4 (so they may be sorted)
socio_select4[is.na(socio_select4)] <- 4

#Create column that reflects min year in which data is available
socio_select5 <- socio_select4 %>% 
  group_by(`Town`, Birth.Weight, Gestational.Age, Mother.s.Education, Mother.s.Marital.Status) %>% 
  mutate(min_year = min(year_avail))

#Set up for assignments
socio_select5 <- as.data.frame(socio_select5)

#Assign year available based on value of min year column
socio_select6 <- socio_select5 %>% 
  mutate(Earliest.Data.Available = ifelse(min_year  == 1, "2014",
                                   ifelse(min_year== 2, "2012-2014",
                                   ifelse(min_year == 3, "2010-2014",
                                   ifelse(min_year == 4, NA, NA)))))

#Remove extra columns
socio_available <- socio_select6 %>% 
  select(-c(year_avail, Year, Variable, Value, value_avail, max_value_avail, year_avail, min_year))

#Remove duplicates (isolate latest year available)
socio_available <- socio_available[!duplicated(socio_available), ]

write.table(
  demo_available,
  sep = ",",
  file.path(path_to_data, "demo_available.csv"),
  row.names = F
)

demo_select6_print <- demo_select6[1:30,]
demo_select6_print$FIPS <- NULL

write.table(
  demo_select6_print,
  sep = ",",
  file.path(path_to_data, "demo_example.csv"),
  row.names = F
)



write.table(
  behav_available,
  sep = ",",
  file.path(path_to_data, "behav_available.csv"),
  row.names = F
)

write.table(
  socio_available,
  sep = ",",
  file.path(path_to_data, "socio_available.csv"),
  row.names = F
)



