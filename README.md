#### The CT Department of Public Health releases data that provides select birth outcomes (birthweight, gestational age) for Connecticut resident births along with select demographic, behavioral, and socioeconmic characteristics about the mother. 

CTdata.org provides these data sets in 1, 3, and 5 year averages due some of the levels showing little data at the one year level. Therefore, depending on which combination of characteristics the user is interested in, data may not be available at the 1 or 3 level data, instead only available at the 5 year level. 

**But how can the user know at which level should they start exploring?**

*That's where these tables come in.* 

#### The tables found in this repository are meant to guide the user to the earliest level of aggregation in which data are available for a given town and combination of characteristics. 

For example, *are you looking for how many births took place in Hartford to mother's age 45 years and older and that carried to full term?* 

You'll have to first look that the 3-year aggregation data set for that information, becasue no data are available in the 1-year data set. These are the types of questions that can be answered with these tables. 

### Following example code is for the Demographics data sets:

#### Bring in individual aggregations
```{r}
demo_1 <- read.csv(paste0(path_to_data, "/", "maternal_characteristics_demographics_1-Year.csv"), stringsAsFactors = F, header=T)
demo_3 <- read.csv(paste0(path_to_data, "/", "maternal_characteristics_demographics_3-Year.csv"), stringsAsFactors = F, header=T)
demo_5 <- read.csv(paste0(path_to_data, "/", "maternal_characteristics_demographics_5-Year.csv"), stringsAsFactors = F, header=T)
```

#### Combine all aggregations
```{r}
demo_all <- rbind(demo_1, demo_3, demo_5)
```

#### Isolate to latest years and numbers only (no percent values)
```{r}
years <- c("2014", "2012-2014", "2010-2014")
demo_select <- demo_all[demo_all$Measure.Type == "Number" & demo_all$Year %in% years,]
demo_select$Year <- factor(demo_select$Year, levels = c("2014", "2012-2014", "2010-2014"))
```

#### Create column that checks to see if data is available for a given year
```{r}
demo_select2 <- demo_select %>% 
  select(-Measure.Type) %>% 
  arrange(Town, Birth.Weight, Gestational.Age, Mother.s.Age, Mother.s.Race.Ethnicity) %>% 
  group_by(Town, Birth.Weight, Gestational.Age, Mother.s.Age, Mother.s.Race.Ethnicity, Year) %>% 
  mutate(value_avail = ifelse((Value > 0), 1, 0)) 
```

#### Based on availability column, finds groups that have data vs groups with no data
```{r}
demo_select3 <- demo_select2 %>% 
  group_by(`Town`, Birth.Weight, Gestational.Age, Mother.s.Age, Mother.s.Race.Ethnicity) %>% 
  mutate(max_value_avail = max(value_avail))
```

#### Conditionally assigns year_avail based on comparison of availabilty column and max column
```{r}
demo_select4 <- demo_select3 %>% 
  mutate(year_avail = ifelse((value_avail == 0 & max_value_avail == 0), "None", NA), 
         year_avail = ifelse((value_avail == 0 & max_value_avail == 1), NA, NA), 
         year_avail = ifelse((value_avail == 1 & max_value_avail == 1), Year, NA))
```

#### Assigns all NAs to 4 (so they may be sorted)
```{r}
demo_select4[is.na(demo_select4)] <- 4
```

#### Create column that reflects min year in which data is available
```{r}
demo_select5 <- demo_select4 %>% 
  group_by(`Town`, Birth.Weight, Gestational.Age, Mother.s.Age, Mother.s.Race.Ethnicity) %>% 
  mutate(min_year = min(year_avail))
```

#### Set up for assignments
```{r}
demo_select5 <- as.data.frame(demo_select5)
```

#### Assign year available based on value of min year column
```{r}
demo_select6 <- demo_select5 %>% 
  mutate(Earliest.Data.Available = ifelse(min_year  == 1, "2014",
                    ifelse(min_year== 2, "2012-2014",
                    ifelse(min_year == 3, "2010-2014",
                    ifelse(min_year == 4, NA, NA)))))
```

#### Remove extra columns
```{r}
demo_available <- demo_select6 %>% 
  select(-c(year_avail, Year, Variable, Value, value_avail, max_value_avail, year_avail, min_year))
```

#### Remove duplicates (isolate latest year available)
```{r}
demo_available <- demo_available[!duplicated(demo_available), ]
```


