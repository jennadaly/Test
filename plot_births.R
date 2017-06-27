library(plotly)
library(dplyr)
#Setup environment
sub_folders <- list.files()
raw_data_location <- grep("raw", sub_folders, value=T)
data_location <- grep("data$", sub_folders, value=T)
path_to_data <-  (paste0(getwd(), "/", data_location))

source('~/Desktop/Data/Births/viz_births.R')

demo_all_values <- demo_all[demo_all$Birth.Weight == "All" & demo_all$Gestational.Age == "All" & demo_all$Mother.s.Age == "All" & demo_all$Mother.s.Race.Ethnicity == "All",]
  
demo_all_values2 <- demo_all_values %>% 
  select(Town, Year, Value) %>% 
  group_by(Year) %>% 
  summarise(Total = sum(Value))

annual <- demo_all_values[!grepl("-", demo_all_values$Year), ]
  
three <- c("1999-2001", "2000-2002", "2001-2003", "2002-2004", "2003-2005", "2004-2006", "2005-2007", 
           "2006-2008", "2007-2009", "2008-2010", "2009-2011", "2010-2012", "2011-2013", "2012-2014")

three_year <- demo_all_values[demo_all_values$Year %in% three,]

five_year <- demo_all_values[!demo_all_values$Year %in% three & grepl("-", demo_all_values$Year),]


total_births <- demo_all_values2 %>% 
  plot_ly(x = ~Year, y = ~Total, type = 'scatter', mode = 'markers', name = 'Value',
                  marker = list(color = 'red', width = 3))  %>% 
  layout(title = "Total Births")


annual_plot2 <- annual %>% 
  group_by(Year) %>% 
  summarise(Total = sum(Value)) %>% 
  plot_ly(x = ~Year, y = ~Total, type = 'scatter', mode = 'lines+markers', name = 'Annual Births',
          marker = list(color = 'blue', width = 3))  %>% 
  layout(title = "Total Annual Births in Connecticut")

three_plot <- three_year %>% 
  group_by(Year) %>% 
  summarise(Total = sum(Value)) %>% 
  plot_ly(x = ~Year, y = ~Total, type = 'scatter', mode = 'lines+markers', name = 'Three Year Average Births',
          marker = list(color = 'orange', width = 3))  %>% 
  layout(title = "Total 3-Year Births")

five_plot <- five_year %>% 
  group_by(Year) %>% 
  summarise(Total = sum(Value)) %>% 
  plot_ly(x = ~Year, y = ~Total, type = 'scatter', mode = 'lines+markers', name = 'Five Year Average Births',
          marker = list(color = 'green', width = 3))  %>% 
  layout(title = "Total Births in CT")

plot_all <- subplot(annual_plot, three_plot, five_plot)

#######################################################################################################################################################################################
behav_all_values <- behav_all[behav_all$Birth.Weight == "All" & behav_all$Gestational.Age == "All" & behav_all$Initiation.of.Prenatal.Care == "All" & behav_all$Smoking.During.Pregnancy == "All",]

behav_all_values2 <- behav_all_values %>% 
  select(Town, Year, Value) %>% 
  group_by(Year) %>% 
  summarise(Total = sum(Value))

annual_behav <- behav_all_values[!grepl("-", behav_all_values$Year), ]

behav_three_year <- behav_all_values[behav_all_values$Year %in% three,]

behav_five_year <- behav_all_values[!behav_all_values$Year %in% three & grepl("-", behav_all_values$Year),]


total_births_behav <- behav_all_values2 %>% 
  plot_ly(x = ~Year, y = ~Total, type = 'scatter', mode = 'markers', name = 'Value',
          marker = list(color = 'red', width = 3))  %>% 
  layout(title = "Total Births")

annual_plot_behav <- annual_behav %>% 
  group_by(Year) %>% 
  summarise(Total = sum(Value)) %>% 
  plot_ly(x = ~Year, y = ~Total, type = 'scatter', mode = 'lines+markers', name = 'Annual Births',
          marker = list(color = 'blue', width = 3))  %>% 
  layout(title = "Total Annual Births in Connecticut")

plot_annual <- subplot(annual_plot, annual_plot_behav) #demo and behav tell same story when filters are set to All - confirmed

##############################################################################################################################################################################################

#Births by Town
# library(rgdal)
# 
# dsn <- system.file("vectors", package = "rgdal")
# ogrInfo(dsn=dsn, layer="cities")
# 
# shp <- readOGR(dsn="/home/jdaly/Desktop/Data/Births/", layer="cb_2016_09_cousub_500k")
# 
# ogrInfo(dsn="cb_2016_09_cousub_500k.shp", layer="cities")
# 
# 
# shape <- readOGR(dsn = ".", layer = "/home/jdaly/Desktop/Data/Births/cb_2016_09_cousub_500k", verbose=T)

#Births by Age
########################################################################################################################################
age_annual <- demo_all[demo_all$Birth.Weight == "All" & demo_all$Gestational.Age == "All" & 
                         demo_all$Mother.s.Race.Ethnicity == "All" & demo_all$Measure.Type == "Number" &
                         !grepl("-", demo_all$Year) ,]

age2 <- age_annual %>% 
  select(-c(FIPS, Birth.Weight, Gestational.Age, Mother.s.Race.Ethnicity, Measure.Type, Variable)) %>% 
  group_by(Year, Mother.s.Age) %>% 
  summarise(Total = sum(Value))

library(reshape)
age_wide <- cast(age2, Year ~ Mother.s.Age)

x <- list(
  title = "Years", 
  ticklen = 5,
  tickwidth = 1
)
y <- list(
  title = "Total Births",
  ticklen = 15,
  tickwidth = 1
)
m <- list(l=50, r=20) # l = left; r = right; t = top; b = bottom


age_plot1 <- plot_ly(age_wide) %>% 
  add_trace(x = ~Year, y = ~`0 to 14 years`, type = 'bar', name = 'Total_0to14', marker = list(color = 'blue', width = 3)) %>% 
  layout(margin=m,
         title = "Total Births Per Age Group - 0 to 14 Years", 
         xaxis = x, yaxis = y)

age_plot2 <- plot_ly(age_wide) %>% 
  add_trace(x = ~Year, y = ~`15 to 19 years`, type = 'bar', name = 'Total_15to19', marker = list(color = 'red', width = 3)) %>% 
  layout(margin=m,
         title = "Total Births Per Age Group - 15 to 19 years", 
         xaxis = x, yaxis = y)

age_plot3 <- plot_ly(age_wide) %>% 
  add_trace(x = ~Year, y = ~`20 to 24 years`, type = 'bar', name = 'Total_20to24', marker = list(color = 'green', width = 3)) %>% 
  layout(margin=m,
         title = "Total Births Per Age Group", 
         xaxis = x, yaxis = y)

age_plot4 <- plot_ly(age_wide) %>% 
  add_trace(x = ~Year, y = ~`25 to 29 years`, type = 'bar', name = 'Total_25to29', marker = list(color = 'purple', width = 3)) %>% 
  layout(margin=m,
         title = "Total Births Per Age Group", 
         xaxis = x, yaxis = y)

age_plot5 <- plot_ly(age_wide) %>% 
  add_trace(x = ~Year, y = ~`30 to 34 years`, type = 'bar', name = 'Total_30to34', marker = list(color = 'pink', width = 3)) %>% 
  layout(margin=m,
         title = "Total Births Per Age Group", 
         xaxis = x, yaxis = y)

age_plot6 <- plot_ly(age_wide) %>% 
  add_trace(x = ~Year, y = ~`35 to 39 years`, type = 'bar', name = 'Total_35to39', marker = list(color = 'orange', width = 3)) %>% 
  layout(margin=m,
         title = "Total Births Per Age Group", 
         xaxis = x, yaxis = y)
age_plot7 <- plot_ly(age_wide) %>% 
  add_trace(x = ~Year, y = ~`40 to 44 years`, type = 'bar', name = 'Total_40to44', marker = list(color = 'yellow', width = 3)) %>% 
  layout(margin=m,
         title = "Total Births Per Age Group", 
         xaxis = x, yaxis = y)
age_plot8 <- plot_ly(age_wide) %>% 
  add_trace(x = ~Year, y = ~`45 years and older`, type = 'bar', name = 'Total_45+', marker = list(color = 'black', width = 3)) %>% 
  layout(margin=m,
         title = "Total Births Per Age Group", 
         xaxis = x, yaxis = y)

plot_all_ages <- subplot(age_plot1, age_plot2, age_plot3, age_plot4, 
                         age_plot5, age_plot6, age_plot7, age_plot8, shareY = TRUE)

plot_young_old <- subplot(age_plot1, age_plot8, shareY=TRUE)

plot_all_age_plots <- subplot(plot_all_ages, plot_young_old, nrows=2, margin = 0.09, heights = c(0.6, 0.3))
plot_all_age_plots

#Births by Education
###########################################################################################################################################
edu_annual <- socio_all[socio_all$Birth.Weight == "All" & socio_all$Gestational.Age == "All" & 
                          socio_all$Mother.s.Marital.Status == "All" & socio_all$Measure.Type == "Number" &
                          !grepl("-", socio_all$Year) ,]

edu2 <- edu_annual %>% 
  select(-c(FIPS, Birth.Weight, Gestational.Age, Mother.s.Marital.Status, Measure.Type, Variable)) %>% 
  group_by(Year, Mother.s.Education) %>% 
  summarise(Total = sum(Value))

library(reshape)
edu_wide <- cast(edu2, Year ~ Mother.s.Education)

x <- list(
  title = "Years", 
  ticklen = 5,
  tickwidth = 1
)
y <- list(
  title = "Total Births",
  ticklen = 15,
  tickwidth = 1
)
m <- list(l=50, r=20) # l = left; r = right; t = top; b = bottom

edu_plot1 <- plot_ly(edu_wide) %>% 
  add_trace(x = ~Year, y = ~`Less than 12 years`, type = 'bar', name = 'Total_Less12', marker = list(color = 'blue', width = 3)) %>% 
  layout(margin=m,
         title = "Total Births Per Education Level", 
         xaxis = x, yaxis = y)

edu_plot2 <- plot_ly(edu_wide) %>% 
  add_trace(x = ~Year, y = ~`High School (12 yrs or GED)`, type = 'bar', name = 'Total_HighSchoolGED', marker = list(color = 'red', width = 3)) %>% 
  layout(margin=m,
         title = "Total Births Per Education Level", 
         xaxis = x, yaxis = y)

edu_plot3 <- plot_ly(edu_wide) %>% 
  add_trace(x = ~Year, y = ~`College (13-16 yrs)`, type = 'bar', name = 'Total_College', marker = list(color = 'green', width = 3)) %>% 
  layout(margin=m,
         title = "Total Births Per Education Level", 
         xaxis = x, yaxis = y)

edu_plot4 <- plot_ly(edu_wide) %>% 
  add_trace(x = ~Year, y = ~`Post-College (17 or more yrs)`, type = 'bar', name = 'Total_PostCollege', marker = list(color = 'purple', width = 3)) %>% 
  layout(margin=m,
         title = "Total Births Per Education Level", 
         xaxis = x, yaxis = y)

edu_plot5 <- plot_ly(edu_wide) %>% 
  add_trace(x = ~Year, y = ~`Unknown`, type = 'bar', name = 'Total_Unknown', marker = list(color = 'black', width = 3)) %>% 
  layout(margin=m,
         title = "Total Births Per Education Level", 
         xaxis = x, yaxis = y)

plot_all_edu <- subplot(edu_plot1, edu_plot2, edu_plot3, edu_plot4, edu_plot5, shareY = TRUE)

plot_all_edu

#Births by Initiation of Health Care
###########################################################################################################################################
init_annual <- behav_all[behav_all$Birth.Weight == "All" & behav_all$Gestational.Age == "All" & 
                           behav_all$Smoking.During.Pregnancy == "All" & behav_all$Measure.Type == "Number" &
                           !grepl("-", behav_all$Year) ,]

init2 <- init_annual %>% 
  select(-c(FIPS, Birth.Weight, Gestational.Age, Smoking.During.Pregnancy, Measure.Type, Variable)) %>% 
  group_by(Year, Initiation.of.Prenatal.Care) %>% 
  summarise(Total = sum(Value))

library(reshape)
init_wide <- cast(init2, Year ~ Initiation.of.Prenatal.Care)

x <- list(
  title = "Years", 
  ticklen = 5,
  tickwidth = 1
)
y <- list(
  title = "Total Births",
  ticklen = 15,
  tickwidth = 1
)
m <- list(l=50, r=20) # l = left; r = right; t = top; b = bottom

init_plot1 <- plot_ly(init_wide) %>% 
  add_trace(x = ~Year, y = ~`None`, type = 'bar', name = 'None', marker = list(color = 'blue', width = 3)) %>% 
  layout(margin=m,
         title = "otal Births by Initiation of Prenatal Care", 
         xaxis = x, yaxis = y)

init_plot2 <- plot_ly(init_wide) %>% 
  add_trace(x = ~Year, y = ~`First Trimester`, type = 'bar', name = 'Total_First', marker = list(color = 'red', width = 3)) %>% 
  layout(margin=m,
         title = "otal Births by Initiation of Prenatal Care", 
         xaxis = x, yaxis = y)

init_plot3 <- plot_ly(init_wide) %>% 
  add_trace(x = ~Year, y = ~`Second Trimester`, type = 'bar', name = 'Total_Second', marker = list(color = 'green', width = 3)) %>% 
  layout(margin=m,
         title = "otal Births by Initiation of Prenatal Care", 
         xaxis = x, yaxis = y)

init_plot4 <- plot_ly(init_wide) %>% 
  add_trace(x = ~Year, y = ~`Third Trimester`, type = 'bar', name = 'Total_Third', marker = list(color = 'purple', width = 3)) %>% 
  layout(margin=m,
         title = "otal Births by Initiation of Prenatal Care", 
         xaxis = x, yaxis = y)

init_plot5 <- plot_ly(init_wide) %>% 
  add_trace(x = ~Year, y = ~`Unknown`, type = 'bar', name = 'Total_Unknown', marker = list(color = 'black', width = 3)) %>% 
  layout(margin=m,
         title = "Total Births by Initiation of Prenatal Care", 
         xaxis = x, yaxis = y)

#Create values for "Late" intiation
init_wide$Late <- NULL
init_wide$Late <- (init_wide$`Second Trimester` + init_wide$`Third Trimester`)

init_plot6 <- plot_ly(init_wide) %>% 
  add_trace(x = ~Year, y = ~`Late`, type = 'bar', name = 'Total_Late', marker = list(color = 'grey', width = 3)) %>% 
  layout(margin=m,
         title = "Total Births by Initiation of Prenatal Care", 
         xaxis = x, yaxis = y)

plot_all_init <- subplot(init_plot1, init_plot2, init_plot3, init_plot4, init_plot5, shareY = TRUE)

plot_all_init_plots <- subplot(plot_all_init, init_plot6, nrows=2, margin = 0.09, heights = c(0.6, 0.3))

plot_all_init


#Towns with the most births
library(rgdal)
CT_towns_shp <- readOGR("/home/jdaly/Desktop/Data/Births/Shapefile/cb_2016_09_cousub_500k.shp", "cb_2016_09_cousub_500k")
plot(CT_towns_shp)

total_births_by_town_2014 <- age_annual[age_annual$Year == "2014" & age_annual$Mother.s.Age == "All" & age_annual$Town != "Connecticut",]

total_births_by_town_2014 <- total_births_by_town_2014 %>% 
  select(Town, Year, Value)

################################################################################################################
install.packages("gpclib")
library(gpclib)
library(maptools)
# class(CT_towns_shp)
# names(CT_towns_shp)
# print(CT_towns_shp$NAME)
# num.towns <- length(CT_towns_shp$NAME)
# mydata <- data.frame(NAME=CT_towns_shp$NAME, id=CT_towns_shp$ID_1)

CT_towns <- fortify(CT_towns_shp, region = "NAME")
merge.shp<-merge(CT_towns, total_births_by_town_2014, by.x="id", by.y = "Town", all.x=TRUE)
final.plot<-merge.shp[order(merge.shp$order), ] 

# popup1 <- paste0("<span style='color: #7f0000'><strong>Total Births 2014</strong></span>",
#                  "<br><span style='color: salmon;'><strong>Town: </strong></span>", 
#                  total_births_by_town_2014$Town, 
#                  "<br><span style='color: salmon;'><strong>Number of Births: </strong></span>", 
#                  total_births_by_town_2014$Value
# )


mymap2 <- ggplot() + 
          geom_polygon(data = final.plot, aes(x = long, y = lat, group = id, fill = Value), 
                       color = "black") +
  scale_fill_distiller(name="Births", palette = "RdBu") +
  labs(title="Total Births in 2014")

gg <- ggplotly(mymap2)

################################################################################################################

#Towns with most increase of births overall

trend_towns <- behav_all_values[!grepl("-", behav_all_values$Year) & behav_all_values$Town != "Connecticut",] %>% 
  select(Town, Year, Value)
trend_towns$Year <- as.numeric(trend_towns$Year)

trend_towns_recent <- trend_towns[trend_towns$Year > 2009,]

mylinegraph <- ggplot(trend_towns, aes(x=Year, y=Value, color=Town)) +
  geom_line(aes(group = Town)) +
  scale_colour_hue(name="Town", l=30)

ll <- ggplotly(mylinegraph)

ll

#Towns with most decrease of births overall





