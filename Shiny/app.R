library(shiny)
library(dplyr)

#Setup environment
sub_folders <- list.files()
raw_data_location <- grep("raw", sub_folders, value=T)
data_location <- grep("data$", sub_folders, value=T)
path_to_data <-  (paste0(getwd(), "/", data_location))

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

#################################################################################################################################################
ui <- shinyUI(
  fluidPage(
    titlePanel("Maternal Behaviors"),

    sidebarLayout(
      sidebarPanel(
        helpText("To start, please first choose a town and either a
                 Birth Weight Category or a Gestational Age Category."),
        selectInput("town",
                    label = "Choose a Town",
                    choices = c("", as.vector(unique(behav_available$Town)))),

        selectInput("birthweight",
                    label = "Choose a Birth Weight",
                    choices = c("", as.vector(unique(behav_available$Birth.Weight)))),

        selectInput("gestationalage",
                    label = "Choose a Gestational Age",
                    choices = c("", as.vector(unique(behav_available$Gestational.Age))))
      ),

      mainPanel(
        h3("Maternal Behavior Characteristics include Initiation of Prenatal Care and Smoking During Pregnancy."),
        tableOutput("table"), 
        uiOutput("secondSelection")
      )
    )
  ))

server <- shinyServer(function(input,output){

  output$table <- renderTable({
    town_sel <- if (nchar(input$town)==0) unique(as.vector(behav_available$Town)) else input$town
    bw_sel <- if (nchar(input$birthweight)==0) unique(as.vector(behav_available$Birth.Weight)) else input$birthweight
    ga_sel <- if (nchar(input$gestationalage)==0) unique(as.vector(behav_available$Gestational.Age)) else input$gestationalage
    # in_sel <- if(nchar(input$initiation)==0) unique(as.vector(behav_available$Initiation.of.Prenatal.Care)) else input$intiation
    # sm_sel <- if(nchar(input$smoking)==0) unique(as.vector(behav_available$Smoking.During.Pregnancy)) else input$smoking
    # da_sel <- if(nchar(input$date)==0) unique(as.vector(behav_available$Earliest.Data.Available)) else input$date

    output$secondSelection <- renderUI({
      selectInput("User", "Date:", choices = as.character(dat5[dat5$email==input$Select,"date"]))
    })
    
    filter(behav_available, Town %in% town_sel, Birth.Weight %in% bw_sel, Gestational.Age %in% ga_sel
           #, Initiation.of.Prenatal.Care %in% in_sel,
          # Smoking.During.Pregnancy %in% sm_sel, Earliest.Data.Available %in% da_sel
           )
  })
})

shinyApp(ui = ui, server = server)

