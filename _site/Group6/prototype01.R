library(shiny)
library(tidyverse)
library(lubridate)
library(patchwork)
library(trelliscopejs)
library(sf)
library(tmap)
library(ggthemes)


pub <- read_csv('Data/Pubs.csv')
Restaurants <- read_csv('Data/Restaurants.csv')
#CheckinJournal <- read_csv('Data/CheckinJournal.csv')
CheckinJournal_group <- read_rds('Data/CheckinJournal_group.rds')
ERworkplace2 <- read_rds('Data/ERworkplace2.rds')
ER_break <- read_rds('Data/ER_break.rds')
pubstr <- read_rds('Data/pubstr.rds') #to calculate revenue
#Participants <- read_csv("Data/Participants.csv")
#jobs <- read_csv("Data/Jobs.csv")
#ER <- read_sf("Data/Employers.csv", options = "GEOM_POSSIBLE_NAMES=location")
buildings <- read_sf("Data/Buildings.csv", options = "GEOM_POSSIBLE_NAMES=location")

#Data Wrangling

#ER$employerId <- as.character(ER$employerId)

#workplace <- CheckinJournal %>% 
#  filter(venueType =='Workplace')%>%
#  mutate(venueId = as.character(factor(venueId)))

# Extract turnover rate for Employer

#ERworkplace <- workplace %>% 
#  left_join(y = ER, by = c("venueId" = "employerId"))%>%
#  left_join(y=Participants, by = c("participantId" = "participantId"))%>%
#  rename(CompanyID = venueId)%>%
#  mutate(CompanyID = as.character(factor(CompanyID)))%>%
#  select(-c('interestGroup','householdSize'))

#draft <- ERworkplace %>% 
#  group_by(participantId, CompanyID)%>% 
#  count(CompanyID)%>%
#  mutate(n = 1)


#cc_list <- draft %>% 
#  group_by(participantId) %>% 
#  summarise(TurnoverFreq = sum(n))%>%
#  filter(TurnoverFreq > 1)

#check <- left_join(draft, cc_list, by = c("participantId" = "participantId")) %>%
#  filter(TurnoverFreq >= 1)

#staff<- draft %>%
#  group_by(CompanyID) %>% 
#  summarise(TurnoverFreq = sum(n)) %>%
#  rename(No.of_Staff = TurnoverFreq)


#company_churn<- check %>% 
#  group_by(CompanyID)%>% 
#  summarise(Frequent = sum(n)) %>%
#  rename(Turn_Freq = Frequent)


#turn_bycc<- company_churn %>% 
#  left_join(y = staff, by = c("CompanyID" = "CompanyID")) %>%
#  mutate(Turnover_Rate = round((Turn_Freq / No.of_Staff),2)) %>%
#  mutate(percent = paste(round((Turn_Freq / No.of_Staff)*100,2),"%",sep="")) %>%
#  mutate(Turn_Level = cut(Turnover_Rate, breaks = c(0,0.33,0.66,1), labels = c("Low Rate","Medium","High Rate")))

# Merge turnover information with employer details

#ERworkplace <- ERworkplace %>%
#  left_join(y = turn_bycc, by = "CompanyID")


#ER <- ER %>%
#  left_join(y=turn_bycc, by = c("employerId" = "CompanyID")) %>%
#  mutate(Turn_Level = coalesce(Turn_Level, "NA"))

#ER_selected <- ER %>%
#  filter(Turnover_Rate > 0)

#ER_break <- ER_selected %>%
#  rename(Turnover_Count = Turn_Freq)%>%
#  rename(Percent = percent)%>%
#  select(-c('location'))

#ERworkplace<- ERworkplace %>%
#  filter( Turnover_Rate >0) %>%
#  mutate(Age_Level = cut(age, breaks = c(0,20,40,61), labels = c("Below 20","20-40","40-60")))


#CheckinJournal_pub_res <- CheckinJournal %>%
#  filter(venueType %in% c('Pub','Restaurant')) %>%
#  mutate(Dates = as.Date(timestamp,format="%m/%d/%Y")) %>%
#  select(Dates,venueId)

#CheckinJournal_group <- CheckinJournal_pub_res %>%
#  group_by(Dates,venueId)%>%
#  summarise(n = n())

#Calculate revenue
#CheckinJournal_group <- CheckinJournal_group %>%
#  left_join(y= Restaurants, 
#            by = c('venueId' = 'restaurantId')) 
#CheckinJournal_group$revenue_res <- CheckinJournal_group$foodCost * CheckinJournal_group$n

#CheckinJournal_group <- CheckinJournal_group %>%
#  select(Dates,venueId,n,revenue_res,maxOccupancy)

#CheckinJournal_group <- CheckinJournal_group %>%
#  left_join(y= pub, 
#            by = c('venueId' = 'pubId'))%>%
#  select(Dates,venueId,n,revenue_res,hourlyCost,maxOccupancy.x,maxOccupancy.y)

#CheckinJournal_group[is.na(CheckinJournal_group)] = 0
#CheckinJournal_group <- CheckinJournal_group %>%
#  mutate(maxOccupancy = maxOccupancy.x+maxOccupancy.y) %>%
#  select(Dates,venueId,n,revenue_res,hourlyCost,maxOccupancy)

#pubstr <- TravelJournal %>%
#  filter(purpose == 'Recreation (Social Gathering)') %>%
#  mutate(travelTime = travelEndTime - travelStartTime) %>%
#  select(-c(travelStartTime: travelEndTime, endingBalance)) %>%
#  inner_join(y= pub, 
#             by = c('travelEndLocationId'= 'pubId')) %>%
#  mutate(visitDuration = checkOutTime - checkInTime,
#         spending = as.numeric(visitDuration/60)* hourlyCost) %>%
#  select(-c(purpose, location, checkOutTime)) %>%
#  rename('pubId' = 'travelEndLocationId')

#pubstr <-pubstr %>%
#  mutate(Dates = as.Date(checkInTime,format="%m/%d/%Y")) %>%
#  group_by(Dates,pubId)%>%
#  summarise(revenue_pub = sum(spending))

#CheckinJournal_group <- CheckinJournal_group %>%
#  left_join(y= pubstr, 
#            by = c('venueId' = 'pubId',
#                   'Dates'='Dates'))%>%
#  select(Dates,venueId,n,revenue_res,revenue_pub,maxOccupancy)

#CheckinJournal_group[is.na(CheckinJournal_group)] = 0
#CheckinJournal_group <- CheckinJournal_group %>%
#  mutate(revenue = revenue_res+revenue_pub) %>%
#  select(Dates,venueId,n,revenue,maxOccupancy)

#CheckinJournal_group$yearmonth  <- format(as.Date(CheckinJournal_group$Dates), "%Y-%m")




# Sidebar
Total_ID <- mapply(c, pub, Restaurants)

ui <- navbarPage(title = h4("Group 6"),position="fixed-top",
                 navbarMenu('Employment Pattern',
                            tags$style(type="text/css", "body {padding-top: 70px;}"), 
                            
                            tabPanel("Turnover Status Distribution",
                                     tabsetPanel(
                                       sidebarLayout(
                                         sidebarPanel(
                                           selectInput(inputId = "variable3_3",
                                                       label = "Company Turnover Level",
                                                       choices = c("High Level" = "High Rate",
                                                                   "Medium" = "Medium",
                                                                   "Low Level" = "Low Rate"),
                                                       selected = "High Rate")
                                         ),
                                         mainPanel(
                                           tmapOutput("detailPlot"),
                                           DT::dataTableOutput(outputId = "aTable2")
                                         )
                                       )
                                     )
                            ),
                            tabPanel("Employment Factor Ratio",
                                     tabsetPanel(
                                       sidebarLayout(
                                         sidebarPanel(
                                           selectInput(inputId = "variable3_4",
                                                       label = "Pattern:",
                                                       choices = c("Education" = "educationLevel",
                                                                   "Age" = "Age_Level",
                                                                   "Having Kids" = "haveKids"),
                                                       selected = "educationLevel")
                                           
                                         ),
                                         mainPanel(
                                           plotOutput("stackPlot")
                                         )
                                       )
                                     )
                            )
                 ),
                 
                 
                 
                 navbarMenu('Business performance',
                            tags$style(type="text/css", "body {padding-top: 70px;}"), 
                tabPanel("Summary",
                         mainPanel(
                           trelliscopeOutput("Summary", width = "100%", height = "500px")
                         )),
                tabPanel("Revenue",
                          tabsetPanel(
                            sidebarLayout(
                              sidebarPanel(
                                selectInput(inputId = "variable",
                                            label = "Select ID:",
                                            choices = Total_ID[, c('pubId')],
                                            selected = '442'),
                                dateRangeInput("Date_range2", "Date range:",
                                               start = "2022-03-01",
                                               end   = "2023-04-30")
                              ),
                              mainPanel(
                                plotOutput("Revenue")
                              )
                            )
                          )
                ),                
                 tabPanel("Visit",
                          tabsetPanel(
                            sidebarLayout(
                              sidebarPanel(
                                selectInput(inputId = "variable2",
                                            label = "Select ID:",
                                            choices = Total_ID[, c('pubId')],
                                            selected = '442'),
                                dateRangeInput("Date_range", "Date range:",
                                               start = "2022-03-01",
                                               end   = "2023-04-30")
                              ),
                              mainPanel(
                                plotOutput("visit")
                              )
                              )
                            )
                          )))




server <- function(input,output){
  
  output$visit <- renderPlot({
    select_ID <- CheckinJournal_group %>% 
      filter(venueId==input$variable2,Dates>=input$Date_range[1] & Dates<=input$Date_range[2])
    
    p <- ggplot(select_ID, aes(x=Dates, y=n)) +
      geom_line() + 
      xlab("Dates")
    
    select_ID$month <- factor(month(select_ID$Dates), levels=1:12, labels=month.abb, ordered=TRUE)
    select_ID$year  <- year(select_ID$Dates)
    select_ID$year_days  <- format(as.Date(select_ID$Dates), "%Y-%d")
    select_ID$day  <- day(select_ID$Dates)
    
    hline.data <- select_ID%>%
      group_by(month)%>%
      summarise(avgvalue=mean(n))
    
    k<-ggplot() + 
      geom_line(aes(x=year_days, y=n, group=month), data=select_ID, colour="blue") +
      geom_hline(aes(yintercept=avgvalue), data=hline.data, colour="red", size=1) + 
      facet_grid(~month) +

      
      
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank())
    
    p/k })
    
    
    output$Revenue <- renderPlot({
      select_ID <- CheckinJournal_group %>% 
        filter(venueId==input$variable,Dates>=input$Date_range2[1] & Dates<=input$Date_range2[2])
      
      j <- ggplot(select_ID, aes(x=Dates, y=revenue)) +
        geom_line() + 
        xlab("Dates")
      
      select_ID$month <- factor(month(select_ID$Dates), levels=1:12, labels=month.abb, ordered=TRUE)
      select_ID$year  <- year(select_ID$Dates)
      select_ID$year_days  <- format(as.Date(select_ID$Dates), "%Y-%d")
      select_ID$day  <- day(select_ID$Dates)
      
      hline.data <- select_ID%>%
        group_by(month)%>%
        summarise(avgvalue=mean(revenue))
      
      r<-ggplot() + 
        geom_line(aes(x=year_days, y=revenue, group=month), data=select_ID, colour="blue") +
        geom_hline(aes(yintercept=avgvalue), data=hline.data, colour="red", size=1) + 
        facet_grid(~month) +
        
        
        
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank())
      
      j/r
  })

    output$Summary <- renderTrelliscope({ 
      CheckinJournal_group_sum <- ggplot(CheckinJournal_group, aes(x= as.factor(yearmonth), y= revenue)) +
        geom_col(fill= '#008080') +
        labs(x= 'Month Year', y= 'n\revenue',
             title = 'Monthly Customer Visits and revenue') +
        facet_trelliscope(~ venueId, 
                          nrow = 2, ncol = 2, width = 1000,
                          path = 'www/trellisp',
                          self_contained = TRUE) +
        theme(axis.title.y= element_text(angle=0), 
              axis.text.x = element_text(angle = 30, vjust = 0.5, hjust = 0.3),
              axis.ticks.x= element_blank(),
              panel.background= element_blank(), 
              axis.line= element_line(color= 'grey'))
      print(CheckinJournal_group_sum)
      
    
  }) 
    
    # Map with DT table, define reactive table
    dataset = reactive({
      ER_break %>%
        filter(Turn_Level == input$variable3_3) 
    })
    
    output$detailPlot <- renderTmap({
      ER_break%>%
        filter(Turn_Level == input$variable3_3)
      
      tm_shape(buildings)+
        tm_polygons(col = "grey60",
                    size = 1.2,
                    border.col = "black",
                    border.lwd = 1) +
        tm_shape(shp = dataset(),
                 bbox = st_bbox(ER_break)) +
        tm_bubbles(col = "Turnover_Rate",
                   size = "Turnover_Rate",
                   border.col = "black",
                   border.lwd = 0.5)
    })
    
    output$aTable2 <- DT::renderDataTable({
      DT::datatable(data = dataset() %>%
                      select(1:7),
                    options= list(pageLength = 5),
                    rownames = FALSE)
      
    })
    # Employment fraction 
    output$stackPlot <- renderPlot({
      xx<- unlist(ERworkplace2[, input$variable3_4])
      ggplot(ERworkplace2, aes(fill= xx, y=Turnover_Rate, x=Turn_Level)) + 
        geom_bar(position="fill", stat="identity") +
        scale_fill_brewer(palette = "Paired", name = input$variable3_4) +
        labs(y="Turnover Rate",x="Turnonver Level", title = "Employment pattern comparision per Employer Turnover Level ") +
        scale_y_continuous(labels = scales::percent) +
        theme_minimal() + theme(axis.title.y= element_text(angle=0))
    })
}



shinyApp(ui = ui, server = server)
