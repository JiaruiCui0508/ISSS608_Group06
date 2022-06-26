library(shiny)
library(tidyverse)
library(lubridate)
library(patchwork)
library(trelliscopejs)

pub <- read_csv('Data/Pubs.csv')
Restaurants <- read_csv('Data/Restaurants.csv')
CheckinJournal <- read_csv('Data/CheckinJournal.csv')
TravelJournal <- read_csv('Data/TravelJournal.csv') #to calculate revenue

CheckinJournal <- CheckinJournal %>%
  filter(venueType %in% c('Pub','Restaurant')) %>%
  mutate(Dates = as.Date(timestamp,format="%m/%d/%Y")) %>%
  select(Dates,venueId)

CheckinJournal_group <- CheckinJournal %>%
  group_by(Dates,venueId)%>%
  summarise(n = n())

#Calculate revenue
CheckinJournal_group <- CheckinJournal_group %>%
  left_join(y= Restaurants, 
            by = c('venueId' = 'restaurantId')) 
CheckinJournal_group$revenue_res <- CheckinJournal_group$foodCost * CheckinJournal_group$n

CheckinJournal_group <- CheckinJournal_group %>%
  select(Dates,venueId,n,revenue_res,maxOccupancy)

CheckinJournal_group <- CheckinJournal_group %>%
  left_join(y= pub, 
            by = c('venueId' = 'pubId'))%>%
  select(Dates,venueId,n,revenue_res,hourlyCost,maxOccupancy.x,maxOccupancy.y)

CheckinJournal_group[is.na(CheckinJournal_group)] = 0
CheckinJournal_group <- CheckinJournal_group %>%
  mutate(maxOccupancy = maxOccupancy.x+maxOccupancy.y) %>%
  select(Dates,venueId,n,revenue_res,hourlyCost,maxOccupancy)

pubstr <- TravelJournal %>%
  filter(purpose == 'Recreation (Social Gathering)') %>%
  mutate(travelTime = travelEndTime - travelStartTime) %>%
  select(-c(travelStartTime: travelEndTime, endingBalance)) %>%
  inner_join(y= pub, 
             by = c('travelEndLocationId'= 'pubId')) %>%
  mutate(visitDuration = checkOutTime - checkInTime,
         spending = as.numeric(visitDuration/60)* hourlyCost) %>%
  select(-c(purpose, location, checkOutTime)) %>%
  rename('pubId' = 'travelEndLocationId')

pubstr <-pubstr %>%
  mutate(Dates = as.Date(checkInTime,format="%m/%d/%Y")) %>%
  group_by(Dates,pubId)%>%
  summarise(revenue_pub = sum(spending))

CheckinJournal_group <- CheckinJournal_group %>%
  left_join(y= pubstr, 
            by = c('venueId' = 'pubId',
                   'Dates'='Dates'))%>%
  select(Dates,venueId,n,revenue_res,revenue_pub,maxOccupancy)

CheckinJournal_group[is.na(CheckinJournal_group)] = 0
CheckinJournal_group <- CheckinJournal_group %>%
  mutate(revenue = revenue_res+revenue_pub) %>%
  select(Dates,venueId,n,revenue,maxOccupancy)

CheckinJournal_group$yearmonth  <- format(as.Date(CheckinJournal_group$Dates), "%Y-%m")

# Sidebar
Total_ID <- mapply(c, pub, Restaurants)

ui <- navbarPage(title = h4("Group 6"),position="fixed-top",
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
                                                       selected = '442')
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
                                                       selected = '442')
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
      filter(venueId==input$variable2)
    
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
      filter(venueId==input$variable)
    
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
                        path = 'trellisp/',
                        self_contained = TRUE) +
      theme(axis.title.y= element_text(angle=0), 
            axis.text.x = element_text(angle = 30, vjust = 0.5, hjust = 0.3),
            axis.ticks.x= element_blank(),
            panel.background= element_blank(), 
            axis.line= element_line(color= 'grey'))
    print(CheckinJournal_group_sum)
    
    
  }) 
}



shinyApp(ui = ui, server = server)

