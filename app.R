library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)


post <- read.csv("surveyapp_todo.csv")
##multi users --> 1 email
##combine all different user_id to the same one if they belong to the same email
useremail <- read.csv("user.csv")
uniquetable <- useremail[!duplicated(useremail$email),c('id','email')]
post <- post %>% 
  left_join(uniquetable,by=c('user_id' = 'id'))

post$user_id <- factor(post$email)
post$created <- as.POSIXct(post$created,format="%Y-%m-%d %H:%M:%S",tz=Sys.timezone())

# Choose 27 as log metric; emotional engagement as survey metric
# First time point: before 10/03
# Second time point: before 10/24
# Third time point: before 11/14
t1<- as.POSIXct('2022-10-03',format="%Y-%m-%d",tz=Sys.timezone())
t1_post <- post %>% 
  filter(created < t1) %>% 
  group_by(user_id) %>% 
  summarise(post_n = n())


all_part <- t1_post$user_id


t2<- as.POSIXct('2022-10-24',format="%Y-%m-%d",tz=Sys.timezone())
t2_post <- post %>% 
  filter(created < t2 & created >= t1) %>% 
  group_by(user_id) %>% 
  summarise(post_n = n())

sum(t1_post$post_n)

t3<- as.POSIXct('2022-11-14',format="%Y-%m-%d",tz=Sys.timezone())
t3_post <- post %>% 
  filter(created < t3 & created >= t2) %>% 
  group_by(user_id) %>% 
  summarise(post_n = n())


Real_part <- t3_post$user_id
Leaver <- all_part[!(all_part %in% t3_post$user_id)]
Early_leaver <- all_part[!(all_part %in% t2_post$user_id)]
Late_leaver <- Leaver[!(Leaver %in% Early_leaver)]

# at time point 1, early_leaver left; at time point 2, both early_leaver and late_leaver left
# for log data, compare the frequency of 27 at time 1 and 2 between participants and leavers
# for survey data, compare the emotional engagement at both times betwenn participants and leavers

dat.log <- read.csv("log_log.csv")
dat.log <- dat.log %>% 
  left_join(uniquetable,by=c('user_id' = 'id')) %>% 
  select(email, event, trigger_time)
dat.log$trigger_time <- as.POSIXct(dat.log$trigger_time,format="%Y-%m-%d %H:%M:%S",tz=Sys.timezone())
dat.log$trigger_day <- as.Date(dat.log$trigger_time)


#write.csv(dat.log,"shinylog.csv")

ELnetid <- gsub(" ", "", tolower(gsub("@.*","",Early_leaver)))
Lnetid <- gsub(" ", "", tolower(gsub("@.*","",Leaver)))
LLnetid <- gsub(" ", "", tolower(gsub("@.*","",Late_leaver)))
#group <- list(ELnetid=ELnetid,Lnetid=Lnetid,LLnetid=LLnetid)

not_sel <- "Not Selected"

DataInput_page <- tabPanel(
  title = "DataInput",
  titlePanel("DataInput"),
    mainPanel(
      tabsetPanel(
        tabPanel(
          title = "Log Data",
          tableOutput("logdata")
        )
      )
    )
  )


Summary_page <- tabPanel(
  title = "Summary",
  titlePanel("Overview"),
  sidebarLayout(
    sidebarPanel(
      title = "Summary",
      selectInput("event", "event", choices = c(not_sel)),
      selectInput("group", "group", choices = c('P','EL','LL'),multiple = TRUE),
      tableOutput("data")
      
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          title = "LogbyEvent",
          plotOutput("plot", brush = "plot_brush"),
        )
      )
    )
  )
)




ui <- navbarPage(
  title = "Log Survey Lab",
  theme = shinytheme('lumen'),
  DataInput_page,
  Summary_page
  
)

server <- function(input, output, session) {
  

  df <- reactive({
    dat.log %>% 
      filter(!is.na(email) & email != "") %>% 
      mutate(email= gsub(" ", "", tolower(gsub("@.*","",email)))) %>% 
      mutate(trigger_day = as.character(trigger_day),trigger_time =as.character(trigger_time)) 
  })
  
  output$logdata <- renderTable(
    df()
  )
  
  observeEvent(df(),{
    choices <-c(not_sel,unique(df()$event))
    updateSelectInput(inputId = "event", choices = choices)
    
  })

  output$plot <- renderPlot({
    df() %>%
      filter( event == input$event) %>%
      group_by(trigger_day,email) %>%
      summarise(y = n()) %>% 
      mutate(group= ifelse(email %in% Lnetid,ifelse(email %in% ELnetid, 'EL','LL'),'P')) %>% 
      filter(group %in% input$group) %>% 
      ggplot()+
      geom_point(aes(trigger_day,y,colour = factor(group)),position = position_dodge(0.02))+
      geom_line(aes(trigger_day,y,group=email,colour = factor(group)),position = position_dodge(0.02))+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))

  }, res = 96)


  output$data <- renderTable({
    brushedPoints(df() %>%
                    filter( event == input$event) %>%
                    group_by(trigger_day,email) %>%
                    summarise(y = n()) %>% 
                   # mutate(trigger_day = as.character(trigger_day)) %>% 
                    mutate(group= ifelse(email %in% Lnetid,ifelse(email %in% ELnetid, 'EL','LL'),'P')), input$plot_brush)
  })

  
}

shinyApp(ui, server)