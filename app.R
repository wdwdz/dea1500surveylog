library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(tidyr)
library(rstatix)
library(lme4)
library(emmeans)
library(corrplot)
library(ggpubr)

#########################################
###########HELP FUNCTIONS################
#########################################

# Qualtrics data processing
surveyave <- function(dat.e1){
  dat.e1 <- dat.e1[-1:-2,]
  dat.e1 <- dat.e1[!is.na(dat.e1$Q20),]
  #colnames(dat.e1)
  
  for (col in c('Q5','Q6','Q7','Q8','Q9','Q10','Q11','Q12','Q13','Q14')) {
    dat.e1[,col] <- as.numeric(factor(dat.e1[,col], levels = c("Strongly disagree","Somewhat disagree","Neither agree nor disagree","Somewhat agree","Strongly agree")))
  }
  
  dat.e1<- dat.e1[!is.na(dat.e1$Q6),]
  dat.e1$Q7 <- 6 - dat.e1$Q7
  dat.e1$emo<-rowMeans(dat.e1[,c('Q5','Q6')])
  dat.e1$cog <- rowMeans(dat.e1[,c('Q8','Q9','Q10')])
  dat.e1$socbe <- rowMeans(dat.e1[,c('Q11','Q12','Q13','Q14')])
  
  dat.e1.ave <- dat.e1 %>% 
    dplyr::select(StartDate,Q20,emo,cog,socbe)
  
  return(dat.e1.ave)
  
}

# Main data processing function for process survey and log data
datprocess<- function(dat.fil,eng1){
  df.output <- data.frame(matrix(ncol = 34, nrow = 0))
  colnames(df.output) <- c("usernetid",as.character(seq(1:30)),"total_time","total_freq","ave_time")
  thisrow <- 1
  df.survey <- eng1
  for(thisnetid in df.survey$usernetid){
    #Step1: filter logs before thisnetid's time
    survey_time <- eng1[eng1$usernetid == thisnetid,'StartDate']
    df <- dat.fil %>% 
      filter(trigger_time <= survey_time, usernetid == thisnetid) 
    #Step2: (df) -> (table) summarize counts by event 
    df$event <- factor(df$event,levels = as.character(seq(1:30)))
    temp <- as.data.frame.matrix(t(table(df$event)))
    #Step3: (df) -> (table) summarize duration in each page
    if(nrow(df)<2){
      next
    }
    df$flag <- 1
    for (i in 1:(nrow(df)-1)) {
      cur <- df$trigger_time[i]
      nex <- df$trigger_time[i+1] 
      df$flag[i+1] <- ifelse({difftime(nex,cur,units = "hours") <= 0.5},df$flag[i],df$flag[i] + 1)
    }
    dur <- df %>% 
      group_by(flag) %>% 
      summarise(duration = max(trigger_time) - min(trigger_time)) %>% 
      summarise(total_time = sum(duration),
                total_freq = length(unique(flag)),
                ave_time = mean(duration))
    #Step4: combine dfs
    df.output[thisrow,'usernetid'] <- thisnetid
    df.output[thisrow,2:31] <- temp
    df.output[thisrow,32:34] <- dur
    thisrow <- thisrow + 1
  }
  return(df.output)
}

# Rewrite correlation visuliation function
cor.mtest <- function(mat, ...)
{
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) 
  {
    for (j in (i + 1):n)
    {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}


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
colnames(dat.log)[1] <- "usernetid"
dat.log$trigger_time <- as.POSIXct(dat.log$trigger_time,format="%Y-%m-%d %H:%M:%S",tz=Sys.timezone())
dat.log$trigger_day <- as.Date(dat.log$trigger_time)


#write.csv(dat.log,"shinylog.csv")

ELnetid <- gsub(" ", "", tolower(gsub("@.*","",Early_leaver)))
Lnetid <- gsub(" ", "", tolower(gsub("@.*","",Leaver)))
LLnetid <- gsub(" ", "", tolower(gsub("@.*","",Late_leaver)))
#group <- list(ELnetid=ELnetid,Lnetid=Lnetid,LLnetid=LLnetid)

dat.e1<- read.csv("engagement1_February+25,+2023_14.26.csv",na.strings=c("NA","NaN", ""))
eng1 <- surveyave(dat.e1)
colnames(eng1)[2] <- 'usernetid'
eng1$usernetid <- gsub(" ", "", tolower(gsub("@.*","",eng1$usernetid)))
eng1$StartDate <- as.POSIXct(eng1$StartDate,format="%Y-%m-%d %H:%M:%S",tz=Sys.timezone())
timediff <- 2 * 60 * 60
eng1$StartDate <- eng1$StartDate + timediff
#firt 2 responses in the eng 1 survey are tests
df.survey1 <- eng1[3:123,]
df.survey1 <- df.survey1[!duplicated(df.survey1$usernetid),]

dat.e1<- read.csv("engagement2_March+3,+2023_15.01.csv",na.strings=c("NA","NaN", ""))
eng1 <- surveyave(dat.e1)
colnames(eng1)[2] <- 'usernetid'
eng1$usernetid <- gsub(" ", "", tolower(gsub("@.*","",eng1$usernetid)))
eng1$StartDate <- as.POSIXct(eng1$StartDate,format="%Y-%m-%d %H:%M:%S",tz=Sys.timezone())
timediff <- 2 * 60 * 60
eng1$StartDate <- eng1$StartDate + timediff
df.survey2 <- eng1
df.survey2 <- df.survey2[!duplicated(df.survey2$usernetid),]

dat.e1<- read.csv("engagement3_March+3,+2023_15.10.csv",na.strings=c("NA","NaN", ""))
eng1 <- surveyave(dat.e1)
colnames(eng1)[2] <- 'usernetid'
eng1$usernetid <- gsub(" ", "", tolower(gsub("@.*","",eng1$usernetid)))
eng1$StartDate <- as.POSIXct(eng1$StartDate,format="%Y-%m-%d %H:%M:%S",tz=Sys.timezone())
timediff <- 2 * 60 * 60
eng1$StartDate <- eng1$StartDate + timediff
df.survey3 <- eng1
df.survey3 <- df.survey3[!duplicated(df.survey3$usernetid),]

dat.ave <-rbind(df.survey1 %>% 
  pivot_longer(emo:socbe,names_to = 'eng',values_to = 'result') %>% 
  mutate(time = 't1'),
df.survey2 %>% 
  pivot_longer(emo:socbe,names_to = 'eng',values_to = 'result') %>% 
  mutate(time = 't2'),
df.survey3 %>% 
  pivot_longer(emo:socbe,names_to = 'eng',values_to = 'result') %>% 
  mutate(time = 't3'))

# summary <- dat.ave %>%
#   group_by(eng,time) %>%
#   get_summary_stats(result, type = "mean_ci")

not_sel <- c("Not Selected","all")

DataInput_page <- tabPanel(
  title = "DataInput",
  titlePanel("DataInput"),
  mainPanel(
    tabsetPanel(
      tabPanel(
        title = "Log Data",
        tableOutput("logdata")
      ),
      tabPanel(
        title = "Survey1 Data",
        tableOutput("survey1")
      ),
      tabPanel(
        title = "Survey2 Data",
        tableOutput("survey2")
      ),
      tabPanel(
        title = "Survey3 Data",
        tableOutput("survey3")
      ),
      tabPanel(
        title = "logsurveytable",
        tableOutput("logsurveytable")
      )
    )
  )
)


Log_page <- tabPanel(
  title = "Log",
  titlePanel("Log Analysis"),
  sidebarLayout(
    sidebarPanel(
      title = "Log",
      selectInput("event", "event", choices = c(not_sel)),
      selectInput("group", "group", choices = c('P','EL','LL'),multiple = TRUE),
      tableOutput("data")
      
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          title = "Log Summary",
          plotOutput("plot_ls"),
        ),
        tabPanel(
          title = "Log by Time",
          plotOutput("plot_lp", brush = "plot_brush"),
        )
      )
    )
  )
)

Survey_page <- tabPanel(
  title = "Survey",
  titlePanel("Survey Analysis"),
  sidebarLayout(
    sidebarPanel(
      title = "Survey",
      selectInput("engage", "engage", choices = c(not_sel)),
      selectInput("group2", "group", choices = c('P','EL','LL'),multiple = TRUE),
      tableOutput("data2")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          title = "Survey Summary",
          plotOutput("plot_ss"),
        ),
        tabPanel(
          title = "SurveybyTime",
          plotOutput("plot_sp", brush = "plot_brush2")
        )
      )
    )
  )
)

LogSurvey_page <- tabPanel(
  title = "LogSurvey",
  titlePanel("Log-Survey"),
  sidebarLayout(
    sidebarPanel(
      title = "LogSurvey",
      selectInput("time", "time", choices = c(not_sel)),
      selectInput("group3", "group", choices = c('P','EL','LL'),multiple = TRUE),
      selectInput("method","method", choices = c("pearson","spearman")),
      selectInput("event3", "event", choices = c(not_sel)),
      selectInput("engage3", "engage", choices = c(not_sel)),
      tableOutput("data3")
      
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          title = "Correlation Summary",
          plotOutput("plot_cs"),
        ),
        tabPanel(
          title = "CorrelationbyPair",
          plotOutput("plot_cp"),
        )
      )
    )
  )
)





###########################
###########################
######APP FROM HERE########
###########################
###########################


ui <- navbarPage(
  title = "Log Survey Lab",
  theme = shinytheme('lumen'),
  DataInput_page,
  Log_page,
  Survey_page,
  LogSurvey_page,
  
)

server <- function(input, output, session) {
  

  df <- reactive({
    dat.log %>% 
      filter(!is.na(usernetid) & usernetid != "") %>% 
      mutate(usernetid= gsub(" ", "", tolower(gsub("@.*","",usernetid)))) 
  })
  
  dfsur <- reactive({
    dat.ave %>% 
      mutate(usernetid= gsub(" ", "", tolower(gsub("@.*","",usernetid)))) 
  })
  
  dfsurvey1 <- reactive({
    df.survey1
  })

  dfsurvey2 <- reactive({
    df.survey2
  })

  dfsurvey3 <- reactive({
    df.survey3
  })
  
  output$logdata <- renderTable(
    df() %>% 
      mutate(trigger_day = as.character(trigger_day),trigger_time =as.character(trigger_time)) 
    
  )
  
  output$survey1 <- renderTable(
    dfsurvey1() %>% 
      mutate(StartDate = as.character(StartDate))
  )

  output$survey2 <- renderTable(
    dfsurvey2() %>% 
      mutate(StartDate = as.character(StartDate))
  )

  output$survey3 <- renderTable(
    dfsurvey3() %>% 
      mutate(StartDate = as.character(StartDate))
  )

  
  observeEvent(df(),{
    choices1 <- c(not_sel,unique(dat.ave$time))
    updateSelectInput(inputId = "time", choices = choices1)
    choices2 <-c(not_sel,unique(df()$event))
    updateSelectInput(inputId = "event", choices = choices2)
    choices3 <- c(not_sel,unique(dfsur()$eng))
    updateSelectInput(inputId = "engage", choices = choices3)
    choices4 <-c(not_sel,colnames(surveylog()))
    updateSelectInput(inputId = "event3", choices = choices4)
    choices5 <- c(not_sel,unique(dfsur()$eng))
    updateSelectInput(inputId = "engage3", choices = choices5)
    
  })
  
  output$plot_ls <- renderPlot({
    df() %>% 
      filter(if(input$event == "all") TRUE else event == input$event) %>% 
      mutate(group= ifelse(usernetid %in% Lnetid,ifelse(usernetid %in% ELnetid, 'EL','LL'),'P')) %>% 
      filter(group %in% input$group) %>% 
      group_by(trigger_day,group) %>% 
      summarise(y = n()) %>% 
      ggplot()+
      geom_line(aes(trigger_day,y,color = factor(group)))
  }, res=96)

  output$plot_lp <- renderPlot({
    df() %>%
      filter(if(input$event == "all") TRUE else event == input$event) %>%
      group_by(trigger_day,usernetid) %>%
      summarise(y = n()) %>% 
      mutate(group= ifelse(usernetid %in% Lnetid,ifelse(usernetid %in% ELnetid, 'EL','LL'),'P')) %>% 
      filter(group %in% input$group) %>% 
      ggplot()+
      geom_point(aes(trigger_day,y,colour = factor(group)),position = position_dodge(0.02))+
      geom_line(aes(trigger_day,y,group=usernetid,colour = factor(group)),position = position_dodge(0.02))+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  }, res = 96)


  output$data <- renderTable({
    brushedPoints(df() %>%
                    filter( event == input$event) %>%
                    group_by(trigger_day,usernetid) %>%
                    summarise(y = n()) %>% 
                   # mutate(trigger_day = as.character(trigger_day)) %>% 
                    mutate(group= ifelse(usernetid %in% Lnetid,ifelse(usernetid %in% ELnetid, 'EL','LL'),'P')), input$plot_brush)
  })
  
  output$plot_ss <- renderPlot({
    dfsur() %>%
      mutate(group= ifelse(usernetid %in% Lnetid,ifelse(usernetid %in% ELnetid, 'EL','LL'),'P')) %>%
      filter(group %in% input$group2) %>%
      group_by(eng,time) %>%
      get_summary_stats(result, type = "mean_ci") %>% 
      ggplot(aes(x=time, y=mean, group=eng, color=eng))+
      geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.1, position=position_dodge(width=.3)) +
      geom_point(position=position_dodge(width=.3))+
      geom_line(position=position_dodge(width=.3))
  })
  
  output$plot_sp <- renderPlot({
    dfsur() %>%
      filter(if(input$engage == "all") TRUE else eng == input$engage ) %>%
      mutate(group= ifelse(usernetid %in% Lnetid,ifelse(usernetid %in% ELnetid, 'EL','LL'),'P')) %>%
      filter(group %in% input$group2) %>%
      ggplot()+
      geom_point(aes(StartDate,result,color = factor(eng)))+
      geom_line(aes(StartDate,result,group = interaction(usernetid,eng),color = factor(eng)))+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  }, res = 96)
  
  output$data2 <- renderTable({
    brushedPoints(dfsur() %>%
                    filter(if(input$engage == "all") TRUE else eng == input$engage ) %>%
                    mutate(group= ifelse(usernetid %in% Lnetid,ifelse(usernetid %in% ELnetid, 'EL','LL'),'P')), input$plot_brush2)
  })
  
  
  surveylog <- reactive({
    if(input$time == 't1')
      datprocess(df(),dfsurvey1()) %>% 
        left_join(dfsurvey1(),by='usernetid')
    else if(input$time == 't2')
      datprocess(df(),dfsurvey2()) %>% 
        left_join(dfsurvey2(),by='usernetid')
    else
      datprocess(df(),dfsurvey3()) %>% 
        left_join(dfsurvey3(),by='usernetid')
      
  })
  
  output$logsurveytable <- renderTable({
    surveylog()
  })
  
  output$plot_cs <- renderPlot({
    mat <- surveylog() %>% 
      mutate(group= ifelse(usernetid %in% Lnetid,ifelse(usernetid %in% ELnetid, 'EL','LL'),'P')) %>%
      filter(group %in% input$group3) %>%
      select_if(is.numeric) 
    mat <- mat[, colSums(mat) != 0]
    if(input$method == "pearson"){
      M <- cor(mat)
      p.mat <- cor.mtest(mat)
      corrplot(M, type = "upper", order = "hclust", 
               p.mat = p.mat, sig.level = 0.05)
    }else{
      M <- cor(mat, method = "spearm")
      p.mat <- cor.mtest(mat, method = "spearm")
      corrplot(M, type = "upper", order = "hclust",
               p.mat = p.mat, sig.level = 0.05)
    }
  })
  
  output$plot_cp <- renderPlot({
    surveylog() %>% 
      mutate(group= ifelse(usernetid %in% Lnetid,ifelse(usernetid %in% ELnetid, 'EL','LL'),'P')) %>%
      filter(group %in% input$group3) %>%
      ggplot()+
      geom_point(aes(.data[[input$event3]],.data[[input$engage3]]),position = position_dodge(0.02))+
      stat_cor(aes(.data[[input$event3]],.data[[input$engage3]]),method = "pearson")

  },res=96)
  
  

  
}

shinyApp(ui, server)