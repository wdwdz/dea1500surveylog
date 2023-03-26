# survey log project
# library(stats)
library(dplyr)
library(ggplot2)
library(lubridate)
library(corrplot)

# """
# 1. Navigator
#     Message 1
#     logout 2
#     login 3
#     sign up 4
#     icon 5
#     current 26
#     create 27
#     community 28
# 2. Main page
#     sign up 6
# 3. Login page
#     login button 7
# 4. Register page
#     sign up 8
# 5. Current page
#     create 9
#     update 10
#     chat 11
#     agree 12
# 6. Create page
#     create 13
# 7. Community page
#     sort 14
#     like 15
#     cool 16
#     funny 17
#     useful 18
#     cancellike 25
#     cancelcool 19
#     cancelfunny 20
#     canceluseful 21
#     post 22
#     group 29
#     next 30
#     previous 31
# 8. profile page
#     add to friend 23
# 9. Message
#     send message 24
# """

#survey : <40; <43; <46; <49

###############log
dat.log <- read.csv('log.csv')
head(dat.log)
user <- read.csv("user.csv")
user$id <- factor(user$id)
dat.log$uid <- factor(dat.log$user_id)
# log sum by day
dat.log$day <- as.Date(dat.log$trigger_time)
temp <- dat.log %>%
  group_by(day) %>%
  count
temp <- data.frame(temp)
str(temp)
ggplot(data = temp) +
  geom_line(aes(x = day, y = n))

dat.log$week <- week(dat.log$day)

temp <- dat.log %>%
  group_by(user_id,week) %>%
  count

# log sum by user
logsum <- table(dat.log$uid)
logsum <- data.frame(sort(logsum, decreasing = TRUE))
dat <- left_join(logsum, user, by=c('Var1'='id'))
temp <- dat %>% select(username,Var1,email,Freq)
colnames(temp) <- c('username', 'uid','email','logsum')
temp$usernetid <- gsub(" ", "", tolower(gsub("@.*","",temp$email)))
datsum <- aggregate(logsum ~ usernetid, temp, sum)
dat.survey <- read.csv("survey.csv")
dat.survey <- dat.survey %>% 
  left_join(datsum,by='usernetid')
length(unique(datsum))

# filter logs from 126 users
uid <- temp[temp$usernetid %in% dat.survey$usernetid,'uid']
dat.fil <- dat.log[dat.log$user_id %in% uid,]
dat.fil <- dat.fil %>% 
  left_join(temp,by='uid')
colnames(dat.fil)
dat.fil <- dat.fil %>% 
  select(event, obj, trigger_time, device, day, week, usernetid)

temp <- dat.log %>%
  group_by(day) %>%
  count
temp <- data.frame(temp)

temp2 <- dat.fil %>% 
  group_by(day) %>% 
  count
temp2 <- data.frame(temp2)

ggplot()+
  geom_line(data = temp, aes(x = day, y = n))+
  geom_line(dat = temp2, aes(x = day, y = n, color = 'red'))

temp <- dat.log %>%
  group_by(week) %>%
  count
temp <- data.frame(temp)

temp2 <- dat.fil %>% 
  group_by(week) %>% 
  count
temp2 <- data.frame(temp2)

# Total logs by week
ggplot()+
  geom_line(data = temp, aes(x = week, y = n))+
  geom_line(dat = temp2, aes(x = week, y = n, color = 'red'))

# Total logs by event
ggplot()+
  geom_histogram(data = dat.fil, aes(x=factor(event)), stat="count")

# 1. Navigator
#     Message 1
#     current 26
#     create 27
#     community 28
# 2. Main page
#     sign up 6
# 5. Current page
#     create 9
#     update 10
# 6. Create page
#     create 13
# 7. Community page
#     sort 14
#     cancelcool 19
#     post 22
#     group 29
# Total time by week
head(dat.fil$trigger_time)
dat.fil$time <- as.POSIXct(dat.fil$trigger_time,format="%Y-%m-%d %H:%M:%S",tz=Sys.timezone())

###########################################################
##########This part is for informal analysis can be skipped
###########################################################
# Total logs t1
tot_log_t1 <- dat.fil %>% 
  filter(week < 40) %>% 
  group_by(usernetid) %>% 
  count() %>% 
  left_join(dat.survey,by='usernetid') %>% 
  select(usernetid, n,t1_be, t1_cog)

plot(tot_log_t1$n,tot_log_t1$t1_be)
cor(tot_log_t1$n,tot_log_t1$t1_be, use = "complete.obs")
cor(tot_log_t1$n,tot_log_t1$t1_cog, use = "complete.obs")


tot_log_t2 <- dat.fil %>% 
  filter(week < 43) %>% 
  group_by(usernetid) %>% 
  count() %>% 
  left_join(dat.survey,by='usernetid') %>% 
  select(usernetid, n,t2_be, t2_cog)

plot(tot_log_t2$n,tot_log_t2$t2_be)
cor(tot_log_t2$n,tot_log_t2$t2_be, use = "complete.obs")
cor(tot_log_t2$n,tot_log_t2$t2_cog, use = "complete.obs")
cor.test(tot_log_t2$n,tot_log_t2$t2_be, use = "complete.obs")
cor.test(tot_log_t2$n,tot_log_t2$t2_cog, use = "complete.obs")
cor.test(tot_log_t2$n,tot_log_t2$t2_be, use = "complete.obs",method = "spearman")


unique(dat.fil$usernetid)


#########################################
#########################################
#########################################
###########START HERE####################
#########################################
#########################################
#########################################

library(stats)
library(dplyr)
library(ggplot2)
library(lubridate)
library(corrplot)

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
  dat.e1$emo<-rowMeans(dat.e1[,c('Q5','Q6','Q7')])
  dat.e1$cog <- rowMeans(dat.e1[,c('Q8','Q9','Q10')])
  dat.e1$be <- rowMeans(dat.e1[,c('Q11','Q12')])
  dat.e1$soc <- rowMeans(dat.e1[,c('Q13','Q14')])
  
  dat.e1.ave <- dat.e1 %>% 
    dplyr::select(StartDate,Q20,emo,cog,be,soc)
  
  return(dat.e1.ave)
  
}


# Main data processing function for process survey and log data
datprocess<- function(dat.fil,eng1){
  df.output <- data.frame(matrix(ncol = 34, nrow = 0))
  colnames(df.output) <- c("usernetid",as.character(seq(1:30)),"total_time","total_freq","ave_time")
  thisrow <- 1
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

#########################################
###########INPUT OUTPUT##################
#########################################

##### Read and process dat.fil and dat.survey
### dat.fil is constant
dat.log <- read.csv('log.csv')
user <- read.csv("user.csv")
user$id <- factor(user$id)
dat.log$uid <- factor(dat.log$user_id)
# log sum by user
logsum <- table(dat.log$uid)
logsum <- data.frame(sort(logsum, decreasing = TRUE))
dat <- left_join(logsum, user, by=c('Var1'='id'))
temp <- dat %>% select(username,Var1,email,Freq)
colnames(temp) <- c('username', 'uid','email','logsum')
temp$usernetid <- gsub(" ", "", tolower(gsub("@.*","",temp$email)))
datsum <- aggregate(logsum ~ usernetid, temp, sum)
dat.survey <- read.csv("survey.csv")
dat.survey <- dat.survey %>% 
  left_join(datsum,by='usernetid')
# filter logs from 126 users
uid <- temp[temp$usernetid %in% dat.survey$usernetid,'uid']
dat.fil <- dat.log[dat.log$user_id %in% uid,]
dat.fil <- dat.fil %>% 
  left_join(temp,by='uid')
dat.fil$trigger_time <- as.POSIXct(dat.fil$trigger_time,format="%Y-%m-%d %H:%M:%S",tz=Sys.timezone())

### dat.survey is each of the 4 engagement surveys
### engagement1_February+25,+2023_14.26.csv
### engagement2_March+3,+2023_15.01.csv
### engagement3_March+3,+2023_15.10.csv
dat.e1<- read.csv("engagement2_March+3,+2023_15.01.csv",na.strings=c("NA","NaN", ""))
eng1 <- surveyave(dat.e1)
colnames(eng1)[2] <- 'usernetid'
eng1$usernetid <- gsub(" ", "", tolower(gsub("@.*","",eng1$usernetid)))
eng1$StartDate <- as.POSIXct(eng1$StartDate,format="%Y-%m-%d %H:%M:%S",tz=Sys.timezone())
timediff <- 2 * 60 * 60
eng1$StartDate <- eng1$StartDate + timediff
#firt 2 responses in the eng 1 survey are tests
#df.survey <- eng1[3:123,]
df.survey <- eng1
df.survey <- df.survey[!duplicated(df.survey$usernetid),]

###### PROECESS and VIS

surveylog <- datprocess(dat.fil,df.survey)
surveylog <- surveylog %>% 
  left_join(df.survey,by='usernetid')

mat <- surveylog %>% 
  select_if(is.numeric)
mat <- mat[, colSums(mat) != 0]

M <- cor(mat)
p.mat <- cor.mtest(mat)
#p.mat <- p.adjust(p.mat, method = "BH")
#colnames(p.mat) <- rownames(p.mat) <- colnames(mat)

corrplot(M, type = "upper", order = "hclust", 
         p.mat = p.mat, sig.level = 0.05)


M <- cor(mat, method = "spearm")
p.mat <- cor.mtest(mat, method = "spearm")


corrplot(M, type = "upper", order = "hclust", 
         p.mat = p.mat, sig.level = 0.05, 
         insig = "blank")



####################################
####################################
######DIFFERENT USERS###############
####################################
####################################


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

user$id <- as.numeric(user$id)
dat.log <- read.csv("log_log.csv")
dat.log <- dat.log %>% 
  left_join(uniquetable,by=c('user_id' = 'id'))

dat.log$trigger_time <- as.POSIXct(dat.log$trigger_time,format="%Y-%m-%d %H:%M:%S",tz=Sys.timezone())
ELnetid <- gsub(" ", "", tolower(gsub("@.*","",Early_leaver)))
Lnetid <- gsub(" ", "", tolower(gsub("@.*","",Leaver)))
LLnetid <- gsub(" ", "", tolower(gsub("@.*","",Late_leaver)))

###TIME 1

df.log1 <- dat.log %>% 
  filter(trigger_time < t1) %>% 
  group_by(email) %>% 
  filter(event == 27) %>% 
  summarise(log_27 = n()) 
df.log1$email <- gsub(" ", "", tolower(gsub("@.*","",df.log1$email)))


dat.e1<- read.csv("engagement1_February+25,+2023_14.26.csv",na.strings=c("NA","NaN", ""))
eng1 <- surveyave(dat.e1)
colnames(eng1)[2] <- 'usernetid'
eng1$usernetid <- gsub(" ", "", tolower(gsub("@.*","",eng1$usernetid)))
df.survey <- eng1[3:123,]
df.survey <- eng1
df.survey <- df.survey[!duplicated(df.survey$usernetid),]


dat.com <- df.log1 %>% 
  left_join(df.survey, by=c('email' = 'usernetid')) %>% 
  filter(!is.na(email) & email != "") %>% 
  select(email, log_27, emo,soc,be,cog)
#dat.com$cat <- ifelse(dat.com$email %in% ELnetid, 'L','P')
dat.com$cat <- ifelse(dat.com$email %in% Lnetid, ifelse(dat.com$email %in% LLnetid, 'LL','EL'),'P')
dat1 <- dat.com
dat.com %>% 
  group_by(cat) %>% 
  summarise(n = n(),log = mean(log_27),emo = mean(emo,na.rm = TRUE),soc = mean(soc,na.rm = TRUE),be = mean(be,na.rm = TRUE),cog = mean(cog,na.rm = TRUE))

###TIME 2

df.log2 <- dat.log %>% 
  filter(trigger_time < t2) %>% 
  group_by(email) %>% 
  filter(event == 27) %>% 
  summarise(log_27 = n()) 

df.log2$email <- gsub(" ", "", tolower(gsub("@.*","",df.log2$email)))

dat.e2<- read.csv("engagement2_March+3,+2023_15.01.csv",na.strings=c("NA","NaN", ""))
eng2 <- surveyave(dat.e2)
colnames(eng2)[2] <- 'usernetid'
eng2$usernetid <- gsub(" ", "", tolower(gsub("@.*","",eng2$usernetid)))
df.survey <- eng2
df.survey <- df.survey[!duplicated(df.survey$usernetid),]

dat.com <- df.log2 %>% 
  left_join(df.survey, by=c('email' = 'usernetid')) %>% 
  filter(!is.na(email) & email != "") %>% 
  select(email, log_27, emo,soc,be,cog)
dat.com$cat <- ifelse(dat.com$email %in% Lnetid, ifelse(dat.com$email %in% LLnetid, 'LL','EL'),'P')
dat2 <- dat.com
dat.com %>% 
  group_by(cat) %>% 
  summarise(n = n(),log = mean(log_27),emo = mean(emo,na.rm = TRUE),soc = mean(soc,na.rm = TRUE),be = mean(be,na.rm = TRUE),cog = mean(cog,na.rm = TRUE))

dat2[!(dat2$email %in% dat1$email),'email']
dat1[!(dat1$email %in% dat2$email),'email']
View(useremail)
####################################
####################################
######CORRELATION BY DIFFERENT USERS
####################################
####################################
### Take eng 2 survey for example
### dat.survey is each of the 4 engagement surveys
### engagement1_February+25,+2023_14.26.csv
### engagement2_March+3,+2023_15.01.csv
### engagement3_March+3,+2023_15.10.csv
dat.e1<- read.csv("engagement1_February+25,+2023_14.26.csv",na.strings=c("NA","NaN", ""))
eng1 <- surveyave(dat.e1)
colnames(eng1)[2] <- 'usernetid'
eng1$usernetid <- gsub(" ", "", tolower(gsub("@.*","",eng1$usernetid)))
eng1$StartDate <- as.POSIXct(eng1$StartDate,format="%Y-%m-%d %H:%M:%S",tz=Sys.timezone())
timediff <- 2 * 60 * 60
eng1$StartDate <- eng1$StartDate + timediff
#firt 2 responses in the eng 1 survey are tests
#df.survey <- eng1[3:123,]
df.survey <- eng1
df.survey <- df.survey[!duplicated(df.survey$usernetid),]

###### PROECESS and VIS

surveylog <- datprocess(dat.fil,df.survey)
surveylog <- surveylog %>% 
  left_join(df.survey,by='usernetid') %>% 
  mutate(
    usergroup = ifelse(usernetid %in% Lnetid, ifelse(usernetid %in% LLnetid, 'LL','EL'),'P'),
    usergroup2 = ifelse(usernetid %in% Lnetid, 'L','P')
  ) 

table(surveylog$usergroup2)

# Only select P (participants) data L (leaver)
mat <- surveylog %>% 
  filter(usergroup2 == 'L') %>% 
  select_if(is.numeric) 

mat <- mat[, colSums(mat) != 0]

M <- cor(mat)
p.mat <- cor.mtest(mat)
#p.mat <- p.adjust(p.mat, method = "BH")
#colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
corrplot(M, type = "upper", order = "hclust", 
         p.mat = p.mat, sig.level = 0.05)


M <- cor(mat, method = "spearm")
p.mat <- cor.mtest(mat, method = "spearm")
corrplot(M, type = "upper", order = "hclust", 
         p.mat = p.mat, sig.level = 0.05)




###########################
###########################
########FACTOR ANALYSIS####
###########################
###########################
library(psych)
library(GPArotation)
surveymat <- function(dat.e1){
  dat.e1 <- dat.e1[-1:-2,]
  dat.e1 <- dat.e1[!is.na(dat.e1$Q20),]
  #colnames(dat.e1)
  
  for (col in c('Q5','Q6','Q7','Q8','Q9','Q10','Q11','Q12','Q13','Q14')) {
    dat.e1[,col] <- as.numeric(factor(dat.e1[,col], levels = c("Strongly disagree","Somewhat disagree","Neither agree nor disagree","Somewhat agree","Strongly agree")))
  }
  
  dat.e1<- dat.e1[!is.na(dat.e1$Q6),]
  dat.e1$Q7 <- 6 - dat.e1$Q7
  
  
  mat <- dat.e1 %>% 
    dplyr::select('Q20','Q5','Q6','Q7','Q8','Q9','Q10','Q11','Q12','Q13','Q14')
  
  return(mat)
  
}
### engagement1_February+25,+2023_14.26.csv
### engagement2_March+3,+2023_15.01.csv
### engagement3_March+3,+2023_15.10.csv

dat.e1<- read.csv("engagement1_February+25,+2023_14.26.csv",na.strings=c("NA","NaN", ""))
eng1 <- surveymat(dat.e1)
#firt 2 responses in the eng 1 survey are tests
#df.survey <- eng1[3:123,]
df.survey <- eng1
df.survey <- df.survey[!duplicated(df.survey$Q20),]
envi_EFA <- df.survey[,-1]

# Calculate the correlation matrix first
envi_EFA_cor <- cor(envi_EFA, use = "pairwise.complete.obs")
# Decide the number of factors
fa.parallel(envi_EFA_cor, n.obs = nrow(envi_EFA), fm = "ml", n.iter = 100, main = "Scree plots with parallel analysis")
# Factor analysis, number of factors is 3
fa <- fa(envi_EFA_cor, nfactors = 3, rotate = "none", fm = "pa")
fa
# varimaxal Rotation 
fa.varimax <- fa(envi_EFA_cor, nfactors = 3, rotate = "varimax", fm = "pa")
fa.varimax
# Promaxal rotation
fa.promax<- fa(envi_EFA_cor,nfactors = 3, rotate = "promax",fm = "pa")
h<-fa.promax$loadings

fa.diagram(fa.promax,simple =TRUE)
# Cronbach alpha
alpha(envi_EFA2[,c(18:21)])

testdata <- read.csv("shinylog.csv")
testdata %>% ggplot(aes(event)) +
  geom_histogram()
  
dat.log$trigger_time <- as.POSIXct(dat.log$trigger_time,format="%Y-%m-%d %H:%M:%S",tz=Sys.timezone())
dat.log$trigger_day <- as.Date(dat.log$trigger_time)
dat.log %>% 
  filter(!is.na(email) & email != "") %>% 
  filter( event == 27) %>% 
  group_by(trigger_day,email) %>% 
  summarise(y = n()) %>% 
  ggplot()+ 
  geom_point(aes(trigger_day,y),position = position_dodge(0.02))+
  geom_line(aes(trigger_day,y,group=email),position = position_dodge(0.02))
