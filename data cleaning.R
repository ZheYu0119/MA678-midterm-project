library(readr)
library(tidyverse)
library(stringr)
library(rstanarm)
library(knitr)
library(magrittr)
library(kableExtra)
library(gridExtra)
library(tidytext)
library(lubridate)
library(car) 
data("stop_words")

# import data
ted_talks_en <- read_csv("ted dataset/ted_talks_en.csv")
ted_talks_en <- na.omit(ted_talks_en)
talks <- ted_talks_en[,c(1,2,3,4,5,7,8,9,10,11,12,13,14,17)]

# clean data

## count the number of speakers
talks$num_speakers <- str_extract_all(talks$all_speakers,"[0-4]")
talks$num_speakers <- as.character(talks$num_speakers)
talks$num_speakers <- str_count(talks$num_speakers)
talks$num_speakers = c('1'=1,'11'=2,'16'=3,'21'=4,'26'=5)[ as.character(talks$num_speakers)]


## count the number of available language
talks$num_lang <- str_count(talks$available_lang,",")
talks$num_lang <- talks$num_lang+1


## clean column `occupation`
talks %<>% separate(occupations, c("occ1","occ2"), sep = ",") 
talks %<>% separate(occ1, c("de1","first occupation","de2"), sep = "'")
talks %<>% separate(occ2, c("de3","second occupation","de4"), sep = "'")
talks %<>% select(-c("de1","de2","de3","de4"))


ted_talks<- read_csv("ted_metadata_kaggle.csv")
ted_youtube <- read_csv("ted_metadata_youtube.csv")
colnames(ted_talks)[19] <- "id"

df <- left_join(ted_talks,ted_youtube,"id")
df <- na.omit(df)
df <- df[,c(2,4,5,7,8,10,14,16:18,22,26,28,30:32)]
colnames(df)[2] <- "duration_ted"
colnames(df)[8] <- "title"
colnames(df)[10] <- "view_ted"
colnames(df)[11] <- "view_yt"
colnames(df)[13] <- "duration_yt"

df2 <- left_join(talks,df,"title")
df2 %<>% filter(is.na(average_rating)==F)
df2 <- df2[,c(1:3,5:6,8:11,15:19,21,23,26:32)]
colnames(df2)[13] <- "comments"
colnames(df2)[8] <- "event"
write.csv(df2,"talks.csv",row.names = F)
# EDA

## number of speakers
ggplot(data = talks) + 
  aes(num_speakers) + 
  geom_histogram(binwidth = 1,fill="skyblue")+
  labs(title = "distribution of number of speakers",x = "number of speakers")

##
ggplot(data = talks) + 
  aes(x=num_lang,y=..density..) + 
  geom_histogram(binwidth = 2,fill="steelblue")+
  labs(title = "distribution of available languages",x = "number of available languages")

ggplot(data = talks) + 
  aes(comments) + 
  geom_histogram(binwidth = 100,fill="darkseagreen")+
  xlim(0,2000)+
  labs(title = "distribution of comments",x = "number of comments")

ggplot(data = talks) + 
  aes(views) + 
  geom_histogram(binwidth = 500000,fill="darkorange")+
  xlim(0,20000000)+
  labs(title = "distribution of views",x = "number of views")

ggplot(data = talks) + 
  aes(duration/60) + 
  geom_histogram(binwidth = 1,fill="hotpink3")+
  labs(title = "distribution of duration",x = "duration/min")

## occupation
talks %>%
count(`first occupation`, sort = TRUE) %>%
  filter(n > 25) %>%
  mutate(`first occupation` = reorder(`first occupation`, n)) %>%
  ggplot(aes(n, `first occupation`)) +
  geom_col(fill="tan2") +
  labs(y = NULL)
  
## speaker
talks %>%
  count(speaker_1, sort = TRUE) %>%
  filter(n > 4) %>%
  mutate(speaker_1 = reorder(speaker_1, n)) %>%
  ggplot(aes(n, speaker_1)) +
  geom_col(fill="tan3") +
  labs(y = NULL)

##events
talks %>%
  count(event, sort = TRUE) %>%
  filter(n > 50) %>%
  mutate(event = reorder(event, n)) %>%
  ggplot(aes(n, event)) +
  geom_col(fill="tan4") +
  labs(y = NULL)

## year
talks$recorded_date <- ymd(talks$recorded_date)
talks$published_date <- ymd(talks$published_date)
talks$published_year <- as.factor(year(talks$published_date))
talks$published_month <- as.factor(month(talks$published_date))
ggplot(talks,aes(published_year))+
  geom_bar()+
  labs(title = "distribution of published year",x="published year")
ggplot(talks,aes(published_month))+
  geom_bar()+
  labs(title = "distribution of published month",x="published month")
ggplot(talks,aes(published_date,comments))+geom_line()

## scatter plot
ggplot(data = talks)+
  aes(views,comments)+
  geom_point(alpha = 0.3,color = "orchid3")+
  xlim(0,20000000)+
  ylim(0,2000)+
  labs(title = "views vs comments")+
  geom_smooth(method = "lm")

ggplot(data = talks)+
  aes(duration,comments)+
  geom_point(alpha = 0.3,color = "tomato3")+
  scale_fill_brewer(direction = -1)+
  ylim(0,2000)+
  xlim(0,2000)+
  labs(title = "comments vs duration")+
  geom_smooth()

ggplot(data = talks)+
  aes(num_lang,comments)+
  geom_point(alpha = 0.3,color = "brown4")+
  labs(title = "comments vs duration")+
  geom_smooth()

#text mining
topics <- data.frame(talks_id = ted_talks_en$talk_id, text = ted_talks_en$topics)
topics %<>% unnest_tokens(word, text) %>% anti_join(stop_words)
ggplot(topics[1:10,],aes(word,n))+
  geom_col()+
  coord_flip()

topics %>%
  count(word, sort = TRUE) %>%
  filter(n > 500) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col(fill="tan3") +
  labs(y = NULL)

##data process
talks %<>% mutate(duration_min = duration/60)
talks %<>% mutate(log_views = log(views))
talks %<>% mutate(log_comments = log(comments))
talks %<>% filter(log_comments>=0)
talks %<>% mutate(english = ifelse(native_lang=="en",1,0))
talks %<>% mutate(s_duration = (duration-mean(duration))/sd(duration))
talks %<>% mutate(s_views = (views-mean(views))/sd(views))
talks %<>% mutate(s_numlang = (num_lang-mean(num_lang)/sd(num_lang)))

##model
fit1 <- lm(comments~duration_min+log_views+num_lang,talks)
summary(fit1)

ggplot()+
  geom_point(aes(fit1$fitted.values,fit1$residuals))+
  geom_smooth(aes(fit1$fitted.values,fit1$residuals))

par(mfrow = c(2,2))
plot(fit1)

fit2 <- lm(log_comments~duration_min+log_views+num_lang,talks)
par(mfrow = c(2,2))
plot(fit2)

fit3 <- lm(log_comments~s_duration+s_views+s_numlang,talks)
summary(fit3)
par(mfrow = c(2,2))
plot(fit3)

outlierTest(fit3)
influence.measures(fit3)
