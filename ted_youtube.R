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

talks <- read.csv("talks.csv",header = T)

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
  aes(view_ted) + 
  geom_histogram(binwidth = 500000,fill="darkorange2")+
  xlim(0,20000000)+
  labs(title = "distribution of views",x = "number of views")

ggplot(data = talks) + 
  aes(duration_ted/60) + 
  geom_histogram(binwidth = 1,fill="hotpink3")+
  labs(title = "distribution of duration",x = "duration/min")

## occupation
talks %>%
  count(first.occupation, sort = TRUE) %>%
  filter(n > 25) %>%
  mutate(`first occupation` = reorder(first.occupation, n)) %>%
  ggplot(aes(n, first.occupation)) +
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
  aes(view_ted,comments)+
  geom_point(alpha = 0.3,color = "orchid3")+
  xlim(0,20000000)+
  ylim(0,2000)+
  labs(title = "views vs comments")+
  geom_smooth(method = "lm")

ggplot(data = talks)+
  aes(duration_ted,comments)+
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
talks %<>% mutate(duration_min = duration_ted/60)
talks %<>% mutate(log_views = log(view_ted))
talks %<>% mutate(log_comments = log(comments))
talks %<>% filter(log_comments>=0)
talks %<>% mutate(english = ifelse(native_lang=="en",1,0))
talks %<>% mutate(s_duration = (duration_ted-mean(duration_ted))/sd(duration_ted))
talks %<>% mutate(s_views = (view_ted-mean(view_ted))/sd(view_ted))
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
