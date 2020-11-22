library(readr)
library(tidyverse)
library(stringr)
library(rstanarm)
library(knitr)
library(magrittr)
library(kableExtra)
library(gridExtra)
library(tidytext)
data("stop_words")

# import data
ted_talks_en <- read_csv("ted dataset/ted_talks_en.csv")
ted_talks_en <- na.omit(ted_talks_en)
talks <- ted_talks_en[,c(1,3,4,5,7,8,9,10,11,12,13,14)]

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


# EDA

## number of speakers
ggplot(data = talks) + 
  aes(num_speakers) + 
  geom_histogram(binwidth = 1,fill="skyblue")

##
ggplot(data = talks) + 
  aes(x=num_lang,y=..density..) + 
  geom_histogram(binwidth = 2,fill="steelblue")

ggplot(data = talks) + 
  aes(comments) + 
  geom_histogram(binwidth = 100,fill="darkseagreen")+
  xlim(0,2000)

ggplot(data = talks) + 
  aes(views) + 
  geom_histogram(binwidth = 500000,fill="aquamarine4")+
  xlim(0,20000000)

ggplot(data = talks) + 
  aes(duration) + 
  geom_histogram(binwidth = 50,fill="aquamarine4")


ggplot(data = talks)+
  aes(views,comments)+
  geom_point(aes(color=duration))+
  xlim(0,40000000)

ggplot(data = talks)+
  aes(duration,comments)+
  geom_point(aes(color=views))+
  scale_fill_brewer(direction = -1)

ggplot(data = talks)+
  aes(num_lang,comments)+
  geom_point(aes(color=duration))

#text mining
topics <- data.frame(talks_id = ted_talks_en$talk_id, text = ted_talks_en$topics)
topics %<>% unnest_tokens(word, text) %>% anti_join(stop_words)%>%count(word,sort = T)
ggplot(topics[1:10,],aes(word,n))+
  geom_col()+
  coord_flip()
