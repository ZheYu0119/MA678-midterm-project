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
talks <- talks[,-c(5,7,8,10)]


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
df2 %<>% separate(categories, c("de1","categories","de2"), sep = "'")
df2 <-  df2[,-c(19,21)]
write.csv(df2,"talks.csv",row.names = F)

