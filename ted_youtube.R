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
library(gvlma)
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

##category
talks %>%
  count(categories, sort = TRUE) %>%
  mutate(event = reorder(categories, n)) %>%
  ggplot(aes(n, categories)) +
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
  aes(log(view_ted),log(comments))+
  geom_point(aes(color = factor(english)),alpha = 0.3)+
  labs(title = "views vs comments")+
  geom_smooth(aes(color = factor(english)),method = "lm")
ggplot(data = talks)+
  aes(log(view_ted),log(comments))+
  geom_point(aes(color = factor(categories)),alpha = 0.3)+
  labs(title = "views vs comments")+
  geom_smooth(aes(color = factor(categories)),method = "lm",se=F)

ggplot(data = talks)+
  aes(log(duration_ted),log(comments))+
  geom_point(alpha = 0.3,aes(color = factor(english)))+
  scale_fill_brewer(direction = -1)+
  labs(title = "comments vs duration")+
  geom_smooth(aes(color = factor(english)))
ggplot(data = talks)+
  aes(log(duration_ted),log(comments))+
  geom_point(alpha = 0.3,aes(color = factor(categories)))+
  scale_fill_brewer(direction = -1)+
  labs(title = "comments vs duration")+
  geom_smooth(aes(color = factor(categories)),se=F,method = "lm")

ggplot(data = talks[-931,])+
  aes(log(num_lang),log(comments))+
  geom_point(alpha = 0.3,aes(color = factor(english)))+
  labs(title = "comments vs number of available languages")+
  geom_smooth(aes(color = factor(english)),method="lm",se=F)
ggplot(data = talks[-931,])+
  aes(log(num_lang),log(comments))+
  geom_point(alpha = 0.3,aes(color = factor(categories)))+
  labs(title = "comments vs number of available languages")+
  geom_smooth(aes(color = factor(categories)),se=F,method = "lm")



ggplot(data = talks)+
  aes(dislike_count,average_rating)+
  geom_point(alpha = 0.3,color = "brown4")+
  labs(title = "average rating vs number of dislike")+
  xlim(0,2500)+
  geom_smooth()

ggplot(data = talks)+
  aes(like_count,average_rating)+
  geom_point(alpha = 0.3,color = "brown4")+
  labs(title = "average rating vs number of like")+
  xlim(0,15000)+
  geom_smooth()
  
ggplot(data = talks)+geom_boxplot(aes(like_count))

ggplot(data = talks)+
  aes(duration_yt,average_rating)+
  geom_point(alpha = 0.3,color = "brown4")+
  labs(title = "average rating vs duration")+
  xlim(0,2000)+
  geom_smooth()

ggplot(data = talks)+
  aes(log(view_yt),average_rating)+
  geom_point(alpha = 0.3,color = "brown4")+
  labs(title = "average rating vs number of views")+
  geom_smooth()

ggplot(data = talks)+
  aes(view_yt,average_rating)+
  geom_point(alpha = 0.3,color = "brown4")+
  labs(title = "average rating vs number of views")+
  geom_smooth()

ggplot(data = talks)+
  aes(dislike_count,like_count)+
  geom_point(alpha = 0.3,color = "brown4")

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
talks$s_numlang <- (talks$num_lang-mean(talks$num_lang))/sd(talks$num_lang)
talks$s_duration <- (talks$duration_ted-mean(talks$duration_ted))/sd(talks$duration_ted)
talks %<>% mutate(log_duration = log(duration_ted))
##model for ted
pairs(~log_comments+log_views+duration_min+num_lang,data=talks,lower.panel=panel.smooth,
      upper.panel=panel.cor)

fit1 <- lm(comments~s_duration+s_views+s_numlang,talks)
summary(fit1)

ggplot()+
  geom_point(aes(fit1$fitted.values,fit1$residuals))+
  geom_smooth(aes(fit1$fitted.values,fit1$residuals))

par(mfrow = c(2,2))
plot(fit1)

fit2 <- lm(log_comments~log_views+s_numlang,talks)
summary(fit2)
par(mfrow = c(2,2))
plot(fit2)

fit3 <- lm(log_comments~log(view_ted)+log(num_lang)+log_duration+english+english:log(view_ted)+english:num_lang+categories,talks[-931,])
summary(fit3)
par(mfrow = c(2,2))
plot(fit3)


crPlots(fit3)
qqPlot(fit3)
gvlma(fit3)
durbinWatsonTest(fit3)
ncvTest(fit3)
outlierTest(fit3)

library("lme4")
library(arm)

fit4 <- lmer(log_comments~log_duration+log_views+log(num_lang)+(1+log_duration|categories)+english:log(num_lang)+english,talks[-931,])
summary(fit4)
plot(fit4,which=2)
qqPlot(fit4)

confint(fit4)
ranef(fit4)
coef(fit4)

binnedplot(fitted(fit4),resid(fit4))
ggplot()+
  geom_point(aes(fitted(fit4),fit))+
  geom_smooth(aes(fitted(fit4),resid(fit4)))

ggplot()+geom_density(aes(resid(fit4)),size=2.5)


fit5 <- lmer(log_comments~s_duration+s_views+s_numlang+(1|categories),talks[-931,])
display(fit5)
confint(fit5)
plot(fit5)
binnedplot(fitted(fit5),resid(fit5))
ggplot()+
  geom_text(aes(fitted(fit5),resid(fit5),label = talks[-931,]$categories))+
  geom_smooth(aes(fitted(fit5),resid(fit5)))

ggplot()+geom_density(aes(resid(fit5)))
ranef(fit5)
coef(fit5)
coefplot(fit3)
ggplot() + 
  geom_text(aes(x=fitted(fit4),y=resid(fit4),color=talks[-931,]$categories,label = talks[-931,]$categories)) + 
  theme_bw() + 
  theme(legend.position="none")

## model for youtube


y1 <- lm(average_rating~log(dislike_count)+log(like_count)+log(view_yt)+duration_yt+num_lang,talks[-c(1753,1875),])
summary(y1)
plot(y1)

crPlots(y1)
spreadLevelPlot(y1)
qqPlot(y1)

#process
talks %<>% mutate(sd_duration = (duration_yt-mean(duration_yt))/sd(duration_yt))
talks %<>% mutate(sd_views = (view_yt-mean(view_yt))/sd(view_yt))
talks %<>% mutate(sd_rating = (average_rating-mean(average_rating))/sd(average_rating))
talks %<>% mutate(sd_dislike = (dislike_count-mean(dislike_count))/sd(dislike_count))
talks %<>% mutate(sd_like = (like_count-mean(like_count))/sd(like_count))
talks$rating2 <- (talks$average_rating)^2
yt <- filter(talks,average_rating>4.5&like_count<5000)
yt %<>% mutate(sd_duration = (duration_yt-mean(duration_yt))/sd(duration_yt))
yt %<>% mutate(sd_views = (view_yt-mean(view_yt))/sd(view_yt))
yt %<>% mutate(sd_rating = (average_rating-mean(average_rating))/sd(average_rating))
yt %<>% mutate(sd_dislike = (dislike_count-mean(dislike_count))/sd(dislike_count))
yt %<>% mutate(sd_like = (like_count-mean(like_count))/sd(like_count))
yt$rating2 <- (talks$average_rating)^2

ysd <- lm(average_rating~sd_dislike+sd_like+sd_views,yt[-c(1175,715),])
summary(ysd)
par(mfrow = c(2,2))
crPlots(ysd)
plot(ysd)

y2 <- lm(average_rating~log(dislike_count)+log(like_count)+log(view_yt)+sd_duration+s_numlang,yt[-c(1175,715),])##
summary(y2)
plot(y2)

y3 <- lm(average_rating~log(dislike_count)+like_count+log(view_yt)+sd_duration+num_lang+dislike_count:like_count+I(like_count^2)+I(dislike_count^2),yt)##best
summary(y3)
plot(y3)

y4 <- lm(average_rating~dislike_count+log(like_count)+sqrt(view_yt)+sd_duration+s_numlang+dislike_count:like_count,yt)
summary(y4)
plot(y4)

crPlots(y4)
gvlma(y4)

ytest <- lm(average_rating~log(dislike_count)+log(like_count),yt)
crPlots(ytest)
summary(ytest)
ggplot()+
  geom_point(aes(ytest$fitted.values,ytest$residuals))+
  geom_smooth(aes(ytest$fitted.values,ytest$residuals))
plot(ytest)
qqPlot(ytest)

durbinWatsonTest(y3)

crPlots(y2)


crPlots(y5)
spreadLevelPlot(y2)
spreadLevelPlot(y4)
qqPlot(y3,labels=row.names(talks),id.method="identify",simulate=TRUE,main="Q-Q Plot")
qqPlot(fit3,labels=row.names(states),id.method="identify",simulate=TRUE,main="Q-Q Plot")
vif(y4)
step(y4)
boxTidwell(fit3)
ncvTest(y3)
durbinWatsonTest(y3)
outlierTest(y3)
# outlier
plot(x=fitted(y3),y=rstudent(y3))
abline(h=3,col="red",lty=2)
abline(h=-3,col="red",lty=2)
#leverage
hat.plot <- function(fit){
  p <- length(coefficients(fit))
  n <- length(fitted(fit))
  plot(hatvalues(fit),main = "Index Plot of Hat Values")
  abline(h=c(2,3)*p/n,col="red",lty=2)
  identify(1:n, hatvalues(fit), names(hatvalues(fit)))  
}
hat.plot(y3)

test <- talks[,c(12,18,20,21:23)]
test$log_dislike <- log(test$dislike_count)
test$log_like <- log(test$like_count)
test$log_rating <- log(test$average_rating)
scatterplotMatrix(talks[,c(12,18,20:23)], spread = F, smoother=loessLine, main = "Scatter Plot Matrix",groups = "categories")
corrplot(cor(test[,6:9]),order="AOE",addCoef.col = 'grey')#?Ÿ???
pairs(~like_count+dislike_count+view_yt+duration_yt+average_rating+num_lang,data=talks,lower.panel=panel.smooth,
          upper.panel=panel.cor)

panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
