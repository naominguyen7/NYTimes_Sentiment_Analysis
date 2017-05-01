setwd("C:/Users/Dancy Pants/NYtimes/")

library(RSentiment)
us <- readr::read_csv("us.csv")
us$score_snippet <- calculate_score(us$snippet)
library(stringr)


### US ###
us$date <- as.Date(as.character(us$date),format = "%Y%m%d")
us_daily <- us %>% group_by(date) %>% 
  summarise(snippet = mean(score_snippet[score_snippet!=99]),title = mean(score_title[score_title!=99]))
us_daily$date <- as.Date(as.character(us_daily$date),format = "%Y-%m-%d")
us$month <- format(us$date,"%b %Y")
us_monthly <- us %>% group_by(month) %>% 
  summarise(snippet = mean(score_snippet[score_snippet!=99]),title = mean(score_title[score_title!=99]))
us_monthly$month <- ordered(us_monthly$month,levels = c("Oct 2016","Nov 2016","Dec 2016","Jan 2017","Feb 2017","Mar 2017"))


plot(us_daily[,-2],type = "l",xlab = "Date",ylab = "Headline Sentiment",main = "US Section")+axis.Date(1, at = seq(as.Date("2016-10-01","%Y-%m-%d"),as.Date("2017-03-31","%Y-%m-%d"),"months"))
abline(v=as.Date("2016-11-08","%Y-%m-%d"),col = "red")


foreign <- factor(world$score_title[!grepl("[a-z]",world$title)],levels = c(-1,0,1),labels = c("Negative","Neutral","Positive"))
barplot(table(foreign))

### WORLD ###
world$date <- as.Date(as.character(world$date),format = "%Y%m%d")
world_daily <- world %>% group_by(date) %>% 
  summarise(snippet = mean(score_snippet[score_snippet!=99]),title = mean(score_title[score_title!=99]))
world_daily$date <- as.Date(as.character(world_daily$date),format = "%Y-%m-%d")
world$month <- format(world$date,"%b %Y")
world_monthly <- world %>% group_by(month) %>% 
  summarise(snippet = mean(score_snippet[score_snippet!=99]),title = mean(score_title[score_title!=99]))
world_monthly$month <- ordered(world_monthly$month,levels = c("Oct 2016","Nov 2016","Dec 2016","Jan 2017","Feb 2017","Mar 2017"))

par(mfcol = c(1,2))
barplot(prop.table(table(us$score_title[us$score_title!=99])),main = "US",ylim = c(0,0.6),xlab = "Sentiment score",ylab = "Percentage")
barplot(prop.table(table(world$score_title[world$score_title!=99])),main = "World",ylim = c(0,0.6),xlab = "Sentiment score",ylab = "Percentage")