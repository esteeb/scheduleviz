library(readxl)
library(tidyverse)
library(ggplot2)
library(stringr)
library(lubridate)
library(reshape2)

df <- read.csv("test.csv", na.strings = 0)
df[is.na(df)] <- 0

#p <- ggplot(df, aes(monthweek, weekdayf, fill = amznStock$adjusted)) + 
#    geom_tile(colour = "white") + facet_grid(year(amznStock$date)~monthf) + 
#  scale_fill_gradient(low="red", high="green") +  
#  xlab("Day of Week") + ylab("Time of Day") + 
#  ggtitle("Time-Series Calendar Heatmap: AMZN Stock Prices")

mon <- df%>%
  filter(day1=="M")%>%
  select(-class,-start,-end,-reg,-day1,-x)%>%
  colSums(na.rm=FALSE)
mon <- data.frame(lapply(mon, type.convert), stringsAsFactors=FALSE)
mon <- mon%>%
  mutate(day = "M")%>%
  melt(id.vars = "day")

tues <- df%>%
  filter(day1=="T")%>%
  select(-class,-start,-end,-reg,-day1,-x)%>%
  colSums(na.rm=FALSE)
tues <- data.frame(lapply(tues, type.convert), stringsAsFactors=FALSE)
tues <- tues%>%
  mutate(day = "T")%>%
  melt(id.vars = "day")

weds <- df%>%
  filter(day1=="W")%>%
  select(-class,-start,-end,-reg,-day1,-x)%>%
  colSums(na.rm=FALSE)
weds <- data.frame(lapply(weds, type.convert), stringsAsFactors=FALSE)
weds <- weds%>%
  mutate(day = "W")%>%
  melt(id.vars = "day")

thur <- df%>%
  filter(day1=="Th")%>%
  select(-class,-start,-end,-reg,-day1,-x)%>%
  colSums(na.rm=FALSE)
thur <- data.frame(lapply(thur, type.convert), stringsAsFactors=FALSE)
thur <- thur%>%
  mutate(day = "Th")%>%
  melt(id.vars = "day")

df1 <- rbind(mon, tues, weds, thur)

colnames(df1) <- c("day","time","in.class")
df1$time <- as.factor(gsub("X","",df1$time))
df1$day <- as.factor(df1$day)

p <- ggplot(df1, aes(x =1, y=time, fill = in.class)) + 
  geom_tile(colour = "white") + facet_grid(cols = vars(day)) + 
  #scale_y_continuous(trans = "reverse", breaks = unique(df1$time)) + 
  scale_fill_gradient(low="green", high="red") +  
  xlab("Day of Week") + ylab("Time of Day") + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())+
  ggtitle("Time-Series Calendar Heatmap: Tepperonis in Electives, Mini 1")


