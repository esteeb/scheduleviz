library(readxl)
library(tidyverse)
library(ggplot2)
library(stringr)
library(lubridate)
library(reshape2)



#
#p <- ggplot(amznStock, aes(monthweek, weekdayf, fill = amznStock$adjusted)) + 
#  geom_tile(colour = "white") + facet_grid(year(amznStock$date)~monthf) + scale_fill_gradient(low="red", high="green") +  xlab("Week of Month") + ylab("") + ggtitle("Time-Series Calendar Heatmap: AMZN Stock Prices") + labs(fill = "Price")

mini1 <- read_xlsx("Mini1.xlsx", col_names = FALSE)
mini2 <- read_xlsx("Mini2.xlsx", col_names = FALSE)

colnames(mini1) <- c("class", "sec", "mod","day","time","con","reg","waitlist")
colnames(mini2) <- c("class", "sec", "mod","day","time","con","reg","waitlist")

test <- mini1%>%
  separate(time, c("start", "end"), sep = "-", remove = TRUE)%>%
  mutate(days = gsub("(?!^)(?=[[:upper:]])", " ", mini1$day, perl=T))%>%
  separate(days, c("first", "second"), sep = " ", remove = TRUE)%>%
  separate(reg, c("reg", "max"), sep = "/", remove=TRUE)%>%
  pivot_longer(test, cols = c(first, second), values_to = 'day1', names_to = 'name1') %>%
  select(-name1)%>%
  filter(!is.na(day1))%>%
  mutate(reg = as.numeric(reg))%>%
  select(class,start,end,con,reg,day1)

write.csv(test, "test.csv")
vect <- format( seq.POSIXt(as.POSIXct(Sys.Date()), as.POSIXct(Sys.Date()+1), by = "10 min"),
                "%H%M", tz="GMT")
write.csv(vect, "vect.csv")
