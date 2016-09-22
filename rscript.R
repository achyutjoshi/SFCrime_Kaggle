setwd("~/Desktop/Kaggle/San Francisco Criminal")


library(dplyr)
library(readr)
library(lubridate)

coltypes <-
  list(Dates = col_datetime("%Y-%m-%d %H:%M:%S"))
train = read_csv("train.csv", col_types = coltypes)
test = read_csv("test.csv", col_types = coltypes)

train <-
  train %>%
  mutate(Year  = factor(year(Dates), levels=2003:2015),
         Month = factor(month(Dates), levels=1:12),
         Day   = day(Dates),
         Hour  = factor(hour(Dates), levels=0:23),
         dayDate = as.POSIXct(round(Dates, units = "days")),
         DayOfWeek = factor(DayOfWeek, levels=c("Monday",
                                                "Tuesday",
                                                "Wednesday",
                                                "Thursday",
                                                "Friday",
                                                "Saturday",
                                                "Sunday"))
  )

test <-
  test %>%
  mutate(Year  = factor(year(Dates), levels=2003:2015),
         Month = factor(month(Dates), levels=1:12),
         Day   = day(Dates),
         Hour  = factor(hour(Dates), levels=0:23),
         dayDate = as.POSIXct(round(Dates, units = "days")),
         DayOfWeek = factor(DayOfWeek, levels=c("Monday",
                                                "Tuesday",
                                                "Wednesday",
                                                "Thursday",
                                                "Friday",
                                                "Saturday",
                                                "Sunday"))
  )

train$Weekday = wday(train$Dates)
test$Weekday = wday(test$Dates)

train$isWeekend = 0
test$isWeekend = 0

k = which(train$Weekday == 1)
train$isWeekend[k] = 1
k = which(train$Weekday == 7)
train$isWeekend[k] = 1

k = which(test$Weekday == 1)
test$isWeekend[k] = 1
k = which(test$Weekday == 7)
test$isWeekend[k] = 1

train$daytime = 0
train$daytime = recode(train$Hour, "c('1','2','3','4','5','6','7','8','9','10','11')='1';c('12','13','14','15','16','17','18','19')='2';c('20','21','22','23','24','0')='3'; else='0'")

test$daytime = 0
test$daytime = recode(test$Hour, "c('1','2','3','4','5','6','7','8','9','10','11')='1';c('12','13','14','15','16','17','18','19')='2';c('20','21','22','23','24','0')='3'; else='0'")

ggplot(train,aes(x = train$Category)) + geom_bar()
ggplot(data=train,aes(x=DayOfWeek,fill=PdDistrict)) + geom_bar() + ggtitle("Weekly distribution of crimes in various districts")
ggplot(data=train,aes(x=train$Year)) + geom_bar(fill="green") + ggtitle("Year-wise distribution of crimes in various districts")
ggplot(data=train,aes(x=train$Month)) + geom_bar(fill="brown") + ggtitle("Monthly distribution of crimes in various districts")
ggplot(data=train,aes(x=train$Hour)) + geom_bar(fill="blue") + ggtitle("Day-wise distribution of crimes in various districts")
ggplot(data=train,aes(x=train$isWeekend)) + geom_bar(fill="grey") + ggtitle("Hourly distribution of crimes in various districts")

ggplot(data=train, aes(x=Hour)) +
  geom_bar(colour="black", fill="skyblue") +
  ylab('Count') + 
  facet_wrap(~DayOfWeek)

data_plot = train %>%
  group_by(Category) %>%
  summarise(count = n()) %>%
  transform(Category = reorder(Category,-count))

train$Category = as.factor(train$Category)
submit = data.frame(matrix(nrow = dim(test)[1]))
submit$Id = test$Id
submit$matrix.nrow...dim.test..1..= NULL
submit[,unique(train$Category)] <- NA


#data$AddressTypeIsOf[grep('.?of.?', data$Address)] <- TRUE
#idx <- which(!is.na(data$Category))
train$AddressTypeIsOf = 0
test$AddressTypeIsOf = 0

train$AddressTypeIsOf[grep('.?of.?', train$Address)] <- TRUE
test$AddressTypeIsOf[grep('.?of.?', train$Address)] <- TRUE

