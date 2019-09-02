library(tidyverse)
library(lubridate)
#
sdo <- read_csv("./ns_sdoh.csv")
sdo_monthly <- read_csv("./ns_sdoh_mm.csv")
sdo_monthly$Year <- year(sdo_monthly$Ymd)
sdo_monthly$Month <- month(sdo_monthly$Ymd)
sdo_monthly$Day <- day(sdo_monthly$Ymd)
sdo_monthly$Ymd <- as.Date(sdo_monthly$Ymd)
tbl_df(sdo_monthly)
ggplot(data=sdo_monthly,aes(x=Ymd,y=Wn,col="North")) + geom_line() +
  geom_line(data=sdo_monthly,aes(x=Ymd,y=Ws,col="South")) + ggtitle("SDO N/S Wolf: 2012 - 2019")
# Current Mimimn 2014 - 2019
sdo_current <- sdo_monthly %>% filter(Year >= 2014)
ggplot(data=sdo_current,aes(x=Ymd,y=Wn,col="North")) + geom_line() +
  geom_line(data=sdo_current,aes(x=Ymd,y=Ws,col="South")) + ggtitle("SDO Current N/S Wolf: 2014 - 2019")
#
# Use tidyverse spread function to create crosstab tables.
# North table for Current Solar Min.
#
print("Current(North) Solar Min. 2014-2019")
sdo_current %>% select(Year,Month,Wn) %>% spread(Month,Wn)
#
# South Table for Current Min.
print("Current(South) Solar Min. 2014-2019")
sdo_current %>% select(Year,Month,Ws) %>% spread(Month,Ws)
# A Couple of Histographs N/S:
ggplot(data=sdo_current,aes(x=Wn)) +geom_histogram() +
  ggtitle("SDO (North) Histogram: 2014-2019")
#
ggplot(data=sdo_current,aes(x=Ws)) +geom_histogram() +
  ggtitle("SDO (South) Histogram: 2014-2019")
