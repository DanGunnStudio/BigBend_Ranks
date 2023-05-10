# This is code to read-in and clean info about Big Bend National Park
#Code developed by Dan Gunn, May 2023

library(tidyverse)
library(readr)
library(ggplot2)


#reading in total parks data
NPS_AnnualSummary_AllParks <- read_csv("data/NPS_AnnualSummary_AllParks.csv", 
                                       skip = 2)

#contains 'NPRES' national preserves as well. Some like Katmai have both NP and PRES
#I need to use filter twice or use regex
NPS_NPs_only <- NPS_AnnualSummary_AllParks %>% 
  filter(str_detect(ParkName, "NP")) %>% 
  filter(!str_detect(ParkName, "NPRES"))
unique(NPS_NPs_only$ParkName)

#inspecting
NPS_NPs_only %>% group_by(ParkName) %>% summarize() %>% print(n=100)

#creating average attendance figures
natl_parks_avg <- NPS_NPs_only %>% 
  filter(ParkName != "Big Bend NP") %>% 
  group_by(Year, ParkName)%>%
  summarize(avg_visitors = mean(RecreationVisitors))

#creating average attendance figures for BIBE
bibe_avg <- NPS_NPs_only %>% 
  filter(ParkName == "Big Bend NP")  %>% 
  group_by(Year)%>%
  summarize(avg_visitors = mean(RecreationVisitors))

ggplot()+
  geom_line(data=natl_parks_avg, aes(x=Year, y=avg_visitors))+
  geom_smooth(data=natl_parks_avg, aes(x=Year, y=avg_visitors),method="loess")+
  geom_line(data=bibe_avg, aes(x=Year, y=avg_visitors), color ='#009E73') +
  geom_smooth(data=bibe_avg, aes(x=Year, y=avg_visitors),color ='#009E73', method="loess")+
  geom_vline(xintercept=c(1941,1945, 1968,2008, 2020), linetype="dashed", 
             color = "red", size=0.5)+
  scale_y_continuous(labels= scales::comma, n.breaks=8)

#from EDA BIBE attendance grew more slowly than average in the postwar years and then about at the same rate in the post 1965, then went up again in the last decade
#points to 
NPS_NPs_only %>% 
  filter(ParkName != "Big Bend NP") %>% 
  group_by(Year)%>%
  summarize(avg_visitors = mean(RecreationVisitors))%>%
  ggplot(aes(Year, avg_visitors))+
  geom_line() +
  geom_vline(xintercept=c(1941,1945, 1968,2008, 2020), linetype="dashed", 
             color = "red", size=0.5)+
  scale_y_continuous(labels= scales::comma, n.breaks=8)

NPS_NPs_only %>% 
  filter(ParkName == "Big Bend NP") %>% 
  ggplot(aes(Year, RecreationVisitors))+
  geom_line() +
  geom_vline(xintercept=c(1941,1945, 1965,2008, 2020), linetype="dashed", 
             color = "red", size=0.5)+
  scale_y_continuous(labels= scales::comma, n.breaks=8)

#creating ranks for the national parks per year by attendance. 
NPs_ranked <- NPS_NPs_only %>% group_by(Year) %>% 
  mutate( rank = rank(desc(RecreationVisitors))) %>%
  summarize(ParkName, rank, RecreationVisitors)%>%
  arrange(rank)

NPs_ranked %>% filter(ParkName == "Big Bend NP") %>%
  ggplot(aes(Year, rank))+
  geom_line(size=1)+
  scale_y_reverse()

NPs_ranked %>% filter(rank <= 10)%>%
  ggplot(aes(Year, rank, color=ParkName))+
  geom_line()+
  scale_y_reverse()

NPs_ranked %>%
  ggplot(aes(Year, rank, color=ParkName))+
  geom_line(size=1)+
  scale_y_reverse()
