library(tidyverse)
library(tidytuesdayR)
library(lubridate)
library(ggtext)

ultra_rankings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-26/ultra_rankings.csv')
race <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-26/race.csv')

glimpse(race)

#To find the top events and races by participants
race%>%
  mutate(Year=year(date))%>%
  distinct()%>%
  select(event,race,country,participants)%>%
  group_by(event,race,country)%>%
  summarise(Total=sum(participants))%>%
  arrange(desc(Total))


#To filter for the top events and races
race%>%
  mutate(Year=year(date))%>%
  filter(event=="UTMB®"& race=="UTMB®"|
           (event=="Sinister 7 Ultra" & race=="Solo")|
           (event=="Le Grand Raid De La Réunion" & race=="La Diagonale Des Fous")|
           (event=="Javelina Jundred" & race=="100 Miles")|
           (event=="Coldwater Rumble" & race=="100 Miles"))%>%
  select(Year,event,race,participants,country)%>%
  arrange(event)%>%
  select(Event=event,Race=race,Country=country,Year,Participants=participants)%>%
  data.frame()->box

#ordering data
box%>%
  mutate(Event=fct_relevel(Event,levels= "Coldwater Rumble",
                           "Javelina Jundred","Le Grand Raid De La Réunion",
                           "Sinister 7 Ultra","UTMB®"
                          ))->box
  
#renaming the events to include races and countries
box%>%
  mutate(Event=recode(Event,"Coldwater Rumble"="**Coldwater Rumble**<br>(100 Miles, United States)",
                      "Javelina Jundred"="**Javelina Jundred**<br>(100 Miles, United States)",
                      "Le Grand Raid De La Réunion"="**Le Grand Raid De La Réunion**<br>(La Diagonale Des Fous, France)",
                      "Sinister 7 Ultra"="**Sinister 7 Ultra**<br>(Solo, Canada)",
                      "UTMB®"="**UTMB®**<br>(UTMB®, France)"))->box

#boxplot
box%>%
ggplot(aes(x=Event, y=Participants,fill="#f23b4b")) +
 geom_boxplot(width=0.5,alpha=1,colour="white",fill="#f23b4b")+
      geom_jitter(size=2.5,alpha=0.6,colour="#757ede",width=0.12)+
  scale_y_continuous(limits=c(0,3000),breaks=c(500,1000,1500,2000,2500,3000))+
  coord_flip()+
  theme(axis.text.y=element_markdown(color="#f7b383", size=12, hjust=.5),
        axis.text.x=element_text(color="#f7b383", size=10),
        axis.title=element_blank(),
        axis.ticks = element_blank(),
        plot.margin = unit(c(.5, 1, .5, 1), "cm"),
        plot.background = element_rect(fill="black"),
        panel.background = element_rect(fill="black"),
        panel.grid = element_blank(),
        legend.position = "none",
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.title=element_text(size=16, colour = "#f7b383", face="bold"),
        plot.subtitle=element_markdown(size=12, colour="#f7b383", margin=margin(b=15)),
        plot.caption=element_text(hjust=0, size=9, colour="#f7b383", margin=margin(t=15)))+
  labs(title="TOP ULTRA-RUNNING EVENTS",
       subtitle="The top five ultra-running events (along with the races, countries they were held in) that saw the highest number of participants from 2012 to 2021",
       caption = "Data:BjnNowak-Github Repo via Tidy Tuesday | Design: @annapurani93")->boxplot

ggsave("boxplot.png",boxplot,width=12,height=6.1)
  
