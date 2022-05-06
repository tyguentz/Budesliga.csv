library(tidyverse)
library(dslabs)
library(ggplot2)
library(caret)
Sccr <- read_csv("Bundesliga.csv")
Sccr <- as.data.frame(Sccr)
str(Sccr)
by <- Sccr %>% filter(HomeTeam == "Bayern Muenchen", AwayTeam == "Borussia Dortmund")
head(by)
by <- by %>% ggplot()
by + geom_point(aes(Date,HomeGoals,col = HomeTeam, size = 4)) + 
  geom_segment(aes(x=Date,xend=Date,y=HomeGoals,yend=0,size = 1)) +
  geom_label(aes(Date,HomeGoals,label = signif(HomeGoals,5)), colour = "darkred", nudge_x = 0, fill = 2) +
  labs(title = "Bayern Vs Dortmund Home Goals", x = "Date", y = "Home Goals") +
  theme(legend.position="none")

bcy <- Sccr %>% filter(HomeTeam == "Bayern Muenchen", AwayTeam == "Borussia Dortmund")
head(bcy)
bcy <- bcy %>% ggplot()
bcy + geom_point(aes(Year,AwayGoals,col = AwayTeam, size = 4)) + 
  geom_segment(aes(x=Year,xend=Year,y=AwayGoals,yend=0,size = 1))+
  geom_label(aes(Year,AwayGoals,label = signif(AwayGoals,3)), colour = "Blue", nudge_x = 0, fill = 4) +
  labs(title = "Dortmund Vs Bayern Away Goals", x = "Date", y = "Away Goals") +
  theme(legend.position="none")

Bay <- Sccr %>% filter(Sccr$HomeTeam == "Bayern Muenchen")
Tr <- Bay %>% select(HomeGoals)

Dort <- Sccr %>% filter(Sccr$AwayTeam == "Borussia Dortmund")
br <- Dort %>% select(AwayGoals)

t.test(Tr,br)


