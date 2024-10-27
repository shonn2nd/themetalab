# load packages -----------------------------------------------------------
#install.packages("tidyverse")
#install.packages("haven")
#install.packages("psych")

library(tidyverse)
library(haven)
library(psych)

# import data -------------------------------------------------------------
data<-read_sav("workplace_survey_data.sav")

data

names(data)

# create composite scores -------------------------------------------------
#distributive justice
data<-data %>% mutate (dj = rowMeans(select(., c(DJ1:DJ5)), na.rm=T))

#procedural justice
data<-data %>% mutate (pj = rowMeans(select(., c(PJ1:PJ5)), na.rm=T))

#autonomous motivation
data<-data %>% mutate (am = rowMeans(select(., c(AM1:AM5)), na.rm=T))

#controlled motivation
data<-data %>% mutate (cm = rowMeans(select(., c(CM1:CM5)), na.rm=T))

#job satisfaction
data<-data %>% mutate (js = rowMeans(select(., c(JS1:JS5)), na.rm=T))

#job burnout
data<-data %>% mutate (jb = rowMeans(select(., c(JB1:JB5)), na.rm=T))