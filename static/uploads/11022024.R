# load packages -----------------------------------------------------------
#install.packages("tidyverse")
#install.packages("haven")
#install.packages("psych")
#install.packages("lavaan")
#install.packages("userfriendlyscience")
#install.packages("devtools")
#install.packages("writexl")

library(tidyverse)
library(psych)
library(haven)
library(lavaan)
library(userfriendlyscience)
library(devtools)
install_github("Matherion/userfriendlyscience")
library(writexl)

# import data -------------------------------------------------------------
data<-read_sav("workplace_survey_data.sav")

#take a look at your data
data

#check variable names
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

# descriptive statistics --------------------------------------------------
data %>% select(dj, pj, am, cm, js, jb) %>% describe()
des<-data %>% select(dj, pj, am, cm, js, jb) %>% describe()
write_xlsx(des, "des.xlsx")

# reliability -------------------------------------------------------------
#don't use Cronbach's alpha
plot.new()
data %>% select(DJ1:DJ5) %>% scaleStructure()
data %>% select(PJ1:PJ5) %>% scaleStructure()
data %>% select(AM1:AM5) %>% scaleStructure()
data %>% select(CM1:CM5) %>% scaleStructure()
data %>% select(JS1:JS5) %>% scaleStructure()
data %>% select(JB1:JB5) %>% scaleStructure()

