# load packages -----------------------------------------------------------
#install.packages("tidyverse")
#install.packages("haven")
#install.packages("psych")
#install.packages("lavaan")
#install.packages("userfriendlyscience")
#install.packages("devtools")
#install.packages("writexl")
#install.packages("Hmisc")

library(tidyverse)
library(psych)
library(haven)
library(lavaan)
library(userfriendlyscience)
library(devtools)
install_github("Matherion/userfriendlyscience")
library(writexl)
library(Hmisc)
library(labelled)
library(seminr)
library(readr)

# import data -------------------------------------------------------------
data<-read_sav("workplace_survey_data.sav")
data <- read_csv("workplace_survey_data.csv")
data("corp_rep_data", package = "seminr")


#take a look at your data
data

#check variable names
names(data)


# export data -------------------------------------------------------------
write.csv(data, "workplace_survey_data.csv", row.names = FALSE)

# remove labels -----------------------------------------------------------
data<-remove_labels(data)
str(data)
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

# bivariate correlations --------------------------------------------------
#Pearson zero-order correlation
data %>% select(dj:jb) %>% as.matrix() %>% Hmisc::rcorr()
cor<-data %>% select(dj:jb) %>% as.matrix() %>% Hmisc::rcorr()
cor$r<-as.data.frame(cor$r)
cor$P<-as.data.frame(cor$P)
write_xlsx(cor$r, "r.xlsx")
write_xlsx(cor$P, "p.xlsx")

# structural model: 1 x, 1 m, 1 y ------------------------------------------
#run process macro
process(data=data, y="jb", x="pj", m=c("am"), model = 4, seed=12345)

# structural model: 1 x, 2 m, 1 y ------------------------------------------
#run process macro
process(data=data, y="jb", x="pj", m=c("am", "cm"), bmatrix=c(1,1,0,0,1,1), seed=12345)

# structural model: 2 x, 2 m, 1 y ------------------------------------------
#run process macro
process(data=data, y="jb", x="dj", cov=c("pj"), m=c("am", "cm"), bmatrix=c(1,1,0,0,1,1), cmatrix=c(1,1,0), seed=12345)
process(data=data, y="jb", x="pj", cov=c("dj"), m=c("am", "cm"), bmatrix=c(1,1,0,0,1,1), cmatrix=c(1,1,0), seed=12345)

# structural model: 2 x, 2 m, 2 y ------------------------------------------
#run process macro
process(data=data, y="jb", x="dj", cov=c("pj"), m=c("am", "cm"), bmatrix=c(1,1,0,0,1,1), cmatrix=c(1,1,0), seed=12345)
process(data=data, y="jb", x="pj", cov=c("dj"), m=c("am", "cm"), bmatrix=c(1,1,0,0,1,1), cmatrix=c(1,1,0), seed=12345)

process(data=data, y="js", x="dj", cov=c("pj"), m=c("am", "cm"), bmatrix=c(1,1,0,0,1,1), cmatrix=c(1,1,0), seed=12345)
process(data=data, y="js", x="pj", cov=c("dj"), m=c("am", "cm"), bmatrix=c(1,1,0,0,1,1), cmatrix=c(1,1,0), seed=12345)

# paired samples t-test ---------------------------------------------------
#load packages
library(tidyverse)
library(effectsize)

#simulation
set.seed(123)
data <- tibble(
  subject_id = 1:100,
  y1 = rnorm(100, mean = 50, sd = 10),
  y2 = rnorm(100, mean = 55, sd = 10)
)

#check our data
data

#descriptive statistics
data %>% psych::describe()

#model
t_test_result <- t.test(data$y1, data$y2, paired = TRUE)
t_test_result

#effect size
#d = 0.2 small
#d = 0.5 medium
#d = 0.8 large
cohens_d(data$y1, data$y2, paired = TRUE)

#percentile
0.22*37 = 8.14

# independent samples t-test ----------------------------------------------
#load packages
library(tidyverse)
library(effectsize)

#simulation
set.seed(123)
data <- tibble(
  x = rep(c(1, 0), each = 100),  # 1 for training, 0 for control
  y = c(rnorm(100, mean = 7, sd = 1.5), rnorm(100, mean = 6, sd =1.5))  # y values
)

#check our data
data

#descriptive statistics
data %>% 
  group_by(x) %>% 
  summarize(m = mean(y),
            sd = sd(y))

#model
t_test_result<-t.test(data$y ~ data$x, paired = FALSE)
t_test_result

#effect size
#effect size
#d = 0.2 small
#d = 0.5 medium
#d = 0.8 large
cohens_d(data$y ~ data$x, paired = FALSE)

#percentile
0.92*37 = 34.04

# moderation ----------------------------------------------
#load packages
library(tidyverse)
library(haven)
library(psych)
library(labelled)
library(userfriendlyscience)
library(interactions)

#modeling
process (data=data,y="ang",x="SEN",w="BDY_r",model=1,plot=1,jn=1)

#visualize interaction
fit <- lm(ang ~ SEN * BDY_r, data = data)
sim_slopes(fit, pred = SEN, modx = BDY_r, jnplot = TRUE)