# load packages -----------------------------------------------------------
#install.packages("tidyverse")
#install.packages("haven")
#install.packages("psych")
#install.packages("lavaan")
#install.packages("userfriendlyscience")
#install.packages("devtools")
#install.packages("writexl")
#install.packages("Hmisc")
#install.packages("rstatix")
#install.packages("emmeans")

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
library(effectsize)
library(car)
library(rstatix)
library(emmeans)

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

#check out data
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

#check out data
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


# anova -------------------------------------------------------------------
#generate data
set.seed(42)

# Group sizes
group_size <- 100

# Generate scores for the Lecture group
pre_scores_lecture <- rnorm(group_size, mean = 70, sd = 5) # Higher pre-scores for Lecture group
post_scores_lecture <- pre_scores_lecture + rnorm(group_size, mean = 3, sd = 3) # Improvement for Lecture group

# Generate scores for the Case Study group
pre_scores_case_study <- rnorm(group_size, mean = 80, sd = 5) # Moderate pre-scores for Case Study group
post_scores_case_study <- pre_scores_case_study + rnorm(group_size, mean = 5, sd = 3) # Improvement for Case Study group

# Generate scores for the Control group
pre_scores_control <- rnorm(group_size, mean = 60, sd = 5) # Lower pre-scores for Control group
post_scores_control <- pre_scores_control + rnorm(group_size, mean = 2, sd = 3) # Minimal improvement for Control group

# Combine data into a dataframe
data <- data.frame(
  Group = rep(c("Lecture", "Case Study", "Control"), each = group_size),
  Pre_Score = c(pre_scores_lecture, pre_scores_case_study, pre_scores_control),
  Post_Score = c(post_scores_lecture, post_scores_case_study, post_scores_control)
)

# display the first few rows of the dataset
head(data)

data

#descriptive stats
data %>% 
  dplyr::group_by(Group) %>% 
  dplyr::summarize(postm = mean(Post_Score), 
            postsd = sd(Post_Score))

#homogeneity of variances
leveneTest(Post_Score ~ Group, data = data) # Levene's Test

#normality of residuals
plot(fit) # diagnostic plots
shapiro.test(residuals(fit)) # test for normality

#ANOVA & postdoc test
fit <- oneway(data$Group, y = data$Post_Score, posthoc = 'games-howell')
fit

#eta-squared and omega-squared
#small = 0.01
#medium = 0.06
#large = 0.14

# ancova ------------------------------------------------------------------
#descriptive stats
data %>% 
  dplyr::group_by(Group) %>% 
  dplyr::summarize(prem = mean(Pre_Score), 
                   presd = sd(Pre_Score),
                   postm = mean(Post_Score),
                   postsd = sd(Post_Score))

#homogeneity of regression slopes
interaction_model <- aov(Post_Score ~ Group * Pre_Score, data = data)
summary(interaction_model)

#normality of residuals: use a Shapiro-Wilk test or diagnostic plots
shapiro.test(residuals(fit))
plot(fit)

#homogeneity of variances: use Levene's test.
leveneTest(Post_Score ~ Group, data = data)

#ancova
fit <- aov(Post_Score ~ Group + Pre_Score, data = data)
summary(fit)

# post-hoc pairwise comparisons
pairwise <- emmeans(fit, pairwise ~ Group)
summary(pairwise)

#effectsize
# calculate partial eta-squared
eta_squared(fit)

#partial eta-squared and omega-squared
#small = 0.01
#medium = 0.06
#large = 0.14

# EFA ---------------------------------------------------------------------
#共同因子模型
library(tidyverse)
library(psych)
library(haven)

#import data
data<-read_sav("data.sav")
names(data)

#subset
data<-data %>% select(V101:V120)

#correlation
data %>% as.matrix() %>% Hmisc::rcorr()

#Evaluate the correlation matrix

#KMO: 
#“How tight are the clusters?”
#Are the items related closely enough to form reliable groups (factors)?
#Do the items cluster well enough to find common factors?

#0.00 to 0.49 unacceptable
#0.50 to 0.59 miserable
#0.60 to 0.69 mediocre
#0.70 to 0.79 middling
#0.80 to 0.89 meritorious
#0.90 to 1.00 marvelous
KMO(data)

#Bartlett’s test for sphericity
#“Are the pieces even related?”
#Is there enough evidence that your variables are not random?
#	Are the item correlations significant?

cortest.bartlett(data)

#Determine number of factors to extract
ev <- eigen(cor(data)) # get eigenvalues
ev$values
scree(data, pc=FALSE)
fa.parallel(data, fa="fa")

Nfacs <- 4  # This is for four factors. You can change this as needed.
fit <- factanal(data, Nfacs, rotation="promax")
print(fit, digits=2, sort=FALSE)
print(fit, digits=2, cutoff=0.4, sort=FALSE)

#plot factor 1 by factor 4
load <- fit$loadings[,c(1, 4)]
plot(load,type="n") # set up plot
text(load,labels=names(data),cex=.7)

#visualization
loads <- fit$loadings
fa.diagram(loads, digits = 2, sort=TRUE)
