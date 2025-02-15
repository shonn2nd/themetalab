# load packages ------------------------------------------------------------
library(tidyverse)
library(readr)
library(psych)
library(class)
library(gmodels)

# import -------------------------------------------------------------
wbcd <- read.csv("Chapter 03/wisc_bc_data.csv")

str(wbcd)

# trans:norm ----------------------------------------------------------
wbcd<-wbcd %>% select(-id)

names(wbcd)

table(wbcd$diagnosis)
prop.table(table(wbcd$diagnosis))

wbcd<-wbcd %>% mutate(diagnosis = factor(diagnosis, levels = c("B", "M"), labels = c("Benign", "Malignant")))

#create a normalize function
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

wbcd_n <- wbcd %>%
  mutate(across(2:31, normalize)) %>%
  as.data.frame()

describe(wbcd_n)

#training vs test

wbcd_train <- wbcd_n %>% slice(1:469) %>% select(-diagnosis)
wbcd_test  <- wbcd_n %>% slice(470:569) %>% select(-diagnosis)

#pull the diagnosis column

wbcd_train_labels <- wbcd %>% slice(1:469) %>% pull(1)
wbcd_test_labels  <- wbcd %>% slice(470:569) %>% pull(1)

# model:norm ----------------------------------------------------------------
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k = 21)

# eval:norm --------------------------------------------------------------------
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq = FALSE)

# trans:z ----------------------------------------------------------
wbcd<-wbcd %>% select(-id)

names(wbcd)

table(wbcd$diagnosis)
prop.table(table(wbcd$diagnosis))

wbcd<-wbcd %>% mutate(diagnosis = factor(diagnosis, levels = c("B", "M"), labels = c("Benign", "Malignant")))

wbcd_z <- wbcd %>%
  mutate(across(2:31, scale)) %>%
  as.data.frame()

describe(wbcd_z)

#training vs test

wbcd_train <- wbcd_z %>% slice(1:469) %>% select(-diagnosis)
wbcd_test  <- wbcd_z %>% slice(470:569) %>% select(-diagnosis)

#pull the diagnosis column

wbcd_train_labels <- wbcd %>% slice(1:469) %>% pull(1)
wbcd_test_labels  <- wbcd %>% slice(470:569) %>% pull(1)

# model:norm ----------------------------------------------------------------
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k = 21)

# eval:norm --------------------------------------------------------------------
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq = FALSE)

# model:multiple k values ----------------------------------------------------------------
#define k values
k_values <- c(1, 5, 11, 15, 21, 27)

#run KNN for each k and generate CrossTable
results <- map(k_values, ~{
  wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,
                        cl = wbcd_train_labels, k = .x)
  
  CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq = FALSE)
  
  return(wbcd_test_pred)  # store predictions if needed
})