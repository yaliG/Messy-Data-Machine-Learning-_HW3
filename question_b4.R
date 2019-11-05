#Quesiton B4
library(tidyverse)

#import the data
sqf.data <- read_csv("data/sqf_08_16.csv")

sqf.data <- sqf.data %>% 
  mutate(arrest.summary = arrested == TRUE)

train <- sqf.data %>% 
  filter(year == 2010)
test <- sqf.data %>% 
  filter(year == 2011)

model <- glm(arrest.summary ~ as.factor(month) + as.factor(time.period) +
               as.factor(suspect.race) + as.factor(suspect.build) + as.factor(suspect.sex)
             + as.factor(precinct) + as.factor(location.housing + additional.report + additional.investigation
             + additional.proximity + additional.evasive + additional.associating +
               additional.direction + additional.highcrime + additional.time + additional.sights +
               additional.other + stopped.bc.object + stopped.bc.desc + stopped.bc.casing + stopped.bc.lookout +
               stopped.bc.clothing + stopped.bc.drugs + stopped.bc.furtive + stopped.bc.violent + stopped.bc.bulge + 
               stopped.bc.other + suspect.age, data = train, family = "binomial"))
