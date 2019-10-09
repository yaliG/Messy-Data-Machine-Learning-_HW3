#Part A
library(tidyverse)
library(dplyr)
library(lubridate)
#Question A1: Setup
#import data
poll_data <- read_tsv("poll_data.tsv", col_names = TRUE) %>% 
  as.tibble()
View(poll_data)
#convert vote_2008 into a factor
poll_data <- poll_data %>% 
  mutate(vote_2008=as.factor(vote_2008))

#make "john mcCain" as the reference category
poll_data$vote_2008 <- relevel(poll_data$vote_2008, ref = "john mcCain")
table(poll_data$vote_2008)

#Question A2: Fitting a model
#fit a binary logistic regression model
poll_data <- poll_data %>% 
  mutate(state = as.factor(state),
         sex = as.factor(sex),
         race = as.factor(race),
         age = as.factor(age),
         education = as.factor(education),
         party = as.factor(party),
         ideology = as.factor(ideology),
         state_contestedness = as.factor(state_contestedness))
model <- glm(vote_2008 ~ 1 + state + sex + race +
               age + education + party +ideology + state_contestedness,
             data = poll_data, family = 'binomial')
model

#estimates individuals's probality
pred <- predict(model, type = 'response')
pred

#store the coefficient names and the estimates
summary(model)

coefficient_estimate <- coef(model)
coefficient_estimate

coefficient_name <- names(coefficient_estimate) 
coefficient_name

question_a <- tibble(coefficient_name, coefficient_estimate)
question_a

question_a2_coefficients <- question_a %>% 
  group_by(coefficient_name)
question_a2_coefficients

write_csv(question_a2_coefficients, "question_a2_coefficients.csv")

#create 3 column: variable, number_of_levels, number_of_fitted_coefficient
variable <- c("state", "sex", "race", "age", "education", "party", "ideology", "state_contestedness")
number_of_levels <- c(49, 2, 4, 4, 4, 3, 3, 4)
#????????
number_of_fitted_coefficients <- number_of_levels - 1

question_a2_levels_vs_coefficients <- tibble(variable, number_of_levels, number_of_fitted_coefficients)

write_csv(question_a2_levels_vs_coefficients, "question_a2_levels_vs_coefficients.csv")

#A3: Evaluating the model
#estimates probality of voting for Obama
predicted_probability <- predict(model, type = 'response')
predicted_probability

#separate probability into two variable: predictions_point_5, predictions_point_7.
#set probability >=0.5 as 1, probability < 0.5 as 0
predictions_point_5 <- ifelse(predicted_probability >= 0.5, 1, 0)
predictions_point_5

#set probability >=0.7 as 1, probability <0.7 as 0
predictions_point_7 <- ifelse(predicted_probability >= 0.7, 1, 0)
predictions_point_7

question_a3 <- data.frame(predicted_probability, predictions_point_5, predictions_point_7)

write.csv(question_a3, "question_a3.csv")

#compute accuracy

