#Part B
#Part B2

source('/Users/vitygao/Desktop/Fall 2019/Messy data and Machine Learning/Homework_3/script/library.R')
#import dataset
sqf.data <- readr::read_csv("data/sqf_08_16.csv")
table(sqf.data$year)

sqf.data_cpw <- sqf.data %>% 
  filter(suspected.crime == "cpw")

#conduct the logistic regression model on 2008
sqf.data_cpw_2008 <- sqf.data_cpw %>% 
  filter(year == 2008)
table(sqf.data_cpw_2008$year)

#standardized the predictor variable
sqf.data_cpw_2008 <- sqf.data_cpw_2008 %>% 
  mutate(suspect.age.std = standardize(suspect.age),
         suspect.height.std = standardize(suspect.height),
         suspect.weight.std = standardize(suspect.weight),
         observation.period.std = standardize(observation.period))

#conduct the logistic regression
model <- glm(as.factor(found.weapon)~ 1 + as.factor(precinct) + as.factor(location.housing) + additional.report +
               additional.investigation + additional.proximity + additional.evasive + 
               additional.associating + additional.direction + additional.highcrime +
               additional.time + additional.sights + additional.other + stopped.bc.object +
               stopped.bc.desc + stopped.bc.casing + stopped.bc.lookout + stopped.bc.clothing +
               stopped.bc.drugs + stopped.bc.furtive + stopped.bc.violent + stopped.bc.bulge +
               stopped.bc.other + suspect.age.std + suspect.height.std + suspect.weight.std +
               observation.period.std + as.factor(suspect.sex) + as.factor(suspect.build) + inside + radio.run +
               as.factor(day) + as.factor(month) + as.factor(time.period), data = sqf.data_cpw_2008, family = 'binomial')
model

#store ten largest and tem smallest coefficient names and estimates in a dataframe
summary(model)

sum_model <- as.data.frame(coef(model))

sum_model <- sum_model %>% 
  mutate(coefficient_name = rownames(sum_model)) %>% 
  arrange(desc(coef(model))) %>% 
  filter(row_number() %in% c(1:10,122:131))

write_csv(sum_model, "data/question_b2_coefficients.csv")

#B3
#precinct, time.period??????
suppose_model <- data.frame(suspect.age.std=(30-mean(sqf.data_cpw_2008$suspect.age))/sd(sqf.data_cpw_2008$suspect.age),
                            suspect.height.std=(6-mean(sqf.data_cpw_2008$suspect.height))/sd(sqf.data_cpw_2008$suspect.height),
                            suspect.weight.std=(165-mean(sqf.data_cpw_2008$suspect.weight))/sd(sqf.data_cpw_2008$suspect.weight),
                            observation.period.std=(10-mean(sqf.data_cpw_2008$observation.period))/sd(sqf.data_cpw_2008$observation.period),
                            suspect.build = "medium", location.housing = "transit", month = "October", day = "Wednesday", time.period = 6,
                            precinct = 6, radio.run = FALSE, suspect.sex = c("male","female"), stopped.bc.bulge = TRUE, 
                            additional.report = FALSE, additional.investigation = FALSE, additional.proximity = FALSE, additional.evasive = FALSE,
                            additional.associating = FALSE, additional.direction = FALSE, additional.highcrime = FALSE, additional.time = FALSE,
                            additional.sights = FALSE, additional.other = FALSE, stopped.bc.desc = FALSE, stopped.bc.object =FALSE, stopped.bc.casing = FALSE,
                            stopped.bc.lookout = FALSE, stopped.bc.clothing = FALSE, stopped.bc.drugs = FALSE, stopped.bc.furtive = FALSE,
                            stopped.bc.violent = FALSE, stopped.bc.other = FALSE, inside = TRUE)
#make the probability
predict(model, suppose_model, type = "response")
#???no results suspect.weight: mean, sd:NA

#filter 2009 data
sqf.data_cpw_2009 <- sqf.data_cpw %>% 
  filter(year == 2009) %>% 
  mutate(suspect.age.std = standardize(suspect.age),
         suspect.height.std = standardize(suspect.height),
         suspect.weight.std = standardize(suspect.weight),
         observation.period.std = standardize(observation.period))

#make the predict in 2009
pred_2009 <- predict(model, sqf.data_cpw_2009, type="response")
pred_ROCR <- ROCR::prediction(pred_2009, sqf.data_cpw_2009$found.weapon)

#compute the AUC
test.R <- performance(pred_ROCR,"auc")
cat('the auc score is ', test.R@y.values[[1]], "\n") 
#0.811

