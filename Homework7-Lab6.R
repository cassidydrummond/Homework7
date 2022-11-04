#Cassidy Drummond
#November 4th, 2022
#Homework 7, Lab 6

#1. Cassidy
#2. Below
#3. On the other uploaded file

#Attach data
attach(Household_Pulse_data)
#Summary to ensure the data looks correct and to know how to eventually break it up/create a subgroup
summary(Household_Pulse_data)
model_logit1 <- glm(Household_Pulse_data$RECVDVACC ~ EEDUC,
                    family = binomial, data = Household_Pulse_data)
model_logit1

#Create dataset to just look at vaccination status, birth year below 2000, edducation level, race, marital status, and gender
Household_Pulse_data$vaxx <- (Household_Pulse_data$RECVDVACC == "yes got vaxx")
is.na(Household_Pulse_data$vaxx) <- which(Household_Pulse_data$RECVDVACC == "NA")
table(Household_Pulse_data$vaxx,Household_Pulse_data$EEDUC)

#Create generalized linear model
model_logit1 <- glm(vaxx ~ TBIRTH_YEAR + EEDUC + MS + RRACE + RHISPANIC + GENID_DESCRIBE,
                    family = binomial, data = dat_use1)
summary(model_logit1)

#Remove NAs to ensure it is actually a binominal (yes or no)
pick_use1 <- (Household_Pulse_data$TBIRTH_YEAR < 2000) 
dat_use1 <- subset(Household_Pulse_data, pick_use1)
dat_use1$RECVDVACC <- droplevels(dat_use1$RECVDVACC) 

#Created baseline model, and now running generalized linear model
model_logit2 <- glm(vaxx ~ EEDUC + MS + RRACE + GENID_DESCRIBE, family = binomial, data = dat_use1)
summary(model_logit2)

#Now we get a lil funky with the data and start testing different variables
to_be_predicted2 <- data.frame(EEDUC = "HS diploma", MS = "never", RRACE = "White", GENID_DESCRIBE = "male", data = dat_use1)
to_be_predicted2$yhat <- predict(model_logit2, to_be_predicted2, type = "response")
summary(to_be_predicted2$yhat)
#We see that high  school educated white men who have never been married over the age of 22 are prediced probability to be vaccinated is .7179 

#Now we look at the data of hispanic men who have never been married with a bachelors degree born in 1990
new_data_to_be_predicted <- data.frame(TBIRTH_YEAR = 1990,
                                       EEDUC = factor("bach deg", levels = levels(dat_use1$EEDUC)),
                                       MS = factor("never",levels = levels(dat_use1$MS)),
                                       RRACE = factor("Black",levels = levels(dat_use1$RRACE)),
                                       RHISPANIC = factor("Hispanic",levels = levels(dat_use1$RHISPANIC)),
                                       GENID_DESCRIBE = factor("male", levels = levels(dat_use1$GENID_DESCRIBE))
)
predict(model_logit1,new_data_to_be_predicted)
#We get the result of 2.618087

#Now we look at the data of hispanic women who are married with some high school born in 1965
new_data_to_be_predicted2 <- data.frame(TBIRTH_YEAR = 1965,
                                       EEDUC = factor("some hs", levels = levels(dat_use1$EEDUC)),
                                       MS = factor("married",levels = levels(dat_use1$MS)),
                                       RRACE = factor("Black",levels = levels(dat_use1$RRACE)),
                                       RHISPANIC = factor("Hispanic",levels = levels(dat_use1$RHISPANIC)),
                                       GENID_DESCRIBE = factor("female", levels = levels(dat_use1$GENID_DESCRIBE))
)
predict(model_logit1,new_data_to_be_predicted2)
#We get the result of 1.504923

#Now we look at a probit model
model_probit2 <- glm(vaxx ~ EEDUC + MS + RRACE  + GENID_DESCRIBE + ANYWORK*INCOME,
                     family = binomial (link = 'probit'), data = dat_use1)
summary(model_probit2)
to_be_predicted3<- data.frame(EEDUC = "some hs", MS = "married", RRACE = "hispanic", GENID_DESCRIBE = "female", ANYWORK = "yes employment in last 7 days", INCOME= "HH income $100 - 149",  data = dat_use1)
to_be_predicted3$yhat<-predict(model_probit2, to_be_predicted3, type="response")
summary(to_be_predicted3$yhat)
