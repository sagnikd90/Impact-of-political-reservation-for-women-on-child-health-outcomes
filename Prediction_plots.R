library(dplyr)
library(foreign)
library(ggplot2)
library(Hmisc)
library(plm)
library(broom)
library(jtools)
library(tidyverse)
library(lmtest)
library(reshape2)
library(stargazer)
library(hrbrthemes)
library(sjPlot)
library(ggeffects)

setwd("C:\\Users\\das90\\OneDrive - Cuny GradCenter\\Research\\Political reservation and age of marriage\\Github\\Political-reservation-and-age-of-marriage-age-of-first-birth\\Final version\\R results\\R_graphs")

df<- read.csv("C:\\Users\\das90\\OneDrive - Cuny GradCenter\\Research\\Political reservation and age of marriage\\Github\\Political-reservation-and-age-of-marriage-age-of-first-birth\\Final version\\Cleaned datasets\\Preferred_sample_data_for_summary_analysis.csv")

theme_set(theme_ipsum())

df$birth_order[df$birth_order>=3]<- 3

df_mothers_age<- df%>%
  subset(mothers_age_at_birth>=15)

###################### Neonatal survival probability ############################

reg_1<- glm(data=df_mothers_age,
            formula = neonat~mothers_age_at_birth + factor(birth_order) + sex +
              factor(state_name) + 
              factor(cohort),
            family = binomial)

p_1<-ggpredict(reg_1,c("mothers_age_at_birth[meansd]","birth_order","sex"))

png("Predicted_probability_neonat_mother_age.png")

plot(p_1,
     ci.style = "errorbar",
     colors = "hero",
     dot.size = 5,
     dot.alpha = 0.3)+
  labs(x="Mother's age at birth",
       y="Predicted probability of survival till the age of 1 month",
       color="Birth order",
       title = "")

dev.off()

###################### Infant survival probability ############################

reg_2<- glm(data=df_mothers_age,
            formula = infant~mothers_age_at_birth + factor(birth_order) + sex +
              factor(state_name) + 
              factor(cohort),
            family = binomial)

p_2<-ggpredict(reg_2,c("mothers_age_at_birth[meansd]","birth_order","sex"))

png("Predicted_probability_infant_mother_age.png")

plot(p_2,
     ci.style = "errorbar",
     colors = "hero",
     dot.size = 5,
     dot.alpha = 0.3)+
  labs(x="Mother's age at birth",
       y="Predicted probability of survival till the age of 1 year",
       color="Birth order",
       title = "")

dev.off()

###################### Under 5 survival probability ############################

reg_3<- glm(data=df_mothers_age,
            formula = under5~mothers_age_at_birth + factor(birth_order) + sex +
              factor(state_name) + 
              factor(cohort),
            family = binomial)

p_3<-ggpredict(reg_3,c("mothers_age_at_birth[meansd]","birth_order","sex"))

png("Predicted_probability_under5_mother_age.png")

plot(p_3,
     ci.style = "errorbar",
     colors = "hero",
     dot.size = 5,
     dot.alpha = 0.3)+
  labs(x="Mother's age at birth",
       y="Predicted probability of survival till the age of 5 years",
       color="Birth order",
       title = "")

dev.off()

########################### Under 10 survival probability ###########################

reg_4<- glm(data=df_mothers_age,
           formula = under10~mothers_age_at_birth + factor(birth_order) + sex +
             factor(state_name) + 
             factor(cohort),
           family = binomial)

p_4<-ggpredict(reg_4,c("mothers_age_at_birth[meansd]","birth_order","sex"))

png("Predicted_probability_udner10_mother_age.png")

plot(p_4,
     ci.style = "errorbar",
     colors = "hero",
     dot.size = 5,
     dot.alpha = 0.3)+
  labs(x="Mother's age at birth",
       y="Predicted probability of survival till the age of 10 years",
       color="Birth order",
       title = "")

dev.off()