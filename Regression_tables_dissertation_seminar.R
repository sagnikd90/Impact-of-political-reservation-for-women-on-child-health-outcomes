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
library(miceadds)
library(fixest)

setwd("C:\\Users\\das90\\OneDrive - Cuny GradCenter\\Research\\Political reservation and age of marriage\\Github\\Political-reservation-and-age-of-marriage-age-of-first-birth\\Final version\\R results")

df<- read.csv("C:\\Users\\das90\\OneDrive - Cuny GradCenter\\Research\\Political reservation and age of marriage\\Github\\Political-reservation-and-age-of-marriage-age-of-first-birth\\Final version\\Cleaned datasets\\Preferred_sample_data_for_analysis.csv")

############################## Data cleaning for first and second order comparison ##########################

df_first_second_match<- df%>%                               ##### Selecting the data for birth order 2 vs 1 analysis ###########
subset(birth_order==1|birth_order==2 & !is.na(alive))%>%
  group_by(id)%>%
  summarise(group_size=length(id))%>%
  subset(group_size==2)

df_first_second_total<- df%>%
  subset(birth_order==1|birth_order==2)


df_first_second<- merge(df_first_second_total,df_first_second_match,by="id")

df_first_second<- df_first_second[order(df_first_second$id,df_first_second$cohort),]

############################## Data cleaning for first and third order comparison ##########################

df_first_third_match<- df%>%                                ##### Selecting the data for birth order 3 vs 1 analysis ###########
subset(birth_order==1|birth_order==3 & !is.na(alive))%>%
  group_by(id)%>%
  summarise(group_size=length(id))%>%
  subset(group_size>=2)

df_first_third_total<- df%>%
  subset(birth_order==1|birth_order==3)

df_first_third<- merge(df_first_third_total,df_first_third_match,by="id")

df_first_third<- df_first_third[order(df_first_third$id,df_first_third$cohort),]

############################## Data cleaning for second and third order comparison ##########################

df_second_third_match<- df%>%                                ##### Selecting the data for birth order 3 vs 2 analysis ###########
subset(birth_order==2|birth_order==3 & !is.na(alive))%>%
  group_by(id)%>%
  summarise(group_size=length(id))%>%
  subset(group_size>=2)

df_second_third_total<- df%>%
  subset(birth_order==2|birth_order==3)

df_second_third<- merge(df_second_third_total,df_second_third_match,by="id")

df_second_third<- df_second_third[order(df_second_third$id,df_second_third$cohort),]



############################# Baseline estimates for each birth order separately ######################################

df_1<- df%>%
  subset(birth_order==1)

df_2<- df%>%
  subset(birth_order==2)

df_3<- df%>%
  subset(birth_order==3)


################################## Baseline regressions ########################

######################### Neonatal survival probability ###########################

reg_1<- lm(data = df_1,
           formula = neonat~treat+
             factor(state_name)+
             factor(cohort)+
             sex+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_1<- vcovHC(reg_1, 
              type = "HC0", 
              cluster = "state_name")

se_1<- sqrt(diag(se_1))

reg_2<- lm(data = df_2,
           formula = neonat~treat+
             factor(state_name)+
             factor(cohort)+
             sex+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_2<- vcovHC(reg_2, 
              type = "HC0", 
              cluster = "state_name")

se_2<- sqrt(diag(se_2))

reg_3<- lm(data = df_3,
           formula = neonat~treat+
             factor(state_name)+
             factor(cohort)+
             sex+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_3<- vcovHC(reg_3, 
              type = "HC0", 
              cluster = "state_name")

se_3<- sqrt(diag(se_3))


######################### Infant survival probability ###########################

reg_4<- lm(data = df_1,
           formula = infant~treat+
             factor(state_name)+
             factor(cohort)+
             sex+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_4<- vcovHC(reg_4, 
              type = "HC0", 
              cluster = "state_name")

se_4<- sqrt(diag(se_4))

reg_5<- lm(data = df_2,
           formula = infant~treat+
             factor(state_name)+
             factor(cohort)+
             sex+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_5<- vcovHC(reg_5, 
              type = "HC0", 
              cluster = "state_name")

se_5<- sqrt(diag(se_5))

reg_6<- lm(data = df_3,
           formula = infant~treat+
             factor(state_name)+
             factor(cohort)+
             sex+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_6<- vcovHC(reg_6, 
              type = "HC0", 
              cluster = "state_name")

se_6<- sqrt(diag(se_6))


######################### Under 5 survival probability ###########################

reg_7<- lm(data = df_1,
           formula = under5~treat+
             factor(state_name)+
             factor(cohort)+
             sex+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_7<- vcovHC(reg_7, 
              type = "HC0", 
              cluster = "state_name")

se_7<- sqrt(diag(se_7))

reg_8<- lm(data = df_2,
           formula = under5~treat+
             factor(state_name)+
             factor(cohort)+
             sex+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_8<- vcovHC(reg_8, 
              type = "HC0", 
              cluster = "state_name")

se_8<- sqrt(diag(se_8))

reg_9<- lm(data = df_3,
           formula = under5~treat+
             factor(state_name)+
             factor(cohort)+
             sex+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_9<- vcovHC(reg_9, 
              type = "HC0", 
              cluster = "state_name")

se_9<- sqrt(diag(se_9))

######################### Under 10 survival probability ###########################

reg_10<- lm(data = df_1,
            formula = under10~treat+
              factor(state_name)+
              factor(cohort)+
              sex+
              mothers_age_at_birth+
              total_child+
              read_write_woman+
              religion+
              caste+
              type_of_house) 

se_10<- vcovHC(reg_10, 
               type = "HC0", 
               cluster = "state_name")

se_10<- sqrt(diag(se_10))

reg_11<- lm(data = df_2,
            formula = under10~treat+
              factor(state_name)+
              factor(cohort)+
              sex+
              mothers_age_at_birth+
              total_child+
              read_write_woman+
              religion+
              caste+
              type_of_house) 

se_11<- vcovHC(reg_11, 
               type = "HC0", 
               cluster = "state_name")

se_11<- sqrt(diag(se_11))

reg_12<- lm(data = df_3,
            formula = under10~treat+
              factor(state_name)+
              factor(cohort)+
              sex+
              mothers_age_at_birth+
              total_child+
              read_write_woman+
              religion+
              caste+
              type_of_house) 

se_12<- vcovHC(reg_12, 
               type = "HC0", 
               cluster = "state_name")

se_12<- sqrt(diag(se_12))

stargazer(reg_1,reg_2,reg_3,reg_4,reg_5,reg_6,
          se=list(se_1,se_2,se_3,se_4,se_5,se_6),
          omit = c("state_name",
                   "cohort",
                   "read_write_woman",
                   "caste",
                   "religion",
                   "type_of_house",
                   "Constant"),
          order = c("^treat$",
                    "^sex$",
                    "^mothers_age_at_birth$",
                    "^total_child$"),
          covariate.labels = c("Post-treat",
                               "Sex of the child",
                               "Mother's age at birth",
                               "Total fertility measure"))


stargazer(reg_7,reg_8,reg_9,reg_10,reg_11,reg_12,
          se=list(se_7,se_8,se_9,se_10,se_11,se_12),
          omit = c("state_name",
                   "cohort",
                   "read_write_woman",
                   "caste",
                   "religion",
                   "type_of_house",
                   "Constant"),
          order = c("^treat$",
                    "^sex$",
                    "^mothers_age_at_birth$",
                    "^total_child$"),
          covariate.labels = c("Post-treat",
                               "Sex of the child",
                               "Mother's age at birth",
                               "Total fertility measure"))



############################# Regressions ####################################

reg_1<- lm(data = df_first_second,
           formula = neonat~factor(birth_order)*treat+
             factor(state_name)+
             factor(cohort)+
             sex+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_1 <- vcovHC(reg_1, 
               type = "HC0", 
               cluster = "state_name")

se_1 <- sqrt(diag(se_1))

reg_2<- lm(data = df_first_second,
           formula = infant~factor(birth_order)*treat+
             factor(state_name)+
             factor(cohort)+
             sex+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_2 <- vcovHC(reg_2, 
               type = "HC0", 
               cluster = "state_name")

se_2 <- sqrt(diag(se_2))


reg_3<- lm(data = df_first_second,
           formula = under5~factor(birth_order)*treat+
             factor(state_name)+
             factor(cohort)+
             sex+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_3 <- vcovHC(reg_3, 
               type = "HC0", 
               cluster = "state_name")

se_3 <- sqrt(diag(se_3))

reg_4<- lm(data = df_first_second,
           formula = under10~factor(birth_order)*treat+
             factor(state_name)+
             factor(cohort)+
             sex+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_4 <- vcovHC(reg_4, 
               type = "HC0", 
               cluster = "state_name")

se_4 <- sqrt(diag(se_4))

stargazer(reg_1,reg_2,reg_3,reg_4,
          se=list(se_1,se_2,se_3,se_4),
          omit = c("state_name",
                   "cohort",
                   "read_write_woman",
                   "caste",
                   "religion",
                   "type_of_house",
                   "Constant"),
          order = c("^factor(birth_order)2:treat$",
                    "sex",
                    "^mothers_age_at_birth$",
                    "^total_child$"),
          covariate.labels = c("Sex of the child",
                               "Mother's age at birth",
                               "Total fertility measure",
                               "(Birth order=2 vs 1)",
                               "Post-treat",
                               "(Birth order=2 vs 1)*post-treat"))


############################ First vs third ########################

reg_5<- lm(data = df_first_third,
           formula = neonat~factor(birth_order)*treat+
             factor(state_name)+
             factor(cohort)+
             sex+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_5 <- vcovHC(reg_5, 
               type = "HC0", 
               cluster = "state_name")

se_5 <- sqrt(diag(se_5))

reg_6<- lm(data = df_first_third,
           formula = infant~factor(birth_order)*treat+
             factor(state_name)+
             factor(cohort)+
             sex+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_6 <- vcovHC(reg_6, 
               type = "HC0", 
               cluster = "state_name")

se_6 <- sqrt(diag(se_6))

reg_7<- lm(data = df_first_third,
           formula = under5~factor(birth_order)*treat+
             factor(state_name)+
             factor(cohort)+
             sex+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_7 <- vcovHC(reg_7, 
               type = "HC0", 
               cluster = "state_name")

se_7 <- sqrt(diag(se_7))


reg_8<- lm(data = df_first_third,
           formula = under10~factor(birth_order)*treat+
             factor(state_name)+
             factor(cohort)+
             sex+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_8 <- vcovHC(reg_8, 
               type = "HC0", 
               cluster = "state_name")

se_8 <- sqrt(diag(se_8))

stargazer(reg_5,reg_6,reg_7,reg_8,
          se=list(se_5,se_6,se_7,se_8),
          omit = c("state_name",
                   "cohort",
                   "read_write_woman",
                   "caste",
                   "religion",
                   "type_of_house",
                   "Constant"),
          order = c("^factor(birth_order)3:treat$",
                    "sex",
                    "^mothers_age_at_birth$",
                    "^total_child$"),
          covariate.labels = c("Sex of the child",
                               "Mother's age at birth",
                               "Total fertility measure",
                               "(Birth order=3 vs 1)",
                               "Post-treat",
                               "(Birth order=3 vs 1)*post-treat"))

######################### Second vs third ########################

reg_9<- lm(data = df_second_third,
           formula = neonat~factor(birth_order)*treat+
             factor(state_name)+
             factor(cohort)+
             sex+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_9 <- vcovHC(reg_9, 
               type = "HC0", 
               cluster = "state_name")

se_9 <- sqrt(diag(se_9))

reg_10<- lm(data = df_second_third,
            formula = infant~factor(birth_order)*treat+
              factor(state_name)+
              factor(cohort)+
              sex+
              mothers_age_at_birth+
              total_child+
              read_write_woman+
              religion+
              caste+
              type_of_house) 

se_10 <- vcovHC(reg_10, 
                type = "HC0", 
                cluster = "state_name")

se_10 <- sqrt(diag(se_10))

reg_11<- lm(data = df_second_third,
            formula = under5~factor(birth_order)*treat+
              factor(state_name)+
              factor(cohort)+
              sex+
              mothers_age_at_birth+
              total_child+
              read_write_woman+
              religion+
              caste+
              type_of_house) 

se_11 <- vcovHC(reg_11, 
                type = "HC0", 
                cluster = "state_name")

se_11 <- sqrt(diag(se_11))

reg_12<- lm(data = df_second_third,
            formula = under10~factor(birth_order)*treat+
              factor(state_name)+
              factor(cohort)+
              sex+
              mothers_age_at_birth+
              total_child+
              read_write_woman+
              religion+
              caste+
              type_of_house) 

se_12 <- vcovHC(reg_12, 
                type = "HC0", 
                cluster = "state_name")

se_12 <- sqrt(diag(se_12))

stargazer(reg_9,reg_10,reg_11,reg_12,
          se=list(se_9,se_10,se_11,se_12),
          omit = c("state_name",
                   "cohort",
                   "read_write_woman",
                   "caste",
                   "religion",
                   "type_of_house",
                   "Constant"),
          order = c("^factor(birth_order)3:treat$",
                    "sex",
                    "^mothers_age_at_birth$",
                    "^total_child$"),
          covariate.labels = c("Sex of the child",
                               "Mother's age at birth",
                               "Total fertility measure",
                               "(Birth order=3 vs 2)",
                               "Post-treat",
                               "(Birth order=3 vs 2)*post-treat"))

##################################### Survival probability when first born is a boy ###################################

df_first_second_boys<- df_first_second%>%
  subset(first_born_boy==1)

df_first_third_boys<- df_first_third%>%
  subset(first_born_boy==1)

df_second_third_boys<- df_second_third%>%
  subset(first_born_boy==1)

####################################### Regressions ####################################

reg_1<- lm(data = df_first_second_boys,
           formula = neonat~factor(birth_order)*treat+
             factor(state_name)+
             factor(cohort)+
             sex+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_1 <- vcovHC(reg_1, 
               type = "HC0", 
               cluster = "state_name")

se_1 <- sqrt(diag(se_1))

reg_2<- lm(data = df_first_second_boys,
           formula = infant~factor(birth_order)*treat+
             factor(state_name)+
             factor(cohort)+
             sex+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_2 <- vcovHC(reg_2, 
               type = "HC0", 
               cluster = "state_name")

se_2 <- sqrt(diag(se_2))


reg_3<- lm(data = df_first_second_boys,
           formula = under5~factor(birth_order)*treat+
             factor(state_name)+
             factor(cohort)+
             sex+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_3 <- vcovHC(reg_3, 
               type = "HC0", 
               cluster = "state_name")

se_3 <- sqrt(diag(se_3))

reg_4<- lm(data = df_first_second_boys,
           formula = under10~factor(birth_order)*treat+
             factor(state_name)+
             factor(cohort)+
             sex+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_4 <- vcovHC(reg_4, 
               type = "HC0", 
               cluster = "state_name")

se_4 <- sqrt(diag(se_4))

stargazer(reg_1,reg_2,reg_3,reg_4,
          se=list(se_1,se_2,se_3,se_4),
          omit = c("state_name",
                   "cohort",
                   "read_write_woman",
                   "caste",
                   "religion",
                   "type_of_house",
                   "Constant"),
          order = c("^factor(birth_order)2:treat$",
                    "sex",
                    "^mothers_age_at_birth$",
                    "^total_child$"),
          covariate.labels = c("Sex of the child",
                               "Mother's age at birth",
                               "Total fertility measure",
                               "(Birth order=2 vs 1)",
                               "Post-treat",
                               "(Birth order=2 vs 1)*post-treat"))


############################ First vs third ########################

reg_5<- lm(data = df_first_third_boys,
           formula = neonat~factor(birth_order)*treat+
             factor(state_name)+
             factor(cohort)+
             sex+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_5 <- vcovHC(reg_5, 
               type = "HC0", 
               cluster = "state_name")

se_5 <- sqrt(diag(se_5))

reg_6<- lm(data = df_first_third_boys,
           formula = infant~factor(birth_order)*treat+
             factor(state_name)+
             factor(cohort)+
             sex+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_6 <- vcovHC(reg_6, 
               type = "HC0", 
               cluster = "state_name")

se_6 <- sqrt(diag(se_6))

reg_7<- lm(data = df_first_third_boys,
           formula = under5~factor(birth_order)*treat+
             factor(state_name)+
             factor(cohort)+
             sex+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_7 <- vcovHC(reg_7, 
               type = "HC0", 
               cluster = "state_name")

se_7 <- sqrt(diag(se_7))


reg_8<- lm(data = df_first_third_boys,
           formula = under10~factor(birth_order)*treat+
             factor(state_name)+
             factor(cohort)+
             sex+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_8 <- vcovHC(reg_8, 
               type = "HC0", 
               cluster = "state_name")

se_8 <- sqrt(diag(se_8))

stargazer(reg_5,reg_6,reg_7,reg_8,
          se=list(se_5,se_6,se_7,se_8),
          omit = c("state_name",
                   "cohort",
                   "read_write_woman",
                   "caste",
                   "religion",
                   "type_of_house",
                   "Constant"),
          order = c("^factor(birth_order)3:treat$",
                    "sex",
                    "^mothers_age_at_birth$",
                    "^total_child$"),
          covariate.labels = c("Sex of the child",
                               "Mother's age at birth",
                               "Total fertility measure",
                               "(Birth order=3 vs 1)",
                               "Post-treat",
                               "(Birth order=3 vs 1)*post-treat"))

######################### Second vs third ########################

reg_9<- lm(data = df_second_third_boys,
           formula = neonat~factor(birth_order)*treat+
             factor(state_name)+
             factor(cohort)+
             sex+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_9 <- vcovHC(reg_9, 
               type = "HC0", 
               cluster = "state_name")

se_9 <- sqrt(diag(se_9))

reg_10<- lm(data = df_second_third_boys,
            formula = infant~factor(birth_order)*treat+
              factor(state_name)+
              factor(cohort)+
              sex+
              mothers_age_at_birth+
              total_child+
              read_write_woman+
              religion+
              caste+
              type_of_house) 

se_10 <- vcovHC(reg_10, 
                type = "HC0", 
                cluster = "state_name")

se_10 <- sqrt(diag(se_10))

reg_11<- lm(data = df_second_third_boys,
            formula = under5~factor(birth_order)*treat+
              factor(state_name)+
              factor(cohort)+
              sex+
              mothers_age_at_birth+
              total_child+
              read_write_woman+
              religion+
              caste+
              type_of_house) 

se_11 <- vcovHC(reg_11, 
                type = "HC0", 
                cluster = "state_name")

se_11 <- sqrt(diag(se_11))

reg_12<- lm(data = df_second_third_boys,
            formula = under10~factor(birth_order)*treat+
              factor(state_name)+
              factor(cohort)+
              sex+
              mothers_age_at_birth+
              total_child+
              read_write_woman+
              religion+
              caste+
              type_of_house) 

se_12 <- vcovHC(reg_12, 
                type = "HC0", 
                cluster = "state_name")

se_12 <- sqrt(diag(se_12))

stargazer(reg_9,reg_10,reg_11,reg_12,
          se=list(se_9,se_10,se_11,se_12),
          omit = c("state_name",
                   "cohort",
                   "read_write_woman",
                   "caste",
                   "religion",
                   "type_of_house",
                   "Constant"),
          order = c("^factor(birth_order)3:treat$",
                    "sex",
                    "^mothers_age_at_birth$",
                    "^total_child$"),
          covariate.labels = c("Sex of the child",
                               "Mother's age at birth",
                               "Total fertility measure",
                               "(Birth order=3 vs 2)",
                               "Post-treat",
                               "(Birth order=3 vs 2)*post-treat"))


##################################### Survival probability when first two born are boys ###################################

########################################### First vs second birth order comparison ##########################

df_first_third_2_boys<- df_first_third%>%
  subset(first_two_boys==1)

df_second_third_2_boys<- df_second_third%>%
  subset(first_two_boys==1)


############################ First vs third ########################

reg_1<- lm(data = df_first_third_2_boys,
           formula = neonat~factor(birth_order)*treat+
             factor(state_name)+
             factor(cohort)+
             sex+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_1<- vcovHC(reg_1, 
              type = "HC0", 
              cluster = "state_name")

se_1 <- sqrt(diag(se_1))

reg_2<- lm(data = df_first_third_2_boys,
           formula = infant~factor(birth_order)*treat+
             factor(state_name)+
             factor(cohort)+
             sex+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_2 <- vcovHC(reg_2, 
               type = "HC0", 
               cluster = "state_name")

se_2 <- sqrt(diag(se_2))

reg_3<- lm(data = df_first_third_2_boys,
           formula = under5~factor(birth_order)*treat+
             factor(state_name)+
             factor(cohort)+
             sex+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_3 <- vcovHC(reg_3, 
               type = "HC0", 
               cluster = "state_name")

se_3 <- sqrt(diag(se_3))

reg_4<- lm(data = df_first_third_2_boys,
           formula = under10~factor(birth_order)*treat+
             factor(state_name)+
             factor(cohort)+
             sex+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_4 <- vcovHC(reg_4, 
               type = "HC0", 
               cluster = "state_name")

se_4 <- sqrt(diag(se_4))

reg_5<- lm(data = df_second_third_2_boys,
           formula = neonat~factor(birth_order)*treat+
             factor(state_name)+
             factor(cohort)+
             sex+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_5<- vcovHC(reg_5, 
              type = "HC0", 
              cluster = "state_name")

se_5 <- sqrt(diag(se_5))

reg_6<- lm(data = df_second_third_2_boys,
           formula = infant~factor(birth_order)*treat+
             factor(state_name)+
             factor(cohort)+
             sex+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_6<- vcovHC(reg_6, 
              type = "HC0", 
              cluster = "state_name")

se_6 <- sqrt(diag(se_6))

reg_7<- lm(data = df_second_third_2_boys,
           formula = under5~factor(birth_order)*treat+
             factor(state_name)+
             factor(cohort)+
             sex+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_7 <- vcovHC(reg_7, 
               type = "HC0", 
               cluster = "state_name")

se_7 <- sqrt(diag(se_7))

reg_8<- lm(data = df_second_third_2_boys,
           formula = under10~factor(birth_order)*treat+
             factor(state_name)+
             factor(cohort)+
             sex+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_8 <- vcovHC(reg_8, 
               type = "HC0", 
               cluster = "state_name")

se_8 <- sqrt(diag(se_8))

stargazer(reg_1,reg_2,reg_3,reg_4,
          se=list(se_1,se_2,se_3,se_4),
          omit = c("state_name",
                   "cohort",
                   "read_write_woman",
                   "caste",
                   "religion",
                   "type_of_house",
                   "Constant"),
          order = c("^factor(birth_order)3:treat$",
                    "sex",
                    "^mothers_age_at_birth$",
                    "^total_child$"),
          covariate.labels = c("Sex of the child",
                               "Mother's age at birth",
                               "Total fertility measure",
                               "(Birth order=3 vs 1)",
                               "Post-treat",
                               "(Birth order=3 vs 1)*post-treat"))

stargazer(reg_5,reg_6,reg_7,reg_8,
          se=list(se_5,se_6,se_7,se_8),
          omit = c("state_name",
                   "cohort",
                   "read_write_woman",
                   "caste",
                   "religion",
                   "type_of_house",
                   "Constant"),
          order = c("^factor(birth_order)3:treat$",
                    "sex",
                    "^mothers_age_at_birth$",
                    "^total_child$"),
          covariate.labels = c("Sex of the child",
                               "Mother's age at birth",
                               "Total fertility measure",
                               "(Birth order=3 vs 1)",
                               "Post-treat",
                               "(Birth order=3 vs 1)*post-treat"))


##################################### Survival probability when first two born are girls ###################################

########################################### First vs second birth order comparison ##########################

df_first_third_2_girls<- df_first_third%>%
  subset(first_two_girls==1)

df_second_third_2_girls<- df_second_third%>%
  subset(first_two_girls==1)


############################ First vs third ########################

reg_1<- lm(data = df_first_third_2_girls,
           formula = neonat~factor(birth_order)*treat+
             factor(state_name)+
             factor(cohort)+
             sex+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_1<- vcovHC(reg_1, 
              type = "HC0", 
              cluster = "state_name")

se_1 <- sqrt(diag(se_1))

reg_2<- lm(data = df_first_third_2_girls,
           formula = infant~factor(birth_order)*treat+
             factor(state_name)+
             factor(cohort)+
             sex+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_2 <- vcovHC(reg_2, 
               type = "HC0", 
               cluster = "state_name")

se_2 <- sqrt(diag(se_2))

reg_3<- lm(data = df_first_third_2_girls,
           formula = under5~factor(birth_order)*treat+
             factor(state_name)+
             factor(cohort)+
             sex+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_3 <- vcovHC(reg_3, 
               type = "HC0", 
               cluster = "state_name")

se_3 <- sqrt(diag(se_3))

reg_4<- lm(data = df_first_third_2_girls,
           formula = under10~factor(birth_order)*treat+
             factor(state_name)+
             factor(cohort)+
             sex+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_4 <- vcovHC(reg_4, 
               type = "HC0", 
               cluster = "state_name")

se_4 <- sqrt(diag(se_4))

reg_5<- lm(data = df_second_third_2_girls,
           formula = neonat~factor(birth_order)*treat+
             factor(state_name)+
             factor(cohort)+
             sex+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_5<- vcovHC(reg_5, 
              type = "HC0", 
              cluster = "state_name")

se_5 <- sqrt(diag(se_5))

reg_6<- lm(data = df_second_third_2_girls,
           formula = infant~factor(birth_order)*treat+
             factor(state_name)+
             factor(cohort)+
             sex+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_6<- vcovHC(reg_6, 
              type = "HC0", 
              cluster = "state_name")

se_6 <- sqrt(diag(se_6))

reg_7<- lm(data = df_second_third_2_girls,
           formula = under5~factor(birth_order)*treat+
             factor(state_name)+
             factor(cohort)+
             sex+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_7 <- vcovHC(reg_7, 
               type = "HC0", 
               cluster = "state_name")

se_7 <- sqrt(diag(se_7))

reg_8<- lm(data = df_second_third_2_girls,
           formula = under10~factor(birth_order)*treat+
             factor(state_name)+
             factor(cohort)+
             sex+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_8 <- vcovHC(reg_8, 
               type = "HC0", 
               cluster = "state_name")

se_8 <- sqrt(diag(se_8))

stargazer(reg_1,reg_2,reg_3,reg_4,
          se=list(se_1,se_2,se_3,se_4),
          omit = c("state_name",
                   "cohort",
                   "read_write_woman",
                   "caste",
                   "religion",
                   "type_of_house",
                   "Constant"),
          order = c("^factor(birth_order)3:treat$",
                    "sex",
                    "^mothers_age_at_birth$",
                    "^total_child$"),
          covariate.labels = c("Sex of the child",
                               "Mother's age at birth",
                               "Total fertility measure",
                               "(Birth order=3 vs 1)",
                               "Post-treat",
                               "(Birth order=3 vs 1)*post-treat"))

stargazer(reg_5,reg_6,reg_7,reg_8,
          se=list(se_5,se_6,se_7,se_8),
          omit = c("state_name",
                   "cohort",
                   "read_write_woman",
                   "caste",
                   "religion",
                   "type_of_house",
                   "Constant"),
          order = c("^factor(birth_order)3:treat$",
                    "sex",
                    "^mothers_age_at_birth$",
                    "^total_child$"),
          covariate.labels = c("Sex of the child",
                               "Mother's age at birth",
                               "Total fertility measure",
                               "(Birth order=3 vs 1)",
                               "Post-treat",
                               "(Birth order=3 vs 1)*post-treat"))

########################################## Effect by sex #################################################


df_1<- read.csv("C:\\Users\\das90\\OneDrive - Cuny GradCenter\\Research\\Political reservation and age of marriage\\Github\\Political-reservation-and-age-of-marriage-age-of-first-birth\\Final version\\Cleaned datasets\\Preferred_sample_data_for_analysis_only_boys.csv")

df_2<- read.csv("C:\\Users\\das90\\OneDrive - Cuny GradCenter\\Research\\Political reservation and age of marriage\\Github\\Political-reservation-and-age-of-marriage-age-of-first-birth\\Final version\\Cleaned datasets\\Preferred_sample_data_for_analysis_only_girls.csv")

############################## Data cleaning for first and second order comparison for only boys ##########################

df_first_second_match_boys<- df_1%>%                               ##### Selecting the data for birth order 2 vs 1 analysis ###########
subset(birth_order==1|birth_order==2 & !is.na(alive))%>%
  group_by(id)%>%
  summarise(group_size=length(id))%>%
  subset(group_size==2)

df_first_second_total_boys<- df_1%>%
  subset(birth_order==1|birth_order==2)


df_first_second_boys<- merge(df_first_second_total_boys,df_first_second_match_boys,by="id")

df_first_second_boys<- df_first_second_boys[order(df_first_second_boys$id,df_first_second_boys$cohort),]

############################## Data cleaning for first and third order comparison for only boys ##########################

df_first_third_match_boys<- df_1%>%                                ##### Selecting the data for birth order 3 vs 1 analysis ###########
subset(birth_order==1|birth_order==3 & !is.na(alive))%>%
  group_by(id)%>%
  summarise(group_size=length(id))%>%
  subset(group_size>=2)

df_first_third_total_boys<- df_1%>%
  subset(birth_order==1|birth_order==3)

df_first_third_boys<- merge(df_first_third_total_boys,df_first_third_match_boys,by="id")

df_first_third_boys<- df_first_third_boys[order(df_first_third_boys$id,df_first_third_boys$cohort),]

############################## Data cleaning for second and third order comparison for only boys ##########################

df_second_third_match_boys<- df_1%>%                                ##### Selecting the data for birth order 3 vs 2 analysis ###########
subset(birth_order==2|birth_order==3 & !is.na(alive))%>%
  group_by(id)%>%
  summarise(group_size=length(id))%>%
  subset(group_size>=2)

df_second_third_total_boys<- df_1%>%
  subset(birth_order==2|birth_order==3)

df_second_third_boys<- merge(df_second_third_total_boys,df_second_third_match_boys,by="id")

df_second_third_boys<- df_second_third_boys[order(df_second_third_boys$id,df_second_third_boys$cohort),]


############################## Data cleaning for first and second order comparison for only girls ##########################

df_first_second_match_girls<- df_2%>%                               ##### Selecting the data for birth order 2 vs 1 analysis ###########
subset(birth_order==1|birth_order==2 & !is.na(alive))%>%
  group_by(id)%>%
  summarise(group_size=length(id))%>%
  subset(group_size==2)

df_first_second_total_girls<- df_2%>%
  subset(birth_order==1|birth_order==2)


df_first_second_girls<- merge(df_first_second_total_girls,df_first_second_match_girls,by="id")

df_first_second_girls<- df_first_second_boys[order(df_first_second_girls$id,df_first_second_girls$cohort),]

############################## Data cleaning for first and third order comparison for only girls ##########################

df_first_third_match_girls<- df_2%>%                                ##### Selecting the data for birth order 3 vs 1 analysis ###########
subset(birth_order==1|birth_order==3 & !is.na(alive))%>%
  group_by(id)%>%
  summarise(group_size=length(id))%>%
  subset(group_size>=2)

df_first_third_total_girls<- df_2%>%
  subset(birth_order==1|birth_order==3)

df_first_third_girls<- merge(df_first_third_total_girls,df_first_third_match_girls,by="id")

df_first_third_girls<- df_first_third_girls[order(df_first_third_girls$id,df_first_third_girls$cohort),]

############################## Data cleaning for second and third order comparison for only girls ##########################

df_second_third_match_girls<- df_2%>%                                ##### Selecting the data for birth order 3 vs 2 analysis ###########
subset(birth_order==2|birth_order==3 & !is.na(alive))%>%
  group_by(id)%>%
  summarise(group_size=length(id))%>%
  subset(group_size>=2)

df_second_third_total_girls<- df_2%>%
  subset(birth_order==2|birth_order==3)

df_second_third_girls<- merge(df_second_third_total_girls,df_second_third_match_girls,by="id")

df_second_third_girls<- df_second_third_girls[order(df_second_third_girls$id,df_second_third_girls$cohort),]

########################################### First vs second birth order comparison ##########################

reg_1<- lm(data = df_first_second_boys,
           formula = neonat~factor(birth_order)*treat+
             factor(state_name)+
             factor(cohort)+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_1<- vcovHC(reg_1, 
              type = "HC0", 
              cluster = "state_name")

se_1<- sqrt(diag(se_1))


reg_2<- lm(data = df_first_second_girls,
           formula = neonat~factor(birth_order)*treat+
             factor(state_name)+
             factor(cohort)+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_2<- vcovHC(reg_2, 
              type = "HC0", 
              cluster = "state_name")

se_2<- sqrt(diag(se_2))


reg_3<- lm(data = df_first_second_boys,
           formula = infant~factor(birth_order)*treat+
             factor(state_name)+
             factor(cohort)+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_3<- vcovHC(reg_3, 
              type = "HC0", 
              cluster = "state_name")

se_3<- sqrt(diag(se_3))


reg_4<- lm(data = df_first_second_girls,
           formula = infant~factor(birth_order)*treat+
             factor(state_name)+
             factor(cohort)+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_4<- vcovHC(reg_4, 
              type = "HC0", 
              cluster = "state_name")

se_4<- sqrt(diag(se_4))


reg_5<- lm(data = df_first_second_boys,
           formula = under5~factor(birth_order)*treat+
             factor(state_name)+
             factor(cohort)+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_5<- vcovHC(reg_5, 
              type = "HC0", 
              cluster = "state_name")

se_5<- sqrt(diag(se_5))


reg_6<- lm(data = df_first_second_girls,
           formula = under5~factor(birth_order)*treat+
             factor(state_name)+
             factor(cohort)+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_6<- vcovHC(reg_6, 
              type = "HC0", 
              cluster = "state_name")

se_6<- sqrt(diag(se_6))

reg_7<- lm(data = df_first_second_boys,
           formula = under10~factor(birth_order)*treat+
             factor(state_name)+
             factor(cohort)+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_7<- vcovHC(reg_7, 
              type = "HC0", 
              cluster = "state_name")

se_7<- sqrt(diag(se_7))


reg_8<- lm(data = df_first_second_girls,
           formula = under10~factor(birth_order)*treat+
             factor(state_name)+
             factor(cohort)+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_8<- vcovHC(reg_8, 
              type = "HC0", 
              cluster = "state_name")

se_8<- sqrt(diag(se_8))

stargazer(reg_1,reg_2,reg_3,reg_4,reg_5,reg_6,reg_7,reg_8,
          se=list(se_1,se_2,se_3,se_4,se_5,se_6,se_7,se_8),
          omit = c("state_name",
                   "cohort",
                   "read_write_woman",
                   "caste",
                   "religion",
                   "type_of_house",
                   "Constant"),
          order = c("^factor(birth_order)2:treat$",
                    "^mothers_age_at_birth$",
                    "^total_child$"),
          covariate.labels = c("Mother's age at birth",
                               "Total fertility measure",
                               "(Birth order=2 vs 1)",
                               "Post-treat",
                               "(Birth order=2 vs 1)*post-treat"))

########################################### First vs third birth order comparison ##########################

reg_1<- lm(data = df_first_third_boys,
           formula = neonat~factor(birth_order)*treat+
             factor(state_name)+
             factor(cohort)+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_1<- vcovHC(reg_1, 
              type = "HC0", 
              cluster = "state_name")

se_1<- sqrt(diag(se_1))


reg_2<- lm(data = df_first_third_girls,
           formula = neonat~factor(birth_order)*treat+
             factor(state_name)+
             factor(cohort)+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_2<- vcovHC(reg_2, 
              type = "HC0", 
              cluster = "state_name")

se_2<- sqrt(diag(se_2))


reg_3<- lm(data = df_first_third_boys,
           formula = infant~factor(birth_order)*treat+
             factor(state_name)+
             factor(cohort)+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_3<- vcovHC(reg_3, 
              type = "HC0", 
              cluster = "state_name")

se_3<- sqrt(diag(se_3))


reg_4<- lm(data = df_first_third_girls,
           formula = infant~factor(birth_order)*treat+
             factor(state_name)+
             factor(cohort)+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_4<- vcovHC(reg_4, 
              type = "HC0", 
              cluster = "state_name")

se_4<- sqrt(diag(se_4))


reg_5<- lm(data = df_first_third_boys,
           formula = under5~factor(birth_order)*treat+
             factor(state_name)+
             factor(cohort)+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_5<- vcovHC(reg_5, 
              type = "HC0", 
              cluster = "state_name")

se_5<- sqrt(diag(se_5))


reg_6<- lm(data = df_first_third_girls,
           formula = under5~factor(birth_order)*treat+
             factor(state_name)+
             factor(cohort)+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_6<- vcovHC(reg_6, 
              type = "HC0", 
              cluster = "state_name")

se_6<- sqrt(diag(se_6))

reg_7<- lm(data = df_first_third_boys,
           formula = under10~factor(birth_order)*treat+
             factor(state_name)+
             factor(cohort)+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_7<- vcovHC(reg_7, 
              type = "HC0", 
              cluster = "state_name")

se_7<- sqrt(diag(se_7))


reg_8<- lm(data = df_first_third_girls,
           formula = under10~factor(birth_order)*treat+
             factor(state_name)+
             factor(cohort)+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_8<- vcovHC(reg_8, 
              type = "HC0", 
              cluster = "state_name")

se_8<- sqrt(diag(se_8))

stargazer(reg_1,reg_2,reg_3,reg_4,
          se=list(se_1,se_2,se_3,se_4),
          omit = c("state_name",
                   "cohort",
                   "read_write_woman",
                   "caste",
                   "religion",
                   "type_of_house",
                   "Constant"),
          order = c("^factor(birth_order)3:treat$",
                    "^mothers_age_at_birth$",
                    "^total_child$"),
          covariate.labels = c("Mother's age at birth",
                               "Total fertility measure",
                               "(Birth order=3 vs 1)",
                               "Post-treat",
                               "(Birth order=3 vs 1)*post-treat"))


stargazer(reg_5,reg_6,reg_7,reg_8,
          se=list(se_5,se_6,se_7,se_8),
          omit = c("state_name",
                   "cohort",
                   "read_write_woman",
                   "caste",
                   "religion",
                   "type_of_house",
                   "Constant"),
          order = c("^factor(birth_order)3:treat$",
                    "^mothers_age_at_birth$",
                    "^total_child$"),
          covariate.labels = c("Mother's age at birth",
                               "Total fertility measure",
                               "(Birth order=3 vs 1)",
                               "Post-treat",
                               "(Birth order=3 vs 1)*post-treat"))


########################################### Second vs third birth order comparison ##########################

reg_1<- lm(data = df_second_third_boys,
           formula = neonat~factor(birth_order)*treat+
             factor(state_name)+
             factor(cohort)+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_1<- vcovHC(reg_1, 
              type = "HC0", 
              cluster = "state_name")

se_1<- sqrt(diag(se_1))


reg_2<- lm(data = df_second_third_girls,
           formula = neonat~factor(birth_order)*treat+
             factor(state_name)+
             factor(cohort)+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_2<- vcovHC(reg_2, 
              type = "HC0", 
              cluster = "state_name")

se_2<- sqrt(diag(se_2))


reg_3<- lm(data = df_second_third_boys,
           formula = infant~factor(birth_order)*treat+
             factor(state_name)+
             factor(cohort)+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_3<- vcovHC(reg_3, 
              type = "HC0", 
              cluster = "state_name")

se_3<- sqrt(diag(se_3))


reg_4<- lm(data = df_second_third_girls,
           formula = infant~factor(birth_order)*treat+
             factor(state_name)+
             factor(cohort)+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_4<- vcovHC(reg_4, 
              type = "HC0", 
              cluster = "state_name")

se_4<- sqrt(diag(se_4))


reg_5<- lm(data = df_second_third_boys,
           formula = under5~factor(birth_order)*treat+
             factor(state_name)+
             factor(cohort)+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_5<- vcovHC(reg_5, 
              type = "HC0", 
              cluster = "state_name")

se_5<- sqrt(diag(se_5))


reg_6<- lm(data = df_second_third_girls,
           formula = under5~factor(birth_order)*treat+
             factor(state_name)+
             factor(cohort)+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_6<- vcovHC(reg_6, 
              type = "HC0", 
              cluster = "state_name")

se_6<- sqrt(diag(se_6))

reg_7<- lm(data = df_second_third_boys,
           formula = under10~factor(birth_order)*treat+
             factor(state_name)+
             factor(cohort)+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_7<- vcovHC(reg_7, 
              type = "HC0", 
              cluster = "state_name")

se_7<- sqrt(diag(se_7))


reg_8<- lm(data = df_second_third_girls,
           formula = under10~factor(birth_order)*treat+
             factor(state_name)+
             factor(cohort)+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_8<- vcovHC(reg_8, 
              type = "HC0", 
              cluster = "state_name")

se_8<- sqrt(diag(se_8))

stargazer(reg_1,reg_2,reg_3,reg_4,reg_5,reg_6,reg_7,reg_8,
          se=list(se_1,se_2,se_3,se_4,se_5,se_6,se_7,se_8),
          omit = c("state_name",
                   "cohort",
                   "read_write_woman",
                   "caste",
                   "religion",
                   "type_of_house",
                   "Constant"),
          order = c("^factor(birth_order)3:treat$",
                    "^mothers_age_at_birth$",
                    "^total_child$"),
          covariate.labels = c("Mother's age at birth",
                               "Total fertility measure",
                               "(Birth order=3 vs 2)",
                               "Post-treat",
                               "(Birth order=3 vs 2)*post-treat"))

############################################################ When first born is a boy ######################################################################

df_first_second_boys_boys<- df_first_second_boys%>%
  subset(first_born_boy==1)

df_first_second_boys_girls<- df_first_second_girls%>%
  subset(first_born_boy==1)

df_first_third_boys_boys<- df_first_third_boys%>%
  subset(first_born_boy==1)

df_first_third_boys_girls<- df_first_third_girls%>%
  subset(first_born_boy==1)

df_second_third_boys_boys<- df_second_third_boys%>%
  subset(first_born_boy==1)

df_second_third_boys_girls<- df_second_third_girls%>%
  subset(first_born_boy==1)

########################################### First vs second birth order comparison ##########################

reg_1<- lm(data = df_first_second_boys_boys,
           formula = neonat~factor(birth_order)*treat+
             factor(state_name)+
             factor(cohort)+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_1<- vcovHC(reg_1, 
              type = "HC0", 
              cluster = "state_name")

se_1<- sqrt(diag(se_1))


reg_2<- lm(data = df_first_second_boys_girls,
           formula = neonat~factor(birth_order)*treat+
             factor(state_name)+
             factor(cohort)+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_2<- vcovHC(reg_2, 
              type = "HC0", 
              cluster = "state_name")

se_2<- sqrt(diag(se_2))


reg_3<- lm(data = df_first_second_boys_boys,
           formula = infant~factor(birth_order)*treat+
             factor(state_name)+
             factor(cohort)+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_3<- vcovHC(reg_3, 
              type = "HC0", 
              cluster = "state_name")

se_3<- sqrt(diag(se_3))


reg_4<- lm(data = df_first_second_boys_girls,
           formula = infant~factor(birth_order)*treat+
             factor(state_name)+
             factor(cohort)+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_4<- vcovHC(reg_4, 
              type = "HC0", 
              cluster = "state_name")

se_4<- sqrt(diag(se_4))


reg_5<- lm(data = df_first_second_boys_boys,
           formula = under5~factor(birth_order)*treat+
             factor(state_name)+
             factor(cohort)+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_5<- vcovHC(reg_5, 
              type = "HC0", 
              cluster = "state_name")

se_5<- sqrt(diag(se_5))


reg_6<- lm(data = df_first_second_boys_girls,
           formula = under5~factor(birth_order)*treat+
             factor(state_name)+
             factor(cohort)+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_6<- vcovHC(reg_6, 
              type = "HC0", 
              cluster = "state_name")

se_6<- sqrt(diag(se_6))

reg_7<- lm(data = df_first_second_boys_boys,
           formula = under10~factor(birth_order)*treat+
             factor(state_name)+
             factor(cohort)+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_7<- vcovHC(reg_7, 
              type = "HC0", 
              cluster = "state_name")

se_7<- sqrt(diag(se_7))


reg_8<- lm(data = df_first_second_boys_girls,
           formula = under10~factor(birth_order)*treat+
             factor(state_name)+
             factor(cohort)+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_8<- vcovHC(reg_8, 
              type = "HC0", 
              cluster = "state_name")

se_8<- sqrt(diag(se_8))

stargazer(reg_1,reg_2,reg_3,reg_4,reg_5,reg_6,reg_7,reg_8,
          se=list(se_1,se_2,se_3,se_4,se_5,se_6,se_7,se_8),
          omit = c("state_name",
                   "cohort",
                   "read_write_woman",
                   "caste",
                   "religion",
                   "type_of_house",
                   "Constant"),
          order = c("^factor(birth_order)2:treat$",
                    "^mothers_age_at_birth$",
                    "^total_child$"),
          covariate.labels = c("Mother's age at birth",
                               "Total fertility measure",
                               "(Birth order=2 vs 1)",
                               "Post-treat",
                               "(Birth order=2 vs 1)*post-treat"))

########################################### First vs third birth order comparison ##########################

reg_1<- lm(data = df_first_third_boys_boys,
           formula = neonat~factor(birth_order)*treat+
             factor(state_name)+
             factor(cohort)+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_1<- vcovHC(reg_1, 
              type = "HC0", 
              cluster = "state_name")

se_1<- sqrt(diag(se_1))


reg_2<- lm(data = df_first_third_boys_girls,
           formula = neonat~factor(birth_order)*treat+
             factor(state_name)+
             factor(cohort)+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_2<- vcovHC(reg_2, 
              type = "HC0", 
              cluster = "state_name")

se_2<- sqrt(diag(se_2))


reg_3<- lm(data = df_first_third_boys_boys,
           formula = infant~factor(birth_order)*treat+
             factor(state_name)+
             factor(cohort)+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_3<- vcovHC(reg_3, 
              type = "HC0", 
              cluster = "state_name")

se_3<- sqrt(diag(se_3))


reg_4<- lm(data = df_first_third_boys_girls,
           formula = infant~factor(birth_order)*treat+
             factor(state_name)+
             factor(cohort)+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_4<- vcovHC(reg_4, 
              type = "HC0", 
              cluster = "state_name")

se_4<- sqrt(diag(se_4))


reg_5<- lm(data = df_first_third_boys_boys,
           formula = under5~factor(birth_order)*treat+
             factor(state_name)+
             factor(cohort)+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_5<- vcovHC(reg_5, 
              type = "HC0", 
              cluster = "state_name")

se_5<- sqrt(diag(se_5))


reg_6<- lm(data = df_first_third_boys_girls,
           formula = under5~factor(birth_order)*treat+
             factor(state_name)+
             factor(cohort)+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_6<- vcovHC(reg_6, 
              type = "HC0", 
              cluster = "state_name")

se_6<- sqrt(diag(se_6))

reg_7<- lm(data = df_first_third_boys_boys,
           formula = under10~factor(birth_order)*treat+
             factor(state_name)+
             factor(cohort)+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_7<- vcovHC(reg_7, 
              type = "HC0", 
              cluster = "state_name")

se_7<- sqrt(diag(se_7))


reg_8<- lm(data = df_first_third_boys_girls,
           formula = under10~factor(birth_order)*treat+
             factor(state_name)+
             factor(cohort)+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_8<- vcovHC(reg_8, 
              type = "HC0", 
              cluster = "state_name")

se_8<- sqrt(diag(se_8))

stargazer(reg_1,reg_2,reg_3,reg_4,reg_5,reg_6,reg_7,reg_8,
          se=list(se_1,se_2,se_3,se_4,se_5,se_6,se_7,se_8),
          omit = c("state_name",
                   "cohort",
                   "read_write_woman",
                   "caste",
                   "religion",
                   "type_of_house",
                   "Constant"),
          order = c("^factor(birth_order)3:treat$",
                    "^mothers_age_at_birth$",
                    "^total_child$"),
          covariate.labels = c("Mother's age at birth",
                               "Total fertility measure",
                               "(Birth order=3 vs 1)",
                               "Post-treat",
                               "(Birth order=3 vs 1)*post-treat"))


########################################### Second vs third birth order comparison ##########################

reg_1<- lm(data = df_second_third_boys_boys,
           formula = neonat~factor(birth_order)*treat+
             factor(state_name)+
             factor(cohort)+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_1<- vcovHC(reg_1, 
              type = "HC0", 
              cluster = "state_name")

se_1<- sqrt(diag(se_1))


reg_2<- lm(data = df_second_third_boys_girls,
           formula = neonat~factor(birth_order)*treat+
             factor(state_name)+
             factor(cohort)+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_2<- vcovHC(reg_2, 
              type = "HC0", 
              cluster = "state_name")

se_2<- sqrt(diag(se_2))


reg_3<- lm(data = df_second_third_boys_boys,
           formula = infant~factor(birth_order)*treat+
             factor(state_name)+
             factor(cohort)+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_3<- vcovHC(reg_3, 
              type = "HC0", 
              cluster = "state_name")

se_3<- sqrt(diag(se_3))


reg_4<- lm(data = df_second_third_boys_girls,
           formula = infant~factor(birth_order)*treat+
             factor(state_name)+
             factor(cohort)+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_4<- vcovHC(reg_4, 
              type = "HC0", 
              cluster = "state_name")

se_4<- sqrt(diag(se_4))


reg_5<- lm(data = df_second_third_boys_boys,
           formula = under5~factor(birth_order)*treat+
             factor(state_name)+
             factor(cohort)+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_5<- vcovHC(reg_5, 
              type = "HC0", 
              cluster = "state_name")

se_5<- sqrt(diag(se_5))


reg_6<- lm(data = df_second_third_boys_girls,
           formula = under5~factor(birth_order)*treat+
             factor(state_name)+
             factor(cohort)+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_6<- vcovHC(reg_6, 
              type = "HC0", 
              cluster = "state_name")

se_6<- sqrt(diag(se_6))

reg_7<- lm(data = df_second_third_boys_boys,
           formula = under10~factor(birth_order)*treat+
             factor(state_name)+
             factor(cohort)+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_7<- vcovHC(reg_7, 
              type = "HC0", 
              cluster = "state_name")

se_7<- sqrt(diag(se_7))


reg_8<- lm(data = df_second_third_boys_girls,
           formula = under10~factor(birth_order)*treat+
             factor(state_name)+
             factor(cohort)+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_8<- vcovHC(reg_8, 
              type = "HC0", 
              cluster = "state_name")

se_8<- sqrt(diag(se_8))

stargazer(reg_1,reg_2,reg_3,reg_4,reg_5,reg_6,reg_7,reg_8,
          se=list(se_1,se_2,se_3,se_4,se_5,se_6,se_7,se_8),
          omit = c("state_name",
                   "cohort",
                   "read_write_woman",
                   "caste",
                   "religion",
                   "type_of_house",
                   "Constant"),
          order = c("^factor(birth_order)2:treat$",
                    "^mothers_age_at_birth$",
                    "^total_child$"),
          covariate.labels = c("Mother's age at birth",
                               "Total fertility measure",
                               "(Birth order=2 vs 1)",
                               "Post-treat",
                               "(Birth order=2 vs 1)*post-treat"))


################################################### District reservations ############################################################

memory.limit(size = 40000)

df<- read.csv("C:\\Users\\das90\\OneDrive - Cuny GradCenter\\Research\\Political reservation and age of marriage\\Github\\Political-reservation-and-age-of-marriage-age-of-first-birth\\Final version\\Cleaned datasets\\Preferred_sample_data_for_dist_level_analysis.csv")

############################## Data cleaning for first and second order comparison ##########################

df_first_second_match<- df%>%
  subset(birth_order==1|birth_order==2 & !is.na(alive))%>%
  group_by(id)%>%
  summarise(group_size=length(id))%>%
  subset(group_size==2)

df_first_second_total<- df%>%
  subset(birth_order==1|birth_order==2)


df_first_second<- merge(df_first_second_total,df_first_second_match,by="id")

df_first_second<- df_first_second[order(df_first_second$id,df_first_second$cohort),]


############################## Data cleaning for first and third order comparison ##########################

df_first_third_match<- df%>%
  subset(birth_order==1|birth_order==3 & !is.na(alive))%>%
  group_by(id)%>%
  summarise(group_size=length(id))%>%
  subset(group_size>=2)

df_first_third_total<- df%>%
  subset(birth_order==1|birth_order==3)

df_first_third<- merge(df_first_third_total,df_first_third_match,by="id")

df_first_third<- df_first_third[order(df_first_third$id,df_first_third$cohort),]

############################## Data cleaning for second and third order comparison ##########################

df_second_third_match<- df%>%
  subset(birth_order==2|birth_order==3 & !is.na(alive))%>%
  group_by(id)%>%
  summarise(group_size=length(id))%>%
  subset(group_size>=2)

df_second_third_total<- df%>%
  subset(birth_order==2|birth_order==3)

df_second_third<- merge(df_second_third_total,df_second_third_match,by="id")

df_second_third<- df_second_third[order(df_second_third$id,df_second_third$cohort),]


################################ First vs second #####################################

################ Neonatal ######################

reg_1<- lm(data = df_first_second,
           formula = neonat~factor(birth_order)*treat*dist_resv+
             factor(state_name)+
             factor(district_name)+
             factor(cohort)+
             sex+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_1<- vcovHC(reg_1, 
              type = "HC0", 
              cluster = "district_name")

se_1<- sqrt(diag(se_1))

################ Infant ######################

reg_2<- lm(data = df_first_second,
           formula = infant~factor(birth_order)*treat*dist_resv+
             factor(state_name)+
             factor(district_name)+
             factor(cohort)+
             sex+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_2<- vcovHC(reg_2, 
              type = "HC0", 
              cluster = "district_name")

se_2<- sqrt(diag(se_2))

################ Under 5 ######################

reg_3<- lm(data = df_first_second,
           formula = under5~factor(birth_order)*treat*dist_resv+
             factor(state_name)+
             factor(district_name)+
             factor(cohort)+
             sex+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_3<- vcovHC(reg_3, 
              type = "HC0", 
              cluster = "district_name")

se_3<- sqrt(diag(se_3))

################ Under 10 ######################

reg_4<- lm(data = df_first_second,
           formula = under10~factor(birth_order)*treat*dist_resv+
             factor(state_name)+
             factor(district_name)+
             factor(cohort)+
             sex+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_4<- vcovHC(reg_4, 
              type = "HC0", 
              cluster = "district_name")

se_4<- sqrt(diag(se_4))

###################### First vs third ##############################

####################### Neonatal ###########################

reg_5<- lm(data = df_first_third,
           formula = neonat~factor(birth_order)*treat*dist_resv+
             factor(state_name)+
             factor(district_name)+
             factor(cohort)+
             sex+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_5<- vcovHC(reg_5, 
              type = "HC0", 
              cluster = "district_name")

se_5<- sqrt(diag(se_5))

################## Infant ###############################

reg_6<- lm(data = df_first_third,
           formula = infant~factor(birth_order)*treat*dist_resv+
             factor(state_name)+
             factor(district_name)+
             factor(cohort)+
             sex+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_6<- vcovHC(reg_6, 
              type = "HC0", 
              cluster = "district_name")

se_6<- sqrt(diag(se_6))

################## Under 5 ###############################

reg_7<- lm(data = df_first_third,
           formula = under5~factor(birth_order)*treat*dist_resv+
             factor(state_name)+
             factor(district_name)+
             factor(cohort)+
             sex+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_7<- vcovHC(reg_7, 
              type = "HC0", 
              cluster = "district_name")

se_7<- sqrt(diag(se_7))

################## Under 10 ###############################

reg_8<- lm(data = df_first_third,
           formula = under10~factor(birth_order)*treat*dist_resv+
             factor(state_name)+
             factor(district_name)+
             factor(cohort)+
             sex+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_8<- vcovHC(reg_8, 
              type = "HC0", 
              cluster = "district_name")

se_8<- sqrt(diag(se_8))

############################ Second vs Third ############################

############################## Neonatal #########################

reg_9<- lm(data = df_second_third,
           formula = neonat~factor(birth_order)*treat*dist_resv+
             factor(state_name)+
             factor(district_name)+
             factor(cohort)+
             sex+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

se_9<- vcovHC(reg_9, 
              type = "HC0", 
              cluster = "district_name")

se_9<- sqrt(diag(se_9))

############################## Infant #########################

reg_10<- lm(data = df_second_third,
            formula = infant~factor(birth_order)*treat*dist_resv+
              factor(state_name)+
              factor(district_name)+
              factor(cohort)+
              sex+
              mothers_age_at_birth+
              total_child+
              read_write_woman+
              religion+
              caste+
              type_of_house) 

se_10<- vcovHC(reg_10, 
               type = "HC0", 
               cluster = "district_name")

se_10<- sqrt(diag(se_10))

############################## Under 5 #########################

reg_11<- lm(data = df_second_third,
            formula = under5~factor(birth_order)*treat*dist_resv+
              factor(state_name)+
              factor(district_name)+
              factor(cohort)+
              sex+
              mothers_age_at_birth+
              total_child+
              read_write_woman+
              religion+
              caste+
              type_of_house) 

se_11<- vcovHC(reg_11, 
               type = "HC0", 
               cluster = "district_name")

se_11<- sqrt(diag(se_11))

############################## Under 10 #########################

reg_12<- lm(data = df_second_third,
            formula = under10~factor(birth_order)*treat*dist_resv+
              factor(state_name)+
              factor(district_name)+
              factor(cohort)+
              sex+
              mothers_age_at_birth+
              total_child+
              read_write_woman+
              religion+
              caste+
              type_of_house) 

se_12<- vcovHC(reg_12, 
               type = "HC0", 
               cluster = "district_name")

se_12<- sqrt(diag(se_12))

stargazer(reg_1,reg_2,reg_3,reg_4,reg_5,reg_6,reg_7,reg_8,reg_9,reg_10,reg_11,reg_12,
          se=list(se_1,se_2,se_3,se_4,se_5,se_6,se_7,se_8,se_9,se_10,se_11,se_12),
          omit = c("state_name",
                   "district_name",
                   "cohort",
                   "read_write_woman",
                   "caste",
                   "religion",
                   "type_of_house",
                   "Constant"),
          order = c("^factor(birth_order)2:dist_resv:treat$",
                    "^factor(birth_order)3:dist_resv:treat$",
                    "^sex$",
                    "^mothers_age_at_birth$",
                    "^total_child$"),
          covariate.labels = c("Sex of child",
                               "Mother's age at birth",
                               "Total fertility measure",
                               "(Birth order=2 vs 1)",
                               "(Birth order=3 vs 1)",
                               "Post-treat",
                               "DR",
                               "(Birth order=2 vs 1)*post-treat",
                               "(Birth order=2 vs 1)*DR",
                               "(Birth order=3 vs 1)*post-treat",
                               "(Birth order=3 vs 1)*DR",
                               "Post-treat*DR",
                               "(Birth order=2 vs 1)*post-treat*DR",
                               "(Birth order=3 vs 1)*post-treat*DR"))


stargazer(reg_5,reg_6,reg_7,reg_8,
          se=list(se_5,se_6,se_7,se_8),
          omit = c("state_name",
                   "district_name",
                   "cohort",
                   "read_write_woman",
                   "caste",
                   "religion",
                   "type_of_house",
                   "Constant"),
          order = c("^factor(birth_order)3:dist_resv:treat$",
                    "^sex$",
                    "^mothers_age_at_birth$",
                    "^total_child$"),
          covariate.labels = c("Sex of child",
                               "Mother's age at birth",
                               "Total fertility measure",
                               "(Birth order=3 vs 1)",
                               "Post-treat",
                               "DR",
                               "(Birth order=3 vs 1)*post-treat",
                               "(Birth order=3 vs 1)*DR",
                               "Post-treat*DR",
                               "(Birth order=3 vs 1)*post-treat*DR"))
