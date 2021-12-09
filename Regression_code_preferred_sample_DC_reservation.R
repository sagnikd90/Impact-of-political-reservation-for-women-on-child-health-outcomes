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
library(rsq)

memory.limit(size = 40000)

setwd("C:\\Users\\das90\\OneDrive - Cuny GradCenter\\Research\\Political reservation and age of marriage\\Github\\Political-reservation-and-age-of-marriage-age-of-first-birth\\Final version\\R results\\Regression_output")

df<- read.csv("C:\\Users\\das90\\OneDrive - Cuny GradCenter\\Research\\Political reservation and age of marriage\\Github\\Political-reservation-and-age-of-marriage-age-of-first-birth\\Final version\\Cleaned datasets\\Preferred_sample_data_for_dist_level_analysis.csv")


############################## Data cleaning for first and second order comparison ##########################

df_first_second<- df%>%                               ##### Selecting the data for birth order 2 vs 1 analysis ###########
subset(birth_order==1|birth_order==2 & !is.na(alive))

n_first_second<- data.frame(table(df_first_second$id))

n_first_second[n_first_second$Freq>1,]

df_first_second<-df_first_second[df_first_second$id %in% n_first_second$Var1[n_first_second$Freq > 1],]

df_first_second<- df_first_second[order(df_first_second$id,df_first_second$cohort),]

############################## Data cleaning for first and third order comparison ##########################

df_first_third<- df%>%                               ##### Selecting the data for birth order 3 vs 1 analysis ###########
subset(birth_order==1|birth_order==3 & !is.na(alive))

n_first_third<- data.frame(table(df_first_third$id))

n_first_third[n_first_third$Freq>1,]

df_first_third<-df_first_third[df_first_third$id %in% n_first_third$Var1[n_first_third$Freq > 1],]

df_first_third<- df_first_third[order(df_first_third$id,df_first_third$cohort),]

############################## Data cleaning for second and third order comparison ##########################

df_second_third<- df%>%                               ##### Selecting the data for birth order 3 vs 1 analysis ###########
subset(birth_order==2|birth_order==3 & !is.na(alive))

n_second_third<- data.frame(table(df_second_third$id))

n_second_third[n_second_third$Freq>1,]

df_second_third<-df_second_third[df_second_third$id %in% n_second_third$Var1[n_second_third$Freq > 1],]

df_second_third<- df_second_third[order(df_second_third$id,df_second_third$cohort),]

######################################################################################################################

################################ First vs second #####################################

################ Neonatal ######################

reg_1<- lm(data = df_first_second,
           formula = neonat~factor(birth_order)*treat*dist_resv+
             factor(state_name)+
             factor(district_name)+
             factor(cohort)+
             sex+
             mothers_age_at_birth+
             wait_time+
             read_write_woman+
             religion+
             caste+
             type_of_house+
             state_name:seq_along((cohort)^2)) 

se_1<- coeftest(reg_1,
                vcov = vcovHC(reg_1,
                              type = "HC0",
                              cluster = "district_name"))

rsq_1<- round(rsq(reg_1),digits = 3)

################ Infant ######################

reg_2<- lm(data = df_first_second,
           formula = infant~factor(birth_order)*treat*dist_resv+
             factor(state_name)+
             factor(district_name)+
             factor(cohort)+
             sex+
             mothers_age_at_birth+
             wait_time+
             read_write_woman+
             religion+
             caste+
             type_of_house+
             state_name:seq_along((cohort)^2)) 

se_2<- coeftest(reg_2,
                vcov = vcovHC(reg_2,
                              type = "HC0",
                              cluster = "district_name"))

rsq_2<- round(rsq(reg_2),digits = 3)

################ Under 5 ######################

reg_3<- lm(data = df_first_second,
           formula = under5~factor(birth_order)*treat*dist_resv+
             factor(state_name)+
             factor(district_name)+
             factor(cohort)+
             sex+
             mothers_age_at_birth+
             wait_time+
             read_write_woman+
             religion+
             caste+
             type_of_house+
             state_name:seq_along((cohort)^2)) 

se_3<- coeftest(reg_3,
                vcov = vcovHC(reg_3,
                              type = "HC0",
                              cluster = "district_name"))

rsq_3<- round(rsq(reg_3),digits = 3)

################ Under 10 ######################

reg_4<- lm(data = df_first_second,
           formula = under10~factor(birth_order)*treat*dist_resv+
             factor(state_name)+
             factor(district_name)+
             factor(cohort)+
             sex+
             mothers_age_at_birth+
             wait_time+
             read_write_woman+
             religion+
             caste+
             type_of_house+
             state_name:seq_along((cohort)^2)) 

se_4<- coeftest(reg_4,
                vcov = vcovHC(reg_4,
                              type = "HC0",
                              cluster = "district_name"))

rsq_4<- round(rsq(reg_4),digits = 3)

###################### First vs third ##############################

####################### Neonatal ###########################

reg_5<- lm(data = df_first_third,
           formula = neonat~factor(birth_order)*treat*dist_resv+
             factor(state_name)+
             factor(district_name)+
             factor(cohort)+
             sex+
             mothers_age_at_birth+
             wait_time+
             read_write_woman+
             religion+
             caste+
             type_of_house+
             state_name:seq_along((cohort)^2)) 

se_5<- coeftest(reg_5,
                vcov = vcovHC(reg_5,
                              type = "HC0",
                              cluster = "district_name"))

rsq_5<- round(rsq(reg_5),digits = 3)

################## Infant ###############################

reg_6<- lm(data = df_first_third,
           formula = infant~factor(birth_order)*treat*dist_resv+
             factor(state_name)+
             factor(district_name)+
             factor(cohort)+
             sex+
             mothers_age_at_birth+
             wait_time+
             read_write_woman+
             religion+
             caste+
             type_of_house+
             state_name:seq_along((cohort)^2)) 

se_6<- coeftest(reg_6,
                vcov = vcovHC(reg_6,
                              type = "HC0",
                              cluster = "district_name"))

rsq_6<- round(rsq(reg_6),digits = 3)

################## Under 5 ###############################

reg_7<- lm(data = df_first_third,
           formula = under5~factor(birth_order)*treat*dist_resv+
             factor(state_name)+
             factor(district_name)+
             factor(cohort)+
             sex+
             mothers_age_at_birth+
             wait_time+
             read_write_woman+
             religion+
             caste+
             type_of_house+
             state_name:seq_along((cohort)^2)) 

se_7<- coeftest(reg_7,
                vcov = vcovHC(reg_7,
                              type = "HC0",
                              cluster = "district_name"))

rsq_7<- round(rsq(reg_7),digits = 3)

################## Under 10 ###############################

reg_8<- lm(data = df_first_third,
           formula = under10~factor(birth_order)*treat*dist_resv+
             factor(state_name)+
             factor(district_name)+
             factor(cohort)+
             sex+
             mothers_age_at_birth+
             wait_time+
             read_write_woman+
             religion+
             caste+
             type_of_house+
             state_name:seq_along((cohort)^2)) 

se_8<- coeftest(reg_8,
                vcov = vcovHC(reg_8,
                              type = "HC0",
                              cluster = "district_name"))

rsq_8<- round(rsq(reg_8),digits = 3)

############################ Second vs Third ############################

############################## Neonatal #########################

reg_9<- lm(data = df_second_third,
           formula = neonat~factor(birth_order)*treat*dist_resv+
             factor(state_name)+
             factor(district_name)+
             factor(cohort)+
             sex+
             mothers_age_at_birth+
             wait_time+
             read_write_woman+
             religion+
             caste+
             type_of_house+
             state_name:seq_along((cohort)^2)) 

se_9<- coeftest(reg_9,
                vcov = vcovHC(reg_9,
                              type = "HC0",
                              cluster = "district_name"))

rsq_9<- round(rsq(reg_9),digits = 3)

############################## Infant #########################

reg_10<- lm(data = df_second_third,
           formula = infant~factor(birth_order)*treat*dist_resv+
             factor(state_name)+
             factor(district_name)+
             factor(cohort)+
             sex+
             mothers_age_at_birth+
             wait_time+
             read_write_woman+
             religion+
             caste+
             type_of_house+
             state_name:seq_along((cohort)^2)) 

se_10<- coeftest(reg_10,
                vcov = vcovHC(reg_10,
                              type = "HC0",
                              cluster = "district_name"))

rsq_10<- round(rsq(reg_10),digits = 3)

############################## Under 5 #########################

reg_11<- lm(data = df_second_third,
            formula = under5~factor(birth_order)*treat*dist_resv+
              factor(state_name)+
              factor(district_name)+
              factor(cohort)+
              sex+
              mothers_age_at_birth+
              wait_time+
              read_write_woman+
              religion+
              caste+
              type_of_house+
              state_name:seq_along((cohort)^2)) 

se_11<- coeftest(reg_11,
                vcov = vcovHC(reg_11,
                              type = "HC0",
                              cluster = "district_name"))

rsq_11<- round(rsq(reg_11),digits = 3)


############################## Under 10 #########################

reg_12<- lm(data = df_second_third,
            formula = under10~factor(birth_order)*treat*dist_resv+
              factor(state_name)+
              factor(district_name)+
              factor(cohort)+
              sex+
              mothers_age_at_birth+
              wait_time+
              read_write_woman+
              religion+
              caste+
              type_of_house+
              state_name:seq_along((cohort)^2)) 

se_12<- coeftest(reg_12,
                vcov = vcovHC(reg_12,
                              type = "HC0",
                              cluster = "district_name"))

rsq_12<- round(rsq(reg_12),digits = 3)

stargazer(se_1,se_2,se_3,se_4,se_5,se_6,se_7,se_8,se_9,se_10,se_11,se_12,
          omit = c("state_name",
                   "district_name",
                   "cohort",
                   "caste",
                   "religion",
                   "read_write_woman",
                   "type_of_house",
                   "Constant"),
          order = c("^factor(birth_order)2:dist_resv:treat$",
                    "^factor(birth_order)3:dist_resv:treat$",
                    "^treat$",
                    "^sex$",
                    "^mothers_age_at_birth$",
                    "^wait_time$"),
          covariate.labels = c("Post treat",
                               "Child is female",
                               "Mother's age at birth",
                               "Gap between births",
                               "(Birth order=2 vs 1)",
                               "(Birth order=3 vs 1)",
                               "DC resv",
                               "(Birth order=2 vs 1)*post-treat",
                               "(Birth order=2 vs 1)*DC resv",
                               "(Birth order=3 vs 1)*post-treat",
                               "(Birth order=3 vs 1)*DC resv",
                               "Post-treat*DC resv",
                               "(Birth order=2 vs 1)*post-treat*DC resv",
                               "(Birth order=3 vs 1)*post-treat*DC resv"),
          add.lines = list(c("State quadratic trend","x","x","x","x","x","x","x","x","x","x","x","x"),
                           c("Individual controls","x","x","x","x","x","x","x","x","x","x","x","x"),
                           c("State FE","x","x","x","x","x","x","x","x","x","x","x","x"),
                           c("District FE","x","x","x","x","x","x","x","x","x","x","x","x"),
                           c("Cohort FE","x","x","x","x","x","x","x","x","x","x","x","x"),
                           c("R-sq",rsq_1,rsq_2,rsq_3,rsq_4,rsq_5,rsq_6,rsq_7,rsq_8,rsq_9,rsq_10,rsq_11,rsq_12)),
          omit.stat = c("adj.rsq"),
          no.space = TRUE,
          out = "Dist_regression_all.tex")

#################################### Survival probability when first born is a boy ###################################

df_first_second_boys<- df_first_second%>%
  subset(first_born_boy==1)

df_first_third_boys<- df_first_third%>%
  subset(first_born_boy==1)

df_second_third_boys<- df_second_third%>%
  subset(first_born_boy==1)

################ Neonatal ######################

reg_1<- lm(data = df_first_second_boys,
           formula = neonat~factor(birth_order)*treat*dist_resv+
             factor(state_name)+
             factor(district_name)+
             factor(cohort)+
             sex+
             mothers_age_at_birth+
             wait_time+
             read_write_woman+
             religion+
             caste+
             type_of_house+
             state_name:seq_along((cohort)^2)) 

se_1<- coeftest(reg_1,
                vcov = vcovHC(reg_1,
                              type = "HC0",
                              cluster = "district_name"))

rsq_1<- round(rsq(reg_1),digits = 3)

################ Infant ######################

reg_2<- lm(data = df_first_second_boys,
           formula = infant~factor(birth_order)*treat*dist_resv+
             factor(state_name)+
             factor(district_name)+
             factor(cohort)+
             sex+
             mothers_age_at_birth+
             wait_time+
             read_write_woman+
             religion+
             caste+
             type_of_house+
             state_name:seq_along((cohort)^2)) 

se_2<- coeftest(reg_2,
                vcov = vcovHC(reg_1,
                              type = "HC0",
                              cluster = "district_name"))

rsq_2<- round(rsq(reg_2),digits = 3)

################ Under 5 ######################

reg_3<- lm(data = df_first_second_boys,
           formula = under5~factor(birth_order)*treat*dist_resv+
             factor(state_name)+
             factor(district_name)+
             factor(cohort)+
             sex+
             mothers_age_at_birth+
             wait_time+
             read_write_woman+
             religion+
             caste+
             type_of_house+
             state_name:seq_along((cohort)^2)) 

se_3<- coeftest(reg_3,
                vcov = vcovHC(reg_3,
                              type = "HC0",
                              cluster = "district_name"))

rsq_3<- round(rsq(reg_3),digits = 3)

################ Under 10 ######################

reg_4<- lm(data = df_first_second_boys,
           formula = under10~factor(birth_order)*treat*dist_resv+
             factor(state_name)+
             factor(district_name)+
             factor(cohort)+
             sex+
             mothers_age_at_birth+
             wait_time+
             read_write_woman+
             religion+
             caste+
             type_of_house+
             state_name:seq_along((cohort)^2)) 

se_4<- coeftest(reg_4,
                vcov = vcovHC(reg_4,
                              type = "HC0",
                              cluster = "district_name"))

rsq_4<- round(rsq(reg_4),digits = 3)

###################### First vs third ##############################

####################### Neonatal ###########################

reg_5<- lm(data = df_first_third_boys,
           formula = neonat~factor(birth_order)*treat*dist_resv+
             factor(state_name)+
             factor(district_name)+
             factor(cohort)+
             sex+
             mothers_age_at_birth+
             wait_time+
             read_write_woman+
             religion+
             caste+
             type_of_house+
             state_name:seq_along((cohort)^2)) 

se_5<- coeftest(reg_5,
                vcov = vcovHC(reg_5,
                              type = "HC0",
                              cluster = "district_name"))

rsq_5<- round(rsq(reg_5),digits = 3)

################## Infant ###############################

reg_6<- lm(data = df_first_third_boys,
           formula = infant~factor(birth_order)*treat*dist_resv+
             factor(state_name)+
             factor(district_name)+
             factor(cohort)+
             sex+
             mothers_age_at_birth+
             wait_time+
             read_write_woman+
             religion+
             caste+
             type_of_house+
             state_name:seq_along((cohort)^2)) 

se_6<- coeftest(reg_6,
                vcov = vcovHC(reg_6,
                              type = "HC0",
                              cluster = "district_name"))

rsq_6<- round(rsq(reg_6),digits = 3)

################## Under 5 ###############################

reg_7<- lm(data = df_first_third_boys,
           formula = under5~factor(birth_order)*treat*dist_resv+
             factor(state_name)+
             factor(district_name)+
             factor(cohort)+
             sex+
             mothers_age_at_birth+
             wait_time+
             read_write_woman+
             religion+
             caste+
             type_of_house+
             state_name:seq_along((cohort)^2)) 

se_7<- coeftest(reg_7,
                vcov = vcovHC(reg_7,
                              type = "HC0",
                              cluster = "district_name"))

rsq_7<- round(rsq(reg_7),digits = 3)

################## Under 10 ###############################

reg_8<- lm(data = df_first_third_boys,
           formula = under10~factor(birth_order)*treat*dist_resv+
             factor(state_name)+
             factor(district_name)+
             factor(cohort)+
             sex+
             mothers_age_at_birth+
             wait_time+
             read_write_woman+
             religion+
             caste+
             type_of_house+
             state_name:seq_along((cohort)^2)) 

se_8<- coeftest(reg_8,
                vcov = vcovHC(reg_8,
                              type = "HC0",
                              cluster = "district_name"))

rsq_8<- round(rsq(reg_8),digits = 3)

############################ Second vs Third ############################

############################## Neonatal #########################

reg_9<- lm(data = df_second_third_boys,
           formula = neonat~factor(birth_order)*treat*dist_resv+
             factor(state_name)+
             factor(district_name)+
             factor(cohort)+
             sex+
             mothers_age_at_birth+
             wait_time+
             read_write_woman+
             religion+
             caste+
             type_of_house+
             state_name:seq_along((cohort)^2)) 

se_9<- coeftest(reg_9,
                vcov = vcovHC(reg_9,
                              type = "HC0",
                              cluster = "district_name"))

rsq_9<- round(rsq(reg_9),digits = 3)

############################## Infant #########################

reg_10<- lm(data = df_second_third_boys,
            formula = infant~factor(birth_order)*treat*dist_resv+
              factor(state_name)+
              factor(district_name)+
              factor(cohort)+
              sex+
              mothers_age_at_birth+
              wait_time+
              read_write_woman+
              religion+
              caste+
              type_of_house+
              state_name:seq_along((cohort)^2)) 

se_10<- coeftest(reg_10,
                 vcov = vcovHC(reg_10,
                               type = "HC0",
                               cluster = "district_name"))

rsq_10<- round(rsq(reg_10),digits = 3)

############################## Under 5 #########################

reg_11<- lm(data = df_second_third_boys,
            formula = under5~factor(birth_order)*treat*dist_resv+
              factor(state_name)+
              factor(district_name)+
              factor(cohort)+
              sex+
              mothers_age_at_birth+
              wait_time+
              read_write_woman+
              religion+
              caste+
              type_of_house+
              state_name:seq_along((cohort)^2)) 

se_11<- coeftest(reg_11,
                 vcov = vcovHC(reg_11,
                               type = "HC0",
                               cluster = "district_name"))

rsq_11<- round(rsq(reg_11),digits = 3)

############################## Under 10 #########################

reg_12<- lm(data = df_second_third_boys,
            formula = under10~factor(birth_order)*treat*dist_resv+
              factor(state_name)+
              factor(district_name)+
              factor(cohort)+
              sex+
              mothers_age_at_birth+
              wait_time+
              read_write_woman+
              religion+
              caste+
              type_of_house+
              state_name:seq_along((cohort)^2)) 

se_12<- coeftest(reg_12,
                 vcov = vcovHC(reg_12,
                               type = "HC0",
                               cluster = "district_name"))

rsq_12<- round(rsq(reg_12),digits = 3)

stargazer(se_1,se_2,se_3,se_4,se_5,se_6,se_7,se_8,se_9,se_10,se_11,se_12,
          omit = c("state_name",
                   "district_name",
                   "cohort",
                   "caste",
                   "religion",
                   "read_write_woman",
                   "type_of_house",
                   "Constant"),
          order = c("^factor(birth_order)2:dist_resv:treat$",
                    "^factor(birth_order)3:dist_resv:treat$",
                    "^treat$",
                    "^sex$",
                    "^mothers_age_at_birth$",
                    "^wait_time$"),
          covariate.labels = c("Post treat",
                               "Child is female",
                               "Mother's age at birth",
                               "Gap between births",
                               "(Birth order=2 vs 1)",
                               "(Birth order=3 vs 1)",
                               "DC resv",
                               "(Birth order=2 vs 1)*post-treat",
                               "(Birth order=2 vs 1)*DC resv",
                               "(Birth order=3 vs 1)*post-treat",
                               "(Birth order=3 vs 1)*DC resv",
                               "Post-treat*DC resv",
                               "(Birth order=2 vs 1)*post-treat*DC resv",
                               "(Birth order=3 vs 1)*post-treat*DC resv"),
          add.lines = list(c("State quadratic trend","x","x","x","x","x","x","x","x","x","x","x","x"),
                           c("Individual controls","x","x","x","x","x","x","x","x","x","x","x","x"),
                           c("State FE","x","x","x","x","x","x","x","x","x","x","x","x"),
                           c("District FE","x","x","x","x","x","x","x","x","x","x","x","x"),
                           c("Cohort FE","x","x","x","x","x","x","x","x","x","x","x","x"),
                           c("R-sq",rsq_1,rsq_2,rsq_3,rsq_4,rsq_5,rsq_6,rsq_7,rsq_8,rsq_9,rsq_10,rsq_11,rsq_12)),
          omit.stat = c("adj.rsq"),
          out = "Dist_regression_first_boy.tex")


##################################### Survival probability when first two born are boys ###################################

########################################### First vs second birth order comparison ##########################

df_first_third_2_boys<- df_first_third%>%
  subset(first_two_boys==1)

df_second_third_2_boys<- df_second_third%>%
  subset(first_two_boys==1)

###################### First vs third ##############################

####################### Neonatal ###########################

reg_1<- lm(data = df_first_third_2_boys,
           formula = neonat~factor(birth_order)*treat*dist_resv+
             factor(state_name)+
             factor(district_name)+
             factor(cohort)+
             sex+
             mothers_age_at_birth+
             wait_time+
             read_write_woman+
             religion+
             caste+
             type_of_house+
             state_name:seq_along((cohort)^2)) 

se_1<- coeftest(reg_1,
                vcov = vcovHC(reg_1,
                              type = "HC0",
                              cluster = "district_name"))

rsq_1<- round(rsq(reg_1),digits = 3)

################## Infant ###############################

reg_2<- lm(data = df_first_third_2_boys,
           formula = infant~factor(birth_order)*treat*dist_resv+
             factor(state_name)+
             factor(district_name)+
             factor(cohort)+
             sex+
             mothers_age_at_birth+
             wait_time+
             read_write_woman+
             religion+
             caste+
             type_of_house+
             state_name:seq_along((cohort)^2)) 

se_2<- coeftest(reg_2,
                vcov = vcovHC(reg_2,
                              type = "HC0",
                              cluster = "district_name"))

rsq_2<- round(rsq(reg_2),digits = 3)

################## Under 5 ###############################

reg_3<- lm(data = df_first_third_2_boys,
           formula = under5~factor(birth_order)*treat*dist_resv+
             factor(state_name)+
             factor(district_name)+
             factor(cohort)+
             sex+
             mothers_age_at_birth+
             wait_time+
             read_write_woman+
             religion+
             caste+
             type_of_house+
             state_name:seq_along((cohort)^2)) 

se_3<- coeftest(reg_3,
                vcov = vcovHC(reg_3,
                              type = "HC0",
                              cluster = "district_name"))

rsq_3<- round(rsq(reg_3),digits = 3)

################## Under 10 ###############################

reg_4<- lm(data = df_first_third_2_boys,
           formula = under10~factor(birth_order)*treat*dist_resv+
             factor(state_name)+
             factor(district_name)+
             factor(cohort)+
             sex+
             mothers_age_at_birth+
             wait_time+
             read_write_woman+
             religion+
             caste+
             type_of_house+
             state_name:seq_along((cohort)^2)) 

se_4<- coeftest(reg_4,
                vcov = vcovHC(reg_4,
                              type = "HC0",
                              cluster = "district_name"))

rsq_4<- round(rsq(reg_4),digits = 3)

############################ Second vs Third ############################

############################## Neonatal #########################

reg_5<- lm(data = df_second_third_2_boys,
           formula = neonat~factor(birth_order)*treat*dist_resv+
             factor(state_name)+
             factor(district_name)+
             factor(cohort)+
             sex+
             mothers_age_at_birth+
             wait_time+
             read_write_woman+
             religion+
             caste+
             type_of_house+
             state_name:seq_along((cohort)^2)) 

se_5<- coeftest(reg_5,
                vcov = vcovHC(reg_5,
                              type = "HC0",
                              cluster = "district_name"))

rsq_5<- round(rsq(reg_5),digits = 3)

############################## Infant #########################

reg_6<- lm(data = df_second_third_2_boys,
            formula = infant~factor(birth_order)*treat*dist_resv+
              factor(state_name)+
              factor(district_name)+
              factor(cohort)+
              sex+
              mothers_age_at_birth+
              wait_time+
              read_write_woman+
              religion+
              caste+
              type_of_house+
             state_name:seq_along((cohort)^2)) 

se_6<- coeftest(reg_6,
                 vcov = vcovHC(reg_6,
                               type = "HC0",
                               cluster = "district_name"))

rsq_6<- round(rsq(reg_6),digits = 3)

############################## Under 5 #########################

reg_7<- lm(data = df_second_third_2_boys,
            formula = under5~factor(birth_order)*treat*dist_resv+
              factor(state_name)+
              factor(district_name)+
              factor(cohort)+
              sex+
              mothers_age_at_birth+
              wait_time+
              read_write_woman+
              religion+
              caste+
              type_of_house+
             state_name:seq_along((cohort)^2)) 

se_7<- coeftest(reg_7,
                 vcov = vcovHC(reg_7,
                               type = "HC0",
                               cluster = "district_name"))

rsq_7<- round(rsq(reg_7),digits = 3)

############################## Under 10 #########################

reg_8<- lm(data = df_second_third_2_boys,
            formula = under10~factor(birth_order)*treat*dist_resv+
              factor(state_name)+
              factor(district_name)+
              factor(cohort)+
              sex+
              mothers_age_at_birth+
              wait_time+
              read_write_woman+
              religion+
              caste+
              type_of_house+
             state_name:seq_along((cohort)^2)) 

se_8<- coeftest(reg_8,
                 vcov = vcovHC(reg_8,
                               type = "HC0",
                               cluster = "district_name"))

rsq_8<- round(rsq(reg_8),digits = 3)

stargazer(se_1,se_2,se_3,se_4,se_5,se_6,se_7,se_8,
          omit = c("state_name",
                   "district_name",
                   "cohort",
                   "caste",
                   "religion",
                   "read_write_woman",
                   "type_of_house",
                   "Constant"),
          order = c("^factor(birth_order)3:dist_resv:treat$",
                    "^treat$",
                    "^sex$",
                    "^mothers_age_at_birth$",
                    "^wait_time$"),
          covariate.labels = c("Post treat",
                               "Child is female",
                               "Mother's age at birth",
                               "Gap between births",
                               "(Birth order=3 vs 1)",
                               "DC resv",
                               "(Birth order=3 vs 1)*post-treat",
                               "(Birth order=3 vs 1)*DC resv",
                               "Post-treat*DC resv",
                               "(Birth order=3 vs 1)*post-treat*DC resv"),
          add.lines = list(c("State quadratic trend","x","x","x","x","x","x","x","x"),
                           c("Individual controls","x","x","x","x","x","x","x","x"),
                           c("State FE","x","x","x","x","x","x","x","x"),
                           c("District FE","x","x","x","x","x","x","x","x"),
                           c("Cohort FE","x","x","x","x","x","x","x","x"),
                           c("R-sq",rsq_1,rsq_2,rsq_3,rsq_4,rsq_5,rsq_6,rsq_7,rsq_8)),
          omit.stat = c("adj.rsq"),
          out = "Dist_regression_first_2_boys.tex")

##################################### Survival probability when first two born are girls ###################################

########################################### First vs second birth order comparison ##########################

df_first_third_2_girls<- df_first_third%>%
  subset(first_two_girls==1)

df_second_third_2_girls<- df_second_third%>%
  subset(first_two_girls==1)


###################### First vs third ##############################

####################### Neonatal ###########################

reg_1<- lm(data = df_first_third_2_girls,
           formula = neonat~factor(birth_order)*treat*dist_resv+
             factor(state_name)+
             factor(district_name)+
             factor(cohort)+
             sex+
             mothers_age_at_birth+
             wait_time+
             read_write_woman+
             religion+
             caste+
             type_of_house+
             state_name:seq_along((cohort)^2)) 

se_1<- coeftest(reg_1,
                vcov = vcovHC(reg_1,
                              type = "HC0",
                              cluster = "district_name"))

rsq_1<- round(rsq(reg_1),digits = 3)

################## Infant ###############################

reg_2<- lm(data = df_first_third_2_girls,
           formula = infant~factor(birth_order)*treat*dist_resv+
             factor(state_name)+
             factor(district_name)+
             factor(cohort)+
             sex+
             mothers_age_at_birth+
             wait_time+
             read_write_woman+
             religion+
             caste+
             type_of_house+
             state_name:seq_along((cohort)^2)) 

se_2<- coeftest(reg_2,
                vcov = vcovHC(reg_2,
                              type = "HC0",
                              cluster = "district_name"))

rsq_2<- round(rsq(reg_2),digits = 3)

################## Under 5 ###############################

reg_3<- lm(data = df_first_third_2_girls,
           formula = under5~factor(birth_order)*treat*dist_resv+
             factor(state_name)+
             factor(district_name)+
             factor(cohort)+
             sex+
             mothers_age_at_birth+
             wait_time+
             read_write_woman+
             religion+
             caste+
             type_of_house+
             state_name:seq_along((cohort)^2)) 

se_3<- coeftest(reg_3,
                vcov = vcovHC(reg_3,
                              type = "HC0",
                              cluster = "district_name"))

rsq_3<- round(rsq(reg_3),digits = 3)

################## Under 10 ###############################

reg_4<- lm(data = df_first_third_2_girls,
           formula = under10~factor(birth_order)*treat*dist_resv+
             factor(state_name)+
             factor(district_name)+
             factor(cohort)+
             sex+
             mothers_age_at_birth+
             wait_time+
             read_write_woman+
             religion+
             caste+
             type_of_house+
             state_name:seq_along((cohort)^2)) 

se_4<- coeftest(reg_4,
                vcov = vcovHC(reg_4,
                              type = "HC0",
                              cluster = "district_name"))

rsq_4<- round(rsq(reg_4),digits = 3)

############################ Second vs Third ############################

############################## Neonatal #########################

reg_5<- lm(data = df_second_third_2_girls,
           formula = neonat~factor(birth_order)*treat*dist_resv+
             factor(state_name)+
             factor(district_name)+
             factor(cohort)+
             sex+
             mothers_age_at_birth+
             wait_time+
             read_write_woman+
             religion+
             caste+
             type_of_house+
             state_name:seq_along((cohort)^2)) 

se_5<- coeftest(reg_5,
                vcov = vcovHC(reg_5,
                              type = "HC0",
                              cluster = "district_name"))

rsq_5<- round(rsq(reg_5),digits = 3)

############################## Infant #########################

reg_6<- lm(data = df_second_third_2_girls,
           formula = infant~factor(birth_order)*treat*dist_resv+
             factor(state_name)+
             factor(district_name)+
             factor(cohort)+
             sex+
             mothers_age_at_birth+
             wait_time+
             read_write_woman+
             religion+
             caste+
             type_of_house+
             state_name:seq_along((cohort)^2)) 

se_6<- coeftest(reg_6,
                vcov = vcovHC(reg_6,
                              type = "HC0",
                              cluster = "district_name"))

rsq_6<- round(rsq(reg_6),digits = 3)

############################## Under 5 #########################

reg_7<- lm(data = df_second_third_2_girls,
           formula = under5~factor(birth_order)*treat*dist_resv+
             factor(state_name)+
             factor(district_name)+
             factor(cohort)+
             sex+
             mothers_age_at_birth+
             wait_time+
             read_write_woman+
             religion+
             caste+
             type_of_house+
             state_name:seq_along((cohort)^2)) 

se_7<- coeftest(reg_7,
                vcov = vcovHC(reg_7,
                              type = "HC0",
                              cluster = "district_name"))

rsq_7<- round(rsq(reg_7),digits = 3)

############################## Under 10 #########################

reg_8<- lm(data = df_second_third_2_girls,
           formula = under10~factor(birth_order)*treat*dist_resv+
             factor(state_name)+
             factor(district_name)+
             factor(cohort)+
             sex+
             mothers_age_at_birth+
             wait_time+
             read_write_woman+
             religion+
             caste+
             type_of_house+
             state_name:seq_along((cohort)^2)) 

se_8<- coeftest(reg_8,
                vcov = vcovHC(reg_8,
                              type = "HC0",
                              cluster = "district_name"))

rsq_8<- round(rsq(reg_8),digits = 3)

stargazer(se_1,se_2,se_3,se_4,se_5,se_6,se_7,se_8,
          omit = c("state_name",
                   "district_name",
                   "cohort",
                   "caste",
                   "religion",
                   "read_write_woman",
                   "type_of_house",
                   "Constant"),
          order = c("^factor(birth_order)3:dist_resv:treat$",
                    "^treat$",
                    "^sex$",
                    "^mothers_age_at_birth$",
                    "^wait_time$"),
          covariate.labels = c("Post treat",
                               "Child is female",
                               "Mother's age at birth",
                               "Gap between births",
                               "(Birth order=3 vs 1)",
                               "DC resv",
                               "(Birth order=3 vs 1)*post-treat",
                               "(Birth order=3 vs 1)*DC resv",
                               "Post-treat*DC resv",
                               "(Birth order=3 vs 1)*post-treat*DC resv"),
          add.lines = list(c("State quadratic trend","x","x","x","x","x","x","x","x"),
                           c("Individual controls","x","x","x","x","x","x","x","x"),
                           c("State FE","x","x","x","x","x","x","x","x"),
                           c("District FE","x","x","x","x","x","x","x","x"),
                           c("Cohort FE","x","x","x","x","x","x","x","x"),
                           c("R-sq",rsq_1,rsq_2,rsq_3,rsq_4,rsq_5,rsq_6,rsq_7,rsq_8)),
          omit.stat = c("adj.rsq"),
          out = "Dist_regression_first_2_girls.tex")
