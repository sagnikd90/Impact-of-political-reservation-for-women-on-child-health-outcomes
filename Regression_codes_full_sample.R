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
library(clusterSEs)
library(rsq)

setwd("C:\\Users\\das90\\OneDrive - Cuny GradCenter\\Research\\Political reservation and age of marriage\\Github\\Political-reservation-and-age-of-marriage-age-of-first-birth\\Final version\\R results\\Regression_output")

df<- read.csv("C:\\Users\\das90\\OneDrive - Cuny GradCenter\\Research\\Political reservation and age of marriage\\Github\\Political-reservation-and-age-of-marriage-age-of-first-birth\\Final version\\Cleaned datasets\\Full_sample_data_for_analysis.csv")

############################# Baseline estimates for each birth order separately ######################################

df_1<- df%>%
  subset(birth_order==1)

df_2<- df%>%
  subset(birth_order==2)

df_3<- df%>%
  subset(birth_order==3)

######################### Neonatal survival probability ###########################

reg_1<- lm(data = df_1,
           formula = neonat~treat+
             factor(state_name)+
             factor(cohort)+
             sex+
             mothers_age_at_birth+
             wait_time+
             read_write_woman+
             religion+
             caste+
             type_of_house+
             state_name:seq_along(cohort)) 

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
             wait_time+
             read_write_woman+
             religion+
             caste+
             type_of_house+
             state_name:seq_along(cohort)) 

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
             wait_time+
             read_write_woman+
             religion+
             caste+
             type_of_house+
             state_name:seq_along(cohort)) 

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
             wait_time+
             read_write_woman+
             religion+
             caste+
             type_of_house+
             state_name:seq_along(cohort)) 

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
             wait_time+
             read_write_woman+
             religion+
             caste+
             type_of_house+
             state_name:seq_along(cohort)) 

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
             wait_time+
             read_write_woman+
             religion+
             caste+
             type_of_house+
             state_name:seq_along(cohort)) 

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
             wait_time+
             read_write_woman+
             religion+
             caste+
             type_of_house+
             state_name:seq_along(cohort)) 

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
             wait_time+
             read_write_woman+
             religion+
             caste+
             type_of_house+
             state_name:seq_along(cohort)) 

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
             wait_time+
             read_write_woman+
             religion+
             caste+
             type_of_house+
             state_name:seq_along(cohort)) 

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
              wait_time+
              read_write_woman+
              religion+
              caste+
              type_of_house+
              state_name:seq_along(cohort)) 

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
              wait_time+
              read_write_woman+
              religion+
              caste+
              type_of_house+
              state_name:seq_along(cohort)) 

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
              wait_time+
              read_write_woman+
              religion+
              caste+
              type_of_house+
              state_name:seq_along(cohort)) 

se_12<- vcovHC(reg_12, 
               type = "HC0", 
               cluster = "state_name")

se_12<- sqrt(diag(se_12))


stargazer(reg_1,reg_2,reg_3,reg_4,reg_5,reg_6,reg_7,reg_8,reg_9,reg_10,reg_11,reg_12,
          se=list(se_1,se_2,se_3,se_4,se_5,se_6,se_7,se_8,se_9,se_10,se_11,se_12),
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
                    "^wait_time$"),
          covariate.labels = c("Post-treat",
                               "Child is female",
                               "Mother's age at birth",
                               "Gap between births"),
          add.lines = list(c("Individual controls","x","x","x","x","x","x","x","x","x","x","x","x"),
                           c("State FE","x","x","x","x","x","x","x","x","x","x","x","x"),
                           c("Cohort FE","x","x","x","x","x","x","x","x","x","x","x","x")),
          no.space = TRUE,
          omit.stat = c("ser","f","adj.rsq"),
          out = "Baseline_reg_2_full_sample.tex")


rm(reg_1,
   reg_2,
   reg_3,
   reg_4,
   reg_5,
   reg_6,
   reg_7,
   reg_8,
   reg_9,
   reg_10,
   reg_11,
   reg_12,
   se_1,
   se_2,
   se_3,
   se_4,
   se_5,
   se_6,
   se_7,
   se_8,
   se_9,
   se_10,
   se_11,
   se_12,
   df_1,
   df_2,
   df_3)

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

########################################## Neonatal survival probability ###################################

options(boot.ncups=8)

reg_1 <- glm(data = df_first_second,
             formula = neonat~factor(birth_order)*treat+
               state_name+
               factor(cohort)+
               sex+
               mothers_age_at_birth+
               wait_time+
               read_write_woman+
               religion+
               caste+
               type_of_house+
               state_name:seq_along(cohort^2),
             family = "gaussian")

se_1 <- vcovHC(reg_1, 
               type = "HC0", 
               cluster = "state_name")

se_1 <- sqrt(diag(se_1))

pval_1<-  cluster.wild.glm(reg_1,
                           dat = df_first_second,
                           cluster = ~state_name,
                           boot.reps = 1000,
                           seed = 100)

rsq_1<- round(rsq(reg_1),digits = 3)

reg_2 <- glm(data = df_first_third,
             formula = neonat~factor(birth_order)*treat+
               state_name+
               factor(cohort)+
               sex+
               mothers_age_at_birth+
               wait_time+
               read_write_woman+
               religion+
               caste+
               type_of_house+
               state_name:seq_along(cohort^2),
             family = "gaussian")

se_2 <- vcovHC(reg_2, 
               type = "HC0", 
               cluster = "state_name")

se_2 <- sqrt(diag(se_2))

pval_2<-  cluster.wild.glm(reg_2,
                           dat = df_first_third,
                           cluster = ~state_name,
                           boot.reps = 1000,
                           seed = 100)

rsq_2<- round(rsq(reg_2),digits = 3)

reg_3 <- glm(data = df_second_third,
             formula = neonat~factor(birth_order)*treat+
               state_name+
               factor(cohort)+
               sex+
               mothers_age_at_birth+
               wait_time+
               read_write_woman+
               religion+
               caste+
               type_of_house+
               state_name:seq_along(cohort^2),
             family = "gaussian")

se_3 <- vcovHC(reg_3, 
               type = "HC0", 
               cluster = "state_name")

se_3 <- sqrt(diag(se_3))

pval_3<-  cluster.wild.glm(reg_3,
                           dat = df_second_third,
                           cluster = ~state_name,
                           boot.reps = 1000,
                           seed = 100)

rsq_3<- round(rsq(reg_3),digits = 3)

########################################## Infant survival probability ###################################

reg_4 <- glm(data = df_first_second,
             formula = infant~factor(birth_order)*treat+
               state_name+
               factor(cohort)+
               sex+
               mothers_age_at_birth+
               wait_time+
               read_write_woman+
               religion+
               caste+
               type_of_house+
               state_name:seq_along(cohort^2),
             family = "gaussian")

se_4 <- vcovHC(reg_4, 
               type = "HC0", 
               cluster = "state_name")

se_4 <- sqrt(diag(se_4))

pval_4<-  cluster.wild.glm(reg_4,
                           dat = df_first_second,
                           cluster = ~state_name,
                           boot.reps = 1000,
                           seed = 100)

rsq_4<- round(rsq(reg_4),digits = 3)

reg_5 <- glm(data = df_first_third,
             formula = infant~factor(birth_order)*treat+
               state_name+
               factor(cohort)+
               sex+
               mothers_age_at_birth+
               wait_time+
               read_write_woman+
               religion+
               caste+
               type_of_house+
               state_name:seq_along(cohort^2),
             family = "gaussian")

se_5 <- vcovHC(reg_5, 
               type = "HC0", 
               cluster = "state_name")

se_5 <- sqrt(diag(se_5))

pval_5<-  cluster.wild.glm(reg_5,
                           dat = df_first_third,
                           cluster = ~state_name,
                           boot.reps = 1000,
                           seed = 100)

rsq_5<- round(rsq(reg_5),digits = 3)

reg_6 <- glm(data = df_second_third,
             formula = infant~factor(birth_order)*treat+
               state_name+
               factor(cohort)+
               sex+
               mothers_age_at_birth+
               wait_time+
               read_write_woman+
               religion+
               caste+
               type_of_house+
               state_name:seq_along(cohort^2),
             family = "gaussian")

se_6 <- vcovHC(reg_6, 
               type = "HC0", 
               cluster = "state_name")

se_6 <- sqrt(diag(se_6))

pval_6<-  cluster.wild.glm(reg_6,
                           dat = df_second_third,
                           cluster = ~state_name,
                           boot.reps = 1000,
                           seed = 100)

rsq_6<- round(rsq(reg_6),digits = 3)


########################################## Under 5 survival probability ###################################

reg_7 <- glm(data = df_first_second,
             formula = under5~factor(birth_order)*treat+
               state_name+
               factor(cohort)+
               sex+
               mothers_age_at_birth+
               wait_time+
               read_write_woman+
               religion+
               caste+
               type_of_house+
               state_name:seq_along(cohort^2),
             family = "gaussian")

se_7 <- vcovHC(reg_7, 
               type = "HC0", 
               cluster = "state_name")

se_7 <- sqrt(diag(se_7))

pval_7<-  cluster.wild.glm(reg_7,
                           dat = df_first_second,
                           cluster = ~state_name,
                           boot.reps = 1000,
                           seed = 100)

rsq_7<- round(rsq(reg_7),digits = 3)

reg_8 <- glm(data = df_first_third,
             formula = under5~factor(birth_order)*treat+
               state_name+
               factor(cohort)+
               sex+
               mothers_age_at_birth+
               wait_time+
               read_write_woman+
               religion+
               caste+
               type_of_house+
               state_name:seq_along(cohort^2),
             family = "gaussian")

se_8 <- vcovHC(reg_8, 
               type = "HC0", 
               cluster = "state_name")

se_8 <- sqrt(diag(se_8))

pval_8<-  cluster.wild.glm(reg_8,
                           dat = df_first_third,
                           cluster = ~state_name,
                           boot.reps = 1000,
                           seed = 100)

rsq_8<- round(rsq(reg_8),digits = 3)

reg_9 <- glm(data = df_second_third,
             formula = under5~factor(birth_order)*treat+
               state_name+
               factor(cohort)+
               sex+
               mothers_age_at_birth+
               wait_time+
               read_write_woman+
               religion+
               caste+
               type_of_house+
               state_name:seq_along(cohort^2),
             family = "gaussian")

se_9 <- vcovHC(reg_9, 
               type = "HC0", 
               cluster = "state_name")

se_9 <- sqrt(diag(se_9))

pval_9<-  cluster.wild.glm(reg_9,
                           dat = df_second_third,
                           cluster = ~state_name,
                           boot.reps = 1000,
                           seed = 100)

rsq_9<- round(rsq(reg_9),digits = 3)

########################################## Under 10 survival probability ###################################

reg_10 <- glm(data = df_first_second,
             formula = under10~factor(birth_order)*treat+
               state_name+
               factor(cohort)+
               sex+
               mothers_age_at_birth+
               wait_time+
               read_write_woman+
               religion+
               caste+
               type_of_house+
               state_name:seq_along(cohort^2),
             family = "gaussian")

se_10<- vcovHC(reg_10, 
               type = "HC0", 
               cluster = "state_name")

se_10 <- sqrt(diag(se_10))

pval_10<-  cluster.wild.glm(reg_10,
                           dat = df_first_second,
                           cluster = ~state_name,
                           boot.reps = 1000,
                           seed = 100)

rsq_10<- round(rsq(reg_10),digits = 3)

reg_11 <- glm(data = df_first_third,
             formula = under10~factor(birth_order)*treat+
               state_name+
               factor(cohort)+
               sex+
               mothers_age_at_birth+
               wait_time+
               read_write_woman+
               religion+
               caste+
               type_of_house+
               state_name:seq_along(cohort^2),
             family = "gaussian")

se_11 <- vcovHC(reg_11, 
               type = "HC0", 
               cluster = "state_name")

se_11 <- sqrt(diag(se_11))

pval_11<-  cluster.wild.glm(reg_11,
                           dat = df_first_third,
                           cluster = ~state_name,
                           boot.reps = 1000,
                           seed = 100)

rsq_11<- round(rsq(reg_11),digits = 3)

reg_12 <- glm(data = df_second_third,
             formula = under10~factor(birth_order)*treat+
               state_name+
               factor(cohort)+
               sex+
               mothers_age_at_birth+
               wait_time+
               read_write_woman+
               religion+
               caste+
               type_of_house+
               state_name:seq_along(cohort^2),
             family = "gaussian")

se_12 <- vcovHC(reg_12, 
               type = "HC0", 
               cluster = "state_name")

se_12 <- sqrt(diag(se_12))

pval_12<-  cluster.wild.glm(reg_12,
                           dat = df_second_third,
                           cluster = ~state_name,
                           boot.reps = 1000,
                           seed = 100)

rsq_12<- round(rsq(reg_12),digits = 3)

stargazer(reg_1,reg_4,reg_7,reg_10,reg_2,reg_5,reg_8,reg_11,reg_3,reg_6,reg_9,reg_12,
          se=list(se_1,se_4,se_7,se_10,se_2,se_5,se_8,se_11,se_3,se_6,se_9,se_12),
          p=c(pval_1,pval_4,pval_7,pval_10,pval_2,pval_5,pval_8,pval_11,pval_3,pval_6,pval_9,pval_12),
          omit = c("state_name",
                   "cohort",
                   "read_write_woman",
                   "caste",
                   "religion",
                   "type_of_house",
                   "Constant"),
          order = c("^factor(birth_order)2:treat$",
                    "^factor(birth_order)3:treat$",
                    "sex",
                    "^mothers_age_at_birth$",
                    "^wait_time$"),
          covariate.labels = c("Child is female",
                               "Mother's age at birth",
                               "Gap between births",
                               "(Birth order=2 vs 1)",
                               "(Birth order=3 vs 1)",
                               "Post-treat",
                               "(Birth order=2 vs 1)*post-treat",
                               "(Birth order=3 vs 1)*post-treat"),
          add.lines = list(c("Time-trend","x","x","x","x","x","x","x","x","x","x","x","x"),
                           c("Individual controls","x","x","x","x","x","x","x","x","x","x","x","x"),
                           c("State FE","x","x","x","x","x","x","x","x","x","x","x","x"),
                           c("Cohort FE","x","x","x","x","x","x","x","x","x","x","x","x"),
                           c("R-sq",rsq_1,rsq_4,rsq_7,rsq_10,rsq_2,rsq_5,rsq_8,rsq_11,rsq_3,rsq_6,rsq_9,rsq_12)),
          omit.stat = c("ll","aic"),
          no.space = TRUE,
          out = "Full_sample_reg.tex")

##################################### Survival probability when first born is a boy ###################################

df_first_second_boys<- df_first_second%>%
  subset(first_born_boy==1)

df_first_third_boys<- df_first_third%>%
  subset(first_born_boy==1)

df_second_third_boys<- df_second_third%>%
  subset(first_born_boy==1)

########################################## Neonatal survival probability ###################################

options(boot.ncups=8)

reg_1 <- glm(data = df_first_second_boys,
             formula = neonat~factor(birth_order)*treat+
               state_name+
               factor(cohort)+
               sex+
               mothers_age_at_birth+
               wait_time+
               read_write_woman+
               religion+
               caste+
               type_of_house+
               state_name:seq_along(cohort^2),
             family = "gaussian")

se_1 <- vcovHC(reg_1, 
               type = "HC0", 
               cluster = "state_name")

se_1 <- sqrt(diag(se_1))

pval_1<-  cluster.wild.glm(reg_1,
                           dat = df_first_second_boys,
                           cluster = ~state_name,
                           boot.reps = 1000,
                           seed = 100)

rsq_1<- round(rsq(reg_1),digits = 3)

reg_2 <- glm(data = df_first_third_boys,
             formula = neonat~factor(birth_order)*treat+
               state_name+
               factor(cohort)+
               sex+
               mothers_age_at_birth+
               wait_time+
               read_write_woman+
               religion+
               caste+
               type_of_house+
               state_name:seq_along(cohort^2),
             family = "gaussian")

se_2 <- vcovHC(reg_2, 
               type = "HC0", 
               cluster = "state_name")

se_2 <- sqrt(diag(se_2))

pval_2<-  cluster.wild.glm(reg_2,
                           dat = df_first_third_boys,
                           cluster = ~state_name,
                           boot.reps = 1000,
                           seed = 100)

rsq_2<- round(rsq(reg_2),digits = 3)

reg_3 <- glm(data = df_second_third_boys,
             formula = neonat~factor(birth_order)*treat+
               state_name+
               factor(cohort)+
               sex+
               mothers_age_at_birth+
               wait_time+
               read_write_woman+
               religion+
               caste+
               type_of_house+
               state_name:seq_along(cohort^2),
             family = "gaussian")

se_3 <- vcovHC(reg_3, 
               type = "HC0", 
               cluster = "state_name")

se_3 <- sqrt(diag(se_3))

pval_3<-  cluster.wild.glm(reg_3,
                           dat = df_second_third_boys,
                           cluster = ~state_name,
                           boot.reps = 1000,
                           seed = 100)

rsq_3<- round(rsq(reg_3),digits = 3)

########################################## Infant survival probability ###################################

reg_4 <- glm(data = df_first_second_boys,
             formula = infant~factor(birth_order)*treat+
               state_name+
               factor(cohort)+
               sex+
               mothers_age_at_birth+
               wait_time+
               read_write_woman+
               religion+
               caste+
               type_of_house+
               state_name:seq_along(cohort^2),
             family = "gaussian")

se_4 <- vcovHC(reg_4, 
               type = "HC0", 
               cluster = "state_name")

se_4 <- sqrt(diag(se_4))

pval_4<-  cluster.wild.glm(reg_4,
                           dat = df_first_second_boys,
                           cluster = ~state_name,
                           boot.reps = 1000,
                           seed = 100)

rsq_4<- round(rsq(reg_4),digits = 3)

reg_5 <- glm(data = df_first_third_boys,
             formula = infant~factor(birth_order)*treat+
               state_name+
               factor(cohort)+
               sex+
               mothers_age_at_birth+
               wait_time+
               read_write_woman+
               religion+
               caste+
               type_of_house+
               state_name:seq_along(cohort^2),
             family = "gaussian")

se_5 <- vcovHC(reg_5, 
               type = "HC0", 
               cluster = "state_name")

se_5 <- sqrt(diag(se_5))

pval_5<-  cluster.wild.glm(reg_5,
                           dat = df_first_third_boys,
                           cluster = ~state_name,
                           boot.reps = 1000,
                           seed = 100)

rsq_5<- round(rsq(reg_5),digits = 3)

reg_6 <- glm(data = df_second_third_boys,
             formula = infant~factor(birth_order)*treat+
               state_name+
               factor(cohort)+
               sex+
               mothers_age_at_birth+
               wait_time+
               read_write_woman+
               religion+
               caste+
               type_of_house+
               state_name:seq_along(cohort^2),
             family = "gaussian")

se_6 <- vcovHC(reg_6, 
               type = "HC0", 
               cluster = "state_name")

se_6 <- sqrt(diag(se_6))

pval_6<-  cluster.wild.glm(reg_6,
                           dat = df_second_third_boys,
                           cluster = ~state_name,
                           boot.reps = 1000,
                           seed = 100)

rsq_6<- round(rsq(reg_6),digits = 3)

########################################## Under 5 survival probability ###################################

reg_7 <- glm(data = df_first_second_boys,
             formula = under5~factor(birth_order)*treat+
               state_name+
               factor(cohort)+
               sex+
               mothers_age_at_birth+
               wait_time+
               read_write_woman+
               religion+
               caste+
               type_of_house+
               state_name:seq_along(cohort^2),
             family = "gaussian")

se_7 <- vcovHC(reg_7, 
               type = "HC0", 
               cluster = "state_name")

se_7 <- sqrt(diag(se_7))

pval_7<-  cluster.wild.glm(reg_7,
                           dat = df_first_second_boys,
                           cluster = ~state_name,
                           boot.reps = 1000,
                           seed = 100)

rsq_7<- round(rsq(reg_7),digits = 3)

reg_8 <- glm(data = df_first_third_boys,
             formula = under5~factor(birth_order)*treat+
               state_name+
               factor(cohort)+
               sex+
               mothers_age_at_birth+
               wait_time+
               read_write_woman+
               religion+
               caste+
               type_of_house+
               state_name:seq_along(cohort^2),
             family = "gaussian")

se_8 <- vcovHC(reg_8, 
               type = "HC0", 
               cluster = "state_name")

se_8 <- sqrt(diag(se_8))

pval_8<-  cluster.wild.glm(reg_8,
                           dat = df_first_third_boys,
                           cluster = ~state_name,
                           boot.reps = 1000,
                           seed = 100)

rsq_8<- round(rsq(reg_8),digits = 3)

reg_9 <- glm(data = df_second_third_boys,
             formula = under5~factor(birth_order)*treat+
               state_name+
               factor(cohort)+
               sex+
               mothers_age_at_birth+
               wait_time+
               read_write_woman+
               religion+
               caste+
               type_of_house+
               state_name:seq_along(cohort^2),
             family = "gaussian")

se_9 <- vcovHC(reg_9, 
               type = "HC0", 
               cluster = "state_name")

se_9 <- sqrt(diag(se_9))

pval_9<-  cluster.wild.glm(reg_9,
                           dat = df_second_third_boys,
                           cluster = ~state_name,
                           boot.reps = 1000,
                           seed = 100)

rsq_9<- round(rsq(reg_9),digits = 3)

########################################## Under 10 survival probability ###################################

reg_10 <- glm(data = df_first_second_boys,
              formula = under10~factor(birth_order)*treat+
                state_name+
                factor(cohort)+
                sex+
                mothers_age_at_birth+
                wait_time+
                read_write_woman+
                religion+
                caste+
                type_of_house+
                state_name:seq_along(cohort^2),
              family = "gaussian")

se_10<- vcovHC(reg_10, 
               type = "HC0", 
               cluster = "state_name")

se_10 <- sqrt(diag(se_10))

pval_10<-  cluster.wild.glm(reg_10,
                            dat = df_first_second_boys,
                            cluster = ~state_name,
                            boot.reps = 1000,
                            seed = 100)

rsq_10<- round(rsq(reg_10),digits = 3)

reg_11 <- glm(data = df_first_third_boys,
              formula = under10~factor(birth_order)*treat+
                state_name+
                factor(cohort)+
                sex+
                mothers_age_at_birth+
                wait_time+
                read_write_woman+
                religion+
                caste+
                type_of_house+
                state_name:seq_along(cohort^2),
              family = "gaussian")

se_11 <- vcovHC(reg_11, 
                type = "HC0", 
                cluster = "state_name")

se_11 <- sqrt(diag(se_11))

pval_11<-  cluster.wild.glm(reg_11,
                            dat = df_first_third_boys,
                            cluster = ~state_name,
                            boot.reps = 1000,
                            seed = 100)

rsq_11<- round(rsq(reg_11),digits = 3)

reg_12 <- glm(data = df_second_third_boys,
              formula = under10~factor(birth_order)*treat+
                state_name+
                factor(cohort)+
                sex+
                mothers_age_at_birth+
                wait_time+
                read_write_woman+
                religion+
                caste+
                type_of_house+
                state_name:seq_along(cohort^2),
              family = "gaussian")

se_12 <- vcovHC(reg_12, 
                type = "HC0", 
                cluster = "state_name")

se_12 <- sqrt(diag(se_12))

pval_12<-  cluster.wild.glm(reg_12,
                            dat = df_second_third_boys,
                            cluster = ~state_name,
                            boot.reps = 1000,
                            seed = 100)

rsq_12<- round(rsq(reg_12),digits = 3)

stargazer(reg_1,reg_4,reg_7,reg_10,reg_2,reg_5,reg_8,reg_11,reg_3,reg_6,reg_9,reg_12,
          se=list(se_1,se_4,se_7,se_10,se_2,se_5,se_8,se_11,se_3,se_6,se_9,se_12),
          p=c(pval_1,pval_4,pval_7,pval_10,pval_2,pval_5,pval_8,pval_11,pval_3,pval_6,pval_9,pval_12),
          omit = c("state_name",
                   "cohort",
                   "read_write_woman",
                   "caste",
                   "religion",
                   "type_of_house",
                   "Constant"),
          order = c("^factor(birth_order)2:treat$",
                    "^factor(birth_order)3:treat$",
                    "sex",
                    "^mothers_age_at_birth$",
                    "^wait_time$"),
          covariate.labels = c("Child is female",
                               "Mother's age at birth",
                               "Gap between births",
                               "(Birth order=2 vs 1)",
                               "(Birth order=3 vs 1)",
                               "Post-treat",
                               "(Birth order=2 vs 1)*post-treat",
                               "(Birth order=3 vs 1)*post-treat"),
          add.lines = list(c("Time-trend","x","x","x","x","x","x","x","x","x","x","x","x"),
                           c("Individual controls","x","x","x","x","x","x","x","x","x","x","x","x"),
                           c("State FE","x","x","x","x","x","x","x","x","x","x","x","x"),
                           c("Cohort FE","x","x","x","x","x","x","x","x","x","x","x","x"),
                           c("R-sq",rsq_1,rsq_4,rsq_7,rsq_10,rsq_2,rsq_5,rsq_8,rsq_11,rsq_3,rsq_6,rsq_9,rsq_12)),
          omit.stat = c("ll","aic"),
          no.space = TRUE,
          out = "Full_sample_1st_boy_reg.tex")


##################################### Survival probability when first two born are boys ###################################

df_first_third_2_boys<- df_first_third%>%
  subset(first_two_boys==1)

df_second_third_2_boys<- df_second_third%>%
  subset(first_two_boys==1)


reg_1 <- glm(data = df_first_third_2_boys,
             formula = neonat~factor(birth_order)*treat+
               state_name+
               factor(cohort)+
               sex+
               wait_time+
               mothers_age_at_birth+
               read_write_woman+
               religion+
               caste+
               type_of_house+
               state_name:seq_along((cohort)^2),
             family = "gaussian")

se_1 <- vcovHC(reg_1, 
               type = "HC0", 
               cluster = "state_name")

se_1 <- sqrt(diag(se_1))

pval_1<-  cluster.wild.glm(reg_1,
                           dat = df_first_third_2_boys,
                           cluster = ~state_name,
                           boot.reps = 1000,
                           seed = 100)

rsq_1<- round(rsq(reg_1),digits = 3)


reg_2 <- glm(data = df_second_third_2_boys,
             formula = neonat~factor(birth_order)*treat+
               state_name+
               factor(cohort)+
               sex+
               wait_time+
               mothers_age_at_birth+
               read_write_woman+
               religion+
               caste+
               type_of_house+
               state_name:seq_along((cohort)^2),
             family = "gaussian")

se_2 <- vcovHC(reg_2, 
               type = "HC0", 
               cluster = "state_name")

se_2 <- sqrt(diag(se_2))

pval_2<-  cluster.wild.glm(reg_2,
                           dat = df_second_third_2_boys,
                           cluster = ~state_name,
                           boot.reps = 1000,
                           seed = 100)

rsq_2<- round(rsq(reg_2),digits = 3)

reg_3 <- glm(data = df_first_third_2_boys,
             formula = infant~factor(birth_order)*treat+
               state_name+
               factor(cohort)+
               sex+
               wait_time+
               mothers_age_at_birth+
               read_write_woman+
               religion+
               caste+
               type_of_house+
               state_name:seq_along((cohort)^2),
             family = "gaussian")

se_3 <- vcovHC(reg_3, 
               type = "HC0", 
               cluster = "state_name")

se_3 <- sqrt(diag(se_3))

pval_3<-  cluster.wild.glm(reg_3,
                           dat = df_first_third_2_boys,
                           cluster = ~state_name,
                           boot.reps = 1000,
                           seed = 100)

rsq_3<- round(rsq(reg_3),digits = 3)


reg_4 <- glm(data = df_second_third_2_boys,
             formula = infant~factor(birth_order)*treat+
               state_name+
               factor(cohort)+
               sex+
               wait_time+
               mothers_age_at_birth+
               read_write_woman+
               religion+
               caste+
               type_of_house+
               state_name:seq_along((cohort)^2),
             family = "gaussian")

se_4 <- vcovHC(reg_4, 
               type = "HC0", 
               cluster = "state_name")

se_4 <- sqrt(diag(se_4))

pval_4<-  cluster.wild.glm(reg_4,
                           dat = df_second_third_2_boys,
                           cluster = ~state_name,
                           boot.reps = 1000,
                           seed = 100)

rsq_4<- round(rsq(reg_4),digits = 3)

reg_5 <- glm(data = df_first_third_2_boys,
             formula = under5~factor(birth_order)*treat+
               state_name+
               factor(cohort)+
               sex+
               wait_time+
               mothers_age_at_birth+
               read_write_woman+
               religion+
               caste+
               type_of_house+
               state_name:seq_along((cohort)^2),
             family = "gaussian")

se_5 <- vcovHC(reg_5, 
               type = "HC0", 
               cluster = "state_name")

se_5 <- sqrt(diag(se_5))

pval_5<-  cluster.wild.glm(reg_5,
                           dat = df_first_third_2_boys,
                           cluster = ~state_name,
                           boot.reps = 1000,
                           seed = 100)

rsq_5<- round(rsq(reg_5),digits = 3)


reg_6 <- glm(data = df_second_third_2_boys,
             formula = under5~factor(birth_order)*treat+
               state_name+
               factor(cohort)+
               sex+
               wait_time+
               mothers_age_at_birth+
               read_write_woman+
               religion+
               caste+
               type_of_house+
               state_name:seq_along((cohort)^2),
             family = "gaussian")

se_6 <- vcovHC(reg_6, 
               type = "HC0", 
               cluster = "state_name")

se_6 <- sqrt(diag(se_6))

pval_6<-  cluster.wild.glm(reg_6,
                           dat = df_second_third_2_boys,
                           cluster = ~state_name,
                           boot.reps = 1000,
                           seed = 100)

rsq_6<- round(rsq(reg_6),digits = 3)

reg_7 <- glm(data = df_first_third_2_boys,
             formula = under10~factor(birth_order)*treat+
               state_name+
               factor(cohort)+
               sex+
               wait_time+
               mothers_age_at_birth+
               read_write_woman+
               religion+
               caste+
               type_of_house+
               state_name:seq_along((cohort)^2),
             family = "gaussian")

se_7 <- vcovHC(reg_7, 
               type = "HC0", 
               cluster = "state_name")

se_7 <- sqrt(diag(se_7))

pval_7<-  cluster.wild.glm(reg_7,
                           dat = df_first_third_2_boys,
                           cluster = ~state_name,
                           boot.reps = 1000,
                           seed = 100)

rsq_7<- round(rsq(reg_7),digits = 3)

reg_8 <- glm(data = df_second_third_2_boys,
             formula = under10~factor(birth_order)*treat+
               state_name+
               factor(cohort)+
               sex+
               wait_time+
               mothers_age_at_birth+
               read_write_woman+
               religion+
               caste+
               type_of_house+
               state_name:seq_along((cohort)^2),
             family = "gaussian")

se_8 <- vcovHC(reg_8, 
               type = "HC0", 
               cluster = "state_name")

se_8 <- sqrt(diag(se_8))

pval_8<-  cluster.wild.glm(reg_8,
                           dat = df_second_third_2_boys,
                           cluster = ~state_name,
                           boot.reps = 1000,
                           seed = 100)

rsq_8<- round(rsq(reg_8),digits = 3)

stargazer(reg_1,reg_3,reg_5,reg_7,reg_2,reg_4,reg_6,reg_8,
          se=list(se_1,se_3,se_5,se_7,se_2,se_4,se_6,se_8),
          p=c(pval_1,pval_3,pval_5,pval_7,pval_2,pval_4,pval_6,pval_8),
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
                    "^wait_time$"),
          covariate.labels = c("Child is female",
                               "Mother's age at birth",
                               "Gap between births",
                               "(Birth order=3 vs 1)",
                               "Post-treat",
                               "(Birth order=3 vs 1)*post-treat"),
          add.lines = list(c("Time trend","x","x","x","x","x","x","x","x"),
                           c("Individual controls","x","x","x","x","x","x","x","x"),
                           c("State FE","x","x","x","x","x","x","x","x"),
                           c("Cohort FE","x","x","x","x","x","x","x","x"),
                           c("R-sq",rsq_1,rsq_3,rsq_5,rsq_7,rsq_2,rsq_4,rsq_6,rsq_8)),
          no.space = TRUE,
          omit.stat = c("aic","ll"),
          out = "Full_sample_reg_2_boys.tex")


##################################### Survival probability when first two born are girls ###################################

df_first_third_2_girls<- df_first_third%>%
  subset(first_two_girls==1)

df_second_third_2_girls<- df_second_third%>%
  subset(first_two_girls==1)

reg_1 <- glm(data = df_first_third_2_girls,
             formula = neonat~factor(birth_order)*treat+
               state_name+
               factor(cohort)+
               sex+
               wait_time+
               mothers_age_at_birth+
               read_write_woman+
               religion+
               caste+
               type_of_house+
               state_name:seq_along((cohort)^2),
             family = "gaussian")

se_1 <- vcovHC(reg_1, 
               type = "HC0", 
               cluster = "state_name")

se_1 <- sqrt(diag(se_1))

pval_1<-  cluster.wild.glm(reg_1,
                           dat = df_first_third_2_girls,
                           cluster = ~state_name,
                           boot.reps = 1000,
                           seed = 100)

rsq_1<- round(rsq(reg_1),digits = 3)


reg_2 <- glm(data = df_second_third_2_girls,
             formula = neonat~factor(birth_order)*treat+
               state_name+
               factor(cohort)+
               sex+
               wait_time+
               mothers_age_at_birth+
               read_write_woman+
               religion+
               caste+
               type_of_house+
               state_name:seq_along((cohort)^2),
             family = "gaussian")

se_2 <- vcovHC(reg_2, 
               type = "HC0", 
               cluster = "state_name")

se_2 <- sqrt(diag(se_2))

pval_2<-  cluster.wild.glm(reg_2,
                           dat = df_second_third_2_girls,
                           cluster = ~state_name,
                           boot.reps = 1000,
                           seed = 100)

rsq_2<- round(rsq(reg_2),digits = 3)

reg_3 <- glm(data = df_first_third_2_girls,
             formula = infant~factor(birth_order)*treat+
               state_name+
               factor(cohort)+
               sex+
               wait_time+
               mothers_age_at_birth+
               read_write_woman+
               religion+
               caste+
               type_of_house+
               state_name:seq_along((cohort)^2),
             family = "gaussian")

se_3 <- vcovHC(reg_3, 
               type = "HC0", 
               cluster = "state_name")

se_3 <- sqrt(diag(se_3))

pval_3<-  cluster.wild.glm(reg_3,
                           dat = df_first_third_2_girls,
                           cluster = ~state_name,
                           boot.reps = 1000,
                           seed = 100)

rsq_3<- round(rsq(reg_3),digits = 3)


reg_4 <- glm(data = df_second_third_2_girls,
             formula = infant~factor(birth_order)*treat+
               state_name+
               factor(cohort)+
               sex+
               wait_time+
               mothers_age_at_birth+
               read_write_woman+
               religion+
               caste+
               type_of_house+
               state_name:seq_along((cohort)^2),
             family = "gaussian")

se_4 <- vcovHC(reg_4, 
               type = "HC0", 
               cluster = "state_name")

se_4 <- sqrt(diag(se_4))

pval_4<-  cluster.wild.glm(reg_4,
                           dat = df_second_third_2_girls,
                           cluster = ~state_name,
                           boot.reps = 1000,
                           seed = 100)

rsq_4<- round(rsq(reg_4),digits = 3)

reg_5 <- glm(data = df_first_third_2_girls,
             formula = under5~factor(birth_order)*treat+
               state_name+
               factor(cohort)+
               sex+
               wait_time+
               mothers_age_at_birth+
               read_write_woman+
               religion+
               caste+
               type_of_house+
               state_name:seq_along((cohort)^2),
             family = "gaussian")

se_5 <- vcovHC(reg_5, 
               type = "HC0", 
               cluster = "state_name")

se_5 <- sqrt(diag(se_5))

pval_5<-  cluster.wild.glm(reg_5,
                           dat = df_first_third_2_girls,
                           cluster = ~state_name,
                           boot.reps = 1000,
                           seed = 100)

rsq_5<- round(rsq(reg_5),digits = 3)


reg_6 <- glm(data = df_second_third_2_girls,
             formula = under5~factor(birth_order)*treat+
               state_name+
               factor(cohort)+
               sex+
               wait_time+
               mothers_age_at_birth+
               read_write_woman+
               religion+
               caste+
               type_of_house+
               state_name:seq_along((cohort)^2),
             family = "gaussian")

se_6 <- vcovHC(reg_6, 
               type = "HC0", 
               cluster = "state_name")

se_6 <- sqrt(diag(se_6))

pval_6<-  cluster.wild.glm(reg_6,
                           dat = df_second_third_2_girls,
                           cluster = ~state_name,
                           boot.reps = 1000,
                           seed = 100)

rsq_6<- round(rsq(reg_6),digits = 3)

reg_7 <- glm(data = df_first_third_2_girls,
             formula = under10~factor(birth_order)*treat+
               state_name+
               factor(cohort)+
               sex+
               wait_time+
               mothers_age_at_birth+
               read_write_woman+
               religion+
               caste+
               type_of_house+
               state_name:seq_along((cohort)^2),
             family = "gaussian")

se_7 <- vcovHC(reg_7, 
               type = "HC0", 
               cluster = "state_name")

se_7 <- sqrt(diag(se_7))

pval_7<-  cluster.wild.glm(reg_7,
                           dat = df_first_third_2_girls,
                           cluster = ~state_name,
                           boot.reps = 1000,
                           seed = 100)

rsq_7<- round(rsq(reg_7),digits = 3)

reg_8 <- glm(data = df_second_third_2_girls,
             formula = under10~factor(birth_order)*treat+
               state_name+
               factor(cohort)+
               sex+
               wait_time+
               mothers_age_at_birth+
               read_write_woman+
               religion+
               caste+
               type_of_house+
               state_name:seq_along((cohort)^2),
             family = "gaussian")

se_8 <- vcovHC(reg_8, 
               type = "HC0", 
               cluster = "state_name")

se_8 <- sqrt(diag(se_8))

pval_8<-  cluster.wild.glm(reg_8,
                           dat = df_second_third_2_girls,
                           cluster = ~state_name,
                           boot.reps = 1000,
                           seed = 100)

rsq_8<- round(rsq(reg_8),digits = 3)

stargazer(reg_1,reg_3,reg_5,reg_7,reg_2,reg_4,reg_6,reg_8,
          se=list(se_1,se_3,se_5,se_7,se_2,se_4,se_6,se_8),
          p=c(pval_1,pval_3,pval_5,pval_7,pval_2,pval_4,pval_6,pval_8),
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
                    "^wait_time$"),
          covariate.labels = c("Chils is female",
                               "Mother's age at birth",
                               "Gap between births",
                               "(Birth order=3 vs 1)",
                               "Post-treat",
                               "(Birth order=3 vs 1)*post-treat"),
          add.lines = list(c("Time trend","x","x","x","x","x","x","x","x"),
                           c("Individual controls","x","x","x","x","x","x","x","x"),
                           c("State FE","x","x","x","x","x","x","x","x"),
                           c("Cohort FE","x","x","x","x","x","x","x","x"),
                           c("R-sq",rsq_1,rsq_3,rsq_5,rsq_7,rsq_2,rsq_4,rsq_6,rsq_8)),
          no.space = TRUE,
          omit.stat = c("aic","ll"),
          out = "Full_sample_reg_2_girls.tex")
