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
library(parallel)

setwd("C:\\Users\\Sagnik\\OneDrive - Cuny GradCenter\\Research\\Political reservation and age of marriage\\Github\\Political-reservation-and-age-of-marriage-age-of-first-birth\\Final version\\R results\\Regression_output")

df_1<- read.csv("C:\\Users\\Sagnik\\OneDrive - Cuny GradCenter\\Research\\Political reservation and age of marriage\\Github\\Political-reservation-and-age-of-marriage-age-of-first-birth\\Final version\\Cleaned datasets\\Preferred_sample_data_for_analysis_only_boys.csv")

df_2<- read.csv("C:\\Users\\Sagnik\\OneDrive - Cuny GradCenter\\Research\\Political reservation and age of marriage\\Github\\Political-reservation-and-age-of-marriage-age-of-first-birth\\Final version\\Cleaned datasets\\Preferred_sample_data_for_analysis_only_girls.csv")

############################## Data cleaning for first and second order comparison ##########################

df_first_second_boys<- df_1%>%                               ##### Selecting the data for birth order 2 vs 1 analysis ###########
subset(birth_order==1|birth_order==2 & !is.na(alive))

n_first_second<- data.frame(table(df_first_second_boys$id))

n_first_second[n_first_second$Freq>1,]

df_first_second_boys<-df_first_second_boys[df_first_second_boys$id %in% n_first_second$Var1[n_first_second$Freq > 1],]

df_first_second_boys<- df_first_second_boys[order(df_first_second_boys$id,df_first_second_boys$cohort),]

############################## Data cleaning for first and third order comparison ##########################

df_first_third_boys<- df_1%>%                               ##### Selecting the data for birth order 3 vs 1 analysis ###########
subset(birth_order==1|birth_order==3 & !is.na(alive))

n_first_third<- data.frame(table(df_first_third_boys$id))

n_first_third[n_first_third$Freq>1,]

df_first_third_boys<-df_first_third_boys[df_first_third_boys$id %in% n_first_third$Var1[n_first_third$Freq > 1],]

df_first_third_boys<- df_first_third_boys[order(df_first_third_boys$id,df_first_third_boys$cohort),]

############################## Data cleaning for second and third order comparison ##########################

df_second_third_boys<- df_1%>%                               ##### Selecting the data for birth order 3 vs 1 analysis ###########
subset(birth_order==2|birth_order==3 & !is.na(alive))

n_second_third<- data.frame(table(df_second_third_boys$id))

n_second_third[n_second_third$Freq>1,]

df_second_third_boys<-df_second_third_boys[df_second_third_boys$id %in% n_second_third$Var1[n_second_third$Freq > 1],]

df_second_third_boys<- df_second_third_boys[order(df_second_third_boys$id,df_second_third_boys$cohort),]


############################## Data cleaning for first and second order comparison ##########################

df_first_second_girls<- df_2%>%                               ##### Selecting the data for birth order 2 vs 1 analysis ###########
subset(birth_order==1|birth_order==2 & !is.na(alive))

n_first_second<- data.frame(table(df_first_second_girls$id))

n_first_second[n_first_second$Freq>1,]

df_first_second_girls<-df_first_second_girls[df_first_second_girls$id %in% n_first_second$Var1[n_first_second$Freq > 1],]

df_first_second_girls<- df_first_second_girls[order(df_first_second_girls$id,df_first_second_girls$cohort),]

############################## Data cleaning for first and third order comparison ##########################

df_first_third_girls<- df_2%>%                               ##### Selecting the data for birth order 3 vs 1 analysis ###########
subset(birth_order==1|birth_order==3 & !is.na(alive))

n_first_third<- data.frame(table(df_first_third_girls$id))

n_first_third[n_first_third$Freq>1,]

df_first_third_girls<-df_first_third_girls[df_first_third_girls$id %in% n_first_third$Var1[n_first_third$Freq > 1],]

df_first_third_girls<- df_first_third_girls[order(df_first_third_girls$id,df_first_third_girls$cohort),]

############################## Data cleaning for second and third order comparison ##########################

df_second_third_girls<- df_2%>%                               ##### Selecting the data for birth order 3 vs 1 analysis ###########
subset(birth_order==2|birth_order==3 & !is.na(alive))

n_second_third<- data.frame(table(df_second_third_girls$id))

n_second_third[n_second_third$Freq>1,]

df_second_third_girls<-df_second_third_girls[df_second_third_girls$id %in% n_second_third$Var1[n_second_third$Freq > 1],]

df_second_third_girls<- df_second_third_girls[order(df_second_third_girls$id,df_second_third_girls$cohort),]


options(boot.ncups=8)

#################### Neonatal survivability #############################

########################################### First vs second birth order comparison ##########################

reg_1<- glm(data = df_first_second_boys,
           formula = neonat~factor(birth_order)*treat+
             factor(state_name)+
             factor(cohort)+
             mothers_age_at_birth+
             wait_time+
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
                           dat = df_first_second_boys,
                           cluster = ~state_name,
                           boot.reps = 1000,
                           seed = 100)

rsq_1<- round(rsq(reg_1),digits = 3)

reg_2<- glm(data = df_first_second_girls,
           formula = neonat~factor(birth_order)*treat+
             factor(state_name)+
             factor(cohort)+
             mothers_age_at_birth+
             wait_time+
             read_write_woman+
             religion+
             caste+
             type_of_house+
             state_name:seq_along((cohort)^2),
           family = "gaussian") 

se_2<- vcovHC(reg_2, 
               type = "HC0", 
               cluster = "state_name")

se_2<- sqrt(diag(se_2))

pval_2<-  cluster.wild.glm(reg_2,
                           dat = df_first_second_girls,
                           cluster = ~state_name,
                           boot.reps = 1000,
                           seed = 100)

rsq_2<- round(rsq(reg_2),digits = 3)

########################################### First vs third birth order comparison ##########################

reg_3<- glm(data = df_first_third_boys,
           formula = neonat~factor(birth_order)*treat+
             factor(state_name)+
             factor(cohort)+
             mothers_age_at_birth+
             wait_time+
             read_write_woman+
             religion+
             caste+
             type_of_house+
             state_name:seq_along((cohort)^2),
           family = "gaussian") 

se_3<- vcovHC(reg_3, 
               type = "HC0", 
               cluster = "state_name")

se_3<- sqrt(diag(se_3))

pval_3<-  cluster.wild.glm(reg_3,
                           dat = df_first_third_boys,
                           cluster = ~state_name,
                           boot.reps = 1000,
                           seed = 100)

rsq_3<- round(rsq(reg_3),digits = 3)


reg_4<- glm(data = df_first_third_girls,
           formula = neonat~factor(birth_order)*treat+
             factor(state_name)+
             factor(cohort)+
             mothers_age_at_birth+
             wait_time+
             read_write_woman+
             religion+
             caste+
             type_of_house+
             state_name:seq_along((cohort)^2),
           family = "gaussian") 

se_4<- vcovHC(reg_4, 
              type = "HC0", 
              cluster = "state_name")

se_4<- sqrt(diag(se_4))

pval_4<-  cluster.wild.glm(reg_4,
                           dat = df_first_third_girls,
                           cluster = ~state_name,
                           boot.reps = 1000,
                           seed = 100)

rsq_4<- round(rsq(reg_4),digits = 3)

########################################### Second vs third birth order comparison ##########################

reg_5<- glm(data = df_second_third_boys,
           formula = neonat~factor(birth_order)*treat+
             factor(state_name)+
             factor(cohort)+
             mothers_age_at_birth+
             wait_time+
             read_write_woman+
             religion+
             caste+
             type_of_house+
             state_name:seq_along((cohort)^2),
           family = "gaussian") 

se_5<- vcovHC(reg_5, 
              type = "HC0", 
              cluster = "state_name")

se_5<- sqrt(diag(se_5))

pval_5<-  cluster.wild.glm(reg_5,
                           dat = df_second_third_boys,
                           cluster = ~state_name,
                           boot.reps = 1000,
                           seed = 100)

rsq_5<- round(rsq(reg_5),digits = 3)

reg_6<- glm(data = df_second_third_girls,
           formula = neonat~factor(birth_order)*treat+
             factor(state_name)+
             factor(cohort)+
             mothers_age_at_birth+
             wait_time+
             read_write_woman+
             religion+
             caste+
             type_of_house+
             state_name:seq_along((cohort)^2),
           family = "gaussian") 

se_6<- vcovHC(reg_6, 
              type = "HC0", 
              cluster = "state_name")

se_6<- sqrt(diag(se_6))

pval_6<-  cluster.wild.glm(reg_6,
                           dat = df_second_third_girls,
                           cluster = ~state_name,
                           boot.reps = 1000,
                           seed = 100)

rsq_6<- round(rsq(reg_6),digits = 3)

stargazer(reg_1,reg_2,reg_3,reg_4,reg_5,reg_6,
          se=list(se_1,se_2,se_3,se_4,se_5,se_6),
          p=c(pval_1,pval_2,pval_3,pval_4,pval_5,pval_6),
          omit = c("state_name",
                   "cohort",
                   "read_write_woman",
                   "caste",
                   "religion",
                   "type_of_house",
                   "Constant"),
          order = c("^factor(birth_order)2:treat$",
                    "^factor(birth_order)3:treat$",
                    "^mothers_age_at_birth$",
                    "^wait_time$"),
          covariate.labels = c("Mother's age at birth",
                               "Gap between births",
                               "(Birth order=2 vs 1)",
                               "(Birth order=3 vs 1)",
                               "Post-treat",
                               "(Birth order=2 vs 1)*post-treat",
                               "(Birth order=3 vs 1)*post-treat"),
          add.lines = list(c("Time-trend","x","x","x","x","x","x"),
                           c("Individual controls","x","x","x","x","x","x"),
                           c("State FE","x","x","x","x","x","x"),
                           c("Cohort FE","x","x","x","x","x","x"),
                           c("R-sq",rsq_1,rsq_2,rsq_3,rsq_4,rsq_5,rsq_6)),
          omit.stat = c("ll","aic"),
          no.space = TRUE,
          out = "Neonatal_reg_boys_girls.tex")


#################### Infant survivability #############################

########################################### First vs second birth order comparison ##########################

reg_1<- glm(data = df_first_second_boys,
            formula = infant~factor(birth_order)*treat+
              factor(state_name)+
              factor(cohort)+
              mothers_age_at_birth+
              wait_time+
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
                           dat = df_first_second_boys,
                           cluster = ~state_name,
                           boot.reps = 1000,
                           seed = 100)

rsq_1<- round(rsq(reg_1),digits = 3)

reg_2<- glm(data = df_first_second_girls,
            formula = infant~factor(birth_order)*treat+
              factor(state_name)+
              factor(cohort)+
              mothers_age_at_birth+
              wait_time+
              read_write_woman+
              religion+
              caste+
              type_of_house+
              state_name:seq_along((cohort)^2),
            family = "gaussian") 

se_2<- vcovHC(reg_2, 
              type = "HC0", 
              cluster = "state_name")

se_2<- sqrt(diag(se_2))

pval_2<-  cluster.wild.glm(reg_2,
                           dat = df_first_second_girls,
                           cluster = ~state_name,
                           boot.reps = 1000,
                           seed = 100)

rsq_2<- round(rsq(reg_2),digits = 3)

########################################### First vs third birth order comparison ##########################

reg_3<- glm(data = df_first_third_boys,
            formula = infant~factor(birth_order)*treat+
              factor(state_name)+
              factor(cohort)+
              mothers_age_at_birth+
              wait_time+
              read_write_woman+
              religion+
              caste+
              type_of_house+
              state_name:seq_along((cohort)^2),
            family = "gaussian") 

se_3<- vcovHC(reg_3, 
              type = "HC0", 
              cluster = "state_name")

se_3<- sqrt(diag(se_3))

pval_3<-  cluster.wild.glm(reg_3,
                           dat = df_first_third_boys,
                           cluster = ~state_name,
                           boot.reps = 1000,
                           seed = 100)

rsq_3<- round(rsq(reg_3),digits = 3)


reg_4<- glm(data = df_first_third_girls,
            formula = infant~factor(birth_order)*treat+
              factor(state_name)+
              factor(cohort)+
              mothers_age_at_birth+
              wait_time+
              read_write_woman+
              religion+
              caste+
              type_of_house+
              state_name:seq_along((cohort)^2),
            family = "gaussian") 

se_4<- vcovHC(reg_4, 
              type = "HC0", 
              cluster = "state_name")

se_4<- sqrt(diag(se_4))

pval_4<-  cluster.wild.glm(reg_4,
                           dat = df_first_third_girls,
                           cluster = ~state_name,
                           boot.reps = 1000,
                           seed = 100)

rsq_4<- round(rsq(reg_4),digits = 3)

########################################### Second vs third birth order comparison ##########################

reg_5<- glm(data = df_second_third_boys,
            formula = infant~factor(birth_order)*treat+
              factor(state_name)+
              factor(cohort)+
              mothers_age_at_birth+
              wait_time+
              read_write_woman+
              religion+
              caste+
              type_of_house+
              state_name:seq_along((cohort)^2),
            family = "gaussian") 

se_5<- vcovHC(reg_5, 
              type = "HC0", 
              cluster = "state_name")

se_5<- sqrt(diag(se_5))

pval_5<-  cluster.wild.glm(reg_5,
                           dat = df_second_third_boys,
                           cluster = ~state_name,
                           boot.reps = 1000,
                           seed = 100)

rsq_5<- round(rsq(reg_5),digits = 3)

reg_6<- glm(data = df_second_third_girls,
            formula = infant~factor(birth_order)*treat+
              factor(state_name)+
              factor(cohort)+
              mothers_age_at_birth+
              wait_time+
              read_write_woman+
              religion+
              caste+
              type_of_house+
              state_name:seq_along((cohort)^2),
            family = "gaussian") 

se_6<- vcovHC(reg_6, 
              type = "HC0", 
              cluster = "state_name")

se_6<- sqrt(diag(se_6))

pval_6<-  cluster.wild.glm(reg_6,
                           dat = df_second_third_girls,
                           cluster = ~state_name,
                           boot.reps = 1000,
                           seed = 100)

rsq_6<- round(rsq(reg_6),digits = 3)

stargazer(reg_1,reg_2,reg_3,reg_4,reg_5,reg_6,
          se=list(se_1,se_2,se_3,se_4,se_5,se_6),
          p=c(pval_1,pval_2,pval_3,pval_4,pval_5,pval_6),
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
          covariate.labels = c("Mother's age at birth",
                               "Gap between births",
                               "(Birth order=2 vs 1)",
                               "(Birth order=3 vs 1)",
                               "Post-treat",
                               "(Birth order=2 vs 1)*post-treat",
                               "(Birth order=3 vs 1)*post-treat"),
          add.lines = list(c("Time-trend","x","x","x","x","x","x"),
                           c("Individual controls","x","x","x","x","x","x"),
                           c("State FE","x","x","x","x","x","x"),
                           c("Cohort FE","x","x","x","x","x","x"),
                           c("R-sq",rsq_1,rsq_2,rsq_3,rsq_4,rsq_5,rsq_6)),
          omit.stat = c("ll","aic"),
          no.space = TRUE,
          out = "Infant_reg_boys_girls.tex")


#################### Under5 survivability #############################

########################################### First vs second birth order comparison ##########################

reg_1<- glm(data = df_first_second_boys,
            formula = under5~factor(birth_order)*treat+
              factor(state_name)+
              factor(cohort)+
              mothers_age_at_birth+
              wait_time+
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
                           dat = df_first_second_boys,
                           cluster = ~state_name,
                           boot.reps = 1000,
                           seed = 100)

rsq_1<- round(rsq(reg_1),digits = 3)

reg_2<- glm(data = df_first_second_girls,
            formula = under5~factor(birth_order)*treat+
              factor(state_name)+
              factor(cohort)+
              mothers_age_at_birth+
              wait_time+
              read_write_woman+
              religion+
              caste+
              type_of_house+
              state_name:seq_along((cohort)^2),
            family = "gaussian") 

se_2<- vcovHC(reg_2, 
              type = "HC0", 
              cluster = "state_name")

se_2<- sqrt(diag(se_2))

pval_2<-  cluster.wild.glm(reg_2,
                           dat = df_first_second_girls,
                           cluster = ~state_name,
                           boot.reps = 1000,
                           seed = 100)

rsq_2<- round(rsq(reg_2),digits = 3)

########################################### First vs third birth order comparison ##########################

reg_3<- glm(data = df_first_third_boys,
            formula = under5~factor(birth_order)*treat+
              factor(state_name)+
              factor(cohort)+
              mothers_age_at_birth+
              wait_time+
              read_write_woman+
              religion+
              caste+
              type_of_house+
              state_name:seq_along((cohort)^2),
            family = "gaussian") 

se_3<- vcovHC(reg_3, 
              type = "HC0", 
              cluster = "state_name")

se_3<- sqrt(diag(se_3))

pval_3<-  cluster.wild.glm(reg_,
                           dat = df_first_third_boys,
                           cluster = ~state_name,
                           boot.reps = 1000,
                           seed = 100)

rsq_3<- round(rsq(reg_3),digits = 3)


reg_4<- glm(data = df_first_third_girls,
            formula = under5~factor(birth_order)*treat+
              factor(state_name)+
              factor(cohort)+
              mothers_age_at_birth+
              wait_time+
              read_write_woman+
              religion+
              caste+
              type_of_house+
              state_name:seq_along((cohort)^2),
            family = "gaussian") 

se_4<- vcovHC(reg_4, 
              type = "HC0", 
              cluster = "state_name")

se_4<- sqrt(diag(se_4))

pval_4<-  cluster.wild.glm(reg_4,
                           dat = df_first_third_girls,
                           cluster = ~state_name,
                           boot.reps = 1000,
                           seed = 100)

rsq_4<- round(rsq(reg_4),digits = 3)

########################################### Second vs third birth order comparison ##########################

reg_5<- glm(data = df_second_third_boys,
            formula = under5~factor(birth_order)*treat+
              factor(state_name)+
              factor(cohort)+
              mothers_age_at_birth+
              wait_time+
              read_write_woman+
              religion+
              caste+
              type_of_house+
              state_name:seq_along((cohort)^2),
            family = "gaussian") 

se_5<- vcovHC(reg_5, 
              type = "HC0", 
              cluster = "state_name")

se_5<- sqrt(diag(se_5))

pval_5<-  cluster.wild.glm(reg_5,
                           dat = df_second_third_boys,
                           cluster = ~state_name,
                           boot.reps = 1000,
                           seed = 100)

rsq_5<- round(rsq(reg_5),digits = 3)

reg_6<- glm(data = df_second_third_girls,
            formula = under5~factor(birth_order)*treat+
              factor(state_name)+
              factor(cohort)+
              mothers_age_at_birth+
              wait_time+
              read_write_woman+
              religion+
              caste+
              type_of_house+
              state_name:seq_along((cohort)^2),
            family = "gaussian") 

se_6<- vcovHC(reg_6, 
              type = "HC0", 
              cluster = "state_name")

se_6<- sqrt(diag(se_6))

pval_6<-  cluster.wild.glm(reg_6,
                           dat = df_second_third_girls,
                           cluster = ~state_name,
                           boot.reps = 1000,
                           seed = 100)

rsq_6<- round(rsq(reg_6),digits = 3)

stargazer(reg_1,reg_2,reg_3,reg_4,reg_5,reg_6,
          se=list(se_1,se_2,se_3,se_4,se_5,se_6),
          p=c(pval_1,pval_2,pval_3,pval_4,pval_5,pval_6),
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
          covariate.labels = c("Sex of the child",
                               "Mother's age at birth",
                               "Gap between births",
                               "(Birth order=2 vs 1)",
                               "(Birth order=3 vs 1)",
                               "Post-treat",
                               "(Birth order=2 vs 1)*post-treat",
                               "(Birth order=3 vs 1)*post-treat"),
          add.lines = list(c("Time-trend","x","x","x","x","x","x"),
                           c("Individual controls","x","x","x","x","x","x"),
                           c("State FE","x","x","x","x","x","x"),
                           c("Cohort FE","x","x","x","x","x","x"),
                           c("R-sq",rsq_1,rsq_2,rsq_3,rsq_4,rsq_5,rsq_6)),
          omit.stat = c("ll","aic"),
          no.space = TRUE,
          out = "Under5_reg_boys_girls.tex")

#################### Under 10 survivability #############################

########################################### First vs second birth order comparison ##########################

reg_1<- glm(data = df_first_second_boys,
            formula = under10~factor(birth_order)*treat+
              factor(state_name)+
              factor(cohort)+
              mothers_age_at_birth+
              wait_time+
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
                           dat = df_first_second_boys,
                           cluster = ~state_name,
                           boot.reps = 1000,
                           seed = 100)

rsq_1<- round(rsq(reg_1),digits = 3)

reg_2<- glm(data = df_first_second_girls,
            formula = under10~factor(birth_order)*treat+
              factor(state_name)+
              factor(cohort)+
              mothers_age_at_birth+
              wait_time+
              read_write_woman+
              religion+
              caste+
              type_of_house+
              state_name:seq_along((cohort)^2),
            family = "gaussian") 

se_2<- vcovHC(reg_2, 
              type = "HC0", 
              cluster = "state_name")

se_2<- sqrt(diag(se_2))

pval_2<-  cluster.wild.glm(reg_2,
                           dat = df_first_second_girls,
                           cluster = ~state_name,
                           boot.reps = 1000,
                           seed = 100)

rsq_2<- round(rsq(reg_2),digits = 3)

########################################### First vs third birth order comparison ##########################

reg_3<- glm(data = df_first_third_boys,
            formula = under10~factor(birth_order)*treat+
              factor(state_name)+
              factor(cohort)+
              mothers_age_at_birth+
              wait_time+
              read_write_woman+
              religion+
              caste+
              type_of_house+
              state_name:seq_along((cohort)^2),
            family = "gaussian") 

se_3<- vcovHC(reg_3, 
              type = "HC0", 
              cluster = "state_name")

se_3<- sqrt(diag(se_3))

pval_3<-  cluster.wild.glm(reg_3,
                           dat = df_first_third_boys,
                           cluster = ~state_name,
                           boot.reps = 1000,
                           seed = 100)

rsq_3<- round(rsq(reg_3),digits = 3)


reg_4<- glm(data = df_first_third_girls,
            formula = under10~factor(birth_order)*treat+
              factor(state_name)+
              factor(cohort)+
              mothers_age_at_birth+
              wait_time+
              read_write_woman+
              religion+
              caste+
              type_of_house+
              state_name:seq_along((cohort)^2),
            family = "gaussian") 

se_4<- vcovHC(reg_4, 
              type = "HC0", 
              cluster = "state_name")

se_4<- sqrt(diag(se_4))

pval_4<-  cluster.wild.glm(reg_4,
                           dat = df_first_third_girls,
                           cluster = ~state_name,
                           boot.reps = 1000,
                           seed = 100)

rsq_4<- round(rsq(reg_4),digits = 3)

########################################### Second vs third birth order comparison ##########################

reg_5<- glm(data = df_second_third_boys,
            formula = under10~factor(birth_order)*treat+
              factor(state_name)+
              factor(cohort)+
              mothers_age_at_birth+
              wait_time+
              read_write_woman+
              religion+
              caste+
              type_of_house+
              state_name:seq_along((cohort)^2),
            family = "gaussian") 

se_5<- vcovHC(reg_5, 
              type = "HC0", 
              cluster = "state_name")

se_5<- sqrt(diag(se_5))

pval_5<-  cluster.wild.glm(reg_5,
                           dat = df_second_third_boys,
                           cluster = ~state_name,
                           boot.reps = 1000,
                           seed = 100)

rsq_5<- round(rsq(reg_5),digits = 3)

reg_6<- glm(data = df_second_third_girls,
            formula = under10~factor(birth_order)*treat+
              factor(state_name)+
              factor(cohort)+
              mothers_age_at_birth+
              wait_time+
              read_write_woman+
              religion+
              caste+
              type_of_house+
              state_name:seq_along((cohort)^2),
            family = "gaussian") 

se_6<- vcovHC(reg_6, 
              type = "HC0", 
              cluster = "state_name")

se_6<- sqrt(diag(se_6))

pval_6<-  cluster.wild.glm(reg_6,
                           dat = df_second_third_girls,
                           cluster = ~state_name,
                           boot.reps = 1000,
                           seed = 100)

rsq_6<- round(rsq(reg_6),digits = 3)

stargazer(reg_1,reg_2,reg_3,reg_4,reg_5,reg_6,
          se=list(se_1,se_2,se_3,se_4,se_5,se_6),
          p=c(pval_1,pval_2,pval_3,pval_4,pval_5,pval_6),
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
          covariate.labels = c("Sex of the child",
                               "Mother's age at birth",
                               "Gap between births",
                               "(Birth order=2 vs 1)",
                               "(Birth order=3 vs 1)",
                               "Post-treat",
                               "(Birth order=2 vs 1)*post-treat",
                               "(Birth order=3 vs 1)*post-treat"),
          add.lines = list(c("Time-trend","x","x","x","x","x","x"),
                           c("Individual controls","x","x","x","x","x","x"),
                           c("State FE","x","x","x","x","x","x"),
                           c("Cohort FE","x","x","x","x","x","x"),
                           c("R-sq",rsq_1,rsq_2,rsq_3,rsq_4,rsq_5,rsq_6)),
          omit.stat = c("ll","aic"),
          no.space = TRUE,
          out = "Under10_reg_boys_girls.tex")

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

#################### Neonatal survivability #############################

########################################### First vs second birth order comparison ##########################

reg_1<- glm(data = df_first_second_boys_boys,
           formula = neonat~factor(birth_order)*treat+
             factor(state_name)+
             factor(cohort)+
             mothers_age_at_birth+
             wait_time+
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
                           dat = df_first_second_boys_boys,
                           cluster = ~state_name,
                           boot.reps = 1000,
                           seed = 100)

rsq_1<- round(rsq(reg_1),digits = 3)

reg_2<- glm(data = df_first_second_boys_girls,
           formula = neonat~factor(birth_order)*treat+
             factor(state_name)+
             factor(cohort)+
             mothers_age_at_birth+
             wait_time+
             read_write_woman+
             religion+
             caste+
             type_of_house+
             state_name:seq_along((cohort)^2),
           family = "gaussian") 

se_2<- vcovHC(reg_2, 
              type = "HC0", 
              cluster = "state_name")

se_2<- sqrt(diag(se_2))

pval_2<-  cluster.wild.glm(reg_1,
                           dat = df_first_second_boys_girls,
                           cluster = ~state_name,
                           boot.reps = 1000,
                           seed = 100)

rsq_2<- round(rsq(reg_2),digits = 3)

########################################### First vs third birth order comparison ##########################

reg_3<- glm(data = df_first_third_boys_boys,
           formula = neonat~factor(birth_order)*treat+
             factor(state_name)+
             factor(cohort)+
             mothers_age_at_birth+
             wait_time+
             read_write_woman+
             religion+
             caste+
             type_of_house+
             state_name:seq_along((cohort)^2),
           family = "gaussian") 

se_3<- vcovHC(reg_3, 
              type = "HC0", 
              cluster = "state_name")

se_3<- sqrt(diag(se_3))

pval_3<-  cluster.wild.glm(reg_3,
                           dat = df_first_third_boys_boys,
                           cluster = ~state_name,
                           boot.reps = 1000,
                           seed = 100)

rsq_3<- round(rsq(reg_3),digits = 3)


reg_4<- glm(data = df_first_third_boys_girls,
           formula = neonat~factor(birth_order)*treat+
             factor(state_name)+
             factor(cohort)+
             mothers_age_at_birth+
             wait_time+
             read_write_woman+
             religion+
             caste+
             type_of_house+
             state_name:seq_along((cohort)^2),
           family = "gaussian") 

se_4<- vcovHC(reg_4, 
              type = "HC0", 
              cluster = "state_name")

se_4<- sqrt(diag(se_4))

pval_4<-  cluster.wild.glm(reg_4,
                           dat = df_first_third_boys_girls,
                           cluster = ~state_name,
                           boot.reps = 1000,
                           seed = 100)

rsq_4<- round(rsq(reg_4),digits = 3)

########################################### Second vs third birth order comparison ##########################

reg_5<- glm(data = df_second_third_boys_boys,
           formula = neonat~factor(birth_order)*treat+
             factor(state_name)+
             factor(cohort)+
             mothers_age_at_birth+
             wait_time+
             read_write_woman+
             religion+
             caste+
             type_of_house+
             state_name:seq_along((cohort)^2),
           family = "gaussian") 

se_5<- vcovHC(reg_5, 
              type = "HC0", 
              cluster = "state_name")

se_5<- sqrt(diag(se_5))

reg_6<- glm(data = df_second_third_boys_girls,
           formula = neonat~factor(birth_order)*treat+
             factor(state_name)+
             factor(cohort)+
             mothers_age_at_birth+
             wait_time+
             read_write_woman+
             religion+
             caste+
             type_of_house+
             state_name:seq_along((cohort)^2),
           family = "gaussian") 

se_6<- vcovHC(reg_6, 
              type = "HC0", 
              cluster = "state_name")

se_6<- sqrt(diag(se_6))

pval_6<-  cluster.wild.glm(reg_6,
                           dat = df_second_third_boys_girls,
                           cluster = ~state_name,
                           boot.reps = 1000,
                           seed = 100)

rsq_6<- round(rsq(reg_6),digits = 3)

stargazer(reg_1,reg_2,reg_3,reg_4,reg_5,reg_6,
          se=list(se_1,se_2,se_3,se_4,se_5,se_6),
          p=c(pval_1,pval_2,pval_3,pval_4,pval_5,pval_6),
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
          covariate.labels = c("Sex of the child",
                               "Mother's age at birth",
                               "Gap between births",
                               "(Birth order=2 vs 1)",
                               "(Birth order=3 vs 1)",
                               "Post-treat",
                               "(Birth order=2 vs 1)*post-treat",
                               "(Birth order=3 vs 1)*post-treat"),
          add.lines = list(c("Time-trend","x","x","x","x","x","x"),
                           c("Individual controls","x","x","x","x","x","x"),
                           c("State FE","x","x","x","x","x","x"),
                           c("Cohort FE","x","x","x","x","x","x"),
                           c("R-sq",rsq_1,rsq_2,rsq_3,rsq_4,rsq_5,rsq_6)),
          omit.stat = c("ll","aic"),
          no.space = TRUE,
          out = "Neonatal_reg_boys_girls_1st_boy.tex")

#################### Infant survivability #############################

########################################### First vs second birth order comparison ##########################

reg_1<- glm(data = df_first_second_boys_boys,
            formula = infant~factor(birth_order)*treat+
              factor(state_name)+
              factor(cohort)+
              mothers_age_at_birth+
              wait_time+
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
                           dat = df_first_second_boys_boys,
                           cluster = ~state_name,
                           boot.reps = 1000,
                           seed = 100)

rsq_1<- round(rsq(reg_1),digits = 3)

reg_2<- glm(data = df_first_second_boys_girls,
            formula = infant~factor(birth_order)*treat+
              factor(state_name)+
              factor(cohort)+
              mothers_age_at_birth+
              wait_time+
              read_write_woman+
              religion+
              caste+
              type_of_house+
              state_name:seq_along((cohort)^2),
            family = "gaussian") 

se_2<- vcovHC(reg_2, 
              type = "HC0", 
              cluster = "state_name")

se_2<- sqrt(diag(se_2))

pval_2<-  cluster.wild.glm(reg_1,
                           dat = df_first_second_boys_girls,
                           cluster = ~state_name,
                           boot.reps = 1000,
                           seed = 100)

rsq_2<- round(rsq(reg_2),digits = 3)

########################################### First vs third birth order comparison ##########################

reg_3<- glm(data = df_first_third_boys_boys,
            formula = infant~factor(birth_order)*treat+
              factor(state_name)+
              factor(cohort)+
              mothers_age_at_birth+
              wait_time+
              read_write_woman+
              religion+
              caste+
              type_of_house+
              state_name:seq_along((cohort)^2),
            family = "gaussian") 

se_3<- vcovHC(reg_3, 
              type = "HC0", 
              cluster = "state_name")

se_3<- sqrt(diag(se_3))

pval_3<-  cluster.wild.glm(reg_3,
                           dat = df_first_third_boys_boys,
                           cluster = ~state_name,
                           boot.reps = 1000,
                           seed = 100)

rsq_3<- round(rsq(reg_3),digits = 3)


reg_4<- glm(data = df_first_third_boys_girls,
            formula = infant~factor(birth_order)*treat+
              factor(state_name)+
              factor(cohort)+
              mothers_age_at_birth+
              wait_time+
              read_write_woman+
              religion+
              caste+
              type_of_house+
              state_name:seq_along((cohort)^2),
            family = "gaussian") 

se_4<- vcovHC(reg_4, 
              type = "HC0", 
              cluster = "state_name")

se_4<- sqrt(diag(se_4))

pval_4<-  cluster.wild.glm(reg_4,
                           dat = df_first_third_boys_girls,
                           cluster = ~state_name,
                           boot.reps = 1000,
                           seed = 100)

rsq_4<- round(rsq(reg_4),digits = 3)

########################################### Second vs third birth order comparison ##########################

reg_5<- glm(data = df_second_third_boys_boys,
            formula = infant~factor(birth_order)*treat+
              factor(state_name)+
              factor(cohort)+
              mothers_age_at_birth+
              wait_time+
              read_write_woman+
              religion+
              caste+
              type_of_house+
              state_name:seq_along((cohort)^2),
            family = "gaussian") 

se_5<- vcovHC(reg_5, 
              type = "HC0", 
              cluster = "state_name")

se_5<- sqrt(diag(se_5))

reg_6<- glm(data = df_second_third_boys_girls,
            formula = infant~factor(birth_order)*treat+
              factor(state_name)+
              factor(cohort)+
              mothers_age_at_birth+
              wait_time+
              read_write_woman+
              religion+
              caste+
              type_of_house+
              state_name:seq_along((cohort)^2),
            family = "gaussian") 

se_6<- vcovHC(reg_6, 
              type = "HC0", 
              cluster = "state_name")

se_6<- sqrt(diag(se_6))

pval_6<-  cluster.wild.glm(reg_6,
                           dat = df_second_third_boys_girls,
                           cluster = ~state_name,
                           boot.reps = 1000,
                           seed = 100)

rsq_6<- round(rsq(reg_6),digits = 3)

stargazer(reg_1,reg_2,reg_3,reg_4,reg_5,reg_6,
          se=list(se_1,se_2,se_3,se_4,se_5,se_6),
          p=c(pval_1,pval_2,pval_3,pval_4,pval_5,pval_6),
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
          covariate.labels = c("Sex of the child",
                               "Mother's age at birth",
                               "Gap between births",
                               "(Birth order=2 vs 1)",
                               "(Birth order=3 vs 1)",
                               "Post-treat",
                               "(Birth order=2 vs 1)*post-treat",
                               "(Birth order=3 vs 1)*post-treat"),
          add.lines = list(c("Time-trend","x","x","x","x","x","x"),
                           c("Individual controls","x","x","x","x","x","x"),
                           c("State FE","x","x","x","x","x","x"),
                           c("Cohort FE","x","x","x","x","x","x"),
                           c("R-sq",rsq_1,rsq_2,rsq_3,rsq_4,rsq_5,rsq_6)),
          omit.stat = c("ll","aic"),
          no.space = TRUE,
          out = "Infant_reg_boys_girls_1st_boy.tex")


#################### Under 5 survivability #############################

########################################### First vs second birth order comparison ##########################

reg_1<- glm(data = df_first_second_boys_boys,
            formula = under5~factor(birth_order)*treat+
              factor(state_name)+
              factor(cohort)+
              mothers_age_at_birth+
              wait_time+
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
                           dat = df_first_second_boys_boys,
                           cluster = ~state_name,
                           boot.reps = 1000,
                           seed = 100)

rsq_1<- round(rsq(reg_1),digits = 3)

reg_2<- glm(data = df_first_second_boys_girls,
            formula = under5~factor(birth_order)*treat+
              factor(state_name)+
              factor(cohort)+
              mothers_age_at_birth+
              wait_time+
              read_write_woman+
              religion+
              caste+
              type_of_house+
              state_name:seq_along((cohort)^2),
            family = "gaussian") 

se_2<- vcovHC(reg_2, 
              type = "HC0", 
              cluster = "state_name")

se_2<- sqrt(diag(se_2))

pval_2<-  cluster.wild.glm(reg_1,
                           dat = df_first_second_boys_girls,
                           cluster = ~state_name,
                           boot.reps = 1000,
                           seed = 100)

rsq_2<- round(rsq(reg_2),digits = 3)

########################################### First vs third birth order comparison ##########################

reg_3<- glm(data = df_first_third_boys_boys,
            formula = under5~factor(birth_order)*treat+
              factor(state_name)+
              factor(cohort)+
              mothers_age_at_birth+
              wait_time+
              read_write_woman+
              religion+
              caste+
              type_of_house+
              state_name:seq_along((cohort)^2),
            family = "gaussian") 

se_3<- vcovHC(reg_3, 
              type = "HC0", 
              cluster = "state_name")

se_3<- sqrt(diag(se_3))

pval_3<-  cluster.wild.glm(reg_3,
                           dat = df_first_third_boys_boys,
                           cluster = ~state_name,
                           boot.reps = 1000,
                           seed = 100)

rsq_3<- round(rsq(reg_3),digits = 3)


reg_4<- glm(data = df_first_third_boys_girls,
            formula = under5~factor(birth_order)*treat+
              factor(state_name)+
              factor(cohort)+
              mothers_age_at_birth+
              wait_time+
              read_write_woman+
              religion+
              caste+
              type_of_house+
              state_name:seq_along((cohort)^2),
            family = "gaussian") 

se_4<- vcovHC(reg_4, 
              type = "HC0", 
              cluster = "state_name")

se_4<- sqrt(diag(se_4))

pval_4<-  cluster.wild.glm(reg_4,
                           dat = df_first_third_boys_girls,
                           cluster = ~state_name,
                           boot.reps = 1000,
                           seed = 100)

rsq_4<- round(rsq(reg_4),digits = 3)

########################################### Second vs third birth order comparison ##########################

reg_5<- glm(data = df_second_third_boys_boys,
            formula = under5~factor(birth_order)*treat+
              factor(state_name)+
              factor(cohort)+
              mothers_age_at_birth+
              wait_time+
              read_write_woman+
              religion+
              caste+
              type_of_house+
              state_name:seq_along((cohort)^2),
            family = "gaussian") 

se_5<- vcovHC(reg_5, 
              type = "HC0", 
              cluster = "state_name")

se_5<- sqrt(diag(se_5))

reg_6<- glm(data = df_second_third_boys_girls,
            formula = under5~factor(birth_order)*treat+
              factor(state_name)+
              factor(cohort)+
              mothers_age_at_birth+
              wait_time+
              read_write_woman+
              religion+
              caste+
              type_of_house+
              state_name:seq_along((cohort)^2),
            family = "gaussian") 

se_6<- vcovHC(reg_6, 
              type = "HC0", 
              cluster = "state_name")

se_6<- sqrt(diag(se_6))

pval_6<-  cluster.wild.glm(reg_6,
                           dat = df_second_third_boys_girls,
                           cluster = ~state_name,
                           boot.reps = 1000,
                           seed = 100)

rsq_6<- round(rsq(reg_6),digits = 3)

stargazer(reg_1,reg_2,reg_3,reg_4,reg_5,reg_6,
          se=list(se_1,se_2,se_3,se_4,se_5,se_6),
          p=c(pval_1,pval_2,pval_3,pval_4,pval_5,pval_6),
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
          covariate.labels = c("Sex of the child",
                               "Mother's age at birth",
                               "Gap between births",
                               "(Birth order=2 vs 1)",
                               "(Birth order=3 vs 1)",
                               "Post-treat",
                               "(Birth order=2 vs 1)*post-treat",
                               "(Birth order=3 vs 1)*post-treat"),
          add.lines = list(c("Time-trend","x","x","x","x","x","x"),
                           c("Individual controls","x","x","x","x","x","x"),
                           c("State FE","x","x","x","x","x","x"),
                           c("Cohort FE","x","x","x","x","x","x"),
                           c("R-sq",rsq_1,rsq_2,rsq_3,rsq_4,rsq_5,rsq_6)),
          omit.stat = c("ll","aic"),
          no.space = TRUE,
          out = "Under5_reg_boys_girls_1st_boy.tex")


#################### Under 10 survivability #############################

########################################### First vs second birth order comparison ##########################

reg_1<- glm(data = df_first_second_boys_boys,
            formula = under10~factor(birth_order)*treat+
              factor(state_name)+
              factor(cohort)+
              mothers_age_at_birth+
              wait_time+
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
                           dat = df_first_second_boys_boys,
                           cluster = ~state_name,
                           boot.reps = 1000,
                           seed = 100)

rsq_1<- round(rsq(reg_1),digits = 3)

reg_2<- glm(data = df_first_second_boys_girls,
            formula = under10~factor(birth_order)*treat+
              factor(state_name)+
              factor(cohort)+
              mothers_age_at_birth+
              wait_time+
              read_write_woman+
              religion+
              caste+
              type_of_house+
              state_name:seq_along((cohort)^2),
            family = "gaussian") 

se_2<- vcovHC(reg_2, 
              type = "HC0", 
              cluster = "state_name")

se_2<- sqrt(diag(se_2))

pval_2<-  cluster.wild.glm(reg_1,
                           dat = df_first_second_boys_girls,
                           cluster = ~state_name,
                           boot.reps = 1000,
                           seed = 100)

rsq_2<- round(rsq(reg_2),digits = 3)

########################################### First vs third birth order comparison ##########################

reg_3<- glm(data = df_first_third_boys_boys,
            formula = under10~factor(birth_order)*treat+
              factor(state_name)+
              factor(cohort)+
              mothers_age_at_birth+
              wait_time+
              read_write_woman+
              religion+
              caste+
              type_of_house+
              state_name:seq_along((cohort)^2),
            family = "gaussian") 

se_3<- vcovHC(reg_3, 
              type = "HC0", 
              cluster = "state_name")

se_3<- sqrt(diag(se_3))

pval_3<-  cluster.wild.glm(reg_3,
                           dat = df_first_third_boys_boys,
                           cluster = ~state_name,
                           boot.reps = 1000,
                           seed = 100)

rsq_3<- round(rsq(reg_3),digits = 3)


reg_4<- glm(data = df_first_third_boys_girls,
            formula = under10~factor(birth_order)*treat+
              factor(state_name)+
              factor(cohort)+
              mothers_age_at_birth+
              wait_time+
              read_write_woman+
              religion+
              caste+
              type_of_house+
              state_name:seq_along((cohort)^2),
            family = "gaussian") 

se_4<- vcovHC(reg_4, 
              type = "HC0", 
              cluster = "state_name")

se_4<- sqrt(diag(se_4))

pval_4<-  cluster.wild.glm(reg_4,
                           dat = df_first_third_boys_girls,
                           cluster = ~state_name,
                           boot.reps = 1000,
                           seed = 100)

rsq_4<- round(rsq(reg_4),digits = 3)

########################################### Second vs third birth order comparison ##########################

reg_5<- glm(data = df_second_third_boys_boys,
            formula = under10~factor(birth_order)*treat+
              factor(state_name)+
              factor(cohort)+
              mothers_age_at_birth+
              wait_time+
              read_write_woman+
              religion+
              caste+
              type_of_house+
              state_name:seq_along((cohort)^2),
            family = "gaussian") 

se_5<- vcovHC(reg_5, 
              type = "HC0", 
              cluster = "state_name")

se_5<- sqrt(diag(se_5))

reg_6<- glm(data = df_second_third_boys_girls,
            formula = under10~factor(birth_order)*treat+
              factor(state_name)+
              factor(cohort)+
              mothers_age_at_birth+
              wait_time+
              read_write_woman+
              religion+
              caste+
              type_of_house+
              state_name:seq_along((cohort)^2),
            family = "gaussian") 

se_6<- vcovHC(reg_6, 
              type = "HC0", 
              cluster = "state_name")

se_6<- sqrt(diag(se_6))

pval_6<-  cluster.wild.glm(reg_6,
                           dat = df_second_third_boys_girls,
                           cluster = ~state_name,
                           boot.reps = 1000,
                           seed = 100)

rsq_6<- round(rsq(reg_6),digits = 3)

stargazer(reg_1,reg_2,reg_3,reg_4,reg_5,reg_6,
          se=list(se_1,se_2,se_3,se_4,se_5,se_6),
          p=c(pval_1,pval_2,pval_3,pval_4,pval_5,pval_6),
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
          covariate.labels = c("Sex of the child",
                               "Mother's age at birth",
                               "Gap between births",
                               "(Birth order=2 vs 1)",
                               "(Birth order=3 vs 1)",
                               "Post-treat",
                               "(Birth order=2 vs 1)*post-treat",
                               "(Birth order=3 vs 1)*post-treat"),
          add.lines = list(c("Time-trend","x","x","x","x","x","x"),
                           c("Individual controls","x","x","x","x","x","x"),
                           c("State FE","x","x","x","x","x","x"),
                           c("Cohort FE","x","x","x","x","x","x"),
                           c("R-sq",rsq_1,rsq_2,rsq_3,rsq_4,rsq_5,rsq_6)),
          omit.stat = c("ll","aic"),
          no.space = TRUE,
          out = "Under10_reg_boys_girls_1st_boy.tex")

