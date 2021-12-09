library(plyr)
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
library(parallel)
library(ggeffects)
library(ggiraphExtra)
library(sjPlot)
library(margins)
library(effects)

memory.limit(size = 50000)


setwd("C:\\Users\\das90\\OneDrive - Cuny GradCenter\\Research\\Political reservation and age of marriage\\Github\\Political-reservation-and-age-of-marriage-age-of-first-birth\\Final version\\R results\\R_graphs")

df<- read.csv("C:\\Users\\das90\\OneDrive - Cuny GradCenter\\Research\\Political reservation and age of marriage\\Github\\Political-reservation-and-age-of-marriage-age-of-first-birth\\Final version\\Cleaned datasets\\Preferred_sample_data_for_analysis.csv")

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


####################### Logit regressions ########################

########################## Second vs first birth order ########################

df_first_second<- df_first_second%>%
  mutate(birth_order=as.factor(birth_order),
         treat=as.factor(treat))%>%
  mutate(birthorder=ifelse(birth_order==2,1,0))%>%
  mutate(treated=as.numeric(treat))%>%
  mutate(did=birthorder*treated)

########################### Neonatal survival probability ################################

reg_1<- glm(data = df_first_second,
            formula = neonat~birth_order*treat+
              state_name+
              cohort+
              sex+
              mothers_age_at_birth+
              wait_time+
              read_write_woman+
              religion+
              caste+
              type_of_house+
              state_name:seq_along((cohort)^2),
            family = "binomial")

png("pred_prob_neonat_1_2.png")

plot_model(reg_1,
           type = "pred",
           terms = c("treat","birth_order"))+
  geom_line(aes(linetype=group),size=2,show.legend = FALSE)+
  theme_bw()+
  xlab("Treatment status")+
  ylab("Predicted probability of neonatal survival survival")+
  ggtitle(" ")+
  labs(color="Birth order")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

dev.off()

mar_1<- margins(reg_1,variables=c("birth_order","treat"),type = "response",at=list(birthorder=0:1))

png("pred_prob_ME_neonat_1_2.png")

mar_1%>%
  subset(treat==1)%>%
  ggplot(aes(x=fitted,
             y=dydx_treat1,color=birth_order))+
  geom_point(size=4,
             alpha=0.5,
             shape=1)+
  geom_smooth(color="black")+
  theme_ipsum()+
  xlab("Predicted probability")+
  ylab("Marginal effect of treatment")+
  scale_color_discrete(name="Birth order")

dev.off()

########################## Infant survival probability ###############################

reg_2<- glm(data = df_first_second,
            formula = infant~birth_order*treat+
              state_name+
              cohort+
              sex+
              mothers_age_at_birth+
              wait_time+
              read_write_woman+
              religion+
              caste+
              type_of_house+
              state_name:seq_along((cohort)^2),
            family = "binomial")

png("pred_prob_infant_1_2.png")

plot_model(reg_2,
           type = "pred",
           terms = c("treat","birth_order"))+
  geom_line(aes(linetype=group),size=2,show.legend = FALSE)+
  theme_bw()+
  xlab("Treatment status")+
  ylab("Predicted probability of infant survival survival")+
  ggtitle(" ")+
  labs(color="Birth order")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

dev.off()

mar_2<- margins(reg_2,variables=c("birth_order","treat"),type = "response",at=list(birthorder=0:1))

png("pred_prob_ME_infant_1_2.png")

mar_2%>%
  subset(treat==1)%>%
  ggplot(aes(x=fitted,
             y=dydx_treat1,color=birth_order))+
  geom_point(size=4,
             alpha=0.5,
             shape=1)+
  geom_smooth(color="black")+
  theme_ipsum()+
  xlab("Predicted probability")+
  ylab("Marginal effect of treatment")+
  scale_color_discrete(name="Birth order")

dev.off()

########################## Under 5 survival probability ###############################

reg_3<- glm(data = df_first_second,
            formula = under5~birth_order*treat+
              state_name+
              cohort+
              sex+
              mothers_age_at_birth+
              wait_time+
              read_write_woman+
              religion+
              caste+
              type_of_house+
              state_name:seq_along((cohort)^2),
            family = "binomial")

png("pred_prob_under5_1_2.png")

plot_model(reg_3,
           type = "pred",
           terms = c("treat","birth_order"))+
  geom_line(aes(linetype=group),size=2,show.legend = FALSE)+
  theme_bw()+
  xlab("Treatment status")+
  ylab("Predicted probability of under 5 survival survival")+
  ggtitle(" ")+
  labs(color="Birth order")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

dev.off()

mar_3<- margins(reg_3,variables=c("birth_order","treat"),type = "response",at=list(birthorder=0:1))

png("pred_prob_ME_under5_1_2.png")

mar_3%>%
  subset(treat==1)%>%
  ggplot(aes(x=fitted,
             y=dydx_treat1,color=birth_order))+
  geom_point(size=4,
             alpha=0.5,
             shape=1)+
  geom_smooth(color="black")+
  theme_ipsum()+
  xlab("Predicted probability")+
  ylab("Marginal effect of treatment")+
  scale_color_discrete(name="Birth order")

dev.off()

########################## Under 10 survival probability ###############################

reg_4<- glm(data = df_first_second,
            formula = under10~birth_order*treat+
              state_name+
              cohort+
              sex+
              mothers_age_at_birth+
              wait_time+
              read_write_woman+
              religion+
              caste+
              type_of_house+
              state_name:seq_along((cohort)^2),
            family = "binomial")

png("pred_prob_under10_1_2.png")

plot_model(reg_4,
           type = "pred",
           terms = c("treat","birth_order"))+
  geom_line(aes(linetype=group),size=2,show.legend = FALSE)+
  theme_bw()+
  xlab("Treatment status")+
  ylab("Predicted probability of under 10 survival survival")+
  ggtitle(" ")+
  labs(color="Birth order")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

dev.off()

mar_4<- margins(reg_4,variables=c("birth_order","treat"),type = "response",at=list(birthorder=0:1))

png("pred_prob_ME_under10_1_2.png")

mar_4%>%
  subset(treat==1)%>%
  ggplot(aes(x=fitted,
             y=dydx_treat1,color=birth_order))+
  geom_point(size=4,
             alpha=0.5,
             shape=1)+
  geom_smooth(color="black")+
  theme_ipsum()+
  xlab("Predicted probability")+
  ylab("Marginal effect of treatment")+
  scale_color_discrete(name="Birth order")

dev.off()

########################## Third vs first birth order ########################

df_first_third<- df_first_third%>%
  mutate(birth_order=as.factor(birth_order),
         treat=as.factor(treat))%>%
  mutate(birthorder=ifelse(birth_order==3,1,0))%>%
  mutate(treated=as.numeric(treat))%>%
  mutate(did=birthorder*treated)

########################### Neonatal survival probability ################################

reg_1<- glm(data = df_first_third,
            formula = neonat~birth_order*treat+
              state_name+
              cohort+
              sex+
              mothers_age_at_birth+
              wait_time+
              read_write_woman+
              religion+
              caste+
              type_of_house+
              state_name:seq_along((cohort)^2),
            family = "binomial")

png("pred_prob_neonat_1_3.png")

plot_model(reg_1,
           type = "pred",
           terms = c("treat","birth_order"))+
  geom_line(aes(linetype=group),size=2,show.legend = FALSE)+
  theme_bw()+
  xlab("Treatment status")+
  ylab("Predicted probability of neonatal survival survival")+
  ggtitle(" ")+
  labs(color="Birth order")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

dev.off()

mar_1<- margins(reg_1,variables=c("birth_order","treat"),type = "response",at=list(birthorder=0:1))

png("pred_prob_ME_neonat_1_3.png")

mar_1%>%
  subset(treat==1)%>%
  ggplot(aes(x=fitted,
             y=dydx_treat1,color=birth_order))+
  geom_point(size=4,
             alpha=0.5,
             shape=1)+
  geom_smooth(color="black")+
  theme_ipsum()+
  xlab("Predicted probability")+
  ylab("Marginal effect of treatment")+
  scale_color_discrete(name="Birth order")

dev.off()

########################## Infant survival probability ###############################

reg_2<- glm(data = df_first_third,
            formula = infant~birth_order*treat+
              state_name+
              cohort+
              sex+
              mothers_age_at_birth+
              wait_time+
              read_write_woman+
              religion+
              caste+
              type_of_house+
              state_name:seq_along((cohort)^2),
            family = "binomial")

png("pred_prob_infant_1_3.png")

plot_model(reg_2,
           type = "pred",
           terms = c("treat","birth_order"))+
  geom_line(aes(linetype=group),size=2,show.legend = FALSE)+
  theme_bw()+
  xlab("Treatment status")+
  ylab("Predicted probability of infant survival survival")+
  ggtitle(" ")+
  labs(color="Birth order")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

dev.off()

mar_2<- margins(reg_2,variables=c("birth_order","treat"),type = "response",at=list(birthorder=0:1))

png("pred_prob_ME_infant_1_3.png")

mar_2%>%
  subset(treat==1)%>%
  ggplot(aes(x=fitted,
             y=dydx_treat1,color=birth_order))+
  geom_point(size=4,
             alpha=0.5,
             shape=1)+
  geom_smooth(color="black")+
  theme_ipsum()+
  xlab("Predicted probability")+
  ylab("Marginal effect of treatment")+
  scale_color_discrete(name="Birth order")

dev.off()

########################## Under 5 survival probability ###############################

reg_3<- glm(data = df_first_third,
            formula = under5~birth_order*treat+
              state_name+
              cohort+
              sex+
              mothers_age_at_birth+
              wait_time+
              read_write_woman+
              religion+
              caste+
              type_of_house+
              state_name:seq_along((cohort)^2),
            family = "binomial")

png("pred_prob_under5_1_3.png")

plot_model(reg_3,
           type = "pred",
           terms = c("treat","birth_order"))+
  geom_line(aes(linetype=group),size=2,show.legend = FALSE)+
  theme_bw()+
  xlab("Treatment status")+
  ylab("Predicted probability of under 5 survival survival")+
  ggtitle(" ")+
  labs(color="Birth order")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

dev.off()

mar_3<- margins(reg_3,variables=c("birth_order","treat"),type = "response",at=list(birthorder=0:1))

png("pred_prob_ME_under5_1_3.png")

mar_3%>%
  subset(treat==1)%>%
  ggplot(aes(x=fitted,
             y=dydx_treat1,color=birth_order))+
  geom_point(size=4,
             alpha=0.5,
             shape=1)+
  geom_smooth(color="black")+
  theme_ipsum()+
  xlab("Predicted probability")+
  ylab("Marginal effect of treatment")+
  scale_color_discrete(name="Birth order")

dev.off()

########################## Under 10 survival probability ###############################

reg_4<- glm(data = df_first_third,
            formula = under10~birth_order*treat+
              state_name+
              cohort+
              sex+
              mothers_age_at_birth+
              wait_time+
              read_write_woman+
              religion+
              caste+
              type_of_house+
              state_name:seq_along((cohort)^2),
            family = "binomial")

png("pred_prob_under10_1_3.png")

plot_model(reg_4,
           type = "pred",
           terms = c("treat","birth_order"))+
  geom_line(aes(linetype=group),size=2,show.legend = FALSE)+
  theme_bw()+
  xlab("Treatment status")+
  ylab("Predicted probability of under 10 survival survival")+
  ggtitle(" ")+
  labs(color="Birth order")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

dev.off()

mar_4<- margins(reg_4,variables=c("birth_order","treat"),type = "response",at=list(birthorder=0:1))

png("pred_prob_ME_under10_1_3.png")

mar_4%>%
  subset(treat==1)%>%
  ggplot(aes(x=fitted,
             y=dydx_treat1,color=birth_order))+
  geom_point(size=4,
             alpha=0.5,
             shape=1)+
  geom_smooth(color="black")+
  theme_ipsum()+
  xlab("Predicted probability")+
  ylab("Marginal effect of treatment")+
  scale_color_discrete(name="Birth order")

dev.off()

########################## Third vs second birth order ########################

df_second_third<- df_second_third%>%
  mutate(birth_order=as.factor(birth_order),
         treat=as.factor(treat))%>%
  mutate(birthorder=ifelse(birth_order==3,1,0))%>%
  mutate(treated=as.numeric(treat))%>%
  mutate(did=birthorder*treated)

########################### Neonatal survival probability ################################

reg_1<- glm(data = df_second_third,
            formula = neonat~birth_order*treat+
              state_name+
              cohort+
              sex+
              mothers_age_at_birth+
              wait_time+
              read_write_woman+
              religion+
              caste+
              type_of_house+
              state_name:seq_along((cohort)^2),
            family = "binomial")

png("pred_prob_neonat_2_3.png")

plot_model(reg_1,
           type = "pred",
           terms = c("treat","birth_order"))+
  geom_line(aes(linetype=group),size=2,show.legend = FALSE)+
  theme_bw()+
  xlab("Treatment status")+
  ylab("Predicted probability of neonatal survival survival")+
  ggtitle(" ")+
  labs(color="Birth order")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

dev.off()

mar_1<- margins(reg_1,variables=c("birth_order","treat"),type = "response",at=list(birthorder=0:1))

png("pred_prob_ME_infant_2_3.png")

mar_1%>%
  subset(treat==1)%>%
  ggplot(aes(x=fitted,
             y=dydx_treat1,color=birth_order))+
  geom_point(size=4,
             alpha=0.5,
             shape=1)+
  geom_smooth(color="black")+
  theme_ipsum()+
  xlab("Predicted probability")+
  ylab("Marginal effect of treatment")+
  scale_color_discrete(name="Birth order")

dev.off()

########################## Infant survival probability ###############################

reg_2<- glm(data = df_second_third,
            formula = infant~birth_order*treat+
              state_name+
              cohort+
              sex+
              mothers_age_at_birth+
              wait_time+
              read_write_woman+
              religion+
              caste+
              type_of_house+
              state_name:seq_along((cohort)^2),
            family = "binomial")

png("pred_prob_infant_2_3.png")

plot_model(reg_2,
           type = "pred",
           terms = c("treat","birth_order"))+
  geom_line(aes(linetype=group),size=2,show.legend = FALSE)+
  theme_bw()+
  xlab("Treatment status")+
  ylab("Predicted probability of infant survival survival")+
  ggtitle(" ")+
  labs(color="Birth order")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

dev.off()

mar_2<- margins(reg_2,variables=c("birth_order","treat"),type = "response",at=list(birthorder=0:1))

png("pred_prob_ME_infant_2_3.png")

mar_2%>%
  subset(treat==1)%>%
  ggplot(aes(x=fitted,
             y=dydx_treat1,color=birth_order))+
  geom_point(size=4,
             alpha=0.5,
             shape=1)+
  geom_smooth(color="black")+
  theme_ipsum()+
  xlab("Predicted probability")+
  ylab("Marginal effect of treatment")+
  scale_color_discrete(name="Birth order")

dev.off()

########################## Under 5 survival probability ###############################

reg_3<- glm(data = df_second_third,
            formula = under5~birth_order*treat+
              state_name+
              cohort+
              sex+
              mothers_age_at_birth+
              wait_time+
              read_write_woman+
              religion+
              caste+
              type_of_house+
              state_name:seq_along((cohort)^2),
            family = "binomial")

png("pred_prob_under5_2_3.png")

plot_model(reg_3,
           type = "pred",
           terms = c("treat","birth_order"))+
  geom_line(aes(linetype=group),size=2,show.legend = FALSE)+
  theme_bw()+
  xlab("Treatment status")+
  ylab("Predicted probability of under 5 survival survival")+
  ggtitle(" ")+
  labs(color="Birth order")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

dev.off()

mar_3<- margins(reg_3,variables=c("birth_order","treat"),type = "response",at=list(birthorder=0:1))

png("pred_prob_ME_under5_2_3.png")

mar_3%>%
  subset(treat==1)%>%
  ggplot(aes(x=fitted,
             y=dydx_treat1,color=birth_order))+
  geom_point(size=4,
             alpha=0.5,
             shape=1)+
  geom_smooth(color="black")+
  theme_ipsum()+
  xlab("Predicted probability")+
  ylab("Marginal effect of treatment")+
  scale_color_discrete(name="Birth order")

dev.off()

########################## Under 10 survival probability ###############################

reg_4<- glm(data = df_second_third,
            formula = under10~birth_order*treat+
              state_name+
              cohort+
              sex+
              mothers_age_at_birth+
              wait_time+
              read_write_woman+
              religion+
              caste+
              type_of_house+
              state_name:seq_along((cohort)^2),
            family = "binomial")

png("pred_prob_under10_2_3.png")

plot_model(reg_4,
           type = "pred",
           terms = c("treat","birth_order"))+
  geom_line(aes(linetype=group),size=2,show.legend = FALSE)+
  theme_bw()+
  xlab("Treatment status")+
  ylab("Predicted probability of under 10 survival survival")+
  ggtitle(" ")+
  labs(color="Birth order")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

dev.off()

mar_4<- margins(reg_4,variables=c("birth_order","treat"),type = "response",at=list(birthorder=0:1))

png("pred_prob_ME_under10_2_3.png")

mar_4%>%
  subset(treat==1)%>%
  ggplot(aes(x=fitted,
             y=dydx_treat1,color=birth_order))+
  geom_point(size=4,
             alpha=0.5,
             shape=1)+
  geom_smooth(color="black")+
  theme_ipsum()+
  xlab("Predicted probability")+
  ylab("Marginal effect of treatment")+
  scale_color_discrete(name="Birth order")

dev.off()