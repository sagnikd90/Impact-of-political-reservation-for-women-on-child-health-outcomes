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
library(did)
library(DRDID)

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

################## Callaway-Sant' Anna Event studies ##########################

df_first_second<- df_first_second%>%
  subset(!is.na(birth_order) &
           !is.na(cohort)&
           !is.na(infant))%>%
  mutate(birth_order=ifelse(birth_order==2,1,0))%>%
  mutate(election_year=birth_order*first_election)

df_first_second$state<- as.numeric(as.factor(df_first_second$state_name))

df_first_third<- df_first_third%>%
  subset(!is.na(birth_order) &
           !is.na(cohort)&
           !is.na(infant))%>%
  mutate(birth_order=ifelse(birth_order==3,1,0))%>%
  mutate(election_year=birth_order*first_election)

df_first_third$state<- as.numeric(as.factor(df_first_third$state_name))

df_second_third<- df_second_third%>%
  subset(!is.na(birth_order) &
           !is.na(cohort)&
           !is.na(infant))%>%
  mutate(birth_order=ifelse(birth_order==3,1,0))%>%
  mutate(election_year=birth_order*first_election)

df_first_second$state<- as.numeric(as.factor(df_first_second$state_name))


########################### First vs second ##########################

######################### Neonatal survival #####################

gt_1_2_neonat<- att_gt(y="neonat",
                gname = "election_year",
                tname = "cohort_year",
                xformla = ~1,
                est_method = "dr",
                data = df_first_second,
                control_group = c("nevertreated"),
                panel = FALSE,
                clustervars = "state_name",
                alp = 0.05)


evnt_study_1_2_neonat<-aggte(gt_1_2_neonat, 
                      type = "dynamic",
                      na.rm = TRUE,
                      min_e = -4,
                      max_e = 5)


png("event_study_1_2_neonat.png")

ggdid(evnt_study_1_2_neonat)+
  theme_bw() +
  labs(x="Time to election in years",y = "Estimate")+
  theme_bw()+
  ggtitle("")+
  geom_line(linetype="dashed")+
  geom_point(size=3)+
  scale_color_discrete(name="Treatment status",
                       labels=c("Pre","Post"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

dev.off()

######################### Infant survival #####################

gt_1_2_infant<- att_gt(y="infant",
                       gname = "election_year",
                       tname = "cohort_year",
                       xformla = ~1,
                       est_method = "dr",
                       data = df_first_second,
                       control_group = c("nevertreated"),
                       panel = FALSE,
                       clustervars = "state_name",
                       alp = 0.05)


evnt_study_1_2_infant<-aggte(gt_1_2_infant, 
                             type = "dynamic",
                             na.rm = TRUE,
                             min_e = -4,
                             max_e = 5)

png("event_study_1_2_infant.png")

ggdid(evnt_study_1_2_infant)+
  theme_bw() +
  labs(x="Time to election in years",y = "Estimate")+
  theme_bw()+
  ggtitle("")+
  geom_line(linetype="dashed")+
  geom_point(size=3)+
  scale_color_discrete(name="Treatment status",
                       labels=c("Pre","Post"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

dev.off()

######################### Under 5 survival #####################

gt_1_2_under5<- att_gt(y="under5",
                       gname = "election_year",
                       tname = "cohort_year",
                       xformla = ~1,
                       est_method = "dr",
                       data = df_first_second,
                       control_group = c("nevertreated"),
                       panel = FALSE,
                       clustervars = "state_name",
                       alp = 0.05)


evnt_study_1_2_under5<-aggte(gt_1_2_under5, 
                             type = "dynamic",
                             na.rm = TRUE,
                             min_e = -4,
                             max_e = 5)

png("event_study_1_2_under5.png")

ggdid(evnt_study_1_2_under5)+
  theme_bw() +
  labs(x="Time to election in years",y = "Estimate")+
  theme_bw()+
  ggtitle("")+
  geom_line(linetype="dashed")+
  geom_point(size=3)+
  scale_color_discrete(name="Treatment status",
                       labels=c("Pre","Post"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

dev.off()

######################### Under 10 survival #####################

gt_1_2_under10<- att_gt(y="under10",
                       gname = "election_year",
                       tname = "cohort_year",
                       xformla = ~1,
                       est_method = "dr",
                       data = df_first_second,
                       control_group = c("nevertreated"),
                       panel = FALSE,
                       clustervars = "state_name",
                       alp = 0.05)


evnt_study_1_2_under10<-aggte(gt_1_2_under10, 
                             type = "dynamic",
                             na.rm = TRUE,
                             min_e = -4,
                             max_e = 5)

png("event_study_1_2_under10.png")

ggdid(evnt_study_1_2_under10)+
  theme_bw() +
  labs(x="Time to election in years",y = "Estimate")+
  theme_bw()+
  ggtitle("")+
  geom_line(linetype="dashed")+
  geom_point(size=3)+
  scale_color_discrete(name="Treatment status",
                       labels=c("Pre","Post"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

dev.off()

########################### First vs third ##########################


######################### Neonatal survival #####################

gt_1_3_neonat<- att_gt(y="neonat",
                       gname = "election_year",
                       tname = "cohort_year",
                       xformla = ~1,
                       est_method = "dr",
                       data = df_first_third,
                       control_group = c("nevertreated"),
                       panel = FALSE,
                       clustervars = "state_name",
                       alp = 0.05)


evnt_study_1_3_neonat<-aggte(gt_1_3_neonat, 
                             type = "dynamic",
                             na.rm = TRUE,
                             min_e = -4,
                             max_e = 5)

png("event_study_1_3_neonat.png")

ggdid(evnt_study_1_3_neonat)+
  theme_bw() +
  labs(x="Time to election in years",y = "Estimate")+
  theme_bw()+
  ggtitle("")+
  geom_line(linetype="dashed")+
  geom_point(size=3)+
  scale_color_discrete(name="Treatment status",
                       labels=c("Pre","Post"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

dev.off()

######################### Infant survival #####################

gt_1_3_infant<- att_gt(y="infant",
                       gname = "election_year",
                       tname = "cohort_year",
                       xformla = ~1,
                       est_method = "dr",
                       data = df_first_third,
                       control_group = c("nevertreated"),
                       panel = FALSE,
                       clustervars = "state_name",
                       alp = 0.05)


evnt_study_1_3_infant<-aggte(gt_1_3_infant, 
                             type = "dynamic",
                             na.rm = TRUE,
                             min_e = -4,
                             max_e = 5)

png("event_study_1_3_infant.png")

ggdid(evnt_study_1_3_infant)+
  theme_bw() +
  labs(x="Time to election in years",y = "Estimate")+
  theme_bw()+
  ggtitle("")+
  geom_line(linetype="dashed")+
  geom_point(size=3)+
  scale_color_discrete(name="Treatment status",
                       labels=c("Pre","Post"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


dev.off()

######################### Under 5 survival #####################

gt_1_3_under5<- att_gt(y="under5",
                       gname = "election_year",
                       tname = "cohort_year",
                       xformla = ~1,
                       est_method = "dr",
                       data = df_first_third,
                       control_group = c("nevertreated"),
                       panel = FALSE,
                       clustervars = "state_name",
                       alp = 0.05)


evnt_study_1_3_under5<-aggte(gt_1_3_under5, 
                             type = "dynamic",
                             na.rm = TRUE,
                             min_e = -4,
                             max_e = 5)

png("event_study_1_3_under5.png")

ggdid(evnt_study_1_3_under5)+
  theme_bw() +
  labs(x="Time to election in years",y = "Estimate")+
  theme_bw()+
  ggtitle("")+
  geom_line(linetype="dashed")+
  geom_point(size=3)+
  scale_color_discrete(name="Treatment status",
                       labels=c("Pre","Post"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


dev.off()

######################### Under 10 survival #####################

gt_1_3_under10<- att_gt(y="under10",
                        gname = "election_year",
                        tname = "cohort_year",
                        xformla = ~1,
                        est_method = "dr",
                        data = df_first_third,
                        control_group = c("nevertreated"),
                        panel = FALSE,
                        clustervars = "state_name",
                        alp = 0.05)


evnt_study_1_3_under10<-aggte(gt_1_3_under10, 
                              type = "dynamic",
                              na.rm = TRUE,
                              min_e = -4,
                              max_e = 5)
png("event_study_1_3_under10.png")

ggdid(evnt_study_1_3_under10)+
  theme_bw() +
  labs(x="Time to election in years",y = "Estimate")+
  theme_bw()+
  ggtitle("")+
  geom_line(linetype="dashed")+
  geom_point(size=3)+
  scale_color_discrete(name="Treatment status",
                       labels=c("Pre","Post"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

dev.off()

########################### Second vs third ##########################


######################### Neonatal survival #####################

gt_2_3_neonat<- att_gt(y="neonat",
                       gname = "election_year",
                       tname = "cohort_year",
                       xformla = ~1,
                       est_method = "dr",
                       data = df_second_third,
                       control_group = c("nevertreated"),
                       panel = FALSE,
                       clustervars = "state_name",
                       alp = 0.05)


evnt_study_2_3_neonat<-aggte(gt_2_3_neonat, 
                             type = "dynamic",
                             na.rm = TRUE,
                             min_e = -4,
                             max_e = 5)

png("event_study_2_3_neonat.png")

ggdid(evnt_study_2_3_neonat)+
  theme_bw() +
  labs(x="Time to election in years",y = "Estimate")+
  theme_bw()+
  ggtitle("")+
  geom_line(linetype="dashed")+
  geom_point(size=3)+
  scale_color_discrete(name="Treatment status",
                       labels=c("Pre","Post"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

dev.off()

######################### Infant survival #####################

gt_2_3_infant<- att_gt(y="infant",
                       gname = "election_year",
                       tname = "cohort_year",
                       xformla = ~1,
                       est_method = "dr",
                       data = df_second_third,
                       control_group = c("nevertreated"),
                       panel = FALSE,
                       clustervars = "state_name",
                       alp = 0.05)


evnt_study_2_3_infant<-aggte(gt_2_3_infant, 
                             type = "dynamic",
                             na.rm = TRUE,
                             min_e = -4,
                             max_e = 5)

png("event_study_2_3_infant.png")

ggdid(evnt_study_2_3_infant)+
  theme_bw() +
  labs(x="Time to election in years",y = "Estimate")+
  theme_bw()+
  ggtitle("")+
  geom_line(linetype="dashed")+
  geom_point(size=3)+
  scale_color_discrete(name="Treatment status",
                       labels=c("Pre","Post"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


dev.off()

######################### Under 5 survival #####################

gt_2_3_under5<- att_gt(y="under5",
                       gname = "election_year",
                       tname = "cohort_year",
                       xformla = ~1,
                       est_method = "dr",
                       data = df_second_third,
                       control_group = c("nevertreated"),
                       panel = FALSE,
                       clustervars = "state_name",
                       alp = 0.05)


evnt_study_2_3_under5<-aggte(gt_2_3_under5, 
                             type = "dynamic",
                             na.rm = TRUE,
                             min_e = -4,
                             max_e = 5)

png("event_study_2_3_under5.png")

ggdid(evnt_study_2_3_under5)+
  theme_bw() +
  labs(x="Time to election in years",y = "Estimate")+
  theme_ipsum()+
  ggtitle("")+
  geom_line(linetype="dashed")+
  geom_point(size=3)+
  scale_color_discrete(name="Treatment status",
                       labels=c("Pre","Post"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


dev.off()

######################### Under 10 survival #####################

gt_2_3_under10<- att_gt(y="under10",
                        gname = "election_year",
                        tname = "cohort_year",
                        xformla = ~1,
                        est_method = "dr",
                        data = df_second_third,
                        control_group = c("nevertreated"),
                        panel = FALSE,
                        clustervars = "state_name",
                        alp = 0.05)


evnt_study_2_3_under10<-aggte(gt_2_3_under10, 
                              type = "dynamic",
                              na.rm = TRUE,
                              min_e = -4,
                              max_e = 5)

png("event_study_2_3_under10.png")

ggdid(evnt_study_2_3_under10)+
  theme_bw() +
  labs(x="Time to election in years",y = "Estimate")+
  theme_ipsum()+
  ggtitle("")+
  geom_line(linetype="dashed")+
  geom_point(size=3)+
  scale_color_discrete(name="Treatment status",
                       labels=c("Pre","Post"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

dev.off()