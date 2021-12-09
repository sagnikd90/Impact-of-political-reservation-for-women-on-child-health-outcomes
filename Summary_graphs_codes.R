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
library(ggalt)

setwd("C:\\Users\\das90\\OneDrive - Cuny GradCenter\\Research\\Political reservation and age of marriage\\Github\\Political-reservation-and-age-of-marriage-age-of-first-birth\\Final version\\R results\\R_graphs")

df<- read.csv("C:\\Users\\das90\\OneDrive - Cuny GradCenter\\Research\\Political reservation and age of marriage\\Github\\Political-reservation-and-age-of-marriage-age-of-first-birth\\Final version\\Cleaned datasets\\Preferred_sample_data_for_summary_analysis.csv")

############################# Share of children by birth orders #################################

png("Share_of_children_by_birth_order.png",width = 800,height = 800)

df%>%
  group_by(birth_order)%>%
  summarise(number_of_births=sum(total_child))%>%
  mutate(total_child_sum=sum(number_of_births))%>%
  mutate(share_births=round((number_of_births/total_child_sum),digits = 2))%>%
  ggplot(aes(x=as.factor(birth_order),y=share_births,fill=as.factor(birth_order)))+
  geom_bar(stat = "identity",
           position = "dodge")+
  theme_ipsum()+
  geom_text(aes(x=factor(birth_order),
                y=share_births, 
                label = share_births, hjust=1.5), 
            position = position_dodge(width=1),
            check_overlap = TRUE,
            size=5)+
  coord_flip()+
  labs(x="Birth order",
       y="Share of children belonging to different birth orders")+
  scale_fill_discrete(name="Birth order",
                      labels=c("1","2","3","4","5",">=6"))

dev.off()

########################### Age at birth across time ####################################

png("Mothers_age_at_birth_across_time_and_cohort.png",width = 800,height = 800)

df%>%
  subset(cohort_year>=1989)%>%
  group_by(cohort_year,birth_order)%>%
  summarise(mothers_age_at_birth=mean(mothers_age_at_birth,na.rm = TRUE))%>%
  ggplot(aes(x=cohort_year,y=mothers_age_at_birth,color=factor(birth_order)))+
  geom_point(alpha=0.4,size=7)+
  geom_line(aes(color=factor(birth_order)))+
  theme_ipsum()+
  scale_color_discrete(name="Birth order",
                     labels=c("1","2","3","4","5",">=6"))+
  labs(x="Birth cohort",
       y="Mean age of mother at birth")

dev.off()

########################## Wait time for higehr order births ###########################

png("Birth_spacing_across_time.png",width = 800,height = 800)

df%>%
  subset(birth_order>1)%>%
  group_by(cohort_year,birth_order)%>%
  summarise(wait_time=mean(wait_time,na.rm = TRUE))%>%
  ggplot(aes(x=cohort_year,y=wait_time,color=factor(birth_order)))+
  geom_point(aes(shape=factor(birth_order)),size=5)+
  geom_line(aes(color=factor(birth_order)),show.legend = FALSE)+
  theme_bw()+
  xlab("Birth cohort")+
  ylab("Birth spacing in months")+
  scale_shape(guide=FALSE)+
  scale_color_discrete(name="Birth order",
                       labels=c("2","3","4","5",">=6"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


dev.off()


########################## Consolidating the higher birth orders to 3 ##########################

df$birth_order[df$birth_order>=3]<- 3

####################### Mothers age at birth pre and post treatment ###############

png("Mothers_age_at_birth.png")

df%>%
  subset(!is.na(treat))%>%
  ggplot(aes(x=factor(birth_order),y=mothers_age_at_birth,fill=factor(treat)))+
  geom_boxplot()+
  theme_bw()+
  xlab("Birth order")+
  ylab("Mother's age at birth")+
  scale_fill_discrete(name="Treatment status",
                      labels=c("Pre-treatment","Post-treatment"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


dev.off()


######################## Neonatal Survival probability across time and across birth order ####################

png("Neonatal_survival_probability_across_time_birth_order.png",width = 800,height = 800)

df%>%
  subset(cohort_year>=1990)%>%
  group_by(cohort_year,birth_order)%>%
  summarise(neonat=mean(neonat,na.rm = TRUE))%>%
  ggplot(aes(x=cohort_year,y=neonat,color=factor(birth_order)))+
  geom_point(size=5,alpha=0.5)+
  geom_line()+
  theme_bw()+
  labs(x="Birth cohort",
       y="Mean neonatal survival probability")+
  scale_color_discrete(name="Birth order",
                       labels=c("1","2",">=3"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

  
png(dev.off())

######################## Infant Survival probability across time and across birth order ####################

png("Infant_survival_probability_across_time_birth_order.png",width = 800,height = 800)

df%>%
  subset(cohort_year>=1990)%>%
  group_by(cohort_year,birth_order)%>%
  summarise(infant=mean(infant,na.rm = TRUE))%>%
  ggplot(aes(x=cohort_year,y=infant,color=factor(birth_order)))+
  geom_point(size=5,alpha=0.5)+
  geom_line()+
  theme_bw()+
  labs(x="Birth cohort",
       y="Mean infant survival probability")+
  scale_color_discrete(name="Birth order",
                       labels=c("1","2",">=3"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


dev.off()

######################## Under 5 Survival probability across time and across birth order ####################

png("Under_5_probability_across_time_birth_order.png",width = 800,height = 800)

df%>%
  subset(cohort_year>=1990)%>%
  group_by(cohort_year,birth_order)%>%
  summarise(under5=mean(under5,na.rm = TRUE))%>%
  ggplot(aes(x=cohort_year,y=under5,color=factor(birth_order)))+
  geom_point(size=5,alpha=0.5)+
  geom_line()+
  theme_bw()+
  labs(x="Birth cohort",
       y="Mean under 5 years survival probability")+
  scale_color_discrete(name="Birth order",
                       labels=c("1","2",">=3"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


dev.off()

######################## Under 10 Survival probability across time and across birth order ####################

png("Under_10_survival_probability_across_time_birth_order.png",width = 800,height = 800)

df%>%
  subset(cohort_year>=1990)%>%
  group_by(cohort_year,birth_order)%>%
  summarise(under10=mean(under10,na.rm = TRUE))%>%
  ggplot(aes(x=cohort_year,y=under10,color=factor(birth_order)))+
  geom_point(size=5,alpha=0.5)+
  geom_line()+
  theme_bw()+
  labs(x="Birth cohort",
       y="Mean under 10 years survival probability")+
  scale_color_discrete(name="Birth order",
                       labels=c("1","2",">=3"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


dev.off()

###################################### First born child is a boy ###################################

######################## Neonatal Survival probability across time and across birth order ####################

png("Neonatal_survival_probability_first_boy.png",width = 800,height = 800)

df%>%
  subset(cohort_year>=1990 & first_born_boy==1 & birth_order>1)%>%
  group_by(cohort_year,birth_order)%>%
  summarise(neonat=mean(neonat,na.rm = TRUE))%>%
  ggplot(aes(x=cohort_year,y=neonat,color=factor(birth_order)))+
  geom_point(size=5,alpha=0.5)+
  geom_line()+
  theme_bw()+
  labs(x="Birth cohort",
       y="Mean neonatal survival probability when first born is a boy")+
  scale_color_discrete(name="Birth order",
                       labels=c("2",">=3"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


dev.off()

######################## Infant Survival probability across time and across birth order ####################

png("Infant_survival_probability_first_boy.png",width = 800,height = 800)

df%>%
  subset(cohort_year>=1990 & first_born_boy==1 & birth_order>1)%>%
  group_by(cohort_year,birth_order)%>%
  summarise(infant=mean(infant,na.rm = TRUE))%>%
  ggplot(aes(x=cohort_year,y=infant,color=factor(birth_order)))+
  geom_point(size=5,alpha=0.5)+
  geom_line()+
  theme_bw()+
  labs(x="Birth cohort",
       y="Mean infant survival probability when first born is a boy")+
  scale_color_discrete(name="Birth order",
                       labels=c("2",">=3"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


dev.off()

######################## Under 5 Survival probability across time and across birth order ####################

png("Under_5_survival_probability_first_boy.png",width = 800,height = 800)

df%>%
  subset(cohort_year>=1990 & first_born_boy==1 & birth_order>1)%>%
  group_by(cohort_year,birth_order)%>%
  summarise(under5=mean(under5,na.rm = TRUE))%>%
  ggplot(aes(x=cohort_year,y=under5,color=factor(birth_order)))+
  geom_point(size=5,alpha=0.5)+
  geom_line()+
  theme_bw()+
  labs(x="Birth cohort",
       y="Mean under 5 years survival probability when first born is a boy")+
  scale_color_discrete(name="Birth order",
                       labels=c("2",">=3"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


dev.off()

######################## Under 10 Survival probability across time and across birth order ####################

png("Under_10_survival_probability_first_boy.png",width = 800,height = 800)

df%>%
  subset(cohort_year>=1990  & first_born_boy==1 & birth_order>1)%>%
  group_by(cohort_year,birth_order)%>%
  summarise(under10=mean(under10,na.rm = TRUE))%>%
  ggplot(aes(x=cohort_year,y=under10,color=factor(birth_order)))+
  geom_point(size=5,alpha=0.5)+
  geom_line()+
  theme_bw()+
  labs(x="Birth cohort",
       y="Mean under 10 years survival probability when first born is a boy")+
  scale_color_discrete(name="Birth order",
                       labels=c("2",">=3"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


dev.off()

################################# First two born are boys ##################################

png("Survival_probability_first_2_boys.png",width = 800,height = 800)

df%>%
  subset(cohort_year>=1990 & first_two_boys==1 & birth_order==3)%>%
  group_by(cohort_year)%>%
  summarise(neonat=mean(neonat,na.rm = TRUE),
            infant=mean(infant,na.rm = TRUE),
            under5=mean(under5,na.rm = TRUE),
            under10=mean(under10,na.rm = TRUE))%>%
  select(cohort_year,neonat,infant,under5,under10)%>%
  pivot_longer(.,cols=c(neonat,infant,under5,under10),names_to="var",values_to="val")%>%
  ggplot(aes(x=cohort_year,y=val,color=var))+
  geom_point(size=5,alpha=0.5)+
  geom_line()+
  theme_bw()+
  scale_color_discrete(name="Outcome variables",
                       labels=c("Infant","Neonatal","Under 5 years","Under 10 years"))+
  labs(x="Birth cohort",
       y="Survival probability of birth order >=3")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


dev.off()

################################# First two born are girls ##################################

png("Survival_probability_first_2_girls.png",width = 800,height = 800)

df%>%
  subset(cohort_year>=1990 & first_two_girls==1 & birth_order==3)%>%
  group_by(cohort_year)%>%
  summarise(neonat=mean(neonat,na.rm = TRUE),
            infant=mean(infant,na.rm = TRUE),
            under5=mean(under5,na.rm = TRUE),
            under10=mean(under10,na.rm = TRUE))%>%
  select(cohort_year,neonat,infant,under5,under10)%>%
  pivot_longer(.,cols=c(neonat,infant,under5,under10),names_to="var",values_to="val")%>%
  ggplot(aes(x=cohort_year,y=val,color=var))+
  geom_point(size=5,alpha=0.5)+
  geom_line()+
  theme_bw()+
  scale_color_discrete(name="Outcome variables",
                       labels=c("Infant","Neonatal","Under 5 years","Under 10 years"))+
  labs(x="Birth cohort",
       y="Survival probability of birth order >=3")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


dev.off()


################################### Comparison across sex ###############################################

########################## Neonatal survival probability across sex and birth order ########################

png("Neonatal_survival_probability_across_sex.png",width = 800,height = 800)


df%>%
  group_by(birth_order)%>%
  summarise(male_neonat=mean(neonat[sex==0],na.rm = TRUE),
            female_neonat=mean(neonat[sex==1],na.rm = TRUE))%>%
  ggplot() +
  geom_segment(aes(y=factor(birth_order), yend=factor(birth_order), 
                   x=male_neonat, xend=female_neonat), 
               color="#b2b2b2", 
               size_x=3,
               color_x="red",
               color_xend="blue")+
  geom_dumbbell(aes(y=factor(birth_order), x=male_neonat, xend=female_neonat),
                size=5, color="grey", size_x=7, size_xend = 7, colour_x = "red", colour_xend = "blue",
                dot_guide = TRUE, dot_guide_size = 1)+
  labs(x="Neonatal survival probability",
       y="Birth order")+
  theme_bw()+
  geom_text(x=0.9294,y=1,label="Female",size=5)+
  geom_text(x=0.9095,y=1,label="Male",size=5)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


png(dev.off())


########################## Infant survival probability across sex, birth order and cohort ########################

png("Infant_survival_probability_across_sex.png",width = 800,height = 800)


df%>%
  group_by(birth_order)%>%
  summarise(male_infant=mean(infant[sex==0],na.rm = TRUE),
            female_infant=mean(infant[sex==1],na.rm = TRUE))%>%
  ggplot() +
  geom_segment(aes(y=factor(birth_order), yend=factor(birth_order), 
                   x=male_infant, xend=female_infant), 
               color="#b2b2b2", 
               size_x=3,
               color_x="red",
               color_xend="blue")+
  geom_dumbbell(aes(y=factor(birth_order), x=male_infant, xend=female_infant),
                size=5, color="grey", size_x=7, size_xend = 7, colour_x = "red", colour_xend = "blue",
                dot_guide = TRUE, dot_guide_size = 1)+
  labs(x="Infant survival probability",
       y="Birth order")+
  theme_bw()+
  geom_text(x=0.9046,y=1,label="Female",size=5)+
  geom_text(x=0.885,y=1,label="Male",size=5)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


png(dev.off())


########################## Under 5 survival probability across sex, birth order and cohort ########################

png("Under5_survival_probability_across_sex.png",width = 800,height = 800)


df%>%
  group_by(birth_order)%>%
  summarise(male_under5=mean(under5[sex==0],na.rm = TRUE),
            female_under5=mean(under5[sex==1],na.rm = TRUE))%>%
  ggplot() +
  geom_segment(aes(y=factor(birth_order), yend=factor(birth_order), 
                   x=male_under5, xend=female_under5), 
               color="#b2b2b2", 
               size_x=3,
               color_x="red",
               color_xend="blue")+
  geom_dumbbell(aes(y=factor(birth_order), x=male_under5, xend=female_under5),
                size=5, color="grey", size_x=7, size_xend = 7, colour_x = "red", colour_xend = "blue",
                dot_guide = TRUE, dot_guide_size = 1)+
  labs(x="Under 5 years survival probability",
       y="Birth order")+
  theme_bw()+
  geom_text(x=0.8886,y=1,label="Female",size=5)+
  geom_text(x=0.8725,y=1,label="Male",size=5)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


png(dev.off())


########################## Under 10 survival probability across sex, birth order and cohort ########################

png("Under10_survival_probability_across_sex.png",width = 800,height = 800)


df%>%
  group_by(birth_order)%>%
  summarise(male_under10=mean(under10[sex==0],na.rm = TRUE),
            female_under10=mean(under10[sex==1],na.rm = TRUE))%>%
  ggplot() +
  geom_segment(aes(y=factor(birth_order), yend=factor(birth_order), 
                   x=male_under10, xend=female_under10), 
               color="#b2b2b2", 
               size_x=3,
               color_x="red",
               color_xend="blue")+
  geom_dumbbell(aes(y=factor(birth_order), x=male_under10, xend=female_under10),
                size=5, color="grey", size_x=7, size_xend = 7, colour_x = "red", colour_xend = "blue",
                dot_guide = TRUE, dot_guide_size = 1)+
  labs(x="Under 10 years survival probability",
       y="Birth order")+
  theme_bw()+
  geom_text(x=0.8855,y=1,label="Female",size=5)+
  geom_text(x=0.8700,y=1,label="Male",size=5)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


png(dev.off())


###################################### First born child is a boy ###################################

######################## Neonatal Survival probability across time, birth order and sex ####################

png("Neonatal_survival_probability_first_boy_sex_2.png",width = 800,height = 800)

df%>%
  subset(birth_order>1)%>%
  subset(!is.na(first_born_boy) &
           birth_order==2)%>%
  group_by(first_born_boy)%>%
  summarise(male_neonat=mean(neonat[sex==0],na.rm = TRUE),
            female_neonat=mean(neonat[sex==1],na.rm = TRUE))%>%
  ggplot() +
  geom_segment(aes(y=factor(first_born_boy), yend=factor(first_born_boy), 
                   x=male_neonat, xend=female_neonat), 
               color="#b2b2b2", 
               size_x=3,
               color_x="red",
               color_xend="blue")+
  geom_dumbbell(aes(y=factor(first_born_boy), x=male_neonat, xend=female_neonat),
                size=5, color="grey", size_x=7, size_xend = 7, colour_x = "red", colour_xend = "blue",
                dot_guide = TRUE, dot_guide_size = 1)+
  labs(x="Neonatal survival probability of birth order=2",
       y="First born is a boy")+
  theme_bw()+
  geom_text(x=0.9285,y="0",label="Male",size=5)+
  geom_text(x=0.9368,y="0",label="Female",size=5)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  coord_flip()

dev.off()


png("Neonatal_survival_probability_first_boy_sex_3.png",width = 800,height = 800)

df%>%
  subset(birth_order>1)%>%
  subset(!is.na(first_born_boy) &
           birth_order==3)%>%
  group_by(first_born_boy)%>%
  summarise(male_neonat=mean(neonat[sex==0],na.rm = TRUE),
            female_neonat=mean(neonat[sex==1],na.rm = TRUE))%>%
  ggplot() +
  geom_segment(aes(y=factor(first_born_boy), yend=factor(first_born_boy), 
                   x=male_neonat, xend=female_neonat), 
               color="#b2b2b2", 
               size_x=3,
               color_x="red",
               color_xend="blue")+
  geom_dumbbell(aes(y=factor(first_born_boy), x=male_neonat, xend=female_neonat),
                size=5, color="grey", size_x=7, size_xend = 7, colour_x = "red", colour_xend = "blue",
                dot_guide = TRUE, dot_guide_size = 1)+
  labs(x="Neonatal survival probability of birth order=3",
       y="First born is a boy")+
  theme_bw()+
  geom_text(x=0.9412,y="0",label="Male",size=5)+
  geom_text(x=0.9375,y="0",label="Female",size=5)+
  coord_flip()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


dev.off()


######################## Infant Survival probability across time, birth order and sex ####################

png("Infant_survival_probability_first_boy_sex_2.png",width = 800,height = 800)

df%>%
  subset(birth_order>1)%>%
  subset(!is.na(first_born_boy) &
           birth_order==2)%>%
  group_by(first_born_boy)%>%
  summarise(male_infant=mean(infant[sex==0],na.rm = TRUE),
            female_infant=mean(infant[sex==1],na.rm = TRUE))%>%
  ggplot() +
  geom_segment(aes(y=factor(first_born_boy), yend=factor(first_born_boy), 
                   x=male_infant, xend=female_infant), 
               color="#b2b2b2", 
               size_x=3,
               color_x="red",
               color_xend="blue")+
  geom_dumbbell(aes(y=factor(first_born_boy), x=male_infant, xend=female_infant),
                size=5, color="grey", size_x=7, size_xend = 7, colour_x = "red", colour_xend = "blue",
                dot_guide = TRUE, dot_guide_size = 1)+
  labs(x="Infant survival probability of birth order=2",
       y="First born is a boy")+
  theme_bw()+
  geom_text(x=0.9037,y="0",label="Male",size=5)+
  geom_text(x=0.908,y="0",label="Female",size=5)+
  coord_flip()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


dev.off()


png("Infant_survival_probability_first_boy_sex_3.png",width = 800,height = 800)

df%>%
  subset(birth_order>1)%>%
  subset(!is.na(first_born_boy) &
           birth_order==3)%>%
  group_by(first_born_boy)%>%
  summarise(male_infant=mean(infant[sex==0],na.rm = TRUE),
            female_infant=mean(infant[sex==1],na.rm = TRUE))%>%
  ggplot() +
  geom_segment(aes(y=factor(first_born_boy), yend=factor(first_born_boy), 
                   x=male_infant, xend=female_infant), 
               color="#b2b2b2", 
               size_x=3,
               color_x="red",
               color_xend="blue")+
  geom_dumbbell(aes(y=factor(first_born_boy), x=male_infant, xend=female_infant),
                size=5, color="grey", size_x=7, size_xend = 7, colour_x = "red", colour_xend = "blue",
                dot_guide = TRUE, dot_guide_size = 1)+
  labs(x="Infant survival probability of birth order=3",
       y="First born is a boy")+
  theme_bw()+
  geom_text(x=0.9167,y="0",label="Male",size=5)+
  geom_text(x=0.9025,y="0",label="Female",size=5)+
  coord_flip()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


dev.off()

######################## Under 5 Survival probability across time and across birth order ####################

png("Under_5_survival_probability_first_boy_sex_2.png",width = 800,height = 800)

df%>%
  subset(birth_order>1)%>%
  subset(!is.na(first_born_boy) &
           birth_order==2)%>%
  group_by(first_born_boy)%>%
  summarise(male_under5=mean(under5[sex==0],na.rm = TRUE),
            female_under5=mean(under5[sex==1],na.rm = TRUE))%>%
  ggplot() +
  geom_segment(aes(y=factor(first_born_boy), yend=factor(first_born_boy), 
                   x=male_under5, xend=female_under5), 
               color="#b2b2b2", 
               size_x=3,
               color_x="red",
               color_xend="blue")+
  geom_dumbbell(aes(y=factor(first_born_boy), x=male_under5, xend=female_under5),
                size=5, color="grey", size_x=7, size_xend = 7, colour_x = "red", colour_xend = "blue",
                dot_guide = TRUE, dot_guide_size = 1)+
  labs(x="Under 5 survival probability of birth order=2",
       y="First born is a boy")+
  theme_bw()+
  geom_text(x=0.8906,y="0",label="Male",size=5)+
  geom_text(x=0.887,y="0",label="Female",size=5)+
  coord_flip()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


dev.off()


png("Under_5_survival_probability_first_boy_sex_3.png",width = 800,height = 800)

df%>%
  subset(birth_order>1)%>%
  subset(!is.na(first_born_boy) &
           birth_order==3)%>%
  group_by(first_born_boy)%>%
  summarise(male_under5=mean(under5[sex==0],na.rm = TRUE),
            female_under5=mean(under5[sex==1],na.rm = TRUE))%>%
  ggplot() +
  geom_segment(aes(y=factor(first_born_boy), yend=factor(first_born_boy), 
                   x=male_under5, xend=female_under5), 
               color="#b2b2b2", 
               size_x=3,
               color_x="red",
               color_xend="blue")+
  geom_dumbbell(aes(y=factor(first_born_boy), x=male_under5, xend=female_under5),
                size=5, color="grey", size_x=7, size_xend = 7, colour_x = "red", colour_xend = "blue",
                dot_guide = TRUE, dot_guide_size = 1)+
  labs(x="Under 5 survival probability of birth order=3",
       y="First born is a boy")+
  theme_bw()+
  geom_text(x=0.9035,y="0",label="Male",size=5)+
  geom_text(x=0.8795,y="0",label="Female",size=5)+
  coord_flip()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


dev.off()

######################## Under 10 Survival probability across time, birth order and sex ####################

png("Under_10_survival_probability_first_boy_sex_2.png",width = 800,height = 800)

df%>%
  subset(birth_order>1)%>%
  subset(!is.na(first_born_boy) &
           birth_order==2)%>%
  group_by(first_born_boy)%>%
  summarise(male_under10=mean(under10[sex==0],na.rm = TRUE),
            female_under10=mean(under10[sex==1],na.rm = TRUE))%>%
  ggplot() +
  geom_segment(aes(y=factor(first_born_boy), yend=factor(first_born_boy), 
                   x=male_under10, xend=female_under10), 
               color="#b2b2b2", 
               size_x=3,
               color_x="red",
               color_xend="blue")+
  geom_dumbbell(aes(y=factor(first_born_boy), x=male_under10, xend=female_under10),
                size=5, color="grey", size_x=7, size_xend = 7, colour_x = "red", colour_xend = "blue",
                dot_guide = TRUE, dot_guide_size = 1)+
  labs(x="Under 10 survival probability of birth order=2",
       y="First born is a boy")+
  theme_bw()+
  geom_text(x=0.8872,y="0",label="Male",size=5)+
  geom_text(x=0.8844,y="0",label="Female",size=5)+
  coord_flip()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


dev.off()


png("Under_10_survival_probability_first_boy_sex_3.png",width = 800,height = 800)

df%>%
  subset(birth_order>1)%>%
  subset(!is.na(first_born_boy) &
           birth_order==3)%>%
  group_by(first_born_boy)%>%
  summarise(male_under10=mean(under10[sex==0],na.rm = TRUE),
            female_under10=mean(under10[sex==1],na.rm = TRUE))%>%
  ggplot() +
  geom_segment(aes(y=factor(first_born_boy), yend=factor(first_born_boy), 
                   x=male_under10, xend=female_under10), 
               color="#b2b2b2", 
               size_x=3,
               color_x="red",
               color_xend="blue")+
  geom_dumbbell(aes(y=factor(first_born_boy), x=male_under10, xend=female_under10),
                size=5, color="grey", size_x=7, size_xend = 7, colour_x = "red", colour_xend = "blue",
                dot_guide = TRUE, dot_guide_size = 1)+
  labs(x="Under 10 survival probability of birth order=3",
       y="First born is a boy")+
  theme_bw()+
  geom_text(x=0.902,y="0",label="Male",size=5)+
  geom_text(x=0.8775,y="0",label="Female",size=5)+
  coord_flip()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


dev.off()


########################## Neonatal survival probability across sex composition ###########################

################################# First two born are boys across sex ##################################

png("Neonatal_first_2_boys_sex.png",width = 800,height = 800)

df%>%
  subset(!is.na(first_two_boys) &
           birth_order==3)%>%
  group_by(first_two_boys)%>%
  summarise(male_neonat=mean(neonat[sex==0],na.rm = TRUE),
            female_neonat=mean(neonat[sex==1],na.rm = TRUE))%>%
  ggplot() +
  geom_segment(aes(y=factor(first_two_boys), yend=factor(first_two_boys), 
                   x=male_neonat, xend=female_neonat), 
               color="#b2b2b2", 
               size_x=3,
               color_x="red",
               color_xend="blue")+
  geom_dumbbell(aes(y=factor(first_two_boys), x=male_neonat, xend=female_neonat),
                size=5, color="grey", size_x=7, size_xend = 7, colour_x = "red", colour_xend = "blue",
                dot_guide = TRUE, dot_guide_size = 1)+
  labs(x="Neonatal survival probability of birth order=3",
       y="First two born are boys")+
  theme_bw()+
  geom_text(x=0.9434,y="0",label="Male",size=5)+
  geom_text(x=0.9405,y="0",label="Female",size=5)+
  coord_flip()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))



dev.off()

################################# First two born are girls ##################################

png("Neonatal_first_2_girls_sex.png",width = 800,height = 800)

df%>%
  subset(!is.na(first_two_girls) &
           birth_order==3)%>%
  group_by(first_two_girls)%>%
  summarise(male_neonat=mean(neonat[sex==0],na.rm = TRUE),
            female_neonat=mean(neonat[sex==1],na.rm = TRUE))%>%
  ggplot() +
  geom_segment(aes(y=factor(first_two_girls), yend=factor(first_two_girls), 
                   x=male_neonat, xend=female_neonat), 
               color="#b2b2b2", 
               size_x=3,
               color_x="red",
               color_xend="blue")+
  geom_dumbbell(aes(y=factor(first_two_girls), x=male_neonat, xend=female_neonat),
                size=5, color="grey", size_x=7, size_xend = 7, colour_x = "red", colour_xend = "blue",
                dot_guide = TRUE, dot_guide_size = 1)+
  labs(x="Neonatal survival probability of birth order=3",
       y="First two born are girls")+
  theme_bw()+
  geom_text(x=0.9384,y="0",label="Male",size=5)+
  geom_text(x=0.9429,y="0",label="Female",size=5)+
  coord_flip()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))



dev.off()

########################## Infant survival probability across sex composition ###########################

################################# First two born are boys across sex ##################################

png("Infant_first_2_boys_sex.png",width = 800,height = 800)

df%>%
  subset(!is.na(first_two_boys) &
           birth_order==3)%>%
  group_by(first_two_boys)%>%
  summarise(male_infant=mean(infant[sex==0],na.rm = TRUE),
            female_infant=mean(infant[sex==1],na.rm = TRUE))%>%
  ggplot() +
  geom_segment(aes(y=factor(first_two_boys), yend=factor(first_two_boys), 
                   x=male_infant, xend=female_infant), 
               color="#b2b2b2", 
               size_x=3,
               color_x="red",
               color_xend="blue")+
  geom_dumbbell(aes(y=factor(first_two_boys), x=male_infant, xend=female_infant),
                size=5, color="grey", size_x=7, size_xend = 7, colour_x = "red", colour_xend = "blue",
                dot_guide = TRUE, dot_guide_size = 1)+
  labs(x="Infant survival probability of birth order=3",
       y="First two born are boys")+
  theme_bw()+
  geom_text(x=0.920,y="0",label="Male",size=5)+
  geom_text(x=0.9062,y="0",label="Female",size=5)+
  coord_flip()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))



dev.off()

################################# First two born are girls ##################################

png("Infant_first_2_girls_sex.png",width = 800,height = 800)

df%>%
  subset(!is.na(first_two_girls) &
           birth_order==3)%>%
  group_by(first_two_girls)%>%
  summarise(male_infant=mean(infant[sex==0],na.rm = TRUE),
            female_infant=mean(infant[sex==1],na.rm = TRUE))%>%
  ggplot() +
  geom_segment(aes(y=factor(first_two_girls), yend=factor(first_two_girls), 
                   x=male_infant, xend=female_infant), 
               color="#b2b2b2", 
               size_x=3,
               color_x="red",
               color_xend="blue")+
  geom_dumbbell(aes(y=factor(first_two_girls), x=male_infant, xend=female_infant),
                size=5, color="grey", size_x=7, size_xend = 7, colour_x = "red", colour_xend = "blue",
                dot_guide = TRUE, dot_guide_size = 1)+
  labs(x="Infant survival probability of birth order=3",
       y="First two born are girls")+
  theme_bw()+
  geom_text(x=0.914,y="0",label="Male",size=5)+
  geom_text(x=0.9075,y="0",label="Female",size=5)+
  coord_flip()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))



dev.off()


########################## Under 5 survival probability across sex composition ###########################

################################# First two born are boys across sex ##################################

png("Under5_first_2_boys_sex.png",width = 800,height = 800)

df%>%
  subset(!is.na(first_two_boys) &
           birth_order==3)%>%
  group_by(first_two_boys)%>%
  summarise(male_under5=mean(under5[sex==0],na.rm = TRUE),
            female_under5=mean(under5[sex==1],na.rm = TRUE))%>%
  ggplot() +
  geom_segment(aes(y=factor(first_two_boys), yend=factor(first_two_boys), 
                   x=male_under5, xend=female_under5), 
               color="#b2b2b2", 
               size_x=3,
               color_x="red",
               color_xend="blue")+
  geom_dumbbell(aes(y=factor(first_two_boys), x=male_under5, xend=female_under5),
                size=5, color="grey", size_x=7, size_xend = 7, colour_x = "red", colour_xend = "blue",
                dot_guide = TRUE, dot_guide_size = 1)+
  labs(x="Under 5 years survival probability of birth order=3",
       y="First two born are boys")+
  theme_bw()+
  geom_text(x=0.9064,y="0",label="Male",size=5)+
  geom_text(x=0.883,y="0",label="Female",size=5)+
  coord_flip()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))



dev.off()

################################# First two born are girls ##################################

png("Under5_first_2_girls_sex.png",width = 800,height = 800)

df%>%
  subset(!is.na(first_two_girls) &
           birth_order==3)%>%
  group_by(first_two_girls)%>%
  summarise(male_under5=mean(under5[sex==0],na.rm = TRUE),
            female_under5=mean(under5[sex==1],na.rm = TRUE))%>%
  ggplot() +
  geom_segment(aes(y=factor(first_two_girls), yend=factor(first_two_girls), 
                   x=male_under5, xend=female_under5), 
               color="#b2b2b2", 
               size_x=3,
               color_x="red",
               color_xend="blue")+
  geom_dumbbell(aes(y=factor(first_two_girls), x=male_under5, xend=female_under5),
                size=5, color="grey", size_x=7, size_xend = 7, colour_x = "red", colour_xend = "blue",
                dot_guide = TRUE, dot_guide_size = 1)+
  labs(x="Under 5 years survival probability of birth order=3",
       y="First two born are girls")+
  theme_bw()+
  geom_text(x=0.90,y="0",label="Male",size=5)+
  geom_text(x=0.884,y="0",label="Female",size=5)+
  coord_flip()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))



dev.off()

########################## Under 10 survival probability across sex composition ###########################

################################# First two born are boys across sex ##################################

png("Under10_first_2_boys_sex.png",width = 800,height = 800)

df%>%
  subset(!is.na(first_two_boys) &
           birth_order==3)%>%
  group_by(first_two_boys)%>%
  summarise(male_under10=mean(under10[sex==0],na.rm = TRUE),
            female_under10=mean(under10[sex==1],na.rm = TRUE))%>%
  ggplot() +
  geom_segment(aes(y=factor(first_two_boys), yend=factor(first_two_boys), 
                   x=male_under10, xend=female_under10), 
               color="#b2b2b2", 
               size_x=3,
               color_x="red",
               color_xend="blue")+
  geom_dumbbell(aes(y=factor(first_two_boys), x=male_under10, xend=female_under10),
                size=5, color="grey", size_x=7, size_xend = 7, colour_x = "red", colour_xend = "blue",
                dot_guide = TRUE, dot_guide_size = 1)+
  labs(x="Under 10 years survival probability of birth order=3",
       y="First two born are boys")+
  theme_bw()+
  geom_text(x=0.9050,y="0",label="Male",size=5)+
  geom_text(x=0.881,y="0",label="Female",size=5)+
  coord_flip()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))



dev.off()

################################# First two born are girls ##################################

png("Under10_first_2_girls_sex.png",width = 800,height = 800)

df%>%
  subset(!is.na(first_two_girls) &
           birth_order==3)%>%
  group_by(first_two_girls)%>%
  summarise(male_under10=mean(under10[sex==0],na.rm = TRUE),
            female_under10=mean(under10[sex==1],na.rm = TRUE))%>%
  ggplot() +
  geom_segment(aes(y=factor(first_two_girls), yend=factor(first_two_girls), 
                   x=male_under10, xend=female_under10), 
               color="#b2b2b2", 
               size_x=3,
               color_x="red",
               color_xend="blue")+
  geom_dumbbell(aes(y=factor(first_two_girls), x=male_under10, xend=female_under10),
                size=5, color="grey", size_x=7, size_xend = 7, colour_x = "red", colour_xend = "blue",
                dot_guide = TRUE, dot_guide_size = 1)+
  labs(x="Under 10 years survival probability of birth order=3",
       y="First two born are girls")+
  theme_bw()+
  geom_text(x=0.8992,y="0",label="Male",size=5)+
  geom_text(x=0.8815,y="0",label="Female",size=5)+
  coord_flip()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))



dev.off()
