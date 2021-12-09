library(dplyr)
library(foreign)
library(ggplot2)
library(Hmisc)
library(plm)
library(broom)
library(jtools)
library(tidyverse)
library(reshape2)
library(stargazer)
library(knitr)


setwd("C:\\Users\\das90\\OneDrive - Cuny GradCenter\\Research\\Political reservation and age of marriage\\Github\\Political-reservation-and-age-of-marriage-age-of-first-birth\\Final version\\R results\\R_graphs")

df<- read.csv("C:\\Users\\das90\\OneDrive - Cuny GradCenter\\Research\\Political reservation and age of marriage\\Github\\Political-reservation-and-age-of-marriage-age-of-first-birth\\Final version\\Cleaned datasets\\Preferred_sample_data_for_summary_tables.csv")

################################# Introductory summary table ##############################

df_intro_sum<- df%>%
  group_by(state_name)%>%
  summarise(neonat=round(mean(neonat,na.rm = TRUE),digits = 2),
            infant=round(mean(infant,na.rm = TRUE),digits = 2),
            under5=round(mean(under5,na.rm = TRUE),digits=2),
            under10=round(mean(under10,na.rm = TRUE),digits = 2))

df_elections<- df%>%
  group_by(state_name,first_election,first_election_month)%>%
  summarise(neonat=mean(neonat,na.rm = TRUE))%>%
  select(state_name,first_election,first_election_month)

df_intro_sum<- merge(df_elections,df_intro_sum,by="state_name")

kable(df_intro_sum,"latex")

############################# Mother's characteristics ########################

df<- df%>%
  filter(state_name %in% c("andhra pradesh",
                           "gujarat",
                           "karnataka",
                           "kerala",
                           "madhya pradesh",
                           "rajasthan",
                           "tripura",
                           "uttar pradesh",
                           "west bengal"))

df_mothers<- df

df_mothers$before_election[df_mothers$birth_order==1 & df_mothers$treat==0]<- 1
df_mothers$before_election[is.na(df_mothers$before_election)]<- 0



df_mothers_char<- df_mothers%>%
  subset(birth_order==1 & read_write_woman!=9)%>%
  mutate(solid_house=ifelse(type_of_house=="pucca",1,0),
         sc_st=ifelse(caste=="scheduled caste"|caste=="scheduled tribe",1,0),
         hindu=ifelse(religion=="hindu",1,0),
         literate=ifelse(read_write_woman==1,1,0))%>%
  group_by(before_election)%>%
  summarise(womans_age=round(mean(age_woman,na.rm = TRUE),digits = 3),
            age_at_mrrg=round(mean(age_at_mrrg_woman,na.rm = TRUE),digits = 3),
            literate=round(mean(literate,na.rm = TRUE),digits = 3),
            solid_house=round(mean(solid_house,na.rm = TRUE),digits = 3),
            sc_st=round(mean(sc_st,na.rm = TRUE),digits = 3),
            hindu=round(mean(hindu,na.rm = TRUE),digits = 3),
            age_at_first_birth=round(mean(mothers_age_at_birth,na.rm = TRUE),digits = 3),
            total_child=round(mean(total_child,na.rm = TRUE),digits = 3),
            first_child_boy=round(mean(first_born_boy,na.rm = TRUE),digits = 3),
            first_two_boys=round(mean(first_two_boys,na.rm = TRUE),digits = 3),
            first_two_girls=round(mean(first_two_girls,na.rm = TRUE),digits = 3))%>%
  select(before_election,
         womans_age,
         age_at_mrrg,
         literate,
         solid_house,
         sc_st,
         hindu,
         age_at_first_birth,
         total_child,
         first_child_boy,
         first_two_boys,
         first_two_girls)%>%
  pivot_longer(.,cols=c(womans_age,
                        age_at_mrrg,
                        literate,
                        solid_house,
                        sc_st,
                        hindu,
                        age_at_first_birth,
                        total_child,
                        first_child_boy,
                        first_two_boys,
                        first_two_girls),names_to="var",values_to="val")%>%
  pivot_wider(names_from = "before_election",values_from= "val")%>%
  select("var","1","0")


kable(df_mothers_char,"latex")

########################### Summary tables by birth order #############################

##################### Neonatal survival probability ########################

df_neonat<- df%>%
  select(birth_order,treat,sex,neonat)%>%
  group_by(birth_order,sex)%>%
  summarise(neonat_before=round(mean(neonat[treat==0],na.rm = TRUE),digits = 3),
            neonat_after=round(mean(neonat[treat==1],na.rm = TRUE),digits = 3))%>%
  mutate(diff=neonat_before-neonat_after)

df_neonat_1_pval_m<- df%>%
  subset(sex==0 & birth_order==1)%>%
  mutate(p_val=t.test(neonat~treat,var.equal=TRUE)$p.value)%>%
  subset(id==637)%>%
  select(birth_order,sex,p_val)

df_neonat_1_pval_f<- df%>%
  subset(sex==1 & birth_order==1)%>%
  mutate(p_val=t.test(neonat~treat,var.equal=TRUE)$p.value)%>%
  subset(id==638)%>%
  select(birth_order,sex,p_val)

df_neonat_2_pval_m<- df%>%
  subset(sex==0 & birth_order==2)%>%
  mutate(p_val=t.test(neonat~treat,var.equal=TRUE)$p.value)%>%
  subset(id==642)%>%
  select(birth_order,sex,p_val)

df_neonat_2_pval_f<- df%>%
  subset(sex==1 & birth_order==2)%>%
  mutate(p_val=t.test(neonat~treat,var.equal=TRUE)$p.value)%>%
  subset(id==637)%>%
  select(birth_order,sex,p_val)

df_neonat_3_pval_m<- df%>%
  subset(sex==0 & birth_order==3)%>%
  mutate(p_val=t.test(neonat~treat,var.equal=TRUE)$p.value)%>%
  subset(id==637)%>%
  select(birth_order,sex,p_val)

df_neonat_3_pval_f<- df%>%
  subset(sex==1 & birth_order==3)%>%
  mutate(p_val=t.test(neonat~treat,var.equal=TRUE)$p.value)%>%
  subset(id==637)%>%
  select(birth_order,sex,p_val)


df_neonat_pval<- rbind(df_neonat_1_pval_m,
                       df_neonat_1_pval_f,
                       df_neonat_2_pval_m,
                       df_neonat_2_pval_f,
                       df_neonat_3_pval_m,
                       df_neonat_3_pval_f)

df_neonat<- cbind(df_neonat,df_neonat_pval)

df_neonat<- df_neonat%>%
  select(birth_order...1,
         sex...2,
         neonat_before,
         neonat_after,
         diff,
         p_val)

kable(df_neonat,"latex")


##################### Infant survival probability ########################

df_infant<- df%>%
  select(birth_order,treat,sex,infant)%>%
  group_by(birth_order,sex)%>%
  summarise(infant_before=round(mean(infant[treat==0],na.rm = TRUE),digits = 3),
            infant_after=round(mean(infant[treat==1],na.rm = TRUE),digits = 3))%>%
  mutate(diff=infant_before-infant_after)

df_infant_1_pval_m<- df%>%
  subset(sex==0 & birth_order==1)%>%
  mutate(p_val=t.test(infant~treat,var.equal=TRUE)$p.value)%>%
  subset(id==637)%>%
  select(birth_order,sex,p_val)

df_infant_1_pval_f<- df%>%
  subset(sex==1 & birth_order==1)%>%
  mutate(p_val=t.test(infant~treat,var.equal=TRUE)$p.value)%>%
  subset(id==638)%>%
  select(birth_order,sex,p_val)

df_infant_2_pval_m<- df%>%
  subset(sex==0 & birth_order==2)%>%
  mutate(p_val=t.test(infant~treat,var.equal=TRUE)$p.value)%>%
  subset(id==642)%>%
  select(birth_order,sex,p_val)

df_infant_2_pval_f<- df%>%
  subset(sex==1 & birth_order==2)%>%
  mutate(p_val=t.test(infant~treat,var.equal=TRUE)$p.value)%>%
  subset(id==637)%>%
  select(birth_order,sex,p_val)

df_infant_3_pval_m<- df%>%
  subset(sex==0 & birth_order==3)%>%
  mutate(p_val=t.test(infant~treat,var.equal=TRUE)$p.value)%>%
  subset(id==637)%>%
  select(birth_order,sex,p_val)

df_infant_3_pval_f<- df%>%
  subset(sex==1 & birth_order==3)%>%
  mutate(p_val=t.test(infant~treat,var.equal=TRUE)$p.value)%>%
  subset(id==637)%>%
  select(birth_order,sex,p_val)


df_infant_pval<- rbind(df_infant_1_pval_m,
                       df_infant_1_pval_f,
                       df_infant_2_pval_m,
                       df_infant_2_pval_f,
                       df_infant_3_pval_m,
                       df_infant_3_pval_f)

df_infant<- cbind(df_infant,df_infant_pval)

df_infant<- df_infant%>%
  select(birth_order...1,
         sex...2,
         infant_before,
         infant_after,
         diff,
         p_val)

kable(df_infant,"latex")


##################### Under 5 survival probability ########################

df_under5<- df%>%
  select(birth_order,treat,sex,under5)%>%
  group_by(birth_order,sex)%>%
  summarise(under5_before=round(mean(under5[treat==0],na.rm = TRUE),digits = 3),
            under5_after=round(mean(under5[treat==1],na.rm = TRUE),digits = 3))%>%
  mutate(diff=under5_before-under5_after)

df_under5_1_pval_m<- df%>%
  subset(sex==0 & birth_order==1)%>%
  mutate(p_val=t.test(under5~treat,var.equal=TRUE)$p.value)%>%
  subset(id==637)%>%
  select(birth_order,sex,p_val)

df_under5_1_pval_f<- df%>%
  subset(sex==1 & birth_order==1)%>%
  mutate(p_val=t.test(under5~treat,var.equal=TRUE)$p.value)%>%
  subset(id==638)%>%
  select(birth_order,sex,p_val)

df_under5_2_pval_m<- df%>%
  subset(sex==0 & birth_order==2)%>%
  mutate(p_val=t.test(under5~treat,var.equal=TRUE)$p.value)%>%
  subset(id==642)%>%
  select(birth_order,sex,p_val)

df_under5_2_pval_f<- df%>%
  subset(sex==1 & birth_order==2)%>%
  mutate(p_val=t.test(under5~treat,var.equal=TRUE)$p.value)%>%
  subset(id==637)%>%
  select(birth_order,sex,p_val)

df_under5_3_pval_m<- df%>%
  subset(sex==0 & birth_order==3)%>%
  mutate(p_val=t.test(under5~treat,var.equal=TRUE)$p.value)%>%
  subset(id==637)%>%
  select(birth_order,sex,p_val)

df_under5_3_pval_f<- df%>%
  subset(sex==1 & birth_order==3)%>%
  mutate(p_val=t.test(under5~treat,var.equal=TRUE)$p.value)%>%
  subset(id==637)%>%
  select(birth_order,sex,p_val)


df_under5_pval<- rbind(df_under5_1_pval_m,
                       df_under5_1_pval_f,
                       df_under5_2_pval_m,
                       df_under5_2_pval_f,
                       df_under5_3_pval_m,
                       df_under5_3_pval_f)

df_under5<- cbind(df_under5,df_under5_pval)

df_under5<- df_under5%>%
  select(birth_order...1,
         sex...2,
         under5_before,
         under5_after,
         diff,
         p_val)

kable(df_under5,"latex")


##################### Under 10 survival probability ########################

df_under10<- df%>%
  select(birth_order,treat,sex,under10)%>%
  group_by(birth_order,sex)%>%
  summarise(under10_before=round(mean(under10[treat==0],na.rm = TRUE),digits = 3),
            under10_after=round(mean(under10[treat==1],na.rm = TRUE),digits = 3))%>%
  mutate(diff=under10_before-under10_after)

df_under10_1_pval_m<- df%>%
  subset(sex==0 & birth_order==1)%>%
  mutate(p_val=t.test(under10~treat,var.equal=TRUE)$p.value)%>%
  subset(id==637)%>%
  select(birth_order,sex,p_val)

df_under10_1_pval_f<- df%>%
  subset(sex==1 & birth_order==1)%>%
  mutate(p_val=t.test(under10~treat,var.equal=TRUE)$p.value)%>%
  subset(id==638)%>%
  select(birth_order,sex,p_val)

df_under10_2_pval_m<- df%>%
  subset(sex==0 & birth_order==2)%>%
  mutate(p_val=t.test(under10~treat,var.equal=TRUE)$p.value)%>%
  subset(id==642)%>%
  select(birth_order,sex,p_val)

df_under10_2_pval_f<- df%>%
  subset(sex==1 & birth_order==2)%>%
  mutate(p_val=t.test(under10~treat,var.equal=TRUE)$p.value)%>%
  subset(id==637)%>%
  select(birth_order,sex,p_val)

df_under10_3_pval_m<- df%>%
  subset(sex==0 & birth_order==3)%>%
  mutate(p_val=t.test(under10~treat,var.equal=TRUE)$p.value)%>%
  subset(id==637)%>%
  select(birth_order,sex,p_val)

df_under10_3_pval_f<- df%>%
  subset(sex==1 & birth_order==3)%>%
  mutate(p_val=t.test(under10~treat,var.equal=TRUE)$p.value)%>%
  subset(id==637)%>%
  select(birth_order,sex,p_val)


df_under10_pval<- rbind(df_under10_1_pval_m,
                       df_under10_1_pval_f,
                       df_under10_2_pval_m,
                       df_under10_2_pval_f,
                       df_under10_3_pval_m,
                       df_under10_3_pval_f)

df_under10<- cbind(df_under10,df_under10_pval)

df_under10<- df_under10%>%
  select(birth_order...1,
         sex...2,
         under10_before,
         under10_after,
         diff,
         p_val)

kable(df_under10,"latex")


#################### When the first born is a boy ###############################


##################### Neonatal survival probability ########################

df_neonat<- df%>%
  subset(first_born_boy==1 & birth_order>1)%>%
  select(birth_order,treat,sex,neonat)%>%
  group_by(birth_order,sex)%>%
  summarise(neonat_before=round(mean(neonat[treat==0],na.rm = TRUE),digits = 3),
            neonat_after=round(mean(neonat[treat==1],na.rm = TRUE),digits = 3))%>%
  mutate(diff=neonat_before-neonat_after)


df_neonat_2_pval_m<- df%>%
  subset(sex==0 & birth_order==2 & first_born_boy==1)%>%
  mutate(p_val=t.test(neonat~treat,var.equal=TRUE)$p.value)%>%
  subset(id==642)%>%
  select(birth_order,sex,p_val)

df_neonat_2_pval_f<- df%>%
  subset(sex==1 & birth_order==2 & first_born_boy==1)%>%
  mutate(p_val=t.test(neonat~treat,var.equal=TRUE)$p.value)%>%
  subset(id==640)%>%
  select(birth_order,sex,p_val)

df_neonat_3_pval_m<- df%>%
  subset(sex==0 & birth_order==3 & first_born_boy==1)%>%
  mutate(p_val=t.test(neonat~treat,var.equal=TRUE)$p.value)%>%
  subset(id==8)%>%
  select(birth_order,sex,p_val)

df_neonat_3_pval_f<- df%>%
  subset(sex==1 & birth_order==3 & first_born_boy==1)%>%
  mutate(p_val=t.test(neonat~treat,var.equal=TRUE)$p.value)%>%
  subset(id==8)%>%
  select(birth_order,sex,p_val)


df_neonat_pval<- rbind(df_neonat_2_pval_m,
                       df_neonat_2_pval_f,
                       df_neonat_3_pval_m,
                       df_neonat_3_pval_f)

df_neonat<- cbind(df_neonat,df_neonat_pval)

df_neonat<- df_neonat%>%
  select(birth_order...1,
         sex...2,
         neonat_before,
         neonat_after,
         diff,
         p_val)

kable(df_neonat,"latex")


##################### Infant survival probability ########################

df_infant<- df%>%
  subset(first_born_boy==1 & birth_order>1)%>%
  select(birth_order,treat,sex,infant)%>%
  group_by(birth_order,sex)%>%
  summarise(infant_before=round(mean(infant[treat==0],na.rm = TRUE),digits = 3),
            infant_after=round(mean(infant[treat==1],na.rm = TRUE),digits = 3))%>%
  mutate(diff=infant_before-infant_after)

df_infant_2_pval_m<- df%>%
  subset(sex==0 & birth_order==2 & first_born_boy==1)%>%
  mutate(p_val=t.test(infant~treat,var.equal=TRUE)$p.value)%>%
  subset(id==642)%>%
  select(birth_order,sex,p_val)

df_infant_2_pval_f<- df%>%
  subset(sex==1 & birth_order==2 & first_born_boy==1)%>%
  mutate(p_val=t.test(infant~treat,var.equal=TRUE)$p.value)%>%
  subset(id==640)%>%
  select(birth_order,sex,p_val)

df_infant_3_pval_m<- df%>%
  subset(sex==0 & birth_order==3 & first_born_boy==1)%>%
  mutate(p_val=t.test(infant~treat,var.equal=TRUE)$p.value)%>%
  subset(id==8)%>%
  select(birth_order,sex,p_val)

df_infant_3_pval_f<- df%>%
  subset(sex==1 & birth_order==3 & first_born_boy==1)%>%
  mutate(p_val=t.test(infant~treat,var.equal=TRUE)$p.value)%>%
  subset(id==8)%>%
  select(birth_order,sex,p_val)


df_infant_pval<- rbind(df_infant_2_pval_m,
                       df_infant_2_pval_f,
                       df_infant_3_pval_m,
                       df_infant_3_pval_f)

df_infant<- cbind(df_infant,df_infant_pval)

df_infant<- df_infant%>%
  select(birth_order...1,
         sex...2,
         infant_before,
         infant_after,
         diff,
         p_val)

kable(df_infant,"latex")


##################### Under 5 survival probability ########################

df_under5<- df%>%
  subset(first_born_boy==1 & birth_order>1)%>%
  select(birth_order,treat,sex,under5)%>%
  group_by(birth_order,sex)%>%
  summarise(under5_before=round(mean(under5[treat==0],na.rm = TRUE),digits = 3),
            under5_after=round(mean(under5[treat==1],na.rm = TRUE),digits = 3))%>%
  mutate(diff=under5_before-under5_after)

df_under5_2_pval_m<- df%>%
  subset(sex==0 & birth_order==2 & first_born_boy==1)%>%
  mutate(p_val=t.test(under5~treat,var.equal=TRUE)$p.value)%>%
  subset(id==642)%>%
  select(birth_order,sex,p_val)

df_under5_2_pval_f<- df%>%
  subset(sex==1 & birth_order==2 & first_born_boy==1)%>%
  mutate(p_val=t.test(under5~treat,var.equal=TRUE)$p.value)%>%
  subset(id==640)%>%
  select(birth_order,sex,p_val)

df_under5_3_pval_m<- df%>%
  subset(sex==0 & birth_order==3 & first_born_boy==1)%>%
  mutate(p_val=t.test(under5~treat,var.equal=TRUE)$p.value)%>%
  subset(id==8)%>%
  select(birth_order,sex,p_val)

df_under5_3_pval_f<- df%>%
  subset(sex==1 & birth_order==3 & first_born_boy==1)%>%
  mutate(p_val=t.test(under5~treat,var.equal=TRUE)$p.value)%>%
  subset(id==8)%>%
  select(birth_order,sex,p_val)


df_under5_pval<- rbind(df_under5_2_pval_m,
                       df_under5_2_pval_f,
                       df_under5_3_pval_m,
                       df_under5_3_pval_f)

df_under5<- cbind(df_under5,df_under5_pval)

df_under5<- df_under5%>%
  select(birth_order...1,
         sex...2,
         under5_before,
         under5_after,
         diff,
         p_val)

kable(df_under5,"latex")


##################### Under 10 survival probability ########################

df_under10<- df%>%
  subset(first_born_boy==1 & birth_order>1)%>%
  select(birth_order,treat,sex,under10)%>%
  group_by(birth_order,sex)%>%
  summarise(under10_before=round(mean(under10[treat==0],na.rm = TRUE),digits = 3),
            under10_after=round(mean(under10[treat==1],na.rm = TRUE),digits = 3))%>%
  mutate(diff=under10_before-under10_after)

df_under10_2_pval_m<- df%>%
  subset(sex==0 & birth_order==2 & first_born_boy==1)%>%
  mutate(p_val=t.test(under10~treat,var.equal=TRUE)$p.value)%>%
  subset(id==642)%>%
  select(birth_order,sex,p_val)

df_under10_2_pval_f<- df%>%
  subset(sex==1 & birth_order==2 & first_born_boy==1)%>%
  mutate(p_val=t.test(under10~treat,var.equal=TRUE)$p.value)%>%
  subset(id==640)%>%
  select(birth_order,sex,p_val)

df_under10_3_pval_m<- df%>%
  subset(sex==0 & birth_order==3 & first_born_boy==1)%>%
  mutate(p_val=t.test(under10~treat,var.equal=TRUE)$p.value)%>%
  subset(id==8)%>%
  select(birth_order,sex,p_val)

df_under10_3_pval_f<- df%>%
  subset(sex==1 & birth_order==3 & first_born_boy==1)%>%
  mutate(p_val=t.test(under10~treat,var.equal=TRUE)$p.value)%>%
  subset(id==8)%>%
  select(birth_order,sex,p_val)

df_under10_pval<- rbind(df_under10_2_pval_m,
                        df_under10_2_pval_f,
                        df_under10_3_pval_m,
                        df_under10_3_pval_f)

df_under10<- cbind(df_under10,df_under10_pval)

df_under10<- df_under10%>%
  select(birth_order...1,
         sex...2,
         under10_before,
         under10_after,
         diff,
         p_val)

kable(df_under10,"latex")

#################### When the first 2 born are boys ###############################


##################### Neonatal survival probability ########################

df_neonat<- df%>%
  subset(first_two_boys==1 & birth_order==3)%>%
  select(treat,sex,neonat)%>%
  group_by(sex)%>%
  summarise(neonat_before=round(mean(neonat[treat==0],na.rm = TRUE),digits = 3),
            neonat_after=round(mean(neonat[treat==1],na.rm = TRUE),digits = 3))%>%
  mutate(diff=neonat_before-neonat_after)

df_neonat_3_pval_m<- df%>%
  subset(sex==0 & birth_order==3 & first_two_boys==1)%>%
  mutate(p_val=t.test(neonat~treat,var.equal=TRUE)$p.value)%>%
  subset(id==24)%>%
  select(sex,p_val)

df_neonat_3_pval_f<- df%>%
  subset(sex==1 & birth_order==3 & first_two_boys==1)%>%
  mutate(p_val=t.test(neonat~treat,var.equal=TRUE)$p.value)%>%
  subset(id==44)%>%
  select(sex,p_val)


df_neonat_pval<- rbind(df_neonat_3_pval_m,
                       df_neonat_3_pval_f)

df_neonat<- cbind(df_neonat,df_neonat_pval)

kable(df_neonat,"latex")


##################### Infant survival probability ########################

df_infant<- df%>%
  subset(first_two_boys==1 & birth_order==3)%>%
  select(treat,sex,infant)%>%
  group_by(sex)%>%
  summarise(infant_before=round(mean(infant[treat==0],na.rm = TRUE),digits = 3),
            infant_after=round(mean(infant[treat==1],na.rm = TRUE),digits = 3))%>%
  mutate(diff=infant_before-infant_after)

df_infant_3_pval_m<- df%>%
  subset(sex==0 & birth_order==3 & first_two_boys==1)%>%
  mutate(p_val=t.test(infant~treat,var.equal=TRUE)$p.value)%>%
  subset(id==24)%>%
  select(sex,p_val)

df_infant_3_pval_f<- df%>%
  subset(sex==1 & birth_order==3 & first_two_boys==1)%>%
  mutate(p_val=t.test(infant~treat,var.equal=TRUE)$p.value)%>%
  subset(id==44)%>%
  select(sex,p_val)

df_infant_pval<- rbind(df_infant_3_pval_m,
                       df_infant_3_pval_f)

df_infant<- cbind(df_infant,df_infant_pval)

kable(df_infant,"latex")

##################### Under 5 survival probability ########################

df_under5<- df%>%
  subset(first_two_boys==1 & birth_order==3)%>%
  select(treat,sex,under5)%>%
  group_by(sex)%>%
  summarise(under5_before=round(mean(under5[treat==0],na.rm = TRUE),digits = 3),
            under5_after=round(mean(under5[treat==1],na.rm = TRUE),digits = 3))%>%
  mutate(diff=under5_before-under5_after)

df_under5_3_pval_m<- df%>%
  subset(sex==0 & birth_order==3 & first_two_boys==1)%>%
  mutate(p_val=t.test(under5~treat,var.equal=TRUE)$p.value)%>%
  subset(id==24)%>%
  select(sex,p_val)

df_under5_3_pval_f<- df%>%
  subset(sex==1 & birth_order==3 & first_two_boys==1)%>%
  mutate(p_val=t.test(under5~treat,var.equal=TRUE)$p.value)%>%
  subset(id==44)%>%
  select(sex,p_val)

df_under5_pval<- rbind(df_under5_3_pval_m,
                       df_under5_3_pval_f)

df_under5<- cbind(df_under5,df_under5_pval)

kable(df_under5,"latex")


##################### Under 10 survival probability ########################

df_under10<- df%>%
  subset(first_two_boys==1 & birth_order==3)%>%
  select(treat,sex,under10)%>%
  group_by(sex)%>%
  summarise(under10_before=round(mean(under10[treat==0],na.rm = TRUE),digits = 3),
            under10_after=round(mean(under10[treat==1],na.rm = TRUE),digits = 3))%>%
  mutate(diff=under10_before-under10_after)

df_under10_3_pval_m<- df%>%
  subset(sex==0 & birth_order==3 & first_two_boys==1)%>%
  mutate(p_val=t.test(under10~treat,var.equal=TRUE)$p.value)%>%
  subset(id==24)%>%
  select(sex,p_val)

df_under10_3_pval_f<- df%>%
  subset(sex==1 & birth_order==3 & first_two_boys==1)%>%
  mutate(p_val=t.test(under10~treat,var.equal=TRUE)$p.value)%>%
  subset(id==44)%>%
  select(sex,p_val)

df_under10_pval<- rbind(df_under10_3_pval_m,
                       df_under10_3_pval_f)

df_under10<- cbind(df_under10,df_under10_pval)

kable(df_under10,"latex")

#################### When the first 2 born are girls ###############################


##################### Neonatal survival probability ########################

df_neonat<- df%>%
  subset(first_two_girls==1 & birth_order==3)%>%
  select(treat,sex,neonat)%>%
  group_by(sex)%>%
  summarise(neonat_before=round(mean(neonat[treat==0],na.rm = TRUE),digits = 3),
            neonat_after=round(mean(neonat[treat==1],na.rm = TRUE),digits = 3))%>%
  mutate(diff=neonat_before-neonat_after)

df_neonat_3_pval_m<- df%>%
  subset(sex==0 & birth_order==3 & first_two_girls==1)%>%
  mutate(p_val=t.test(neonat~treat,var.equal=TRUE)$p.value)%>%
  subset(id==2)%>%
  select(sex,p_val)

df_neonat_3_pval_f<- df%>%
  subset(sex==1 & birth_order==3 & first_two_girls==1)%>%
  mutate(p_val=t.test(neonat~treat,var.equal=TRUE)$p.value)%>%
  subset(id==2)%>%
  select(sex,p_val)


df_neonat_pval<- rbind(df_neonat_3_pval_m,
                       df_neonat_3_pval_f)

df_neonat<- cbind(df_neonat,df_neonat_pval)

kable(df_neonat,"latex")


##################### Infant survival probability ########################

df_infant<- df%>%
  subset(first_two_girls==1 & birth_order==3)%>%
  select(treat,sex,infant)%>%
  group_by(sex)%>%
  summarise(infant_before=round(mean(infant[treat==0],na.rm = TRUE),digits = 3),
            infant_after=round(mean(infant[treat==1],na.rm = TRUE),digits = 3))%>%
  mutate(diff=infant_before-infant_after)

df_infant_3_pval_m<- df%>%
  subset(sex==0 & birth_order==3 & first_two_girls==1)%>%
  mutate(p_val=t.test(infant~treat,var.equal=TRUE)$p.value)%>%
  subset(id==2)%>%
  select(sex,p_val)

df_infant_3_pval_f<- df%>%
  subset(sex==1 & birth_order==3 & first_two_girls==1)%>%
  mutate(p_val=t.test(infant~treat,var.equal=TRUE)$p.value)%>%
  subset(id==2)%>%
  select(sex,p_val)

df_infant_pval<- rbind(df_infant_3_pval_m,
                       df_infant_3_pval_f)

df_infant<- cbind(df_infant,df_infant_pval)

kable(df_infant,"latex")

##################### Under 5 survival probability ########################

df_under5<- df%>%
  subset(first_two_girls==1 & birth_order==3)%>%
  select(treat,sex,under5)%>%
  group_by(sex)%>%
  summarise(under5_before=round(mean(under5[treat==0],na.rm = TRUE),digits = 3),
            under5_after=round(mean(under5[treat==1],na.rm = TRUE),digits = 3))%>%
  mutate(diff=under5_before-under5_after)

df_under5_3_pval_m<- df%>%
  subset(sex==0 & birth_order==3 & first_two_girls==1)%>%
  mutate(p_val=t.test(under5~treat,var.equal=TRUE)$p.value)%>%
  subset(id==2)%>%
  select(sex,p_val)

df_under5_3_pval_f<- df%>%
  subset(sex==1 & birth_order==3 & first_two_girls==1)%>%
  mutate(p_val=t.test(under5~treat,var.equal=TRUE)$p.value)%>%
  subset(id==2)%>%
  select(sex,p_val)

df_under5_pval<- rbind(df_under5_3_pval_m,
                       df_under5_3_pval_f)

df_under5<- cbind(df_under5,df_under5_pval)

kable(df_under5,"latex")

##################### Under 10 survival probability ########################

df_under10<- df%>%
  subset(first_two_girls==1 & birth_order==3)%>%
  select(treat,sex,under10)%>%
  group_by(sex)%>%
  summarise(under10_before=round(mean(under10[treat==0],na.rm = TRUE),digits = 3),
            under10_after=round(mean(under10[treat==1],na.rm = TRUE),digits = 3))%>%
  mutate(diff=under10_before-under10_after)

df_under10_3_pval_m<- df%>%
  subset(sex==0 & birth_order==3 & first_two_girls==1)%>%
  mutate(p_val=t.test(under10~treat,var.equal=TRUE)$p.value)%>%
  subset(id==2)%>%
  select(sex,p_val)

df_under10_3_pval_f<- df%>%
  subset(sex==1 & birth_order==3 & first_two_girls==1)%>%
  mutate(p_val=t.test(under10~treat,var.equal=TRUE)$p.value)%>%
  subset(id==2)%>%
  select(sex,p_val)

df_under10_pval<- rbind(df_under10_3_pval_m,
                        df_under10_3_pval_f)

df_under10<- cbind(df_under10,df_under10_pval)

kable(df_under10,"latex")
