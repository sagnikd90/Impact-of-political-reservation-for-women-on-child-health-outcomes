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

setwd("C:\\Users\\Sagnik\\OneDrive - Cuny GradCenter\\Research\\Political reservation and age of marriage\\Github\\Political-reservation-and-age-of-marriage-age-of-first-birth\\Final version\\Cleaned datasets")

df_1<- read.csv("C:\\Users\\Sagnik\\OneDrive - Cuny GradCenter\\Research\\Political reservation and age of marriage\\Github\\Political-reservation-and-age-of-marriage-age-of-first-birth\\Cleaned_DLHS_2.csv")


main_data<- df_1%>%
  filter(state_name %in% c("andhra pradesh",    ########### Selecting the states which had an election by 1995 ##############
                           "gujarat",
                           "karnataka",
                           "kerala",
                           "madhya pradesh",
                           "rajasthan",
                           "tripura",
                           "uttar pradesh",
                           "west bengal"))%>%
  mutate(read_write_woman=ifelse(read_write_woman==1,1,0)) ####### Converting the read and write variable into a binary variable ############# 

############# Election years ############################

main_data$first_election=""
main_data$second_election=""
main_data$last_election=""

### Andhra Pradesh ###

main_data$first_election[main_data$state_name=="andhra pradesh"]<- 1995        ########### Creating the election year variable ########
main_data$second_election[main_data$state_name=="andhra pradesh"]<- 2001
main_data$last_election[main_data$state_name=="andhra pradesh"]<- 2006

main_data$first_election_month[main_data$state_name=="andhra pradesh"]<- 3

### Madhya Pradesh ###

main_data$first_election[main_data$state_name=="madhya pradesh"]<- 1994
main_data$second_election[main_data$state_name=="madhya pradesh"]<- 1999
main_data$last_election[main_data$state_name=="madhya pardesh"]<- 2004

main_data$first_election_month[main_data$state_name=="madhya pradesh"]<- 6

###### Rajasthan #########

main_data$first_election[main_data$state_name=="rajasthan"]<- 1995
main_data$second_election[main_data$state_name=="rajasthan"]<- 2000
main_data$last_election[main_data$state_name=="rajasthan"]<- 2005

main_data$first_election_month[main_data$state_name=="rajasthan"]<- 3

### Tripura ###

main_data$first_election[main_data$state_name=="tripura"]<- 1994
main_data$second_election[main_data$state_name=="tripura"]<- 1999
main_data$last_election[main_data$state_name=="tripura"]<- 2004

main_data$first_election_month[main_data$state_name=="tripura"]<- 8

### Uttar Pradesh ###

main_data$first_election[main_data$state_name=="uttar pradesh"]<- 1995
main_data$second_election[main_data$state_name=="uttar pradesh"]<- 2000
main_data$last_election[main_data$state_name=="uttar pradesh"]<- 2005

main_data$first_election_month[main_data$state_name=="uttar pradesh"]<- 4

### Gujrat ###

main_data$first_election[main_data$state_name=="gujarat"]<- 1995
main_data$second_election[main_data$state_name=="gujarat"]<- 2000
main_data$last_election[main_data$state_name=="gujarat"]<- 2007

main_data$first_election_month[main_data$state_name=="gujarat"]<- 6

### Kerala ###

main_data$first_election[main_data$state_name=="kerala"]<- 1995
main_data$second_election[main_data$state_name=="kerala"]<- 2000
main_data$last_election[main_data$state_name=="kerala"]<- 2005

main_data$first_election_month[main_data$state_name=="kerala"]<- 9

### Karnataka ###

main_data$first_election[main_data$state_name=="karnataka"]<- 1995
main_data$second_election[main_data$state_name=="karnataka"]<- 2000
main_data$last_election[main_data$state_name=="karnataka"]<- 2005

main_data$first_election_month[main_data$state_name=="karnataka"]<- 12

### West Bengal ###

main_data$first_election[main_data$state_name=="west bengal"]<- 1993
main_data$second_election[main_data$state_name=="west bengal"]<- 1998
main_data$last_election[main_data$state_name=="west bengal"]<- 2003

main_data$first_election_month[main_data$state_name=="west bengal"]<- 5

main_data$first_election<- as.integer(main_data$first_election)
main_data$second_election<- as.integer(main_data$second_election)
main_data$last_election<- as.integer(main_data$last_election)

main_data$first_election_month<- as.integer(main_data$first_election_month)

main_data<- main_data%>%
  mutate(election=(first_election*12)+first_election_month,         ########## Converting the date of birth of each child into months ############
         months_1_born=(birth_yr_1_born*12)+birth_month_1_born,
         months_2_born=(birth_yr_2_born*12)+birth_month_2_born,
         months_3_born=(birth_yr_3_born*12)+birth_month_3_born,
         months_4_born=(birth_yr_4_born*12)+birth_month_4_born,
         months_5_born=(birth_yr_5_born*12)+birth_month_5_born,
         months_6_born=(birth_yr_6_born*12)+birth_month_6_born,
         months_7_born=(birth_yr_7_born*12)+birth_month_7_born,
         months_8_born=(birth_yr_8_born*12)+birth_month_8_born,
         months_9_born=(birth_yr_9_born*12)+birth_month_9_born,
         months_10_born=(birth_yr_10_born*12)+birth_month_10_born,
         months_11_born=(birth_yr_11_born*12)+birth_month_11_born,
         months_12_born=(birth_yr_12_born*12)+birth_month_12_born,
         months_13_born=(birth_yr_13_born*12)+birth_month_13_born,
         months_14_born=(birth_yr_14_born*12)+birth_month_14_born)

############################################### Share of children alive by birth order ########################################################

main_data$alive_3_born<- as.character(main_data$alive_3_born)
main_data$alive_4_born<- as.character(main_data$alive_4_born) 

main_data$alive_1_born<- ifelse(main_data$alive_1_born==1,1,0)
main_data$alive_2_born<- ifelse(main_data$alive_2_born==1,1,0)     ############ Converting the survival status of each child into a binary variable ##########
main_data$alive_3_born<- ifelse(main_data$alive_3_born=="yes",1,0)
main_data$alive_4_born<- ifelse(main_data$alive_4_born=="yes",1,0)
main_data$alive_5_born<- ifelse(main_data$alive_5_born=="yes",1,0)
main_data$alive_6_born<- ifelse(main_data$alive_6_born=="yes",1,0)
main_data$alive_7_born<- ifelse(main_data$alive_7_born=="yes",1,0)
main_data$alive_8_born<- ifelse(main_data$alive_8_born=="yes",1,0)
main_data$alive_9_born<- ifelse(main_data$alive_9_born=="yes",1,0)
main_data$alive_10_born<- ifelse(main_data$alive_10_born=="yes",1,0)
main_data$alive_11_born<- ifelse(main_data$alive_11_born=="yes",1,0)
main_data$alive_12_born<- ifelse(main_data$alive_12_born=="yes",1,0)
main_data$alive_13_born<- ifelse(main_data$alive_13_born=="yes",1,0)
main_data$alive_14_born<- ifelse(main_data$alive_14_born=="yes",1,0)

################## Neonatal/Infant/Under 5/Under 10 mortality ###############################

main_data<- main_data%>%
  mutate(neonat_1=ifelse(alive_1_born==1|age_month_death_1_born>1,1,0),       ########## Creating the neonatal, infant, under 5 years and under 10 years survival status variable ######
         neonat_2=ifelse(alive_2_born==1|age_month_death_2_born>1,1,0),
         neonat_3=ifelse(alive_3_born==1|age_month_death_3_born>1,1,0),
         neonat_4=ifelse(alive_4_born==1|age_month_death_4_born>1,1,0),
         neonat_5=ifelse(alive_5_born==1|age_month_death_5_born>1,1,0),
         neonat_6=ifelse(alive_6_born==1|age_month_death_6_born>1,1,0),
         neonat_7=ifelse(alive_7_born==1|age_month_death_7_born>1,1,0),
         neonat_8=ifelse(alive_8_born==1|age_month_death_8_born>1,1,0),
         neonat_9=ifelse(alive_9_born==1|age_month_death_9_born>1,1,0),
         neonat_10=ifelse(alive_10_born==1|age_month_death_10_born>1,1,0),
         neonat_11=ifelse(alive_11_born==1|age_month_death_11_born>1,1,0),
         neonat_12=ifelse(alive_12_born==1|age_month_death_12_born>1,1,0),
         neonat_13=ifelse(alive_13_born==1|age_month_death_13_born>1,1,0),
         neonat_14=ifelse(alive_14_born==1|age_month_death_14_born>1,1,0),
         infant_1=ifelse(alive_1_born==1|age_month_death_1_born>12,1,0),
         infant_2=ifelse(alive_2_born==1|age_month_death_2_born>12,1,0),
         infant_3=ifelse(alive_3_born==1|age_month_death_3_born>12,1,0),
         infant_4=ifelse(alive_4_born==1|age_month_death_4_born>12,1,0),
         infant_5=ifelse(alive_5_born==1|age_month_death_5_born>12,1,0),
         infant_6=ifelse(alive_6_born==1|age_month_death_6_born>12,1,0),
         infant_7=ifelse(alive_7_born==1|age_month_death_7_born>12,1,0),
         infant_8=ifelse(alive_8_born==1|age_month_death_8_born>12,1,0),
         infant_9=ifelse(alive_9_born==1|age_month_death_9_born>12,1,0),
         infant_10=ifelse(alive_10_born==1|age_month_death_10_born>12,1,0),
         infant_11=ifelse(alive_11_born==1|age_month_death_11_born>12,1,0),
         infant_12=ifelse(alive_12_born==1|age_month_death_12_born>12,1,0),
         infant_13=ifelse(alive_13_born==1|age_month_death_13_born>12,1,0),
         infant_14=ifelse(alive_14_born==1|age_month_death_14_born>12,1,0),
         under5_1=ifelse(alive_1_born==1|age_month_death_1_born>60,1,0),
         under5_2=ifelse(alive_2_born==1|age_month_death_2_born>60,1,0),
         under5_3=ifelse(alive_3_born==1|age_month_death_3_born>60,1,0),
         under5_4=ifelse(alive_4_born==1|age_month_death_4_born>60,1,0),
         under5_5=ifelse(alive_5_born==1|age_month_death_5_born>60,1,0),
         under5_6=ifelse(alive_6_born==1|age_month_death_6_born>60,1,0),
         under5_7=ifelse(alive_7_born==1|age_month_death_7_born>60,1,0),
         under5_8=ifelse(alive_8_born==1|age_month_death_8_born>60,1,0),
         under5_9=ifelse(alive_9_born==1|age_month_death_9_born>60,1,0),
         under5_10=ifelse(alive_10_born==1|age_month_death_10_born>60,1,0),
         under5_11=ifelse(alive_11_born==1|age_month_death_11_born>60,1,0),
         under5_12=ifelse(alive_12_born==1|age_month_death_12_born>60,1,0),
         under5_13=ifelse(alive_13_born==1|age_month_death_13_born>60,1,0),
         under5_14=ifelse(alive_14_born==1|age_month_death_14_born>60,1,0),
         under10_1=ifelse(alive_1_born==1|age_month_death_1_born>120,1,0),
         under10_2=ifelse(alive_2_born==1|age_month_death_2_born>120,1,0),
         under10_3=ifelse(alive_3_born==1|age_month_death_3_born>120,1,0),
         under10_4=ifelse(alive_4_born==1|age_month_death_4_born>120,1,0),
         under10_5=ifelse(alive_5_born==1|age_month_death_5_born>120,1,0),
         under10_6=ifelse(alive_6_born==1|age_month_death_6_born>120,1,0),
         under10_7=ifelse(alive_7_born==1|age_month_death_7_born>120,1,0),
         under10_8=ifelse(alive_8_born==1|age_month_death_8_born>120,1,0),
         under10_9=ifelse(alive_9_born==1|age_month_death_9_born>120,1,0),
         under10_10=ifelse(alive_10_born==1|age_month_death_10_born>120,1,0),
         under10_11=ifelse(alive_11_born==1|age_month_death_11_born>120,1,0),
         under10_12=ifelse(alive_12_born==1|age_month_death_12_born>120,1,0),
         under10_13=ifelse(alive_13_born==1|age_month_death_13_born>120,1,0),
         under10_14=ifelse(alive_14_born==1|age_month_death_14_born>120,1,0))


############# Mother's age at birth for different birth orders ##############

main_data<- main_data%>%
  mutate(wom_age_1=birth_yr_1_born-yr_birth_woman, ######### Creating the woman's age at birth for each birth ###############
         wom_age_2=birth_yr_2_born-yr_birth_woman,
         wom_age_3=birth_yr_3_born-yr_birth_woman,
         wom_age_4=birth_yr_4_born-yr_birth_woman,
         wom_age_5=birth_yr_5_born-yr_birth_woman,
         wom_age_6=birth_yr_6_born-yr_birth_woman,
         wom_age_7=birth_yr_7_born-yr_birth_woman,
         wom_age_8=birth_yr_8_born-yr_birth_woman,
         wom_age_9=birth_yr_9_born-yr_birth_woman,
         wom_age_10=birth_yr_10_born-yr_birth_woman,
         wom_age_11=birth_yr_11_born-yr_birth_woman,
         wom_age_12=birth_yr_12_born-yr_birth_woman,
         wom_age_13=birth_yr_13_born-yr_birth_woman,
         wom_age_14=birth_yr_14_born-yr_birth_woman)

############################### Redefining the required variables #############################

main_data<- main_data%>%
  mutate(sex_1_born=ifelse(sex_1_born==2,1,0), ############ Converting other variables into binary variables ###############
         sex_2_born=ifelse(sex_2_born==2,1,0),
         sex_3_born=ifelse(sex_3_born==2,1,0),
         sex_4_born=ifelse(sex_4_born==2,1,0),
         sex_5_born=ifelse(sex_5_born==2,1,0),
         sex_6_born=ifelse(sex_6_born==2,1,0),
         sex_7_born=ifelse(sex_7_born==2,1,0),
         sex_8_born=ifelse(sex_8_born==2,1,0),
         sex_9_born=ifelse(sex_9_born==2,1,0),
         sex_10_born=ifelse(sex_10_born==2,1,0),
         sex_11_born=ifelse(sex_11_born==2,1,0),
         sex_12_born=ifelse(sex_12_born==2,1,0),
         sex_13_born=ifelse(sex_13_born==2,1,0),
         sex_14_born=ifelse(sex_14_born==2,1,0),
         birth_1_boy=ifelse(sex_1_born==0,1,0),
         first_born_alive=ifelse(alive_1_born==1,1,0),
         second_born_alive=ifelse(alive_2_born==1,1,0),
         second_born_girl=ifelse(sex_2_born==1,1,0),
         third_born_girl=ifelse(sex_3_born==1|
                                  sex_4_born==1|
                                  sex_5_born==1|
                                  sex_6_born==1|
                                  sex_7_born==1|
                                  sex_8_born==1|
                                  sex_9_born==1|
                                  sex_10_born==1|
                                  sex_11_born==1|
                                  sex_12_born==1|
                                  sex_13_born==1|
                                  sex_14_born==1,1,0),
         first_born_boy=ifelse(alive_1_born==1 & sex_1_born==0,1,0),
         first_two_boys=ifelse(sex_1_born==0 & sex_2_born==0,1,0),
         first_two_girls=ifelse(sex_1_born==1 & sex_2_born==1,1,0),
         first_boy_2nd_girl=ifelse(sex_1_born==0 & sex_2_born==1,1,0),
         first_girl_2nd_boy=ifelse(sex_1_born==1 & sex_2_born==0,1,0),
         id=row_number())

df_2<- main_data%>%
  select(id,
         alive_1_born,
         alive_2_born,
         alive_3_born,
         alive_4_born,
         alive_5_born,
         alive_6_born,
         alive_7_born,
         alive_8_born,
         alive_9_born,
         alive_10_born,
         alive_11_born,
         alive_12_born,
         alive_13_born,
         alive_14_born,
         neonat_1,
         neonat_2,
         neonat_3,
         neonat_4,
         neonat_5,
         neonat_6,
         neonat_7,
         neonat_8,
         neonat_9,
         neonat_10,
         neonat_11,
         neonat_12,
         neonat_13,
         neonat_14,
         infant_1,
         infant_2,
         infant_3,
         infant_4,
         infant_5,
         infant_6,
         infant_7,
         infant_8,
         infant_9,
         infant_10,
         infant_11,
         infant_12,
         infant_13,
         infant_14,
         under5_1,
         under5_2,
         under5_3,
         under5_4,
         under5_5,
         under5_6,
         under5_7,
         under5_8,
         under5_9,
         under5_10,
         under5_11,
         under5_12,
         under5_13,
         under5_14,
         under10_1,
         under10_2,
         under10_3,
         under10_4,
         under10_5,
         under10_6,
         under10_7,
         under10_8,
         under10_9,
         under10_10,
         under10_11,
         under10_12,
         under10_13,
         under10_14,
         birth_yr_1_born,
         birth_yr_2_born,
         birth_yr_3_born,
         birth_yr_4_born,
         birth_yr_5_born,
         birth_yr_6_born,
         birth_yr_7_born,
         birth_yr_8_born,
         birth_yr_9_born,
         birth_yr_10_born,
         birth_yr_11_born,
         birth_yr_12_born,
         birth_yr_13_born,
         birth_yr_14_born,
         birth_month_1_born,
         birth_month_2_born,
         birth_month_3_born,
         birth_month_4_born,
         birth_month_5_born,
         birth_month_6_born,
         birth_month_7_born,
         birth_month_8_born,
         birth_month_9_born,
         birth_month_10_born,
         birth_month_11_born,
         birth_month_12_born,
         birth_month_13_born,
         birth_month_14_born,
         months_1_born,
         months_2_born,
         months_3_born,
         months_4_born,
         months_5_born,
         months_6_born,
         months_7_born,
         months_8_born,
         months_9_born,
         months_10_born,
         months_11_born,
         months_12_born,
         months_13_born,
         months_14_born,
         sex_1_born,
         sex_2_born,
         sex_3_born,
         sex_4_born,
         sex_5_born,
         sex_6_born,
         sex_7_born,
         sex_8_born,
         sex_9_born,
         sex_10_born,
         sex_11_born,
         sex_12_born,
         sex_13_born,
         sex_14_born,
         wom_age_1,
         wom_age_2,
         wom_age_3,
         wom_age_4,
         wom_age_5,
         wom_age_6,
         wom_age_7,
         wom_age_8,
         wom_age_9,
         wom_age_10,
         wom_age_11,
         wom_age_12,
         wom_age_13,
         wom_age_14,
         yr_mrrg_woman)

########################### First order births ############################

df_2_1<- df_2%>%
  select(id,months_1_born)

df_2_1<- melt(df_2_1,id.vars = c("id","months_1_born"))

df_2_1<- df_2_1%>%
  mutate(cohort=months_1_born)%>%
  select(id,cohort)

df_2_2<- df_2%>%
  select(id,sex_1_born)

df_2_2<- melt(df_2_2,id.vars = c("id","sex_1_born"))

df_2_2<- df_2_2%>%
  mutate(sex=sex_1_born)%>%
  select(id,sex)

df_2_3<- df_2%>%
  select(id,alive_1_born)

df_2_3<- melt(df_2_3,id.vars = c("id","alive_1_born"))

df_2_3<- df_2_3%>%
  mutate(alive=alive_1_born)%>%
  select(id,alive)

df_2_4<- df_2%>%
  select(id,neonat_1)

df_2_4<- melt(df_2_4,id.vars = c("id","neonat_1"))

df_2_4<- df_2_4%>%
  mutate(neonat=neonat_1)%>%
  select(id,neonat)

df_2_5<- df_2%>%
  select(id,infant_1)

df_2_5<- melt(df_2_5,id.vars = c("id","infant_1"))

df_2_5<- df_2_5%>%
  mutate(infant=infant_1)%>%
  select(id,infant)

df_2_6<- df_2%>%
  select(id,under5_1)

df_2_6<- melt(df_2_6,id.vars = c("id","under5_1"))

df_2_6<- df_2_6%>%
  mutate(under5=under5_1)%>%
  select(id,under5)

df_2_7<- df_2%>%
  select(id,under10_1)

df_2_7<- melt(df_2_7,id.vars = c("id","under10_1"))

df_2_7<- df_2_7%>%
  mutate(under10=under10_1)%>%
  select(id,under10)

df_2_8<- df_2%>%
  select(id,birth_yr_1_born)

df_2_8<- melt(df_2_8,id.vars = c("id","birth_yr_1_born"))

df_2_8<- df_2_8%>%
  mutate(cohort_year=birth_yr_1_born)%>%
  select(id,cohort_year)

df_2_9<- df_2%>%
  select(id,wom_age_1)

df_2_9<- melt(df_2_9,id.vars = c("id","wom_age_1"))

df_2_9<- df_2_9%>%
  mutate(mothers_age_at_birth=wom_age_1)%>%
  select(id,mothers_age_at_birth)

df_2_10<- df_2%>%
  select(id,yr_mrrg_woman,birth_yr_1_born,birth_month_1_born)

df_2_10<- melt(df_2_10,id.vars = c("id","yr_mrrg_woman","birth_yr_1_born","birth_month_1_born"))

df_2_10<- df_2_10%>%
  mutate(wait_time=(birth_yr_1_born-yr_mrrg_woman)*12+(birth_month_1_born))%>%
  select(id,wait_time)

df_first_order<- merge(df_2_1,df_2_2[, c("id", setdiff(colnames(df_2_2),colnames(df_2_1)))], by="id")

df_first_order<- merge(df_first_order,df_2_3[, c("id", setdiff(colnames(df_2_3),colnames(df_first_order)))], by="id")

df_first_order<- merge(df_first_order,df_2_4[, c("id", setdiff(colnames(df_2_4),colnames(df_first_order)))], by="id")

df_first_order<- merge(df_first_order,df_2_5[, c("id", setdiff(colnames(df_2_5),colnames(df_first_order)))], by="id")

df_first_order<- merge(df_first_order,df_2_6[, c("id", setdiff(colnames(df_2_6),colnames(df_first_order)))], by="id")

df_first_order<- merge(df_first_order,df_2_7[, c("id", setdiff(colnames(df_2_7),colnames(df_first_order)))], by="id")

df_first_order<- merge(df_first_order,df_2_8[, c("id", setdiff(colnames(df_2_8),colnames(df_first_order)))], by="id")

df_first_order<- merge(df_first_order,df_2_9[, c("id", setdiff(colnames(df_2_9),colnames(df_first_order)))], by="id")

df_first_order<- merge(df_first_order,df_2_10[, c("id", setdiff(colnames(df_2_10),colnames(df_first_order)))], by="id")

df_first_order<- df_first_order%>%
  mutate(birth_order=1)

rm(df_2_1,
   df_2_2,
   df_2_3,
   df_2_4,
   df_2_5,
   df_2_6,
   df_2_7,
   df_2_8,
   df_2_9,
   df_2_10)

########################### Second order births ############################

df_2_1<- df_2%>%
  select(id,months_2_born)

df_2_1<- melt(df_2_1,id.vars = c("id","months_2_born"))

df_2_1<- df_2_1%>%
  mutate(cohort=months_2_born)%>%
  select(id,cohort)

df_2_2<- df_2%>%
  select(id,sex_2_born)

df_2_2<- melt(df_2_2,id.vars = c("id","sex_2_born"))

df_2_2<- df_2_2%>%
  mutate(sex=sex_2_born)%>%
  select(id,sex)

df_2_3<- df_2%>%
  select(id,alive_2_born)

df_2_3<- melt(df_2_3,id.vars = c("id","alive_2_born"))

df_2_3<- df_2_3%>%
  mutate(alive=alive_2_born)%>%
  select(id,alive)

df_2_4<- df_2%>%
  select(id,neonat_2)

df_2_4<- melt(df_2_4,id.vars = c("id","neonat_2"))

df_2_4<- df_2_4%>%
  mutate(neonat=neonat_2)%>%
  select(id,neonat)

df_2_5<- df_2%>%
  select(id,infant_2)

df_2_5<- melt(df_2_5,id.vars = c("id","infant_2"))

df_2_5<- df_2_5%>%
  mutate(infant=infant_2)%>%
  select(id,infant)

df_2_6<- df_2%>%
  select(id,under5_2)

df_2_6<- melt(df_2_6,id.vars = c("id","under5_2"))

df_2_6<- df_2_6%>%
  mutate(under5=under5_2)%>%
  select(id,under5)

df_2_7<- df_2%>%
  select(id,under10_2)

df_2_7<- melt(df_2_7,id.vars = c("id","under10_2"))

df_2_7<- df_2_7%>%
  mutate(under10=under10_2)%>%
  select(id,under10)

df_2_8<- df_2%>%
  select(id,birth_yr_2_born)

df_2_8<- melt(df_2_8,id.vars = c("id","birth_yr_2_born"))

df_2_8<- df_2_8%>%
  mutate(cohort_year=birth_yr_2_born)%>%
  select(id,cohort_year)

df_2_9<- df_2%>%
  select(id,wom_age_2)

df_2_9<- melt(df_2_9,id.vars = c("id","wom_age_2"))

df_2_9<- df_2_9%>%
  mutate(mothers_age_at_birth=wom_age_2)%>%
  select(id,mothers_age_at_birth)

df_2_10<- df_2%>%
  select(id,birth_yr_1_born,birth_yr_2_born,birth_month_1_born,birth_month_2_born)

df_2_10<- melt(df_2_10,id.vars = c("id","birth_yr_1_born","birth_month_1_born","birth_yr_2_born","birth_month_2_born"))

df_2_10<- df_2_10%>%
  mutate(wait_time=((birth_yr_2_born*12+birth_month_2_born)-(birth_yr_1_born*12+birth_month_1_born)))%>%
  select(id,wait_time)

df_second_order<- merge(df_2_1,df_2_2[, c("id", setdiff(colnames(df_2_2),colnames(df_2_1)))], by="id")

df_second_order<- merge(df_second_order,df_2_3[, c("id", setdiff(colnames(df_2_3),colnames(df_second_order)))], by="id")

df_second_order<- merge(df_second_order,df_2_4[, c("id", setdiff(colnames(df_2_4),colnames(df_second_order)))], by="id")

df_second_order<- merge(df_second_order,df_2_5[, c("id", setdiff(colnames(df_2_5),colnames(df_second_order)))], by="id")

df_second_order<- merge(df_second_order,df_2_6[, c("id", setdiff(colnames(df_2_6),colnames(df_second_order)))], by="id")

df_second_order<- merge(df_second_order,df_2_7[, c("id", setdiff(colnames(df_2_7),colnames(df_second_order)))], by="id")

df_second_order<- merge(df_second_order,df_2_8[, c("id", setdiff(colnames(df_2_8),colnames(df_second_order)))], by="id")

df_second_order<- merge(df_second_order,df_2_9[, c("id", setdiff(colnames(df_2_9),colnames(df_second_order)))], by="id")

df_second_order<- merge(df_second_order,df_2_10[, c("id", setdiff(colnames(df_2_10),colnames(df_second_order)))], by="id")

df_second_order<- df_second_order%>%
  mutate(birth_order=2)%>%
  subset(sex==1)

rm(df_2_1,
   df_2_2,
   df_2_3,
   df_2_4,
   df_2_5,
   df_2_6,
   df_2_7,
   df_2_8,
   df_2_9,
   df_2_10)

################# Third order births ###########################

df_2_1<- df_2%>%
  select(id,months_3_born)

df_2_1<- melt(df_2_1,id.vars = c("id","months_3_born"))

df_2_1<- df_2_1%>%
  mutate(cohort=months_3_born)%>%
  select(id,cohort)

df_2_2<- df_2%>%
  select(id,sex_3_born)

df_2_2<- melt(df_2_2,id.vars = c("id","sex_3_born"))

df_2_2<- df_2_2%>%
  mutate(sex=sex_3_born)%>%
  select(id,sex)

df_2_3<- df_2%>%
  select(id,alive_3_born)

df_2_3<- melt(df_2_3,id.vars = c("id","alive_3_born"))

df_2_3<- df_2_3%>%
  mutate(alive=alive_3_born)%>%
  select(id,alive)

df_2_4<- df_2%>%
  select(id,neonat_3)

df_2_4<- melt(df_2_4,id.vars = c("id","neonat_3"))

df_2_4<- df_2_4%>%
  mutate(neonat=neonat_3)%>%
  select(id,neonat)

df_2_5<- df_2%>%
  select(id,infant_3)

df_2_5<- melt(df_2_5,id.vars = c("id","infant_3"))

df_2_5<- df_2_5%>%
  mutate(infant=infant_3)%>%
  select(id,infant)

df_2_6<- df_2%>%
  select(id,under5_3)

df_2_6<- melt(df_2_6,id.vars = c("id","under5_3"))

df_2_6<- df_2_6%>%
  mutate(under5=under5_3)%>%
  select(id,under5)

df_2_7<- df_2%>%
  select(id,under10_3)

df_2_7<- melt(df_2_7,id.vars = c("id","under10_3"))

df_2_7<- df_2_7%>%
  mutate(under10=under10_3)%>%
  select(id,under10)

df_2_8<- df_2%>%
  select(id,birth_yr_3_born)

df_2_8<- melt(df_2_8,id.vars = c("id","birth_yr_3_born"))

df_2_8<- df_2_8%>%
  mutate(cohort_year=birth_yr_3_born)%>%
  select(id,cohort_year)

df_2_9<- df_2%>%
  select(id,wom_age_3)

df_2_9<- melt(df_2_9,id.vars = c("id","wom_age_3"))

df_2_9<- df_2_9%>%
  mutate(mothers_age_at_birth=wom_age_3)%>%
  select(id,mothers_age_at_birth)

df_2_10<- df_2%>%
  select(id,birth_yr_2_born,birth_yr_3_born,birth_month_2_born,birth_month_3_born)

df_2_10<- melt(df_2_10,id.vars = c("id","birth_yr_2_born","birth_month_2_born","birth_yr_3_born","birth_month_3_born"))

df_2_10<- df_2_10%>%
  mutate(wait_time=((birth_yr_3_born*12+birth_month_3_born)-(birth_yr_2_born*12+birth_month_2_born)))%>%
  select(id,wait_time)

df_third_order<- merge(df_2_1,df_2_2[, c("id", setdiff(colnames(df_2_2),colnames(df_2_1)))], by="id")

df_third_order<- merge(df_third_order,df_2_3[, c("id", setdiff(colnames(df_2_3),colnames(df_third_order)))], by="id")

df_third_order<- merge(df_third_order,df_2_4[, c("id", setdiff(colnames(df_2_4),colnames(df_third_order)))], by="id")

df_third_order<- merge(df_third_order,df_2_5[, c("id", setdiff(colnames(df_2_5),colnames(df_third_order)))], by="id")

df_third_order<- merge(df_third_order,df_2_6[, c("id", setdiff(colnames(df_2_6),colnames(df_third_order)))], by="id")

df_third_order<- merge(df_third_order,df_2_7[, c("id", setdiff(colnames(df_2_7),colnames(df_third_order)))], by="id")

df_third_order<- merge(df_third_order,df_2_8[, c("id", setdiff(colnames(df_2_8),colnames(df_third_order)))], by="id")

df_third_order<- merge(df_third_order,df_2_9[, c("id", setdiff(colnames(df_2_9),colnames(df_third_order)))], by="id")

df_third_order<- merge(df_third_order,df_2_10[, c("id", setdiff(colnames(df_2_10),colnames(df_third_order)))], by="id")

df_third_order<- df_third_order%>%
  mutate(birth_order=3)%>%
  subset(sex==1)

rm(df_2_1,
   df_2_2,
   df_2_3,
   df_2_4,
   df_2_5,
   df_2_6,
   df_2_7,
   df_2_8,
   df_2_9,
   df_2_10)

################# Fourth order births ###########################

df_2_1<- df_2%>%
  select(id,months_4_born)

df_2_1<- melt(df_2_1,id.vars = c("id","months_4_born"))

df_2_1<- df_2_1%>%
  mutate(cohort=months_4_born)%>%
  select(id,cohort)

df_2_2<- df_2%>%
  select(id,sex_4_born)

df_2_2<- melt(df_2_2,id.vars = c("id","sex_4_born"))

df_2_2<- df_2_2%>%
  mutate(sex=sex_4_born)%>%
  select(id,sex)

df_2_3<- df_2%>%
  select(id,alive_4_born)

df_2_3<- melt(df_2_3,id.vars = c("id","alive_4_born"))

df_2_3<- df_2_3%>%
  mutate(alive=alive_4_born)%>%
  select(id,alive)

df_2_4<- df_2%>%
  select(id,neonat_4)

df_2_4<- melt(df_2_4,id.vars = c("id","neonat_4"))

df_2_4<- df_2_4%>%
  mutate(neonat=neonat_4)%>%
  select(id,neonat)

df_2_5<- df_2%>%
  select(id,infant_4)

df_2_5<- melt(df_2_5,id.vars = c("id","infant_4"))

df_2_5<- df_2_5%>%
  mutate(infant=infant_4)%>%
  select(id,infant)

df_2_6<- df_2%>%
  select(id,under5_4)

df_2_6<- melt(df_2_6,id.vars = c("id","under5_4"))

df_2_6<- df_2_6%>%
  mutate(under5=under5_4)%>%
  select(id,under5)

df_2_7<- df_2%>%
  select(id,under10_4)

df_2_7<- melt(df_2_7,id.vars = c("id","under10_4"))

df_2_7<- df_2_7%>%
  mutate(under10=under10_4)%>%
  select(id,under10)

df_2_8<- df_2%>%
  select(id,birth_yr_4_born)

df_2_8<- melt(df_2_8,id.vars = c("id","birth_yr_4_born"))

df_2_8<- df_2_8%>%
  mutate(cohort_year=birth_yr_4_born)%>%
  select(id,cohort_year)

df_2_9<- df_2%>%
  select(id,wom_age_4)

df_2_9<- melt(df_2_9,id.vars = c("id","wom_age_4"))

df_2_9<- df_2_9%>%
  mutate(mothers_age_at_birth=wom_age_4)%>%
  select(id,mothers_age_at_birth)

df_2_10<- df_2%>%
  select(id,birth_yr_3_born,birth_yr_4_born,birth_month_3_born,birth_month_4_born)

df_2_10<- melt(df_2_10,id.vars = c("id","birth_yr_3_born","birth_month_3_born","birth_yr_4_born","birth_month_4_born"))

df_2_10<- df_2_10%>%
  mutate(wait_time=((birth_yr_4_born*12+birth_month_4_born)-(birth_yr_3_born*12+birth_month_3_born)))%>%
  select(id,wait_time)

df_fourth_order<- merge(df_2_1,df_2_2[, c("id", setdiff(colnames(df_2_2),colnames(df_2_1)))], by="id")

df_fourth_order<- merge(df_fourth_order,df_2_3[, c("id", setdiff(colnames(df_2_3),colnames(df_fourth_order)))], by="id")

df_fourth_order<- merge(df_fourth_order,df_2_4[, c("id", setdiff(colnames(df_2_4),colnames(df_fourth_order)))], by="id")

df_fourth_order<- merge(df_fourth_order,df_2_5[, c("id", setdiff(colnames(df_2_5),colnames(df_fourth_order)))], by="id")

df_fourth_order<- merge(df_fourth_order,df_2_6[, c("id", setdiff(colnames(df_2_6),colnames(df_fourth_order)))], by="id")

df_fourth_order<- merge(df_fourth_order,df_2_7[, c("id", setdiff(colnames(df_2_7),colnames(df_fourth_order)))], by="id")

df_fourth_order<- merge(df_fourth_order,df_2_8[, c("id", setdiff(colnames(df_2_8),colnames(df_fourth_order)))], by="id")

df_fourth_order<- merge(df_fourth_order,df_2_9[, c("id", setdiff(colnames(df_2_9),colnames(df_fourth_order)))], by="id")

df_fourth_order<- merge(df_fourth_order,df_2_10[, c("id", setdiff(colnames(df_2_10),colnames(df_fourth_order)))], by="id")

df_fourth_order<- df_fourth_order%>%
  mutate(birth_order=3)%>%
  subset(sex==1)

rm(df_2_1,
   df_2_2,
   df_2_3,
   df_2_4,
   df_2_5,
   df_2_6,
   df_2_7,
   df_2_8,
   df_2_9,
   df_2_10)

################# Fifth order births ###########################

df_2_1<- df_2%>%
  select(id,months_5_born)

df_2_1<- melt(df_2_1,id.vars = c("id","months_5_born"))

df_2_1<- df_2_1%>%
  mutate(cohort=months_5_born)%>%
  select(id,cohort)

df_2_2<- df_2%>%
  select(id,sex_5_born)

df_2_2<- melt(df_2_2,id.vars = c("id","sex_5_born"))

df_2_2<- df_2_2%>%
  mutate(sex=sex_5_born)%>%
  select(id,sex)

df_2_3<- df_2%>%
  select(id,alive_5_born)

df_2_3<- melt(df_2_3,id.vars = c("id","alive_5_born"))

df_2_3<- df_2_3%>%
  mutate(alive=alive_5_born)%>%
  select(id,alive)

df_2_4<- df_2%>%
  select(id,neonat_5)

df_2_4<- melt(df_2_4,id.vars = c("id","neonat_5"))

df_2_4<- df_2_4%>%
  mutate(neonat=neonat_5)%>%
  select(id,neonat)

df_2_5<- df_2%>%
  select(id,infant_5)

df_2_5<- melt(df_2_5,id.vars = c("id","infant_5"))

df_2_5<- df_2_5%>%
  mutate(infant=infant_5)%>%
  select(id,infant)

df_2_6<- df_2%>%
  select(id,under5_5)

df_2_6<- melt(df_2_6,id.vars = c("id","under5_5"))

df_2_6<- df_2_6%>%
  mutate(under5=under5_5)%>%
  select(id,under5)

df_2_7<- df_2%>%
  select(id,under10_5)

df_2_7<- melt(df_2_7,id.vars = c("id","under10_5"))

df_2_7<- df_2_7%>%
  mutate(under10=under10_5)%>%
  select(id,under10)

df_2_8<- df_2%>%
  select(id,birth_yr_5_born)

df_2_8<- melt(df_2_8,id.vars = c("id","birth_yr_5_born"))

df_2_8<- df_2_8%>%
  mutate(cohort_year=birth_yr_5_born)%>%
  select(id,cohort_year)

df_2_9<- df_2%>%
  select(id,wom_age_5)

df_2_9<- melt(df_2_9,id.vars = c("id","wom_age_5"))

df_2_9<- df_2_9%>%
  mutate(mothers_age_at_birth=wom_age_5)%>%
  select(id,mothers_age_at_birth)

df_2_10<- df_2%>%
  select(id,birth_yr_4_born,birth_yr_5_born,birth_month_4_born,birth_month_5_born)

df_2_10<- melt(df_2_10,id.vars = c("id","birth_yr_4_born","birth_month_4_born","birth_yr_5_born","birth_month_5_born"))

df_2_10<- df_2_10%>%
  mutate(wait_time=((birth_yr_5_born*12+birth_month_5_born)-(birth_yr_4_born*12+birth_month_4_born)))%>%
  select(id,wait_time)


df_fifth_order<- merge(df_2_1,df_2_2[, c("id", setdiff(colnames(df_2_2),colnames(df_2_1)))], by="id")

df_fifth_order<- merge(df_fifth_order,df_2_3[, c("id", setdiff(colnames(df_2_3),colnames(df_fifth_order)))], by="id")

df_fifth_order<- merge(df_fifth_order,df_2_4[, c("id", setdiff(colnames(df_2_4),colnames(df_fifth_order)))], by="id")

df_fifth_order<- merge(df_fifth_order,df_2_5[, c("id", setdiff(colnames(df_2_5),colnames(df_fifth_order)))], by="id")

df_fifth_order<- merge(df_fifth_order,df_2_6[, c("id", setdiff(colnames(df_2_6),colnames(df_fifth_order)))], by="id")

df_fifth_order<- merge(df_fifth_order,df_2_7[, c("id", setdiff(colnames(df_2_7),colnames(df_fifth_order)))], by="id")

df_fifth_order<- merge(df_fifth_order,df_2_8[, c("id", setdiff(colnames(df_2_8),colnames(df_fifth_order)))], by="id")

df_fifth_order<- merge(df_fifth_order,df_2_9[, c("id", setdiff(colnames(df_2_9),colnames(df_fifth_order)))], by="id")

df_fifth_order<- merge(df_fifth_order,df_2_10[, c("id", setdiff(colnames(df_2_10),colnames(df_fifth_order)))], by="id")

df_fifth_order<- df_fifth_order%>%
  mutate(birth_order=3)%>%
  subset(sex==1)

rm(df_2_1,
   df_2_2,
   df_2_3,
   df_2_4,
   df_2_5,
   df_2_6,
   df_2_7,
   df_2_8,
   df_2_9,
   df_2_10)

################# Sixth order births ###########################

df_2_1<- df_2%>%
  select(id,months_6_born)

df_2_1<- melt(df_2_1,id.vars = c("id","months_6_born"))

df_2_1<- df_2_1%>%
  mutate(cohort=months_6_born)%>%
  select(id,cohort)

df_2_2<- df_2%>%
  select(id,sex_6_born)

df_2_2<- melt(df_2_2,id.vars = c("id","sex_6_born"))

df_2_2<- df_2_2%>%
  mutate(sex=sex_6_born)%>%
  select(id,sex)

df_2_3<- df_2%>%
  select(id,alive_6_born)

df_2_3<- melt(df_2_3,id.vars = c("id","alive_6_born"))

df_2_3<- df_2_3%>%
  mutate(alive=alive_6_born)%>%
  select(id,alive)

df_2_4<- df_2%>%
  select(id,neonat_6)

df_2_4<- melt(df_2_4,id.vars = c("id","neonat_6"))

df_2_4<- df_2_4%>%
  mutate(neonat=neonat_6)%>%
  select(id,neonat)

df_2_5<- df_2%>%
  select(id,infant_6)

df_2_5<- melt(df_2_5,id.vars = c("id","infant_6"))

df_2_5<- df_2_5%>%
  mutate(infant=infant_6)%>%
  select(id,infant)

df_2_6<- df_2%>%
  select(id,under5_6)

df_2_6<- melt(df_2_6,id.vars = c("id","under5_6"))

df_2_6<- df_2_6%>%
  mutate(under5=under5_6)%>%
  select(id,under5)

df_2_7<- df_2%>%
  select(id,under10_6)

df_2_7<- melt(df_2_7,id.vars = c("id","under10_6"))

df_2_7<- df_2_7%>%
  mutate(under10=under10_6)%>%
  select(id,under10)

df_2_8<- df_2%>%
  select(id,birth_yr_6_born)

df_2_8<- melt(df_2_8,id.vars = c("id","birth_yr_6_born"))

df_2_8<- df_2_8%>%
  mutate(cohort_year=birth_yr_6_born)%>%
  select(id,cohort_year)

df_2_9<- df_2%>%
  select(id,wom_age_6)

df_2_9<- melt(df_2_9,id.vars = c("id","wom_age_6"))

df_2_9<- df_2_9%>%
  mutate(mothers_age_at_birth=wom_age_6)%>%
  select(id,mothers_age_at_birth)

df_2_10<- df_2%>%
  select(id,birth_yr_5_born,birth_yr_6_born,birth_month_5_born,birth_month_6_born)

df_2_10<- melt(df_2_10,id.vars = c("id","birth_yr_5_born","birth_month_5_born","birth_yr_6_born","birth_month_6_born"))

df_2_10<- df_2_10%>%
  mutate(wait_time=((birth_yr_6_born*12+birth_month_6_born)-(birth_yr_5_born*12+birth_month_5_born)))%>%
  select(id,wait_time)

df_sixth_order<- merge(df_2_1,df_2_2[, c("id", setdiff(colnames(df_2_2),colnames(df_2_1)))], by="id")

df_sixth_order<- merge(df_sixth_order,df_2_3[, c("id", setdiff(colnames(df_2_3),colnames(df_sixth_order)))], by="id")

df_sixth_order<- merge(df_sixth_order,df_2_4[, c("id", setdiff(colnames(df_2_4),colnames(df_sixth_order)))], by="id")

df_sixth_order<- merge(df_sixth_order,df_2_5[, c("id", setdiff(colnames(df_2_5),colnames(df_sixth_order)))], by="id")

df_sixth_order<- merge(df_sixth_order,df_2_6[, c("id", setdiff(colnames(df_2_6),colnames(df_sixth_order)))], by="id")

df_sixth_order<- merge(df_sixth_order,df_2_7[, c("id", setdiff(colnames(df_2_7),colnames(df_sixth_order)))], by="id")

df_sixth_order<- merge(df_sixth_order,df_2_8[, c("id", setdiff(colnames(df_2_8),colnames(df_sixth_order)))], by="id")

df_sixth_order<- merge(df_sixth_order,df_2_9[, c("id", setdiff(colnames(df_2_9),colnames(df_sixth_order)))], by="id")

df_sixth_order<- merge(df_sixth_order,df_2_10[, c("id", setdiff(colnames(df_2_10),colnames(df_sixth_order)))], by="id")

df_sixth_order<- df_sixth_order%>%
  mutate(birth_order=3)%>%
  subset(sex==1)

rm(df_2_1,
   df_2_2,
   df_2_3,
   df_2_4,
   df_2_5,
   df_2_6,
   df_2_7,
   df_2_8,
   df_2_9,
   df_2_10)

################# Seventh order births ###########################

df_2_1<- df_2%>%
  select(id,months_7_born)

df_2_1<- melt(df_2_1,id.vars = c("id","months_7_born"))

df_2_1<- df_2_1%>%
  mutate(cohort=months_7_born)%>%
  select(id,cohort)

df_2_2<- df_2%>%
  select(id,sex_7_born)

df_2_2<- melt(df_2_2,id.vars = c("id","sex_7_born"))

df_2_2<- df_2_2%>%
  mutate(sex=sex_7_born)%>%
  select(id,sex)

df_2_3<- df_2%>%
  select(id,alive_7_born)

df_2_3<- melt(df_2_3,id.vars = c("id","alive_7_born"))

df_2_3<- df_2_3%>%
  mutate(alive=alive_7_born)%>%
  select(id,alive)

df_2_4<- df_2%>%
  select(id,neonat_7)

df_2_4<- melt(df_2_4,id.vars = c("id","neonat_7"))

df_2_4<- df_2_4%>%
  mutate(neonat=neonat_7)%>%
  select(id,neonat)

df_2_5<- df_2%>%
  select(id,infant_7)

df_2_5<- melt(df_2_5,id.vars = c("id","infant_7"))

df_2_5<- df_2_5%>%
  mutate(infant=infant_7)%>%
  select(id,infant)

df_2_6<- df_2%>%
  select(id,under5_7)

df_2_6<- melt(df_2_6,id.vars = c("id","under5_7"))

df_2_6<- df_2_6%>%
  mutate(under5=under5_7)%>%
  select(id,under5)

df_2_7<- df_2%>%
  select(id,under10_7)

df_2_7<- melt(df_2_7,id.vars = c("id","under10_7"))

df_2_7<- df_2_7%>%
  mutate(under10=under10_7)%>%
  select(id,under10)

df_2_8<- df_2%>%
  select(id,birth_yr_7_born)

df_2_8<- melt(df_2_8,id.vars = c("id","birth_yr_7_born"))

df_2_8<- df_2_8%>%
  mutate(cohort_year=birth_yr_7_born)%>%
  select(id,cohort_year)

df_2_9<- df_2%>%
  select(id,wom_age_7)

df_2_9<- melt(df_2_9,id.vars = c("id","wom_age_7"))

df_2_9<- df_2_9%>%
  mutate(mothers_age_at_birth=wom_age_7)%>%
  select(id,mothers_age_at_birth)

df_2_10<- df_2%>%
  select(id,birth_yr_6_born,birth_yr_7_born,birth_month_6_born,birth_month_7_born)

df_2_10<- melt(df_2_10,id.vars = c("id","birth_yr_6_born","birth_month_6_born","birth_yr_7_born","birth_month_7_born"))

df_2_10<- df_2_10%>%
  mutate(wait_time=((birth_yr_7_born*12+birth_month_7_born)-(birth_yr_6_born*12+birth_month_6_born)))%>%
  select(id,wait_time)

df_seventh_order<- merge(df_2_1,df_2_2[, c("id", setdiff(colnames(df_2_2),colnames(df_2_1)))], by="id")

df_seventh_order<- merge(df_seventh_order,df_2_3[, c("id", setdiff(colnames(df_2_3),colnames(df_seventh_order)))], by="id")

df_seventh_order<- merge(df_seventh_order,df_2_4[, c("id", setdiff(colnames(df_2_4),colnames(df_seventh_order)))], by="id")

df_seventh_order<- merge(df_seventh_order,df_2_5[, c("id", setdiff(colnames(df_2_5),colnames(df_seventh_order)))], by="id")

df_seventh_order<- merge(df_seventh_order,df_2_6[, c("id", setdiff(colnames(df_2_6),colnames(df_seventh_order)))], by="id")

df_seventh_order<- merge(df_seventh_order,df_2_7[, c("id", setdiff(colnames(df_2_7),colnames(df_seventh_order)))], by="id")

df_seventh_order<- merge(df_seventh_order,df_2_8[, c("id", setdiff(colnames(df_2_8),colnames(df_seventh_order)))], by="id")

df_seventh_order<- merge(df_seventh_order,df_2_9[, c("id", setdiff(colnames(df_2_9),colnames(df_seventh_order)))], by="id")

df_seventh_order<- merge(df_seventh_order,df_2_10[, c("id", setdiff(colnames(df_2_10),colnames(df_seventh_order)))], by="id")

df_seventh_order<- df_seventh_order%>%
  mutate(birth_order=3)%>%
  subset(sex==1)

rm(df_2_1,
   df_2_2,
   df_2_3,
   df_2_4,
   df_2_5,
   df_2_6,
   df_2_7,
   df_2_8,
   df_2_9,
   df_2_10)

################# Eighth order births ###########################

df_2_1<- df_2%>%
  select(id,months_8_born)

df_2_1<- melt(df_2_1,id.vars = c("id","months_8_born"))

df_2_1<- df_2_1%>%
  mutate(cohort=months_8_born)%>%
  select(id,cohort)

df_2_2<- df_2%>%
  select(id,sex_8_born)

df_2_2<- melt(df_2_2,id.vars = c("id","sex_8_born"))

df_2_2<- df_2_2%>%
  mutate(sex=sex_8_born)%>%
  select(id,sex)

df_2_3<- df_2%>%
  select(id,alive_8_born)

df_2_3<- melt(df_2_3,id.vars = c("id","alive_8_born"))

df_2_3<- df_2_3%>%
  mutate(alive=alive_8_born)%>%
  select(id,alive)

df_2_4<- df_2%>%
  select(id,neonat_8)

df_2_4<- melt(df_2_4,id.vars = c("id","neonat_8"))

df_2_4<- df_2_4%>%
  mutate(neonat=neonat_8)%>%
  select(id,neonat)

df_2_5<- df_2%>%
  select(id,infant_8)

df_2_5<- melt(df_2_5,id.vars = c("id","infant_8"))

df_2_5<- df_2_5%>%
  mutate(infant=infant_8)%>%
  select(id,infant)

df_2_6<- df_2%>%
  select(id,under5_8)

df_2_6<- melt(df_2_6,id.vars = c("id","under5_8"))

df_2_6<- df_2_6%>%
  mutate(under5=under5_8)%>%
  select(id,under5)

df_2_7<- df_2%>%
  select(id,under10_8)

df_2_7<- melt(df_2_7,id.vars = c("id","under10_8"))

df_2_7<- df_2_7%>%
  mutate(under10=under10_8)%>%
  select(id,under10)

df_2_8<- df_2%>%
  select(id,birth_yr_8_born)

df_2_8<- melt(df_2_8,id.vars = c("id","birth_yr_8_born"))

df_2_8<- df_2_8%>%
  mutate(cohort_year=birth_yr_8_born)%>%
  select(id,cohort_year)

df_2_9<- df_2%>%
  select(id,wom_age_8)

df_2_9<- melt(df_2_9,id.vars = c("id","wom_age_8"))

df_2_9<- df_2_9%>%
  mutate(mothers_age_at_birth=wom_age_8)%>%
  select(id,mothers_age_at_birth)

df_2_10<- df_2%>%
  select(id,birth_yr_7_born,birth_yr_8_born,birth_month_7_born,birth_month_8_born)

df_2_10<- melt(df_2_10,id.vars = c("id","birth_yr_7_born","birth_month_7_born","birth_yr_8_born","birth_month_8_born"))

df_2_10<- df_2_10%>%
  mutate(wait_time=((birth_yr_8_born*12+birth_month_8_born)-(birth_yr_7_born*12+birth_month_7_born)))%>%
  select(id,wait_time)

df_eighth_order<- merge(df_2_1,df_2_2[, c("id", setdiff(colnames(df_2_2),colnames(df_2_1)))], by="id")

df_eighth_order<- merge(df_eighth_order,df_2_3[, c("id", setdiff(colnames(df_2_3),colnames(df_eighth_order)))], by="id")

df_eighth_order<- merge(df_eighth_order,df_2_4[, c("id", setdiff(colnames(df_2_4),colnames(df_eighth_order)))], by="id")

df_eighth_order<- merge(df_eighth_order,df_2_5[, c("id", setdiff(colnames(df_2_5),colnames(df_eighth_order)))], by="id")

df_eighth_order<- merge(df_eighth_order,df_2_6[, c("id", setdiff(colnames(df_2_6),colnames(df_eighth_order)))], by="id")

df_eighth_order<- merge(df_eighth_order,df_2_7[, c("id", setdiff(colnames(df_2_7),colnames(df_eighth_order)))], by="id")

df_eighth_order<- merge(df_eighth_order,df_2_8[, c("id", setdiff(colnames(df_2_8),colnames(df_eighth_order)))], by="id")

df_eighth_order<- merge(df_eighth_order,df_2_9[, c("id", setdiff(colnames(df_2_9),colnames(df_eighth_order)))], by="id")

df_eighth_order<- merge(df_eighth_order,df_2_10[, c("id", setdiff(colnames(df_2_10),colnames(df_eighth_order)))], by="id")

df_eighth_order<- df_eighth_order%>%
  mutate(birth_order=3)%>%
  subset(sex==1)

rm(df_2_1,
   df_2_2,
   df_2_3,
   df_2_4,
   df_2_5,
   df_2_6,
   df_2_7,
   df_2_8,
   df_2_9,
   df_2_10)

################# Ninth order births ###########################

df_2_1<- df_2%>%
  select(id,months_9_born)

df_2_1<- melt(df_2_1,id.vars = c("id","months_9_born"))

df_2_1<- df_2_1%>%
  mutate(cohort=months_9_born)%>%
  select(id,cohort)

df_2_2<- df_2%>%
  select(id,sex_9_born)

df_2_2<- melt(df_2_2,id.vars = c("id","sex_9_born"))

df_2_2<- df_2_2%>%
  mutate(sex=sex_9_born)%>%
  select(id,sex)

df_2_3<- df_2%>%
  select(id,alive_9_born)

df_2_3<- melt(df_2_3,id.vars = c("id","alive_9_born"))

df_2_3<- df_2_3%>%
  mutate(alive=alive_9_born)%>%
  select(id,alive)

df_2_4<- df_2%>%
  select(id,neonat_9)

df_2_4<- melt(df_2_4,id.vars = c("id","neonat_9"))

df_2_4<- df_2_4%>%
  mutate(neonat=neonat_9)%>%
  select(id,neonat)

df_2_5<- df_2%>%
  select(id,infant_9)

df_2_5<- melt(df_2_5,id.vars = c("id","infant_9"))

df_2_5<- df_2_5%>%
  mutate(infant=infant_9)%>%
  select(id,infant)

df_2_6<- df_2%>%
  select(id,under5_9)

df_2_6<- melt(df_2_6,id.vars = c("id","under5_9"))

df_2_6<- df_2_6%>%
  mutate(under5=under5_9)%>%
  select(id,under5)

df_2_7<- df_2%>%
  select(id,under10_9)

df_2_7<- melt(df_2_7,id.vars = c("id","under10_9"))

df_2_7<- df_2_7%>%
  mutate(under10=under10_9)%>%
  select(id,under10)

df_2_8<- df_2%>%
  select(id,birth_yr_9_born)

df_2_8<- melt(df_2_8,id.vars = c("id","birth_yr_9_born"))

df_2_8<- df_2_8%>%
  mutate(cohort_year=birth_yr_9_born)%>%
  select(id,cohort_year)

df_2_9<- df_2%>%
  select(id,wom_age_9)

df_2_9<- melt(df_2_9,id.vars = c("id","wom_age_9"))

df_2_9<- df_2_9%>%
  mutate(mothers_age_at_birth=wom_age_9)%>%
  select(id,mothers_age_at_birth)

df_2_10<- df_2%>%
  select(id,birth_yr_8_born,birth_yr_9_born,birth_month_8_born,birth_month_9_born)

df_2_10<- melt(df_2_10,id.vars = c("id","birth_yr_8_born","birth_month_8_born","birth_yr_9_born","birth_month_9_born"))

df_2_10<- df_2_10%>%
  mutate(wait_time=((birth_yr_9_born*12+birth_month_9_born)-(birth_yr_8_born*12+birth_month_8_born)))%>%
  select(id,wait_time)


df_ninth_order<- merge(df_2_1,df_2_2[, c("id", setdiff(colnames(df_2_2),colnames(df_2_1)))], by="id")

df_ninth_order<- merge(df_ninth_order,df_2_3[, c("id", setdiff(colnames(df_2_3),colnames(df_ninth_order)))], by="id")

df_ninth_order<- merge(df_ninth_order,df_2_4[, c("id", setdiff(colnames(df_2_4),colnames(df_ninth_order)))], by="id")

df_ninth_order<- merge(df_ninth_order,df_2_5[, c("id", setdiff(colnames(df_2_5),colnames(df_ninth_order)))], by="id")

df_ninth_order<- merge(df_ninth_order,df_2_6[, c("id", setdiff(colnames(df_2_6),colnames(df_ninth_order)))], by="id")

df_ninth_order<- merge(df_ninth_order,df_2_7[, c("id", setdiff(colnames(df_2_7),colnames(df_ninth_order)))], by="id")

df_ninth_order<- merge(df_ninth_order,df_2_8[, c("id", setdiff(colnames(df_2_8),colnames(df_ninth_order)))], by="id")

df_ninth_order<- merge(df_ninth_order,df_2_9[, c("id", setdiff(colnames(df_2_9),colnames(df_ninth_order)))], by="id")

df_ninth_order<- merge(df_ninth_order,df_2_10[, c("id", setdiff(colnames(df_2_10),colnames(df_ninth_order)))], by="id")

df_ninth_order<- df_ninth_order%>%
  mutate(birth_order=3)%>%
  subset(sex==1)

rm(df_2_1,
   df_2_2,
   df_2_3,
   df_2_4,
   df_2_5,
   df_2_6,
   df_2_7,
   df_2_8,
   df_2_9,
   df_2_10)

################# Tenth order births ###########################

df_2_1<- df_2%>%
  select(id,months_10_born)

df_2_1<- melt(df_2_1,id.vars = c("id","months_10_born"))

df_2_1<- df_2_1%>%
  mutate(cohort=months_10_born)%>%
  select(id,cohort)

df_2_2<- df_2%>%
  select(id,sex_10_born)

df_2_2<- melt(df_2_2,id.vars = c("id","sex_10_born"))

df_2_2<- df_2_2%>%
  mutate(sex=sex_10_born)%>%
  select(id,sex)

df_2_3<- df_2%>%
  select(id,alive_10_born)

df_2_3<- melt(df_2_3,id.vars = c("id","alive_10_born"))

df_2_3<- df_2_3%>%
  mutate(alive=alive_10_born)%>%
  select(id,alive)

df_2_4<- df_2%>%
  select(id,neonat_10)

df_2_4<- melt(df_2_4,id.vars = c("id","neonat_10"))

df_2_4<- df_2_4%>%
  mutate(neonat=neonat_10)%>%
  select(id,neonat)

df_2_5<- df_2%>%
  select(id,infant_10)

df_2_5<- melt(df_2_5,id.vars = c("id","infant_10"))

df_2_5<- df_2_5%>%
  mutate(infant=infant_10)%>%
  select(id,infant)

df_2_6<- df_2%>%
  select(id,under5_10)

df_2_6<- melt(df_2_6,id.vars = c("id","under5_10"))

df_2_6<- df_2_6%>%
  mutate(under5=under5_10)%>%
  select(id,under5)

df_2_7<- df_2%>%
  select(id,under10_10)

df_2_7<- melt(df_2_7,id.vars = c("id","under10_10"))

df_2_7<- df_2_7%>%
  mutate(under10=under10_10)%>%
  select(id,under10)

df_2_8<- df_2%>%
  select(id,birth_yr_10_born)

df_2_8<- melt(df_2_8,id.vars = c("id","birth_yr_10_born"))

df_2_8<- df_2_8%>%
  mutate(cohort_year=birth_yr_10_born)%>%
  select(id,cohort_year)

df_2_9<- df_2%>%
  select(id,wom_age_10)

df_2_9<- melt(df_2_9,id.vars = c("id","wom_age_10"))

df_2_9<- df_2_9%>%
  mutate(mothers_age_at_birth=wom_age_10)%>%
  select(id,mothers_age_at_birth)

df_2_10<- df_2%>%
  select(id,birth_yr_9_born,birth_yr_10_born,birth_month_9_born,birth_month_10_born)

df_2_10<- melt(df_2_10,id.vars = c("id","birth_yr_9_born","birth_month_9_born","birth_yr_10_born","birth_month_10_born"))

df_2_10<- df_2_10%>%
  mutate(wait_time=((birth_yr_10_born*12+birth_month_10_born)-(birth_yr_9_born*12+birth_month_9_born)))%>%
  select(id,wait_time)

df_tenth_order<- merge(df_2_1,df_2_2[, c("id", setdiff(colnames(df_2_2),colnames(df_2_1)))], by="id")

df_tenth_order<- merge(df_tenth_order,df_2_3[, c("id", setdiff(colnames(df_2_3),colnames(df_tenth_order)))], by="id")

df_tenth_order<- merge(df_tenth_order,df_2_4[, c("id", setdiff(colnames(df_2_4),colnames(df_tenth_order)))], by="id")

df_tenth_order<- merge(df_tenth_order,df_2_5[, c("id", setdiff(colnames(df_2_5),colnames(df_tenth_order)))], by="id")

df_tenth_order<- merge(df_tenth_order,df_2_6[, c("id", setdiff(colnames(df_2_6),colnames(df_tenth_order)))], by="id")

df_tenth_order<- merge(df_tenth_order,df_2_7[, c("id", setdiff(colnames(df_2_7),colnames(df_tenth_order)))], by="id")

df_tenth_order<- merge(df_tenth_order,df_2_8[, c("id", setdiff(colnames(df_2_8),colnames(df_tenth_order)))], by="id")

df_tenth_order<- merge(df_tenth_order,df_2_9[, c("id", setdiff(colnames(df_2_9),colnames(df_tenth_order)))], by="id")

df_tenth_order<- merge(df_tenth_order,df_2_10[, c("id", setdiff(colnames(df_2_10),colnames(df_tenth_order)))], by="id")

df_tenth_order<- df_tenth_order%>%
  mutate(birth_order=3)%>%
  subset(sex==1)

rm(df_2_1,
   df_2_2,
   df_2_3,
   df_2_4,
   df_2_5,
   df_2_6,
   df_2_7,
   df_2_8,
   df_2_9,
   df_2_10)

################# Eleventh order births ###########################

df_2_1<- df_2%>%
  select(id,months_11_born)

df_2_1<- melt(df_2_1,id.vars = c("id","months_11_born"))

df_2_1<- df_2_1%>%
  mutate(cohort=months_11_born)%>%
  select(id,cohort)

df_2_2<- df_2%>%
  select(id,sex_11_born)

df_2_2<- melt(df_2_2,id.vars = c("id","sex_11_born"))

df_2_2<- df_2_2%>%
  mutate(sex=sex_11_born)%>%
  select(id,sex)

df_2_3<- df_2%>%
  select(id,alive_11_born)

df_2_3<- melt(df_2_3,id.vars = c("id","alive_11_born"))

df_2_3<- df_2_3%>%
  mutate(alive=alive_11_born)%>%
  select(id,alive)

df_2_4<- df_2%>%
  select(id,neonat_11)

df_2_4<- melt(df_2_4,id.vars = c("id","neonat_11"))

df_2_4<- df_2_4%>%
  mutate(neonat=neonat_11)%>%
  select(id,neonat)

df_2_5<- df_2%>%
  select(id,infant_11)

df_2_5<- melt(df_2_5,id.vars = c("id","infant_11"))

df_2_5<- df_2_5%>%
  mutate(infant=infant_11)%>%
  select(id,infant)

df_2_6<- df_2%>%
  select(id,under5_11)

df_2_6<- melt(df_2_6,id.vars = c("id","under5_11"))

df_2_6<- df_2_6%>%
  mutate(under5=under5_11)%>%
  select(id,under5)

df_2_7<- df_2%>%
  select(id,under10_11)

df_2_7<- melt(df_2_7,id.vars = c("id","under10_11"))

df_2_7<- df_2_7%>%
  mutate(under10=under10_11)%>%
  select(id,under10)

df_2_8<- df_2%>%
  select(id,birth_yr_11_born)

df_2_8<- melt(df_2_8,id.vars = c("id","birth_yr_11_born"))

df_2_8<- df_2_8%>%
  mutate(cohort_year=birth_yr_11_born)%>%
  select(id,cohort_year)

df_2_9<- df_2%>%
  select(id,wom_age_11)

df_2_9<- melt(df_2_9,id.vars = c("id","wom_age_11"))

df_2_9<- df_2_9%>%
  mutate(mothers_age_at_birth=wom_age_11)%>%
  select(id,mothers_age_at_birth)

df_2_10<- df_2%>%
  select(id,birth_yr_11_born,birth_yr_10_born,birth_month_11_born,birth_month_10_born)

df_2_10<- melt(df_2_10,id.vars = c("id","birth_yr_11_born","birth_month_11_born","birth_yr_10_born","birth_month_10_born"))

df_2_10<- df_2_10%>%
  mutate(wait_time=((birth_yr_11_born*12+birth_month_11_born)-(birth_yr_10_born*12+birth_month_10_born)))%>%
  select(id,wait_time)

df_eleventh_order<- merge(df_2_1,df_2_2[, c("id", setdiff(colnames(df_2_2),colnames(df_2_1)))], by="id")

df_eleventh_order<- merge(df_eleventh_order,df_2_3[, c("id", setdiff(colnames(df_2_3),colnames(df_eleventh_order)))], by="id")

df_eleventh_order<- merge(df_eleventh_order,df_2_4[, c("id", setdiff(colnames(df_2_4),colnames(df_eleventh_order)))], by="id")

df_eleventh_order<- merge(df_eleventh_order,df_2_5[, c("id", setdiff(colnames(df_2_5),colnames(df_eleventh_order)))], by="id")

df_eleventh_order<- merge(df_eleventh_order,df_2_6[, c("id", setdiff(colnames(df_2_6),colnames(df_eleventh_order)))], by="id")

df_eleventh_order<- merge(df_eleventh_order,df_2_7[, c("id", setdiff(colnames(df_2_7),colnames(df_eleventh_order)))], by="id")

df_eleventh_order<- merge(df_eleventh_order,df_2_8[, c("id", setdiff(colnames(df_2_8),colnames(df_eleventh_order)))], by="id")

df_eleventh_order<- merge(df_eleventh_order,df_2_9[, c("id", setdiff(colnames(df_2_9),colnames(df_eleventh_order)))], by="id")

df_eleventh_order<- merge(df_eleventh_order,df_2_10[, c("id", setdiff(colnames(df_2_10),colnames(df_eleventh_order)))], by="id")

df_eleventh_order<- df_eleventh_order%>%
  mutate(birth_order=3)%>%
  subset(sex==1)

rm(df_2_1,
   df_2_2,
   df_2_3,
   df_2_4,
   df_2_5,
   df_2_6,
   df_2_7,
   df_2_8,
   df_2_9,
   df_2_10)

################# Twelveth order births ###########################

df_2_1<- df_2%>%
  select(id,months_12_born)

df_2_1<- melt(df_2_1,id.vars = c("id","months_12_born"))

df_2_1<- df_2_1%>%
  mutate(cohort=months_12_born)%>%
  select(id,cohort)

df_2_2<- df_2%>%
  select(id,sex_12_born)

df_2_2<- melt(df_2_2,id.vars = c("id","sex_12_born"))

df_2_2<- df_2_2%>%
  mutate(sex=sex_12_born)%>%
  select(id,sex)

df_2_3<- df_2%>%
  select(id,alive_12_born)

df_2_3<- melt(df_2_3,id.vars = c("id","alive_12_born"))

df_2_3<- df_2_3%>%
  mutate(alive=alive_12_born)%>%
  select(id,alive)

df_2_4<- df_2%>%
  select(id,neonat_12)

df_2_4<- melt(df_2_4,id.vars = c("id","neonat_12"))

df_2_4<- df_2_4%>%
  mutate(neonat=neonat_12)%>%
  select(id,neonat)

df_2_5<- df_2%>%
  select(id,infant_12)

df_2_5<- melt(df_2_5,id.vars = c("id","infant_12"))

df_2_5<- df_2_5%>%
  mutate(infant=infant_12)%>%
  select(id,infant)

df_2_6<- df_2%>%
  select(id,under5_12)

df_2_6<- melt(df_2_6,id.vars = c("id","under5_12"))

df_2_6<- df_2_6%>%
  mutate(under5=under5_12)%>%
  select(id,under5)

df_2_7<- df_2%>%
  select(id,under10_12)

df_2_7<- melt(df_2_7,id.vars = c("id","under10_12"))

df_2_7<- df_2_7%>%
  mutate(under10=under10_12)%>%
  select(id,under10)

df_2_8<- df_2%>%
  select(id,birth_yr_12_born)

df_2_8<- melt(df_2_8,id.vars = c("id","birth_yr_12_born"))

df_2_8<- df_2_8%>%
  mutate(cohort_year=birth_yr_12_born)%>%
  select(id,cohort_year)

df_2_9<- df_2%>%
  select(id,wom_age_12)

df_2_9<- melt(df_2_9,id.vars = c("id","wom_age_12"))

df_2_9<- df_2_9%>%
  mutate(mothers_age_at_birth=wom_age_12)%>%
  select(id,mothers_age_at_birth)

df_2_10<- df_2%>%
  select(id,birth_yr_11_born,birth_yr_12_born,birth_month_11_born,birth_month_12_born)

df_2_10<- melt(df_2_10,id.vars = c("id","birth_yr_11_born","birth_month_11_born","birth_yr_12_born","birth_month_12_born"))

df_2_10<- df_2_10%>%
  mutate(wait_time=((birth_yr_12_born*12+birth_month_12_born)-(birth_yr_11_born*12+birth_month_11_born)))%>%
  select(id,wait_time)

df_twelveth_order<- merge(df_2_1,df_2_2[, c("id", setdiff(colnames(df_2_2),colnames(df_2_1)))], by="id")

df_twelveth_order<- merge(df_twelveth_order,df_2_3[, c("id", setdiff(colnames(df_2_3),colnames(df_twelveth_order)))], by="id")

df_twelveth_order<- merge(df_twelveth_order,df_2_4[, c("id", setdiff(colnames(df_2_4),colnames(df_twelveth_order)))], by="id")

df_twelveth_order<- merge(df_twelveth_order,df_2_5[, c("id", setdiff(colnames(df_2_5),colnames(df_twelveth_order)))], by="id")

df_twelveth_order<- merge(df_twelveth_order,df_2_6[, c("id", setdiff(colnames(df_2_6),colnames(df_twelveth_order)))], by="id")

df_twelveth_order<- merge(df_twelveth_order,df_2_7[, c("id", setdiff(colnames(df_2_7),colnames(df_twelveth_order)))], by="id")

df_twelveth_order<- merge(df_twelveth_order,df_2_8[, c("id", setdiff(colnames(df_2_8),colnames(df_twelveth_order)))], by="id")

df_twelveth_order<- merge(df_twelveth_order,df_2_9[, c("id", setdiff(colnames(df_2_9),colnames(df_twelveth_order)))], by="id")

df_twelveth_order<- merge(df_twelveth_order,df_2_10[, c("id", setdiff(colnames(df_2_10),colnames(df_twelveth_order)))], by="id")

df_twelveth_order<- df_twelveth_order%>%
  mutate(birth_order=3)%>%
  subset(sex==1)

rm(df_2_1,
   df_2_2,
   df_2_3,
   df_2_4,
   df_2_5,
   df_2_6,
   df_2_7,
   df_2_8,
   df_2_9,
   df_2_10)


################# Thirteenth order births ###########################

df_2_1<- df_2%>%
  select(id,months_13_born)

df_2_1<- melt(df_2_1,id.vars = c("id","months_13_born"))

df_2_1<- df_2_1%>%
  mutate(cohort=months_13_born)%>%
  select(id,cohort)

df_2_2<- df_2%>%
  select(id,sex_13_born)

df_2_2<- melt(df_2_2,id.vars = c("id","sex_13_born"))

df_2_2<- df_2_2%>%
  mutate(sex=sex_13_born)%>%
  select(id,sex)

df_2_3<- df_2%>%
  select(id,alive_13_born)

df_2_3<- melt(df_2_3,id.vars = c("id","alive_13_born"))

df_2_3<- df_2_3%>%
  mutate(alive=alive_13_born)%>%
  select(id,alive)

df_2_4<- df_2%>%
  select(id,neonat_13)

df_2_4<- melt(df_2_4,id.vars = c("id","neonat_13"))

df_2_4<- df_2_4%>%
  mutate(neonat=neonat_13)%>%
  select(id,neonat)

df_2_5<- df_2%>%
  select(id,infant_13)

df_2_5<- melt(df_2_5,id.vars = c("id","infant_13"))

df_2_5<- df_2_5%>%
  mutate(infant=infant_13)%>%
  select(id,infant)

df_2_6<- df_2%>%
  select(id,under5_13)

df_2_6<- melt(df_2_6,id.vars = c("id","under5_13"))

df_2_6<- df_2_6%>%
  mutate(under5=under5_13)%>%
  select(id,under5)

df_2_7<- df_2%>%
  select(id,under10_13)

df_2_7<- melt(df_2_7,id.vars = c("id","under10_13"))

df_2_7<- df_2_7%>%
  mutate(under10=under10_13)%>%
  select(id,under10)

df_2_8<- df_2%>%
  select(id,birth_yr_13_born)

df_2_8<- melt(df_2_8,id.vars = c("id","birth_yr_13_born"))

df_2_8<- df_2_8%>%
  mutate(cohort_year=birth_yr_13_born)%>%
  select(id,cohort_year)

df_2_9<- df_2%>%
  select(id,wom_age_13)

df_2_9<- melt(df_2_9,id.vars = c("id","wom_age_13"))

df_2_9<- df_2_9%>%
  mutate(mothers_age_at_birth=wom_age_13)%>%
  select(id,mothers_age_at_birth)

df_2_10<- df_2%>%
  select(id,birth_yr_13_born,birth_yr_12_born,birth_month_13_born,birth_month_12_born)

df_2_10<- melt(df_2_10,id.vars = c("id","birth_yr_13_born","birth_month_13_born","birth_yr_12_born","birth_month_12_born"))

df_2_10<- df_2_10%>%
  mutate(wait_time=((birth_yr_13_born*12+birth_month_13_born)-(birth_yr_12_born*12+birth_month_12_born)))%>%
  select(id,wait_time)


df_thirteenth_order<- merge(df_2_1,df_2_2[, c("id", setdiff(colnames(df_2_2),colnames(df_2_1)))], by="id")

df_thirteenth_order<- merge(df_thirteenth_order,df_2_3[, c("id", setdiff(colnames(df_2_3),colnames(df_thirteenth_order)))], by="id")

df_thirteenth_order<- merge(df_thirteenth_order,df_2_4[, c("id", setdiff(colnames(df_2_4),colnames(df_thirteenth_order)))], by="id")

df_thirteenth_order<- merge(df_thirteenth_order,df_2_5[, c("id", setdiff(colnames(df_2_5),colnames(df_thirteenth_order)))], by="id")

df_thirteenth_order<- merge(df_thirteenth_order,df_2_6[, c("id", setdiff(colnames(df_2_6),colnames(df_thirteenth_order)))], by="id")

df_thirteenth_order<- merge(df_thirteenth_order,df_2_7[, c("id", setdiff(colnames(df_2_7),colnames(df_thirteenth_order)))], by="id")

df_thirteenth_order<- merge(df_thirteenth_order,df_2_8[, c("id", setdiff(colnames(df_2_8),colnames(df_thirteenth_order)))], by="id")

df_thirteenth_order<- merge(df_thirteenth_order,df_2_9[, c("id", setdiff(colnames(df_2_9),colnames(df_thirteenth_order)))], by="id")

df_thirteenth_order<- merge(df_thirteenth_order,df_2_10[, c("id", setdiff(colnames(df_2_10),colnames(df_thirteenth_order)))], by="id")

df_thirteenth_order<- df_thirteenth_order%>%
  mutate(birth_order=3)%>%
  subset(sex==1)

rm(df_2_1,
   df_2_2,
   df_2_3,
   df_2_4,
   df_2_5,
   df_2_6,
   df_2_7,
   df_2_8,
   df_2_9,
   df_2_10)

################# Fourteenth order births ###########################

df_2_1<- df_2%>%
  select(id,months_14_born)

df_2_1<- melt(df_2_1,id.vars = c("id","months_14_born"))

df_2_1<- df_2_1%>%
  mutate(cohort=months_14_born)%>%
  select(id,cohort)

df_2_2<- df_2%>%
  select(id,sex_14_born)

df_2_2<- melt(df_2_2,id.vars = c("id","sex_14_born"))

df_2_2<- df_2_2%>%
  mutate(sex=sex_14_born)%>%
  select(id,sex)

df_2_3<- df_2%>%
  select(id,alive_14_born)

df_2_3<- melt(df_2_3,id.vars = c("id","alive_14_born"))

df_2_3<- df_2_3%>%
  mutate(alive=alive_14_born)%>%
  select(id,alive)

df_2_4<- df_2%>%
  select(id,neonat_14)

df_2_4<- melt(df_2_4,id.vars = c("id","neonat_14"))

df_2_4<- df_2_4%>%
  mutate(neonat=neonat_14)%>%
  select(id,neonat)

df_2_5<- df_2%>%
  select(id,infant_14)

df_2_5<- melt(df_2_5,id.vars = c("id","infant_14"))

df_2_5<- df_2_5%>%
  mutate(infant=infant_14)%>%
  select(id,infant)

df_2_6<- df_2%>%
  select(id,under5_14)

df_2_6<- melt(df_2_6,id.vars = c("id","under5_14"))

df_2_6<- df_2_6%>%
  mutate(under5=under5_14)%>%
  select(id,under5)

df_2_7<- df_2%>%
  select(id,under10_14)

df_2_7<- melt(df_2_7,id.vars = c("id","under10_14"))

df_2_7<- df_2_7%>%
  mutate(under10=under10_14)%>%
  select(id,under10)

df_2_8<- df_2%>%
  select(id,birth_yr_14_born)

df_2_8<- melt(df_2_8,id.vars = c("id","birth_yr_14_born"))

df_2_8<- df_2_8%>%
  mutate(cohort_year=birth_yr_14_born)%>%
  select(id,cohort_year)

df_2_9<- df_2%>%
  select(id,wom_age_14)

df_2_9<- melt(df_2_9,id.vars = c("id","wom_age_14"))

df_2_9<- df_2_9%>%
  mutate(mothers_age_at_birth=wom_age_14)%>%
  select(id,mothers_age_at_birth)

df_2_10<- df_2%>%
  select(id,birth_yr_13_born,birth_yr_14_born,birth_month_13_born,birth_month_14_born)

df_2_10<- melt(df_2_10,id.vars = c("id","birth_yr_13_born","birth_month_13_born","birth_yr_14_born","birth_month_14_born"))

df_2_10<- df_2_10%>%
  mutate(wait_time=((birth_yr_14_born*12+birth_month_14_born)-(birth_yr_13_born*12+birth_month_13_born)))%>%
  select(id,wait_time)

df_fourteenth_order<- merge(df_2_1,df_2_2[, c("id", setdiff(colnames(df_2_2),colnames(df_2_1)))], by="id")

df_fourteenth_order<- merge(df_fourteenth_order,df_2_3[, c("id", setdiff(colnames(df_2_3),colnames(df_fourteenth_order)))], by="id")

df_fourteenth_order<- merge(df_fourteenth_order,df_2_4[, c("id", setdiff(colnames(df_2_4),colnames(df_fourteenth_order)))], by="id")

df_fourteenth_order<- merge(df_fourteenth_order,df_2_5[, c("id", setdiff(colnames(df_2_5),colnames(df_fourteenth_order)))], by="id")

df_fourteenth_order<- merge(df_fourteenth_order,df_2_6[, c("id", setdiff(colnames(df_2_6),colnames(df_fourteenth_order)))], by="id")

df_fourteenth_order<- merge(df_fourteenth_order,df_2_7[, c("id", setdiff(colnames(df_2_7),colnames(df_fourteenth_order)))], by="id")

df_fourteenth_order<- merge(df_fourteenth_order,df_2_8[, c("id", setdiff(colnames(df_2_8),colnames(df_fourteenth_order)))], by="id")

df_fourteenth_order<- merge(df_fourteenth_order,df_2_9[, c("id", setdiff(colnames(df_2_9),colnames(df_fourteenth_order)))], by="id")

df_fourteenth_order<- merge(df_fourteenth_order,df_2_10[, c("id", setdiff(colnames(df_2_10),colnames(df_fourteenth_order)))], by="id")

df_fourteenth_order<- df_fourteenth_order%>%
  mutate(birth_order=3)%>%
  subset(sex==1)

rm(df_2_1,
   df_2_2,
   df_2_3,
   df_2_4,
   df_2_5,
   df_2_6,
   df_2_7,
   df_2_8,
   df_2_9,
   df_2_10)

df_panel<- rbind(df_first_order,
                 df_second_order,
                 df_third_order,
                 df_fourth_order,
                 df_fifth_order,
                 df_sixth_order,
                 df_seventh_order,
                 df_eighth_order,
                 df_ninth_order,
                 df_tenth_order,
                 df_eleventh_order,
                 df_twelveth_order,
                 df_thirteenth_order,
                 df_fourteenth_order)

df_panel<- df_panel[order(df_panel$id,df_panel$cohort),]

df_panel<- df_panel%>%
  subset(cohort_year>=1987 & cohort_year<=2004)

df_controls<- main_data%>%
  select(id,
         state_name,
         district_name,
         first_election,
         election,
         rural_urban_status,
         read_write_woman,
         caste,
         religion,
         type_of_house,
         total_child,
         birth_1_boy,
         first_born_alive,
         first_born_boy,
         first_two_boys,
         first_two_girls,
         second_born_girl,
         third_born_girl,
         first_boy_2nd_girl,
         first_girl_2nd_boy)

df_panel<- merge(df_panel,df_controls[, c("id", setdiff(colnames(df_controls),colnames(df_panel)))], by="id")

df_panel<- df_panel%>%
  mutate(months_diff=cohort-election,
         treat=ifelse(months_diff>0,1,0))%>%
  subset(!is.na(alive))%>%
  subset(rural_urban_status=="rural")%>%
  mutate(state_id=as.numeric(as.factor(state_name))) 

################ Final data for analysis ###################

write.csv(df_panel,"Preferred_sample_data_for_analysis_only_girls.csv")