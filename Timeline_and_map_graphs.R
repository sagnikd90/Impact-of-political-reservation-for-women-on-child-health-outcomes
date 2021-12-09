library(dplyr)
library(ggplot2)
library(ggalt)
library(hrbrthemes)
library(zoo)
library(data.table)
library(rgdal)
library(sf)


setwd("C:\\Users\\das90\\OneDrive - Cuny GradCenter\\Research\\Political reservation and age of marriage\\Github\\Political-reservation-and-age-of-marriage-age-of-first-birth\\Final version\\R results\\R_graphs")

df<- read.csv("C:\\Users\\das90\\OneDrive - Cuny GradCenter\\Research\\Political reservation and age of marriage\\Github\\Political-reservation-and-age-of-marriage-age-of-first-birth\\Final version\\Cleaned datasets\\Full_sample_data_for_analysis.csv")

df_1<-read_sf("C:\\Users\\das90\\OneDrive - Cuny GradCenter\\Research\\Paper_with_Shilpi\\Political Competetion\\Datasets\\Shape File Constituency\\maps-master\\maps-master\\assembly-constituencies\\India_AC.shp")

df<- df%>%
  mutate(election_date=paste(first_election,first_election_month,"01",sep="-"))

df$election_date<- as.Date(df$election_date,format = "%Y-%m-%d")

df_time_graph<- df%>%
  group_by(state_name,election_date)%>%
  summarise(alive=mean(alive,na.rm = TRUE))%>%
  select(state_name,election_date)

png("election_timelime.png")

df_time_graph%>%
  ggplot(aes(x=factor(election_date),y=factor(state_name)))+
  geom_lollipop(aes(color=factor(state_name)),show.legend = FALSE,size=1)+
  theme_bw()+
  theme(axis.text.x = element_text(angle=45, hjust=1),
        axis.text.y = element_blank())+
  ylab("")+
  xlab("Election years")+
  geom_text(aes(y=state_name,label=factor(state_name)),vjust=0.2,size=4)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

dev.off()


#################################################################################

df_1$ST_NAME<- tolower(df_1$ST_NAME)
df_1$DIST_NAME<- tolower(df_1$DIST_NAME)

df_1$area<- st_area(df_1)

df_1<- df_1 %>% 
  group_by(ST_NAME) %>% 
  summarise(area=sum(area))

df_1<- df_1%>%
  mutate(law_abide=ifelse(ST_NAME=="andhra pradesh"|
                            ST_NAME=="gujarat"|
                            ST_NAME=="karnataka"|
                            ST_NAME=="kerala"|
                            ST_NAME=="madhya pradesh"|
                            ST_NAME=="rajasthan"|
                            ST_NAME=="tripura"|
                            ST_NAME=="uttar pradesh"|
                            ST_NAME=="west bengal",1,0))


df_1$law_abide[df_1$ST_NAME=="arunachal pradesh"|
                 df_1$ST_NAME=="delhi"|
                 df_1$ST_NAME=="himachal pradesh"|
                 df_1$ST_NAME=="jammu & kashmir"|
                 df_1$ST_NAME=="meghalaya"|
                 df_1$ST_NAME=="mizoram"|
                 df_1$ST_NAME=="nagaland"|
                 df_1$ST_NAME=="puducherry"|
                 df_1$ST_NAME=="sikkim"]<- 9

df_1$law_abide[df_1$ST_NAME=="chhattisgarh"|
                 df_1$ST_NAME=="jharkhand"|
                 df_1$ST_NAME=="uttarkhand"]<- 99

df_1$ST_NAME[df_1$ST_NAME=="uttarkhand"]<- "uttarakhand"


png("law_abiding_states.png")

df_1%>%
  ggplot()+
  geom_sf(aes(fill=factor(law_abide)))+
  theme_ipsum()+
  scale_fill_discrete(name="Sample status",
                      labels=c("Elections after 1995","Elections by 1995","Not in sample","States formed after 2001"))

dev.off()

df_2<- read.csv("C:\\Users\\das90\\OneDrive - Cuny GradCenter\\Research\\Political reservation and age of marriage\\Github\\Political-reservation-and-age-of-marriage-age-of-first-birth\\Final version\\Cleaned datasets\\Census_data.csv")

df_2<-df_2%>%
  subset(year==2001)%>%
  group_by(state_name)%>%
  summarise(pop=sum(tot_pop,na.rm = TRUE))%>%
  mutate(tot_pop=sum(pop))%>%
  mutate(percent_pop=round((pop/tot_pop)*100,digits = 3))

colnames(df_1)[which(names(df_1) == "ST_NAME")] <- "state_name"


df_1<- merge(df_1,df_2[, 
                                                            c("state_name", setdiff(colnames(df_2),colnames(df_1)))], by="state_name",all=TRUE)


png("statewise_population.png")

df_1%>%
  ggplot()+
  geom_sf(aes(fill=percent_pop))+
  theme_ipsum()+
  scale_fill_viridis_c(option = "viridis",
                       name="Percentage population")

dev.off()