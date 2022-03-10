library(readr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(forcats)
library(scales)

network_data_2021<- read.csv("aux_playset.csv")
useful_data<-network_data_2021%>%
  pivot_longer(so_part_util_012021:so_part_util_152021, names_to="names_util", values_to="values_util")
useful_data

useful_data$names_util<-as.factor(useful_data$names_util)
useful_data$names_util<-
  fct_recode(useful_data$names_util, 
         "Participating in network-wide meetings" = "so_part_util_012021", 
         "Participating in local improvement team meetings" = "so_part_util_022021",
         "Collaborating with other network members through web-based tools (e.g., Slack, GoogleDrive, Discussion Boards)" = "so_part_util_152021"
  )
useful_data$names_util

useful_data$values_util<-factor(useful_data$values_util, 
          levels=c(1,2,3,4,5,9),
          labels=c("Not at all useful", "A little bit useful", "Moderately useful", 
                   "Useful", "Very useful", 
                   "N/A or I haven't done this activity in the network this school year"))
useful_data$values_util<-relevel(useful_data$values_util, "N/A or I haven't done this activity in the network this school year")


useful_data_nona<-useful_data%>%
  drop_na(values_util)

useful_data_nona$values_util

tab_useful<-useful_data_nona%>%
  group_by(names_util, values_util)%>%
  summarize(Freq=n())%>%
  mutate(Prop=Freq/sum(Freq))%>%
  mutate(Pct=(percent(Prop, accuracy=1)))%>%
  arrange(desc(Pct))%>%
  ungroup()
tab_useful
useful_plot<-ggplot(tab_useful, aes(x=fct_reorder(names_util, Prop), y=Prop, fill=values_util, position="stacked"))+
  geom_col()+
  coord_flip()+
  geom_text(aes(label=ifelse(Prop > 0.05, Pct, "")), size=4, color="black", fontface="bold", 
            position = position_stack(vjust=.5))+
  scale_x_discrete(labels = scales::wrap_format(25))+
  scale_y_continuous(labels= scales::percent)+
  labs (
    title="Please indicate how useful each activity is to you.",
    y="Percent of respondents",
    x="Network activities")+
  scale_fill_brewer(breaks=c("Very useful", "Useful", "Moderately useful", "A little bit useful",
                             "Not at all useful", "N/A or I haven't done this activity in the network this school year"))+
  theme_classic()+
  theme(legend.position="bottom",
        legend.background=element_rect(fill="gray100", color="black"),
        legend.title=element_blank())

useful_plot+guides(fill=guide_legend(nrow=1))+
  theme(
    plot.title = element_text(family="sans", size=10, face="bold.italic"),
    axis.title.y = element_text(family="sans", size=10, color="black"),
    axis.title.x = element_text(family="sans", size=10, color="black"),
    axis.text.y=element_text(family="sans", size=8, color="black"),
    axis.text.x=element_text(family="sans", size=8, color="black")
  )
  