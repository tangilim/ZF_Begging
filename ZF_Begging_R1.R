---
title: "Begging costs rather than food received cause brood size effect on growth in zebra finches-R1"
author: "Marianthi Tangili"
date: "02/07/20255"
---
  
#load necessary packages
library(ggplot2)
library(dplyr)
library(lme4)
library(readxl)
library(gridExtra)
library(car)
library(tidyr)
library(ggpubr)
library(janitor)
library(lmerTest)
library(viridis)
library(ggbeeswarm)
library(ggbreak)
library(cowplot)


#load graph theme
graph_theme <- theme_update(
  panel.grid.major = element_line(colour = NA),
  panel.grid.minor = element_line(colour = NA),
  panel.background = element_rect(colour = NA, fill = NA, size = 0.5),
  axis.title.x = element_text(size = 16, hjust = 0.5, vjust = 0.5, angle = 0, family = "sans"),
  axis.title.y = element_text(size = 16, hjust = 0.5, vjust = 0.5, angle = 90, family = "sans"),
  axis.text.x = element_text(colour = "black", angle = 0, size = 14, family = "sans"),
  axis.text.y = element_text(colour = "black", angle = 0, size = 14, family = "sans"),
  axis.ticks = element_line(colour = "black", size = 0.5),
  axis.line.x = element_line(size = 0.5),
  axis.line.y = element_line(size = 0.5),
  legend.text = element_text(size=14,family = "sans"),
  legend.title = element_text(size=14,family = "sans"),
)

setwd("SOMEPATH")

#import data set 
#database with proportion data for each behavior per chick and parent from 2012,2022,2023
zf <- read_excel("ZF_Begging_Database_Final_R1.xlsx", 
                 sheet="Behavior")


zf[zf == "NA"] <- NA
zf[zf == "-"] <- NA

#number of unique broods
zf %>%
  summarise(n_unique_broods = n_distinct(broodid))
#34 broods  

zf %>%
  group_by(nest) %>%
  summarise(n_unique_broods = n_distinct(broodid))
#21 small broods, 14 large broods

##CHICKS##

#unite left chick and right chick
chicks <- zf %>%
  filter(subject %in% c("left chick", "right chick", NA)) %>%
  group_by(age, broodid, nest, behavior, proportion)


#add age category to chick dataframe
chicks$category <- cut(chicks$age, 
                       breaks = c(0, 4, 9, 15, Inf), 
                       labels = c("0-4", "5-9", "10-15", "16+"))

#number of observations per brood size
chicks %>%
  group_by(nest) %>%
  summarize(n_obs = n())

parents%>%
  group_by(nest) %>%
  summarize(n_obs = n())


# Figures -----------------------------------------------------------------

# Fig.1- Mass Chicks ------------------------------------------------------
#MASS CHICKS
#insert data frame for mass
w <- read_excel("ZF_Begging_Database_Final_R1.xlsx", 
                sheet="Mass")

#insert data for average day at cross-fostering
a <- read_excel("ZF_Begging_Database_Final_R1.xlsx", 
                sheet = "AvgAgeCF")

mean(a$AvgAgeCF)
sd(a$AvgAgeCF)

#calculcate weight gain 5 days to 15 days
w$weight_gain<- w$mass15-w$mass5

#add age at CF info to chick mass data frame
weight_c<- merge(w, a, by =c("broodid", "broodid_2"), all.x=F)

#check difference in mass at age 15
weight15<- weight_c[,c(5,8)]

weight15$Nest<- as.factor(weight15$Nest)

weight_c$Days<- 15-weight_c$AvgAgeCF


#add day information to weight (day 5)
df_weight5 <- weight_c %>%
  mutate(Weight = mass5, Day = 5) 

#add day information to weight (day 15)
df_weight15 <- weight_c %>%
  mutate(Weight = mass15, Day = 15)

# Combine the two data frames
merged_df <- rbind(df_weight5, df_weight15)

merged_df$Day<-as.factor(merged_df$Day)
merged_df$Nest<- as.factor(merged_df$Nest)
weight_c<- merged_df

#add age at CF info to chick weight data frame
weight_c<- weight_c %>%
  select(-AvgAgeCF) %>%  # remove existing AvgAgeCF if present
  left_join(a %>% select(broodid, AvgAgeCF), by = "broodid")

weight_c15<- subset(weight_c, weight_c$Day=="15")

#add day information to weight (day 5)
df_weight5 <- weight_c %>%
  mutate(Weight = mass5, Day = 5) 

#add day information to weight (day 15)
df_weight15 <- weight_c %>%
  mutate(Weight = mass15, Day = 15)

# Combine the two data frames
merged_df <- rbind(df_weight5, df_weight15)

merged_df$Day<-as.factor(merged_df$Day)
merged_df$nest<- as.factor(merged_df$Nest)
weight_c<- merged_df


#find mass gain/day
#calculate days between first mass and day 15 mass
weight_c$Days<- 15-weight_c$AvgAgeCF
#calculate mass gain/day
weight_c$weight_gain_day<- weight_c$weight_gain/weight_c$Days
weight_c_n<- weight_c %>% filter(!weight_c$BirdId=="00680000")

weight_c_n$Nest<- as.factor(weight_c_n$Nest)

tabyl(weight_c_n$Nest)

#keep one observation per BirdID
#there is only one value weight_gain_day per BirdID
weight_c_d <- weight_c%>% 
  distinct(BirdId, .keep_all = TRUE)

weight_c_d$Nest<- as.factor(weight_c_d$Nest)


mg_plot<- ggplot(weight_c_d, aes(x = Nest, y =weight_gain_day, fill=Nest))+
  geom_violin(alpha=0.5, position = position_dodge(width = .75),size=1,color=NA) +
  geom_boxplot(notch = F,  outlier.size = -1, color="black",lwd=1, alpha = 0.7,show.legend = F)+
  ggbeeswarm::geom_quasirandom(shape = 21,size=2, dodge.width = .75, color="black",alpha=.5,show.legend = F)+
  scale_fill_manual(values=c("#999999", "#E69F00")) +
  theme(
    axis.line = element_line(colour = "black",size=1),
    axis.ticks = element_line(size=1,color="black"),
    axis.text = element_text(color="black"),
    axis.ticks.length=unit(0.2,"cm"),
    legend.position = c(1.2, 0.5),
    legend.key.size =unit(0.8, "cm"))+
  labs(x = "Brood size",
       y = "Mass gain (gr) per day ",
       fill = "Brood size")+
  scale_y_continuous(limits = c(0.35, 1.3), expand = c(0, 0),breaks = seq(0.4,1.2, by = 0.2))+
  theme(aspect.ratio = 1.4, panel.border = element_rect(colour = "black", fill=NA, size=0.75),legend.title=element_text(size=14), 
        legend.text=element_text(size=14))+
  guides(fill = guide_legend(override.aes = list(alpha = 0.7,color="black")))+
  annotate(geom="text", x=0.93, y=1.22, label="N= 46", size=6) + 
  annotate(geom="text", x=2, y=1.22, label="44", size=6)+
  stat_summary(fun = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..), color = "black", linetype = "dashed", size = 0.6)+
  scale_fill_manual(values=c("#999999", "#E69F00"), labels = c("small", "large"))

weight_c_d %>%
  group_by(Nest) %>%
  summarise(non_na_count = sum(!is.na(weight_gain_day)))

weight_c_d %>%
  group_by(nest) %>%
  summarise(n_unique_broods = n_distinct(broodid))

mass15<- weight_c %>%
  group_by(Nest) %>%
  summarise(mean = mean(mass15))

#remove duplicated values
weight_c15 <- weight_c15 %>% distinct(BirdId, .keep_all = TRUE)

w15_plot<- ggplot(weight_c15, aes(x = Nest, y=Weight, fill=Nest))+
  geom_violin(alpha=0.5, position = position_dodge(width = .75),size=1,color=NA) +
  geom_boxplot(notch = F,  outlier.size = -1, color="black",lwd=1, alpha = 0.7,show.legend = F)+
  ggbeeswarm::geom_quasirandom(shape = 21,size=2, dodge.width = .75, color="black",alpha=.5,show.legend = F)+
  scale_fill_manual(values=c("#999999", "#E69F00")) +
  theme(
    axis.line = element_line(colour = "black",size=1),
    axis.ticks = element_line(size=1,color="black"),
    axis.text = element_text(color="black"),
    axis.ticks.length=unit(0.2,"cm"),
    legend.position = c(1.2, 0.5),
    legend.key.size =unit(0.8, "cm"))+
  labs(x = "Brood size",
       y = "Mass (gr) on day 15 ",
       fill = "Brood size")+
  scale_y_continuous(limits=c(6,17.5),expand = c(0, 0), breaks = seq(8,17, by = 2))+
  theme(aspect.ratio = 1.4, panel.border = element_rect(colour = "black", fill=NA, size=0.75),legend.title=element_text(size=14), 
        legend.text=element_text(size=14))+
  guides(fill = guide_legend(override.aes = list(alpha = 0.7,color="black")))+
  annotate(geom="text", x=0.93, y=16.5, label="N= 46", size=6) + 
  annotate(geom="text", x=2, y=16.5, label="44", size=6)+
  stat_summary(fun = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..), color = "black", linetype = "dashed", size = 0.6)+
  scale_fill_manual(values=c("#999999", "#E69F00"), labels = c("small", "large"))
w15_plot

weight_c15 %>%
  group_by(Nest) %>%
  summarise(non_na_count = sum(!is.na(Weight)))

tiff("Fig1_R1.tiff", units="in", width=9, height=6, res=300)
ggarrange(mg_plot, w15_plot, nrow=1, ncol=2, align="h", labels = c("(a)", "(b)"),common.legend = T, legend = "right",label.x = c(0.05,0.05),label.y= c(0.98,0.98), widths=c(1,0.99))
dev.off()




# Fig.S1 ------------------------------------------------------------------
tiff("FigS1_R1.tiff", units="in", width=6, height=6, res=300)
ggplot(chicks)+
  geom_histogram(aes(x=age), bins = 10, col="black", fill="lightblue")+
  scale_y_continuous(expand=c(0,0), limits=c(0,270))+
  scale_x_continuous(expand=c(0,0))+
  theme(aspect.ratio = 1, 
        panel.border = element_rect(colour = "black", fill = NA, size = 0.75),
        legend.title=element_text(size=14),
        legend.text=element_text(size=14))+
  ylab("Count of observations")+
  xlab("Age of chicks")
dev.off()

sum(!is.na(chicks$age))
#1077 observations

#Fig.2- 100% stacked bar- Chick Behavior ----------------------------------------
chicks$nest<- as.factor(chicks$nest)

chicks_normalized <- chicks %>%
  group_by(nest, category) %>%
  mutate(normalized_proportion =proportion / sum(proportion))

df$position <- factor(df$position, levels=c('F', 'G', 'C'))
chicks_normalized$behavior<- factor(chicks_normalized$behavior, levels=c("resting", "other",  "moving" ,"feeding", "cleaning", "begging"))

#make the graph
tiff("Fig2.tiff", units="in", width=7, height=8, res=300)
ggplot(data = chicks_normalized, aes(x = nest, y = normalized_proportion^0.5, fill = behavior))+
  geom_bar(position = "fill", stat = "identity") +
  labs(fill = "Behavior",
       x = "Brood size",
       y = expression(sqrt("proportion of time spent on each behavior"))) +
  scale_fill_viridis(discrete = TRUE,option="cividis" ) +
  theme(legend.position = "right") +
  scale_y_continuous(
    expand = c(0, 0)) +
  theme(aspect.ratio = 1, panel.border = element_rect(colour = "black", fill = NA, size = 0.75),legend.title=element_text(size=14),legend.text=element_text(size=14))+
  scale_x_discrete(labels = c("2" = "small", "6" = "large")) 
dev.off()

# Fig.S2 ------------------------------------------------------------------
tiff("FigS2_R1.tiff", units="in", width=8, height=6, res=300)
ggplot(data = chicks_normalized, aes(x = category, y = normalized_proportion^0.5, fill = behavior)) +
  geom_bar(position = "fill", stat = "identity") +
  labs(fill = "Behavior",
       x = "Age category (days)",
       y = expression(sqrt("proportion of time spent on each behavior"))) +
  scale_fill_viridis(discrete = TRUE,option="cividis") +
  theme(legend.position = "right") +
  scale_y_continuous(
    expand = c(0, 0)) +
  theme(aspect.ratio = 1, panel.border = element_rect(colour = "black", fill = NA, size = 0.75)) +
  facet_wrap(~nest)+
  theme(strip.text.x = element_text(
    size = 12, face = "bold"), 
    strip.background = element_rect(
      color="black", fill="white", size=0.75, linetype="solid"
    ),legend.title=element_text(size=14),legend.text=element_text(size=14))+
  facet_wrap(~nest, labeller = labeller(nest = c("2" = "small broods", "6" = "large broods")))
dev.off()




# Fig.3- Begging ----------------------------------------------------------
##BEGGING##
#make dataframe for begging chicks
begging <- subset(chicks, behavior == "begging")
#make nest a factor
begging$nest<- as.factor(begging$nest)
begging$proportion<- as.numeric(begging$proportion)


##BEGGING VIOLINS CHICKS

tiff("Fig3_R1.tiff", units="in", width=10, height=6, res=300)
ggplot(begging, aes(x = category, y=proportion^0.5, fill=nest))+
  geom_violin(alpha=0.5, position = position_dodge(width = .75),size=1,color=NA) +
  geom_boxplot(notch = TRUE,  outlier.size = -1, color="black",lwd=0.5, alpha = 0.7,show.legend = F)+
  ggbeeswarm::geom_quasirandom(shape = 21,size=2, dodge.width = .75, color="black",alpha=.5,show.legend = F)+
  scale_fill_manual(values=c("#999999", "#E69F00"), labels = c("small", "large")) +
  theme(
    axis.line = element_line(colour = "black",size=1),
    legend.position = c(1.2, 0.5),
    legend.key.size =unit(0.8, "cm"))+
  labs(x = "Age (days)",
       y = expression(sqrt("proportion of time chicks spent begging")),
       fill = "Brood size")+
  scale_y_continuous(limits = c(0, 0.9), expand = c(0, 0))+
  theme(aspect.ratio = 1, panel.border = element_rect(colour = "black", fill=NA, size=0.75),legend.title=element_text(size=14), 
        legend.text=element_text(size=12))+
  guides(fill = guide_legend(override.aes = list(alpha = 0.7,color="black")))+
  annotate(geom="text", x=0.66, y=0.75, label="N=6", size=5) + 
  annotate(geom="text", x=1.12, y=0.75, label="22", size=5)+ 
  annotate(geom="text", x=1.8, y=0.75, label="32", size=5)+
  annotate(geom="text", x=2.2, y=0.75, label="32", size=5)+
  annotate(geom="text", x=2.8, y=0.75, label="26", size=5)+
  annotate(geom="text", x=3.2, y=0.75, label="49", size=5)+
  annotate(geom="text", x=3.8, y=0.75, label="16", size=5)+
  annotate(geom="text", x=4.2, y=0.75, label="12", size=5)
dev.off()

begging_obs<- begging %>%
  group_by(nest, category) %>%
  summarize(non_na_obs = sum(!is.na(proportion)))

sum(begging_obs$non_na_obs)

begging %>%
  group_by(nest) %>%
  summarise(n_unique_broods = n_distinct(broodid))

# Fig.4- Feeding Parents --------------------------------------------------
##FEEDING PARENTS##
#subset only feeding data from chicks in data from 2012 and 2022
#subset ONLY feeding behavior for chicks
feeding_c <- subset(chicks, behavior == "feeding")
#change variables to correct factor
feeding_c$proportion<- as.numeric(feeding_c$proportion)
feeding_c$nest<- as.factor(feeding_c$nest)

#subset only feeding data from chicks in data from 2012 and 2022
#feeding_mw<- subset(feeding_c, feeding_c$observer=="M" | feeding_c$observer=="W")

feeding_c <- feeding_c %>%
  left_join(a, by = c("broodid",  "broodid_2"))

#add column for number of chicks alive at time of observation
feeding_c <- feeding_c %>%
  mutate(alive = case_when(
    abs(age - 5) < abs(age - 15) ~ alive_day5,
    abs(age - 15) <= abs(age - 5) ~ alive_day15,
    TRUE ~ NA_real_
  ))

#calculate modified feeding proportion (sum proportion * alive / 2)
modified_feeding_c <- feeding_c %>%
  group_by(broodid, observer, age, nest, category) %>%
  summarise(modified_prop = sum(proportion) * unique(alive) / 2, .groups = "drop")

hist(modified_feeding_c$modified_prop)

colnames(modified_feeding_c)

modified_feeding_c$broodid<- as.factor(modified_feeding_c$broodid)

tabyl(modified_feeding_c, nest,category)
tabyl(modified_feeding_c$nest)

tiff("Fig4_R1.tiff", units="in", width=10, height=6, res=300)
ggplot(modified_feeding_c, aes(x = category, y = modified_prop^0.5, fill=nest))+
  geom_violin(alpha=0.5, position = position_dodge(width = .75),size=1,color=NA) +
  geom_boxplot(notch = T,  outlier.size = -1, color="black",lwd=0.5, alpha = 0.7,show.legend = F)+
  ggbeeswarm::geom_quasirandom(shape = 21,size=2, dodge.width = .75, color="black",alpha=.5,show.legend = F)+
  scale_fill_manual(values=c("#999999", "#E69F00"), labels = c("small", "large")) +
  theme(
    legend.position = c(1.1, 0.5),
    legend.key.size =unit(0.8, "cm"))+
  labs(x = "Age (days)",
       y = expression(sqrt("proportion of time each parent spent feeding chicks")),
       fill = "Brood size")+
  scale_y_continuous(limits = c(0, 0.8), expand = c(0, 0))+
  theme(aspect.ratio = 1, panel.border = element_rect(colour = "black", fill=NA, size=0.75),legend.title=element_text(size=14), 
        legend.text=element_text(size=14))+
  guides(fill = guide_legend(override.aes = list(alpha = 0.7,color="black")))+
  annotate(geom="text", x=0.7, 0.7, label="N=3", size=5) + 
  annotate(geom="text", x=1.1, 0.7, label="4", size=5) + 
  annotate(geom="text", x=1.8, 0.7, label="16", size=5)+
  annotate(geom="text", x=2.2, 0.7, label="9", size=5)+
  annotate(geom="text", x=2.8, 0.7, label="13", size=5)+
  annotate(geom="text", x=3.2, 0.7, label="16", size=5)+
  annotate(geom="text", x=3.8, 0.7, label="8", size=5)+
  annotate(geom="text", x=4.2, 0.7, label="2", size=5)+
  scale_x_discrete(labels = c("2" = "small", "6" = "large"))
dev.off()

feeding_p_obs<- modified_feeding_c%>%
  group_by(nest, category) %>%
  summarize(non_na_obs = sum(!is.na(modified_prop)))

sum(feeding_p_obs$non_na_obs)


modified_feeding_c %>%
  group_by(nest) %>%
  summarise(n_unique_broods = n_distinct(broodid))


# Fig.5- Feeding Chicks ---------------------------------------------------
##FEEDING##
feeding_c <- subset(chicks, behavior == "feeding")
#change variables to correct factor
feeding_c$proportion<- as.numeric(feeding_c$proportion)
feeding_c$nest<- as.factor(feeding_c$nest)

tiff("Fig5_R1.tiff", units="in", width=10, height=6, res=300)
ggplot(feeding_c, aes(x = category, y =proportion^0.5, fill=nest))+
  geom_violin(alpha=0.5, position = position_dodge(width = .75),size=1,color=NA) +
  geom_boxplot(notch = T,  outlier.size = -1, color="black",lwd=0.5, alpha = 0.7,show.legend = F)+
  ggbeeswarm::geom_quasirandom(shape = 21,size=2, dodge.width = .75, color="black",alpha=.5,show.legend = F)+
  scale_fill_manual(values=c("#999999", "#E69F00"), labels = c("small", "large")) +
  theme(
    axis.line = element_line(colour = "black",size=1),
    legend.position = c(1.1, 0.5),
    legend.key.size =unit(0.8, "cm"))+
  labs(x = "Age (days)",
       y =  expression(sqrt("proportion of time chicks spent being fed")),
       fill = "Brood size")+
  scale_y_continuous(limits = c(0, 0.4), expand = c(0, 0))+
  theme(aspect.ratio = 1, panel.border = element_rect(colour = "black", fill=NA, size=0.75),legend.title=element_text(size=14), 
        legend.text=element_text(size=14))+
  guides(fill = guide_legend(override.aes = list(alpha = 0.7,color="black")))+ 
  annotate(geom="text", x=0.65, y=0.32, label="N=6", size=5) + 
  annotate(geom="text", x=1.1, y=0.32, label="22", size=5) + 
  annotate(geom="text", x=1.8, y=0.32, label="32", size=5)+
  annotate(geom="text", x=2.2, y=0.32, label="32", size=5)+
  annotate(geom="text", x=2.8, y=0.32, label="26", size=5)+
  annotate(geom="text", x=3.2, y=0.32, label="49", size=5)+
  annotate(geom="text", x=3.8, y=0.32, label="15", size=5)+
  annotate(geom="text", x=4.2, y=0.32, label="12", size=5)+
  scale_x_discrete(labels = c("2" = "small", "6" = "large"))
dev.off()

feeding_c_obs<- feeding_c%>%
  group_by(nest, category) %>%
  summarize(non_na_obs = sum(!is.na(proportion)))

sum(feeding_c_obs$non_na_obs)


feeding_c %>%
  group_by(nest) %>%
  summarise(n_unique_broods = n_distinct(broodid))
# Stats -------------------------------------------------------------------

# chick mass --------------------------------------------------------------

#add day of first weighing
weight_c_n$day2<- ifelse(weight_c_n$Day=="5", weight_c_n$AvgAgeCF, "15")
weight_c_n$day2<- as.numeric(weight_c_n$day2)
weight_c_n$Nest<- as.factor(weight_c_n$Nest)
weight_c_n <- weight_c_n %>% distinct()

mod_w<- lmer(Weight~ Nest*day2+ (1|BirdId)+(1|broodid), data=weight_c_n)
summary(mod_w)
anova(mod_w)
Anova(mod_w)

plot(mod_w)
qqnorm(residuals(mod_w))
plot(resid(mod_w),fitted(mod_w))
hist(resid(mod_w))

#mass gain per day for each brood size
weight_c_n %>%
  group_by(Nest) %>%
  summarise(mean=mean(weight_gain_day, na.rm = TRUE),
            SD=sd(weight_gain_day, na.rm = TRUE))


0.749-0.706


# begging -----------------------------------------------------------------
begging$nest<- as.factor(begging$nest)
begging$proportion<- as.numeric(begging$proportion)
begging$trans_per<- asin(sqrt(begging$proportion/100))

mod_b_c<- lmer(trans_per~nest+category+(1|broodid)+(1|observer), data=begging)
summary(mod_b_c)
anova(mod_b_c)
Anova(mod_b_c)

#with interaction
mod_b_c_i<- lmer(trans_per~nest*category+(1|broodid)+(1|observer), data=begging)
summary(mod_b_c_i)
anova(mod_b_c_i)

plot(mod_b_c)
qqnorm(residuals(mod_b_c))
plot(resid(mod_b_c),fitted(mod_b_c))
hist(resid(mod_b_c))

#find % difference in average proportion of time chicks from different brood sizes spent begging
begging_summary<- begging  %>%
  group_by(nest) %>%
  summarise(mean=mean(proportion))


dif_begging <- begging_summary$mean[begging_summary$nest == 6] -
  begging_summary$mean[begging_summary$nest == 2]

#percentage difference
dif_begging*100


# feeding parents ---------------------------------------------------------
modified_feeding_c$trans_per<- asin(sqrt(modified_feeding_c$modified_prop/100))
mod_f_p<- lmer(trans_per~nest+category+
                 (1|broodid)+
                 (1|observer), 
               data=modified_feeding_c)
summary(mod_f_p)

plot(mod_f_p)
qqnorm(residuals(mod_f_p))
plot(resid(mod_f_p),fitted(mod_f_p))
hist(resid(mod_f_p))

mod_b_c_i<- lmer(trans_per~nest*category+(1|broodid)+(1|observer), data=begging)
summary(mod_b_c_i)
anova(mod_b_c_i)

#find %  difference in average proportion each parent spent feeding between brood sizes
feeding_p_summary<- modified_feeding_c  %>%
  group_by(nest) %>%
  summarise(mean=mean(modified_prop))

dif_feeding_p <- feeding_p_summary$mean[feeding_p_summary$nest == 6] -
  feeding_p_summary$mean[feeding_p_summary$nest == 2]

#percentage difference
dif_feeding_p*100

# feeding chicks ----------------------------------------------------------
feeding_c$trans_per<- asin(sqrt(feeding_c$proportion/100))

mod_f_c<- lmer(trans_per~nest+category+(1|broodid)+(1|observer), data=feeding_c)
summary(mod_f_c)

plot(mod_f_c)
qqnorm(residuals(mod_f_c))
plot(resid(mod_f_c),fitted(mod_f_c))
hist(resid(mod_f_c))

mod_f_c_i<- lmer(trans_per~nest*category+(1|broodid)+(1|observer), data=feeding_c)
summary(mod_f_c_i)
anova(mod_f_c_i)

#find %  difference in average proportion of time chicks spent feeding between brood sizes
feeding_c_summary<- feeding_c  %>%
  group_by(nest) %>%
  summarise(mean=mean(proportion))


dif_feeding_c <- feeding_c_summary$mean[feeding_c_summary$nest == 6] -
  feeding_c_summary$mean[feeding_c_summary$nest == 2]

#percentage difference
dif_feeding_c*100
