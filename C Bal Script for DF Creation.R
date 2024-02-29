


###Code for visualizing C balance curves and extracting stats using CO2 flux curves of Moss + Soil and Soil samples

library(tidyverse)

###Expected .csv's for this code match the output of the LiCor6800 excel files.
Soil<-read_csv(skip = 13,col_names=FALSE, file.choose()) #Choose the .csv for the soil sample
Moss<-read_csv(skip = 13,col_names=FALSE, file.choose()) #Choose the .csv for the moss tissue sample

###Insert sample name below
Sample <- "BB-F 46a"

##### MOSS CURVE -> Cleaning ####

Moss
positions <- c(3,8)

mclean<- Moss %>% 
  select(positions)%>%
  rownames_to_column(var = "Column")%>%
  rename(Time = X3, `Moss A` = X8)

plot(mclean$`Moss A` ~ mclean$Time)

#Converting Time to minutes
mclean<- mclean%>%
  mutate(Time = Time/60)

mclean %>%
  ggplot(aes(Time,`Moss A`)) +
  geom_point() +
  geom_hline(yintercept=0, linetype=1, color="black", size=1)+
  stat_smooth(se=F,span=0.1,method='loess')+
  scale_x_continuous(breaks = seq(0, 1000, by = 50))+
  geom_vline(xintercept=400, linetype="dashed", color = "green", size=0.5)

#### find zeros function ####
find_zeros_f <- function(df){
  zeroes <- df %>%
    mutate(findzero = A*lag(A,1)) %>%
    filter(findzero <0)%>%
    pull(Time)
  return(zeroes)
}

# Smooth Moss
smooth_moss <- loess(`Moss A` ~ Time, data = mclean,span=0.1)
smooth_moss_pts <- tibble(Time = seq(from=-1,to=1000,by=1), A =predict(smooth_moss,newdata =data.frame(Time=seq(from=-1,to=1000,by=1))) )%>% filter(is.finite(A))

# Moss Zeros
find_zeros_f(smooth_moss_pts)

# Trim Moss (May need to change #)
trim_time_moss <- ifelse(is.na(find_zeros_f(smooth_moss_pts)[3]), mclean$Time[length(mclean$Time)],find_zeros_f(smooth_moss_pts)[3])
#trim_time_moss <- 400

mclean <- mclean %>%
  mutate(`Moss A` = ifelse(Time>trim_time_moss,0,`Moss A`)) %>% 
  filter(Time <trim_time_moss)

m <- mclean %>%
  ggplot(aes(Time,`Moss A`)) +
  geom_point() +
  geom_hline(yintercept=0, linetype=1, color="black", size=1)+
  stat_smooth(se=F,span=0.1,method='loess')+ 
  labs(x="Time (Minutes)",y= bquote ('A ' ~ (mu~mol ~ m^-2 ~s^-1)))+
  scale_x_continuous(breaks = seq(0, 1000, by = 100))+
  scale_y_continuous(breaks = seq(-16, 25, by = 0.5))+
  theme_classic() +
  annotate("text", x=200, y=3, label= 'Moss + Soil', size= 5, color= "black")+
  theme_classic()+
  theme(axis.title.x=element_text(size=14))+
  theme(axis.title.y=element_text(size=14))+
  theme(axis.text.x=element_text(size=13, angle= 45, hjust= 1))+
  theme(axis.text.y=element_text(size=13)) +
  coord_cartesian(xlim=c(0,600), ylim=c(-3,3))
m


###### SOIL CURVE -> Cleaning ####
sclean<- Soil %>% 
  select(positions)%>%
  rownames_to_column(var = "Column")%>%
  rename(`Soil A`= X8, STime = X3) 

#Converting Time to minutes
sclean<- sclean %>%
  mutate(STime = STime/60)

sclean %>%
  ggplot(aes(STime,`Soil A`)) +
  geom_point() +
  geom_hline(yintercept=0, linetype=1, color="black", size=1)+
  stat_smooth(se=F,span=0.1,method='loess')+
  scale_x_continuous(breaks = seq(0, 1000, by = 50))+
  geom_vline(xintercept=340, linetype="dashed", color = "green", size=0.5)

# Smooth Soil
smooth_soil <- loess(`Soil A` ~ STime, data = sclean,span=0.1)
smooth_soil_pts <- tibble(Time = seq(from=-1,to=50000,by=1), A =predict(smooth_soil,newdata =data.frame(STime=seq(from=-1,to=50000,by=1))) )%>% filter(is.finite(A))

# Soil Zeros
find_zeros_f(smooth_soil_pts)

#Trim Soil (May need to change #)
trim_time_soil <- find_zeros_f(smooth_soil_pts)[1]
#trim_time_soil <- trim_time_moss
#trim_time_soil <- ifelse(is.na(find_zeros_f(smooth_soil_pts)[1]), sclean$STime[length(sclean$STime)],find_zeros_f(smooth_soil_pts)[1])
#trim_time_soil <- 340

sclean <- sclean %>%
  mutate(`Soil A` = ifelse(STime>trim_time_soil,0,`Soil A`)) %>% 
  filter(STime<trim_time_soil)

s <- sclean %>%
  ggplot(aes(STime,`Soil A`)) +
  geom_point() +
  geom_hline(yintercept=0, linetype=1, color="black", size=1)+
  stat_smooth(se=F,span=0.1,method='loess') +
  labs(x="Time (Minutes)",y= bquote ('A ' ~ (mu~mol ~ m^-2 ~s^-1)))+
  scale_x_continuous(breaks = seq(0, 1000, by = 100))+
  scale_y_continuous(breaks = seq(-16, 25, by = 0.5))+
  theme_classic() +
  annotate("text", x=200, y=3, label= 'Soil', size= 5, color= "black")+
  theme_classic()+
  theme(axis.title.x=element_text(size=14))+
  theme(axis.title.y=element_text(size=14))+
  theme(axis.text.x=element_text(size=13, angle= 45, hjust= 1))+
  theme(axis.text.y=element_text(size=13)) +
  coord_cartesian(xlim=c(0,600), ylim=c(-3,3))
s


##### COMBINE CURVES ####
positions2 <-c(2,4,5,7)
both <- mclean %>%
  left_join(sclean,by='Column')%>%
  select(positions2) %>%
  mutate(STime = ifelse(is.na(STime),Time,STime),
         `Soil A` = ifelse(is.na(`Soil A`),0,`Soil A`))

both2 <- both %>%
  mutate(
    lin_extrapo = (`Soil A` - lag(`Soil A`,1))/(`STime`-lag(`STime`,1))*(`Time`- lag(`STime`,1))+lag(`Soil A`),
    A = `Moss A` - lin_extrapo)

#plot of normalized c curve by points
plot(both2$A ~ both2$Time)
abline(h=0)

###Finding Carbon Balance 
lofit<- loess(A ~ Time, data = both2,span=0.1)
predtibble <- tibble(Time = seq(from=-1,to=800,by=1), A =predict(lofit,newdata =data.frame(Time=seq(from=-1,to=800,by=1))) )%>% filter(is.finite(A))
predtibble$A[1]<-0
lines(predtibble$Time, predtibble$A, col ="red")

# Trim Time Both = Curve End Time
#trim_time_both <- find_zeros_f(predtibble)
trim_time_both <- ifelse(is.na(find_zeros_f(predtibble)[3]), predtibble$Time[length(predtibble$Time)],find_zeros_f(predtibble)[3])

predtibble <- predtibble %>% filter(Time <trim_time_both)

#### GRAPH ####
g <- ggplot(data=predtibble,aes(x=Time,y=A))+  
  geom_path(col="brown2", size= 1.25)+
  labs(x="Time (Minutes)",y= bquote ('A ' ~ (mu~mol ~ m^-2 ~s^-1)))+
  geom_hline(yintercept=0, linetype=2, color="black", size=1)+
  scale_x_continuous(breaks = seq(0, 1000, by = 100))+
  scale_y_continuous(breaks = seq(-16, 25, by = 0.5))+
  #geom_vline(xintercept=500, linetype="dashed", color = "green", size=0.5)+
  #################################Rename graph
  annotate("text", x=200, y=3, label= Sample, size= 5, color= "black")+
  theme_classic()+
  theme(axis.title.x=element_text(size=14))+
  theme(axis.title.y=element_text(size=14))+
  theme(axis.text.x=element_text(size=13, angle= 45, hjust= 1))+
  theme(axis.text.y=element_text(size=13)) +
  coord_cartesian(xlim=c(0,600), ylim=c(-6,3))
g

#### VIEW 3 GRAPHS SIDE BY SIDE ####
library(grid)
library(gridExtra)
grid.arrange(m, s, g, ncol=3)

##### IMPORTANT STATS ####

##Summary table
summary_stats <- predtibble %>% summarize(
  Sample = Sample,
  total_cbal = sum(A)/1000,
  CO2comp1 = ifelse(is.na(find_zeros_f(.)[1]), trim_time_both ,find_zeros_f(.)[1]),
  CO2comp2 = find_zeros_f(.)[2],
  aphase_area = (A[which(A<0 & Time<CO2comp1)] %>% sum())/1000,
  End_Time = ifelse(is.na(find_zeros_f(.)[3]), predtibble$Time[length(predtibble$Time)] ,find_zeros_f(.)[3]), 
  R_maxval_A = A[which(A<0 & Time < ifelse(is.na(CO2comp1),End_Time,CO2comp1))] %>% min(),
  R_maxtime_A = Time[which(A == R_maxval_A)],P_maxval = max(A),
  bphase_area = ifelse(is.na(CO2comp2),NA,A[which(A>0 & Time>CO2comp1 & Time<CO2comp2)] %>% sum())/1000,
  P_maxval = ifelse(is.na(A[which(A>0 & Time < CO2comp2)]) %>% max(), 0, A[which(A>0 & Time < CO2comp2)] %>% max()),
  P_maxtime = ifelse(Time[which(A == P_maxval)]<R_maxtime_A, 0, Time[which(A == P_maxval)]), 
  R_maxval_C = ifelse(bphase_area>0,A[which(A<0 & Time > ifelse(is.na(CO2comp2),End_Time,CO2comp2))] %>% min(),0),
  R_maxtime_C = ifelse(bphase_area>0,Time[which(A == R_maxval_C)],0),
  cphase_area = ifelse(is.na(CO2comp2),NA,A[which(A<0 & Time>P_maxtime)] %>% sum())/1000,
  # slope 1 (start -> A-bottom)
  slope_1 = (R_maxval_A - 0)/(R_maxtime_A - 0),
  # slope 2a (A-bottom -> zero)
  slope_2a = (0 - R_maxval_A)/(CO2comp1 - R_maxtime_A),
  # slope 2b (zero -> B-top)
  slope_2b = ifelse(is.na(CO2comp2),NA,((P_maxval - 0)/(P_maxtime - CO2comp1))),
  # slope 2 (A-bottom -> B-top)
  slope_2 = ifelse(is.na(CO2comp2),slope_2a,((P_maxval - R_maxval_A)/(P_maxtime - R_maxtime_A))),
  # slope 3 (B-top -> C bottom)
  slope_3 =  ifelse(is.na(CO2comp2),NA,((R_maxval_C - P_maxval)/(R_maxtime_C - P_maxtime))),
  # slope 4 (C-bottom -> end)
  slope_4 = ifelse(is.na(CO2comp2),NA,((0 - R_maxval_C)/(End_Time - R_maxtime_C)))
)
view(summary_stats)


#### EXPORT ####
# Do this the first time
# outputfilename <- paste(getwd(),'/summary_stats.csv',sep='')
# write_csv(summary_stats,outputfilename)

summary_stats2 <- read_csv("/Users/migomez/Documents/MIDD/4 - Fourth Year (2019-2020)/MOSS Dr. Coe (BIOL 0700)/2019 Sheep Creek/summary_stats.csv")

if(Sample %in% summary_stats2$Sample)
{
  summary_stats2 <- summary_stats2 %>% filter(Sample != Sample)
  summary_stats2 <- rbind(summary_stats2,summary_stats)
} else
{
  summary_stats2 <- rbind(summary_stats2,summary_stats)
}

write_csv(summary_stats2,"/Users/migomez/Documents/MIDD/4 - Fourth Year (2019-2020)/MOSS Dr. Coe (BIOL 0700)/2019 Sheep Creek/summary_stats.csv")


