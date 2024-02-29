


###Code to make a Composite Curve of Cbals (in collaboration with Nico Gomez)

library(tidyverse)

bb_dir <- '/Users/migomez/Documents/MIDD/4 - Fourth Year (2019-2020)/MOSS Dr. Coe (BIOL 0700)/2019 Sheep Creek/BB'
bb_files <- list.files(bb_dir, pattern = ".csv") %>% grep(pattern = 'BAD|soil', invert = TRUE, value = TRUE)
bb_df <- data.frame(dir =bb_dir, files=bb_files)
cb_dir <- '/Users/migomez/Documents/MIDD/4 - Fourth Year (2019-2020)/MOSS Dr. Coe (BIOL 0700)/2019 Sheep Creek/CB'
cb_files <- list.files(cb_dir, pattern = ".csv") %>% grep(pattern = 'BAD|soil', invert = TRUE, value = TRUE)
cb_df <- data.frame(dir =cb_dir, files=cb_files)
pj_dir <- '/Users/migomez/Documents/MIDD/4 - Fourth Year (2019-2020)/MOSS Dr. Coe (BIOL 0700)/2019 Sheep Creek/PJ'
pj_files <- list.files(pj_dir, pattern = ".csv") %>% grep(pattern = 'BAD|soil', invert = TRUE, value = TRUE)
pj_df <- data.frame(dir =pj_dir, files=pj_files)

trim_times <- readxl::read_excel("/Users/migomez/Documents/MIDD/4 - Fourth Year (2019-2020)/MOSS Dr. Coe (BIOL 0700)/2019 Sheep Creek/trim_times.xlsx") %>% 
  mutate(Sample = str_to_upper(Sample))

files_df <- bind_rows(bb_df, cb_df, pj_df)

list_of_lofits <- list()
list_of_both2 <- list()
list_of_predtibbles <- list()

for(i in 1:nrow(files_df)){
  cur_dir <- files_df[i,1]
  cur_files <- files_df[i,2]
  
  index <- i
  len <- nrow(files_df)
  
  Moss<-read_csv(skip = 13,col_names=FALSE, file.path(cur_dir, cur_files))#file.choose()) #Choose the .csv for the moss tissue sample
  Soil<-read_csv(skip = 13,col_names=FALSE, file.path(cur_dir, str_replace(string = cur_files, pattern = '\\.csv', replacement = ' soil\\.csv'))) #Choose the .csv for the soil sample
  
  ###Name Sample
  Sample <- str_remove(string = cur_files, pattern = '\\.csv') %>% toupper()
  
  ##### MOSS CURVE -> Cleaning ####
  
  positions <- c(3,8)

mclean<- Moss %>% 
  select(positions)%>%
  rownames_to_column(var = "Column")%>%
  rename(Time = X3, `Moss A` = X8)

#Converting Time to minutes
mclean<- mclean%>%
  mutate(Time = Time/60)
print(
mclean %>%
  ggplot(aes(Time,`Moss A`)) +
  geom_point() +
  geom_hline(yintercept=0, linetype=1, color="black", size=1)+
  stat_smooth(se=F,span=0.1,method='loess'))

#find zeros function ####
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
curr_zerosM <- find_zeros_f(smooth_moss_pts)

## GET USER INPUT 1 ####
# cat(paste0('\nCurrent experiment ', Sample,'  ..... (', paste0(c(index, len), collapse = ' out of '), ')\n\nAvailable zeros: ',paste0(curr_zerosM, collapse = ', '),'\n\nWhich zero?')) #user prompt
# userChoiceM <- readline(prompt='Enter number:')
# userChoiceM <- ifelse(length(userChoiceM) ==0, 3, as.numeric(userChoiceM))

# Trim Moss
moss_trim_var <- trim_times[trim_times$Sample==Sample,'trim_time_moss2'] %>% as.numeric

trim_time_moss <- ifelse(moss_trim_var > 5, moss_trim_var,
                         ifelse(is.na(find_zeros_f(smooth_moss_pts)[moss_trim_var]), mclean$Time[length(mclean$Time)],find_zeros_f(smooth_moss_pts)[moss_trim_var]))

mclean <- mclean %>%
  mutate(`Moss A` = ifelse(Time>trim_time_moss,0,`Moss A`)) %>% 
  filter(Time <trim_time_moss)

###### SOIL CURVE -> Cleaning ####
sclean<- Soil %>% 
  select(positions)%>%
  rownames_to_column(var = "Column")%>%
  rename(`Soil A`= X8, STime = X3) 

#Converting Time to minutes
sclean<- sclean %>%
  mutate(STime = STime/60)

print(
sclean %>%
  ggplot(aes(STime,`Soil A`)) +
  geom_point() +
  geom_hline(yintercept=0, linetype=1, color="black", size=1)+
  stat_smooth(se=F,span=0.1,method='loess'))

# Smooth Soil
smooth_soil <- loess(`Soil A` ~ STime, data = sclean,span=0.1)

smooth_soil_pts <- tibble(Time = seq(from=-1,to=50000,by=1), A =predict(smooth_soil,newdata =data.frame(STime=seq(from=-1,to=50000,by=1))) )%>% filter(is.finite(A))

# Soil Zeros
curr_zerosS <- find_zeros_f(smooth_soil_pts)

## GET USER INPUT
# cat(paste0('\nCurrent experiment ', Sample,'  .....  \n\nAvailable zeros: ',paste0(curr_zerosS, collapse = ', '),'\n\nWhich zero??')) #user prompt
# userChoiceS <- readline(prompt='Enter number:')
# userChoiceS <- ifelse(length(userChoiceS) ==0, 1, as.numeric(userChoiceS))

#Trim Soil
#trim_time_soil <- ifelse(is.na(find_zeros_f(smooth_soil_pts)[userChoiceS]), sclean$STime[length(sclean$STime)],find_zeros_f(smooth_soil_pts)[userChoiceS])

soil_trim_var <- trim_times[trim_times$Sample==Sample,'trim_time_soil2'] %>% as.numeric

trim_time_soil <- ifelse(soil_trim_var > 3, soil_trim_var,
                         ifelse(is.na(find_zeros_f(smooth_soil_pts)[soil_trim_var]), mclean$Time[length(mclean$Time)],find_zeros_f(smooth_soil_pts)[soil_trim_var]))

sclean <- sclean %>%
  mutate(`Soil A` = ifelse(STime>trim_time_soil,0,`Soil A`)) %>% 
  filter(STime<trim_time_soil)

##### COMBINE CURVES ####
#positions2 <-c(2,4,5,7)
both <- mclean %>%
  left_join(sclean,by='Column')%>%
  mutate(STime = ifelse(is.na(STime),Time,STime),
         `Soil A` = ifelse(is.na(`Soil A`),0,`Soil A`))

both2 <- both %>%
  mutate(
    lin_extrapo = (`Soil A` - lag(`Soil A`,1))/(`STime`-lag(`STime`,1))*(`Time`- lag(`STime`,1))+lag(`Soil A`),
    A = `Moss A` - lin_extrapo)

#plot of normalized c curve by points
#plot(both2$A ~ both2$Time)
#abline(h=0)

list_of_both2[[Sample]] <- select(both2,Time, A)

###Finding Carbon Balance 

lofit<- loess(A ~ Time, data = both2,span=0.1)
list_of_lofits[[Sample]] <- lofit
predtibble <- tibble(Time = seq(from=-1,to=800,by=1), A =predict(lofit,newdata =data.frame(Time=seq(from=-1,to=800,by=1))) )%>% filter(is.finite(A))

predtibble$A[1]<-0

list_of_predtibbles[[Sample]] <- predtibble
graphics.off()
} ##### LOOP END ####


#predtibble_master <- bind_rows(list_of_predtibbles, id = 'sample')
#lofit_master <- bind_rows(list_of_lofits, id = 'sample')
#both2_master <- bind_rows(list_of_both2, id = 'sample')

# Combine list of dataframes into single dataframe with id column "sample" containing sample name

# Once for predtiblle
for(i in 1:length(list_of_predtibbles)){
  curr_name <- names(list_of_predtibbles[i])
  list_of_predtibbles[[i]] <- mutate(list_of_predtibbles[[i]], sample = curr_name)
}
predtibble_master <- do.call(rbind, list_of_predtibbles)
predtibble_master <- mutate(predtibble_master, macro_site = str_extract(sample, 'CB|BB|PJ'))

# Once for "both2"
for(i in 1:length(list_of_both2)){
  curr_name <- names(list_of_both2[i])
  list_of_both2[[i]] <- mutate(list_of_both2[[i]], sample = curr_name)
}

both2_master <- do.call(rbind, list_of_both2)
both2_master <- as_tibble(both2_master)
both2_master <- mutate(both2_master, macro_site = str_extract(sample, 'CB|BB|PJ'))

# Once for lowfit
# for(i in 1:length(list_of_lofits)){
#   curr_name <- names(list_of_lofits[i])
#   list_of_lofits[[i]] <- mutate(list_of_lofits[[i]], sample = curr_name)
# }
# lofits_master <- do.call(rbind, list_of_lofits)
# lofits_master <- mutate(list_of_lofits, macro_site = str_extract(sample, 'CB|BB|PJ'))


# Switch Macrosite to Low, Mid, High variables (instead of CB, BB, PJ)
newnames <- data.frame(macro_site = c("CB", "BB", "PJ"), macro_site_new = c("Low", "Mid", "High"), stringsAsFactors = FALSE)
predtibble_master2 <- left_join(predtibble_master, newnames, by = c("macro_site")) %>%
  rename(Macrosite = macro_site_new)

# Factor Macrosite column in correct order
predtibble_master2$Macrosite <- factor(predtibble_master2$Macrosite, levels = c("Low", "Mid", "High"))

#### GRAPH FROM PREDTIBBLE DATA ####
ggplot(predtibble_master2, aes(x=Time,y=A)) + geom_path(aes(group = sample, color = Macrosite), size= .5) +
  facet_wrap(.~Macrosite) + 
  labs(x="Time (Minutes)",y= bquote ('A' ~ (mu~mol ~ m^-2 ~s^-1)))+
  geom_hline(yintercept=0, linetype=2, color="black")+
  scale_x_continuous(breaks = seq(0, 1000, by = 100))+
  scale_y_continuous(breaks = seq(-16, 25, by = 0.5))+
  theme_classic() +
  ggpubr::color_palette(palette = 'npg') +
  coord_cartesian(xlim=c(0,700), ylim=c(-9,2))

## Graph from raw data
ggplot(both2_master, aes(x=Time,y=A)) + geom_path(aes(group = sample, color = macro_site), size= .5) +
  #stat_smooth(method = "auto", formula = y ~ x, se = TRUE, n = 80,fullrange = FALSE, level = 0.95, color = 'black')+
  geom_smooth(color ='black', method = 'loess')+
  facet_wrap(.~macro_site)+
  labs(x="Time (Minutes)",y= bquote ('A ' ~ (mu~mol ~ m^-2 ~s^-1)))+
  geom_hline(yintercept=0, linetype=2, color="black", size=1)+
  scale_x_continuous(breaks = seq(0, 1000, by = 100))+
  scale_y_continuous(breaks = seq(-16, 25, by = 0.5))+
  coord_cartesian(xlim=c(0,trim_time_moss), ylim=c(-9,2))



