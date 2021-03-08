## extract the Ekman transport for selected coordinates and days
# The csv files are wht Sadegh provided in .mat format, I just converted them to .csv so it is readable by R. The first column is the days:
#1-61 meaning 1st of December 2018 until 30th of January 2019
library("readr")
library("tidyverse")
library("reshape2")
library("janitor")
library("dplyr")

## load files and select only the lon where transects were sampled
#T1

T1up <- read_delim("Upwelling/UE_MSM_T1.csv", ",")
T1up <- T1up %>% filter(lon %in% c( "-80.5","-80.25","-80","-79.75", "-79.5","-79.25")) %>% 
  mutate(transect = "Transect 1", 
         station = c("1", "4", "7", "10", "13", "14")) 
T1 <- T1up %>% melt(id.vars = c("lon", "transect", "station"))

# T2
T2up <- read_delim("Upwelling/UE_MSM_T2.csv", ",") 
T2up <- T2up %>% filter(lon %in% c("-80","-79.75", "-79.5", "-79","-78.5", "-78.25")) %>% 
  mutate(transect = "Transect 2",
         station = c(18, 20, 22, 25, 28, 30))

T2 <- T2up %>% melt(id.vars = c("lon", "transect", "station"))
# T3 
T3up <- read_delim("Upwelling/UE_MSM_T3.csv", ",")
T3up <- T3up %>% filter (lon %in% c("-77", "-77.25", "-77.5", "-77.75", "-78")) %>% 
  mutate(transect = "Transect 3",
         station = c("46_22", "45","43", "40", "38"))
  
T3 <- T3up %>% melt(id.vars = c("lon", "transect", "station"))
# T4 

T4up <- read_delim("Upwelling/UE_MSM_T4.csv", ",")
T4up <- T4up %>% filter(lon %in% c("-77.5", "-77", "-76.75", "-76.5", "-76.25", "-76")) %>% 
  mutate(transect = "Transect 4", 
         station = (c("53", "56_3", "58", "66", "63", "65")))
# T5

T5up <- read_delim("Upwelling/UE_MSM_T5.csv", ",")  
T5up <- T5up %>% filter(lon %in% c("-75.25", "-75.5", "-75.75", "-76", "-76.5")) %>% 
  mutate(transect = "Transect 5",
         station = c("78", "74", "80", "82", "67"))

# T6

T6up <- read_delim("Upwelling/UE_MSM_T6.csv", ",")
T6up <- T6up %>% filter(lon %in% c("-74", "-75", "-75.25", "-76", "-74.5" )) %>% 
  mutate(transect = "Transect 6",
         station = c("96", "99", "94_6", "104_1", "94"))



Tallup <- rbind(T1up,T2up,T3up,T4up,T5up,T6up) %>% melt(id.vars = c("lon", "transect", "station"))

all_data <- read_delim("AllData_MSM80_17022021.csv", ",") %>% clean_names()

upwell <- left_join(Tallup, all_data, by = c("station","transect"))
peru_coastline <- read_delim("peru coastline.csv", ";")

ggplot() +coord_fixed()+
 geom_polygon(aes(x=lon, y=lat), colour = "black", fill ="grey", data=peru_coastline)+
  geom_text(mapping=aes(x=lon_e_w, y=lat_n_s, label=station),check_overlap = T, size=3, data=upwell)


# for each transect I select the longtitude that is closest to each station coordinate then I assign the name of the stations. 
#T1 
T1up <- T1up %>% select(c("-80.875", "-80.375","-80.125", "-79.625", "-79.375","-79.125")) %>%
  setNames(c("1", "4", "7", "10", "13", "14"))
T1up <- rownames_to_column(T1up, "day") # give name to column 0 

t1 <- melt(T1up, id.vars = c("lon", "Transect"))

measure.vars =c("1", "4", "7", "10", "13", "14"))
#T2
T2up <- T2up %>% select(c("-80.125", "-79.625","-79.375","-78.625","-78.375") %>% setNames(c("18", "20", "22", "28", "30")))

T2up <- rownames_to_column(T2up, "day") # give name to column 0 


t2 <-melt(T2up, id.vars = "day", measure.vars = c("18", "20", "22", "28", "30"))

#T3
T3up <- T3up %>% select(c("-77.125","-77.375","-77.625", "-77.875","-78.125")%>% setNames(c("38", "40", "43", "45", "46_22")))

T3up <- rownames_to_column(T3up, "day") # give name to column 0 
t3 <-melt(T3up, id.vars = "day", measure.vars = c("38", "40", "43", "45", "46_22"))

#T4
T4up <- T4up %>% select(c("-77.625", "-77.125",  "-76.625", "-76.375", "-76.125", "-75.875","-75.375")) %>% setNames(c("53", "56_3", "58", "66", "63", "65"))

T4up <- rownames_to_column(T4up, "day") # give name to column 0 
t4 <-melt(T4up, id.vars = "day", measure.vars = c("53", "56_3", "58", "66", "63", "65"))

#T5
T5up <- T5up %>% select(c("-75.125","-75.375","-75.625" ,"-75.875","-76.125" )%>% setNames(c("67", "82", "80", "78", "74")))

T5up <- rownames_to_column(T5up, "day") # give name to column 0 
t5 <-melt(T5up, id.vars = "day", measure.vars = c("67", "82", "80", "78", "74"))

#T6
T6up <- T6up %>% select(c("-74.125","-74.375","-75.125", "-75.375", "-76.125" )%>% setNames(c("95", "104", "94_6", "99", "96")))

T6up <- rownames_to_column(T6up, "day") # give name to column 0 
t6 <-melt(T6up, id.vars = "day", measure.vars = c("95", "104", "94_6", "99", "96"))
## And here you select the days you want for each transect. So Transect 1 took place from 27-29th Dec, and if you want
# to have it 2 weeks before just put 13-15, that would mean 13-15th Dec.

T1 <- t1 %>% filter(day %in%c(27,28,29)) ## the average upwelling of 7 days when cruise was taking place. 

T2 <- t2 %>% filter(day %in% c(32,33,34)) ## the average upwelling of 7 days when cruise was taking place.

T3 <- t3 %>% filter(day %in% c(27,28,29))

T4 <- t4 %>% filter(day %in% c(32,33,34))

T5 <- t5 %>% filter(day %in% c(45,46,47,48))

T6 <- t6 %>% filter(day %in% c(53,54,55,56))

setnames(T6, "variable", "station")
setnames(T5, "variable", "station")
setnames(T4, "variable", "station")
setnames(T3, "variable", "station")
setnames(T2, "variable", "station")
setnames(T1, "variable", "station")
setnames(all, "Lat ", "lat")
setnames(all, "Lon", "lon")

## here I put all the transect in one data frame 
Tallup <- rbind(T1,T2,T3,T4,T5,T6)
# here I join my other env data, such as nutrients and physical data together with the upwelling data
cruiseup <- right_join(Tallup, all, by = "station") # combine both data sets 

upwelling= cruiseup %>% group_by(station_new) %>% filter(depth<60) %>% mutate(meannit = mean(Nitrate),
                                                                              avUp = mean(value),
                                                                              meanphos = mean(Phosphate),
                                                                              meansil= mean(Silicate),
                                                                              meantemr = mean(t090C),
                                                                              meanoxy = mean(`sbeox0ML/L`),
                                                                              meansal= mean(sal00),
                                                                              meanchl = mean(Chl_a),
                                                                              sd = sd(value), na.rm = TRUE)
# T1_cor<- filter(upwelling, station_new %in% c("T1_1", "T1_2", "T1_3", "T1_4", "T1_5", "T1_6"))
# T2_cor<- filter(upwelling, station_new %in% c("T2_1", "T2_2", "T2_3", "T2_4", "T2_5"))
# T3_cor <- filter(upwelling, station_new %in% c("T3_1", "T3_2", "T3_3", "T3_4", "T3_5"))
# T4_cor<- filter(upwelling, station_new %in% c("T4_1", "T4_2", "T4_3", "T4_4", "T4_5", "T4_6", "T4_7"))
# T5_cor<- filter(upwelling, station_new %in% c("T5_1", "T5_2", "T5_3", "T5_4", "T5_5"))
# T6_cor <- filter(upwelling, station_new %in% c("T6_1", "T6_2", "T6_3", "T6_4", "T6_5"))

legend <-expression ("Ekman Transport"~
                       m^2*s^-1)

bw_update <- theme_bw() +
  theme(plot.background = element_blank(),
        panel.background = element_rect(colour = NA, fill = NA),
        panel.border = element_rect(colour = "black",
                                    fill = NA, size = 0.8),
        panel.grid.minor = element_line(colour = NA),
        panel.grid.major = element_line(colour = "black",
                                        size = 0.2,
                                        linetype = "dotted"),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 10),
        axis.ticks = element_line(size = 0.5),
        axis.ticks.length = unit(2, "mm"),
        legend.direction = "vertical",
        legend.title = element_text(size = 10),
        legend.text = element_text( size = 10),
        legend.key = element_blank(),
        legend.background = element_blank(),
        plot.title = element_text(size = 10, hjust = 0,face = "italic"),
        strip.background = element_rect(colour = "white", fill = "white"),
        strip.text = element_text(size = 8))

ggplot(upwelling)+
  geom_line(aes(x= Distance_from_shore, y=avUp, group = Transect, colour = Transect), size = 2)+
  geom_point(aes(x= Distance_from_shore, y=avUp, group = Transect, colour = Transect), size = 4)+
  geom_errorbar(aes(x = Distance_from_shore, ymin = avUp-sd, ymax = avUp+sd,colour = Transect))+
  scale_color_brewer(palette = "Dark2")+
  scale_x_reverse(name = 'Distance from shore (km)', limits=c(200,0))+
  scale_y_continuous(name = legend, limits = c(-2.5, 0))+
  bw_update
ggsave(filename = "MSM80_Ekman_phyto.png",width = 10, height = 6, dpi = 150, units = "in", device='png')

