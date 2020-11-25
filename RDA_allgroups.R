# Perform RDA analysis on phytoplankton groups
# load Phytoplankton groups

phyto <- read_delim("HPLC_Prochl_20_11.csv", ";")

# load environmental variabile

env <- read_delim("AllData_MSM80_nounits.csv", ";") %>%
  select(c(Station, Type, Lat, Lon, Depth, MLDepth, MLTemp, MLSalinity, Nitrate, Nitrite, Phosphate, Silicate))
# The RDA requires the two response matrix (species) and the explanatory matrix (environmental) to be of 
# the same length. Thus we create a full table 

# env=env %>% unite(unique_id, "Station", "Depth" , sep = "_", remove = FALSE ) 
# phyto=phyto %>% unite(unique_id, "Station", "Depth" , sep = "_", remove = TRUE ) 

full_table <- left_join(phyto, env, by = c("Station", "Depth")) %>% na.omit()
# create a variable that will distinguis deep and shallow phytoplanton communities
full_table <- full_table %>% mutate(E = ifelse(Depth<= 50, "Shallow", "Deep"))
# remove experiments
setnames(full_table, "Type.x", "type")

no_exp <- filter(full_table, !type %in% c("Experiment1","Experiment2", "ExperimentBirdPoo"))

 

# now we have a full table of both response and explanatory data 
# next is to separate the both matrices 

# Select only the phyto groups
response <- no_exp %>% select(Diatoms:Prochlorophytes)

# Select only environmental variables
library("vegan")
explanatory <- no_exp %>% select(MLDepth:Silicate)
# Standardize the explanatory  to account for differing units
env.z <- decostand(explanatory, method="standardize", na.rm = TRUE) ## standardize x to - 

# Transform phytoplankton to account for widely varying groups 
species_hel <- decostand(response, method="hellinger") # transform the data using hellinger transformation

## rda with transformed variables
spe_rda <- rda(species_hel~., data=env.z, na.action = na.omit) 

summary(spe_rda)
scor = scores(spe_rda, display=c("sp", "cn", "bp"), scaling=2) 

ii=summary(spe_rda)  #View analysis results
sp=as.data.frame(ii$species[,1:6])#Depending on the drawing result, the drawing data can be enlarged or reduced to a certain extent, as follows
st=as.data.frame(ii$sites[,1:2])
yz=as.data.frame(ii$biplot[,1:2])

ggplot() +
  #geom_text_repel(data = st,aes(RDA1,RDA2,label=row.names(st)),size=4)+#Show a Square
  geom_point(data = st,aes(RDA1,RDA2,shape=no_exp$E,fill=no_exp$E),size=3)+ # visualize rda output
  scale_color_manual(values=c("yellow","#E69F00"),breaks = c("High", "Low"))+ # change colour and shape of points
  scale_shape_manual(values = c(22:23))+
  # geom_segment(data = sp,aes(x = 0, y = 0, xend = RDA1, yend = RDA2), 
  #              arrow = arrow(angle=22.5,length = unit(0.35,"cm"),
  #                            type = "closed"),linetype=1, size=0.6,colour = "orange")+
  geom_text(data = sp,aes(RDA1,RDA2,label=row.names(sp)), check_overlap = TRUE)+ #add the phytogroups names
  geom_segment(data = yz,aes(x = 0, y = 0, xend = RDA1, yend = RDA2), 
               arrow = arrow(angle=22.5,length = unit(0.35,"cm"),
                             type = "closed"),linetype=2, size=0.7,colour = "steelblue")+ #adds arrows to env variables
  geom_text(data = yz,aes(RDA1,RDA2,label=row.names(yz)), check_overlap = TRUE)+ # names of env variables
  labs(x=paste("RDA 1 (", format(100 *ii$cont[[1]][2,1], digits=3), "%)", sep=""), # labels
       y=paste("RDA 2 (", format(100 *ii$cont[[1]][2,2], digits=3), "%)", sep=""))+
  geom_hline(yintercept=0,linetype=3,size=1) + # dotted lines that go through the center 
  geom_vline(xintercept=0,linetype=3,size=1)+
  guides(shape=guide_legend(title="Communities",color="black"), # legend fix
         fill=guide_legend(title="Communities"))

