library("tidyverse")
data("mtcars")
glimpse (mtcars)

mtcars _summary <- mtcars %>% group_by(cyl) %>% summarise(mean_disp = mean(disp),
                                                 mean_hp = mean(hp))
ggplot(mtcars, aes(disp, hp, colour - cyl))+
  geom_point()
# commit - graph 
