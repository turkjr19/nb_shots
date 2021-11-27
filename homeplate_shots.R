# file to determine if shots are in homeplate zones
# load libraries
library(tidyverse)
library(sp) # install.packages("sp") if computer doesn't have this
# package installed

# create away homeplate 
x_coord <- c( -89, -69, -54, -54, -69, -89, -89)
y_coord <- c(3, 22, 22, -22, -22, -3, 3)
away_homeplate <- cbind(x_coord, y_coord)

# create home homeplate
x_coord2 <- c(89, 69, 54, 54, 69, 89, 89)
y_coord2 <- c(3, 22, 22, -22, -22, -3, 3)
home_homeplate <- cbind(x_coord, y_coord)

# pull in shot data from google sheet
nb_shots <- read_csv("nb_shots.csv")
shot <- nb_shots %>% 
  select(full_name, x.plot, y.plot, result)

# determine if shots were in homeplate area
shot <- shot %>% 
  mutate(away_hplate = point.in.polygon(shot$x.plot, shot$y.plot,
                                        x_coord,y_coord)) %>% 
  mutate(home_hplate = point.in.polygon(shot$x.plot, shot$y.plot,
                                        x_coord2,y_coord2))

# just homeplate shots
homeplate_shots <- shot %>% 
  filter(away_hplate == 1 | home_hplate == 1)

# just goals within homepate
goals <- shot %>% 
  filter(result == "goal") %>% 
  filter(away_hplate == 1 | home_hplate == 1) 

# analysis of goals:shots
# homeplate
homeplate_report <- shot %>% 
  filter(away_hplate == 1 | home_hplate == 1) %>% 
  group_by(result) %>% 
  tally()
homeplate_report

# outside homeplate
outside_homeplate_report2 <- shot %>% 
  filter(away_hplate == 0 & home_hplate == 0) %>% 
  group_by(result) %>% 
  tally()
outside_homeplate_report2
