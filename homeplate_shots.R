# file to determine if shots are in homeplate zones
# load libraries
library(tidyverse)
library(googlesheets4)
library(sp) # install.packages("sp") if computer doesn't have this
# package installed

# create away home plate polygon
x_coord <- c( -89, -69, -54, -54, -69, -89, -89)
y_coord <- c(3, 22, 22, -22, -22, -3, 3)
away_homeplate <- cbind(x_coord, y_coord)

# create home home plate polygon
x_coord2 <- c(89, 69, 54, 54, 69, 89, 89)
y_coord2 <- c(3, 22, 22, -22, -22, -3, 3)
home_homeplate <- cbind(x_coord, y_coord)

# get google sheet and establish where to append too later in code
ss <- gs4_get('https://docs.google.com/spreadsheets/d/1aueTPPjV3axvQU9Eu7T-LLbzW2CASZKy5iI7vuNrJK8/edit?usp=sharing')

# read sheet from googlesheet (nb_shots shared in claircornish google drive)
df2 <- read_sheet("https://docs.google.com/spreadsheets/d/1aueTPPjV3axvQU9Eu7T-LLbzW2CASZKy5iI7vuNrJK8/edit?usp=sharing")

# pull out what we need
shot <- df2 %>% 
  select(full_name, x.plot, y.plot, result)

# determine if shots were in home plate area
shot <- shot %>% 
  mutate(away_hplate = point.in.polygon(shot$x.plot, shot$y.plot,
                                        x_coord,y_coord)) %>% 
  mutate(home_hplate = point.in.polygon(shot$x.plot, shot$y.plot,
                                        x_coord2,y_coord2))

# *********
# analysis
# home plate shooting percentage
homeplate_report <- shot %>% 
  filter(away_hplate == 1 | home_hplate == 1) %>% 
  group_by(result) %>% 
  tally() %>% 
  ungroup() %>% 
  pivot_wider(names_from = result, values_from = n) %>% 
  mutate(area = "homeplate",
         `shooting %` = round(goal/shot,3)) %>% 
  select(area, everything())
  
# outside homeplate shooting percentage
outside_homeplate_report <- shot %>% 
  filter(away_hplate == 0 & home_hplate == 0) %>% 
  group_by(result) %>% 
  tally() %>% 
  ungroup() %>% 
  pivot_wider(names_from = result, values_from = n) %>% 
  mutate(area = "outside homeplate",
         `shooting %` = round(goal/shot,3)) %>% 
  select(area, everything())

# combine to produce report
shooting_report <- bind_rows(homeplate_report, outside_homeplate_report)

# goal report
# homeplate goals by player
homeplate_goal_report <- shot %>% 
  filter(result == "goal") %>% 
  filter(away_hplate == 1 | home_hplate == 1) %>% 
  group_by(full_name) %>%
  tally() %>% 
  rename(homeplate_goals = "n")
# non homeplate goals by player
non_homeplate_goal_report <- shot %>% 
  filter(result == "goal") %>% 
  filter(away_hplate == 0 & home_hplate == 0) %>% 
  group_by(full_name) %>%
  tally() %>% 
  rename(non_homeplate_goals = "n")

# combine
goal_report <- homeplate_goal_report %>% 
  left_join(non_homeplate_goal_report, by = "full_name") %>% 
  mutate(across(everything(), ~replace_na(.x, 0))) %>% 
  mutate(total_goals = homeplate_goals + non_homeplate_goals) %>% 
  arrange(-total_goals)
