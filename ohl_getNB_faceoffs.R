library(tidyverse)
library(janitor)
library(lubridate)
library(stringr)
library(RJSONIO)
library(jsonlite)
library(googlesheets4)

# pull all ohl games from github
df <- read.csv("https://raw.githubusercontent.com/turkjr19/north_bay/main/ohl_2021_2022_regSeasonGameIDs.csv")
# pull ohl roster names from github
rosters <- read.csv("https://raw.githubusercontent.com/turkjr19/north_bay/main/rosters11-01-21.csv")
# get google sheet and establish where to append too later in code
ss <- gs4_get('https://docs.google.com/spreadsheets/d/1vD_OaxHQ3knkXqQ1ebjMJ5QKFZi7-qiIXff5p5ZvItc/edit?usp=sharing')

# read sheet from googlesheet (nb_shots shared in claircornish google drive)
df2 <- read_sheet("https://docs.google.com/spreadsheets/d/1vD_OaxHQ3knkXqQ1ebjMJ5QKFZi7-qiIXff5p5ZvItc/edit?usp=sharing")

# adjust date to use a filter for scraping
scrape_dates <- today()

# create dataframe that we will use to iterate through to pull json data from events
gameIDs <- df %>% 
  filter(date_played <= scrape_dates) %>% 
  filter(home == "NB" | visitor == "NB") %>% 
  rename(ohl_game_id = "game_id") %>% 
  mutate(opponent = case_when(
    home == "NB" ~ visitor,
    TRUE ~ home
  ))

# previous games with shot data
df2_previous <- df2 %>% 
  select(ohl_game_id) %>% 
  unique()

# update gameIDs dataframe with only games that need shots scraped
gameIDs <- gameIDs %>% 
  anti_join(df2_previous, by = "ohl_game_id")

# logic to check if there were games yesterday
# if there were no games stop and print
# if there were games continue on with lineups and events
if(nrow(gameIDs) == 0){
  stop("all games have been scraped - nothing to scrape")
}

# use this if you want to dig deeper by team name to join
# ohl_teams <- tibble(
#   team_id = c(1,2,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,34),
#   team_name = c("HAM", "KGN", "OSH", "OTT", "PBO", "BAR", "ER", "GUE",
#                 "KIT", "OS", "SUD", "FLNT", "LDN", "SAR", "SSM", "WSR",
#                 "MISS", "NB", "NIA", "SAG")
# )

## now scrape events
# get the number of games to scrape
pb_count <- nrow(gameIDs)

#set random system sleep variable so we don't overload ohl server
tmsleep <- sample(5:10,1)

# set progress bar so we have an idea how long it will take
pb <- txtProgressBar(min = 0, max = pb_count, style = 3)

# create empty dataframe to store data
output <- NULL

# iterative process to read each game id from all games data frame and then run code
for (i in 1:nrow(gameIDs)) {
  game_ID <- (gameIDs[i, 1])
  ohl_game_id <- as.numeric(gameIDs[i,1])
  Sys.sleep(tmsleep)
  faceoffs <- NULL

    str1 <- "https://cluster.leaguestat.com/feed/index.php?feed=gc&key=2976319eb44abe94&client_code=ohl&game_id="
    str2 <- "&lang_code=en&fmt=json&tab=pxpverbose"
    game_url3 <- paste0(str1,game_ID,str2)
    url3 <- game_url3
    
    # Import pxp data from JSON
    # use jsonlite::fromJSON to handle NULL values
    json_data3 <- jsonlite::fromJSON(url3, simplifyDataFrame = TRUE)
  
  # create a tibble of all events
  events <- as_tibble(json_data3[["GC"]][["Pxpverbose"]]) %>%
    mutate(ohl_game_id = ohl_game_id) %>%
    select(ohl_game_id, everything())
  
  # get faceoff data
  faceoffs <- events %>%
    mutate(situation = case_when(
      event == "faceoff" & lag(event == "penalty" & team_id != 19) ~ "pp",
      event == "faceoff" & lag(event == "penalty" & team_id == 19) ~ "pk",
      TRUE ~ "ev"
    )) %>% 
    mutate(situation = case_when(
      event == "faceoff" & lag(event == "penalty", n = 2L) ~ "ev",
      TRUE ~ (as.character(.$situation))
    )) %>% 
    filter(event == "faceoff") %>%
    select(ohl_game_id, event, period, time_formatted, s, x_location, y_location,
           location_id, situation, 
           home_player_id, visitor_player_id, home_win, win_team_id) %>% 
    mutate(win_player_id = case_when(
      home_win == 1 ~ home_player_id,
      TRUE ~ visitor_player_id
    )) %>% 
    mutate(loss_player_id = case_when(
      home_win == 1 ~ visitor_player_id,
      TRUE ~ home_player_id
    ))
  
  output <- bind_rows(faceoffs, output)
  
  
  setTxtProgressBar(pb, i) 
  
}
  
  
  
  # dig in on faceoff data
  x <- output %>%
    mutate(faceoff_id = row_number()) %>%
    select(ohl_game_id, faceoff_id, win_player_id, loss_player_id) %>%
    mutate(win_player_id = as.integer(win_player_id),
           loss_player_id = as.integer(loss_player_id)) %>% 
    pivot_longer(-faceoff_id, names_to = "situation", values_to = "player_id") %>%
    left_join(rosters, by = 'player_id') %>% 
    select(faceoff_id, player_id, situation, full_name, shoots, latest_team_id) %>%
    pivot_wider(names_from  = `situation`,
                values_from = c(player_id, full_name, shoots, latest_team_id)) %>% 
    rename(win = "full_name_win_player_id",
           loss = "full_name_loss_player_id",
           win_shoots = "shoots_win_player_id",
           loss_shoots = "shoots_loss_player_id",
           win_team_id = "latest_team_id_win_player_id",
           loss_team_id = "latest_team_id_loss_player_id",
           ohl_game_id = "player_id_ohl_game_id") %>% 
    select(faceoff_id, ohl_game_id, win_shoots, win, loss, loss_shoots,
           win_team_id, loss_team_id)
  
  # pull out specific data to join with main dataframe x
  y <- output %>%
    #filter(game_id == game_ID) %>% 
    select(location_id, period, situation) %>% 
    mutate(faceoff_id = row_number())
  
  # pull out opponent to join with shot data
  opponent <- gameIDs %>% 
    select(ohl_game_id, date_played, opponent)

  # create final dataframe to save to googlesheets
  faceoff_data <- x %>%
    left_join(y, by = "faceoff_id") %>% 
    select(ohl_game_id, period, location_id, situation, win_shoots:loss_shoots,
           win_team_id, loss_team_id) %>% 
    left_join(opponent, by = "ohl_game_id") %>% 
    select(ohl_game_id, date_played, opponent, everything())
  

# append to google sheet
sheet_append(ss, data = faceoff_data)
  