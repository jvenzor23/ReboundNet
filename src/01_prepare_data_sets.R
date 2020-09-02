# This code examines the basketball pbp data

# Clean workspace
rm(list=ls())

# Setting Working Directory
setwd("~/Desktop/Deep_Learning/nba-movement-data/rebounding_data/landing_spots_full_output2/")

# Calling Necessary Libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)
library(reticulate)

files = dir()
files = files[grep("_rebounding_data", files)]
length(unique(files))

data_tot = data.frame()

for(file in files){
  data_tot = rbind(data_tot, 
                   read.csv(file) %>%
                     select(-X))
}

check = data_tot %>%
  group_by(GAME_ID, GAME_EVENT_ID) %>%
  summarize(count = n()) %>%
  filter((count > 11) | (count < 11))

data_tot = data_tot %>%
  anti_join(check)

# getting rid of play with extreme speeds
wrong_speeds = data_tot %>%
  filter((abs(x_speed) > 22)|(abs(y_speed) > 22),
         !IsBall) %>%
  select(GAME_ID, GAME_EVENT_ID) %>%
  distinct()

data_tot = data_tot %>%
  anti_join(wrong_speeds)

# getting rebounders beyond half court
rebounder_error = data_tot %>%
  filter(IsRebounder,
         (x_loc > 47)|(abs(y_loc - 25) > 26)) %>%
  select(GAME_ID, GAME_EVENT_ID) %>%
  distinct()

data_tot = data_tot %>%
  anti_join(rebounder_error)

setwd("~/Desktop/Deep_Learning/nba-movement-data/data/events/")
scripts = dir()
scripts = scripts[grep(".csv", scripts)]

events_data = data.frame()
for(script in scripts){
  x = read.csv(script) %>%
    select(GAME_ID, EVENTNUM, EVENTMSGTYPE, EVENTMSGACTIONTYPE, HOMEDESCRIPTION, VISITORDESCRIPTION)
  if(dim(x)[1] > 0){
    events_data = rbind(events_data,
                        x)
    
  }
}

shots_with_descs = data_tot %>%
  select(GAME_ID, GAME_EVENT_ID) %>%
  distinct() %>%
  left_join(events_data,
            by = c("GAME_ID", "GAME_EVENT_ID" = "EVENTNUM")) %>%
  mutate(Description = paste0(HOMEDESCRIPTION, VISITORDESCRIPTION)) %>%
  select(-HOMEDESCRIPTION, -VISITORDESCRIPTION)

dunks = shots_with_descs %>%
  filter(grepl("dunk", str_to_lower(Description))|
           grepl("alley oop", str_to_lower(Description))|
           grepl(" tip layup ", str_to_lower(Description))|
           grepl(" putback ", str_to_lower(Description))|
           grepl(" block ", str_to_lower(Description)))

data_tot = data_tot %>%
  anti_join(dunks)

nShots = dim(data_tot %>% select(GAME_ID, GAME_EVENT_ID) %>% distinct())[1]


# Conditioning on Shot Distance -------------------------------------------
shot_events = events_data %>%
  inner_join(data_tot %>%
               filter((SHOT_PLAYER_ID == player_id)|
                        IsBall) %>%
               select(GAME_ID, GAME_EVENT_ID, x_loc, y_loc) %>%
               distinct(),
             by = c("GAME_ID", "EVENTNUM" = "GAME_EVENT_ID")) %>%
  select(GAME_ID, EVENTNUM, VISITORDESCRIPTION, HOMEDESCRIPTION, x_loc, y_loc) %>%
  mutate(DESCRIPTION = paste0(VISITORDESCRIPTION, HOMEDESCRIPTION)) %>%
  select(GAME_ID, EVENTNUM, DESCRIPTION, x_loc, y_loc) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(desc2 = str_split(DESCRIPTION, "'")[[1]][max(length(str_split(DESCRIPTION, "'")[[1]]) - 1, 1)],
         dist = sqrt((x_loc - 5.25)^2 + (y_loc - 25)^2),
         desc3 = case_when(grepl("3PT", desc2) ~ 23,
                           grepl("Layup", desc2) ~ 2,
                           grepl("Hook", desc2) ~ 5,
                           TRUE ~ as.numeric(str_split(desc2, " ")[[1]][length(str_split(desc2, " ")[[1]])])))

shot_doesnt_match_desc = shot_events %>%
  filter(!is.na(desc3) & 
           ((desc3 > 47)|
          ((dist <= 10) & (desc3 >= 20))|
           ((dist >= 20) & (desc3 <= 10)))) %>%
  distinct()

shot_doesnt_match_desc_join = shot_doesnt_match_desc %>%
  select(GAME_ID, EVENTNUM) %>%
  rename(GAME_EVENT_ID = EVENTNUM) %>%
  distinct()

data_tot = data_tot %>%
  anti_join(shot_doesnt_match_desc_join)

nShots = dim(data_tot %>% select(GAME_ID, GAME_EVENT_ID) %>% distinct())[1]


# Reading in Landing Data
setwd("~/Desktop/Deep_Learning/nba-movement-data/rebounding_data/landing_spots_full_output2/")

files = dir()
files = files[grep("_landing_data", files)]
length(unique(files))

landing_data_tot = data.frame()

for(file in files){
  landing_data_tot = rbind(landing_data_tot, 
                           read.csv(file) %>%
                             select(-X))
}

landing_data_tot2 =  landing_data_tot %>%
          filter(x_hit < 40,
                 y_hit > 0,
                 y_hit < 50,
                 x_hit > -3)

# Adding the Landing Data -------------------------------------------------

data_tot = data_tot %>%
  inner_join(landing_data_tot2 %>% select(-quarter))

data_tot$x_hit[is.na(data_tot$x_hit)] = 5.25
data_tot$y_hit[is.na(data_tot$y_hit)] = 25

# Sampling to Get Training, Validation, Test ------------------------------

data_tot_holdout = data_tot %>%
  select(GAME_ID, GAME_EVENT_ID) %>%
  distinct() %>%
  sample_frac(0.3)

data_tot_validation = data_tot_holdout %>%
  sample_frac(0.5)

data_tot_test = data_tot_holdout %>%
  anti_join(data_tot_validation)


# Adding in Extra Plays By Flipping ---------------------------------------
data_tot_flipped = data_tot %>%
  anti_join(data_tot_holdout) %>%
  mutate(y_loc = 50 - y_loc,
         y_speed = -y_speed,
         y_hit = 50 - y_hit,
         GAME_ID = 100000000 + GAME_ID)

data_tot = rbind(data_tot,
                 data_tot_flipped)

data_tot = data_tot %>%
  left_join(rbind(data_tot_test %>%
                    mutate(group = "test"),
                  data_tot_validation %>%
                    mutate(group = "validation"))) %>%
  mutate(group = replace_na(group, "training"))

# Transforming the Data ---------------------------------------------------

# Formatting for Neural Network -------------------------------------------

# FOR ALL 10 PLAYERS (INC. SHOOTER!) ---------------
# (x,y) - (x,y)
# S(x,y) - S(x, y)
# (x,y) - Shooter (x,y)
# (x,y) - Basket (x,y)
# (x,y) - Landing (x,y)
# team interaction flag (0 if same team, 1 otherwise)
# Position (G/F/C ---> 0/1/2)
# ------------------------------------------------------------------------
#  12 variables per slot, 10x10 matrix ------> 1,200 variables per play!
# ------------------------------------------------------------------------

nueral_net_shooter = data_tot %>%
  filter(IsShooter) %>%
  rename(shooter_id = player_id,
         shooter_team_id = team_id,
         shooter_x_loc = x_loc,
         shooter_y_loc = y_loc,
         shooter_x_speed = x_speed,
         shooter_y_speed = y_speed,
         shooterIsRebounder = IsRebounder) %>%
  arrange(GAME_ID, GAME_EVENT_ID) %>%
  select(GAME_ID, GAME_EVENT_ID, shooter_x_loc, shooter_y_loc,
         shooterIsRebounder)

nueral_net_not_ball = data_tot %>%
  filter(!IsBall)

neural_net1 = nueral_net_not_ball %>%
  rename(player_id1 = player_id,
         team_id1 = team_id,
         x_loc1 = x_loc,
         y_loc1 = y_loc,
         x_speed1 = x_speed,
         y_speed1 = y_speed) %>%
  inner_join(nueral_net_not_ball %>%
               rename(player_id2 = player_id,
                      team_id2 = team_id,
                      x_loc2 = x_loc,
                      y_loc2 = y_loc,
                      x_speed2 = x_speed,
                      y_speed2 = y_speed,
                      IsOnOffense2 = IsOnOffense,
                      IsShooter2 = IsShooter) %>%
               select(GAME_ID, GAME_EVENT_ID, 
                      player_id2, team_id2, x_loc2, y_loc2,
                      x_speed2, y_speed2, IsOnOffense2, IsShooter2)) %>%
  mutate(x_diff = x_loc1 - x_loc2,
         y_diff = y_loc1 - y_loc2,
         x_speed_diff = x_speed1 - x_speed2,
         y_speed_diff = y_speed1 - y_speed2,
         teammate_flag = if_else(team_id1 == team_id2, 0, 1)
  ) %>%
  select(GAME_ID, GAME_EVENT_ID, team_id1, team_id2, 
         player_id1, player_id2, x_loc1, y_loc1, x_diff, y_diff, x_speed_diff,
         y_speed_diff, teammate_flag, IsRebounder, IsDefensiveRebound, 
         IsShooter, IsShooter2, IsOnOffense, IsOnOffense2, x_hit, y_hit, t_after_shot, x_speed1, y_speed1, group)

neural_net2 = neural_net1 %>%
  inner_join(nueral_net_shooter) %>%
  mutate(shooter_x_diff = x_loc1 - shooter_x_loc,
         shooter_y_diff = y_loc1 - shooter_y_loc) %>%
  select(-shooter_x_loc, -shooter_y_loc)

neural_net3 = neural_net2 %>%
  mutate(basket_x_diff = x_loc1 - 5.25,
         basket_y_diff = y_loc1 - 25,
         landing_x_diff = x_loc1 - x_hit,
         landing_y_diff = y_loc1 - y_hit) %>%
  select(group, GAME_ID, GAME_EVENT_ID, team_id1, team_id2, 
         player_id1, player_id2, x_diff, y_diff, x_speed_diff,
         y_speed_diff, shooter_x_diff, shooter_y_diff, 
         basket_x_diff, basket_y_diff, landing_x_diff, landing_y_diff,
         teammate_flag, t_after_shot, x_speed1, y_speed1, IsRebounder, shooterIsRebounder, IsDefensiveRebound,
         IsShooter, IsShooter2, IsOnOffense, IsOnOffense2) %>%
  arrange(group, GAME_ID, GAME_EVENT_ID, IsOnOffense, IsShooter, player_id1,
          IsOnOffense2, IsShooter2, player_id2)

neural_net3_reorder = neural_net3 %>%
  filter(group == "training",
         GAME_ID < 100000000) %>%
  mutate(GAME_ID = 200000000 + GAME_ID) %>%
  group_by(group, GAME_ID, GAME_EVENT_ID) %>%
  group_by(group, GAME_ID, GAME_EVENT_ID, IsOnOffense, IsShooter) %>%
  mutate(player_num1 = dense_rank(desc(player_id1))) %>%
  group_by(group, GAME_ID, GAME_EVENT_ID, IsOnOffense2, IsShooter2) %>%
  mutate(player_num2 = dense_rank(desc(player_id2))) %>%
  ungroup() %>%
  mutate(player_num1 = case_when(IsShooter ~ as.integer(10),
                                 IsOnOffense ~ player_num1 + as.integer(5),
                                 TRUE ~ as.integer(player_num1)),
         player_num2 = case_when(IsShooter2 ~ as.integer(10),
                                 IsOnOffense2 ~ player_num2 + as.integer(5),
                                 TRUE ~ as.integer(player_num2))) %>%
  arrange(group, GAME_ID, GAME_EVENT_ID, player_num1, player_num2)

neural_net_train = rbind(neural_net3 %>%
  select(-IsRebounder, -shooterIsRebounder, -IsDefensiveRebound) %>%
  group_by(group, GAME_ID, GAME_EVENT_ID, IsOnOffense, IsShooter) %>%
  mutate(player_num1 = dense_rank(player_id1)) %>%
  group_by(group, GAME_ID, GAME_EVENT_ID, IsOnOffense2, IsShooter2) %>%
  mutate(player_num2 = dense_rank(player_id2)) %>%
  ungroup() %>%
  mutate(player_num1 = case_when(IsShooter ~ as.integer(10),
                                 IsOnOffense ~ player_num1 + as.integer(5),
                                 TRUE ~ as.integer(player_num1)),
         player_num2 = case_when(IsShooter2 ~ as.integer(10),
                                 IsOnOffense2 ~ player_num2 + as.integer(5),
                                 TRUE ~ as.integer(player_num2))),
    neural_net3_reorder %>%
    select(-IsRebounder, -shooterIsRebounder, -IsDefensiveRebound)) %>%
  ungroup() %>%
  mutate(play_id = case_when(nchar(as.character(GAME_EVENT_ID)) == 1 ~ 
                               as.numeric(paste0(as.character(GAME_ID), "000", as.character(GAME_EVENT_ID))),
                             nchar(as.character(GAME_EVENT_ID)) == 2 ~ 
                               as.numeric(paste0(as.character(GAME_ID), "00", as.character(GAME_EVENT_ID))),
                             nchar(as.character(GAME_EVENT_ID)) == 3 ~ 
                               as.numeric(paste0(as.character(GAME_ID), "0", as.character(GAME_EVENT_ID))),
                             TRUE ~ as.numeric(paste0(as.character(GAME_ID), as.character(GAME_EVENT_ID))))) %>%
  arrange(group, GAME_ID, GAME_EVENT_ID, player_num1, player_num2) %>%
  group_by(group) %>%
  mutate(play_num = dense_rank(play_id)) %>%
  ungroup() %>%
  select(group, GAME_ID, GAME_EVENT_ID, play_id, play_num, player_num1, player_num2, everything()) %>%
  select(-starts_with("team_"), -IsShooter, -IsShooter2, -IsOnOffense, -IsOnOffense2)

# players
setwd("~/Desktop/Deep_Learning/nba-movement-data/rebounding_data/landing_spots_full_output2/")
scripts = dir()
scripts = scripts[grep("_player", scripts)]

players_info = data.frame()
for(script in scripts){
  players_info = rbind(players_info,
                       read.csv(script)) %>%
    distinct()
}

players_info = players_info %>%
  select(-gameId, -X, -jersey) %>%
  distinct() %>%
  group_by(firstname, lastname, playerid) %>%
  summarize(position = position[1]) %>%
  mutate(position_num = case_when(grepl('C', position) ~ 2,
                                  grepl('F', position) ~ 1,
                                  TRUE ~ 0)) %>%
  ungroup() %>%
  select(playerid, position_num)


neural_net_train = neural_net_train %>%
  left_join(players_info, by = c("player_id1" = "playerid")) %>%
  left_join(players_info %>%
              rename(position_num2 = position_num),
            by = c("player_id2" = "playerid"))

neural_net_train$position_num[is.na(neural_net_train$position_num)] = 0
neural_net_train$position_num[is.na(neural_net_train$position_num2)] = 0

neural_net_train = neural_net_train %>%
  mutate(position_diff = position_num - position_num2) %>%
  select(-position_num2)

# 1-9 represents ordered player who got rebound. 10 means shooter got the rebound.
# 11 means DEFENSE TEAM rebound. 12 means OFFENSE TEAM rebound. 
neural_net_target = rbind(neural_net3 %>%
  group_by(group, GAME_ID, GAME_EVENT_ID, IsOnOffense, IsShooter) %>%
  mutate(player_num1 = dense_rank(player_id1)) %>%
  group_by(group, GAME_ID, GAME_EVENT_ID, IsOnOffense2, IsShooter2) %>%
  mutate(player_num2 = dense_rank(player_id2)) %>%
  ungroup() %>%
  mutate(player_num1 = case_when(IsShooter ~ as.integer(10),
                                 IsOnOffense ~ player_num1 + as.integer(5),
                                 TRUE ~ as.integer(player_num1)),
         player_num2 = case_when(IsShooter2 ~ as.integer(10),
                                 IsOnOffense2 ~ player_num2 + as.integer(5),
                                 TRUE ~ as.integer(player_num2))),
  neural_net3_reorder) %>%
  ungroup() %>%
  arrange(group, GAME_ID, GAME_EVENT_ID, player_num1, player_num2) %>%
  group_by(group, GAME_ID, GAME_EVENT_ID) %>%
  summarize(player_rebound = player_num1[which.max(IsRebounder)],
            player_rebound_flag = sum(IsRebounder),
            shooter_rebound_flag = as.integer(max(shooterIsRebounder)),
            defensive_team_rebound = as.integer(max(IsDefensiveRebound))) %>%
  mutate(target = case_when(player_rebound_flag > 0 ~ player_rebound,
                            shooter_rebound_flag == 1 ~ as.integer(10),
                            defensive_team_rebound == 1 ~ as.integer(11),
                            TRUE ~ as.integer(12))) %>%
  select(group, GAME_ID, GAME_EVENT_ID, target) %>%
  arrange(group, GAME_ID, GAME_EVENT_ID)

check = neural_net_target %>%
  group_by(target) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

data_score = neural_net_train %>%
  filter(GAME_ID < 100000000) %>%
  mutate(play_num = dense_rank(play_id)) %>%
  arrange(play_num, player_num1, player_num2)

setwd("~/Desktop/Deep_Learning/nba-movement-data/rebounding_data/data_sets/")
write.csv(neural_net_train, "landing_isotropic_train_augmented.csv")
write.csv(neural_net_target, "landing_isotropic_target_augmented.csv")
write.csv(data_score, "landing_isotropic_score_augmented.csv")

# Removing Plays With No Rebounder ----------------------------------------
neural_net_target2 =  neural_net_target%>%
  filter(target <= 10,
         GAME_ID < 200000000) %>%
  arrange(group, GAME_ID, GAME_EVENT_ID)


neural_net_train2 = neural_net_train %>%
  inner_join(neural_net_target2 %>% select(group, GAME_ID, GAME_EVENT_ID)) %>%
  arrange(group, GAME_ID, GAME_EVENT_ID)

data_score2 = data_score %>%
  inner_join(neural_net_target2 %>% select(group, GAME_ID, GAME_EVENT_ID))

nShots2 = dim(data_score2 %>% select(GAME_ID, GAME_EVENT_ID) %>%
              filter(GAME_ID < 100000000) %>% distinct())[1]

check2 = neural_net_target2 %>%
  group_by(group) %>%
  summarize(count = n())

setwd("~/Desktop/Deep_Learning/nba-movement-data/rebounding_data/data_sets/")
write.csv(neural_net_train2, "landing_isotropic_train_augmented2.csv")
write.csv(neural_net_target2, "landing_isotropic_target_augmented2.csv")
write.csv(data_score2, "landing_isotropic_score_augmented2.csv")
