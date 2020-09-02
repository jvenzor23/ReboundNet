# This code examines the basketball pbp data

# Clean workspace
rm(list=ls())

# Setting Working Directory
setwd("~/Desktop/Deep_Learning/nba-movement-data/data/")

# Calling Necessary Libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)
library(reticulate)

files = dir()
files = files[grep(".7z", files)]
files = files[grep("CLE", files)]
files = files[grep("MIN", files)]
file = files[1]

file.info(files[98])$size >1000

# testing process ---------------------------------------------------------
system(paste0('7z e ', file))
setwd("~/Desktop/Deep_Learning/nba-movement-data/src")
system('ls')
use_python("/anaconda3/bin/python", required = T)
py_run_file('get_json_from_csv.py')

# testing data  -----------------------------------------------------------
setwd("~/Desktop/Deep_Learning/nba-movement-data/rebounding_data/intermediate")
system2('ls')

scripts = dir()
player_info_dir = scripts[grep("player_info", scripts)]
nba_pbp_data = read.csv(setdiff(scripts, player_info_dir))
players_info = read.csv(player_info_dir)

game_id_str = str_split(setdiff(scripts, player_info_dir), ".csv")[[1]][1]
game_id = as.integer(game_id_str)

# deleting old file
setwd("~/Desktop/Deep_Learning/nba-movement-data/data/")
system(paste0('rm ', game_id_str, '.json'))

setwd("~/Desktop/Deep_Learning/nba-movement-data/data/shots/")
# shots_data = read.csv("shots_fixed.csv") %>%
#   filter(GAME_ID == game_id)
setwd("~/Desktop/Deep_Learning/nba-movement-data/data/events/")
events_data = read.csv(setdiff(scripts, player_info_dir))

# Adding In Speed to Plays ----------------------------------
# 1 ft/s = 0.3048 m/s
nba_pbp_data_clean = nba_pbp_data %>%
  select(game_id, quarter, game_clock, player_id, team_id, x_loc, y_loc, radius) %>%
  distinct() %>%
  arrange(game_id, quarter, desc(game_clock), team_id, player_id) %>%
  mutate(tot_game_clock = 12*60*(4-quarter) + game_clock) %>%
  group_by(game_id) %>%
  mutate(time_disc = dense_rank(desc(tot_game_clock))) %>%
  group_by(game_id, quarter, player_id) %>%
  mutate(prev_game_clock = lag(game_clock),
         prev_x_loc = lag(x_loc),
         prev_y_loc = lag(y_loc),
         prev_radius = lag(radius)) %>%
  mutate(x_speed = (x_loc - prev_x_loc)/(prev_game_clock - game_clock)*.3048,
         y_speed = (y_loc - prev_y_loc)/(prev_game_clock - game_clock)*.3048,
         radius_speed = (radius - prev_radius)/(prev_game_clock - game_clock)*.3048) %>%
  group_by(game_id, quarter, player_id) %>%
  mutate(prev_x_speed = lag(x_speed),
         prev_y_speed = lag(y_speed),
         prev_radius_speed = lag(radius_speed)) %>%
  mutate(x_acc = (x_speed - prev_x_speed)/(prev_game_clock - game_clock),
         y_acc = (y_speed - prev_y_speed)/(prev_game_clock - game_clock),
         radius_acc = (radius_speed - prev_radius_speed)/(prev_game_clock - game_clock)) %>%
  select(game_id, quarter, game_clock, player_id, team_id, x_loc, y_loc, 
         x_speed, y_speed, x_acc, y_acc, radius, radius_speed, radius_acc)



# STEP 1: Filtering to all missed, non-blocked shots ----------------------
missed_shots = events_data %>%
  filter(EVENTMSGTYPE == 2) %>%
  select(GAME_ID, EVENTNUM, PLAYER1_TEAM_ID, PLAYER1_ID) %>%
  rename(GAME_EVENT_ID = EVENTNUM,
         PLAYER_ID = PLAYER1_ID,
         TEAM_ID = PLAYER1_TEAM_ID)

blocked_shots = events_data %>%
  filter((grepl('block', str_to_lower(HOMEDESCRIPTION)) & 
            grepl('blk', str_to_lower(HOMEDESCRIPTION))) |
           (grepl('block', str_to_lower(VISITORDESCRIPTION)) & 
              grepl('blk', str_to_lower(VISITORDESCRIPTION)))) %>%
  select(GAME_ID, EVENTNUM)

tipped_or_putback_shots = events_data %>%
  filter(grepl(' tip layup', str_to_lower(HOMEDESCRIPTION))|
           grepl(' tip layup', str_to_lower(VISITORDESCRIPTION))|
           grepl('putback', str_to_lower(HOMEDESCRIPTION))|
           grepl('putback', str_to_lower(VISITORDESCRIPTION))|
           grepl(' dunk ', str_to_lower(HOMEDESCRIPTION))|
           grepl(' dunk ', str_to_lower(VISITORDESCRIPTION))|
           grepl('alley oop', str_to_lower(HOMEDESCRIPTION))|
           grepl('alley oop', str_to_lower(VISITORDESCRIPTION)))

missed_shots_no_blocks = missed_shots %>%
  anti_join(blocked_shots, by = c('GAME_ID', 'GAME_EVENT_ID' = 'EVENTNUM')) %>%
  anti_join(tipped_or_putback_shots, by = c('GAME_ID', 'GAME_EVENT_ID' = 'EVENTNUM')) %>%
  rename(SHOT_TEAM_ID = TEAM_ID,
         SHOT_PLAYER_ID = PLAYER_ID) %>%
  inner_join(events_data %>% select(GAME_ID, EVENTNUM, PCTIMESTRING),
             c('GAME_ID', 'GAME_EVENT_ID' = 'EVENTNUM')) %>%
  rowwise() %>%
  mutate(SHOT_TIME_ROUGH = 60*as.integer(str_split(as.character(PCTIMESTRING), ":")[[1]][1]) +
           as.integer(str_split(as.character(PCTIMESTRING), ":")[[1]][2])) %>%
  select(-PCTIMESTRING)

# STEP 2: ADDING IN REBOUNDER ---------------------------------------------

after_shot_result = missed_shots_no_blocks %>%
  mutate(GAME_EVENT_ID) %>%
  inner_join(events_data %>% mutate(EVENTNUM = EVENTNUM - 1), c('GAME_ID', 'GAME_EVENT_ID' = 'EVENTNUM')) %>%
  filter(EVENTMSGTYPE == 4) %>%
  mutate(REBOUND_TEAM_ID = if_else(is.na(PLAYER1_TEAM_ID), as.integer(PLAYER1_ID), as.integer(PLAYER1_TEAM_ID)),
         REBOUND_PLAYER_ID = if_else(is.na(PLAYER1_TEAM_ID), as.integer(0), as.integer(PLAYER1_ID))) %>%
  select(names(missed_shots_no_blocks), REBOUND_TEAM_ID, REBOUND_PLAYER_ID, PERIOD) %>%
  arrange(GAME_ID, GAME_EVENT_ID) %>%
  select(GAME_ID, GAME_EVENT_ID, PERIOD, SHOT_TIME_ROUGH, everything())


# STEP 3: JOINING TO PLAYER TRACKING DATA ---------------------------------

shot_times = nba_pbp_data_clean %>%
  filter(player_id == -1) %>%
  inner_join(after_shot_result, by = c("game_id" = "GAME_ID",
                                       "quarter" = "PERIOD")) %>%
  mutate(time_diff = game_clock - SHOT_TIME_ROUGH) %>%
  filter(time_diff < 6,
         time_diff > -2) %>%
  rename(ball_x_loc = x_loc,
         ball_y_loc = y_loc) %>%
  inner_join(nba_pbp_data_clean %>%
               select("game_id", "game_clock", "quarter", "player_id","x_loc", "y_loc"), 
             by = c("game_id", "game_clock", "quarter","SHOT_PLAYER_ID" = "player_id")) %>%
  mutate(ball_shooter_diff = sqrt((ball_x_loc - x_loc)^2 + (ball_y_loc - y_loc)^2)) %>%
  arrange(game_id, quarter, GAME_EVENT_ID, desc(game_clock)) %>%
  mutate(above_backboard_flag = radius > 11.5) %>%
  group_by(GAME_EVENT_ID, SHOT_TIME_ROUGH) %>%
  mutate(filter_out = cumsum(above_backboard_flag)) %>%
  filter(filter_out == 0) %>%
  filter(ball_shooter_diff < 3) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(radius_below_3 = radius < 3) %>%
  arrange(game_id, GAME_EVENT_ID, game_clock) %>%
  group_by(game_id, GAME_EVENT_ID, SHOT_TIME_ROUGH) %>%
  mutate(elim_lower = cumsum(radius_below_3)) %>%
  filter(elim_lower == 0) %>%
  arrange(game_id, GAME_EVENT_ID, desc(game_clock)) %>%
  group_by(GAME_EVENT_ID, SHOT_TIME_ROUGH) %>%
  mutate(start_shot_flag = radius_acc == max(radius_acc, na.rm = TRUE)) %>%
  mutate(start_shot_filter = cumsum(start_shot_flag)) %>%
  filter(start_shot_filter == 1) %>%
  filter(ball_shooter_diff == min(ball_shooter_diff)) %>%
  ungroup() %>%
  select(game_id, GAME_EVENT_ID, game_clock, SHOT_TIME_ROUGH, ball_shooter_diff, radius_acc) %>%
  rename(SHOT_TIME = game_clock) %>%
  filter(ball_shooter_diff < 1.5) %>%
  select(-radius_acc)


after_shot_result2 = after_shot_result %>%
  inner_join(shot_times, by = c("GAME_ID" = "game_id",
                                "GAME_EVENT_ID",
                                "SHOT_TIME_ROUGH"))


shot_full_data = after_shot_result2 %>%
  inner_join(nba_pbp_data_clean,
             by = c("GAME_ID" = "game_id", 
                    "PERIOD" = "quarter",
                    "SHOT_TIME" = "game_clock")) %>%
  select(GAME_ID, GAME_EVENT_ID, SHOT_TEAM_ID, SHOT_PLAYER_ID, REBOUND_TEAM_ID, REBOUND_PLAYER_ID,
         team_id, player_id, x_loc, y_loc, x_speed, y_speed, radius, radius_acc)


shot_full_data = shot_full_data %>%
  anti_join(shot_full_data %>% 
              group_by(GAME_ID, GAME_EVENT_ID) %>% 
              summarize(count = n()) %>%
              filter(count > 11))
# STEP 4: FLIPPPING COURT TO ALWAYS BE ON THE SAME SIDE -------------------

shot_full_data2 = shot_full_data %>%
  mutate(IsShooter = SHOT_PLAYER_ID == player_id,
         IsRebounder = REBOUND_PLAYER_ID == player_id,
         IsOnOffense = SHOT_TEAM_ID == team_id,
         IsDefensiveRebound = REBOUND_TEAM_ID != SHOT_TEAM_ID,
         IsBall = player_id == -1)

shooter_data = shot_full_data2 %>%
  filter(IsShooter) %>%
  mutate(flip_flag = if_else(x_loc > 47, 1, 0)) %>%
  select(GAME_ID, GAME_EVENT_ID, flip_flag)

shot_full_data3 = shot_full_data2 %>%
  inner_join(shooter_data) %>%
  mutate(x_loc = if_else(flip_flag == 1, 94 - x_loc, x_loc),
         y_loc = if_else(flip_flag == 1, 50 - y_loc, y_loc),
         x_speed = if_else(flip_flag == 1, -1*x_speed, x_speed),
         y_speed = if_else(flip_flag == 1, -1*y_speed, y_speed))

# Checking Data -----------------------------------------------------------

# USE PLAY 465 AS AN EXAMPLE OF HOW WE ARE ONLY APPROXIMATING:
# THE DATA IS MESSY! HERE, A REBOUND WAS RECORDED FOR THE WRONG PLAYER!
# GAME_ID: 0021500424 PLAY_ID 97 for an example of an air ball (nobody should be credited)
#                     PLAY_ID 28 would be credited to Love
#                     
example.play = shot_full_data3 %>%
  left_join(players_info, by = c("player_id" = "playerid")) %>%
  inner_join(events_data %>%
               mutate(Description = as.factor(paste(as.character(HOMEDESCRIPTION), 
                                                    as.character(VISITORDESCRIPTION)))) %>%
               select(EVENTNUM, Description),
             by = c("GAME_EVENT_ID" = "EVENTNUM")) %>%
  # filter(GAME_EVENT_ID %in% unique(shot_full_data3$GAME_EVENT_ID)[5:8]) %>%
  filter(GAME_EVENT_ID == 44) %>%
  mutate(Description = fct_reorder(Description, GAME_EVENT_ID)) %>%
  arrange(GAME_ID, GAME_EVENT_ID, player_id)

ggplot(data=data.frame(x=1,y=1),aes(x,y))+
  scale_color_manual(values = c("#FDBB30", "#78BE20"), guide = FALSE) +
  scale_fill_manual(values = c("#860038", "#236192"), guide = FALSE) +
  ###outside box:
  geom_path(data=data.frame(x=c(-25,-25,25,25,-25),y=c(-47,0,0,-47,-47))) +
  ###halfcourt line:
  geom_path(data=data.frame(x=c(-25,25),y=c(0,0))) +
  ###halfcourt semicircle:
  geom_path(data=data.frame(x=c(-6000:(-1)/1000,1:6000/1000),y=-c(sqrt(6^2-c(-6000:(-1)/1000,1:6000/1000)^2))),aes(x=x,y=y)) +
  ###solid FT semicircle above FT line
  geom_path(data=data.frame(x=c(-6000:(-1)/1000,1:6000/1000),y=-c(28-sqrt(6^2-c(-6000:(-1)/1000,1:6000/1000)^2))),aes(x=x,y=y)) +
  ###dashed FT semicircle below FT line:
  geom_path(data=data.frame(x=c(-6000:(-1)/1000,1:6000/1000),y=-c(28+sqrt(6^2-c(-6000:(-1)/1000,1:6000/1000)^2))),aes(x=x,y=y),linetype='dashed') +
  ###key:
  geom_path(data=data.frame(x=-c(-8,-8,8,8,-8),y=-c(47,28,28,47,47)))+
  ###box inside the key:
  geom_path(data=data.frame(x=c(-6,-6,6,6,-6),y=-c(47,28,28,47,47)))+
  ###restricted area semicircle:
  geom_path(data=data.frame(x=c(-4000:(-1)/1000,1:4000/1000),y=-c(41.25-sqrt(4^2-c(-4000:(-1)/1000,1:4000/1000)^2))),aes(x=x,y=y))+
  ###rim:
  geom_path(data=data.frame(x=c(-750:(-1)/1000,1:750/1000,750:1/1000,-1:-750/1000),y=-c(c(41.75+sqrt(0.75^2-c(-750:(-1)/1000,1:750/1000)^2)),c(41.75-sqrt(0.75^2-c(750:1/1000,-1:-750/1000)^2)))),aes(x=x,y=y))+
  ###backboard:
  geom_path(data=data.frame(x=c(-3,3),y=-c(43,43)),lineend='butt')+
  ###three-point line:
  geom_path(data=data.frame(x=c(-22,-22,-22000:(-1)/1000,1:22000/1000,22,22),y=-c(47,47-169/12,41.75-sqrt(23.75^2-c(-22000:(-1)/1000,1:22000/1000)^2),47-169/12,47)),aes(x=x,y=y))+
  ###plotting players
  geom_point(data = example.play %>% filter(!IsBall), aes(x = y_loc - 25, y = x_loc - 47,
                                                          fill = IsOnOffense, group = player_id), alpha = 1, size = 6, color = 'black', pch = 21) + 
  geom_text(data = example.play %>% filter(!IsBall), aes(x = y_loc - 25, y = x_loc - 47, label = jersey), colour = "white", size = 3.5) +
  geom_point(data = example.play %>% filter(IsBall), aes(x = y_loc - 25, y = x_loc - 47), alpha = 1, size = 3, fill = 'darkorange1',
             color = 'black', pch = 21) +
  geom_point(data = example.play %>% filter(IsRebounder), aes(x = y_loc - 25, y = x_loc - 47), alpha = 1, size = 6, fill = NA,
             color = 'black', pch=21, stroke = 2) +
  geom_segment(data = example.play %>% 
                 filter(!IsBall),
               aes(x = y_loc - 25, y = x_loc - 47, 
                   xend = y_loc - 25 + y_speed*2, yend = x_loc - 47 + x_speed*2, colour = IsOnOffense), 
               arrow = arrow(length = unit(.2,"cm"))) + 
  ###fix aspect ratio to 1:1
  theme(panel.background = element_rect(fill = 'navajowhite1', colour = 'black'),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text.x = element_text(size = 8)) + 
  coord_fixed()  +
  facet_wrap(~Description)


# Animated Plot -----------------------------------------------------------

team_mapping = events_data %>%
  select(PLAYER1_TEAM_ID, PLAYER1_TEAM_CITY, PLAYER1_TEAM_NICKNAME) %>%
  distinct() %>%
  mutate(teamName = paste(PLAYER1_TEAM_CITY, PLAYER1_TEAM_NICKNAME)) %>%
  filter(PLAYER1_TEAM_ID != 0,
         PLAYER1_TEAM_CITY != "") %>%
  select(PLAYER1_TEAM_ID, teamName)

play.animate = nba_pbp_data_clean %>%
  filter(quarter == 1,
         game_clock - 446.76 > -3,
         game_clock - 446.76 < 4) %>%
  mutate(GAME_EVENT_ID = 44) %>%
  inner_join(events_data %>%
               mutate(Description = as.factor(paste(as.character(HOMEDESCRIPTION), 
                                                    as.character(VISITORDESCRIPTION)))) %>%
               select(EVENTNUM, Description),
             by = c("GAME_EVENT_ID" = "EVENTNUM")) %>%
  mutate(x_loc = 94 - x_loc,
         y_loc = 50 -y_loc) %>%
  mutate(frame.id = dense_rank(desc(game_clock))) %>%
  left_join(players_info, by = c("player_id" = "playerid")) %>%
  inner_join(example.play %>%
               select(player_id, IsOnOffense)) %>%
  mutate(fullname = paste(firstname, lastname),
         IsBall = if_else(player_id == -1, TRUE, FALSE)) %>%
  group_by(game_clock, IsBall, IsOnOffense) %>%
  mutate(yloc_legend = row_number(jersey)*3) %>%
  arrange(desc(game_clock), desc(IsBall), IsOnOffense, jersey) %>%
  mutate(yloc_legend = if_else(IsOnOffense, yloc_legend + 30, yloc_legend + 5)) %>%
  left_join(team_mapping, by = c("team_id" = "PLAYER1_TEAM_ID"))

library(gganimate)
library(cowplot)

title_string = toString((play.animate %>% ungroup() %>% select(Description) %>% distinct())$Description)

animate.play = ggplot(data=data.frame(x=1,y=1),aes(x,y))+
  scale_color_manual(values = c("#A1A1A4", "#1D1160"), guide = FALSE) +
  scale_fill_manual(values = c("#CE1141", "#00788C"), guide = FALSE) +
  geom_polygon(data = data.frame(x = c(-25, -25, 25, 25, -25), y = c(-47, 0, 0, -47, -47)),
               aes(x = x, y = y), fill = "navajowhite1") + 
  ###outside box:
  geom_path(data=data.frame(x=c(-25,-25,25,25,-25),y=c(-47,0,0,-47,-47))) +
  ###halfcourt line:
  geom_path(data=data.frame(x=c(-25,25),y=c(0,0))) +
  ###halfcourt semicircle:
  geom_path(data=data.frame(x=c(-6000:(-1)/1000,1:6000/1000),y=-c(sqrt(6^2-c(-6000:(-1)/1000,1:6000/1000)^2))),aes(x=x,y=y)) +
  ###solid FT semicircle above FT line
  geom_path(data=data.frame(x=c(-6000:(-1)/1000,1:6000/1000),y=-c(28-sqrt(6^2-c(-6000:(-1)/1000,1:6000/1000)^2))),aes(x=x,y=y)) +
  ###dashed FT semicircle below FT line:
  geom_path(data=data.frame(x=c(-6000:(-1)/1000,1:6000/1000),y=-c(28+sqrt(6^2-c(-6000:(-1)/1000,1:6000/1000)^2))),aes(x=x,y=y),linetype='dashed') +
  ###key:
  geom_path(data=data.frame(x=-c(-8,-8,8,8,-8),y=-c(47,28,28,47,47))) +
  ###box inside the key:
  geom_path(data=data.frame(x=c(-6,-6,6,6,-6),y=-c(47,28,28,47,47)))+
  ###restricted area semicircle:
  geom_path(data=data.frame(x=c(-4000:(-1)/1000,1:4000/1000),y=-c(41.25-sqrt(4^2-c(-4000:(-1)/1000,1:4000/1000)^2))),aes(x=x,y=y))+
  ###rim:
  geom_path(data=data.frame(x=c(-750:(-1)/1000,1:750/1000,750:1/1000,-1:-750/1000),y=-c(c(41.75+sqrt(0.75^2-c(-750:(-1)/1000,1:750/1000)^2)),c(41.75-sqrt(0.75^2-c(750:1/1000,-1:-750/1000)^2)))),aes(x=x,y=y))+
  ###backboard:
  geom_path(data=data.frame(x=c(-3,3),y=-c(43,43)),lineend='butt')+
  ###three-point line:
  geom_path(data=data.frame(x=c(-22,-22,-22000:(-1)/1000,1:22000/1000,22,22),y=-c(47,47-169/12,41.75-sqrt(23.75^2-c(-22000:(-1)/1000,1:22000/1000)^2),47-169/12,47)),aes(x=x,y=y))+
  ###plotting players
  geom_point(data = play.animate %>% filter(player_id != -1), aes(x = y_loc - 25, y = x_loc - 47,
                                                                        fill = IsOnOffense, color = IsOnOffense, group = lastname), alpha = 1, size = 8, stroke = 2, pch = 21) + 
  geom_text(data = play.animate %>% filter(player_id != -1), aes(x = y_loc - 25, y = x_loc - 47, label = jersey), colour = "white", size = 3.5) +
  geom_point(data = play.animate %>% filter(player_id == -1), aes(x = y_loc - 25, y = x_loc - 47), alpha = 1, size = 3, fill = 'darkorange1',
             color = 'black', pch = 21) +
  geom_point(data = play.animate %>% filter(!IsBall), aes(x = 30, y = -yloc_legend,
                                                          fill = IsOnOffense, color = IsOnOffense, group = lastname), alpha = 1, size = 8, stroke = 2, pch = 21) +
  geom_text(data = play.animate %>% filter(!IsBall), aes(x = 30, y = -yloc_legend, label = jersey, group = lastname), fontface = "bold", color = "white", size = 4) +
  geom_label(data = play.animate %>% filter(!IsBall), aes(x = 32, y = -yloc_legend, label = fullname, group = lastname), color = "black", fill = "white", size = 4, hjust = 0) +
  geom_label(data = play.animate %>% filter(!IsBall,
                                            !IsOnOffense) %>%
               ungroup() %>%
               select(IsOnOffense, teamName) %>%
               distinct(), aes(x = 28, y = -3, label = teamName, 
                               fill = IsOnOffense), color = "white", size = 8, hjust = 0) +
  geom_label(data = play.animate %>% filter(!IsBall,
                                            IsOnOffense) %>%
               ungroup() %>%
               select(IsOnOffense, teamName) %>%
               distinct(), aes(x = 28, y = -28, label = teamName, 
                               fill = IsOnOffense), color = "white", size = 8, hjust = 0) +
  ###fix aspect ratio to 1:1
  theme(panel.background = element_rect(colour = 'black'),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text.x = element_text(size = 15),
        axis.text=element_blank(),
        axis.ticks=element_blank()
  ) + 
  labs(title = title_string,
       y = "",
       x = "") + 
  xlim(-25, 60) +
  coord_fixed() + 
  transition_time(frame.id)  +
  ease_aes('linear')

## Ensure timing of play matches 10 frames-per-second
play.length.ex <- length(unique(play.animate$frame.id))
animate(animate.play, fps = 25, nframe = play.length.ex, height = 7, width = 10, units = "in", res = 200,
        renderer = file_renderer("~/Desktop/Deep_Learning/nba-movement-data/rebounding_data/images/ReboundExGif", prefix = "gganim_plot", overwrite = TRUE))

animate(animate.play, fps = 25, nframe = play.length.ex)
animate(p, nframes = 24, device = "png",
        renderer = file_renderer("~/Desktop/Deep_Learning/nba-movement-data/rebounding_data/images/KLoveWorst.png", prefix = "gganim_plot", overwrite = TRUE))
