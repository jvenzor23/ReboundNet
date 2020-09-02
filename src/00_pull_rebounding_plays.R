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
# files = files[216:length(files)]

for(file in files){
  # ------------------------ ZIP --> JSON --> CSV ----------------------------
  # clearing the workspace (except for the looping variables)
  rm(list=ls()[! ls() %in% c("file", "files")])
  
  setwd("~/Desktop/Deep_Learning/nba-movement-data/data/")
  if(file.info(file)$size >5000){
    
    # ZIP ---> JSON
    setwd("~/Desktop/Deep_Learning/nba-movement-data/data/")
    system(paste0('7z e ', file))
    
    # JSON ---> CSV
    setwd("~/Desktop/Deep_Learning/nba-movement-data/src")
    use_python("/anaconda3/bin/python", required = T)
    py_run_file('get_json_from_csv.py')
    
    setwd("~/Desktop/Deep_Learning/nba-movement-data/rebounding_data/intermediate")

    scripts = dir()
    player_info_dir = scripts[grep("player_info", scripts)]
    nba_pbp_data = read.csv(setdiff(scripts, player_info_dir))
    players_info = read.csv(player_info_dir)
    
    game_id_str = str_split(setdiff(scripts, player_info_dir), ".csv")[[1]][1]
    game_id = as.integer(game_id_str)
    
    # deleting JSON file
    setwd("~/Desktop/Deep_Learning/nba-movement-data/data/")
    system(paste0('rm ', game_id_str, '.json'))
    
    # ------- Reading in Intermediate Data ----------------------
    
    # setwd("~/Desktop/Deep_Learning/nba-movement-data/data/shots/")
    # shots_data = read.csv("shots_fixed.csv") %>%
    #   filter(GAME_ID == game_id)
    
    setwd("~/Desktop/Deep_Learning/nba-movement-data/data/events/")
    events_data = read.csv(setdiff(scripts, player_info_dir))
    
    # if(dim(shots_data)[1] > 0){
    if(dim(events_data)[1] > 0){
    
      
      # ------ DOING THE ANALYSIS ---------------------------------
      # Adding In Speed to Plays ----------------------------------
      # 1 ft/s = 0.3048 m/s
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
      
      qa_check = nba_pbp_data_clean %>%
        group_by(quarter, game_clock) %>%
        summarize(count = n()) %>%
        filter(count > 11) %>%
        ungroup() %>%
        arrange(quarter, desc(game_clock)) %>%
        rename(bad_game_clock = game_clock)
      
      qa_check2 = nba_pbp_data_clean %>%
        ungroup() %>%
        select(quarter, game_clock) %>%
        distinct() %>%
        group_by(quarter) %>%
        mutate(time_lag = lag(game_clock) - game_clock) %>%
        filter(!is.na(time_lag),
               time_lag > 5) %>%
        rename(bad_game_clock = game_clock)
      
      
      
      # STEP 1: Filtering to all missed, non-blocked shots ----------------------
      # missed_shots = shots_data %>%
      #   filter(SHOT_MADE_FLAG == 0,
      #          SHOT_DISTANCE < 94/2) %>%
      #   select(GAME_ID, TEAM_ID, PLAYER_ID, GAME_EVENT_ID, LOC_X, LOC_Y)
      
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
        select(GAME_ID, GAME_EVENT_ID, PERIOD, SHOT_TIME_ROUGH, everything()) %>%
        filter(SHOT_TIME_ROUGH > 14)
      
      
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
      
      events_clean = events_data %>%
        select(GAME_ID, EVENTNUM, PERIOD) %>%
        distinct() %>%
        rename(quarter = PERIOD,
               GAME_EVENT_ID = EVENTNUM)
      
      shots_remove = shot_times %>%
        inner_join(events_clean, by = c("game_id" = "GAME_ID",
                                        "GAME_EVENT_ID")) %>%
        inner_join(qa_check) %>%
        inner_join(qa_check2, by = "quarter") %>%
        filter((abs(SHOT_TIME - bad_game_clock.x) < 1 )|
                 (abs(SHOT_TIME - bad_game_clock.y) < 1 )) %>%
        select(game_id, GAME_EVENT_ID) %>%
        distinct()
      
      shot_times = shot_times %>%
        anti_join(shots_remove)
      
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
      
      # ensuring we don't blow up the data due to an error in GAME_EVENT_ID
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
      
      # Identifying Landing Spot ------------------------------------------------
      
      shot_info_join = shot_full_data3 %>%
        group_by(GAME_ID, GAME_EVENT_ID) %>%
        mutate(hasRebound = max(IsRebounder)) %>%
        filter(hasRebound == 1) %>%
        filter((player_id == -1)|(player_id == REBOUND_PLAYER_ID)) %>%
        select(GAME_ID, GAME_EVENT_ID, player_id, flip_flag) %>%
        inner_join(shot_times,
                   by = c("GAME_ID" = "game_id", "GAME_EVENT_ID")) %>%
        select(-SHOT_TIME_ROUGH, -ball_shooter_diff) %>%
        inner_join(nba_pbp_data_clean,
                   by = c("GAME_ID" = "game_id",
                          "player_id")) %>%
        arrange(desc(GAME_ID, GAME_EVENT_ID, player_id, game_clock)) %>%
        filter(game_clock >= SHOT_TIME -3,
               game_clock <= SHOT_TIME) %>%
        mutate(x_loc = if_else(flip_flag == 1, 94 - x_loc, x_loc),
               y_loc = if_else(flip_flag == 1, 50 - y_loc, y_loc))
      
      # events_clean = events_data %>%
      #   select(GAME_ID, EVENTNUM, PERIOD) %>%
      #   distinct() %>%
      #   rename(quarter = PERIOD,
      #          GAME_EVENT_ID = EVENTNUM)
      
      shot_info_join = shot_info_join %>%
        inner_join(events_clean)
      
      test_shot = shot_info_join %>%
        group_by(GAME_ID, GAME_EVENT_ID, quarter) %>%
        mutate(frame.id = dense_rank(desc(game_clock))) %>%
        group_by(GAME_ID, GAME_EVENT_ID, quarter, game_clock) %>%
        mutate(dist = sqrt((x_loc[1] - x_loc[2])^2 + (y_loc[1] - y_loc[2])^2))
      
      test_shot2 = test_shot %>%
        filter(player_id == -1) %>%
        ungroup() %>%
        filter(frame.id %% 3 == 1) %>%
        group_by(GAME_ID, GAME_EVENT_ID, quarter, player_id) %>%
        mutate(prev_game_clock = lag(game_clock),
               prev_x_loc = lag(x_loc),
               prev_y_loc = lag(y_loc),
               prev_radius = lag(radius)) %>%
        mutate(x_speed = (x_loc - prev_x_loc)/(prev_game_clock - game_clock)*.3048,
               y_speed = (y_loc - prev_y_loc)/(prev_game_clock - game_clock)*.3048,
               radius_speed = (radius - prev_radius)/(prev_game_clock - game_clock)*.3048) %>%
        group_by(GAME_ID, GAME_EVENT_ID, quarter, player_id) %>%
        mutate(prev_x_speed = lag(x_speed),
               prev_y_speed = lag(y_speed),
               prev_radius_speed = lag(radius_speed)) %>%
        mutate(x_acc = (x_speed - prev_x_speed)/(prev_game_clock - game_clock),
               y_acc = (y_speed - prev_y_speed)/(prev_game_clock - game_clock),
               radius_acc = (radius_speed - prev_radius_speed)/(prev_game_clock - game_clock)) %>%
        select(GAME_ID, GAME_EVENT_ID, quarter, game_clock, player_id, team_id, x_loc, y_loc, 
               x_speed, y_speed, x_acc, y_acc, radius, radius_speed, radius_acc, dist, SHOT_TIME) %>%
        arrange(GAME_ID, GAME_EVENT_ID, player_id, quarter, desc(game_clock)) %>%
        mutate(dist_from_hoop1 = sqrt((x_loc - 5.25)^2 + (y_loc - 25)^2),
               dist_from_hoop2 = sqrt((x_loc - 94 - 5.25)^2 + (y_loc - 25)^2)) %>%
        rowwise() %>%
        mutate(dist_from_hoop = min(dist_from_hoop1, dist_from_hoop2),
               rebound_starting = dist < 1.5) %>%
        group_by(GAME_ID, GAME_EVENT_ID, SHOT_TIME, quarter, player_id) %>%
        mutate(shot_descent_starting = if_else(!is.na(lag(radius_speed)),
                                               (radius_speed < 0) & (lag(radius_speed) > 0) & (radius >= 10),
                                               (radius_speed < 0) & (lag(radius) > radius) & (radius >= 10)),
               hitting_hoop = (dist_from_hoop < lag(dist_from_hoop)) & (dist_from_hoop < lead(dist_from_hoop)) & (dist_from_hoop < 6)) %>%
        # mutate(first_filter = cumsum(shot_descent_starting)) %>%
        # filter(first_filter > 0) %>%
        # group_by(GAME_ID, GAME_EVENT_ID, SHOT_TIME, quarter, player_id)
        mutate(second_filter = cumsum(replace_na(hitting_hoop, 0))) %>%
        filter(second_filter > 0) %>%
        group_by(GAME_ID, GAME_EVENT_ID, SHOT_TIME, quarter, player_id) %>%  
        mutate(dist_decreasing = lead(dist) < dist) %>%
        mutate(third_filter = cumsum(rebound_starting & !dist_decreasing)) %>%
        filter(third_filter == 0,
               !hitting_hoop) %>%
        group_by(GAME_ID, GAME_EVENT_ID) %>%
        mutate(frame.rk = dense_rank(desc(game_clock))) %>%
        rowwise() %>%
        mutate(remove_flag = (frame.rk > 2) & ((x_acc > 20)|(y_acc > 20))) %>%
        group_by(GAME_ID, GAME_EVENT_ID) %>%
        mutate(remove_sum = cumsum(remove_flag)) %>%
        filter(remove_sum == 0)
      
      test_shot2_speed_check = test_shot2 %>%
        filter((x_speed > 100)|(y_speed > 100)|(is.infinite(x_speed))|(is.infinite(y_speed))) %>%
        select(GAME_ID, GAME_EVENT_ID) %>%
        distinct()
      
      test_shot2 = test_shot2 %>%
        anti_join(test_shot2_speed_check)
      
      test_shot3 = test_shot2 %>%
        group_by(GAME_ID, GAME_EVENT_ID, SHOT_TIME, quarter) %>%
        mutate(x_speed_avg = if_else(length(x_speed) > 1, 
                                     (x_loc[length(x_loc)] - x_loc[1])/(game_clock[1] - game_clock[length(game_clock)]),
                                     x_speed[1]),
               y_speed_avg = if_else(length(y_speed) > 1, 
                                     (y_loc[length(y_loc)] - y_loc[1])/(game_clock[1] - game_clock[length(game_clock)]),
                                     y_speed[1])) %>%
        ungroup() %>%
        rowwise() %>%
        mutate(t_hit = game_clock - Re(polyroot(c(radius, radius_speed, -.5*32.174)))[Re(polyroot(c(radius, radius_speed, -.5*32.174))) > 0]) %>%
        group_by(GAME_ID, GAME_EVENT_ID, SHOT_TIME, quarter) %>%
        mutate(t_hit_avg = mean(t_hit)) %>%
        group_by(GAME_ID, GAME_EVENT_ID, quarter) %>%
        summarize(t_after_shot = mean(SHOT_TIME) - mean(t_hit_avg),
                  x_hit = x_loc[1] + (game_clock[1] - mean(t_hit_avg))*mean(x_speed_avg),
                  y_hit = y_loc[1] + (game_clock[1] - mean(t_hit_avg))*mean(y_speed_avg))
      
      
      # Looking at Shots with Team Rebound --------------------------------------
      
      TEAM_shot_info_join = shot_full_data3 %>%
        group_by(GAME_ID, GAME_EVENT_ID) %>%
        mutate(hasRebound = max(IsRebounder)) %>%
        filter(hasRebound == 0) %>%
        filter((player_id == -1)) %>%
        select(GAME_ID, GAME_EVENT_ID, player_id, flip_flag) %>%
        inner_join(shot_times,
                   by = c("GAME_ID" = "game_id", "GAME_EVENT_ID")) %>%
        select(-SHOT_TIME_ROUGH, -ball_shooter_diff) %>%
        inner_join(nba_pbp_data_clean,
                   by = c("GAME_ID" = "game_id",
                          "player_id")) %>%
        arrange(desc(GAME_ID, GAME_EVENT_ID, player_id, game_clock)) %>%
        filter(game_clock >= SHOT_TIME -3,
               game_clock <= SHOT_TIME) %>%
        mutate(x_loc = if_else(flip_flag == 1, 94 - x_loc, x_loc),
               y_loc = if_else(flip_flag == 1, 50 - y_loc, y_loc))
      
      if(dim(TEAM_shot_info_join)[1] > 0){
      
      TEAM_shot_info_join = TEAM_shot_info_join %>%
        inner_join(events_clean)
      
      TEAM_test_shot = TEAM_shot_info_join %>%
        group_by(GAME_ID, GAME_EVENT_ID, quarter) %>%
        mutate(frame.id = dense_rank(desc(game_clock)))
      
      
      TEAM_test_shot2 = TEAM_test_shot %>%
        filter(player_id == -1) %>%
        ungroup() %>%
        filter(frame.id %% 3 == 1) %>%
        group_by(GAME_ID, GAME_EVENT_ID, quarter, player_id) %>%
        mutate(prev_game_clock = lag(game_clock),
               prev_x_loc = lag(x_loc),
               prev_y_loc = lag(y_loc),
               prev_radius = lag(radius)) %>%
        mutate(x_speed = (x_loc - prev_x_loc)/(prev_game_clock - game_clock)*.3048,
               y_speed = (y_loc - prev_y_loc)/(prev_game_clock - game_clock)*.3048,
               radius_speed = (radius - prev_radius)/(prev_game_clock - game_clock)*.3048) %>%
        group_by(GAME_ID, GAME_EVENT_ID, quarter, player_id) %>%
        mutate(prev_x_speed = lag(x_speed),
               prev_y_speed = lag(y_speed),
               prev_radius_speed = lag(radius_speed)) %>%
        mutate(x_acc = (x_speed - prev_x_speed)/(prev_game_clock - game_clock),
               y_acc = (y_speed - prev_y_speed)/(prev_game_clock - game_clock),
               radius_acc = (radius_speed - prev_radius_speed)/(prev_game_clock - game_clock)) %>%
        select(GAME_ID, GAME_EVENT_ID, quarter, game_clock, player_id, team_id, x_loc, y_loc, 
               x_speed, y_speed, x_acc, y_acc, radius, radius_speed, radius_acc, SHOT_TIME) %>%
        arrange(GAME_ID, GAME_EVENT_ID, player_id, quarter, desc(game_clock)) %>%
        mutate(dist_from_hoop1 = sqrt((x_loc - 5.25)^2 + (y_loc - 25)^2),
               dist_from_hoop2 = sqrt((x_loc - 94 - 5.25)^2 + (y_loc - 25)^2)) %>%
        rowwise() %>%
        mutate(dist_from_hoop = min(dist_from_hoop1, dist_from_hoop2)) %>%
        group_by(GAME_ID, GAME_EVENT_ID, SHOT_TIME, quarter, player_id) %>%
        mutate(shot_descent_starting = if_else(!is.na(lag(radius_speed)),
                                               (radius_speed < 0) & (lag(radius_speed) > 0) & (radius >= 10),
                                               (radius_speed < 0) & (lag(radius) > radius) & (radius >= 10)),
               hitting_hoop = (dist_from_hoop < lag(dist_from_hoop)) & (dist_from_hoop < lead(dist_from_hoop)) & (dist_from_hoop < 6)) %>%
        # mutate(first_filter = cumsum(shot_descent_starting)) %>%
        # filter(first_filter > 0) %>%
        # group_by(GAME_ID, GAME_EVENT_ID, SHOT_TIME, quarter, player_id)
        mutate(second_filter = cumsum(replace_na(hitting_hoop, 0)),
               actually_hitting_hoop = min(dist_from_hoop) < 6) %>%
        filter(second_filter > 0,
               !hitting_hoop) %>%
        group_by(GAME_ID, GAME_EVENT_ID, SHOT_TIME, quarter, player_id) %>% 
        mutate(frames_after_hit = row_number(desc(game_clock))) %>%
        filter(frames_after_hit <= 3)
      
      TEAM_test_shot2_speed_check = TEAM_test_shot2 %>%
        ungroup() %>%
        filter((x_speed > 100)|(y_speed > 100)|(is.infinite(x_speed))|(is.infinite(y_speed))) %>%
        select(GAME_ID, GAME_EVENT_ID) %>%
        distinct()
      
      TEAM_test_shot2 = TEAM_test_shot2 %>%
        anti_join(TEAM_test_shot2_speed_check)
      
      if(dim(TEAM_test_shot2)[1] > 0){
      
        TEAM_test_shot3 = TEAM_test_shot2 %>%
          group_by(GAME_ID, GAME_EVENT_ID, SHOT_TIME, quarter) %>%
          mutate(x_speed_avg = if_else(length(x_speed) > 1, 
                                       (x_loc[length(x_loc)] - x_loc[1])/(game_clock[1] - game_clock[length(game_clock)]),
                                       x_speed[1]),
                 y_speed_avg = if_else(length(y_speed) > 1, 
                                       (y_loc[length(y_loc)] - y_loc[1])/(game_clock[1] - game_clock[length(game_clock)]),
                                       y_speed[1])) %>%
          ungroup() %>%
          rowwise() %>%
          mutate(t_hit = game_clock - Re(polyroot(c(radius, radius_speed, -.5*32.174)))[Re(polyroot(c(radius, radius_speed, -.5*32.174))) > 0]) %>%
          group_by(GAME_ID, GAME_EVENT_ID, SHOT_TIME, quarter) %>%
          mutate(t_hit_avg = mean(t_hit)) %>%
          group_by(GAME_ID, GAME_EVENT_ID, quarter) %>%
          summarize(t_after_shot = mean(SHOT_TIME) - mean(t_hit_avg),
                    x_hit = x_loc[1] + (game_clock[1] - mean(t_hit_avg))*mean(x_speed_avg),
                    y_hit = y_loc[1] + (game_clock[1] - mean(t_hit_avg))*mean(y_speed_avg))
        
        landing_spots_final = rbind(test_shot3,
                                    TEAM_test_shot3) %>%
          arrange(GAME_EVENT_ID)
        
        }else{
          
          landing_spots_final = test_shot3
          
        }
      
      }else{
        
        landing_spots_final = test_shot3
        
      }
      
      
      setwd("~/Desktop/Deep_Learning/nba-movement-data/rebounding_data/landing_spots_full_output2/")
      write.csv(shot_full_data3, paste0(game_id_str, "_rebounding_data.csv"))
      
      setwd("~/Desktop/Deep_Learning/nba-movement-data/rebounding_data/landing_spots_full_output2/")
      write.csv(players_info, paste0(game_id_str, "_player_info.csv"))
      
      setwd("~/Desktop/Deep_Learning/nba-movement-data/rebounding_data/landing_spots_full_output2/")
      write.csv(landing_spots_final, paste0(game_id_str, "_landing_data.csv"))
      
      # deleting intermediate csv
      setwd("~/Desktop/Deep_Learning/nba-movement-data/rebounding_data/intermediate")
      system('rm *')
      
    }else{
      
      print("THERE WAS NO EVENT DATA!")
      
      # deleting intermediate csv
      setwd("~/Desktop/Deep_Learning/nba-movement-data/rebounding_data/intermediate")
      system('rm *')
    }
    
  }else{
  }
  
  
}
