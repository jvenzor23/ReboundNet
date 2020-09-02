# This code examines the basketball pbp data

# Clean workspace
rm(list=ls())

# Setting Working Directory
setwd("~/Desktop/Deep_Learning/nba-movement-data/rebounding_data/output/")

# Calling Necessary Libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)
library(reticulate)
library(ggstar)


# Reading in the Data -----------------------------------------------------

setwd("~/Desktop/Deep_Learning/nba-movement-data/rebounding_data/landing_spots_full_output2/")
# shot_full_data3 = read.csv("0021500492_rebounding_data.csv")
# shot_full_data3 = read.csv("0021500495_rebounding_data.csv")
# shot_full_data3 = read.csv("0021500424_rebounding_data.csv")
shot_full_data3 = read.csv("0021500094_rebounding_data.csv")
scripts = dir()
scripts = scripts[grep("_player", scripts)]

players_info = data.frame()
for(script in scripts){
  players_info = rbind(players_info,
                       read.csv(script)) %>%
    distinct()
}

players_info = players_info %>%
  select(-gameId, -X, -position) %>%
  distinct() %>%
  group_by(playerid, firstname, lastname) %>%
  summarize(jersey = jersey[1])

setwd("~/Desktop/Deep_Learning/nba-movement-data/data/events/")
events_data = read.csv("0021500094.csv")

team_mapping = events_data %>%
  select(PLAYER1_TEAM_ID, PLAYER1_TEAM_CITY, PLAYER1_TEAM_NICKNAME) %>%
  distinct() %>%
  mutate(teamName = paste(PLAYER1_TEAM_CITY, PLAYER1_TEAM_NICKNAME)) %>%
  filter(PLAYER1_TEAM_ID != 0,
         PLAYER1_TEAM_CITY != "") %>%
  select(PLAYER1_TEAM_ID, teamName)

# results
setwd("~/Desktop/Deep_Learning/nba-movement-data/rebounding_data/data_sets/")
preds = read.csv("landing_isotropic_preds3.csv")


preds_long = preds %>%
  pivot_longer(cols = starts_with("p"),
               names_to = "player",
               values_to = "rebound_prob") %>%
  group_by(GAME_ID, GAME_EVENT_ID) %>%
  mutate(player_num = row_number())

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

landing_data = landing_data_tot %>%
  filter(GAME_ID == 21500094)


# Joining in Rebound Probabilites -----------------------------------------
shot_full_data_w_probs = shot_full_data3 %>%
  arrange(GAME_ID, GAME_EVENT_ID, IsOnOffense, IsShooter, player_id) %>%
  filter(!IsBall) %>%
  group_by(GAME_ID, GAME_EVENT_ID, IsOnOffense, IsShooter) %>%
  mutate(player_num = dense_rank(player_id)) %>%
  mutate(player_num = case_when(IsShooter ~ as.integer(10),
                                 IsOnOffense ~ player_num + as.integer(5),
                                 TRUE ~ as.integer(player_num))) %>%
  inner_join(preds_long) %>%
  select(GAME_ID, GAME_EVENT_ID, rebound_prob, player_id)

shot_full_data3 = shot_full_data3 %>%
  left_join(shot_full_data_w_probs)

defRebProb = shot_full_data3 %>%
  group_by(GAME_ID, GAME_EVENT_ID) %>%
  filter(!IsBall) %>%
  summarize(def_players = sum(rebound_prob*as.numeric(!IsOnOffense))) %>%
  mutate(defRebProbability = def_players) %>%
  select(GAME_ID, GAME_EVENT_ID, defRebProbability)

shot_full_data3  = shot_full_data3 %>%
  inner_join(defRebProb)

no_rebound_plays = shot_full_data3 %>%
  group_by(GAME_ID, GAME_EVENT_ID) %>%
  summarize(value = sum(IsRebounder)) %>%
  filter(value == 0) %>%
  select(GAME_ID, GAME_EVENT_ID)

shot_full_data_w_probs = shot_full_data_w_probs %>%
  anti_join(no_rebound_plays)

# Plotting Data -----------------------------------------------------------

# 3, 42, 1, 12, 4, 64!, 186 (not unique, actual play_id, and this is an example
# where nobody got the rebound)
# GAME_ID 21500495, UNIQUE PLAY 12: great example to use of how to do player eval,
# second offensive player should not get much credit here (as well as eventual rebounder)
# because this was an uncontested rebound (defined by sum of opponent rebound prob)
# Grabbing this board only marginally helped the defense, since it was their rebound to get
# all the way
# -----> IDEA: credit rebounder w/ rebound prob of opposing team (this is how many)
# rebounds they earned their team, not themselves!
# Unique play 14 from same game a great example of a rebound in no man's land that is
# very difficult to predict

# GAME_ID == 21500424, GAME_EVENT_ID == 11, Keven Love's highest etra value!
example.play = shot_full_data3 %>%
  left_join(players_info, by = c("player_id" = "playerid")) %>%
  inner_join(events_data %>%
               mutate(Description = as.factor(paste(as.character(HOMEDESCRIPTION), 
                                                    as.character(VISITORDESCRIPTION)))) %>%
               select(EVENTNUM, Description),
             by = c("GAME_EVENT_ID" = "EVENTNUM")) %>%
  filter(GAME_EVENT_ID %in% unique(shot_full_data3$GAME_EVENT_ID)[1]) %>%
  # filter(GAME_EVENT_ID == 6) %>%
  mutate(Description = fct_reorder(Description, GAME_EVENT_ID)) %>%
  arrange(GAME_ID, GAME_EVENT_ID, player_id) %>%
  mutate(rebound_prob = round(rebound_prob, 2),
         fullname = paste(firstname, lastname)) %>%
  group_by(IsBall, IsOnOffense) %>%
  mutate(yloc_legend = row_number(jersey)*3) %>%
  arrange(desc(IsBall), IsOnOffense, jersey) %>%
  mutate(yloc_legend = if_else(IsOnOffense, yloc_legend + 30, yloc_legend + 5)) %>%
  left_join(team_mapping, by = c("team_id" = "PLAYER1_TEAM_ID"))

ggplot(data=data.frame(x=1,y=1),aes(x,y))+
  scale_color_manual(values = c("#FDBB30", "#78BE20"), guide = FALSE) +
  scale_fill_manual(values = c("#860038", "#236192"), guide = FALSE) +
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
  geom_point(data = example.play %>% filter(!IsBall, !IsRebounder), aes(x = y_loc - 25, y = x_loc - 47,
                                                          fill = IsOnOffense, color = IsOnOffense, group = lastname), alpha = 1, size = 8, stroke = 2, pch = 21) + 
  geom_star(data = example.play %>% filter(IsRebounder), aes(x = y_loc - 25, y = x_loc - 47,
                                                                        fill = IsOnOffense, color = IsOnOffense), starshape = 2, alpha = 1, size = 8, starstroke = 2) + 
  geom_text(data = example.play %>% filter(!IsBall), aes(x = y_loc - 25, y = x_loc - 47, label = jersey, group = lastname), fontface = "bold", color = "white", size = 4) +
  geom_label(data = example.play %>% filter(!IsBall), aes(x = y_loc - 25, y = x_loc - 47, label = rebound_prob, group = lastname), color = "black", fill = "white", size = 4, hjust = 1.5) +
  geom_point(data = example.play %>% filter(IsBall), aes(x = y_loc - 25, y = x_loc - 47), alpha = 1, size = 5, fill = 'darkorange1',
             color = 'black', pch = 21) +
  geom_segment(data = example.play %>% 
                 filter(!IsBall),
               aes(x = y_loc - 25, y = x_loc - 47, 
                   xend = y_loc - 25 + y_speed*2, yend = x_loc - 47 + x_speed*2),
               arrow = arrow(length = unit(.2,"cm")), color = "black") + 
  geom_point(data = landing_data %>%
               filter(GAME_EVENT_ID %in% unique(shot_full_data3$GAME_EVENT_ID)[1]) %>%
               # filter(GAME_EVENT_ID == 6) %>%
               inner_join(events_data %>%
                            mutate(Description = as.factor(paste(as.character(HOMEDESCRIPTION), 
                                                                 as.character(VISITORDESCRIPTION)))) %>%
                            select(EVENTNUM, Description),
                          by = c("GAME_EVENT_ID" = "EVENTNUM")),
             aes(x = y_hit -25, y = x_hit - 47), color = "red", shape = 13, size = 4, stroke = 1.5) +
  geom_point(data = example.play %>% filter(!IsBall), aes(x = 30, y = -yloc_legend,
                                                                        fill = IsOnOffense, color = IsOnOffense, group = lastname), alpha = 1, size = 8, stroke = 2, pch = 21) +
  geom_text(data = example.play %>% filter(!IsBall), aes(x = 30, y = -yloc_legend, label = jersey, group = lastname), fontface = "bold", color = "white", size = 4) +
  geom_label(data = example.play %>% filter(!IsBall), aes(x = 32, y = -yloc_legend, label = fullname, group = lastname), color = "black", fill = "white", size = 4, hjust = 0) +
  geom_label(data = example.play %>% filter(!IsBall,
                                            !IsOnOffense) %>%
               ungroup() %>%
               select(IsOnOffense, teamName) %>%
               distinct(), aes(x = 28, y = -3, label = teamName, 
                               color = IsOnOffense, fill = IsOnOffense), size = 8, hjust = 0) +
  geom_label(data = example.play %>% filter(!IsBall,
                                            IsOnOffense) %>%
               ungroup() %>%
               select(IsOnOffense, teamName) %>%
               distinct(), aes(x = 28, y = -28, label = teamName, 
                               color = IsOnOffense, fill = IsOnOffense), size = 8, hjust = 0) +
  geom_point(data = landing_data, 
             aes(x = -23, y = 2), 
             color = "red", shape = 13, size = 4, stroke = 1.5) +
  geom_text(aes(x = -21.5, y = 2, label = "Estimated Landing Spot"), hjust = 0) +
  geom_star(aes(x = -3, y = 2), fill = "black", color = "black", starshape = 2, alpha = 1, size = 4, starstroke = 2) + 
  geom_text(aes(x = -1.5, y = 2, label = "Eventual Rebounder"), hjust = 0) +
  geom_segment(data = example.play %>% 
                 filter(!IsBall),
               aes(x = 13, y = 1, 
                   xend = 14, yend = 3),
               arrow = arrow(length = unit(.2,"cm")), color = "black") + 
  geom_text(aes(x = 15, y = 2, label = "Player Velocity"), hjust = 0) +
  ###fix aspect ratio to 1:1
  theme(panel.background = element_rect(colour = 'black'),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text.x = element_text(size = 15),
        axis.text=element_blank(),
        axis.ticks=element_blank()
        ) + 
  labs(title = unique(example.play$Description),
       subtitle = paste0("Defensive Rebound Probability: ",
                      round(example.play$defRebProbability[1], 3)),
       y = "",
       x = "") + 
  xlim(-25, 50) +
  coord_fixed()


# Plotting Function -------------------------------------------------------

plotPlay = function(event_id,
                    offenseFill,
                    offenseColor,
                    defenseFill,
                    defenseColor,
                    hjustval = -.5, 
                    vjustval = 0){
  
  example.play = shot_full_data3 %>%
    left_join(players_info, by = c("player_id" = "playerid")) %>%
    inner_join(events_data %>%
                 mutate(Description = as.factor(paste(as.character(HOMEDESCRIPTION), 
                                                      as.character(VISITORDESCRIPTION)))) %>%
                 select(EVENTNUM, Description),
               by = c("GAME_EVENT_ID" = "EVENTNUM")) %>%
    filter(GAME_EVENT_ID == event_id) %>%
    mutate(Description = fct_reorder(Description, GAME_EVENT_ID)) %>%
    arrange(GAME_ID, GAME_EVENT_ID, player_id) %>%
    mutate(rebound_prob = round(rebound_prob, 2),
           fullname = paste(firstname, lastname)) %>%
    group_by(IsBall, IsOnOffense) %>%
    mutate(yloc_legend = row_number(jersey)*3) %>%
    arrange(desc(IsBall), IsOnOffense, jersey) %>%
    mutate(yloc_legend = if_else(IsOnOffense, yloc_legend + 30, yloc_legend + 5)) %>%
    left_join(team_mapping, by = c("team_id" = "PLAYER1_TEAM_ID"))
  
  ggplot(data=data.frame(x=1,y=1),aes(x,y))+
    scale_color_manual(values = c(defenseColor, offenseColor), guide = FALSE) +
    scale_fill_manual(values = c(defenseFill, offenseFill), guide = FALSE) +
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
    geom_point(data = example.play %>% filter(!IsBall, !IsRebounder), aes(x = y_loc - 25, y = x_loc - 47,
                                                                          fill = IsOnOffense, color = IsOnOffense, group = lastname), alpha = 1, size = 8, stroke = 2, pch = 21) + 
    geom_star(data = example.play %>% filter(IsRebounder), aes(x = y_loc - 25, y = x_loc - 47,
                                                               fill = IsOnOffense, color = IsOnOffense), starshape = 2, alpha = 1, size = 8, starstroke = 2) + 
    geom_text(data = example.play %>% filter(!IsBall), aes(x = y_loc - 25, y = x_loc - 47, label = jersey, group = lastname), fontface = "bold", color = "white", size = 4) +
    geom_label(data = example.play %>% filter(!IsBall), aes(x = y_loc - 25, y = x_loc - 47, label = rebound_prob, group = lastname), color = "black", fill = "white", size = 4, hjust = hjustval, vjust = vjustval) +
    geom_point(data = example.play %>% filter(IsBall), aes(x = y_loc - 25, y = x_loc - 47), alpha = 1, size = 5, fill = 'darkorange1',
               color = 'black', pch = 21) +
    geom_segment(data = example.play %>% 
                   filter(!IsBall),
                 aes(x = y_loc - 25, y = x_loc - 47, 
                     xend = y_loc - 25 + y_speed*2, yend = x_loc - 47 + x_speed*2),
                 arrow = arrow(length = unit(.2,"cm")), color = "black") + 
    geom_point(data = landing_data %>%
                 filter(GAME_EVENT_ID == event_id) %>%
                 inner_join(events_data %>%
                              mutate(Description = as.factor(paste(as.character(HOMEDESCRIPTION), 
                                                                   as.character(VISITORDESCRIPTION)))) %>%
                              select(EVENTNUM, Description),
                            by = c("GAME_EVENT_ID" = "EVENTNUM")),
               aes(x = y_hit -25, y = x_hit - 47), color = "red", shape = 13, size = 4, stroke = 1.5) +
    geom_point(data = example.play %>% filter(!IsBall), aes(x = 30, y = -yloc_legend,
                                                            fill = IsOnOffense, color = IsOnOffense, group = lastname), alpha = 1, size = 8, stroke = 2, pch = 21) +
    geom_text(data = example.play %>% filter(!IsBall), aes(x = 30, y = -yloc_legend, label = jersey, group = lastname), fontface = "bold", color = "white", size = 4) +
    geom_label(data = example.play %>% filter(!IsBall), aes(x = 32, y = -yloc_legend, label = fullname, group = lastname), color = "black", fill = "white", size = 4, hjust = 0) +
    geom_label(data = example.play %>% filter(!IsBall,
                                              !IsOnOffense) %>%
                 ungroup() %>%
                 select(IsOnOffense, teamName) %>%
                 distinct(), aes(x = 28, y = -3, label = trimws(teamName), 
                                 fill = IsOnOffense), color = "white", size = 8, hjust = 0) +
    geom_label(data = example.play %>% filter(!IsBall,
                                              IsOnOffense) %>%
                 ungroup() %>%
                 select(IsOnOffense, teamName) %>%
                 distinct(), aes(x = 28, y = -28, label = trimws(teamName), 
                                 fill = IsOnOffense), color = "white", size = 8, hjust = 0) +
    geom_point(data = landing_data, 
               aes(x = -23, y = 2), 
               color = "red", shape = 13, size = 4, stroke = 1.5) +
    geom_text(aes(x = -21.5, y = 2, label = "Estimated Landing Spot"), hjust = 0) +
    geom_star(aes(x = -3, y = 2), fill = "black", color = "black", starshape = 2, alpha = 1, size = 4, starstroke = 2) + 
    geom_text(aes(x = -1.5, y = 2, label = "Eventual Rebounder"), hjust = 0) +
    geom_segment(data = example.play %>% 
                   filter(!IsBall),
                 aes(x = 13, y = 1, 
                     xend = 14, yend = 3),
                 arrow = arrow(length = unit(.2,"cm")), color = "black") + 
    geom_text(aes(x = 15, y = 2, label = "Player Velocity"), hjust = 0) +
    ###fix aspect ratio to 1:1
    theme(panel.background = element_rect(colour = 'black'),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          strip.text.x = element_text(size = 15),
          axis.text=element_blank(),
          axis.ticks=element_blank()
    ) + 
    labs(title = unique(example.play$Description),
         subtitle = paste0("Defensive Rebound Probability: ",
                           round(example.play$defRebProbability[1], 3)),
         y = "",
         x = "") + 
    xlim(-25, 50) +
    coord_fixed()
  
  
}

# Kevin Love Good Play!
# 21500063
plotPlay(85, 
         "#006BB6",
         "#F58426",
         "#860038",
         "#FDBB30")

ggsave("~/Desktop/Deep_Learning/nba-movement-data/rebounding_data/images/KLoveWorst.png",
       height = 7, width = 10)

# Kevin Love 2nd Worst Play!
plotPlay(2, 
         "#007A33",
         "#BA9653",
         "#860038",
         "#FDBB30")

ggsave("~/Desktop/Deep_Learning/nba-movement-data/rebounding_data/images/KLove2Worst.png",
       height = 8, width = 8)

# Kevin Love Best Play!
plotPlay(471, 
         "#860038",
         "#FDBB30",
         "#006BB6",
         "#C4CED4")

ggsave("~/Desktop/Deep_Learning/nba-movement-data/rebounding_data/images/KLoveBest.png",
       height = 7, width = 10)

# Kevin Love 2nd Best Play!
plotPlay(196, 
         "#860038",
         "#FDBB30",
         "#CE1141",
         "#C4CED4",
         hjustval = 1.5)

ggsave("~/Desktop/Deep_Learning/nba-movement-data/rebounding_data/images/KLove2Best.png",
       height = 8, width = 8)

# Generic Example Play NO PROBS
plotPlay(44, 
         "#00788C",
         "#1D1160",
         "#CE1141",
         "#A1A1A4",
         vjust = .5)

ggsave("~/Desktop/Deep_Learning/nba-movement-data/rebounding_data/images/ReboundExNoProbs.png",
       height = 7, width = 10)


# Generic Example Play
plotPlay(44, 
         "#00788C",
         "#1D1160",
         "#CE1141",
         "#A1A1A4",
         vjust = .5)

ggsave("~/Desktop/Deep_Learning/nba-movement-data/rebounding_data/images/ReboundEx.png",
       height = 7, width = 10)

# Example of Importance of Landing Spot
plotPlay(400, 
         "#00788C",
         "#1D1160",
         "#CE1141",
         "#A1A1A4",
         hjustval = .5,
         vjustval = -1)

ggsave("~/Desktop/Deep_Learning/nba-movement-data/rebounding_data/images/LandingEx.png",
       height = 7, width = 10)

# Example of Importance of Speed
plotPlay(87, 
         "#CE1141",
         "#A1A1A4",
         "#00788C",
         "#1D1160",
         hjustval = 1.3)

ggsave("~/Desktop/Deep_Learning/nba-movement-data/rebounding_data/images/SpeedEx.png",
       height = 7, width = 10)

# Example of Importance of Boxing Out
plotPlay(491, 
         "#CE1141",
         "#A1A1A4",
         "#00788C",
         "#1D1160",
         hjustval = .5,
         vjustval = -.5)

ggsave("~/Desktop/Deep_Learning/nba-movement-data/rebounding_data/images/BoxOutEx.png",
       height = 7, width = 10)

plotPlay2 = function(event_id,
                    offenseFill,
                    offenseColor,
                    defenseFill,
                    defenseColor,
                    hjustval = -.5, 
                    vjustval = 0){
  
  example.play = shot_full_data3 %>%
    left_join(players_info, by = c("player_id" = "playerid")) %>%
    inner_join(events_data %>%
                 mutate(Description = as.factor(paste(as.character(HOMEDESCRIPTION), 
                                                      as.character(VISITORDESCRIPTION)))) %>%
                 select(EVENTNUM, Description),
               by = c("GAME_EVENT_ID" = "EVENTNUM")) %>%
    filter(GAME_EVENT_ID == event_id) %>%
    mutate(Description = fct_reorder(Description, GAME_EVENT_ID)) %>%
    arrange(GAME_ID, GAME_EVENT_ID, player_id) %>%
    mutate(fullname = paste(firstname, lastname)) %>%
    group_by(IsBall, IsOnOffense) %>%
    mutate(yloc_legend = row_number(jersey)*3) %>%
    arrange(desc(IsBall), IsOnOffense, jersey) %>%
    mutate(yloc_legend = if_else(IsOnOffense, yloc_legend + 30, yloc_legend + 5)) %>%
    left_join(team_mapping, by = c("team_id" = "PLAYER1_TEAM_ID")) %>%
    mutate(TeamRebProb = if_else(IsOnOffense, 1 - defRebProbability, 
                                 defRebProbability),
           TeamReb = if_else(IsOnOffense, !IsDefensiveRebound, IsDefensiveRebound)) %>%
    mutate(etra = case_when(player_id == -1 ~ 0,
                            TeamReb & !IsRebounder ~ 0,
                            TeamReb & IsRebounder ~ 1 - TeamRebProb,
                            TRUE ~ -rebound_prob/TeamRebProb*(TeamRebProb))) %>%
    mutate(color_group = case_when(etra == 0 ~ 0,
                                   etra > 0 ~ 1,
                                   etra < 0 ~ 2)) %>%
    mutate(rebound_prob = round(rebound_prob, 2),
           etra = round(etra,3))
  
  ggplot(data=data.frame(x=1,y=1),aes(x,y))+
    scale_color_manual(values = c(defenseColor, offenseColor), guide = FALSE) +
    scale_fill_manual(values = c(defenseFill, offenseFill), guide = FALSE) +
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
    geom_point(data = example.play %>% filter(!IsBall, !IsRebounder), aes(x = y_loc - 25, y = x_loc - 47,
                                                                          fill = IsOnOffense, color = IsOnOffense, group = lastname), alpha = 1, size = 8, stroke = 2, pch = 21) + 
    geom_star(data = example.play %>% filter(IsRebounder), aes(x = y_loc - 25, y = x_loc - 47,
                                                               fill = IsOnOffense, color = IsOnOffense), starshape = 2, alpha = 1, size = 8, starstroke = 2) + 
    geom_text(data = example.play %>% filter(!IsBall), aes(x = y_loc - 25, y = x_loc - 47, label = jersey, group = lastname), fontface = "bold", color = "white", size = 4) +
    geom_label(data = example.play %>% filter(!IsBall), aes(x = y_loc - 25, y = x_loc - 47, label = sprintf("%0.2f",rebound_prob), group = lastname), color = "black", fill = "white", size = 4, hjust = hjustval, vjust = vjustval) +
    geom_point(data = example.play %>% filter(IsBall), aes(x = y_loc - 25, y = x_loc - 47), alpha = 1, size = 5, fill = 'darkorange1',
               color = 'black', pch = 21) +
    geom_segment(data = example.play %>% 
                   filter(!IsBall),
                 aes(x = y_loc - 25, y = x_loc - 47, 
                     xend = y_loc - 25 + y_speed*2, yend = x_loc - 47 + x_speed*2),
                 arrow = arrow(length = unit(.2,"cm")), color = "black") + 
    geom_point(data = landing_data %>%
                 filter(GAME_EVENT_ID == event_id) %>%
                 inner_join(events_data %>%
                              mutate(Description = as.factor(paste(as.character(HOMEDESCRIPTION), 
                                                                   as.character(VISITORDESCRIPTION)))) %>%
                              select(EVENTNUM, Description),
                            by = c("GAME_EVENT_ID" = "EVENTNUM")),
               aes(x = y_hit -25, y = x_hit - 47), color = "red", shape = 13, size = 4, stroke = 1.5) +
    geom_point(data = example.play %>% filter(!IsBall), aes(x = 30, y = -yloc_legend,
                                                            fill = IsOnOffense, color = IsOnOffense, group = lastname), alpha = 1, size = 8, stroke = 2, pch = 21) +
    geom_text(data = example.play %>% filter(!IsBall), aes(x = 30, y = -yloc_legend, label = jersey, group = lastname), fontface = "bold", color = "white", size = 4) +
    geom_label(data = example.play %>% filter(!IsBall, color_group == 0), aes(x = 32, y = -yloc_legend, label = paste0(trimws(fullname), " (",
                                                                                                     as.character(etra), ")"), group = lastname), color = "black", fill = "white", size = 4, hjust = 0) +
    geom_label(data = example.play %>% filter(!IsBall, color_group == 1), aes(x = 32, y = -yloc_legend, label = paste0(trimws(fullname), " (+",
                                                                                                     as.character(etra), ")"), group = lastname), color = "white", fill = "darkgreen", size = 4, hjust = 0) +
    geom_label(data = example.play %>% filter(!IsBall, color_group == 2), aes(x = 32, y = -yloc_legend, label = paste0(trimws(fullname), " (",
                                                                                                                       sprintf("%0.3f",etra), ")"), group = lastname), color = "white", fill = "red", size = 4, hjust = 0) +
    geom_label(data = example.play %>% filter(!IsBall,
                                              !IsOnOffense) %>%
                 ungroup() %>%
                 select(IsOnOffense, teamName) %>%
                 distinct(), aes(x = 28, y = -3, label = trimws(teamName), 
                                 fill = IsOnOffense), color = "white", size = 8, hjust = 0) +
    geom_label(data = example.play %>% filter(!IsBall,
                                              IsOnOffense) %>%
                 ungroup() %>%
                 select(IsOnOffense, teamName) %>%
                 distinct(), aes(x = 28, y = -28, label = trimws(teamName), 
                                 fill = IsOnOffense), color = "white", size = 8, hjust = 0) +
    geom_point(data = landing_data, 
               aes(x = -23, y = 2), 
               color = "red", shape = 13, size = 4, stroke = 1.5) +
    geom_text(aes(x = -21.5, y = 2, label = "Estimated Landing Spot"), hjust = 0) +
    geom_star(aes(x = -3, y = 2), fill = "black", color = "black", starshape = 2, alpha = 1, size = 4, starstroke = 2) + 
    geom_text(aes(x = -1.5, y = 2, label = "Eventual Rebounder"), hjust = 0) +
    geom_segment(data = example.play %>% 
                   filter(!IsBall),
                 aes(x = 13, y = 1, 
                     xend = 14, yend = 3),
                 arrow = arrow(length = unit(.2,"cm")), color = "black") + 
    geom_text(aes(x = 15, y = 2, label = "Player Velocity"), hjust = 0) +
    ###fix aspect ratio to 1:1
    theme(panel.background = element_rect(colour = 'black'),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          strip.text.x = element_text(size = 15),
          axis.text=element_blank(),
          axis.ticks=element_blank()
    ) + 
    labs(title = unique(example.play$Description),
         subtitle = paste0("Defensive Rebound Probability: ",
                           round(example.play$defRebProbability[1], 3)),
         y = "",
         x = "") + 
    xlim(-25, 52) +
    coord_fixed()
  
  
}

# Kevin Love Good Play!
# 21500094
plotPlay2(174, 
         "#860038",
         "#FDBB30",
         "#002D62",
         "#FDBB30",
         hjustval = c(-.3, -.3, -.3, .7, -.3, -.3, -.3, -.3, .5, -.3),
         vjustval = c(0,.7,0,-.7,0,.5,0,0,1.5,0))

ggsave("~/Desktop/Deep_Learning/nba-movement-data/rebounding_data/images/KLoveGoodPlayWithScores.png",
       height = 7, width = 10)

example.play = shot_full_data3 %>%
  left_join(players_info, by = c("player_id" = "playerid")) %>%
  inner_join(events_data %>%
               mutate(Description = as.factor(paste(as.character(HOMEDESCRIPTION), 
                                                    as.character(VISITORDESCRIPTION)))) %>%
               select(EVENTNUM, Description),
             by = c("GAME_EVENT_ID" = "EVENTNUM")) %>%
  filter(GAME_EVENT_ID == 85) %>%
  mutate(Description = fct_reorder(Description, GAME_EVENT_ID)) %>%
  arrange(GAME_ID, GAME_EVENT_ID, player_id) %>%
  mutate(fullname = paste(firstname, lastname)) %>%
  group_by(IsBall, IsOnOffense) %>%
  mutate(yloc_legend = row_number(jersey)*3) %>%
  arrange(desc(IsBall), IsOnOffense, jersey) %>%
  mutate(yloc_legend = if_else(IsOnOffense, yloc_legend + 30, yloc_legend + 5)) %>%
  left_join(team_mapping, by = c("team_id" = "PLAYER1_TEAM_ID")) %>%
  mutate(TeamRebProb = if_else(IsOnOffense, 1 - defRebProbability, 
                               defRebProbability),
         TeamReb = if_else(IsOnOffense, !IsDefensiveRebound, IsDefensiveRebound)) %>%
  mutate(etra = case_when(player_id == -1 ~ 0,
                          TeamReb & !IsRebounder ~ 0,
                          TeamReb & IsRebounder ~ 1 - TeamRebProb,
                          TRUE ~ -rebound_prob/TeamRebProb*(TeamRebProb))) %>%
  mutate(rebound_prob = round(rebound_prob, 2),
         etra = round(etra,2))
