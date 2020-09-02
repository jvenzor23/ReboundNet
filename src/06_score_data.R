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
library(ggimage)
library(magick)


# Reading in the Data -----------------------------------------------------

setwd("~/Desktop/Deep_Learning/nba-movement-data/rebounding_data/landing_spots_full_output2/")
files = dir()
files = files[grep("_rebounding_data", files)]
length(unique(files))

data_tot = data.frame()

for(file in files){
  data_tot = rbind(data_tot, 
                   read.csv(file) %>%
                     select(-X))
}


# players
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
  summarize(position = position[1])


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


# Joining in Rebound Probabilites -----------------------------------------
shot_full_data_w_probs = data_tot %>%
  arrange(GAME_ID, GAME_EVENT_ID, IsOnOffense, IsShooter, player_id) %>%
  filter(!IsBall) %>%
  group_by(GAME_ID, GAME_EVENT_ID, IsOnOffense, IsShooter) %>%
  mutate(player_num = dense_rank(player_id)) %>%
  mutate(player_num = case_when(IsShooter ~ as.integer(10),
                                IsOnOffense ~ player_num + as.integer(5),
                                TRUE ~ as.integer(player_num))) %>%
  inner_join(preds_long) %>%
  select(GAME_ID, GAME_EVENT_ID, rebound_prob, player_id)

data_tot2 = data_tot %>%
  inner_join(shot_full_data_w_probs)

defRebProb = data_tot2 %>%
  group_by(GAME_ID, GAME_EVENT_ID) %>%
  filter(!IsBall) %>%
  summarize(def_players = sum(rebound_prob*as.numeric(!IsOnOffense))) %>%
  mutate(defRebProbability = def_players) %>%
  select(GAME_ID, GAME_EVENT_ID, defRebProbability)

data_tot3  = data_tot2 %>%
  inner_join(defRebProb)

no_rebound_plays = data_tot3 %>%
  group_by(GAME_ID, GAME_EVENT_ID) %>%
  summarize(value = sum(IsRebounder)) %>%
  filter(value == 0) %>%
  select(GAME_ID, GAME_EVENT_ID)

data_tot3 = data_tot3 %>%
  anti_join(no_rebound_plays)

aggressiveness_check = data_tot3 %>%
  group_by(GAME_ID,GAME_EVENT_ID) %>%
  summarize(max_prob = max(rebound_prob)) %>%
  ungroup() %>%
  summarize(avg_max_prob = mean(max_prob),
            perc_over_95 = mean(max_prob >= .95),
            perc_over_90 = mean(max_prob >= .90),
            perc_over_50 = mean(max_prob >= .50),
            perc_under_30 = mean(max_prob <= .30))


# Aggregate Totals --------------------------------------------------------
summary_stats = data_tot3 %>%
  mutate(era = as.numeric(IsRebounder) - rebound_prob) %>%
  group_by(player_id) %>%
  summarize(n_games = length(unique(GAME_ID)),
            Plays = n(),
            REB = sum(IsRebounder, na.rm = TRUE),
            REB_perc = REB/Plays,
            eREB = sum(rebound_prob, na.rm = TRUE),
            netReb = sum(era, na.rm = TRUE),
            REB_added_per_play = mean(era, na.rm = TRUE),
            eREBperPlay = sum(rebound_prob, na.rm = TRUE)/Plays) %>%
  filter(Plays > 200) %>%
  arrange(desc(REB_added_per_play)) %>%
  inner_join(players_info, by = c("player_id" = "playerid")) %>%
  select(position, firstname, lastname, player_id, everything()) %>%
  filter(grepl('F', position)|grepl('C', position))


summary_stats %>%
  filter(grepl('F', position)|grepl('C', position)) %>%
  ggplot() +
  geom_point(aes(x = eREBperPlay, y = REB_added_per_play))

summary_stats_off = data_tot3 %>%
  filter(IsOnOffense) %>%
  mutate(era = as.numeric(IsRebounder) - rebound_prob) %>%
  group_by(player_id) %>%
  summarize(OFF_Plays = n(),
            n_games = length(unique(GAME_ID)),
            OFF_REB = sum(IsRebounder, na.rm = TRUE),
            OFF_REB_perc = OFF_REB/OFF_Plays,
            OFF_eREB = sum(rebound_prob, na.rm = TRUE),
            OFF_netReb = sum(era, na.rm = TRUE),
            OFF_REB_added_per_play = mean(era, na.rm = TRUE),
            OFF_eREBperPlay = sum(rebound_prob, na.rm = TRUE)/n()) %>%
  filter(OFF_Plays > 200) %>%
  arrange(desc(OFF_eREBperPlay)) %>%
  inner_join(players_info, by = c("player_id" = "playerid")) %>%
  select(position, firstname, lastname, player_id, everything())

summary_stats_def = data_tot3 %>%
  filter(!IsOnOffense) %>%
  mutate(era = as.numeric(IsRebounder) - rebound_prob) %>%
  group_by(player_id) %>%
  summarize(DEF_Plays = n(),
            DEF_REB = sum(IsRebounder, na.rm = TRUE),
            DEF_REB_perc = DEF_REB/DEF_Plays,
            DEF_eREB = sum(rebound_prob, na.rm = TRUE),
            DEF_netReb = sum(era, na.rm = TRUE),
            DEF_REB_added_per_play = mean(era, na.rm = TRUE),
            DEF_eREBperPlay = sum(rebound_prob, na.rm = TRUE)/n()) %>%
  filter(DEF_Plays > 200) %>%
  arrange(desc(DEF_eREBperPlay)) %>%
  inner_join(players_info, by = c("player_id" = "playerid")) %>%
  select(position, firstname, lastname, player_id, everything())

summary_stats_both = summary_stats_off %>%
  inner_join(summary_stats_def)

summary_stats_both %>%
  filter(OFF_Plays > 500, DEF_Plays > 500) %>%
  filter(grepl('F', position)|grepl('C', position)) %>%
  ggplot() +
  geom_point(aes(x = OFF_REB_added_per_play, y = DEF_REB_added_per_play))

summary_stats_both %>%
  filter(OFF_Plays > 500, DEF_Plays > 500) %>%
  filter(grepl('F', position)|grepl('C', position)) %>%
  ggplot() +
  geom_point(aes(x = OFF_eREBperPlay, y = DEF_eREBperPlay))


# Russel Westbrook Effect -------------------------------------------------
summary_stats_westbrook = data_tot3 %>% 
  filter(!IsOnOffense) %>%
  inner_join(defRebProb) %>%
  mutate(era = as.numeric(IsRebounder) - rebound_prob) %>%
  mutate(def_reb_team_prob = floor(defRebProbability*10)/10) %>%
  group_by(player_id, def_reb_team_prob) %>%
  summarize(Plays = n(),
            REB = sum(IsRebounder, na.rm = TRUE),
            REB_perc = REB/Plays,
            eREB = sum(rebound_prob),
            netReb = sum(era),
            REB_added_per_play = mean(era),
            eREBperPlay = sum(rebound_prob)/Plays) %>%
  inner_join(players_info, by = c("player_id" = "playerid")) %>%
  arrange(desc(def_reb_team_prob)) %>%
  select(position, firstname, lastname, player_id, everything()) %>%
  filter(firstname == "Russell")


# Rebound % Stats ---------------------------------------------------------
summary_stats2 = data_tot3 %>%
  filter(player_id != -1) %>%
  group_by(GAME_ID, GAME_EVENT_ID) %>%
  mutate(rebound_prob_rank = min_rank(desc(rebound_prob))) %>%
  ungroup() %>%
  group_by(player_id) %>%
  summarize(n_games = length(unique(GAME_ID)),
            Plays = n(),
            TopReboundPlays = sum(rebound_prob_rank == 1, na.rm = TRUE),
            TopReboundPlaysPerc = TopReboundPlays/Plays,
            TopReboundPerc = sum(as.numeric(IsRebounder)*as.numeric(rebound_prob_rank == 1), na.rm = TRUE)/sum(rebound_prob_rank == 1, na.rm = TRUE),
            SecondReboundPlays = sum(rebound_prob_rank == 2, na.rm = TRUE),
            SecondReboundPlaysPerc = SecondReboundPlays/Plays,
            SecondReboundPerc = sum(as.numeric(IsRebounder)*as.numeric(rebound_prob_rank ==2), na.rm = TRUE)/sum(rebound_prob_rank == 2, na.rm = TRUE),
            ThirdReboundPlays = sum(rebound_prob_rank == 3, na.rm = TRUE),
            ThirdReboundPlaysPerc = ThirdReboundPlays/Plays,
            ThirdReboundPerc = sum(as.numeric(IsRebounder)*as.numeric(rebound_prob_rank ==3), na.rm = TRUE)/sum(rebound_prob_rank == 3, na.rm = TRUE)) %>%
  filter(Plays > 500) %>%
  arrange(desc(TopReboundPerc)) %>%
  inner_join(players_info, by = c("player_id" = "playerid")) %>%
  select(position, firstname, lastname, player_id, everything()) %>%
  filter(grepl('F', position)|grepl('C', position))

summary_stats2_off = data_tot3 %>%
  filter(player_id != -1) %>%
  group_by(GAME_ID, GAME_EVENT_ID) %>%
  mutate(rebound_prob_rank = min_rank(desc(rebound_prob))) %>%
  ungroup() %>%
  filter(IsOnOffense) %>%
  group_by(player_id) %>%
  summarize(n_games = length(unique(GAME_ID)),
            Plays = n(),
            TopReboundPlays = sum(rebound_prob_rank == 1, na.rm = TRUE),
            TopReboundPlaysPerc = TopReboundPlays/Plays,
            TopReboundPerc = sum(as.numeric(IsRebounder)*as.numeric(rebound_prob_rank == 1), na.rm = TRUE)/sum(rebound_prob_rank == 1, na.rm = TRUE),
            SecondReboundPlays = sum(rebound_prob_rank == 2, na.rm = TRUE),
            SecondReboundPlaysPerc = SecondReboundPlays/Plays,
            SecondReboundPerc = sum(as.numeric(IsRebounder)*as.numeric(rebound_prob_rank ==2), na.rm = TRUE)/sum(rebound_prob_rank == 2, na.rm = TRUE),
            ThirdReboundPlays = sum(rebound_prob_rank == 3, na.rm = TRUE),
            ThirdReboundPlaysPerc = ThirdReboundPlays/Plays,
            ThirdReboundPerc = sum(as.numeric(IsRebounder)*as.numeric(rebound_prob_rank ==3), na.rm = TRUE)/sum(rebound_prob_rank == 3, na.rm = TRUE)) %>%
  filter(TopReboundPlays > 30) %>%
  arrange(desc(TopReboundPerc)) %>%
  inner_join(players_info, by = c("player_id" = "playerid")) %>%
  select(position, firstname, lastname, player_id, everything()) %>%
  filter(grepl('F', position)|grepl('C', position)) 

summary_stats2_def = data_tot3 %>%
  filter(player_id != -1) %>%
  group_by(GAME_ID, GAME_EVENT_ID) %>%
  mutate(rebound_prob_rank = min_rank(desc(rebound_prob))) %>%
  ungroup() %>%
  filter(!IsOnOffense) %>%
  group_by(player_id) %>%
  summarize(n_games = length(unique(GAME_ID)),
            Plays = n(),
            TopReboundPlays = sum(rebound_prob_rank == 1, na.rm = TRUE),
            TopReboundPlaysPerc = TopReboundPlays/Plays,
            TopReboundPerc = sum(as.numeric(IsRebounder)*as.numeric(rebound_prob_rank == 1), na.rm = TRUE)/sum(rebound_prob_rank == 1, na.rm = TRUE),
            SecondReboundPlays = sum(rebound_prob_rank == 2, na.rm = TRUE),
            SecondReboundPlaysPerc = SecondReboundPlays/Plays,
            SecondReboundPerc = sum(as.numeric(IsRebounder)*as.numeric(rebound_prob_rank ==2), na.rm = TRUE)/sum(rebound_prob_rank == 2, na.rm = TRUE),
            ThirdReboundPlays = sum(rebound_prob_rank == 3, na.rm = TRUE),
            ThirdReboundPlaysPerc = ThirdReboundPlays/Plays,
            ThirdReboundPerc = sum(as.numeric(IsRebounder)*as.numeric(rebound_prob_rank ==3), na.rm = TRUE)/sum(rebound_prob_rank == 3, na.rm = TRUE)) %>%
  filter(Plays > 200) %>%
  arrange(desc(TopReboundPerc)) %>%
  inner_join(players_info, by = c("player_id" = "playerid")) %>%
  select(position, firstname, lastname, player_id, everything()) %>%
  filter(grepl('F', position)|grepl('C', position))

# Summary Stats BEST ------------------------------------------------------

# REBOUNDER: Credited with 1 - (Team Rebound Prob)
# Non-Rebounders on Rebounder's Team: No Change
# Non-Rebounders on Rebounder's Opponent: Debited with Team Rebound Prob * Team Rebound % Share

# Zero Sum Crediting and Debting, leading to a TeamRebound_added_per_play metric
summary_stats3 = data_tot3 %>%
  filter(player_id != -1) %>%
  mutate(teamRebProb = if_else(IsOnOffense, 1 - defRebProbability,
                               defRebProbability),
         teamRebShare = rebound_prob/teamRebProb,
         teamReboundFlag = if_else(IsOnOffense, !IsDefensiveRebound,
                                   IsDefensiveRebound)) %>%
  filter(!is.na(teamRebProb)) %>%
  group_by(GAME_ID, GAME_EVENT_ID, IsOnOffense) %>%
  mutate(playerTeamReboundProb = sum(rebound_prob)) %>%
  group_by(GAME_ID, GAME_EVENT_ID) %>%
  mutate(NoPlayerRebound = sum(IsRebounder) == 0) %>%
  ungroup() %>%
  mutate(etra = case_when(NoPlayerRebound & teamReboundFlag ~ (rebound_prob/playerTeamReboundProb)*(1-teamRebProb),
                          NoPlayerRebound & !teamReboundFlag ~ -(rebound_prob/playerTeamReboundProb)*teamRebProb,
                          IsRebounder ~ 1 - teamRebProb,
                          teamReboundFlag ~ 0,
                          TRUE ~ -teamRebProb*(rebound_prob/playerTeamReboundProb))) %>%
  mutate(era = as.numeric(IsRebounder) - rebound_prob) %>%
  group_by(player_id) %>%
  summarize(n_games = length(unique(GAME_ID)),
            Plays = n(),
            REB = sum(IsRebounder, na.rm = TRUE),
            REB_perc = REB/Plays,
            RebPer100 = REB_perc*100,
            eREB = sum(rebound_prob, na.rm = TRUE),
            netReb = sum(era, na.rm = TRUE),
            REB_added_per_play = mean(era, na.rm = TRUE),
            netPlayerRebPer100 = 100*REB_added_per_play,
            eREBperPlay = sum(rebound_prob, na.rm = TRUE)/Plays,
            netTeamReb = sum(etra, na.rm = TRUE),
            TEAM_REB_added_per_play = mean(etra, na.rm = TRUE),
            netTeamRebPer100 = TEAM_REB_added_per_play*100,
            TeamRebAddedPerReb = sum(if_else(etra > 0, etra, 0))/
                                 sum(if_else(etra > 0, 1, 0))) %>%
  arrange(desc(netTeamReb)) %>%
  left_join(players_info, by = c("player_id" = "playerid")) %>%
  select(position, firstname, lastname, player_id, everything())

# Example Play to Explain Metric
summary_stats3ex = data_tot3 %>%
  filter(player_id != -1) %>%
  mutate(teamRebProb = if_else(IsOnOffense, 1 - defRebProbability,
                               defRebProbability),
         teamRebShare = rebound_prob/teamRebProb,
         teamReboundFlag = if_else(IsOnOffense, !IsDefensiveRebound,
                                   IsDefensiveRebound)) %>%
  group_by(GAME_ID, GAME_EVENT_ID, IsOnOffense) %>%
  mutate(playerTeamReboundProb = sum(rebound_prob)) %>%
  group_by(GAME_ID, GAME_EVENT_ID) %>%
  mutate(NoPlayerRebound = sum(IsRebounder) == 0) %>%
  ungroup() %>%
  mutate(etra = case_when(NoPlayerRebound & teamReboundFlag ~ (rebound_prob/playerTeamReboundProb)*(1-teamRebProb),
                          NoPlayerRebound & !teamReboundFlag ~ -(rebound_prob/playerTeamReboundProb)*teamRebProb,
                          IsRebounder ~ 1 - teamRebProb,
                          teamReboundFlag ~ 0,
                          TRUE ~ -teamRebProb*(rebound_prob/playerTeamReboundProb))) %>%
  mutate(era = as.numeric(IsRebounder) - rebound_prob) %>%
  left_join(players_info, by = c("player_id" = "playerid")) %>%
  ungroup() %>%
  group_by(GAME_ID, GAME_EVENT_ID) %>%
  mutate(etra_rank = row_number(etra)) %>%
  mutate(third_worst_etra = rep(etra[etra_rank == 3], 10),
         shot_distance = rep(sqrt((x_loc[IsShooter] - 5.25)^2 + (y_loc[IsShooter] - 25)^2), 10)) %>%
  filter(third_worst_etra < -.1) %>%
  filter(firstname == "Kevin", lastname == "Love") %>%
  arrange(desc(etra))
# GAME_ID == 21500424
# GAME_EVENT_ID == 11

setwd("~/Desktop/Deep_Learning/nba-movement-data/rebounding_data/images/")
playersimages = read.csv("playerspictures.csv") %>%
  mutate(name = as.character(name))

test = image_read("https://web.archive.org/web/20161208080529im_/http://a.espncdn.com/combiner/i?img=/i/headshots/nba/players/full/2774.png")
print(test)
image_crop(test, "600x350+60")

summary_stats3_qualifying = summary_stats3 %>%
  filter(grepl('F', position)|grepl('C', position)) %>%
  filter(Plays > 800) %>%
  mutate(fullname = paste(firstname, lastname)) %>%
  inner_join(playersimages, by = c("fullname" = "name")) %>%
  mutate(url_current = paste0("https://a.espncdn.com/combiner/i?img=/i/headshots/nba/players/full/",
                              as.character(espn_player_id),
                              ".png")) %>%
  mutate(url_old = paste0("https://web.archive.org/web/20161208080529im_/http://a.espncdn.com/combiner/i?img=/i/headshots/nba/players/full/",
                          as.character(espn_player_id),
                          ".png")) %>%
  filter(espn_player_id != 6485) %>%
  arrange(netTeamRebPer100)

stats_plt = summary_stats3_qualifying %>%
  mutate(url_old = as.factor(url_old)) %>%
  mutate(url_old = fct_reorder(url_old, desc(netTeamRebPer100))) %>%
  mutate(eREBper100Plays = eREBperPlay*100)
  
  
ggplot() +
  geom_image(data = stats_plt %>%
               filter(!(fullname %in% c('Doug McDermott',
                                        'Harrison Barnes',
                                        'Jae Crowder',
                                        'Marvin Williams',
                                        'Jeff Green',
                                        'LeBron James',
                                        'Kawhi Leonard'))),
             aes(x = eREBper100Plays, y = netTeamRebPer100, 
                 image=url_old), size=.1, alpha = 0.1) +
  geom_polygon(data = data.frame(x = c(5, 5, 22, 22, 5),
                                 y = c(-2, 2.5, 2.5, -2, -2)),
               aes(x = x, y = y), fill = "white", alpha = 0.7) +
  geom_hline(aes(yintercept = 0), color = "black") + 
  geom_vline(aes(xintercept = 12.5), color = "black") + 
  geom_image(data = stats_plt %>%
               filter(fullname %in% c('Doug McDermott',
                                        'Harrison Barnes',
                                        'Jae Crowder',
                                        'Marvin Williams',
                                        'Jeff Green',
                                        'LeBron James',
                                        'Kawhi Leonard')),
             aes(x = eREBper100Plays, y = netTeamRebPer100, 
                 image=url_old), size=.1) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        panel.grid.major = element_line(colour = 'grey', linetype = "dashed")) +
  labs(x = "Expected Rebounds Per 100 Opportunities",
       y = "Team Rebounds Added Per 100 Opportunities",
       title = "Team Rebounds Added vs. Quality of Opportunity",
       subtitle = "Data From First 40 Games of 2015-2016 Season") +
  xlim(5, 22) +
  ylim(-2,2.5)

ggsave("~/Desktop/Deep_Learning/nba-movement-data/rebounding_data/images/PlayersPlotCstEnv.png",
       height = 7, width = 8)

summary_stats3_justmissedqualifying = summary_stats3 %>%
  filter(grepl('F', position)|grepl('C', position)) %>%
  filter(Plays < 900,
         Plays > 200) %>%
  mutate(fullname = paste(firstname, lastname)) %>%
  inner_join(playersimages, by = c("fullname" = "name")) %>%
  mutate(url_current = paste0("https://a.espncdn.com/combiner/i?img=/i/headshots/nba/players/full/",
                              as.character(espn_player_id),
                              ".png")) %>%
  mutate(url_old = paste0("https://web.archive.org/web/20161208080529im_/http://a.espncdn.com/combiner/i?img=/i/headshots/nba/players/full/",
                          as.character(espn_player_id),
                          ".png")) %>%
  filter(espn_player_id != 6485) %>%
  arrange(netTeamRebPer100)

summary_stats3_justmissedqualifying %>%
  mutate(url_old = as.factor(url_old)) %>%
  mutate(url_old = fct_reorder(url_old, desc(netTeamRebPer100))) %>%
  mutate(eREBper100Plays = eREBperPlay*100) %>%
  ggplot() +
  geom_point(aes(x = eREBper100Plays, y = netTeamRebPer100)) + 
  geom_image(aes(x = eREBper100Plays, y = netTeamRebPer100, 
                 image=url_old), size=.1) +
  geom_hline(aes(yintercept = 0), color = "black") + 
  geom_vline(aes(xintercept = 12.5), color = "black") + 
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        panel.grid.major = element_line(colour = 'grey', linetype = "dashed")) +
  labs(x = "Expected Rebounds Per 100 Opportunities",
       y = "Team Rebounds Added Per 100 Opportunities",
       title = "Team Rebounds Added vs. Quality of Opportunity",
       subtitle = "Data From First 40 Games of 2015-2016 Season")


summary_stats3_qualifyingGuards = summary_stats3 %>%
  filter(!grepl('F', position),
         !grepl('C', position)) %>%
  filter(Plays > 800) %>%
  mutate(fullname = paste(firstname, lastname)) %>%
  inner_join(playersimages, by = c("fullname" = "name")) %>%
  mutate(url_current = paste0("https://a.espncdn.com/combiner/i?img=/i/headshots/nba/players/full/",
                              as.character(espn_player_id),
                              ".png")) %>%
  mutate(url_old = paste0("https://web.archive.org/web/20161208080529im_/http://a.espncdn.com/combiner/i?img=/i/headshots/nba/players/full/",
                          as.character(espn_player_id),
                          ".png")) %>%
  filter(espn_player_id != 6485)

ggsave("~/Desktop/Deep_Learning/nba-movement-data/rebounding_data/images/GuardsPlot.png",
       height = 7, width = 8)

summary_stats3_qualifyingGuards %>%
  mutate(url_old = as.factor(url_old)) %>%
  mutate(url_old = fct_reorder(url_old, desc(netTeamRebPer100))) %>%
  mutate(eREBper100Plays = eREBperPlay*100) %>%
  ggplot() +
  geom_hline(aes(yintercept = 0), color = "black") + 
  geom_vline(aes(xintercept = 6.5), color = "black") + 
  geom_image(aes(x = eREBper100Plays, y = netTeamRebPer100, 
                 image=url_old), size=.1) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        panel.grid.major = element_line(colour = 'grey', linetype = "dashed")) +
  labs(x = "Expected Rebounds Per 100 Opportunities",
       y = "Team Rebounds Added Per 100 Opportunities",
       title = "Rebounds Added vs. Quality of Opportunity",
       subtitle = "Data From First 40 Games of 2015-2016 Season")

summary_stats3_qualifying %>%
  ggplot() +
  geom_point(aes(x = netTeamRebPer100, y = netPlayerRebPer100)) +
  geom_smooth(aes(x = netTeamRebPer100, y = netPlayerRebPer100), method = "lm",
              se = FALSE)

ReboundStealers = summary_stats3_qualifying %>% filter(TEAM_REB_added_per_play < 0, REB_added_per_play > 0) 
UnsungHeroes = summary_stats3_qualifying %>% filter(TEAM_REB_added_per_play > 0, REB_added_per_play < 0) 

summary_stats3disc = data_tot3 %>%
  filter(player_id != -1) %>%
  mutate(teamRebProb = if_else(IsOnOffense, 1 - defRebProbability,
                               defRebProbability),
         teamRebShare = rebound_prob/teamRebProb,
         teamReboundFlag = if_else(IsOnOffense, !IsDefensiveRebound,
                                   IsDefensiveRebound)) %>%
  filter(!is.na(teamRebProb)) %>%
  group_by(GAME_ID, GAME_EVENT_ID, IsOnOffense) %>%
  mutate(playerTeamReboundProb = sum(rebound_prob)) %>%
  group_by(GAME_ID, GAME_EVENT_ID) %>%
  mutate(NoPlayerRebound = sum(IsRebounder) == 0) %>%
  ungroup() %>%
  mutate(etra = case_when(NoPlayerRebound & teamReboundFlag ~ (rebound_prob/playerTeamReboundProb)*(1-teamRebProb),
                          NoPlayerRebound & !teamReboundFlag ~ -(rebound_prob/playerTeamReboundProb)*teamRebProb,
                          IsRebounder ~ 1 - teamRebProb,
                          teamReboundFlag ~ 0,
                          TRUE ~ -teamRebProb*(rebound_prob/playerTeamReboundProb))) %>%
  mutate(era = as.numeric(IsRebounder) - rebound_prob) %>%
  group_by(IsOnOffense, player_id) %>%
  summarize(n_games = length(unique(GAME_ID)),
            Plays = n(),
            REB = sum(IsRebounder, na.rm = TRUE),
            REB_perc = REB/Plays,
            RebPer100 = REB_perc*100,
            eREB = sum(rebound_prob, na.rm = TRUE),
            netReb = sum(era, na.rm = TRUE),
            REB_added_per_play = mean(era, na.rm = TRUE),
            netPlayerRebPer100 = 100*REB_added_per_play,
            eREBperPlay = sum(rebound_prob, na.rm = TRUE)/Plays,
            netTeamReb = sum(etra, na.rm = TRUE),
            TEAM_REB_added_per_play = mean(etra, na.rm = TRUE),
            netTeamRebPer100 = TEAM_REB_added_per_play*100,
            TeamRebAddedPerReb = sum(if_else(etra > 0, etra, 0))/
              sum(if_else(etra > 0, 1, 0))) %>%
  arrange(IsOnOffense,desc(netTeamReb)) %>%
  left_join(players_info, by = c("player_id" = "playerid")) %>%
  select(position, firstname, lastname, player_id, everything()) %>%
  filter(Plays > 400) %>%
  filter(grepl('F', position)|grepl('C', position)) %>%
  select(-Plays, -n_games) %>%
  pivot_wider(names_from = IsOnOffense,
              values_from = contains("REB")) %>%
  filter(!is.na(netTeamRebPer100_TRUE),
         !is.na(netTeamRebPer100_FALSE)) %>%
  select(position, firstname, lastname, netTeamRebPer100_TRUE, netTeamRebPer100_FALSE)


summary_stats3discplt = summary_stats3disc %>%
  mutate(fullname = paste(firstname, lastname)) %>%
  inner_join(playersimages, by = c("fullname" = "name")) %>%
  mutate(url_current = paste0("https://a.espncdn.com/combiner/i?img=/i/headshots/nba/players/full/",
                      as.character(espn_player_id),
                      ".png")) %>%
  mutate(url_old = paste0("https://web.archive.org/web/20161208080529im_/http://a.espncdn.com/combiner/i?img=/i/headshots/nba/players/full/",
                      as.character(espn_player_id),
                      ".png")) %>%
  filter(espn_player_id != 6485) %>%
  arrange(netTeamRebPer100_TRUE)

summary_stats3discplt %>%
  ggplot() +
  geom_point(aes(x = netTeamRebPer100_FALSE, y = netTeamRebPer100_TRUE)) +
  labs(x = "Defensive Net REB Per 100 Possessions",
       y = "Offensive Net REB Per 100 Possessions") +
  geom_image(aes(x = netTeamRebPer100_FALSE, y = netTeamRebPer100_TRUE, 
                 image=url_old), size=.07) +
  geom_hline(aes(yintercept = 0), color = "black") + 
  geom_vline(aes(xintercept = 0), color = "black") + 
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        panel.grid.major = element_line(colour = 'grey', linetype = "dashed"))



# Getting Team Level Stats ------------------------------------------------
setwd("~/Desktop/Deep_Learning/nba-movement-data/data/events/")
scripts = dir()
scripts = scripts[grep(".csv", scripts)]

events_data = data.frame()
for(script in scripts){
  x = read.csv(script) %>%
    select(GAME_ID, starts_with("PLAYER1"))
  if(dim(x)[1] > 0){
    events_data = rbind(events_data,
                        x)
    
  }
}

team_mapping = events_data %>%
  filter(!is.na(PLAYER1_TEAM_ID)) %>%
  mutate(TeamName = paste(PLAYER1_TEAM_CITY, PLAYER1_TEAM_NICKNAME)) %>%
  rename(team_id = PLAYER1_TEAM_ID,
         TeamAbbr = PLAYER1_TEAM_ABBREVIATION) %>%
  select(team_id, TeamName, TeamAbbr) %>%
  distinct()


# Get Team Winning Percentages --------------------------------------------

setwd("~/Desktop/Deep_Learning/nba-movement-data/data/events/")
scripts = dir()
scripts = scripts[grep(".csv", scripts)]

events_data = data.frame()
for(script in scripts){
  x = read.csv(script) %>%
    select(GAME_ID, SCOREMARGIN, HOMEDESCRIPTION,
           VISITORDESCRIPTION, PLAYER1_TEAM_ID)
  if(dim(x)[1] > 0){
    events_data = rbind(events_data,
                        x)
    
  }
}

library(modeest)
hometeam = events_data %>%
  group_by(GAME_ID) %>%
  filter(!grepl("Jump Ball", HOMEDESCRIPTION),
         !grepl("Jump Ball", VISITORDESCRIPTION),
         VISITORDESCRIPTION == "",
         HOMEDESCRIPTION != "",
         !is.na(PLAYER1_TEAM_ID)) %>%
  summarize(homeTeam = mlv(PLAYER1_TEAM_ID, method='mfv'))

awayteam = events_data %>%
  group_by(GAME_ID) %>%
  filter(!grepl("Jump Ball", HOMEDESCRIPTION),
         !grepl("Jump Ball", VISITORDESCRIPTION),
         HOMEDESCRIPTION == "",
         VISITORDESCRIPTION != "",
         !is.na(PLAYER1_TEAM_ID)) %>%
  summarize(awayTeam = mlv(PLAYER1_TEAM_ID, method='mfv'))

winningTeam = events_data %>%
  filter(!is.na(SCOREMARGIN)) %>%
  group_by(GAME_ID) %>%
  filter(row_number() == n()) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(winningTeam = if_else(as.integer(as.character(SCOREMARGIN)) > 0, "home", "away")) %>%
  select(GAME_ID, winningTeam)
  

teamsInGame = hometeam %>%
  inner_join(awayteam) %>%
  inner_join(winningTeam) %>%
  filter(homeTeam != awayTeam) %>%
  mutate(winningTeam = if_else(winningTeam == "away", awayTeam, homeTeam)) %>%
  pivot_longer(cols = c("homeTeam", "awayTeam"),
               names_to = "home_away",
               values_to = "team_id") %>%
  mutate(result = winningTeam == team_id) %>%
  ungroup() %>%
  group_by(team_id) %>%
  summarize(win_perc = mean(result)) %>%
  arrange(desc(win_perc))


summary_stats4 = data_tot3 %>%
  filter(player_id != -1) %>%
  mutate(teamRebProb = if_else(IsOnOffense, 1 - defRebProbability,
                               defRebProbability),
         teamReboundFlag = if_else(IsOnOffense, !IsDefensiveRebound,
                                   IsDefensiveRebound)) %>%
  inner_join(team_mapping) %>%
  select(team_id, TeamName, TeamAbbr, 
         GAME_ID, GAME_EVENT_ID, teamRebProb, teamReboundFlag) %>%
  distinct() %>%
  filter(!is.na(teamRebProb)) %>%
  group_by(team_id, TeamName, TeamAbbr) %>%
  summarize(Plays = n(),
            eREB = sum(teamRebProb, na.rm = TRUE),
            eREBperPlay = eREB/Plays,
            REB = sum(teamReboundFlag, na.rm = TRUE),
            netREB = REB - eREB,
            netREBperPlay = netREB/Plays,
            netREBper100 = 100*netREBperPlay) %>%
  arrange(desc(netREBperPlay))

summary_stats4disc = data_tot3 %>%
  filter(player_id != -1) %>%
  mutate(teamRebProb = if_else(IsOnOffense, 1 - defRebProbability,
                               defRebProbability),
         teamReboundFlag = if_else(IsOnOffense, !IsDefensiveRebound,
                                   IsDefensiveRebound)) %>%
  inner_join(team_mapping) %>%
  select(team_id, TeamName, TeamAbbr, 
         GAME_ID, GAME_EVENT_ID, teamRebProb, teamReboundFlag, IsOnOffense) %>%
  distinct() %>%
  filter(!is.na(teamRebProb)) %>%
  group_by(IsOnOffense, team_id, TeamName, TeamAbbr) %>%
  summarize(Plays = n(),
         eREB = sum(teamRebProb, na.rm = TRUE),
         eREBperPlay = eREB/Plays,
         REB = sum(teamReboundFlag, na.rm = TRUE),
         netREB = REB - eREB,
         netREBperPlay = netREB/Plays,
         netREBper100 = 100*netREBperPlay) %>%
  arrange(IsOnOffense, desc(netREBperPlay)) %>%
  select(-Plays) %>%
  pivot_wider(names_from = IsOnOffense,
              values_from = contains("REB")) %>%
  mutate(team_logo = case_when(TeamAbbr == "ATL" ~ "https://content.sportslogos.net/logos/6/220/full/9168_atlanta_hawks-primary-2016.png",
                               TeamAbbr == "BKN" ~ "https://content.sportslogos.net/logos/6/3786/full/345_brooklyn-nets-secondary-2013.png",
                               TeamAbbr == "BOS" ~ "https://content.sportslogos.net/logos/6/213/full/slhg02hbef3j1ov4lsnwyol5o.png",
                               TeamAbbr == "CHA" ~ "https://content.sportslogos.net/logos/6/5120/full/1926_charlotte__hornets_-primary-2015.png",
                               TeamAbbr == "CHI" ~ "https://content.sportslogos.net/logos/6/221/full/hj3gmh82w9hffmeh3fjm5h874.png",
                               TeamAbbr == "CLE" ~ "https://content.sportslogos.net/logos/6/222/full/e4701g88mmn7ehz2baynbs6e0.png",
                               TeamAbbr == "DAL" ~ "https://content.sportslogos.net/logos/6/228/full/ifk08eam05rwxr3yhol3whdcm.png",
                               TeamAbbr == "DEN" ~ "https://content.sportslogos.net/logos/6/229/full/8926_denver_nuggets-primary-2019.png",
                               TeamAbbr == "DET" ~ "https://content.sportslogos.net/logos/6/223/full/2164_detroit_pistons-primary-2018.png",
                               TeamAbbr == "GSW" ~ "https://content.sportslogos.net/logos/6/235/full/3152_golden_state_warriors-primary-2020.png",
                               TeamAbbr == "HOU" ~ "https://content.sportslogos.net/logos/6/230/full/6830_houston_rockets-primary-2020.png",
                               TeamAbbr == "IND" ~ "https://content.sportslogos.net/logos/6/224/full/4812_indiana_pacers-primary-2018.png",
                               TeamAbbr == "LAC" ~ "https://content.sportslogos.net/logos/6/236/full/5462_los_angeles_clippers-primary-2016.png",
                               TeamAbbr == "LAL" ~ "https://content.sportslogos.net/logos/6/237/full/uig7aiht8jnpl1szbi57zzlsh.png",
                               TeamAbbr == "MEM" ~ "https://content.sportslogos.net/logos/6/231/full/4373_memphis_grizzlies-primary-2019.png",
                               TeamAbbr == "MIA" ~ "https://content.sportslogos.net/logos/6/214/full/burm5gh2wvjti3xhei5h16k8e.gif",
                               TeamAbbr == "MIL" ~ "https://content.sportslogos.net/logos/6/225/full/8275_milwaukee_bucks-primary-2016.png",
                               TeamAbbr == "MIN" ~ "https://content.sportslogos.net/logos/6/232/full/9669_minnesota_timberwolves-primary-2018.png",
                               TeamAbbr == "NOP" ~ "https://content.sportslogos.net/logos/6/4962/full/2681_new_orleans_pelicans-primary-2014.png",
                               TeamAbbr == "NYK" ~ "https://content.sportslogos.net/logos/6/216/full/2nn48xofg0hms8k326cqdmuis.gif",
                               TeamAbbr == "OKC" ~ "https://content.sportslogos.net/logos/6/2687/full/khmovcnezy06c3nm05ccn0oj2.png",
                               TeamAbbr == "ORL" ~ "https://content.sportslogos.net/logos/6/217/full/wd9ic7qafgfb0yxs7tem7n5g4.gif",
                               TeamAbbr == "PHI" ~ "https://content.sportslogos.net/logos/6/218/full/7034_philadelphia_76ers-primary-2016.png",
                               TeamAbbr == "PHX" ~ "https://content.sportslogos.net/logos/6/238/full/4370_phoenix_suns-primary-2014.png",
                               TeamAbbr == "POR" ~ "https://content.sportslogos.net/logos/6/239/full/9725_portland_trail_blazers-primary-2018.png",
                               TeamAbbr == "SAC" ~ "https://content.sportslogos.net/logos/6/240/full/4043_sacramento_kings-primary-2017.png",
                               TeamAbbr == "SAS" ~ "https://content.sportslogos.net/logos/6/233/full/827.png",
                               TeamAbbr == "TOR" ~ "https://content.sportslogos.net/logos/6/227/full/4578_toronto_raptors-primary-2016.png",
                               TeamAbbr == "UTA" ~ "https://content.sportslogos.net/logos/6/234/full/6749_utah_jazz-primary-2017.png",
                               TeamAbbr == "WAS" ~ "https://content.sportslogos.net/logos/6/219/full/5671_washington_wizards-primary-2016.png",
                               TRUE ~ "error"))



summary_stats4disc %>%
  ungroup() %>%
  mutate(team_logo = as.factor(team_logo)) %>%
  mutate(team_logo = fct_reorder(team_logo, desc(netREBper100_TRUE))) %>%
  ggplot() +
  geom_point(aes(x = netREBper100_FALSE, y = netREBper100_TRUE)) +
  labs(x = "Defensive Net REB Per 100 Opportunities",
       y = "Offensive Net REB Per 100 Opportunities",
       title = "Offensive vs. Defensive Team Net Rebound Ratings",
       subtitle = "Data From First 40 Games of 2015-2016 Season") +
  geom_image(aes(x = netREBper100_FALSE, y = netREBper100_TRUE, 
                 image=team_logo), size = 0.06) +
                 #size = I(win_perc/8))) +
  geom_hline(aes(yintercept = 0), color = "black") + 
  geom_vline(aes(xintercept = 0), color = "black") + 
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        panel.grid.major = element_line(colour = 'grey', linetype = "dashed"))

ggsave("~/Desktop/Deep_Learning/nba-movement-data/rebounding_data/images/TeamsPlot.png",
       height = 5, width = 6)

baselineTeamPreds = data_tot3 %>%
  filter(player_id != -1) %>%
  mutate(teamRebProb = if_else(IsOnOffense, 1 - defRebProbability,
                               defRebProbability),
         teamReboundFlag = if_else(IsOnOffense, !IsDefensiveRebound,
                                   IsDefensiveRebound)) %>%
  inner_join(team_mapping) %>%
  select(team_id, TeamName, TeamAbbr, 
         GAME_ID, GAME_EVENT_ID, teamRebProb, teamReboundFlag, IsOnOffense) %>%
  distinct() %>%
  group_by(IsOnOffense) %>%
  summarize(reboundPerc = mean(teamReboundFlag),
            reboundProb = mean(teamRebProb))


# Player-Team Stats -------------------------------------------------------
player_plays = data_tot3 %>%
  filter(player_id != -1,
         rebound_prob > .1) %>%
  select(player_id, team_id, GAME_ID, GAME_EVENT_ID) %>%
  distinct() %>%
  arrange(player_id) 


summary_stats5pt1 = data_tot3 %>%
  filter(player_id != -1) %>%
  mutate(teamRebProb = if_else(IsOnOffense, 1 - defRebProbability,
                               defRebProbability),
         teamReboundFlag = if_else(IsOnOffense, !IsDefensiveRebound,
                                   IsDefensiveRebound)) %>%
  inner_join(team_mapping) %>%
  filter(!is.na(teamRebProb)) %>%
  select(team_id, TeamName, TeamAbbr, player_id,
         GAME_ID, GAME_EVENT_ID, teamRebProb, teamReboundFlag) %>%
  inner_join(player_plays) %>%
  group_by(player_id, team_id, TeamName, TeamAbbr) %>%
  summarize(Plays = n(),
            eREB = sum(teamRebProb, na.rm = TRUE),
            eREBperPlay = eREB/Plays,
            REB = sum(teamReboundFlag, na.rm = TRUE),
            netREB = REB - eREB,
            netREBperPlay = netREB/Plays,
            netREBper100 = 100*netREBperPlay) %>%
  left_join(players_info, by = c("player_id" = "playerid")) %>%
  select(position, firstname, lastname, player_id, everything()) %>%
  arrange(desc(netREB)) %>%
  filter(Plays > 200) %>%
  filter(grepl('F', position)|grepl('C', position),
         !grepl('G', position))

# for all players, get a list of plays that they played in
player_plays = data_tot3 %>%
  filter(player_id != -1) %>%
  select(player_id, team_id, GAME_ID, GAME_EVENT_ID) %>%
  distinct() %>%
  arrange(player_id) 

player_team_not_plays = data_tot3 %>%
  filter(player_id != -1) %>%
  select(team_id, GAME_ID, GAME_EVENT_ID) %>%
  distinct() %>%
  inner_join(player_plays %>%
               select(player_id, team_id) %>%
               distinct()) %>%
  anti_join(player_plays) %>%
  arrange(player_id)

summary_stats5pt2 = player_team_not_plays %>%
  inner_join(data_tot3 %>%
               filter(player_id != -1) %>%
               mutate(teamRebProb = if_else(IsOnOffense, 1 - defRebProbability,
                                            defRebProbability),
                      teamReboundFlag = if_else(IsOnOffense, !IsDefensiveRebound,
                                                IsDefensiveRebound)) %>%
               inner_join(team_mapping) %>%
               select(team_id, TeamName, TeamAbbr, 
                      GAME_ID, GAME_EVENT_ID, teamRebProb, teamReboundFlag) %>%
               distinct(),
             by = c("team_id", "GAME_ID", "GAME_EVENT_ID")) %>%
  select(team_id, TeamName, TeamAbbr, player_id,
         GAME_ID, GAME_EVENT_ID, teamRebProb, teamReboundFlag) %>%
  group_by(player_id, team_id, TeamName, TeamAbbr) %>%
  summarize(not_on_Plays = n(),
            not_on_eREB = sum(teamRebProb, na.rm = TRUE),
            not_on_eREBperPlay = not_on_eREB/not_on_Plays,
            not_on_REB = sum(teamReboundFlag, na.rm = TRUE),
            not_on_netREB = not_on_REB - not_on_eREB,
            not_on_netREBperPlay = not_on_netREB/not_on_Plays,
            not_on_netREBper100 = 100*not_on_netREBperPlay) %>%
  left_join(players_info, by = c("player_id" = "playerid")) %>%
  select(position, firstname, lastname, player_id, everything()) %>%
  arrange(desc(not_on_eREB))

summary_stats5 = summary_stats5pt1 %>%
  inner_join(summary_stats5pt2) %>%
  filter(Plays > 500, 
         not_on_Plays > 200) %>%
  mutate(netREBper100_diff = netREBper100 - not_on_netREBper100) %>%
  arrange(desc(netREBper100_diff))

# Checking Player Rebound Probs
shooter_check = data_tot3 %>%
  filter(IsShooter) %>%
  summarize(reboundFreq = mean(IsRebounder),
            reboundProb = mean(rebound_prob))

small_probs_check1 = data_tot3 %>%
  mutate(rebound_prob_group = (floor(rebound_prob*100) + .5)/100,
         miss = IsRebounder - rebound_prob_group) %>%
  group_by(rebound_prob_group) %>%
  summarize(count = n(),
            reboundFreq = mean(IsRebounder),
            reboundProb = mean(rebound_prob),
            miss = mean(miss),
            miss_perc = miss/reboundProb) %>%
  arrange(rebound_prob_group)


# Off Reb Prob vs. Distance -----------------------------------------------

offRebvsDist = data_tot3 %>%
  filter(IsShooter) %>%
  mutate(offRebProbability = 1- defRebProbability,
         dist = sqrt((x_loc - 5.25)^2 + (y_loc - 25)^2)) %>%
  mutate(dist_disc = floor(dist) + 0.5) %>%
  group_by(dist_disc) %>%
  summarize(count = n(),
            offRebProb = 1 - mean(IsDefensiveRebound)) %>%
  filter(count > 100) %>%
  arrange(dist_disc)

offRebvsDist %>%
  ggplot() +
  geom_point(aes(x = dist_disc, y = offRebProb)) +
  geom_smooth(aes(x = dist_disc, y = offRebProb))

correct_team_prob = data_tot3 %>%
  group_by(GAME_ID, GAME_EVENT_ID) %>%
  filter(IsRebounder) %>%
  mutate(correct_pred = IsDefensiveRebound*(defRebProbability >= .5)) %>%
  ungroup() %>%
  summarize(accuracy = mean(correct_pred))

