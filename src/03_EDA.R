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
library(gridExtra)

files = dir()
files = files[grep("_rebounding_data", files)]
length(unique(files))

data_tot = data.frame()

for(file in files){
  data_tot = rbind(data_tot, 
                   read.csv(file) %>%
                     select(-X))
}

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


# Adding the Landing Data -------------------------------------------------

data_tot = data_tot %>%
  left_join(landing_data_tot %>% select(-quarter))

setwd("~/Desktop/Deep_Learning/nba-movement-data/rebounding_data/data_sets")
clean_plays = read.csv("landing_isotropic_score_augmented2.csv")

data_tot = data_tot %>%
  inner_join(clean_plays %>%
               select(GAME_ID, GAME_EVENT_ID) %>%
               distinct())

landing_data_tot = landing_data_tot %>%
  inner_join(clean_plays %>%
               select(GAME_ID, GAME_EVENT_ID) %>%
               distinct())


# looking at baseline: predicting player closest to basket!
baseline_accuracy = data_tot %>%
  filter(!IsBall) %>%
  filter(!is.na(x_hit)) %>%
  mutate(DistanceFromLanding = sqrt((x_loc - x_hit)^2 + (y_loc - y_hit)^2)) %>%
  group_by(GAME_ID, GAME_EVENT_ID) %>%
  mutate(IsClosestToLanding = DistanceFromLanding == min(DistanceFromLanding)) %>%
  summarize(correct = max(as.numeric(IsClosestToLanding)*as.numeric(IsRebounder))) %>%
  ungroup() %>%
  summarize(baseline_accuracy = mean(correct))

landing_data_tot %>%
  ggplot() +
  geom_hex(aes(x = x_hit, y = y_hit))

ggplot(data=data.frame(x=1,y=1),aes(x,y))+
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
  geom_path(data=data.frame(x=c(-22,-22,-22000:(-1)/1000,1:22000/1000,22,22),y=-c(47,47-169/12,41.75-sqrt(23.75^2-c(-22000:(-1)/1000,1:22000/1000)^2),47-169/12,47)),aes(x=x,y=y)) +
  geom_hex(data = landing_data_tot %>%
             filter(x_hit < 30,
                    y_hit > 0,
                    y_hit < 50,
                    x_hit > -1),
           aes(x = y_hit - 25, y = x_hit - 47), alpha = 0.7) + 
  ###plotting players
  coord_fixed() +
  labs(title = "Eventual Defensive-Rebounder Locations")

data_tot %>%
  ggplot() +
  geom_density(aes(x = x_loc))

data_tot %>%
  ggplot() +
  geom_density(aes(x = y_loc))

data_tot %>%
  filter(!IsBall) %>%
  ggplot() +
  geom_density(aes(x = x_speed)) +
  xlim(-20, 20)

data_tot %>%
  filter(!IsBall) %>%
  ggplot() +
  geom_density(aes(x = y_speed))

data_tot %>%
  filter(IsBall) %>%
  ggplot() +
  geom_density(aes(x = x_speed))

data_tot %>%
  filter(IsBall) %>%
  ggplot() +
  geom_density(aes(x = y_speed))



# Checking for number of massive ball speeds ------------------------------
bad_ball_speeds = data_tot %>%
  inner_join(data_tot %>%
  filter(player_id == -1) %>%
  filter((abs(x_speed) > 100)|(abs(y_speed > 100))) %>%
  select(GAME_ID, GAME_EVENT_ID))


data_tot %>%
  filter(IsBall) %>%
  ggplot() +
  geom_density(aes(x = radius))


data_tot %>%
  filter(IsBall) %>%
  ggplot() +
  geom_density(aes(x = radius_acc))

data_tot %>%
  filter(IsShooter) %>%
  ggplot() +
  geom_hex(aes(x = x_loc, y = y_loc))

rebounder_error = data_tot %>%
  filter(IsRebounder,
         (x_loc > 47)|(abs(y_loc - 25) > 26)) %>%
  select(GAME_ID, GAME_EVENT_ID) %>%
  distinct()


# Looking at Missed Dunks -------------------------------------------------
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

rebound_plays = events_data %>%
  mutate(Description = paste0(HOMEDESCRIPTION, VISITORDESCRIPTION)) %>%
  filter(grepl("rebound", str_to_lower(Description))) %>%
  distinct()

missed_shots_plays = events_data %>%
  mutate(Description = paste0(HOMEDESCRIPTION, VISITORDESCRIPTION)) %>%
  filter(grepl("^MISS ", Description),
         !grepl("free throw", str_to_lower(Description))) %>%
  filter(!(grepl('block', str_to_lower(HOMEDESCRIPTION)) & 
            grepl('blk', str_to_lower(HOMEDESCRIPTION))),
           !(grepl('block', str_to_lower(VISITORDESCRIPTION)) & 
              grepl('blk', str_to_lower(VISITORDESCRIPTION)))) %>%
  filter(!grepl(' tip layup', str_to_lower(HOMEDESCRIPTION)),
         !grepl(' tip layup', str_to_lower(VISITORDESCRIPTION)),
         !grepl('putback', str_to_lower(HOMEDESCRIPTION)),
         !grepl('putback', str_to_lower(VISITORDESCRIPTION))) %>%
  distinct() %>%
  select(-HOMEDESCRIPTION, -VISITORDESCRIPTION) %>%
  anti_join(shots_with_descs)

setwd("~/Desktop/Deep_Learning/nba-movement-data/data/shots/")
shots_data = read.csv("shots_fixed.csv") %>%
  filter(GAME_ID == 21500391)

check_events = events_data %>%
  filter(GAME_ID == 21500391) %>%
  filter(EVENTMSGTYPE == 2)

shots_with_descs = data_tot %>%
  select(GAME_ID, GAME_EVENT_ID) %>%
  distinct() %>%
  left_join(events_data,
            by = c("GAME_ID", "GAME_EVENT_ID" = "EVENTNUM")) %>%
  mutate(Description = paste0(HOMEDESCRIPTION, VISITORDESCRIPTION)) %>%
  select(-HOMEDESCRIPTION, -VISITORDESCRIPTION)

dunks = shots_with_descs %>%
  filter(grepl("dunk", str_to_lower(Description)))



# Plots -------------------------------------------------------------------

p2 = ggplot(data=data.frame(x=1,y=1),aes(x,y))+
  scale_fill_gradientn(colours = c("navajowhite1", "navajowhite1", "dodgerblue4", "lightblue3", "red", "firebrick3"),
                       values = scales::rescale(c(0, .01, 2, 20, 120, 500))) + 
  geom_hex(data = landing_data_tot,
           aes(x = y_hit - 25, y = x_hit - 47), alpha = .7) + 
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
  geom_path(data=data.frame(x=c(-22,-22,-22000:(-1)/1000,1:22000/1000,22,22),y=-c(47,47-169/12,41.75-sqrt(23.75^2-c(-22000:(-1)/1000,1:22000/1000)^2),47-169/12,47)),aes(x=x,y=y)) +
  ###plotting players
  theme(panel.background = element_rect(fill = "navajowhite1", colour = 'black'),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text.x = element_text(size = 15),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        legend.position="none",
        plot.title = element_text(hjust = 0.5)
  ) + 
  coord_fixed() +
  labs(title = "Estimated Landing Locations",
       x = "",
       y = "") + 
  ylim(-50, 0)


p1 = ggplot(data=data.frame(x=1,y=1),aes(x,y))+
  scale_fill_gradientn(colours = c("dodgerblue4", "lightblue3", "red", "firebrick3"),
                       values = scales::rescale(c(0, 20, 120, 500))) + 
  geom_hex(data = data_tot %>%
             filter(IsShooter),
           aes(x = y_loc - 25, y = x_loc - 47), alpha = 0.7) + 
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
  geom_path(data=data.frame(x=c(-22,-22,-22000:(-1)/1000,1:22000/1000,22,22),y=-c(47,47-169/12,41.75-sqrt(23.75^2-c(-22000:(-1)/1000,1:22000/1000)^2),47-169/12,47)),aes(x=x,y=y)) +
  ###plotting players
  theme(panel.background = element_rect(fill = "navajowhite1", colour = 'black'),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text.x = element_text(size = 15),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        legend.position="none",
        plot.title = element_text(hjust = 0.5)
  ) + 
  coord_fixed() +
  labs(title = "Shot Locations",
       x = "",
       y = "") +
  ylim(-50, 0)

grid.arrange(p1, p2, nrow = 1)

x = arrangeGrob(p1, p2, nrow = 1)

ggsave("~/Desktop/Deep_Learning/nba-movement-data/rebounding_data/images/EDAplots.png", x, 
       height = 7, width = 10)

ggplot(data=data.frame(x=1,y=1),aes(x,y))+
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
  geom_path(data=data.frame(x=c(-22,-22,-22000:(-1)/1000,1:22000/1000,22,22),y=-c(47,47-169/12,41.75-sqrt(23.75^2-c(-22000:(-1)/1000,1:22000/1000)^2),47-169/12,47)),aes(x=x,y=y)) +
  geom_hex(data = data_tot %>%
             filter(!IsShooter,
                    IsOnOffense),
           aes(x = y_loc - 25, y = x_loc - 47), alpha = 0.7) + 
  theme(panel.background = element_rect(colour = 'black'),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text.x = element_text(size = 15),
        axis.text=element_blank(),
        axis.ticks=element_blank()
  ) + 
  ###plotting players
  coord_fixed() +
  labs(title = "Offense Non-Shooter Locations")

ggplot(data=data.frame(x=1,y=1),aes(x,y))+
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
  geom_path(data=data.frame(x=c(-22,-22,-22000:(-1)/1000,1:22000/1000,22,22),y=-c(47,47-169/12,41.75-sqrt(23.75^2-c(-22000:(-1)/1000,1:22000/1000)^2),47-169/12,47)),aes(x=x,y=y)) +
  geom_hex(data = data_tot %>%
             filter(!IsOnOffense),
           aes(x = y_loc - 25, y = x_loc - 47), alpha = 0.7) + 
  ###plotting players
  coord_fixed() +
  labs(title = "Defense Locations")

ggplot(data=data.frame(x=1,y=1),aes(x,y))+
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
  geom_path(data=data.frame(x=c(-22,-22,-22000:(-1)/1000,1:22000/1000,22,22),y=-c(47,47-169/12,41.75-sqrt(23.75^2-c(-22000:(-1)/1000,1:22000/1000)^2),47-169/12,47)),aes(x=x,y=y)) +
  geom_hex(data = data_tot %>%
             filter(IsRebounder),
           aes(x = y_loc - 25, y = x_loc - 47), alpha = 0.7) + 
  ###plotting players
  coord_fixed() +
  labs(title = "Eventual Rebounder Locations")

ggplot(data=data.frame(x=1,y=1),aes(x,y))+
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
  geom_path(data=data.frame(x=c(-22,-22,-22000:(-1)/1000,1:22000/1000,22,22),y=-c(47,47-169/12,41.75-sqrt(23.75^2-c(-22000:(-1)/1000,1:22000/1000)^2),47-169/12,47)),aes(x=x,y=y)) +
  geom_hex(data = data_tot %>%
             filter(IsRebounder, 
                    IsOnOffense),
           aes(x = y_loc - 25, y = x_loc - 47), alpha = 0.7) + 
  ###plotting players
  coord_fixed() +
  labs(title = "Eventual Offensive-Rebounder Locations")

ggplot(data=data.frame(x=1,y=1),aes(x,y))+
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
  geom_path(data=data.frame(x=c(-22,-22,-22000:(-1)/1000,1:22000/1000,22,22),y=-c(47,47-169/12,41.75-sqrt(23.75^2-c(-22000:(-1)/1000,1:22000/1000)^2),47-169/12,47)),aes(x=x,y=y)) +
  geom_hex(data = data_tot %>%
             filter(IsRebounder, 
                    !IsOnOffense),
           aes(x = y_loc - 25, y = x_loc - 47), alpha = 0.7) + 
  ###plotting players
  coord_fixed() +
  labs(title = "Eventual Defensive-Rebounder Locations")
