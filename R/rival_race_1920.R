library(fplscrapR)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(gganimate)

`%notin%` <- Negate(`%in%`)

#alwayscheating as example
my_id <- 121959 #FPL_Dave
rival_id <- 294 #toungueinloftus

#pull data for self and rival
my_season <- fplscrapR::get_entry_season(entryid = my_id)
rival_season <- fplscrapR::get_entry_season(entryid = rival_id)

#Get name
my_name <- my_season$name[[1]]
rival_name <- rival_season$name[[1]]

#Set title  
chart_title <- paste(my_name," vs.", rival_name)

#create data frame for own data. Add Id number for GW based on row number. Add variable with player name.

df_player <- my_season %>% 
  dplyr::mutate(points = total_points, gameweek =event) %>%
  dplyr::select(name, gameweek, points)

df_rival <- rival_season %>% 
  dplyr::mutate(points = total_points, gameweek =event) %>%
  dplyr::select(name, gameweek, points)

df <- rbind(df_player, df_rival) %>% 
  dplyr::mutate(name = factor(name)) %>%
  dplyr::filter(gameweek %notin% seq(30,38)) %>%
  dplyr::mutate(gameweek = as.integer(ifelse(gameweek > 38, gameweek -9 , gameweek)))


p <- ggplot(df,
       aes(x=gameweek, y=points, group=name, colour=name)) +
  geom_line() + 
  labs(title = "FPL Head-to-Head",
       subtitle = chart_title,
       x = "Gameweek", 
       y = "Total Points") +
  theme(legend.position = c(0.8, 0.15), 
        legend.title = element_blank(), 
        plot.subtitle = element_text(size=14), 
        legend.direction = "vertical") +
  ggthemes::theme_fivethirtyeight() 

p + gganimate::transition_reveal(gameweek)

gganimate::anim_save("rival_race.gif")
