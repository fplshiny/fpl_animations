#Run code up to the "get_league" command and enter credentials before running remaining code. 
#The FPL website requires this so the fplscrapr package had to implement it in this way.

library(fplscrapR)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(gganimate)

`%notin%` <- Negate(`%in%`)

#Set parameters 
start_gw <- 1
end_gw <- 38
player_limit <- 30
league_code <- 314 #overall league


#set animation paramaters
fpgw <- 10 #frames per GW
fps <- 10 #frames per second
end_pause <- 150

#set labels
xlab <- "FPL Manager"
ylab <- "Points"
caption <- "Top 30 finishers"

#Get league info
league <- fplscrapR::get_league(leagueid = league_code)

#Put key info into a data frame and set a limit on number of FPL players
df<-league$standings$results

df<-df %>%
  head(player_limit)

#Create mini league data frame
df_ml <- df$entry %>% 
  purrr::map_dfr(~fplscrapR::get_entry_season(.x))

#Only select the columns we need and renumber the gameweek numbers which got messed up by the pandemic.
df_ml <- df_ml %>% 
  dplyr::select(name, event, points, total_points, overall_rank) %>%
  dplyr::filter(event %notin% seq(30,38)) %>%
  dplyr::mutate(event = as.integer(ifelse(event > 38, event -9 ,event)))

#Create variable to deal with ordering
df_ml<-df_ml %>%
  dplyr::group_by(event) %>%
  dplyr::mutate(ordering = rank(total_points,ties.method= "first")) %>%
  dplyr::ungroup() 

#function for barchart race
make_barchart_race <- function(title = "Title",
                               xlab = "x",
                               ylab = "y",
                               fps = 10,
                               end_pause = 100){
  p <- ggplot(data = df_ml, 
              aes(x = ordering, 
                  y = total_points, 
                  fill = name, 
                  label = name)) +
    geom_bar(stat = "identity", colour = "black") +
    coord_flip(clip = "off", expand = FALSE) +
    geom_text(aes(label = name), hjust = -0.1) +
    labs(title = title,
         subtitle ='GW: {frame_time}',
         x = xlab,
         y = ylab,
         caption = caption) +
    ggthemes::theme_tufte(14,"Avenir") +
    theme(aspect.ratio = 4/3,
          legend.position = "none",
          plot.title = element_text(hjust = -0.1, size = 22),
          axis.ticks.y = element_blank(),
          axis.text.y  = element_blank(),
          plot.margin = unit(c(2, 6, 2, 1),"cm")) +
    gganimate::transition_time(event) +
    gganimate::ease_aes('cubic-in-out')

  gganimate::animate(p, 
                     nframes = fpgw * end_gw + end_pause, 
                     fps = fps, end_pause = end_pause, 
                     height = 675, 
                     width = 1200)
  
}

#Run function
make_barchart_race(title = league$league$name, 
                   xlab = xlab, 
                   ylab = ylab, 
                   fps = fps, 
                   end_pause = end_pause)

#save to local directory
gganimate::anim_save("overall.gif")
