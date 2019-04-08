############################################################
#### Set up code for other scripts run for presentation ####
############################################################

library(magrittr)
library(dplyr)
library(ggplot2)

# theme function to make transparent chart to use with revealjs league theme
theme_clear <- function(){
  
  theme(panel.background = element_rect(fill = "white", color = "black"),
        plot.background =  element_rect(fill = "transparent", color = NA))
  
}

Sys.setenv(PRES_SETUP = TRUE)