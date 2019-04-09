#############################################
#### Script that sets up data for motion ####
#############################################

library(ggplot)
library(gganimate)

set.seed(42)
nSites <- 8
txtClr <- "white"
cts <- sample(1:50, nSites*5)
grpOrder <- order(cts, decreasing = TRUE)
pData <- data.frame(site = LETTERS[seq(nSites)], 
                    deviations = cts,
                    year = rep(2014:2018, each = nSites))

aniPlot <- ggplot(pData, aes(x = site, y = deviations)) +
  geom_bar(stat = "identity", width = 0.98) +
  
  labs(x = "Site", y = "Deviations") + 
  coord_flip() +
  
  theme(panel.background = element_rect(fill = "white", color = "black"),
        plot.background =  element_rect(fill = "transparent", color = NA),
        axis.title = element_text(size = 24, color = "white"),
        axis.text = element_text(size = 20, color = "white"),
        plot.title = element_text(size = 28, color = "white")) +
  
  transition_states(
    year,
    transition_length = 2,
    state_length = 2
  ) +
  
  enter_fade() +
  exit_shrink() +
  ease_aes('sine-in-out') +
  
  ggtitle("Year: {closest_state}")

anim_save("img/animateViz.gif", aniPlot, bg = "transparent")
