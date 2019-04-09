#############################################
#### Script that sets up data for plotly ####
#############################################

library(ggplot2)
library(plotly)

set.seed(42)
nSites <- 8
txtClr <- "white"
cts <- sample(1:50, nSites)
grpOrder <- order(cts, decreasing = TRUE)
pData <- data.frame(site = LETTERS[seq_along(cts)], 
                    deviations = cts,
                    grps = rep(1:2, each = nSites/2)[grpOrder])

p1 <- ggplot(pData, aes(x = site, y = deviations)) +
  geom_bar(stat = "identity", width = 0.98) +
  
  labs(x = "Site", y = "Deviations") + 
  
  theme(panel.background = element_rect(fill = "white", color = "black"),
        axis.title = element_text(size = 24),
        axis.text = element_text(size = 20))

ggplotly(p1)
  
  
  
