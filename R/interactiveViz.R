#############################################
#### Script that sets up data for plotly ####
#############################################

library(ggplot2)
library(plotly)

set.seed(42)
nSites <- 8
txtClr <- "white"
cts <- sample(1:50, nSites*10)
grpOrder <- order(cts, decreasing = TRUE)
pData <- data.frame(Site = paste("Site", LETTERS[seq(nSites)]), 
                    Investigations = cts,
                    Year = rep(2009:2018, each = nSites))

p1 <- ggplot(pData, aes(x = factor(Year), y = Investigations, fill = Site)) +
  # geom_point() +
  geom_bar(stat = "identity", width = 0.98) +
  
  labs(x = "Year", y = "Investigations", fill = "") + 
  scale_fill_brewer(palette = "Dark2") +
  
  theme(panel.background = element_rect(fill = "white", color = "black"),
        axis.title = element_text(size = 24),
        axis.text = element_text(size = 20),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14))

ggplotly(p1)
  
  
  
