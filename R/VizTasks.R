##########################################################
#### Script that generates elementary task comparison ####
##########################################################

if(!as.logical(Sys.getenv("PRES_SETUP"))){
  library(dplyr)
  library(ggplot2)
}

# set seed and generate data
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
  
  labs(x = "Site", y = "Investigations") + 
  
  theme_clear() +
  theme(axis.title = element_text(size = 24, color = txtClr),
        axis.text = element_text(size = 20, color = txtClr))

p2a <- ggplot(pData, aes(x = reorder(site, seq(8, 1, -1)), y = deviations)) +
  geom_bar(stat = "identity", width = 0.98) +
  
  labs(x = "Site", y = "Investigations") + 
  coord_flip() +
  
  theme_clear() +
  theme(axis.title = element_text(size = 24, color = txtClr),
        axis.text = element_text(size = 20, color = txtClr))
  
p2b <- ggplot(pData, aes(x = reorder(site, deviations), y = deviations)) +
  geom_bar(stat = "identity", width = 0.98) +
  
  labs(x = "Site", y = "Investigations") + 
  coord_flip() +
  
  theme_clear() +
  theme(axis.title = element_text(size = 24, color = txtClr),
        axis.text = element_text(size = 20, color = txtClr))

p3 <- p2a + 
  facet_wrap(grps ~ ., scales = "free_y") +
  theme(strip.background = element_rect(fill = NA),
        strip.text = element_text(color = NA),
        strip.placement = "inside")

p4 <- ggplot(pData, 
             aes(x = "", y = sort(deviations), fill = site)) +
  geom_bar(stat = "identity", width = 0.98) +
  
  labs(title = "Investigations") +
  scale_fill_brewer(name = "Site", palette = "Dark2") +
  coord_polar("y", start = 0) +
  
  theme_clear() +
  theme(axis.title = element_text(color = NA),
        axis.text = element_text(color = NA),
        axis.ticks = element_line(color = NA),
        plot.title = element_text(size = 24, color = txtClr),
        legend.title = element_text(size = 24),
        legend.text = element_text(size = 18))
  
  
# save images
# keeping legacy file names for ordering in slides
ggsave("img/01-vizTasks.png", plot = p1, bg = "transparent")
ggsave("img/02-vizTasks.png", plot = p2a, bg = "transparent")
ggsave("img/03-vizTasks.png", plot = p3, bg = "transparent")
ggsave("img/04-vizTasks.png", plot = p4, bg = "transparent")
ggsave("img/05-vizTasks.png", plot = p2b, bg = "transparent")
