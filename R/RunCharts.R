##################################################################
#### Script that generates SPC charts needed for presentation ####
##################################################################

if(Sys.getenv("PRES_SETUP") == ""){
  library(dplyr)
  library(ggplot2)
  library(magrittr)
}

# helper function to generate random batch number
# n is length of batch number
GenBatchNumber <- function(n = 8){
  
  paste0(sample(1:9, size = n, replace = TRUE), collapse = "")
  
}

# parameters for SPC data and chart
bCt <- 30
cqa_mu <- 4
cqa_sd <- 0.25
ctrlLims <- cqa_mu + c(-3, 3)*cqa_sd

# adding additional set seed not to disrupt previous random generation
set.seed(41)
dom <- sample(1:3, bCt, replace = TRUE) %>%
  cumsum() %>%
  add(as.Date("2018-09-01"))

# set seeed
set.seed(42)

# generate data
kf <- rnorm(bCt, mean = cqa_mu, sd = cqa_sd)
kf[floor(0.6*bCt)] <- cqa_mu + 3.3*cqa_sd

# generate batch numbers
batchNums <- GenBatchNumber() %>%
  as.integer() %>%
  add(seq(bCt)) %>%
  factor()

# create data set for plots
myData <- data.frame(batchNums = factor(batchNums), kf, 
                     dom = as.character(dom), stringsAsFactors = FALSE) %>%
  mutate(viols = (kf > ctrlLims[2]) | (kf < ctrlLims[1]),
         ruleLabel = ifelse(viols, "1", ""))

# base control charts
spc1 <- ggplot(myData, aes(x = batchNums, y = kf)) +
  
  geom_point(aes(color = viols), size = 2.5) +
  geom_text(aes(label = ruleLabel), vjust = -0.5, hjust = -0.25) +
  geom_line(aes(group = 1)) + 
  geom_hline(yintercept = cqa_mu, 
             color="darkgreen", 
             size = 1.1) +
  geom_hline(yintercept = ctrlLims, 
             color="darkred", linetype="dashed", 
             size = 1.1) +
  
  annotate(geom = "text", x = bCt - 0.5, y = ctrlLims[2],
           label = "UCL", vjust = -0.5, size = 5) +
  annotate(geom = "text", x = bCt -0.5, y = ctrlLims[1],
           label = "LCL", vjust = 1.15, size = 5) +
  
  labs(x = "Batch Number", y = "Water Content (%)") +
  scale_color_manual(breaks = c(TRUE, FALSE), 
                     values = c("FALSE" = "black", "TRUE" = "red"), 
                     guide = FALSE) +
  coord_fixed(ratio = 5, ylim = c(.95, 1.05)*ctrlLims) +
  
  theme_clear() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.title = element_text(size = 16, color = "white"),
        axis.text = element_text(size = 12, color = "white"))

# add release limits to control chart
spc2 <- spc1 + 
  geom_hline(yintercept = 20, color = "darkred", size = 1.1) +
  coord_fixed(ratio = 5 * 1.05*ctrlLims[1]/20, 
              ylim =c(0.95*ctrlLims[1], 1.05*20)) +
  annotate(geom = "text", x = bCt - 1, y = 20,
           label = "Spec", vjust = 1.15, size = 5)

# create histogram
set.seed(42)
cqa2_mu <- 101.8
cqa2_sd <- 1.3
rSpecs <- c(95, 105)
assayData <- data.frame(assay = rnorm(150, cqa2_mu, cqa2_sd))

ppkHist <- ggplot(assayData, aes(x = assay)) +
  geom_density(alpha = 0.8, fill = "#213451", color = NA) +
  geom_vline(xintercept = rSpecs, color = "darkred", size = 1.1) +
  stat_function(data = data.frame(x = rnorm(1000, cqa2_mu, cqa2_sd)),
                mapping = aes(x),
                args = list(mean = cqa2_mu, sd = cqa2_sd),
                fun = dnorm, n = 1000, size = 1.1, linetype = "dashed") +
  
  labs(x = "Assay", y = "") +
  annotate(geom = "text", x = rSpecs[1], y = 0.25, angle = 90,
           label = "Lower Spec", vjust = -0.5, size = 5) +
  annotate(geom = "text", x = rSpecs[2], y = 0.25, angle = 90,
           label = "Upper Spec", vjust = 1.2, size = 5) +
  
  scale_y_continuous(breaks = NULL) +
  scale_x_continuous(labels = scales::number) +
  
  theme_clear() +
  theme(axis.title = element_text(size = 16, color = "white"),
        axis.text = element_text(size = 12, color = "white"))

# save images
ggsave("img/BaseCtrlChart.png", plot = spc1, bg = "transparent")
ggsave("img/CtrlChartWRelease.png", plot = spc2, bg = "transparent")
ggsave("img/cqaDistribution.png", plot = ppkHist, bg = "transparent")

# save data for use with JMP
readr::write_csv(myData, "R/ctrlChartdata.csv")

