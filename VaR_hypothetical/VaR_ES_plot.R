rm(list = ls())

# Load the ggplot2 library
library(ggplot2)
library(ggthemes)
theme_set(theme_bw())
library(Cairo)
library(here)
setwd(here())

load(file="VaR.RData")

##################################################
# Plots for value-at-risk and expected shortfall #
##################################################

# Value-at-risk

summary(data.fitted$VaR_p10)

summary(data.fitted$VaR_p5)

VaR_shockdist <- ggplot(data = data.fitted) +
  geom_density(aes(x = VaR_p5/1000, color = "X", fill = "X"), alpha = 0.5, linetype = "solid") +
  geom_density(aes(x = VaR_p10/1000, color = "Y", fill = "Y"), alpha = 0.5, linetype = "solid") +
  labs(title = "", x = "", y = "") + 
  coord_cartesian(xlim = c(-2.5, -0.5)) + 
  scale_x_continuous(breaks = seq(-2.5, -0.5, by = 0.5)) +
  scale_color_manual(name = "",
                     values = c("X" = "blue", "Y" = "red"),
                     labels = c("X" = "VaR 5%", "Y" = "VaR 10%")) +
  scale_fill_manual(name = "",
                    values = c("X" = "blue", "Y" = "red"),
                    labels = c("X" = "VaR 5%", "Y" = "VaR 10%")) +
  scale_linetype_manual(name = "",
                        values = c("X" = "solid", "Y" = "solid"),
                        labels = c("X" = "VaR 5%", "Y" = "VaR 10%")) +
  theme(axis.line = element_line(colour="black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "bottom", legend.direction = "horizontal",
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.key.width = unit(2,"line"))

VaR_shockdist

cairo_ps(filename = "VaR_shockdist.eps", 
         width=3, height=3, pointsize=12, fallback_resolution=300)
VaR_shockdist
dev.off()

# Expected shortfall

summary(data.fitted$ES_p10)

summary(data.fitted$ES_p5)

ES_dist <- ggplot(data = data.fitted) +
  geom_density(aes(x = ES_p5/1000, color = "X", fill = "X"), alpha = 0.5, linetype = "solid") +
  geom_density(aes(x = ES_p10/1000, color = "Y", fill = "Y"), alpha = 0.5, linetype = "solid") +
  labs(title = "", x = "", y = "") + 
  coord_cartesian(xlim = c(-3.0, -0.75)) + 
  scale_x_continuous(breaks = seq(-3.0, -0.75, by = 0.5)) +
  scale_color_manual(name = "",
                     values = c("X" = "blue", "Y" = "red"),
                     labels = c("X" = "ES 5%", "Y" = "ES 10%")) +
  scale_fill_manual(name = "",
                    values = c("X" = "blue", "Y" = "red"),
                    labels = c("X" = "ES 5%", "Y" = "ES 10%")) +
  scale_linetype_manual(name = "",
                        values = c("X" = "solid", "Y" = "solid"),
                        labels = c("X" = "ES 5%", "Y" = "ES 10%")) +
  theme(axis.line = element_line(colour="black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "bottom", legend.direction = "horizontal",
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.key.width = unit(2,"line"))

ES_dist

cairo_ps(filename = "ES_dist.eps", 
         width=3, height=3, pointsize=12, fallback_resolution=300)
ES_dist
dev.off()
