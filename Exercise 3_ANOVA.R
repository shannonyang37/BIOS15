# Load packages
library(ggplot2)

# Import dataset
dat = read.csv("D:/BIOS15/dataset/butterflies.csv")

# Modify the data names to differentiate maternal and larval host plant
dat$MaternalHost = paste0(dat$MaternalHost, "M")
dat$LarvalHost = paste0(dat$LarvalHost, "L")

# Before we fit the ANOVA model, we first compute some summary statistics to understand our data
# Compute mean developmental time of each combination of larval and maternal host plant
means = tapply(dat$DevelopmentTime, list(dat$MaternalHost, dat$LarvalHost), mean)
means

# Compute standard errors of developmental time of each combination of larval and maternal host plant
se = tapply(dat$DevelopmentTime, list(dat$MaternalHost, dat$LarvalHost),
            function(x) sd(x) / sqrt(length(x)))
se

# Generate a boxplot of developmental time of each combination of larval and maternal host plant
ggplot(dat, aes(x = LarvalHost, y = DevelopmentTime, fill = MaternalHost)) +
  
  geom_boxplot(box.linewidth = 0.8,
               outlier.shape = 1, outlier.color = "black", lwd = 1,
               whisker.linetype = 2, whisker.linewidth = 0.6,
               staplewidth = 0.6, staple.linetype = 1, staple.linewidth = 0.6,
               median.linewidth = 1.3,
               key_glyph = "rect") +
  
  theme_classic(base_size = 12) +
  
  scale_fill_manual(values = c("BerteroaM" = "#1a80bb", "BarbareaM" = "#f2c45f"),
                    labels = c("BerteroaM" = "Berteroa", "BarbareaM" = "Barbarea"),
                    guide = guide_legend(byrow = TRUE)
                    ) +
  
  scale_x_discrete(labels = c("BarbareaL" = "Barbarea", "BerteroaL" = "Berteroa")
                   ) +
  
  labs(title = "Larval Developmental Time across Host Plant Combinations",
       x = "Larval Host Plant",
       y = "Larval Developmental Time (days)",
       fill = "Maternal Host Plant"
       ) +
  
  theme(plot.title = element_text(face = "plain", hjust = 0.5),
        axis.text = element_text(size = 12),
        legend.background = element_blank(),
        legend.position = c(0.16, 0.88),
        legend.text = element_text(size = 12),
        legend.key.spacing.y = unit(0.1, "cm"),
        panel.border = element_rect(color = "black",     
                                    fill = NA,
                                    linewidth = 0.8)
        )

# Fit the linear model
m = lm(DevelopmentTime ~ MaternalHost * LarvalHost, data = dat)

# ANOVA table
anova(m)

# Summary table
# The reference factor is set to be BarbareaL and BarbareaM
summary(m)

# Plot the interaction effect of maternal host plant and larval host plant
# Convert the tapply matrices into a tidy data frame for ggplot
plot_data <- data.frame(
  MaternalHost = rep(c("BarbareaM", "BerteroaM"), times = 2),
  LarvalHost   = rep(c("BarbareaL", "BerteroaL"), each = 2),
  Mean_DevTime = c(means["BarbareaM", "BarbareaL"], means["BerteroaM", "BarbareaL"],
                   means["BarbareaM", "BerteroaL"], means["BerteroaM", "BerteroaL"]),
  SE_DevTime   = c(se["BarbareaM", "BarbareaL"], se["BerteroaM", "BarbareaL"],
                   se["BarbareaM", "BerteroaL"], se["BerteroaM", "BerteroaL"])
)

# Generate interaction plot
ggplot(plot_data, aes(x = LarvalHost, y = Mean_DevTime, 
                      group = MaternalHost, 
                      color = MaternalHost,
                      shape = MaternalHost, 
                      linetype = MaternalHost)
       ) +
  
  geom_line(size = 1.2,
            linetype = "solid"
            ) +
  
  geom_errorbar(aes(ymin = Mean_DevTime - SE_DevTime, 
                    ymax = Mean_DevTime + SE_DevTime), 
                width = 0.05, 
                size = 0.6, 
                linetype = "solid"
                ) +
  
  geom_point(size = 3, 
             shape = 16
             ) +
  
  scale_color_manual(values = c("BerteroaM" = "#1a80bb", "BarbareaM" = "#f2c45f"),
                     labels = c("BerteroaM" = "Berteroa", "BarbareaM" = "Barbarea")
                     ) +
  
  scale_x_discrete(labels = c("BarbareaL" = "Barbarea", "BerteroaL" = "Berteroa")
                   ) +
  
  ylim(20, 35) +
  
  theme_classic(base_size = 12) +
  
  labs(title = "Interactive Effects of Maternal and Larval Host Plants \non Larval Development",
       x = "Larval Host Plant",
       y = "Larval Developmental Time (days)",
       shape = "Maternal host",
       linetype = "Maternal host",
       color = "Maternal Host Plant"
       ) +
  
  theme(plot.title = element_text(face = "plain", hjust = 0.5),
        axis.title = element_text(size=12),
        axis.text = element_text(size = 12),
        legend.background = element_blank(),
        legend.position = c(0.16, 0.88),
        legend.text = element_text(size = 12),
        legend.key = element_blank(),
        panel.border = element_rect(
          color = "black",     
          fill = NA,           
          linewidth = 0.8),
        aspect.ratio = 0.8
        )
