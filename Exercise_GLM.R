# Import data
dat = read.csv("D:/ShannonYang/Exchange/BIOS15/dataset/Eulaema.csv")
head(dat)
summary(dat)

# Load libraries
library(ggplot2)
library(reshape2) 
library(MASS)

# Plot a correlation heatmap to check for multicollinearity
# Subset the targeted variables
env_data <- dat[, c("altitude", "MAT", "MAP", "Tseason", "Pseason")]

# Calculate the correlation matrix
cor_matrix <- cor(env_data, use = "complete.obs")

# Melt the matrix into a long format for ggplot2
melted_cor <- melt(cor_matrix)

# Plot the heatmap
ggplot(data = melted_cor, aes(x = Var1, y = Var2, fill = value)) +
  
  # Add boxes
  geom_tile(color = "white") + 
  
  # Define the color gradient
  scale_fill_gradient2(
    low = "#377eb8", 
    mid = "white", 
    high = "#e41a1c",
    midpoint = 0, 
    limit = c(-1, 1), 
    space = "Lab", 
    name = "Pearson\nCorrelation"
    ) +
  
  # Labels and stylings
  labs(
    title = "Correlation Matrix of Targeted Variables",
    x = NULL, y = NULL
    ) +
  
  theme_minimal() + 
  
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), 
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    panel.grid.major = element_blank(), 
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank()
    ) +
  
  coord_fixed() 

# According to the correlation heatmap, 'MAT' and 'altitude' is highly correlated
# Hence, we drop off 'altitude' in our subsequent models, and focus only on climatic variables
# We use 'offset(effort)' to account for variation in sampling hours, rather than viewing the sampling hours as a covariate  

# Model 1: Poisson model fitted to 4 environmental variables 
m1 = glm(Eulaema_nigrita~MAT+MAP+Tseason+Pseason+offset(effort), "poisson", data=dat)
summary(m1)

# In Model 1, residual deviance is much higher than the residual df, suggesting overdispersion
# Hence we fit the model with negative binomial errors
# Model 2: Negative Binomial model fitted to 4 environmental variables 
m2 = glm.nb(Eulaema_nigrita~MAT+MAP+Tseason+Pseason+offset(effort), data=dat)
summary(m2)

# In Model 2, 'MAT' has no statistically significant effect
# Hence 'MAT' is removed to form a simplified model (which also has a lower AIC)
# Model 3: Negative Binomial model fitted to 3 environmental variables (removing 'MAT')
m3 = glm.nb(Eulaema_nigrita~MAP+Tseason+Pseason+offset(effort), data=dat)
summary(m3)

# Model 4: Including interaction term between temperature and precipitation seasonality
m4 = glm.nb(Eulaema_nigrita~MAP+Tseason*Pseason+offset(effort), data=dat)
summary(m4)

# Model 0: Null model (for comparison)
m0 = glm.nb(Eulaema_nigrita~1+offset(effort), data=dat)
summary(m0)

# To generate clearer graphs, the response variable will be plotted as rate of bee abundance:
# Bee abundance divided by the sampling hours (effort is log of sampling hours, hence apply exponential to get raw hours)
dat$rate_per_hr <- dat$Eulaema_nigrita / exp(dat$effort)

# Plot bee abundance against 'MAP' 
# Create a data frame for plotting; Tseason and Pseason are set to constant
pred_MAP <- data.frame(
  MAP = seq(from = min(dat$MAP), to = max(dat$MAP), length.out = 200),
  Tseason = mean(dat$Tseason),
  Pseason = mean(dat$Pseason),
  effort = 0 
)

# Back-transform the predicted value
pred_MAP_link <- predict(m4, newdata = pred_MAP, type = "response", se.fit = T)

# Extract the predicted value
pred_MAP$fit <- pred_MAP_link$fit

# Calculate the values for the 95% confidence polygon
pred_MAP$lwr <- pred_MAP_link$fit - 1.96 * pred_MAP_link$se.fit
pred_MAP$upr <- pred_MAP_link$fit + 1.96 * pred_MAP_link$se.fit

# Plot the graph
ggplot() +
  # Raw data points
  geom_point(data = dat, aes(x = MAP, y = rate_per_hr),
             pch = 1, 
             colour = "black",
             alpha = 0.7,
             size = 2.2
  ) +
  
  # Fitted regression line
  geom_line(data = pred_MAP, aes(x = MAP, y = fit),
            colour = "black", 
            linewidth = 0.8
  ) +
  
  # 95% confidence polygon
  geom_ribbon(data = pred_MAP, aes(x = MAP, ymin = lwr, ymax = upr),
              fill = "steelblue", 
              alpha = 0.2
  ) +
  
  # Labels and styling
  labs(
    title = expression(paste("Effect of Mean Annual Precipitation on ", italic("Eulaema nigrita"), " Abundance")),
    x = "Mean Annual Precipitation (mm)",
    y = "Bee Abundance (bees / hour)"
  ) +
  
  theme_classic(base_size = 12) +
  
  theme(
    plot.title = element_text(hjust = 0.5, size = 13),
    axis.title = element_text(face = "plain"),
    aspect.ratio = 0.85
  )

# Plot bee abundance against 'TSeason' with interaction with 'Pseason' 
# Define means, standard deviation, and TSeason range
mean_MAP <- mean(dat$MAP)
mean_Pseason <- mean(dat$Pseason)
sd_Pseason <- sd(dat$Pseason)
Tseason_seq <- seq(min(dat$Tseason), max(dat$Tseason), length.out=200)

# Create a data frame for plotting; MAP is set to constant
# To visualize the interaction effect of 'TSeason' and 'PSeason', 3 regression line will be plotted:
# first with PSeason kept constant (mean), second as mean plus one standard deviation, and third as mean minus one standard deviation
pred_interaction <- rbind(
  data.frame(Tseason = Tseason_seq, MAP = mean_MAP, effort = 0,
             Pseason = mean_Pseason - sd_Pseason, Level = "Precipitation Seasonality = Mean - SD"),
  data.frame(Tseason = Tseason_seq, MAP = mean_MAP, effort = 0,
             Pseason = mean_Pseason, Level = "Precipitation Seasonality = Mean"),
  data.frame(Tseason = Tseason_seq, MAP = mean_MAP, effort = 0,
             Pseason = mean_Pseason + sd_Pseason, Level = "Precipitation Seasonality = Mean + SD")
)

# Convert 'Levels' to factor for plotting
pred_interaction$Level <- factor(pred_interaction$Level,
                                 levels=c("Precipitation Seasonality = Mean - SD", "Precipitation Seasonality = Mean", "Precipitation Seasonality = Mean + SD"))

# Back-transform the predicted value
pred_interaction$fit <- predict(m4, newdata=pred_interaction, type="response", se.fit=T)$fit

# Plot the graph
ggplot() +
  # Raw data points
  geom_point(data = dat, aes(x = Tseason, y = rate_per_hr),
             pch = 1, 
             colour = "black",
             alpha = 0.7,
             size = 2.2
  ) +
  
  # Fitted regression line
  geom_line(data = pred_interaction, aes(x = Tseason, y = fit, linetype = Level),
            linewidth = 0.8,
            color = "black"
  ) +
  
  # Set the type of different regression lines
  scale_linetype_manual(values = c("dashed", "solid", "dotted")) +
  
  # Labels and stylings
  labs(
    title = "Interactive Effect of Temperature and Precipitation Seasonality",
    x = "Temperature Seasonality",
    y = "Bee Abundance (bees / hour)",
    linetype = NULL
  ) +
  
  theme_classic(base_size = 12) +
  
  theme(
    plot.title = element_text(hjust = 0.5, size = 13),
    legend.position  = c(1, 1),
    legend.justification = c("right", "top"),
    legend.key.width = unit(1.2, "cm"),
    aspect.ratio     = 0.85
  )

# Construct AIC table for comparison
# Combine models into a list
mlist = list(m1, m2, m3, m4, m0)

# Calculate the AIC of the models
AICTab = AIC(m1, m2, m3, m4, m0)

# Calculate loglikelihood and add to the AIC table
AICTab$logLik = unlist(lapply(mlist, logLik))

# Reorder in ascending AIC
AICTab = AICTab[order(AICTab$AIC, decreasing=F),]

# Calculate the AIC difference and add to the AIC table
AICTab$delta = round(AICTab$AIC - min(AICTab$AIC), 2)

# Calculate the weights and add to the AIC table
lh = exp(-0.5*AICTab$delta)
AICTab$w = round(lh/sum(lh), 2)
AICTab
