# Notes
# Generate data and plot
set.seed(85)
x = rnorm(200, 10, 2)
y = 0.4*x + rnorm(200, 0, 1)

plot(x, y, las=1,
     xlab = "Leaf length (mm)",
     ylab = "Leaf width (mm)"
     )

# Fit linear model
m = lm(y~x)

# To see which component we can extract from the fitted model
str(m)

# Extract coefficients (parameter estimates)
cf = m$coefficients
cf

# Obtain predicted values from the fitted model
predvals = cf[1] + cf[2]*x

# Plot with regression line
par(mfrow=c(2,2))
plot(x, y, las=1,
     xlab = "Leaf length (mm)",
     ylab = "Leaf width (mm)")
abline(m)
segments(x, y, x, predvals)  # Residual

# Plot histogram of residuals
hist(residuals(m), xlab = "", las=1)

# Residual graphs
plot(m)

# Keep regression line within data range
newx = seq(min(x), max(x), length.out=200)
predy = cf[1] + cf[2]*newx
plot(x, y, las=1,
     xlab = "Leaf length (mm)",
     ylab = "Leaf width (mm)"
)
lines(newx, predy)

# Statistics summary
summary(m)

# Optional Exercise 1
# Produce a data frame holding x and y values
df = data.frame(x, y)
df

# Bootstrap sample with replacement, then fit model and extract slope
out_slope <- NULL
for(i in 1:1000){
  sample_x <- sample(df$x, replace=TRUE)
  sample_y <- sample(df$y, replace=TRUE)
  n <- lm(sample_y~sample_x)
  out_slope[i] <- n$coefficients[2]
}

# Calculate standard deviation of the sampling distribution, which is an estimate of the standard error
sd(out_slope)

# Notes
# Regression slope is the ratio of the covariance between y and x, and the variance in x
cov(y,x)/var(x)

# How much y changes for a one standard deviation change in x
# we ignore the intercept here because it will be cancel out eventually
cf[2]*(mean(x) + sd(x)) - (cf[2]*mean(x))

# r^2 = square of the Pearson correlation coefficient
cor(x,y)^2

# r^2 can be computed as the variance in the predicted values (y_hat) divided by the total variance in the response variable
y_hat = cf[1] + cf[2]*x
var(y_hat)
var(y_hat)/var(y)

# another way to compute variance explained by a predictor (x)
cf[2]^2*var(x)


# -------------------------
# Exercise: fitting a linear regression to real data
birds = read.csv("D:/ShannonYang/Exchange/BIOS15/dataset/bird_allometry.csv")
head(birds)

# Question is whether the allometric slopes and intercepts for brain-body scaling are similar in males and females
# Split the data into males and females
males = birds[birds$Sex=="m",]
females = birds[birds$Sex=="f",]

# Fit the model: brain-body scaling follows y=ax^b --> linearized through log transform
mm = lm(log(brain_mass)~log(body_mass), data=males)
mf = lm(log(brain_mass)~log(body_mass), data=females)

# Check residuals and statistics
hist(residuals(mm))
summary(mm)
summary(mf)

# Plot a scatterplot for the results
# Create the plot window with log-transformed variables (but no points yet)
plot(log(brain_mass) ~ log(body_mass), data = birds[birds$Sex %in% c("m", "f"), ], 
     type = "n",
     xlab = "Body Mass (log g)",
     ylab = "Brain Mass (log g)",
     main = "Allometric Scaling of Brain Mass with Body Mass")

# Add the points for males (blue) and females (red) with transparency
points(log(brain_mass) ~ log(body_mass), data = males, pch = 21, cex = 1, bg = adjustcolor("steelblue", alpha.f = 0.8), col="black", lwd=0.5)
points(log(brain_mass) ~ log(body_mass), data = females, pch = 21, cex = 1, bg = adjustcolor("brown1", alpha.f = 0.8), col="black", lwd=0.5)

# Draw the fitted regression lines directly using your OLS model coefficients
# mm represents your male model, mf represents your female model
x_male = log(males$body_mass)
newx_male = seq(min(x_male), max(x_male), length.out=615)
predy_male = coef(mm)[1] + coef(mm)[2]*newx_male
lines(newx_male, predy_male, col="steelblue", lwd=2)

x_female = log(females$body_mass)
newx_female = seq(min(x_female), max(x_female), length.out=483)
predy_female = coef(mf)[1] + coef(mf)[2]*newx_female
lines(newx_female, predy_female, col="brown1", lwd=2)

# Label the Ostrich (Struthio camelus) as it sits far to the upper right
ostrich <- birds[birds$Genus_Species == "Struthio_camelus", ]
text(log(ostrich$body_mass), log(ostrich$brain_mass), 
     labels = "Struthio camelus\n(Ostrich)", 
     pos = 2, col = "gray30", cex = 0.8, font = 3)

# Add a clean legend detailing the slopes
legend("bottomright", 
       legend = c(paste("Males (slope =", round(coef(mm)[2], 2), ")"), 
                  paste("Females (slope =", round(coef(mf)[2], 2), ")")),
       col = c("steelblue","brown1"), 
       lty = 1, lwd = 3, pch = 16, bty = "n", cex = 1)

# Identify species falling outside the main cloud (e.g., upper right or large residuals)
# Let's add log-transformed variables and fit a general model to find outliers
birds_clean <- birds[birds$Sex %in% c("m", "f"), ]
birds_clean$log_body <- log(birds_clean$body_mass)
birds_clean$log_brain <- log(birds_clean$brain_mass)
m_all <- lm(log_brain ~ log_body, data = birds_clean)
birds_clean$residual <- residuals(m_all)

# Find top positive residuals (exceptionally large brains for their size)
cat("\n=== SPECIES WITH LARGST RELATIVE BRAIN SIZES (Positive Residuals) ===\n")
print(head(birds_clean[order(-birds_clean$residual), c("Genus_Species", "Sex", "brain_mass", "body_mass", "residual")], 5))

# Find top negative residuals (exceptionally small brains for their size)
cat("\n=== SPECIES WITH SMALLEST RELATIVE BRAIN SIZES (Negative Residuals) ===\n")
print(head(birds_clean[order(birds_clean$residual), c("Genus_Species", "Sex", "brain_mass", "body_mass", "residual")], 5))


# Optional Exercise
# Simulate a dataset
n <- 100
x_true <- rnorm(n, 10, 2)

# Define a relationship between x and y
intercept <- 2
slope <- 1.5
residual_error <- rnorm(n, 0, 1.5)
y <- intercept + slope*x_true + residual_error

# Fit a model without measurement error
model_true <- lm(y~x_true)
summary(model_true)

# Fit a model with measurement error
error_sd <- 1.5
measurement_error <- rnorm(n, 0, error_sd)
x_error <- x_true + measurement_error
model_error <- lm(y~x_error)
summary(model_error)

# Try with different measurement errors
set.seed(123)
error_levels <- seq(0,0.5, by=0.05)

estimated_slopes <- numeric(length(error_levels))
corrected_slopes <- numeric(length(error_levels))

for(i in seq_along(error_levels)){
  current_error_sd <- error_levels[i]
  x_measure <- x_true + rnorm(n, sd=current_error_sd)
  fit <- lm(y~x_measure)
  estimated_slopes[i] <- coef(fit)[2]
  
  K <- 1 - (current_error_sd^2/var(x_true))
  corrected_slopes[i] <- estimated_slopes[i]/K
}

plot(error_levels, estimated_slopes, type = "n",
     xlab = "Error standard deviation in x",
     ylab = "Estimated slope",
     main = "Effect of Predictor Measurement Error on Estimated Slope",
     ylim = c(1.3, 1.47), font.main = 1)

points(error_levels, estimated_slopes,
       pch = 21, bg = adjustcolor("steelblue", alpha.f = 0.7),
       col = "black", cex = 1.3, lwd = 1)

points(error_levels, corrected_slopes,
       pch = 21, bg = adjustcolor("brown", alpha.f = 0.7),
       col = "black", cex = 1.3, lwd = 1)

segments(x0 = error_levels,      # Starting X (the error level)
         y0 = estimated_slopes, # Starting Y (uncorrected slope)
         x1 = error_levels,      # Ending X (same error level, making the line vertical)
         y1 = corrected_slopes, # Ending Y (corrected slope)
         col = "gray40",         # Segment color
         lty = "solid",         # Line type (dotted, dashed, solid, etc.)
         lwd = 1)                # Line width
