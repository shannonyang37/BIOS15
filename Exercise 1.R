# Exercise
set.seed(1)
x <- rnorm(50, 10, 2)

calculate_cv <- function(x){
  cv <- sd(x)/mean(x)
  return(cv)
}

out <- NULL
for(i in 1:1000){
  sample1 <- sample(x, replace=TRUE)
  out[i] <- calculate_cv(sample1)
}

ci <- quantile(out, c(0.025, 0.975))

plot(density(out), main="", xlab="Coefficient of Variation (CV)", ylab="Density")
abline(v = ci, col = "red", lty = 2, lwd = 2)

# Optional exercise
set.seed(1)
n_sims <- 200
results_matrix <- matrix(NA, nrow = n_sims, ncol = 2)
colnames(results_matrix) <- c("CV", "SD_log")

for(i in 1:n_sims){
  set_sd <- runif(1, min=0.05, max=0.35)
  x_log <- rnorm(100, mean=3, sd=set_sd)
  x <- exp(x_log)
  
  cv_x <- calculate_cv(x)
  sd_log_x <- sd(log(x))
  
  results_matrix[i,] <- c(cv_x, sd_log_x) 
}

plot(
  results_matrix[, "CV"], results_matrix[, "SD_log"],
  xlab = "CV(x)",
  ylab = "SD[log(x)]",
  main = "Similarity of SD of log-transformed data to CV",
  pch = 1, col="black"
)
abline(0, 1, col = "red", lty = 3)
