# Load required library
library(tidyverse)

# Function to estimate S with confidence interval
estimate_S <- function(s1, dist = "poisson", lambda_s2 = NULL, size_s2 = NULL, prob_s2 = NULL, n_sim = 10000) {
  # Generate s2 based on the selected discrete distribution
  if (dist == "poisson") {
    if (is.null(lambda_s2)) stop("For Poisson distribution, lambda_s2 must be specified.")
    s2_samples <- rpois(n_sim, lambda_s2)
  } else if (dist == "negative_binomial") {
    if (is.null(size_s2) || is.null(prob_s2)) stop("For Negative Binomial, size_s2 and prob_s2 must be specified.")
    s2_samples <- rnbinom(n_sim, size = size_s2, prob = prob_s2)
  } else {
    stop("Invalid distribution choice. Use 'poisson' or 'negative_binomial'.")
  }
  
  # Compute total S for each simulation
  S_samples <- s1 + s2_samples
  
  # Estimate mean and 95% confidence interval
  S_estimate <- median(S_samples)
  CI <- quantile(S_samples, probs = c(0.025, 0.975))
  
  # Print results
  cat("Estimated S:", S_estimate, "\n")
  cat("95% Confidence Interval: (", CI[1], ",", CI[2], ")\n")
  
  # Plot histogram of S estimates
  ggplot(data.frame(S_samples), aes(x = S_samples)) +
    geom_histogram(binwidth = 10, fill = "steelblue", color = "black", alpha = 0.7) +
    labs(title = paste("Estimated Total Ships Crossing (S) -", dist), x = "S", y = "Frequency") +
    theme_minimal()
}

# Example usage:

## s1 is the know number of ships. It is added to the prior ones, s2, so that
## S = s1 + s2 for each sample. No likelihood in this, all prior.
## The estimate is the median of the samples,
## and the CI is the 25% and 75% percentiles.

# Poisson distribution
estimate_S(s1 = 500, dist = "poisson", lambda_s2 = 200)

# Negative Binomial distribution
estimate_S(s1 = 500, dist = "negative_binomial", size_s2 = 10, prob_s2 = 0.5)
