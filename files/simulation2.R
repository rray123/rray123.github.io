# Sample sizes
n_seq <- c(10, 30, seq(100, 3000, by = 100))
print(n_seq)

# Number of times we run the experiment
ndraws <- 100

# True p
truth <- 0.2

# Run the experiment ndraws times
# Note: there are much faster ways to code this; for demonstration purposes only
draws <- data.frame("n" = NULL, "X_bar" = NULL)
for(n in n_seq) {
  for(s in 1:ndraws) {
    # rbinom(n, 1, truth) flips the biased coin n times
    draws <- bind_rows(draws, data.frame("n" = n, "X_bar" = mean(rbinom(n, 1, truth))))
  }
}

################################## Plots #######################################
library(tidyverse)
library(latex2exp)
# Custom labeller for facets to display "n = 100" nicely
n_labeller <- as_labeller(function(x) paste("n =", x))

# LAW OF LARGE NUMBERS: SCATTER PLOT OF SAMPLE MEAN VS N
ggplot(draws) +
  geom_point(aes(x = n, y = X_bar), alpha = 0.3, size = 0.3, color = "steelblue") +
  geom_hline(aes(yintercept = truth), color = "red", linewidth = 0.8) + 
  theme_classic() +
  labs(
    title = "Law of Large Numbers",
    subtitle = "Sample mean converges to the true probability (p = 0.2)",
    x = "Sample Size (n)",
    y = TeX("$\\bar{X}_n$")
  )

# LAW OF LARGE NUMBERS: DISTRIBUTION OF SAMPLE MEANS FOR DIFFERENT N
ggplot(draws %>% filter(n %in% c(10, 100, 1000))) + 
  geom_histogram(aes(x = X_bar), bins = 30, fill = "steelblue", color = "white") + 
  facet_wrap(~n, ncol = 1, labeller = n_labeller) + 
  theme_classic() +
  labs(
    title = "Distribution of Sample Means",
    x = TeX("$\\bar{X}_n$"),
    y = "Count"
  )

# Define scaled X for the CLT demonstration
# For Bernoulli(p), variance is p(1-p), so sd is sqrt(p(1-p))
sd_theoretical <- sqrt(truth * (1-truth))
draws$X_bar_scaled <- sqrt(draws$n) * (draws$X_bar - truth)

# CENTRAL LIMIT THEOREM: SCATTER PLOT OF SCALED SAMPLE MEAN VS N
ggplot(draws) +
  geom_point(aes(x = n, y = X_bar_scaled), alpha = 0.3, size = 0.3, color = "darkgreen") +
  geom_hline(aes(yintercept = 0), color = "red", linewidth = 0.8) + 
  theme_classic() +
  labs(
    title = "Scaled Sample Mean over Sample Size",
    x = "Sample Size (n)",
    y = TeX("$\\sqrt{n}(\\bar{X}_n - p)$")
  )

# CENTRAL LIMIT THEOREM: DISTRUBTION OF SCALED SAMPLE MEAN FOR DIFFERENT WAYS
# NORMAL DENSITY ACCORDING TO THE CENTRAL LIMIT THEOREM IS OVERLAID
ggplot(draws %>% filter(n %in% c(10, 100, 1000))) + 
  geom_histogram(aes(x = X_bar_scaled, y = after_stat(density)), bins = 30, fill = "darkgreen", color = "white", alpha = 0.7) + 
  stat_function(fun = dnorm, args = list(mean = 0, sd = sd_theoretical), color = "red", linewidth = 1) +
  facet_wrap(~n, ncol = 1, labeller = n_labeller) + 
  theme_classic() +
  labs(
    title = "Central Limit Theorem",
    subtitle = "Scaled distribution converging to Normal",
    x = TeX("$\\sqrt{n}(\\bar{X}_n - p)$"),
    y = "Density"
  )
