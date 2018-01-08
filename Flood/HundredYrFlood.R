library(magrittr)
library(fishmethods)
set.seed(20180106)
## ---- read
floods = read.csv("HundredYrFlood.csv")
n = nrow(floods)

# Maximum Likelihood Estimators ----
## ---- mu-hat
mu_hat = mean(log(floods[["discharge"]]))
## ---- sigma-hat
sigma_hat = (log(floods[["discharge"]]) - mu_hat)^2 %>% mean %>% sqrt

# Adjusted MLE ----
source("finney.R")

# ---- mu-hat-adj
x = log(floods[["discharge"]])
g = function(x) finney_series(x, n)
mu_hat_adj = mean(x) + log(g(var(x)/2))
sigma_hat_adj = exp(2 * mean(x)) * ceiling(g(2*var(x)) - g(var(x) * (n-2)/(n-1)))
bt.log(mean(x), sd(x), n)

# Method of Moments Estimators ----
## ---- sigma-tilde
sigma_tilde = ((floods[["discharge"]]^2 %>% sum %>% log) -
    2 * (floods[["discharge"]] %>% sum %>% log) +
    log(nrow(floods))) %>%
    sqrt
## ---- mu-tilde
mu_tilde = (floods[["discharge"]] %>% sum %>% log) -
            log(nrow(floods)) - 
            (sigma_tilde)^2 / 2

## ---- v-mle
v_mle = qlnorm(p = 0.99, meanlog = mu_hat, sdlog = sigma_hat)

## ---- v-mom
v_mom = qlnorm(p = 0.99, meanlog = mu_tilde, sdlog = sigma_tilde)

## ---- plot-functions

## ---- time-plot-v
time_plot_v = function(data, v = NULL, col = 1, title = "") {
    plot(data, type = "h", lwd = 2)
    title(title, cex = 0.75)
    abline(h = v, col = col, lty = 3)
}

## ---- lnorm-plot-v
lnorm_plot_v = function(mu, sigma, v = NULL, col = 1, add = FALSE) {
    curve(dlnorm(x, meanlog = mu, sdlog = sigma),
          from = 0, to = 30000,
          ylab = "f(D)",
          xlab = "D",
          main = "Distribution of discharge",
          col = col, 
          add = add)
    abline(v = v, col = col, lty = 3)
}

