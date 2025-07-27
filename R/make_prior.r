make_prior <- function(id, side, year) {
  last_year <- glue('posterior/{side}/{min(year)-1}.rds')
  if (file.exists(last_year)) {
    posterior <- readRDS(last_year)
  } else {
    eta <- matrix(NA, ncol = 3)
    sd_eta <- matrix(NA, ncol = 3)
    alpha <- matrix(NA, ncol = 3)
    sd_alpha <- matrix(NA, ncol = 3)
    rss <- rep(10, 3)
    tdf <- rep(1, 3)
  }
  ind <- match(id, rownames(eta))
  new <- is.na(ind)
  mu_eta <- eta[ind, ncol(eta)]
  tau_eta <- sd_eta[ind, ncol(eta)]^(-2)
  mu_alpha <- alpha[ind, ]
  tau_alpha <- sd_alpha[ind, ]^(-2)
  mu_eta[new] <- tau_eta[new] <- 0
  mu_alpha[new,] <- tau_alpha[new,] <- 0

  list(mu_eta = mu_eta, tau_eta = tau_eta, mu_alpha = mu_alpha, tau_alpha = tau_alpha,
    rss = rss, tdf = tdf, new = 1 * new)
}
