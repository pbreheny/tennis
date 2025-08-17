#' Read in last year's prior (or create new one)

make_prior <- function(id, side, year) {
  last_year <- glue('posterior/{side}/{min(year)-1}.rds')
  if (file.exists(last_year)) {
    post <- readRDS(last_year)
    current <- post$eta[, last(.SD), player]
    ind <- match(id, current$player)
    mu_eta <- current[ind]$mean
    mu_eta[is.na(mu_eta)] <- -1
    tau_eta <- current[ind]$sd^(-2)
    tau_eta[is.na(tau_eta)] <- 1
    mu_alpha <- dcast(post$alpha, player ~ surface, value.var = "mean") |>
      _[ind,2:4] |>
      as.matrix()
    mu_alpha[is.na(mu_alpha)] <- 0
    tau_alpha <- post$alpha[, precision := sd^(-2)] |>
      dcast(player ~ surface, value.var = "precision") |>
      _[ind,2:4] |>
      as.matrix()
    tau_alpha[is.na(tau_alpha)] <- 1
    rss <- post$tau$rss
    tdf <- post$tau$tdf
  } else {
    eta_mean <- NA
    eta_prec <- NA
    mu_alpha <- matrix(0, length(id), 3)
    tau_alpha <- matrix(1, length(id), 3)
    rss <- rep(10, 5)
    tdf <- rep(1, 5)
  }
  list(
    mu_eta = mu_eta, tau_eta = tau_eta, mu_alpha = mu_alpha, tau_alpha = tau_alpha,
    rss = rss, tdf = tdf
  )
}
