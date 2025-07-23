convert <- function(fit) {
  MCMC <- as.mcmc.list(fit) ## Convert to coda format
  R2jags:::mcmc2bugs(MCMC, program="jags") ## Convert to R2jags format
}
