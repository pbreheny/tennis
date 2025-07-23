## Import
side <- "atp"
Data <- import(2012:2016, side)
df <- Data$Data
y <- df$Wsets
ns <- df$Wsets + df$Lsets
nm <- nrow(df)
np <- length(Data$PlayerID)

## Fit setup
jData <- with(Data, list(Winner=Winner, Loser=Loser, Surface=Surface, Time=Time, T=max(Time), y=y, ns=ns, nm=nm, np=np))
monitor <- c("eta", "alpha", "sigma")
FUN <- function(chain) {list(eta=matrix(rnorm(np*max(Data$Time)), nrow=np), alpha=matrix(rnorm(np*3), nrow=np))}
inits <- mapply(FUN, 1:3, SIMPLIFY=FALSE)

## Fit
require(runjags)
runjags.options(rng.warning=FALSE)
jagsfit <- run.jags("fun/stat/model.txt", monitor=monitor, data=jData, inits=inits, n.chains=length(inits),
                    method="rjparallel", adapt = 1000, burnin=1000, sample=2000, summarise=FALSE)
# save(jagsfit, Data, file='mcmc.RData')
# fit <- jags(data=jData, param=param, model=model, n.chains=3, n.iter=5000, n.burn=2000, DIC=FALSE, n.thin=1)

## Summarize & Save
fit <- convert(jagsfit)
R2WinBUGS:::attach.bugs(fit)
Eta <- apply(eta, 2:3, mean)
rownames(Eta) <- Data$PlayerID
colnames(Eta) <- Data$TimeID
Delta <- Eta[,ncol(Eta)]
Alpha <- apply(alpha, 2:3, mean)
rownames(Alpha) <- Data$PlayerID
colnames(Alpha) <- Data$SurfaceID
Sigma <- apply(sigma, 2, mean)
save(Eta, Alpha, Delta, Sigma, file=paste0(side, ".RData"))
quit()

## Main
head(sort(Delta, decreasing=TRUE), 10)

## Surface
head(Alpha[order(Delta, decreasing=TRUE),], 10)

## Time series
X <- Eta[head(order(Delta, decreasing=TRUE), 10), ]
X <- Eta[c("Nadal R.", "Djokovic N.", "Murray A.", "Federer R."), ]
x <- as.Date(paste0(colnames(Eta), "-01"))
matplot(x, t(X), type="l", lty=1, lwd=3, col=pal(nrow(X)), las=1, xaxt="n")
axis.Date(1, x=x)
toplegend(legend=rownames(X), col=pal(nrow(X)), lwd=3)
toplegend(legend=rownames(X), col=pal(nrow(X)), lwd=3, horiz=FALSE, ncol=5)

## Sigma
vc <- Sigma^2
names(vc) <- c("Player", "Time", "Surface")
vc

## Prediction
predictMatch("Djokovic N.", "Federer R.", "Hard")
predictMatch("Baghdatis M.", "Goffin D.", "Hard")

## Tournament
predictTour(c("Djokovic N.", "Federer R.", "Baghdatis M.", "Goffin D."), "Hard", Delta, Alpha)
predictTour(c("Djokovic N.", "Baghdatis M.", "Goffin D.", "Federer R."), "Hard", Delta, Alpha)
best <- names(Delta)[order(Delta, decreasing=TRUE)]
predictTour(best[c(1,8,2,7,3,6,4,5)], "Hard", Delta, Alpha)

winnerProb(urlTour("http://www.ausopen.com/en_AU/scores/draws/ms/r4s1.html", "Hard"))
winnerProb(urlTour("http://www.ausopen.com/en_AU/scores/draws/ms/r5s1.html", "Hard"))
winnerProb(urlTour("http://www.ausopen.com/en_AU/scores/draws/ms/r6s1.html", "Hard"))
