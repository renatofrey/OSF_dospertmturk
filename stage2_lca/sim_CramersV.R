cv.test = function(x,y) {
  CV = sqrt(chisq.test(x, y, correct=FALSE)$statistic /
              (length(x) * (min(length(unique(x)),length(unique(y))) - 1)))
  #print.noquote("Cramér V / Phi:")
  return(as.numeric(CV))
}

dat <- data.frame(s1 = rep(c("A", "B", "C", "D"), each=25),
                  s2 = c(rep(c("X", "Y", "Z", "W"), each=25)))

ind <- sample(1:nrow(dat), 25)
dat$s2[ind] <- sample(dat$s2[ind])

V <- cv.test(dat$s1, dat$s2)

if (T) {
  Vs <- NULL
  for (i in 1:10000) {
    if (i %% 100 == 0) print(i)
    Vs <- c(Vs, cv.test(dat$s1, sample(dat$s2)))
  }  
}

hist(Vs, breaks=25, col="brown", border=0, las=1, xlim=c(0, 1), main="Association between R and P", xlab="Cramer's V")
abline(v=V, col="blue", lwd=3)