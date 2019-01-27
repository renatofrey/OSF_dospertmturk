# fit mixed-effects models (both OLS and Bayesian)

# read data
subsampleA <- read.csv("../data/subsampleA_blinded.csv", row.names=1)

# split into three subscales
R <- subsampleA[,paste("R", 1:30, sep="")]
P <- subsampleA[,paste("P", 1:30, sep="")]
B <- subsampleA[,paste("B", 1:30, sep="")]

# make ordinal
Rord <- R
Pord <- P
Bord <- B
for (i in 1:30) {
  Rord[,i] <- as.ordered(Rord[,i])
  Pord[,i] <- as.ordered(Pord[,i])
  Bord[,i] <- as.ordered(Bord[,i])
}



# reshape data into long format
long <- reshape(subsampleA, varying = list(paste("R", 1:30, sep=""),
                                           paste("P", 1:30, sep=""),
                                           paste("B", 1:30, sep="")),
                v.names = c("R", "P", "B"),
                timevar = "item",
                idvar = "unique_ID",
                direction = "long")

long <- long[order(long$unique_ID),]


f <- "R ~ 1 + P + B + (1 | unique_ID)"
f2 <- "R ~ 1 + P + B + (1 + P + B | unique_ID)"


# do a quick first pass with lme4
library(lme4)
m <- lmer(f2, data=long)

# ran rstanarm
set.seed(5555)
library(rstanarm)
t1 <- Sys.time()
mod_rstan <- stan_lmer(
  formula = f2,
  data = long,
  cores = 3,
  chains = 3,
  iter = 2000,
  prior_intercept = normal(location = 0, scale = 10),
  prior = normal(location = 0, scale = 0.2)
)
t2 <- Sys.time()
dur <- t2-t1
print(dur)

r2_bayes <- bayes_R2(mod_rstan)
print(median(r2_bayes))

if (!dir.exists("../objects/stage2")) system("mkdir -p ../objects/stage2")
save(m, mod_rstan, r2_bayes, dur, file="../objects/stage2/lm.Rdata")

# NARROW PRIORS (centered on mean of response scale)
# prior_intercept = normal(location = 4, scale = 0.2),
# prior = normal(location = 0, scale = 0.2)
# => 13 minutes with intercepts only / "mod_rI"

# DEFAULTS PRIORS
# prior_intercept = normal(location = 0, scale = 10),
# prior = normal(location = 0, scale = 2.5)
# => 27 minutes with intercepts only
