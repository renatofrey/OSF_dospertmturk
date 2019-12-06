# fit mixed-effects models (both OLS and Bayesian)

# read data
fullsample <- read.csv("../data/fullsample.csv", row.names=1)

labels <- colnames(fullsample)
labels[grepl("R_", labels, fixed="T")] <- paste("R", 1:30, sep="")
labels[grepl("P_", labels, fixed="T")] <- paste("P", 1:30, sep="")
labels[grepl("B_", labels, fixed="T")] <- paste("B", 1:30, sep="")
colnames(fullsample) <- labels

# split into three subscales
R <- fullsample[,paste("R", 1:30, sep="")]
P <- fullsample[,paste("P", 1:30, sep="")]
B <- fullsample[,paste("B", 1:30, sep="")]

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
long <- reshape(fullsample, varying = list(paste("R", 1:30, sep=""),
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
m1 <- lmer(f, data=long)
m2 <- lmer(f2, data=long)

# ran rstanarm 1
set.seed(5555)
library(rstanarm)
m1_rstan <- stan_lmer(
  formula = f,
  data = long,
  cores = 3,
  chains = 3,
  iter = 2000,
  prior_intercept = normal(location = 0, scale = 10),
  prior = normal(location = 0, scale = 0.2)
)

# ran rstanarm 2
set.seed(5555)
library(rstanarm)
t1 <- Sys.time()
m2_rstan <- stan_lmer(
  formula = 2,
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

r2_rstan_m1 <- bayes_R2(m1_rstan)
r2_rstan_m2 <- bayes_R2(m2_rstan)

if (!dir.exists("../objects/stage3")) system("mkdir -p ../objects/stage3")
save(m1, m2, m1_rstan, m2_rstan, r2_rstan_m1, r2_rstan_m2, dur, file="../objects/stage3/lm.Rdata")

# NARROW PRIORS (centered on mean of response scale)
# prior_intercept = normal(location = 4, scale = 0.2),
# prior = normal(location = 0, scale = 0.2)
# => 13 minutes with intercepts only / "mod_rI"

# DEFAULTS PRIORS
# prior_intercept = normal(location = 0, scale = 10),
# prior = normal(location = 0, scale = 2.5)
# => 27 minutes with intercepts only
