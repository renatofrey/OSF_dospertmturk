# set two arbitrary numbers for splitting and blinding the dataset
seed1 <- 463704
seed2 <- 935708

# read in raw dataset and extract column lables
d_raw <- readRDS("dospert_raw.rds")

names(d_raw) <- gsub("RT", "R", names(d_raw), fixed=T)
names(d_raw) <- gsub("RP", "P", names(d_raw), fixed=T)
names(d_raw) <- gsub("RB", "B", names(d_raw), fixed=T)


# remove sensitive data
rem <- c('Zipcode', 'Past.Surveys', 'Past.week.Surveys', 'Comments', 'payrate', 'othersurveytopic', 'State_County', 'State.x', 'County.x', 'X2013.NCHS.scheme', 'Urban_Rural_Recode')
d_raw <- d_raw[,which(!is.element(colnames(d_raw), rem))]


# add SOEP risk-taking items
d_soep <- readRDS("soep.rds")
ind_r <- match(d_raw$unique_ID, d_soep$serial)
ind_c <- grepl("dohmen", colnames(d_soep))
d_soep <- d_soep[ind_r, ind_c]
rownames(d_soep) <- rownames(d_raw)
d_raw <- cbind(d_raw, d_soep)
cn <- colnames(d_raw)
cn[match("dohmen1", cn)] <- "SOEPgen"
cn[match("dohmencar", cn)] <- "SOEPdri"
cn[match("dohmenfin", cn)] <- "SOEPfin"
cn[match("dohmenrec", cn)] <- "SOEPrec"
cn[match("dohmencareer", cn)] <- "SOEPocc"
cn[match("dohmenhealth", cn)] <- "SOEPhea"
colnames(d_raw) <- cn

# regroup education measure
colnames(d_raw) <- gsub("Education", "Edu_ori", colnames(d_raw))
d_raw$Education <- NA
# High school level education or less (no degree or high school/ged)
d_raw$Education[is.element(d_raw$Edu_ori, c(1,2))] <- 1
# College Level Education (associates of both types or bachelors) 
d_raw$Education[is.element(d_raw$Edu_ori, c(3,4,5))] <- 2
# Graduate Level Education (masters, professional, or doctoral) 
d_raw$Education[is.element(d_raw$Edu_ori, c(6,7,8))] <- 3
d_raw$Education <- as.ordered(d_raw$Education)

# relevel political attitude
d_raw <- d_raw[,-which(colnames(d_raw) == "Political1")]
d_raw$Political <- gsub("1", "Dem.", d_raw$Political)
d_raw$Political <- gsub("2", "Rep.", d_raw$Political)
d_raw$Political <- gsub("3", "Ind.", d_raw$Political)
d_raw$Political <- as.factor(d_raw$Political)

labels <- names(d_raw)

# randomly assign respondents into two subsamples (exploratory and confirmatory subsamples)

## subsample A (N=1,500): exploratory sample
## Note: The first 10 participants are skipped because this subset of the data was accessible to everyone prior the blinding procedure.
N <- 1500
set.seed(seed1)
ind_subsampleA <- sort(sample(11:nrow(d_raw), size=N))

## subsample B (remaining respondents): confirmatory / hold-out sample
ind_subsampleB <- 1:nrow(d_raw)
ind_subsampleB <- ind_subsampleB[-ind_subsampleA]

d_raw$subsample <- NA
d_raw$subsample[ind_subsampleA] <- "A"
d_raw$subsample[ind_subsampleB] <- "B"

# create indices for the three different subscales
ind_R <- which(grepl("R_", labels))
ind_P <- which(grepl("P_", labels))
ind_B <- which(grepl("B_", labels))

# extract data into new objects
R <- d_raw[,ind_R]
P <- d_raw[,ind_P]
B <- d_raw[,ind_B]

# create a new order for the DOSPERT items
set.seed(seed2)
ind_shuffled <- sample(1:length(ind_R))

# reorder items but keep order between subscales consistent
R <- R[,ind_shuffled]
P <- P[,ind_shuffled]
B <- B[,ind_shuffled]

# create key
i <- 1:30
key <- data.frame(original = c(names(R), names(P), names(B)),
                  new = c(paste("R", i, sep=""),
                          paste("P", i, sep=""),
                          paste("B", i, sep=""))
                  )

# rename blinded data
names(R) <- paste("R", i, sep="")
names(P) <- paste("P", i, sep="")
names(B) <- paste("B", i, sep="")

# add identifier and subsample information to blinded data
d_blinded <- cbind(unique_ID = d_raw$unique_ID,
                   subsample = d_raw$subsample,
                   R, P, B)

# subset the various subsamples
subsampleA <- subset(d_raw, subsample == "A")
subsampleB <- subset(d_raw, subsample == "B")
subsampleA_blinded <- subset(d_blinded, subsample == "A")
subsampleB_blinded <- subset(d_blinded, subsample == "B")

# save key and shuffled datasets
write.csv(key, file="key.csv")
write.csv(subsampleA, file="subsampleA.csv")
write.csv(subsampleB, file="subsampleB.csv")
write.csv(subsampleA_blinded, file="subsampleA_blinded.csv")
write.csv(subsampleB_blinded, file="subsampleB_blinded.csv")
write.csv(d_raw, file="fullsample.csv")