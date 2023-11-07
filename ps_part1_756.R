## My key assumption is that everyone is born endowed with their lifetime educational attainment

## I am also assuming that there is no mortality in the first five years of life

## I am also assuming that everyone dies by age 50

setwd("/home/karl/Downloads/")

df <- read.csv("nLx_values.csv")

df$group <- substr(df$cat_race_ed_age,1,3)

df$race <- substr(df$cat_race_ed_age, 2,2)

df$age <- gsub(".*_", "", df$cat_race_ed_age)

bdf <- df[which(df$race=="b"),]

wdf <- df[which(df$race=="w"),]

w <- read.csv("whitemob.csv")
w2 <- w[1:5,2:6]

b <- read.csv("blackmob.csv")
b2 <- b[1:5,2:6]

wgrr <- c(1.76, 1.81, 1.62, 1.54, 1.27)/6
bgrr <- c(2.18, 2.18, 1.67, 1.43, 1.01)/6

uniq <- unique(bdf$group)

uniqage <- as.numeric(unique(bdf$age))

blackmat_S <- list()
for(i in 2:length(uniqage)) {
  newmat <- matrix(0, nrow=5, ncol=5)
  surv <- c()
  for(j in 1:5) {
    surv <- c(surv, bdf$nLx[which(bdf$age==uniqage[i] & bdf$group==uniq[j])]/bdf$nLx[which(bdf$age==uniqage[i]-5 & bdf$group==uniq[j])])
  }
  diag(newmat) <- surv
  blackmat_S <- c(blackmat_S, list(newmat))
  
}

print(blackmat_S)


blackmat_B <- list()
for(i in 3:length(uniqage)) {
  newmat <- matrix(0, nrow=5, ncol=5)
  surv <- c()
  for(j in 1:5) {
    surv <- c(surv, 1-.5*(1-bdf$nLx[which(bdf$age==uniqage[i] & bdf$group==uniq[j])]/bdf$nLx[which(bdf$age==uniqage[i]-5 & bdf$group==uniq[j])]))
  }
  newmat <- (b2*bgrr)*surv/100
  blackmat_B <- c(blackmat_B, list(newmat))
  
}


print(blackmat_B)

uniq <- unique(wdf$group)

whitemat_S <- list()
for(i in 2:length(uniqage)) {
  newmat <- matrix(0, nrow=5, ncol=5)
  surv <- c()
  for(j in 1:5) {
    surv <- c(surv, wdf$nLx[which(wdf$age==uniqage[i] & wdf$group==uniq[j])]/wdf$nLx[which(wdf$age==uniqage[i]-5 & wdf$group==uniq[j])])
  }
  diag(newmat) <- surv
  whitemat_S <- c(whitemat_S, list(newmat))
  
}

print(whitemat_S)

whitemat_B <- list()
for(i in 3:length(uniqage)) {
  newmat <- matrix(0, nrow=5, ncol=5)
  surv <- c()
  for(j in 1:5) {
    surv <- c(surv, 1-.5*(1-wdf$nLx[which(wdf$age==uniqage[i] & wdf$group==uniq[j])]/wdf$nLx[which(wdf$age==uniqage[i]-5 & wdf$group==uniq[j])]))
  }
  newmat <- (w2*wgrr)*surv/100
  whitemat_B <- c(whitemat_B, list(newmat))
  
}


print(whitemat_B)