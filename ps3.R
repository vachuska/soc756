library(HMDHFDplus)

#karl.vachuska@gmail.com
#_PuWPC4T2HNEaHq

df <- readHMDweb(CNTRY="USA")

df2 <- df[which(df$Year==2005),]

## I used the 2005 USA life tables to represent the Wisconsin life tables

library(LifeTables)

df3 <- data.frame(lt.mx(df2$Total, age=df2$Age)$lt)

prob1 <- (df3$lx[32]/df3$lx[17])

prob2 <- 1
for(i in 0:30) {
  prob2 <- prob2*(1-(0.062-0.000053*i^2))
}

print(prob1*prob2)

options(scipen=999)

noaccident <- 1
ca <- 0
for(i in 0:30) {
  if(i>=25) {
  ca <- ca+(0.062-0.000053*i^2)*.5*df3$ndx[i+1]*noaccident
  ca <- ca+(0.062-0.000053*i^2)*df3$lx[i+2]*noaccident
  }
  noaccident <- noaccident*(1-(0.062-0.000053*i^2))
}

print(ca/df3$lx[26])


noaccident <- 1
ds <- 0
for(i in 0:30) {
  if(i>=16) {
    noaccident2 <- noaccident*(1-(.5*(0.062-0.000053*i^2)))
    ds <- ds+df3$ndx[i+1]*noaccident2
  }
  noaccident <- noaccident*(1-(0.062-0.000053*i^2))
}

print(ds/df3$lx[17])