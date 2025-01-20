library(gdata)
library(polycor)
library(psych)
library(psy)
library(lavaan)


#-------------------------------------------------------------------
# BAI
#-------------------------------------------------------------------

# read the data from xls file
bai.all <- read.xls("bai.xls", header=TRUE)
bai <- bai.all[!duplicated(bai.all[,c("id")]),]

head(bai)
summary(bai)

# sort by id
# bai.s <- bai[order(bai$id) , ]

cbind(colMeans(bai[2:22]))
hist(rowSums(bai[2:22]))

# the items values without the participants' id
bai.dat <- bai[,2:22]

# 
y1 <- bai.dat[4] + bai.dat[5] + bai.dat[9] + bai.dat[10] + bai.dat[14] + bai.dat[17] 
y2 <- bai.dat[1] + bai.dat[3] + bai.dat[6] + bai.dat[8] + bai.dat[12] + bai.dat[13] + bai.dat[19]
          


