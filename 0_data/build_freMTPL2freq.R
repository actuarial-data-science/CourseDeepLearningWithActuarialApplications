# Code to clean the freMTPL2 data
# taken from https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3822407
# Appendix A.1
# based on work by F. Loser

library(OpenML)
library(farff)
library(CASdatasets)

freMTPL2freq <- getOMLDataSet(data.id = 41214)$data

dat <- freMTPL2freq [, -2]
dat$VehGas <- factor (dat$VehGas)
data(freMTPL2sev)
sev <- freMTPL2sev
sev$ClaimNb <- 1
dat0 <- aggregate (sev , by=list(IDpol = sev$IDpol), FUN = sum)[c (1 ,3:4)]
names ( dat0 )[2] <- "ClaimTotal"
dat <- merge (x=dat , y=dat0 , by ="IDpol", all.x= TRUE )
dat[is.na( dat )] <- 0
dat <- dat[which( dat$ClaimNb<=5),]
dat$Exposure <- pmin(dat$Exposure, 1)
sev <- sev[which(sev$IDpol %in% dat$IDpol), c(1 ,2)]
dat$VehBrand <- factor(dat$VehBrand, levels =c("B1","B2","B3","B4","B5","B6","B10","B11","B12","B13","B14"))

rm(freMTPL2freq,freMTPL2sev)

freMTPL2freq <- dat

str(freMTPL2freq)

save(freMTPL2freq,file="path_to_file/freMTPL2freq.RData")
