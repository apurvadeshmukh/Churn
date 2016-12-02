####Linear regression R
fit <-lm(totalamount ~ Frequency + Avgamount, data=datacluster)
summary(fit)
