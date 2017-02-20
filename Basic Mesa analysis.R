#basic bulll shit

setwd("~/Dropbox/Wharton/z Other/1 Water start up/Mesa/Mesa")


mesa.raw <- read.csv("CSV-WaterConsumptionbyPremise_2554899078378849689.csv")

head(mesa.raw)
summary(mesa.raw)


mesa.raw <- mesa.raw[!is.na(mesa.raw$SERVICE_CLASS_CODE),]
