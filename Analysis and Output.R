#output and anyais  /// SPLIT IT 

setwd("~/Dropbox/Wharton/z Other/1 Water start up/Mesa/Mesa")



#out.put.left  --> TO DO

load("predict_output_with_raw.RData", verbose = TRUE)

rev.multiplier <- .1*100

#note --> units are in 100s of gallons....I THINK 



#-------------------------------------------------------------------------------------------------------
#SD outside of the amount information

mesa.sd <- apply(mesa.raw[,8:20], MARGIN = c(1), sd)
mesa.sd <- as.data.frame(cbind(mesa.raw$PREMISES_CODE, mesa.sd))
colnames(mesa.sd) <- c("PREMISES_CODE", "SD_00_12")

for(i in 1:nrow(mesa.predict)){
  mesa.predict$SD_00_12[i] <- mesa.sd[mesa.predict$PREMISES_CODE[i]==mesa.sd$PREMISES_CODE, 2]
}

mesa.predict$DIFF <- mesa.predict$PRED_13 - mesa.predict$ACTUAL_13
mesa.predict$N_SD <- mesa.predict$DIFF / mesa.predict$SD_00_12


#-------------------------------------------------------------------------------------------------------
#Outside of RMSE of the predicted value

mesa.predict$N_RMSE <- mesa.predict$DIFF / comparision$RLasso

#USE SD ---> GOOD CC
#Note some issues with variablity equal to 0... there are multiple entries on each premise code ## i dont really want to evaluate right now. 

sort(mesa.predict$N_SD_adj, decreasing = TRUE)

mesa.predict[which.max(mesa.predict$N_SD),]

#clean
mesa.predict$N_SD_adj <- mesa.predict$N_SD
mesa.predict$N_SD_adj[mesa.predict$N_SD_adj == Inf] <- NA

#great!!
mesa.predict[which.max(mesa.predict$N_SD_adj),]

#rev. 
mesa.predict$REV <- rev.multiplier * mesa.predict$DIFF
stolen.water.mult <- mean(mesa.raw$ACTUAL_13)
mesa.other$REV <- rev.multiplier * stolen.water.mult * (1 - apply(mesa.other[,8:20], MARGIN = c(1), sum)/((20-8)*stolen.water.mult)) #ADJ For water reported

sum(mesa.predict$REV[mesa.predict$REV>0])
sum(mesa.other$REV[mesa.other$REV>0])

###
eval.key <- as.data.frame(
  cbind(c(3,2,1,.5,0), c("Very likely", "Likely", "Somewhat likely", "Unclear" ,"Unlikely"))
)


eval <- factor(rep(NA,nrow(mesa.predict)), levels= c("Very likely", "Likely", "Somewhat likely", "Unclear" ,"Unlikely"))



for(i in 1:nrow(mesa.predict)){
  if(is.na(mesa.predict$N_SD_adj[i])){eval[i] <- eval.key[5,2]
  }else if( mesa.predict$N_SD_adj[i] >= as.numeric(eval.key[1,1]) ) {eval[i] <- eval.key[1,2]
  }else if( mesa.predict$N_SD_adj[i] >= as.numeric(eval.key[2,1]) ) {eval[i] <- eval.key[2,2]
  }else if( mesa.predict$N_SD_adj[i] >= as.numeric(eval.key[3,1]) ) {eval[i] <- eval.key[3,2]
  }else if( mesa.predict$N_SD_adj[i] >= as.numeric(eval.key[4,1]) ) {eval[i] <- eval.key[4,2]
    }else{eval[i] <- eval.key[5,2]}
}


mesa.predict$classification <- eval



#-------------------------------------------------------------------------------------------------------
#Replacement proirty calssification /// Take the max (including) possitivie
mesa.predict$REV[mesa.predict$REV<0] <- 0
mesa.other$REV[mesa.other$REV<0] <- 0


#-------------------------------------------------------------------------------------------------------
#Export repaclement 

output.nontheft <- c()

output.nontheft$Meter_location <- mesa.predict$PREMISES_CODE
output.nontheft$Error_likelihood <- mesa.predict$classification
output.nontheft$Unexpected_variability <- mesa.predict$DIFF
output.nontheft$Meter_age <- rep("TBD", nrow(mesa.predict))
output.nontheft$Likelihood_OT_payment <- rep("TBD", nrow(mesa.predict))
output.nontheft$Est_lost_revenue <- mesa.predict$REV

write.csv(output.nontheft, file = "output_broken_meter.csv")

#-------------------------------------------------------------------------------------------------------
#Export stolen meter  

head(mesa.other)

output.theft <- c()

output.theft$Meter_location <- mesa.other$PREMISES_CODE
output.theft$Error_likelihood <- mesa.other$zero.class
output.theft$Meter_age <- rep("TBD", nrow(mesa.other))
output.theft$Likelihood_OT_payment <- rep("TBD", nrow(mesa.other))
output.theft$Est_lost_revenue <- mesa.other$REV

write.csv(output.theft, file = "output_theft.csv")







