
#library(pROC) 
#library(class)
#library(ggplot2)
#library(ISLR)
#library(boot)
library(leaps)
library(glmnet)
library(ggplot2)

source("Basic Mesa analysis.R")
source("Code book v06.R")


#-------------------------------------------------------------------------------------------------------

# Split data 

seed <- 2016
set.seed(seed)

mesa.use <- mesa.raw[mesa.raw$ACTUAL_13!=0,]
mesa.other <- mesa.raw[mesa.raw$ACTUAL_13==0,]

split <- sample(x = c(TRUE, FALSE), size = nrow(mesa.use), prob = c(.4,.6), replace = TRUE)
mesa.train <-mesa.raw[split,]
mesa.val <- mesa.raw[(split!=TRUE),]

nrow(mesa.train) + nrow(mesa.val)
nrow(mesa.raw)

head(mesa.train)



#-------------------------------------------------------------------------------------------------------

k <- 5
n.trials <- 1
number.of.variables <- ncol(mesa.train)-1
set.seed(seed)
RMSE.out <- rep(NA, number.of.variables)

x.in <- mesa.train[,-20]
x.in <- model.matrix(~., x.in)
y.in <- mesa.train$ACTUAL_13
df.in <- cbind(x.in, y.in)

?model.matrix

val.x.in <-model.matrix(~., mesa.val[,-20])

#val.x.in <- mesa.val[,-20]
val.y.in <- mesa.val$ACTUAL_13
comparision <- c()

#-------------------------------------------------------------------------------------------------------
# ridge

ridge.vs.lasso <- 0
best.lamda <- c()

for(i in 1:n.trials){
  cv.out <- cv.glmnet(x = as.matrix(x.in), y = y.in, alpha = ridge.vs.lasso, nfolds = k ) 
  best.lamda[i] <- cv.out$lambda.min
}

mean(best.lamda)
var(best.lamda)
best.lamda.use <- mean(best.lamda)

model.fit <- glmnet(x = as.matrix(x.in), y = y.in, alpha = ridge.vs.lasso, lambda = best.lamda.use)
predicted.use <- predict.glmnet(object = model.fit, newx = val.x.in)
RMSE.out <- RMSE( actual = val.y.in, predicted = predicted.use)

comparision$Ridge <- RMSE.out

#-------------------------------------------------------------------------------------------------------
# lasso

ridge.vs.lasso <- 1
best.lamda <- c()

for(i in 1:n.trials){
  cv.out <- cv.glmnet(x = as.matrix(x.in), y = y.in, alpha = ridge.vs.lasso, nfolds = k ) 
  best.lamda[i] <- cv.out$lambda.min
}

mean(best.lamda)
var(best.lamda)
best.lamda.use <- mean(best.lamda)

model.fit <- glmnet(x = as.matrix(x.in), y = y.in, alpha = ridge.vs.lasso, lambda = best.lamda.use)
predicted.use <- predict.glmnet(object = model.fit, newx = val.x.in)
RMSE.out <- RMSE( actual = val.y.in, predicted = predicted.use)

comparision$RLasso <- RMSE.out

#-------------------------------------------------------------------------------------------------------

#FULL MODEL --> USE LASSO --> THIS OVER FITS.... need to do a bootstrap to do this statstically rigorously.... come back here....

#model.fit <- glmnet(x = model.matrix(~.,mesa.use[,-20]), y = mesa.use$ACTUAL_13 , alpha = 1, lambda = best.lamda.use)

#-------------------------------------------------------------------------------------------------------
#code the output...prediction

predict.use <- predict(model.fit, newx = model.matrix(~.,mesa.use[,-20]))
mesa.predict <- as.data.frame(cbind(mesa.use$PREMISES_CODE, mesa.use$ACTUAL_13, predict.use))
colnames(mesa.predict) <- c("PREMISES_CODE", "ACTUAL_13", "PRED_13")
mesa.predict <- as.data.frame(output.mesa.use)

#mean(sqrt((output.mesa.use$ACTUAL_13 - output.mesa.use$PRED_13)^2))

#-------------------------------------------------------------------------------------------------------

#OHTER 

head(mesa.other)

#if 1 year ago a 0 +- 3 montsh --> fine 
#if last month 0 --> fine 

eval <- as.data.frame(cbind(mesa.other$ACTUAL_00==0,  mesa.other$ACTUAL_01==0, mesa.other$ACTUAL_02==0, mesa.other$ACTUAL_12 ==0))
eval.sum <- apply(eval, MARGIN = c(1), FUN = sum )
eval.key <- as.data.frame(
  cbind(0:4, c("Very likely", "Likely", "Somewhat likely", "Unclear" ,"Unlikely"))
  )

eval.key.apply <- factor(rep(NA,length(eval.sum)), levels= c("Very likely", "Likely", "Somewhat likely", "Unclear" ,"Unlikely"))
  
for(i in 1:length(eval.sum)){
  eval.key.apply[i] <- eval.key[eval.sum[i]==eval.key[,1],2]
}

mesa.other$zero.class <- eval.key.apply

#-------------------------------------------------------------------------------------------------------
#Join together

#mesa.output <- mesa.raw 
#mesa.predict.full <- c()


#for(i in 1:nrow(mesa.predict)){
#  t <- subset(mesa.output, mesa.output$PREMISES_CODE == mesa.predict$PREMISES_CODE[i])
#  t <- as.data.frame(cbind(t, mesa.predict$PRED_13[i]))
#  mesa.predict.full <- as.data.frame(rbind(mesa.predict.full,t))
#}

#colnames(mesa.predict.full)[length(colnames(mesa.predict.full))] <- "PRED_13"

#head(mesa.predict.full)

#nrow(mesa.predict)
#nrow(mesa.predict.full)

#-------------------------------------------------------------------------------------------------------
#write out the predictions for analysis
#DOPE
list.test <- list(mesa.other, mesa.raw, mesa.predict,comparision)

save(mesa.other, mesa.raw, mesa.predict, comparision, file = "predict_output_with_raw.RData")







