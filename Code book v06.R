######################################
#Reference code 
#
######################################

#----------------------------------------

#library used 
#library(randomForest)
#library(pROC) 
#library(class)
#library(ggplot2)
#library(ISLR)
#library(boot)
#library(leaps)
#library(glmnet)


#----------------------------------------



#LOGIT info

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#note this is the model verson 
confusion.matrix <- function(model, p = 0.5){
  predict <- predict(model, type = "response")
  
  #define levels reuquired
  y.name<-model$terms[[2]]
  y.data <- model$data[,colnames(model$data)==y.name]
  y.levels <- levels(y.data)
  
  #predicted levels vs. normal
  predict.levels <- rep(NA, length(predict))
  predict.levels <- data.frame(y.data, predict.levels)
  names(predict.levels) <- c(y.name, "predict")
  predict.levels$predict[predict<p] <- y.levels[1]
  predict.levels$predict[predict>=p] <- y.levels[2]
  
  #table - with right norder 
  t <- table(predict.levels)
  t.out <- t[,rev(colnames(t))]
  t.out <- t.out[rev(rownames(t)),]
  return(t.out)
}

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


rates.for.misclass <- function(confusion.matrix.in){
  
  misclassification.rate <- ( confusion.matrix.in[1,2] + confusion.matrix.in[2,1] ) / sum(confusion.matrix.in)
  sensitivity <- confusion.matrix.in[1,1] / sum(confusion.matrix.in[1,])
  specificity <- confusion.matrix.in[2,2] / sum(confusion.matrix.in[2,])
  
  list.out <- list(
    misclassification.rate = misclassification.rate, 
    sensitivity = sensitivity, 
    specificity = specificity)
  
  return(list.out)
  
}

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


misclassification.rate.only <- function(confusion.matrix.in){
  
  misclassification.rate <- ( confusion.matrix.in[1,2] + confusion.matrix.in[2,1] ) / sum(confusion.matrix.in)

  return(misclassification.rate)
  
}

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

## need to test & validate 
#note this is the model version 
## THIS IS WRONG --> COME BACK 

#ROC.table.glm <- function(model,sensativity=.01){
#p0 <- rep(from = 0, to=1, by = sensativity)
#
#output <- data.frame(p0 = p0, 
#One.minus.Specificity = rep(NA, length(p0)), Sensativity = #rep(NA, length(p0)), 
  #Sens.minus.One.minus.Spec = rep(NA, length(p0)),
  #True.Pos = rep(NA, length(p0)), True.Neg = rep(NA, length(#p0)),
    #False.Pos = rep(NA, length(p0)), False.Neg = rep(NA, length(#p0)))
      #for(i in p0){
      #conf.matrix <- confusion.matrix(model, i)
      #index <- output$p0==i
      #    
      #output$True.Pos[index] <- conf.matrix[1,1]
      #output$True.Neg[index] <- conf.matrix[2,2]
      #output$False.Pos[index] <- conf.matrix[2,1]
      #output$False.Neg[index] <- conf.matrix[1,2]
      #    
      #rates <- rates.for.misclass(conf.matrix)
      #output$One.minus.Specificity[index] <- 1 - rates$specificity
      #output$Sensativity[index] <- rates$sensitivity
      #output$Sens.minus.One.minus.Spec[index] <- output$Sensativity[index] - #output$One.minus.Specificity[index] 
        #}
        #  
        #return(output)
        #  
        #}
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

assign.one.value <- function(predicted.prob, p=.5, true.value=1, false.value=0){
  output <- rep(NA, length(predicted.prob))
  output[predicted.prob>=p] <- true.value
  output[predicted.prob<p] <- false.value
  return(output)
}

mis.class.rate.data.in <- function(actual.in, predicted.prob, p=.5, true.value=1, false.value=0 ){
  
  predicted.val  <- assign.one.value(predicted.prob, p, true.value, false.value)
  incorrect <- sum( actual.in != predicted.val)
  mis.rate <- incorrect / length(actual.in)
  return(mis.rate)
}


ROC.table.predicted.prob <- function(predicted.prob, actual.value, p.sensativity = 0.01, true.value =1, false.value =0){
  
  p <- seq(from = 0, to = 1, by = p.sensativity)
  output <- data.frame(p = p, 
                       One.minus.Specificity = rep(NA, length(p)), Sensativity = rep(NA, length(p)), 
                       Sens.minus.One.minus.Spec = rep(NA, length(p)),
                       True.Pos = rep(NA, length(p)), True.Neg = rep(NA, length(p)),
                       False.Pos = rep(NA, length(p)), False.Neg = rep(NA, length(p)),
                       mis.class = rep(NA, length(p)))
  
  for(i in p){
    predicted.value <-assign.one.value(predicted.prob = predicted.prob, p = i, true.value = true.value, false.value = false.value)
    
    index <- output$p==i
    
    output$True.Pos[index] <- sum(actual.value[actual.value==true.value]==predicted.value[actual.value==true.value])
    output$True.Neg[index] <- sum(actual.value[actual.value==false.value]==predicted.value[actual.value==false.value])
    
    output$False.Pos[index] <- sum(actual.value[actual.value==false.value]!=predicted.value[actual.value==false.value])
    output$False.Neg[index] <- sum(actual.value[actual.value==true.value]!=predicted.value[actual.value==true.value])
    
    output$One.minus.Specificity[index] <- 1 - ( output$True.Neg[index] / (output$True.Neg[index] + output$False.Pos[index]) ) 
    output$Sensativity[index] <- ( output$True.Pos[index] ) / (  output$True.Pos[index] + output$False.Neg[index] )
    output$Sens.minus.One.minus.Spec[index] <- output$Sensativity[index] - output$One.minus.Specificity[index] 
    
    output$mis.class[index] <- ( output$False.Pos[index] + output$False.Neg[index]) / (length(actual.value))
  }
  
  return(output)
}

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#this is the value of mis.classification vs. k for a KNN regression 

knn.k.vs.mis.class <- function(train, val, correct.train, correct.val, possible.k){
  
  output <- data.frame(k = possible.k)
  
  for(k in possible.k){
    knn.out <- knn(train = train, test = val, cl = correct.train, k = k)
    conf.matrix <- table(correct.val, knn.out)
    mis.class.rate <-misclassification.rate.only(conf.matrix)
    output$mis.class.rate[output$k==k] <- mis.class.rate
  }
  return(output)
}











#TO DO --> MAKE A FUNCTION that loops over the knn.k.vs.mis.class !!! --> THIS IS MORE THAN I WANT TO DO RIGHT NOW 




#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#predicted based on regsubsets output -->!!


predict.regsubsets.coeff <- function(coef, data.in, intercept = TRUE){
  start <- 1
  matched.columns <- match(names(coef),names(data.in))
  predicted <- rep(0, nrow(data.in))
  
  if(intercept){
    predicted <- predicted + coef[1]*rep(1, nrow(data.in))
    start <- 2
  }
  
  array <- start:length(matched.columns)
  
  for(i in array){
    predicted <- predicted + coef[i]*data.in[,matched.columns[i]]
  }
  return(predicted)
}


predict.book <- function(object, newdata ,id ,...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object ,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi 
}




#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#predicted based on lasso and ridge output -->!!



predict.lass.ridge.coeff <- function(coef, data.in, intercept = TRUE){
  start <- 1
  matched.columns <- match(names(coef[,1]),names(data.in))
  predicted <- rep(0, nrow(data.in))
  
  if(intercept){
    predicted <- predicted + coef[1]*rep(1, nrow(data.in))
    start <- 2
  }
  
  array <- start:length(matched.columns)
  
  for(i in array){
    predicted <- predicted + coef[i]*data.in[,matched.columns[i]]
  }
  return(predicted)
}



#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

RMSE <- function(actual, predicted){
  MSE <- mean( (actual - predicted) ^2 )
  RMSE <- sqrt(MSE)
  return(RMSE)
}


#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


## OTHER STUFFF

## ------------------------------------------------------------------------
timer.start <- function(name = "Timer 1"){ # A single argument 
  a <- Sys.time() # Save the current time into "a"
  return(list(NAME = name, START = a))  # Return the list
}

timer.stop <- function(st){ # "st" is the output from "timer.start"  
  b <- Sys.time() # Save the current time
  elapsed.time <- b - st[["START"]] # Work out the elapsed time
  print (paste(st[["NAME"]], "took:", # A human readable message
               elapsed.time, attributes(elapsed.time)$units)) 
}

## ------------------------------------------------------------------------

