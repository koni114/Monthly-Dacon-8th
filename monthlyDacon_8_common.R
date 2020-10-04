outlier_IQRTr <- function(x){
  if(is.numeric(x)){
    x.stats <- boxplot.stats(x)$out
    x[x %in% x.stats] <- NA
    as.numeric(x)
  }else{
    x
  }
}

mkAUCValue <- function(YHat, Y){
  # levels(Y)    <- c(0, 1)
  # levels(YHat) <- c(0, 1)
  pred <- prediction(YHat, Y)
  # perf <- performance(pred, measure = "tpr", x.measure = "fpr")
  auc <- performance(pred, measure = "auc")
  auc@y.values[[1]]
}
