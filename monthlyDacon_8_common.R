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


getPackages <- function(packs){
  packages <- unlist(
    tools::package_dependencies(packs, available.packages(),
                                which=c("Depends", "Imports"), recursive=TRUE)
  )
  packages <- union(packs, packages)
  return(packages)
}

lgb.normalizedgini = function(preds, dtrain){
  actual = getinfo(dtrain, "label")
  score  = MLmetrics::NormalizedGini(preds,actual)
  return(list(name = "gini", value = score, higher_better = TRUE))
}


