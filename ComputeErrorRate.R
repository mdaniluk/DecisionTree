ComputeErrorRate <- function(setOfRules, data)
{
  
  ComputeError(data[, setOfRules@yName], PredictAll(setOfRules, data));
}

ComputeError <-function(yRef, yPredict)
{
  sum (yRef != yPredict)/length(yRef)
}