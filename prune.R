pruneSetOfRules <-function(setOfRules, data, cp = 0)
{
  lastLength = lengthOfRules(setOfRules) + 1
  currentLength = lengthOfRules(setOfRules)
  while(currentLength < lastLength)
  {
    lastLength <- currentLength
    setOfRules = pruneRules(setOfRules, data, cp)
    currentLength <- lengthOfRules(setOfRules)
  }
  return (setOfRules)
}

pruneRules <-function(setOfRules, data, cp)
{
  if (lengthOfRules(setOfRules) == 0)
    return (setOfRules)
  
  print ("Pruning Rule...")
  bestSet <- setOfRules
  minErrorRate <- ComputeErrorRate(setOfRules,data) + cp
  
  for(i in 1:length(setOfRules@rules))
  {
    len <- length(setOfRules@rules[[i]]@rule.split)
    
    #rule contains only one split condition. Possibility to remove whole rule
    if (len == 1)
    {
      setOfRulesTmp <- setOfRules
      
      #remove rule from set
      setOfRulesTmp@rules <- setOfRulesTmp@rules[-i]
      errorRate = ComputeErrorRate(setOfRulesTmp, data)
      if (errorRate <= minErrorRate)
      {
        bestSet <- setOfRulesTmp
        minErrorRate <- errorRate
      }
    }
    
    # rule contains more than one split
    if (len > 1)
    {
      for (k in 1:len)
      {
        setOfRulesTmp <- setOfRules
        
        #remove k-th rule from set
        setOfRulesTmp@rules[[i]]@rule.split = setOfRulesTmp@rules[[i]]@rule.split[-k]
        errorRate = ComputeErrorRate(setOfRulesTmp, data)
        if (errorRate <= minErrorRate)
        {
          bestSet <- setOfRulesTmp
          minErrorRate <- errorRate
        }
      }
    }
  }
  return (bestSet)
}
lengthOfRules <- function(setOfRules) 
{
  len = 0
  for (rule in setOfRules@rules)
  {
    len <- len + length(rule@rule.split)
  }
  return (len)
}