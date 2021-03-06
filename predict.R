PredictAll <- function(setOfRules, data)
{
  predictions = c()
  for (i in 1:nrow(data)) {
    predictions = c(predictions, PredictClassFromOneExample(setOfRules, data[i,]))  
  }
  
  return (predictions)
}

PredictClassFromOneExample <- function(setOfRules, example)
{
  ret <- c()
  for (rule in setOfRules@rules)
  {
    if (ApplyRule(rule, example))
    {
      ret = c(ret, rule@yValue)
    }
  }
  
  if (length(ret) == 0) {
    return (setOfRules@yDefaultValue)
  }
  
  rl = rle(ret)
  ret = rl$values[rl$lengths == max(rl$lengths)]
  if (length(ret) == 1)
  {
    return(ret)	
  }
  if (any(ret == setOfRules@yDefaultValue))
  {
    return(setOfRules@yDefaultValue)
  }
  return(ret[1])
  
}


#check particular rule
ApplyRule <- function(rule, example)
{
  for (part in rule@rule.split)
  {
    if (!ApplyRuleSplit(part, example))
    {
      return (FALSE)
    }
  }
  return (TRUE)
}

#check particular condition
ApplyRuleSplit <- function(condition, example)
{
  attribute = example[, condition@attribute]
  if (is.na(attribute))
  {
    return (FALSE)
  }
  if(condition@operand == ">=") {
    return (attribute >= as.numeric(condition@value))
  }
  if(condition@operand == "<") {
    return (attribute < as.numeric(condition@value))
  }
  warning('Unknown operand!')
  return(FALSE)
}