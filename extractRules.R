extractRules <- function(model, print = FALSE)
{
  if (!inherits(model, "rpart")) stop("Not a legitimate rpart tree")

  # Get some information.
  frm     <- model$frame
  names   <- row.names(frm)
  ylevels <- attr(model, "ylevels")
  ds.size <- model$frame[1,]$n
  
  # Loop for each leaf node and convert it to rule
  rules = c();
  for (i in 1:nrow(frm))
  {
    if (frm[i,1] == "<leaf>")
    {
      pth <- path.rpart(model, nodes=as.numeric(names[i]), print.it=FALSE)
      
      if (print) {
        cat("\n")
        cat(sprintf(" Rule number: %s ", names[i]))
        cat(sprintf("[yval=%s cover=%d (%.0f%%) prob=%0.2f]\n",
                    ylevels[frm[i,]$yval], frm[i,]$n,
                    round(100*frm[i,]$n/ds.size), frm[i,]$yval2[,5]))
        cat(sprintf("   %s\n", unlist(pth)[-1]), sep="")
      }
      
      rule.split.list = rpartPathToRules(pth)
      rule1 <- rule.path(rule.split=rule.split.list, yValue = ylevels[frm[i,]$yval])
      
      rules = c (rules, rule1)
      
    }
  }
  return (rules)
}

rpartPathToRules <- function(pth)
{
  rule.split.list = c()
  for (k in 2:length(pth[[1]])) {
    if (grepl("<", pth[[1]][k])) {
      tmp = strsplit(pth[[1]][k], split = "< ", fixed = TRUE)
      attribute = tmp[[1]][1]
      operand = "<";
      value = tmp[[1]][2]
      split = rule.split(attribute=attribute, value=value, operand=operand)
      rule.split.list = c(rule.split.list, split)
    }
    if (grepl(">=", pth[[1]][k])) {
      tmp = strsplit(pth[[1]][k], split = ">=", fixed = TRUE)
      attribute = tmp[[1]][1]
      operand = ">=";
      value = tmp[[1]][2]
      split = rule.split(attribute=attribute, value=value, operand=operand)
      rule.split.list = c(rule.split.list, split)
    }
  }
  return (rule.split.list)
}