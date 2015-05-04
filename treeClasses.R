# rule.split contains information about split selection
rule.split <- setClass("rule.split", slots = c(attribute = "character", value = "character", operand = "character"))

# path  contains information about particular rule
rule.path <- setClass("rule.path", slots = c(rule.split = "list", yValue = "character"))
