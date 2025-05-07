

library(RMark)
library(tidyverse)

# ================================ simple CJS model ========================

aa <- convert.inp("aa",group.df=data.frame(colony=c("Poor","Good")))

# note the structure of the data, you always require ch - history and freq - freqency of each individual history
# colony is a variable added in the dataset


# create processed data

aa.process <- process.data(aa, model="CJS", groups="colony")

# make a DDL - designe data list (short: design data)

aa.ddl <- make.design.data(aa.process)


# create the fucntion with models of interest
# components of the model Phi and p will be combined with each other

aa.models <-  function()
{
  # models for Phi (apparent survival)
  Phi.const <- list(formula = ~1) # simple, intercept only model assuming constant apparent survival
  Phi.time <- list(formula = ~time) # model allowing for apparent survival to vary in time
  # there is a difference between time and Time, the later checks for trend in the survival variation
  
  Phi.colony <- list(formula = ~colony) # using grouping factor as variable to influence apparent survival
  Phi.colony.time <- list(formula = ~time*colony) # checking for effect of intereaction + additive effect
  # to check just for the effect interaction ':' works
  
  
  # models for p (encounter probablity)
  
  p.dot <- list(formula = ~1) # contant
  p.time <- list(formula = ~time) # time dependent
  
  
  cml <- create.model.list("CJS")
  
  results <- mark.wrapper(cml,data = aa.process, ddl = aa.ddl, adjust = FALSE)
  
  return(results)
}




aa.results <- aa.models() # run the model

aa.results # see the ranking list of models

# get the top ranking model beta estimates

aa.results$Phi.colony.p.time$results$beta


# get the top ranking model actual estimates

aa.results$Phi.colony.p.time$results$real









