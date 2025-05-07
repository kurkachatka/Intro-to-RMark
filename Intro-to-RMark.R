

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




