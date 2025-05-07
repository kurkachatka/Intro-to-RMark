

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


# VISUALISATION


for_plots <- aa.results$Phi.colony.p.time$results$real

Phi_estimates <- for_plots[1:2,] %>% mutate(colony = c('Good', 'Poor'))
p_estimates <- for_plots[3:9,] %>% mutate(time = seq(1:7))


Phi_estimates %>% ggplot(aes(y = estimate, x = colony)) +
  geom_errorbar(aes(ymin = lcl, ymax = ucl), size = 2, width = 0.2) +
  geom_point(size = 10) +
  theme_classic() +
  labs(y = 'Apparent survival')



p_estimates %>% ggplot(aes(y = estimate, x = time)) +
  geom_errorbar(aes(ymin = lcl, ymax = ucl), size = 2, width = 0.2) +
  geom_point(size = 10) +
  geom_line(size = 2) +
  theme_classic() +
  labs(y = 'Encounter probability')



# =======================LYSMA Model===================================


# LYSMA models are not implemented in RMark (although they are in MARK)
# a work around is to turn LYSMA data structure into CJS data structure

# function:

lysma_to_cjs <- function(data, ch_col = "ch") {
  result_list <- list()
  uid <- 1  # Unique ID counter for each chick
  
  for (i in seq_len(nrow(data))) {
    ch <- as.character(data[[ch_col]][i])
    counts <- as.numeric(substring(ch, seq(1, nchar(ch), 2), seq(2, nchar(ch), 2)))
    if (counts[1] == 0) next
    
    max_chicks <- counts[1]
    chick_matrix <- matrix(0, nrow = max_chicks, ncol = length(counts))
    
    # Generate encounter histories by assumed disappearance
    for (j in 1:max_chicks) {
      visible <- which(counts >= j)
      chick_matrix[j, visible] <- 1
    }
    
    ch_rows <- apply(chick_matrix, 1, paste0, collapse = "")
    
    # Repeat covariates for each chick
    covariate_rows <- data[rep(i, max_chicks), , drop = FALSE]
    covariate_rows$ch <- ch_rows
    covariate_rows$freq <- 1
    covariate_rows$uid <- seq(uid, uid + max_chicks - 1)
    uid <- uid + max_chicks
    
    result_list[[i]] <- covariate_rows
  }
  
  result <- do.call(rbind, result_list)
  return(result)
}



dummy_data <- data.frame(
  ch = c("03020100", "04040302"),
  Distance = c(123.4, 567.8),
  Habitat = c("urban", "rural")
)

cjs_dummy_data <- lysma_to_cjs(dummy_data)

str(cjs_dummy_data)

cjs_dummy_data <- as.

# now actual models



dd.process <- process.data(cjs_dummy_data, model="CJS", groups= "Habitat")

# make a DDL - designe data list (short: design data)

aa.ddl <- make.design.data(aa.process)










