

library(RMark)
library(tidyverse)

# ================================ simple CJS model ========================

aa <- convert.inp("aa",group.df=data.frame(colony=c("Poor","Good")))

# note the structure of the data, you always require ch - history and freq - frequency of each individual history
# colony is a variable added in the data set


# create processed data

aa.process <- process.data(aa, model="CJS", groups="colony")

# make a DDL - design data list (short: design data)

aa.ddl <- make.design.data(aa.process)


# create the function with models of interest
# components of the model Phi and p will be combined with each other

aa.models <-  function()
{
  # models for Phi (apparent survival)
  Phi.const <- list(formula = ~1) # simple, intercept only model assuming constant apparent survival
  Phi.time <- list(formula = ~time) # model allowing for apparent survival to vary in time
  # there is a difference between time and Time, the later checks for trend in the survival variation
  
  Phi.colony <- list(formula = ~colony) # using grouping factor as variable to influence apparent survival
  Phi.colony.time <- list(formula = ~time*colony) # checking for effect of interaction + additive effect
  # to check just for the effect interaction ':' works
  
  
  # models for p (encounter probability)
  
  p.dot <- list(formula = ~1) # constant
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


# a more functional approach would be to extract the components
# from the row names in the real estimates data frame
# a function for that:


extract_from_row_names <- function(df) {
  # split the row names by space
  split <- strsplit(rownames(df), " ")
  
  # combine into a data frame
  split_df <- as.data.frame(do.call(rbind, split), stringsAsFactors = FALSE)
  
  # name columns
  colnames(split_df) <- paste0("par", seq_len(ncol(split_df)))
  
  
  # combine with the original data frame
  df_fin <- cbind(df, split_df)
  
  return(df_fin)
}




for_plots_full <- extract_from_row_names(for_plots) %>%  # use the function
  rename(Parameter = par1, Colony = par2, time = par5) %>% # rename the columns if you want
  select(-par3, -par4, - fixed, - note) %>% # remove unused columns
  mutate(time = sub("^t", "", time), # remove the small letters from the varaibles
         Colony = sub("^g", "", Colony))



for_plots_full %>% filter(Parameter == 'Phi') %>% 
  ggplot(aes(y = estimate, x = Colony)) +
  geom_errorbar(aes(ymin = lcl, ymax = ucl), size = 2, width = 0.2) +
  geom_point(size = 10) +
  theme_classic() +
  labs(y = 'Apparent survival')



for_plots_full %>% filter(Parameter == 'p') %>%
  ggplot(aes(y = estimate,
             x = as.numeric(time))) + # otherwise the line will not join the points
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
  uid <- 1  # unique ID counter for each chick
  
  for (i in seq_len(nrow(data))) {
    ch <- as.character(data[[ch_col]][i])
    counts <- as.numeric(substring(ch, seq(1, nchar(ch), 2), seq(2, nchar(ch), 2)))
    if (counts[1] == 0) next
    
    max_chicks <- counts[1]
    chick_matrix <- matrix(0, nrow = max_chicks, ncol = length(counts))
    
    # generate encounter histories by assumed disappearance
    for (j in 1:max_chicks) {
      visible <- which(counts >= j)
      chick_matrix[j, visible] <- 1
    }
    
    ch_rows <- apply(chick_matrix, 1, paste0, collapse = "")
    
    # repeat covariates for each chick
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


# Dummy data for example

dummy_data <- data.frame(
  ch = c("03020100", "04040302"),
  Distance = c(123.4, 567.8),
  Habitat = c("urban", "rural")
)

# use the function to convert lysma to cjs

cjs_dummy_data <- lysma_to_cjs(dummy_data)

str(cjs_dummy_data) #check for structure

cjs_dummy_data$Habitat <- as.factor(cjs_dummy_data$Habitat) # grouping variables need to be factors


# now actual models

# process data and make DDL

dd.process <- process.data(cjs_dummy_data, model="CJS", groups= "Habitat")

dd.ddl <- make.design.data(dd.process)


# set models of interest

dd.models <- function()
{
  # models for Phi
  Phi.const <- list(formula = ~1)
  Phi.hab <- list(formula = ~Habitat)
  Phi.dist <- list(formula= ~Distance)
  
  
  #models for p
  p.dot <- list(formula = ~1)
  
  
  cml <- create.model.list("CJS")
  results <- mark.wrapper(cml, data = dd.process,
                       ddl = dd.ddl, adjust = FALSE)
  return(results)
}


dd.results <- dd.models()

dd.results


dd.results$Phi.dist.p.dot$results$beta # since Distance is a continuous variable
# beta estimates are more informative


dd.results$Phi.hab.p.dot$results$real # real estimates for the between-group differences 



# visualizing continuous data is a bit tricky
# estimates need to be simulated - FOR LATER





