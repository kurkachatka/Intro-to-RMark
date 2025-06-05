
library(tidyverse)
library(rptR)


# Toy data set to practice
# You don't need to do this step, but run it and at the end 
# look at the object 'feeding' - this is the structure of data that will work here

set.seed(36) # for reproducibility

# parameters
n_individuals <- 20 # number of individual females
n_repeats <- 10 #max number of times the feeding reaction was measured

# You need the ID of the females, here I just 
# generate them base on the n_individuals variable
ID <- rep(paste0("ID", 1:n_individuals), each = n_repeats)

# Simulate a response variable with some repeatability (not totally random)
# create the tendency towards one category
tendency <- sample(c("pos", "neg", "neu"), size = n_individuals, replace = TRUE)

# Generate response variable with some noise
response <- unlist(lapply(tendency, function(pref) {
  probs <- c(pos = 0.2, neg = 0.2, neu = 0.2)
  probs[pref] <- 0.6  # bias towards preferred response
  sample(c("pos", "neg", "neu"), size = n_repeats, replace = TRUE, prob = probs)
}))

# Combine into a data frame
feeding <- data.frame(ID = ID, response = response)



# the package rptR only supports binary data
# to go around thisisto run a seqence of binnary analysis
# pos vs. combined neg + neu
# neg vs. combined pos + neu
# neu vs. combined pos + neg
