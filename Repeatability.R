
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


# 3 binary varables TRUE/FALSE for each category

feeding$pos_vs_others <- feeding$response == "pos"
feeding$neg_vs_others <- feeding$response == "neg"
feeding$neu_vs_others <- feeding$response == "neu"


# Repeatability analysis

rpt_pos <- rpt(pos_vs_others ~ (1 | ID),
               grname = "ID",
               data = feeding,
               datatype = "Binary",
               nboot = 1000,  #number of bootstraps, can be changed if it takes to long to count
               npermut = 1000) #number of permutations for p, can be changed if it takes too long

rpt_pos

plot(rpt_pos)

# look at the Original-scale approx
# R- measure of variation in data due to consistent differences
# 0 - random ;1 - fully repetitive
# here R = 0.16 meaning 16% of the variation is due to repeatability  of individuals

# p - statistical significance
#one is based on likelihood ratio test, the other on the permutation,
# I like the permutation one better, but in general they should be
# consistent in meaning

# p = 0.001 - repeatability is statistically different from zero
# Meaning there is a statistically significant consistency in positive
# behavior, although it is moderate. 


rpt_neg <- rpt(neg_vs_others ~ (1 | ID),
               grname = "ID",
               data = feeding,
               datatype = "Binary",
               nboot = 500,  # I changed it so it's a bit faster
               npermut = 500) # same here

rpt_neg


#looking at the results the repeatability is stil there,
# but it is weaker than in the case of positive

plot(rpt_neg)



rpt_neu <- rpt(neu_vs_others ~ (1 | ID),
               grname = "ID",
               data = feeding,
               datatype = "Binary",
               nboot = 500,  # I changed it so it's a bit faster
               npermut = 500) # same here

rpt_neu

plot(rpt_neu)


# similar with neutral vs. other - significant repeatability, but not very strong.


