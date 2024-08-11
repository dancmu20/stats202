# Load the packages
library(dplyr)
library(GGally)
	
# Create pair plots to explore interactions visually with all predictors
ggpairs(train_data %>% dplyr::select(query_length, is_homepage, sig1, sig2, sig3, sig4, sig5, sig6, sig7, sig8))