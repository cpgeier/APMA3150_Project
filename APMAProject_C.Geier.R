properties <- read.csv("properties_2017.csv")

# Dimensions of dataset
dim(properties)
# [1] 2985217      58

# Remove na entries
properties <- properties[!is.na(properties$numberofstories),]
properties <- properties[!is.na(properties$taxvaluedollarcnt),]

# Dimensions of dataset
dim(properties)
#[1] 685640     58

# Only include subset with similar properties
# Some entries were not for single house and therefore were much larger
# 700000 was chose as it was the point where the taxvaluedollarcnt value vs index
# stopped being mostly linear. A different value could have been chosen,
# but since this subset includes a great majority of the originial data 
# excluding outliers, the subset is representative of the larger dataset.

properties_700k <- properties[properties$taxvaluedollarcnt<500000,]

hist(properties_700k$basementsqft)
hist(properties_700k$bathroomcnt)
hist(properties_700k$bedroomcnt)
table(properties_700k$numberofstories)
hist(properties_700k$taxvaluedollarcnt)

qqnorm(properties_700k$taxvaluedollarcnt,main="Normal Q-Q Plot of House Tax Valuations")
qqline(properties_700k$taxvaluedollarcnt,col='red')

# Testing whether or not the bedroom count of the house is correlated with the tax valuation of the house 

t.test(properties_700k$taxvaluedollarcnt[properties_700k$bedroomcnt==2],properties_700k$taxvaluedollarcnt[properties_700k$bedroomcnt==3])
# p-value < 2.2e-16
# Highly significant, p value < 0.05. The mean house valuation is different for 2 and 3 bedroom houses

t.test(properties_700k$taxvaluedollarcnt[properties_700k$bedroomcnt==6],properties_700k$taxvaluedollarcnt[properties_700k$bedroomcnt==8])
# p-value = 0.1498
# Not significat, p value > 0.05. The difference in mean house valuation for houses of 6 and 8 bedrooms is not statistically significant.
# This is probably a result of the number of houses having 6 and 8 bedrooms are very small.
