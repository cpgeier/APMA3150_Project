#Analysis of Number of Rooms to House Valuations
# Christopher Geier

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
dev.off()

#-----------------------------------------------------------

#Analysis of Lot Size vs Tax Value
#James Mekavibul 
#11/21/2017
#APMA 3150
James_properties <- read.csv("properties_2017.csv")

#Data cleaning
James_properties2 <- James_properties[!is.na(James_properties$lotsizesquarefeet),]
James_properties3 <- James_properties2[!is.na(James_properties2$taxvaluedollarcnt),]

#Removing outliers
James_properties4 <- James_properties3[which(James_properties3[,'lotsizesquarefeet']<20000000),]

#Looking at a scatter plot and finding linear regression
summary(James_properties4$lotsizesquarefeet)
#     Min.   1st Qu.    Median      Mean     3rd Qu.    Max. 
#       1    189547     322471      446866   518864     128060323 
plot(density(log10(James_properties4$lotsizesquarefeet)))
plot(James_properties4$lotsizesquarefeet,James_properties4$taxvaluedollarcnt,main='Plot of Lot Size vs. Tax Value',xlab='Lot Size (sq ft)',ylab="Tax Value (dollars)")
fit_dollars <- lm ((James_properties4$taxvaluedollarcnt) ~(James_properties4$lotsizesquarefeet)) 
abline(fit_dollars, col='red')
summary(fit_dollars)
#The scatter plot looks a like many values are being taxed a large range regardless of size. 
#In addition, lots of plots are getting taxed the same amount even though they vary in size.

plot(James_properties4$lotsizesquarefeet,James_properties4$taxvaluedollarcnt,log="yx", main='Plot of Lot Size vs. Tax Value',xlab='Lot Size (sq ft)',ylab="Tax Value (dollars)")
#The log scale further emphasizes there is no correlation between lot size and value in dollars
fit_lotsize_dollars <- lm (log10(James_properties4$taxvaluedollarcnt) ~ log10(James_properties4$lotsizesquarefeet)) 
abline(fit_lotsize_dollars, col='red')
summary(fit_lotsize_dollars)

#Residuals:
#Min      1Q  Median      3Q     Max 
#-5.5105 -0.2046  0.0291  0.2368  2.5764 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                                5.2202140  0.0023308  2239.6   <2e-16 ***
#  log10(James_properties4$lotsizesquarefeet) 0.0658906  0.0005875   112.1   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.3976 on 2711980 degrees of freedom
#Multiple R-squared:  0.004616,	Adjusted R-squared:  0.004616 
#F-statistic: 1.258e+04 on 1 and 2711980 DF,  p-value: < 2.2e-16
#The residuals are quite varying, while the 

#The residual standard error is .3976 which is a quite large difference between the residuals. This means that the 
#regression model is not fitting to the data. As a result, there is significant variability between the residuals as seen in the
#summary. Thus, further analysis must be done to see if lot size and tax value is correlated at all.

# Correlation testing:
# Can't Using shapiro-wilk test to find normality of both tax value and lot size
# Sample size is a bit too large


# The pearson correlation test benchmarks linear relationship.
res_pearson<-cor.test(James_properties4$lotsizesquarefeet,James_properties4$taxvaluedollarcnt, method = "pearson"); res_pearson
res_pearson$p.value
# [1] 0.007428044

# The spearman correlation test benchmarks monotonic relationship. 
# As a result, the spearman test is good for non-normal data sets.
res_spear<-cor(James_properties4$lotsizesquarefeet,James_properties4$taxvaluedollarcnt, method = "spearman"); res_spear
# [1] 0.118919

# Both correlation coefficients are very close to 0, meaning there is no correlation between the variables. this is further emphasized by the scatter plot. 
# There is no correlation between lot size and tax value.

dev.off()
#-----------------------------------------------------------

#Analysis of Quality and Year Built vs Tax Value
# Elijah Bosley
properties <- read.csv("properties_2017.csv")

properties2 <- properties[!is.na(properties$buildingqualitytypeid),]
properties2$taxvaluedollarcnt
properties3 <- properties2[!is.na(properties2$taxvaluedollarcnt),]


sort(properties3$taxvaluedollarcnt)
qqplot(properties3$buildingqualitytypeid,properties3$taxvaluedollarcnt)
table(properties3$buildingqualitytypeid)

# Quality goes from lowest to highest
# 1      2      3      4      5      6      7      8      9     10     11     12 
# 1776    319  17849 447959 107013 561475 190239 501882  69418  10628  28488   4123 


summary(properties3[which(properties3[,'buildingqualitytypeid']==1),]$taxvaluedollarcnt)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1887  124343  321210  547948  786996 9875000 
summary(properties3[which(properties3[,'buildingqualitytypeid']==12),]$taxvaluedollarcnt)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 52488  1808753  2826905  3780920  4534618 49061236

par(mfrow=c(4,3))
for (quality in c(1:12)) {
  qual = properties3[which(properties3[,'buildingqualitytypeid']==quality),]
  plot(qual$yearbuilt,qual$taxvaluedollarcnt, main= paste("Year built vs. tax value for quality", quality, "homes"))
  
}
dev.off()
# look at the highest quality homes now
qual12 = properties3[which(properties3[,'buildingqualitytypeid']==12),]
qqnorm(qual12$taxvaluedollarcnt)
qqplot(qexp(ppoints(length(qual12$taxvaluedollarcnt))), qual12$taxvaluedollarcnt)

plot(qual12$regionidneighborhood, qual12$taxvaluedollarcnt)
# almost normal plot, interesting

dev.off()
#-----------------------------------------------------------

#Analysis of Region vs Tax Value:
# Peter Cybriwsky
properties <- read.csv("properties_2017.csv")
properties[which(properties[,'numberofstories']==41),]
properties2 <- properties[!is.na(properties$numberofstories),]
properties2$taxamount
properties3 <- properties2[!is.na(properties2$taxamount),]
sort(properties3$taxamount)
properties2$regionidcounty
table(properties2$regionidcounty)

summary(properties3[which(properties3[,'regionidcounty']==1286),]$taxvaluedollarcnt)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
# 2733   203004   339808   425808   520117 34172644        4 
summary(properties3[which(properties3[,'regionidcounty']==2061),]$taxvaluedollarcnt)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
# 4544   222709   348674   412403   511270 26879210       12 
#
summary(properties3[which(properties3[,'regionidcounty']==3101),]$taxvaluedollarcnt)
# Region ID: 1286
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
# 2733   203004   339808   425808   520117 34172644        4 

# Region ID: 2061
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
# 4544   222709   348674   412403   511270 26879210       12 

# Region ID: 3101
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 1059    205484    409034   1150386    879180 151895087 


p1286<-properties3[which(properties3[,'regionidcounty']==1286),]
p2061<-properties3[which(properties3[,'regionidcounty']==2061),]
p3101<-properties3[which(properties3[,'regionidcounty']==3101),]
allVals<-c(p1286$taxvaluedollarcnt, p2061$taxvaluedollarcnt, p3101$taxvaluedollarcnt)
plot(allVals, col = c("black", "red", "blue"), main = "Tax Value for three regions", ylab = "Tax Value")
legend(x, y = NULL, legend = c("1286", "2061", "3101"), col = c("black", "red", "blue"))
region <- c(rep("1286",468620), rep("2061",203756), rep("3101",12935))
region
region <- factor(region)
region
region_Model = allVals ~ region
region_Model
allVals
boxplot(region_Model, ylab ="Tax Dollar Count", xlab = "Region Code", main="Tax Amount for three regions.")
results = aov(region_Model)
print(summary(results))
# Df    Sum Sq   Mean Sq F value Pr(>F)    
# region           2 6.763e+15 3.382e+15    6495 <2e-16 ***
# Residuals   685292 3.568e+17 5.206e+11                   
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 16 observations deleted due to missingness
dev.off()