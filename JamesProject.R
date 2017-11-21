#Analysis of Lot Size vs Tax Value
#Jm9hp
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


#T test:
t.test(James_properties4$lotsizesquarefeet,James_properties4$taxvaluedollarcnt)
quantile(James_properties4$lotsizesquarefeet)

# Welch Two Sample t-test

# data:  James_properties4$lotsizesquarefeet and James_properties4$taxvaluedollarcnt
# t = -1009.1, df = 2839500, p-value < 2.2e-16
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
# -425261.1 -423612.4
# sample estimates:
#   mean of x mean of y 
# 22429.03 446865.74 
#Reject null hypothesis, accept alternative hypothesis that the means are not equal.

