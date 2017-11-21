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