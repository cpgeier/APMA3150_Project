properties <- read.csv("properties_2017.csv")
properties[which(properties[,'numberofstories']==41),]
properties2 <- properties[!is.na(properties$numberofstories),]
properties2$taxamount
properties3 <- properties2[!is.na(properties2$taxamount),]
sort(properties3$taxamount)
qqplot(properties3$numberofstories,properties3$taxamount)
table(properties3$numberofstories)
#      1      2      3      4      5      6      7      8     14     17     20     41 
# 426471 243035  15708     68     18      4      2      1      1      1      1      1 
summary(properties3[which(properties3[,'numberofstories']==1),]$taxamount)
#Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#16.5    2238.6    3594.0    4510.9    5354.5 1840935.1 
summary(properties3[which(properties3[,'numberofstories']==2),]$taxamount)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#17.7   3108.3   4750.7   5771.5   7017.4 686876.6 
summary(properties3[which(properties3[,'numberofstories']==3),]$taxamount)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#79.4   2905.6   4731.2   6828.8   7381.1 845632.0 
summary(properties3[which(properties3[,'numberofstories']==4),]$taxamount)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#1763    7583   26685   82546   87919  533286 
p4<-properties3[which(properties3[,'numberofstories']==4),]
plot(p4$taxamount,p4$taxvaluedollarcnt)
qqnorm(p4$taxvaluedollarcnt)
qqplot(qexp(ppoints(length(p4$taxvaluedollarcnt))), p4$taxvaluedollarcnt)
mean(properties3$taxvaluedollarcnt,na.rm=TRUE)
#[1] 435499
sd(properties3$taxvaluedollarcnt,na.rm=TRUE)
#[1] 728364.6
test_sub<-properties3[properties3$taxvaluedollarcnt<700000,]
qqnorm(test_sub$taxvaluedollarcnt)
qqline(test_sub$taxvaluedollarcnt)
qqnorm(properties3$taxvaluedollarcnt[properties3$taxvaluedollarcnt<1000000])
qqline(properties3$taxvaluedollarcnt[properties3$taxvaluedollarcnt<1000000])
