setwd('C:/Users/lenovo/Desktop')
Callcenter <- read.csv("BIT-430-RS-Call-Center-Metrics.csv")
# take a look at Dataset
str(Callcenter)
summary(Callcenter)

boxplot.stats(Callcenter$AvgHoldTime)
boxplot.stats(Callcenter$AvgTimePhonePerDay)
boxplot.stats(Callcenter$AvgSpeedAnswer)
boxplot.stats(Callcenter$AvgTimePhoneTalk)
boxplot(Callcenter$AvgTimePhonePerDay)
Callcenter_clean=subset(Callcenter, Callcenter$AvgTimePhonePerDay>81290 & Callcenter$AvgTimePhonePerDay<46672 & Callcenter$AvgTimePhoneTalk>554 & Callcenter$AvgTimePhoneTalk<344)

boxplot.stats(Callcenter_clean$AvgTimePhoneTalk)
boxplot.stats(Callcenter_clean$AvgTimePhonePerDay)
Reg <- lm(Callcenter_clean$AvgCustSatScore ~ 0+Callcenter_clean$AvgHoldTime + Callcenter_clean$AvgSpeedAnswer + Callcenter_clean$AvgTimePhoneTalk + Callcenter_clean$AvgTimePhonePerDay + Callcenter_clean$AvgPercentAbandRate + Callcenter_clean$AvgPercentFirstCallSuccess, data = Callcenter)

summary(Reg)
cor(Callcenter_clean$AvgHoldTime, Callcenter_clean$AvgCustSatScore, method = c("pearson", "kendall", "spearman"))
anova(Reg,test="Chisq")

Reg_dropped <- lm(Callcenter_clean$AvgCustSatScore ~ 0+Callcenter_clean$AvgHoldTime + Callcenter_clean$AvgSpeedAnswer + Callcenter_clean$AvgTimePhoneTalk + Callcenter_clean$AvgTimePhonePerDay + Callcenter_clean$AvgPercentFirstCallSuccess, data = Callcenter_clean)
summary(Reg_dropped)
anova(Reg_dropped)

Reg_final <- lm(Callcenter_clean$AvgCustSatScore ~ 0+ Callcenter_clean$AvgSpeedAnswer + Callcenter_clean$AvgTimePhoneTalk + Callcenter_clean$AvgPercentAbandRate + Callcenter_clean$AvgPercentFirstCallSuccess, data = Callcenter_clean)
summary(Reg_final)
Reg_final <- lm(Callcenter_clean$AvgCustSatScore ~ 0+Callcenter_clean$AvgHoldTime + Callcenter_clean$AvgSpeedAnswer + Callcenter_clean$AvgTimePhoneTalk + Callcenter_clean$AvgPercentFirstCallSuccess, data = Callcenter_clean)
summary(Reg_final)
anova(Reg_final, test="Chi")

Callcenter_clean$Record <-NULL
Callcenter_clean$EmoID <-NULL
Callcenter_clean$EmpID <-NULL
Callcenter_clean$AvgPercentAbandRate <-NULL
dataclust<-data.frame(Callcenter_clean)
dh<-dist(dataclust)
dh_sc<-as.data.frame(scale(dh))
as.dist(dh_sc, diag = FALSE, upper = FALSE)
ch<-hclust(d = dh_sc, method = "complete")








