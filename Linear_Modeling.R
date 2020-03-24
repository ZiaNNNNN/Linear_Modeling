Model = c('Fierro 7B', 'HX 5000', 'Durbin Ultralight', 'Schmidt', 'WSilton Advanced', 'bicyclette velo', 'Supremo Team', 'XYC Racer', 'DOnofrio Pro', 'Americana #6')
Weight = c(17.9, 16.2, 15.0, 16.0, 17.3, 13.2, 16.3, 17.2, 17.7, 14.2)
Price = c(2200, 6350, 8470, 6300, 4100, 8700, 6100, 2680, 3500, 8100)
df <- data.frame(Model, Weight, Price)
plot(Weight, Price, main="Scatterplot", xlab="Bicycles Weight", ylab="Price")
model1 = lm(df$Price ~ df$Weight)
model1$coefficients
summary(model1)


LineSpeed = c(20,20,40,30,60,40)
numberofdefectivepart = c(21,19,15,16,14,17)
df2 = data.frame(LineSpeed, numberofdefectivepart)
plot(LineSpeed, numberofdefectivepart, main="Scatterplot2", xlab="Line Speed", ylab="Number of Defective Parts Found")
model2 = lm(df2$numberofdefectivepart ~ df2$LineSpeed)
summary(model2)

nodp = 50 * -0.14783 + 22.17391
sd(numberofdefectivepart)


WeeklyUsage = c(13,10,20,28,32,17,24,31,40,38)
AMexpense = c(17,22,30,37,47,30.5,32.5,39,51.5,40)
df3 = data.frame(WeeklyUsage, AMexpense)
plot(WeeklyUsage, AMexpense, main="Scatterplot3", xlab="Weekly Usage Hours", ylab="Annual Maintenance Expense")
model3 = lm(df3$AMexpense ~ df3$WeeklyUsage)
summary(model3)

Miles = c(22,29,36,47,63,77,73,87,92,101,110,28,59,68,68,91,42,65,110)
Price = c(16.2,16,13.8,11.5,12.5,12.9,11.2,13,11.8,10.8,8.3,12.5,11.1,15,12.2,13,15.6,12.7,8.3)
df4 = data.frame(Miles, Price)
plot(Miles, Price, main="Scatterplot4", xlab="Miles", ylab="Price")
model4 = lm(df4$Price ~ df4$Miles)
summary(model4)

library(ggplot2)
library(dplyr)
url = "https://raw.githubusercontent.com/jcbonilla/BusinessAnalytics/master/BAData/dodgers.csv"
Dodger = read.csv(url)
df5 = as.data.frame(Dodger)
p = ggplot(df5, aes(x=reorder(opponent, -attend), y=attend)) + stat_summary(fun.y="mean", geom="bar")
p + geom_hline(yintercept=mean(df5$attend), linetype="dashed", color = "red")
model5 = lm(df5$attend ~ df5$bobblehead + df5$fireworks + df5$shirt + df5$cap)
summary(model5)
p1 = ggplot(df5, aes(x=month, y=attend)) + stat_summary(fun.y="sum", geom="bar")
p1
sum(df5[((df5$month == 'JUN'|df5$month == 'JUL'|df5$month == 'AUG') & df5$bobblehead == 'YES'),]$attend)
