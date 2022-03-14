#-------------Question 1------------
college=read.csv("College.csv",header=TRUE)
college
View(college)
college$Private <- as.factor(college$Private)
rownames (college) <- college[, 1]
View (college)
college <- college[, -1]
View (college)
summary(college)
college

pairs(college[,1:10])
View (college)

plot(college$Private, college$Outstate, xlab = "Private", ylab = "Out-of-state tuition (dollars)")

Elite = rep("No", nrow(college))
Elite[college$Top10per>50] = "Yes"
Elite = as.factor(Elite)
college = data.frame(college, Elite)

summary(college$Elite)

plot(college$Elite, college$Outstate, xlab = "Elite", ylab = "Out-of-state tuition (dollars)")

par(mfrow = c(2, 2))
hist(college$Apps, xlab = "Number of applicants", main = "Histogram for all colleges")
hist(college$Apps[college$Private == "Yes"], xlab = "Number of applicants", main = "Histogram for private schools")
hist(college$Apps[college$Private == "No"], xlab = "Number of applicants", main = "Histogram for public schools")
hist(college$Apps[college$Elite == "Yes"], xlab = "Number of applicants", main = "Histogram for elite schools")

par(mfrow = c(2, 2))



hist(college$Accept, xlab = "Acceptance", main = "Histogram for all colleges")
hist(college$Accept[college$Private == "Yes"], xlab = "Acceptance", main = "Histogram for private schools")
hist(college$Accept[college$Private == "No"], xlab = "Acceptance", main = "Histogram for public schools")
hist(college$Accept[college$Elite == "Yes"], xlab = "Acceptance", main = "Histogram for elite schools")

par(mfrow = c(2, 2))
hist(college$F.Undergrad, xlab = "Full time undergrads", main = "Histogram for all colleges")
hist(college$F.Undergrad[college$Private == "Yes"], xlab = "Full time undergrads", main = "Histogram for private schools")
hist(college$F.Undergrad[college$Private == "No"], xlab = "Full time undergrads", main = "Histogram for public schools")
hist(college$F.Undergrad[college$Elite == "Yes"], xlab = "Full time undergrads", main = "Histogram for elite schools")

peffrence.perc=100 - ((college$Accept-college$Enroll)/(college$Accept))*100
college=data.frame(college,peffrence.perc)

dream.status=rep("No", nrow(college))
dream.status[college$peffrence.perc > 90] = "Yes"
dream.status = as.factor(dream.status)
college = data.frame(college, dream.status)

par(mfrow = c(2, 2))
hist(college$peffrence.perc, xlab = "peffrence.perc", main = "Histogram for all colleges")
hist(college$peffrence.perc[college$Private == "Yes"], xlab = "peffrence.perc", main = "Histogram for private schools")
hist(college$peffrence.perc[college$Private == "No"], xlab = "peffrence.perc", main = "Histogram for public schools")
hist(college$peffrence.perc[college$Elite == "Yes"], xlab = "peffrence.perc", main = "Histogram for elite schools")

# This histogram shows us that there is public school have the majority of students getting enrolled with them once they are accepted.
summary(college$peffrence.perc[college$Private == "Yes"])
summary(college$peffrence.perc[college$Private == "No"])
summary(college$peffrence.perc[college$Elite == "No"])
summary(college$F.Undergrad[college$dream.status == "Yes"])

#-------------question2-------------------

library(ISLR2)
View(Auto)
Auto = na.omit(Auto)
dim(Auto)
#--
head(Auto)
#mpg, cylinders, displacement, horsepower, weight, acceleration, year are quantitative
#origin, name are qualitative. Origin is represented in quantitative manner but it is encoded as per the country name.

range(Auto$mpg)
range(Auto$cylinders)
range(Auto$displacement)
range(Auto$horsepower)
range(Auto$weight)
range(Auto$acceleration)
range(Auto$year)

# Following are the ranges of the quantitative variables
# mph- 46.6
# cylinders- 8
# displacement- 455
# horsepower-230
# weight- 5140
# acceleration- 24.8
# year - 82

colMeans(Auto[, 1:7])
apply(Auto[, 1:7], MARGIN = 2, FUN = "sd")

# Varibale          Mean          Standard deviation
# 
# mpg               23.445918     7.805007
# cylinders         5.471939      1.705783 
# displacement      194.411990    104.644004
# horsepower        104.469388    38.491160
# weight            2977.584184   849.402560
# acceleration      15.541327     2.758864
# year              75.979592     3.683737


apply(Auto[-(10:85), 1:7], MARGIN = 2, FUN = "range")
apply(Auto[-(10:85), 1:7], MARGIN = 2, FUN = "mean")
apply(Auto[-(10:85), 1:7], MARGIN = 2, FUN = "sd")  

# Varibale          Mean          Standard deviation     range
# 
# mpg               24.404430     7.867283               46.6             
# cylinders         5.373418      1.654179               8
# displacement      187.240506    99.678367              455
# horsepower        100.721519    35.708853              230
# weight            2935.971519   811.300208             4997
# acceleration      15.726899     2.693721               24.8
# year              77.145570     3.106217               82


par(mfrow = c(1, 3))
plot(Auto$horsepower, Auto$mpg, xlab = "Horsepower", ylab = "Miles per gallon")
plot(Auto$weight, Auto$mpg, xlab = "Car weight (pounds)", ylab = "Miles per gallon")
plot(Auto$weight, Auto$horsepower, xlab = "Car weight (pounds)", ylab = "horsepower")

#As the graphs show weight increases with horsepower,As the horse power increases mpg decreases.
#that means weight and horse power are directly proportional whereas inversely proportional to mph

par(mfrow = c(2, 2))
plot(Auto$weight, Auto$acceleration, xlab = "Car weight (pounds)", ylab = "0 to 60mph time (seconds)")
plot(Auto$cylinders, Auto$acceleration, xlab = "Number of engine cylinders", ylab = "0 to 60mph time (seconds)")
plot(Auto$displacement, Auto$acceleration, xlab = "Engine displacement (cubic inches)", ylab = "0 to 60mph time (seconds)")
plot(Auto$horsepower, Auto$acceleration, xlab = "Horsepower", ylab = "0 to 60mph time (seconds)")
#Acceleration decreases as the weight increases
#Acceleration decreases with increase in horse power and and engine displacement

Auto$origin[Auto$origin == 1] = "American"
Auto$origin[Auto$origin == 2] = "European"
Auto$origin[Auto$origin == 3] = "Japanese"
Auto$origin = as.factor(Auto$origin)
plot(Auto$origin, Auto$mpg, xlab = "Country of origin", ylab = "Miles per gallon")

# To obtain higher mph as per the above plots, horsepower, weight and origin  play a major role in deciding mph
# we can see japanese prouce cars with higher mph followed by europeans then americans.


#------------------------question 3---------------------------------------

library(ISLR2)
View(Boston)
head(Boston)
dim(Boston)

# crim- Per capita crime rate for the tract 
# zn - Percent of residential land zoned for lots over 25000 square feet per town (constant for all tracts within the same town)
# indus- Percent of non-retail business acres per town (constant for all tracts within the same town)
# chas  -Dummy variable to indicate whether or not the tract borders the Charles River (1 = Borders Charles River, 0 = Otherwise)
# nox   -Nitric oxides concentration (in parts per 10 million) per town (constant for all tracts within the same town) 
# rm  -Average number of rooms per dwelling in the tract
# age - Percent of owner-occupied units in the tract built prior to 1940   
# dis -Weighted distance from the tract to five Boston employment centers
# rad -Index of accessibility to radial highways per town (constant for all tracts within the same town)
# tax - Full-value property tax rate per $10000 per town (constant for all tracts within the same town)
# ptratio - Pupil-teacher ratio per town (constant for all tracts within the same town)
# lstat - Percent of tract population designated as lower status
# medv -  Median value of owner-occupied housing in $1000 for the tract

par(mfrow = c(2, 2))
plot(Boston$age, Boston$medv, xlab = "Percent of units built prior to 1940", ylab = "Median home value in $1000s")
plot(Boston$lstat, Boston$medv, xlab = "Percent of lower status residents", ylab = "Median home value in $1000s")
plot(Boston$medv, Boston$ptratio, xlab = "Median home value in $1000s", ylab = "Pupil-teacher ratio")
plot(as.factor(Boston$chas), Boston$medv, xlab = "Borders Charles River", ylab = "Median home value in $1000s")

# the above plots indicate the median home value decreses with increase in percent of lower status residents

par(mfrow = c(2, 2))
plot(Boston$age, Boston$crim, xlab = "Percent of units built prior to 1940", ylab = "Per capita crime rate for the tract ")
plot(Boston$lstat, Boston$crim, xlab = "Percent of lower status residents", ylab = "Per capita crime rate for the tract s")
plot(Boston$medv, Boston$crim, xlab = "Median home value in $1000s", ylab = "Per capita crime rate for the tract ")

# in this plot i tried to establish relation between crime rate and other variables.The neighbourhood with old properties show relatively high crime rate

par(mfrow = c(2, 2))
plot(Boston$lstat, Boston$crim, xlab = "Percent of lower status residents", ylab = "Per capita crime rate")
plot(Boston$medv, Boston$crim, xlab = "Median home value in $1000s", ylab = "Per capita crime rate")
plot(Boston$dis, Boston$crim, xlab = "Weighted distance to Boston employment centers", ylab = "Per capita crime rate")
#crime rate is more if there are more low status residents

par(mfrow = c(2, 2))
hist(Boston$crim, xlab = "Per capita crime rate", main = "Histogram of Boston crime rates")
hist(Boston$tax, xlab = "Tax rate per 10000 USD", main = "Histogram of Boston tax rates")
hist(Boston$ptratio, xlab = "Pupil-teacher ratio", main = "Histogram of Boston pupil-teacher ratios")

summary(Boston[, c(1, 10, 11)])

# # crim               tax           ptratio     
# # Min.   : 0.00632   Min.   :187.0   Min.   :12.60  
# # 1st Qu.: 0.08205   1st Qu.:279.0   1st Qu.:17.40  
# # Median : 0.25651   Median :330.0   Median :19.05  
# # Mean   : 3.61352   Mean   :408.2   Mean   :18.46  
# # 3rd Qu.: 3.67708   3rd Qu.:666.0   3rd Qu.:20.20  
# Max.   :88.97620   Max.   :711.0   Max.   :22.00  
# As the data represents Boston appear to have particularly high crime rates,Tax rates,Pupil-teacher ratios.
# By comparing min,max and median values of the predictor we can to this conclusion

sum(Boston$chas)
#35

summary(Boston$ptratio)
#median 19.05

min(Boston$medv)
Boston[Boston$medv == 5, ]

# crim zn indus chas   nox    rm age    dis rad tax ptratio lstat medv
# 399 38.3518  0  18.1    0 0.693 5.453 100 1.4896  24 666    20.2 30.59    5
# 406 67.9208  0  18.1    0 0.693 5.683 100 1.4254  24 666    20.2 22.98    5

summary(Boston[, c(1:13)])

# crim                zn             indus            chas              nox               rm             age        
# Min.   : 0.00632   Min.   :  0.00   Min.   : 0.46   Min.   :0.00000   Min.   :0.3850   Min.   :3.561   Min.   :  2.90  
# 1st Qu.: 0.08205   1st Qu.:  0.00   1st Qu.: 5.19   1st Qu.:0.00000   1st Qu.:0.4490   1st Qu.:5.886   1st Qu.: 45.02  
# Median : 0.25651   Median :  0.00   Median : 9.69   Median :0.00000   Median :0.5380   Median :6.208   Median : 77.50  
# Mean   : 3.61352   Mean   : 11.36   Mean   :11.14   Mean   :0.06917   Mean   :0.5547   Mean   :6.285   Mean   : 68.57  
# 3rd Qu.: 3.67708   3rd Qu.: 12.50   3rd Qu.:18.10   3rd Qu.:0.00000   3rd Qu.:0.6240   3rd Qu.:6.623   3rd Qu.: 94.08  
# Max.   :88.97620   Max.   :100.00   Max.   :27.74   Max.   :1.00000   Max.   :0.8710   Max.   :8.780   Max.   :100.00  

# dis              rad              tax           ptratio          lstat            medv      
# Min.   : 1.130   Min.   : 1.000   Min.   :187.0   Min.   :12.60   Min.   : 1.73   Min.   : 5.00  
# 1st Qu.: 2.100   1st Qu.: 4.000   1st Qu.:279.0   1st Qu.:17.40   1st Qu.: 6.95   1st Qu.:17.02  
# Median : 3.207   Median : 5.000   Median :330.0   Median :19.05   Median :11.36   Median :21.20  
# Mean   : 3.795   Mean   : 9.549   Mean   :408.2   Mean   :18.46   Mean   :12.65   Mean   :22.53  
# 3rd Qu.: 5.188   3rd Qu.:24.000   3rd Qu.:666.0   3rd Qu.:20.20   3rd Qu.:16.95   3rd Qu.:25.00  
# Max.   :12.127   Max.   :24.000   Max.   :711.0   Max.   :22.00   Max.   :37.97   Max.   :50.00  

sum(Boston$rm > 7)
#64
sum(Boston$rm > 8)
#13
Boston[Boston$rm>8,]

# crim zn indus chas    nox    rm  age    dis rad tax ptratio lstat medv
# 98  0.12083  0  2.89    0 0.4450 8.069 76.0 3.4952   2 276    18.0  4.21 38.7
# 164 1.51902  0 19.58    1 0.6050 8.375 93.9 2.1620   5 403    14.7  3.32 50.0
# 205 0.02009 95  2.68    0 0.4161 8.034 31.9 5.1180   4 224    14.7  2.88 50.0
# 225 0.31533  0  6.20    0 0.5040 8.266 78.3 2.8944   8 307    17.4  4.14 44.8
# 226 0.52693  0  6.20    0 0.5040 8.725 83.0 2.8944   8 307    17.4  4.63 50.0
# 227 0.38214  0  6.20    0 0.5040 8.040 86.5 3.2157   8 307    17.4  3.13 37.6
# 233 0.57529  0  6.20    0 0.5070 8.337 73.3 3.8384   8 307    17.4  2.47 41.7
# 234 0.33147  0  6.20    0 0.5070 8.247 70.4 3.6519   8 307    17.4  3.95 48.3
# 254 0.36894 22  5.86    0 0.4310 8.259  8.4 8.9067   7 330    19.1  3.54 42.8
# 258 0.61154 20  3.97    0 0.6470 8.704 86.9 1.8010   5 264    13.0  5.12 50.0
# 263 0.52014 20  3.97    0 0.6470 8.398 91.5 2.2885   5 264    13.0  5.91 48.8
# 268 0.57834 20  3.97    0 0.5750 8.297 67.0 2.4216   5 264    13.0  7.44 50.0
# 365 3.47428  0 18.10    1 0.7180 8.780 82.9 1.9047  24 666    20.2  5.29 21.9

summary(Boston[Boston$rm > 8, c(1:13)])

# crim               zn            indus             chas             nox               rm             age       
# Min.   :0.02009   Min.   : 0.00   Min.   : 2.680   Min.   :0.0000   Min.   :0.4161   Min.   :8.034   Min.   : 8.40  
# 1st Qu.:0.33147   1st Qu.: 0.00   1st Qu.: 3.970   1st Qu.:0.0000   1st Qu.:0.5040   1st Qu.:8.247   1st Qu.:70.40  
# Median :0.52014   Median : 0.00   Median : 6.200   Median :0.0000   Median :0.5070   Median :8.297   Median :78.30  
# Mean   :0.71879   Mean   :13.62   Mean   : 7.078   Mean   :0.1538   Mean   :0.5392   Mean   :8.349   Mean   :71.54  
# 3rd Qu.:0.57834   3rd Qu.:20.00   3rd Qu.: 6.200   3rd Qu.:0.0000   3rd Qu.:0.6050   3rd Qu.:8.398   3rd Qu.:86.50  
# Max.   :3.47428   Max.   :95.00   Max.   :19.580   Max.   :1.0000   Max.   :0.7180   Max.   :8.780   Max.   :93.90  
# dis             rad              tax           ptratio          lstat           medv     
# Min.   :1.801   Min.   : 2.000   Min.   :224.0   Min.   :13.00   Min.   :2.47   Min.   :21.9  
# 1st Qu.:2.288   1st Qu.: 5.000   1st Qu.:264.0   1st Qu.:14.70   1st Qu.:3.32   1st Qu.:41.7  
# Median :2.894   Median : 7.000   Median :307.0   Median :17.40   Median :4.14   Median :48.3  
# Mean   :3.430   Mean   : 7.462   Mean   :325.1   Mean   :16.36   Mean   :4.31   Mean   :44.2  
# 3rd Qu.:3.652   3rd Qu.: 8.000   3rd Qu.:307.0   3rd Qu.:17.40   3rd Qu.:5.12   3rd Qu.:50.0  
# Max.   :8.907   Max.   :24.000   Max.   :666.0   Max.   :20.20   Max.   :7.44   Max.   :50.0  

#The major thing we can observe with atleast 8 dwellings is lower crime rates, lower nitric oxide levels and less number of lower status residents

#-----------------Question 4----------------------------

library(ISLR2)

Auto=na.omit(Auto)

auto.lin.fit = lm(mpg ~ horsepower, data = Auto)
summary(auto.lin.fit)

# Call:
#   lm(formula = mpg ~ horsepower, data = Auto)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -13.5710  -3.2592  -0.3435   2.7630  16.9240 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 39.935861   0.717499   55.66   <2e-16 ***
#   horsepower  -0.157845   0.006446  -24.49   <2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 4.906 on 390 degrees of freedom
# Multiple R-squared:  0.6059,	Adjusted R-squared:  0.6049 
# F-statistic: 599.7 on 1 and 390 DF,  p-value: < 2.2e-16

# #Simple linear regression gives a model  Y^=39.935861???0.157845X1  between the predictor horsepower and the response mpg. A p-value of essentially zero for  ??^1=???0.157845  gives very strong evidence that there is a relationship between mpg and horsepowerSince  R2=0.6059 ,
# approximately 60.6% of the variability in mpg is explained by a linear regression onto horsepower. This is a modest relationship between the predictor and the response, since as discussed in the chapter we can improve our  R2  value to 0.688 by including a quadratic term. 
# The value of  ??^1  itself indicates that in the model each increase of 1 horsepower results on average in a decrease of 0.157845 miles per gallon. In other words, in this model there is a negative relationship between the predictor and the response.
predict(auto.lin.fit, data.frame(horsepower = 98), interval = "confidence")

# fit      lwr      upr
# 1 24.46708 23.97308 24.96108

predict(auto.lin.fit, data.frame(horsepower = 98), interval = "prediction")
# fit     lwr      upr
# 1 24.46708 14.8094 34.12476

plot(Auto$horsepower, Auto$mpg, xlab = "Horsepower", ylab = "Miles per gallon")
abline(auto.lin.fit, lwd = 3, col = "red")

# as we have seen earlriler wecan observe the negative slope here.

par(mfrow = c(2, 2))
plot(auto.lin.fit)

# # all the plots here represent the un even distribution of data ,the u shape resmbles the same thing(non linear data) ,
# which inturn states that simple regression model is not a good fit.

#--------------question 5-------------------------
Auto$origin[Auto$origin == 1] = "American"
Auto$origin[Auto$origin == 2] = "European"
Auto$origin[Auto$origin == 3] = "Japanese"
Auto$origin = as.factor(Auto$origin)
head(Auto)

pairs(~mpg + cylinders + displacement + horsepower + weight + acceleration + year, Auto)

cor(Auto[,-c(8, 9)])

# mpg  cylinders displacement horsepower     weight acceleration       year
# mpg           1.0000000 -0.7776175   -0.8051269 -0.7784268 -0.8322442    0.4233285  0.5805410
# cylinders    -0.7776175  1.0000000    0.9508233  0.8429834  0.8975273   -0.5046834 -0.3456474
# displacement -0.8051269  0.9508233    1.0000000  0.8972570  0.9329944   -0.5438005 -0.3698552
# horsepower   -0.7784268  0.8429834    0.8972570  1.0000000  0.8645377   -0.6891955 -0.4163615
# weight       -0.8322442  0.8975273    0.9329944  0.8645377  1.0000000   -0.4168392 -0.3091199
# acceleration  0.4233285 -0.5046834   -0.5438005 -0.6891955 -0.4168392    1.0000000  0.2903161
# year          0.5805410 -0.3456474   -0.3698552 -0.4163615 -0.3091199    0.2903161  1.0000000

mpg.fit = lm(mpg ~ . - name, data = Auto)
summary(mpg.fit)

# Residuals:
#   Min      1Q  Median      3Q     Max 
# -9.0095 -2.0785 -0.0982  1.9856 13.3608 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    -1.795e+01  4.677e+00  -3.839 0.000145 ***
#   cylinders      -4.897e-01  3.212e-01  -1.524 0.128215    
# displacement    2.398e-02  7.653e-03   3.133 0.001863 ** 
#   horsepower     -1.818e-02  1.371e-02  -1.326 0.185488    
# weight         -6.710e-03  6.551e-04 -10.243  < 2e-16 ***
#   acceleration    7.910e-02  9.822e-02   0.805 0.421101    
# year            7.770e-01  5.178e-02  15.005  < 2e-16 ***
#   originEuropean  2.630e+00  5.664e-01   4.643 4.72e-06 ***
#   originJapanese  2.853e+00  5.527e-01   5.162 3.93e-07 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 3.307 on 383 degrees of freedom
# Multiple R-squared:  0.8242,	Adjusted R-squared:  0.8205 
# F-statistic: 224.5 on 8 and 383 DF,  p-value: < 2.2e-16

contrasts(Auto$origin)

#             European Japanese
# American        0        0
# European        1        0
# Japanese        0        1

# #Fstatic value is 224.5 and there is stromg relationship with predictors
# #The predictors that appear to have a statistically significant relationship to the response mpg are displacement with a p-value of 0.001863, and weight, year, originEuropean, and originJapanese with p-values of essentially zero. The coefficients for cylinders, horsepower, and acceleration have p-values 
# #which are not small enough to provide evidence of a statistically significant relationship to the response mpg.
# #The coefficient of 0.777 for the year variable suggests that when we fix the number of engine cylinders, engine displacement, horsepower, weight, acceleration, and country of origin, fuel efficiency 
# increases on average by about 0.777 miles per gallon each year

par(mfrow = c(2, 2))
plot(mpg.fit)

#As explained in the previous question the u shape resembles the non linearity of data
#remember that after dropping the rows with null values, there are 392 observations in the data set, giving an average leverage value of  9/392???0.023 . There is one point with a leverage value of about 0.10, which is almost 5 times greater than the average.
#There is another point with a leverage of about 0.20, which is almost 10 times greater than the average.

#using degree (^2)
mpg.fit.all.interactions = lm(mpg ~ (. - name)^2, data = Auto)
summary(mpg.fit.all.interactions)

mpg.fit.reduced.interactions = update(mpg.fit.all.interactions, ~ . - horsepower:origin)
summary(mpg.fit.reduced.interactions)

mpg.fit.reduced.interactions = update(mpg.fit.reduced.interactions, ~ . - displacement:horsepower)
summary(mpg.fit.reduced.interactions)

mpg.fit.reduced.interactions = update(mpg.fit.reduced.interactions, ~ . - weight:acceleration)
summary(mpg.fit.reduced.interactions)

mpg.fit.reduced.interactions = update(mpg.fit.reduced.interactions, ~ . - weight:year)
summary(mpg.fit.reduced.interactions)

mpg.fit.reduced.interactions = update(mpg.fit.reduced.interactions, ~ . - cylinders:horsepower)
summary(mpg.fit.reduced.interactions)

mpg.fit.reduced.interactions = update(mpg.fit.reduced.interactions, ~ . - cylinders:origin)
summary(mpg.fit.reduced.interactions)

mpg.fit.reduced.interactions = update(mpg.fit.reduced.interactions, ~ . + cylinders:origin - displacement:acceleration)
summary(mpg.fit.reduced.interactions)

mpg.fit.reduced.interactions = update(mpg.fit.reduced.interactions, ~ . - cylinders:origin)
summary(mpg.fit.reduced.interactions)

mpg.fit.reduced.interactions = update(mpg.fit.reduced.interactions, ~ . - cylinders:displacement)
summary(mpg.fit.reduced.interactions)

mpg.fit.reduced.interactions = update(mpg.fit.reduced.interactions, ~ . - cylinders:weight)
summary(mpg.fit.reduced.interactions)

summary(lm(mpg ~ . + cylinders:displacement - name, data = Auto))





par(mfrow = c(2, 2))
plot(Auto$displacement, Auto$mpg)
plot(Auto$horsepower, Auto$mpg)
plot(Auto$weight, Auto$mpg)
plot(Auto$acceleration, Auto$mpg)

summary(lm(mpg ~ acceleration, data = Auto))

par(mfrow = c(2, 2))
plot(lm(mpg ~ acceleration, data = Auto))

summary(lm(mpg ~ log(acceleration), data = Auto))

par(mfrow = c(2, 2))
plot(lm(mpg ~ log(acceleration), data = Auto))

displacement.linear = lm(mpg ~ displacement, data = Auto)
summary(displacement.linear)

displacement.quadratic = lm(mpg ~ poly(displacement, 2), data = Auto)
summary(displacement.quadratic)

anova(displacement.linear, displacement.quadratic)

displacement.quintic = lm(mpg ~ poly(displacement, 5), data = Auto)
summary(displacement.quintic)

anova(displacement.quadratic, displacement.quintic)

#----------------question 6-------------------

library(ISLR)
head(Carseats)

carseats.fit.1 = lm(Sales ~ Price + Urban + US, data = Carseats)
summary(carseats.fit.1)
contrasts(Carseats$Urban)

contrasts(Carseats$US)

#p2

# The coefficient of -0.054459 for Price means that, for a given location (i.e. fixed values of Urban and US), 
# increasing the price of a car seat by $1 results in a decrease of sales by approximately 54.46 units, on average, 
# in the model. The coefficient of -0.021916 for UrbanYes means that, for a given carseat price point and value of US, 
# the model predicts urban areas to have approximately 22 fewer carseat sales on average compared to non-urban areas.
# The coefficient of 1.200573 for USYes means that, for a given car seat price point and value of Urban,
# the model predicts that stores in the United States have 1201 more carseat sales on average than stores outside 
# the United States.

#p3

# The model has the following equation.
# 
# Y^=13.043???0.054X1???0.022X2+1.200X3
# 
# Here,  y^  is the estimated carseat sales, in thousands of car seats;
# x1j  is the price of the carseat at the jth store, in dollars; and  x2j  and  x3j  are dummy variables to 
# represent whether or not the  j th store at is located in an urban area and in the United States, respectively. 
# More concretely,  x2j  and  x3j  use the following coding scheme.
# 
# x2j=   {1 if the jth store is in an urban location 0if the jth store is not in an urban location}
# x3j=={1 if the jth store is in the United States, 0if the jth store is not in the United States}



#p4

# The p-values for the intercept, Price, and USYes are all essentially zero,
# which provides strong evidence to reject the null hypothesis  H0:??j=0  for those predictors. 
# The p-value for UrbanYes, however, is 0.936, 
# so there is no evidence to reject the null hypothesis that it has a non-zero coefficient in 
# the true relationship between the predictors and Sales.

#p5
carseats.fit.2 = lm(Sales ~ Price + US, data = Carseats)
summary(carseats.fit.2)
#p6
par(mfrow = c(2, 2))
plot(carseats.fit.1)

par(mfrow = c(2, 2))
plot(carseats.fit.2)

#p7
confint(carseats.fit.2)

#p8
# When we look at the residuals vs. leverage plot for the model from Part 5 that I generated in Part 6,
# we see that there are a number of observations with standardized residuals close to 3 in absolute value.
# Those observations are possible outliers. We can also see in the same plot that 
# there are number of high leverage points with leverage values greatly exceeding the average leverage of  3/400=0.0075 ,
# though those high leverage observations are not likely outliers, as they have studentized residual values with absolute value less than 2.
# 
