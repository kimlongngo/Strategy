##### CREDIT SCORE CASE #####

##### Prepare the working environment #####
rm(list=ls(all=TRUE))
data <- read.table('DATA_3.01_CREDIT.csv',sep=',',header=T)

##### Initial Exploration #####
str(data)
summary(data)
pairs(data)
hist(data$Rating,300)
cor(data[, c(1:5,10)]) #calculate Correllation for all rows and column 1-5 and 10
#only calculate correlation for numerical variables
#cor func do not tell us which is the most important factor, or if they affect 
#possitively or negatively to the outcomes, significance levels, etc.

##### Modeling #####
model <- lm(Rating ~ ., data=data)
summary(model)
#print the fitted.values: this var is auto-generated
model$fitted.values

##### Test if this model is good enough #####
#calculate cor btw the fitted.values and the real values 
cor(model$fitted.values, data$ Rating) # R=0.9867324
plot(model$fitted.values, data$ Rating) 
abline (lm(model$fitted.values ~ data$ Rating))
summary(lm(model$fitted.values ~ data$ Rating)) #R^2 = 0.9736 => can explain 97% of the differences btw 2 observations
# high R and R^2, this is a good model
#however, some of the factors are not stat signi, we can use the following procedure to obtain the optimum model (Lowest AIC)

##### R procedure to obtain the best model #####
# Procedure #1
model <- lm(Rating ~., data=data)
step.model<- step (model, direction = "both") # this is the func for optimization
# BMA Procedure (based on AIC too)
library(BMA)
input <- data[, c(-2)] #assign to input all column but "Rating"
outcome <- data[,c(2)] #assign to outcome column "Rating"
bma.model <- bicreg(input, outcome, strict = F, OR = 20)
summary(bma.model)
imageplot.bma(bma.model) # plot of models selected by BMA
#strict = F : return many models with the highest R^2
#Strict = T : return the best model
#In this case: the optimum model from Pro #1 is the 2nd best from BMA

##### Visualization #####
# divide the plot area into 1 row 2 columns
par(mfrow=c(1,2))
#draw the plots
plot(data$Balance,data$Rating)
plot(data$Income,data$Rating)

##### Transpose model$fitted.values into Viewable data.frame #####
# t() is the func to transpose
# model$fitted.values is not a matrix
x<- t(model$fitted.values) # x is a 300x1 matrix
y<- t(x) # y is 300x1
colnames(y) <- c("fitted.values") #name the column
View(y) #have to apply t() twice to get the expected result
# this proceduce can be apply to print table()

##### Some other functions for the linear model #####
fitted(model) #calculate the fitted values
resid(model) #calculate e = yi - y^i (residual = real values - fitted values)
#calculate outcomes for a set of input : predict.lm()

##### Calculate Confidence Interval and Prediction Interval for a new factor #####

#create a data.frame of input for the prediction functions
new <- data[, c(-2)] #take all rows and all column but col. #2 (Rating)
pred.w.plim <- predict.lm(model, new,se.fit = F, interval = "prediction", level = 0.99) 
pred.w.clim <- predict.lm(model, new, se.fit = F, interval = "confidence", level = 0.99)
#predict.lm()
#interval = type of calculation
#se.fit = indicate if calculate SE or not
#level = level of confidence

################################################################333

##### HR Analytics Case #####

datatot=read.table('DATA_3.02_HR2.csv', header = T,sep=',')

table(datatot$left)/nrow(datatot) # look at percentages for the left variable
#nrow() = # of rows
#ncol() = # of cols
cor(datatot) #eventhough some of the variables are binary, we are still using it as numerical vars
#cor only consider each pair of factor interacting seperately

HR.model = glm(left ~ ., family=binomial(logit), data=datatot)
summary(HR.model)

cutoff = 0.5 #Pi > cutoff means i left
table(HR.model$fitted.values > cutoff ,datatot$left)
## compute the Sensitivity
sum((HR.model$fitted.values < cutoff) & (datatot$left == 0)) /sum(datatot$left == 0)
#sum() to count # of those satisfy the 2 conditions, return F = 0, T = 1
## Compute Specificity
sum((HR.model$fitted.values>cutoff) & (datatot$left == 1)) / sum(datatot$left == 1)
## total of correctly classified
mean((HR.model$fitted.values>cutoff)==(datatot$left==1))
#if the predicted values (left or not) is equal the real values (left or not) (T==T or F == F), will return T = 1, 
#otherwise F = 0. Compute the mean of these 
#Brilliant
# adjust the cutoff to get the preferred Sens and Spec

summary(HR.model)

##### Visualization

### Left ~ TIC

## Plot Left ~ TIC
plot(datatot$TIC,datatot$left,main= "Time and Employee Attrition", ylab="Attrition", xlab= "Time spent")
# this sucks

## Compute the % of leavers by TIC
tempdata <- datatot
rate.left.tic <- aggregate(left ~ TIC, data=tempdata,FUN = mean)
plot(proptemp)

## Compute the # of leavers by TIC
count.left.tic <- aggregate(left ~ TIC, data=tempdata,FUN = length)
symbols(rate.left.tic$TIC, #horizontal
        rate.left.tic$left, #vertical
        circles=count.left.tic$left, #size of the bubble by this variables
        inches=.5, #size of the bubble
        fg="white", bg="red", #foreground, #background
        main= "Time and Employee Attrition", 
        ylab="Average Attrition Rate", 
        xlab= "Time spent"
        )
### Left ~ Satisfaction

tempdata=datatot
## Creating 20 groups pf 
tempdata$rankSatis = round(rank(-tempdata$S)/600) 
# rank () to amplify the values of S from [0;1] to [1;12000] 
# so we can use the round() to create groups where the ith  group contains value from (i-0.5;i+0.5]
# Another grouping method is cut(vector, k). this way we can cut a vector into k subsets of equal length
## We compute the average attrition rate for each category
aggbSatisRank = aggregate(left~ rankSatis, data=tempdata, FUN=mean) 
## We compute the number of employees for each value of TIC
cntbSatisRank = aggregate(left~ rankSatis, data=tempdata, FUN=length) 
symbols(aggbSatisRank$rankSatis,
        aggbSatisRank$left,
        circles=cntbSatisRank$left, 
        inches=.2, 
        fg="white", bg="red",
        main= "Satisfaction and Employee Attrition", 
        ylab="Average Attrition Rate", xlab= "Rank of Satisfaction"
        )


