#### Rename a column
# colnames(dataset)[# of col ] <- "Name"

# names(data) <- c("new_name", "another_new_name")

# library(data.table)
# setnames(data, old=c("old_name","another_old_name"), new=c("new_name", "another_new_name"))

# library(plyr)
# rename(d, c("beta"="two", "gamma"="three"))


#### Make it a data.frame
# predattrition = data.frame(probaToLeave) # Structure the prediction output in a table
# View(predattrition) # View the predattrition dataframe
## can use this for table() too

#### HR 2 Case
rm(list=ls(all=TRUE)) 

dataold=read.table('DATA_3.02_HR2.csv', header = T,sep=',') # The function read.table enables us to read flat files such as .csv files
datanew=read.table('DATA_4.02_HR3.csv', header = T,sep=',') # The new dataset on which we want to make the prediction

str(datanew) # The str() function shows the structure of your dataset and details the type of variables that it contains
summary(datanew) # The summary() function provides for each variable in your dataset the minimum, mean, maximum and quartiles

logreg = glm(left ~ ., family=binomial(logit), data=dataold) # Estimate the drivers of attrition
probaToLeave=predict(logreg,newdata=datanew,type="response") # Make predictions on the out-of-sample data

predattrition = data.frame(probaToLeave) # Structure the prediction output in a table
View(predattrition) # View the predattrition dataframe

predattrition$performance=datanew$LPE # Add a column to the predattrition dataframe containing the performance
View(predattrition) # View the predattrition dataframe

plot(predattrition$probaToLeave,predattrition$performance, pch = 20, col = "blue")
abline(v = 0.3, col ="red")
abline(h = 0.54, col = "red")
text(0.15, 0.7, "As usual", col ="red") #good performance,low left
text(0.35, 0.45, " Up or out", col = "red") #low performance
text(0.6, 0.8, "Must retain", col = "red" ) #high perf, high left

## Calculate Priority point
predattrition$priority=predattrition$performance*predattrition$probaToLeave
View(predattrition)

orderpredattrition=predattrition[order(predattrition$priority,decreasing = TRUE),]
View(orderpredattrition)


##### Maintenance Example - Survival Analysis 

rm(list=ls(all=TRUE))


data=read.table('DATA_4.03_MNT.csv',sep=',',header=TRUE)

str(data) 
summary(data)

linregmodel = lm(lifetime~.-broken,data=data) #exclude "broken" 
summary(linregmodel)
## linreg fails here since the "lifetime" of the unbroken pieces is not its life time
## must use survreg

install.packages("survival") # Install the survival package to your computer
library(survival) # Load the survival package

## choose the dependent vars, the binary(broken) must be in the 2nd place
dependantvars = Surv(data$lifetime, data$broken)
survreg = survreg(dependantvars~pressureInd+moistureInd+temperatureInd+team+provider, dist="gaussian",data=data) # Create your survival regression model
summary(survreg) 
# should check if this is a good model ( out of sample data)
# diff in lt by provider may be due to diff using condition,etc -> explore more

## estimate the median lifetime as the expected moment of "death"
Ebreak=predict(survreg, newdata=data, type="quantile", p= 0.5)
## type = "quantile" -> return value at the p quantile. if not indicate p, return at 0.1 and 0.9
## tyep = "response" -> return the mean value

Forecast=data.frame(Ebreak) 
Forecast$lifetime=data$lifetime  # Add a column in the Forecast dataframe indicating the lifetime of the piece
Forecast$broken=data$broken # Add a column in the Forecast dataframe indicating whether or not the piece is broken
Forecast$RemainingLT=Forecast$Ebreak-data$lifetime # Computed Expected Remaining Lifetime

View(Forecast) 

Forecast=Forecast[order(Forecast$RemainingLT),] # Order the elements by Expected Remaining Lifetime
ActionsPriority=Forecast[Forecast$broken==0,] # And keep only those who are not broken yet
View(ActionsPriority) # View the output and take actions!

##### Cholocate Sales Prediction 

data=read.table('DATA_4.04_CHOC.csv',sep=',',header=TRUE) # The function read.table enables us to read flat files such as .csv files


str(data) 
summary(data$sales) 

plot(data$time,data$sales,
     main="Chocolate sales over time",
     xlab="Time (in month)",
     ylab="Monthly sales",
     ylim=c(0,max(data$sales*1.2)),
     type='l') #lined

regres=lm(sales~month,data=data) # Build a linear regression model
summary(regres)
# use January as the base

plot(data$month,data$sales,
     main="Chocolate sales by month",
     xlab="Month",ylab="Monthly sales",
     ylim=c(0,max(data$sales*1.2)))

## at the fitted lines
lines(data$time,regres$fitted.values,type='l',col='blue',lty=2)
legend("topleft",c("Actual sales","Sales by the model"),lty=c(1,2),col=c('black','blue'))
## lty= line type