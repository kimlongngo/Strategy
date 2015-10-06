###### SKU Case ######
rm(list=ls(all=TRUE))
data=read.table('DATA_2.01_SKU.csv', header = T,sep=',')
#sep = original seperator used in the dataset , or .
str(data) # structure of the set, types and examples of the variables
summary(data)
plot(data$CV, data$ADS, main = "SKU Example", ylab="Average Daily Sales", xlab= "Coefficient of Variation")
abline(v=0.2, col="red") #vertical lines at x = 0.2
abline(h=4, col="blue") #horizontal line at y = 4
text(0.15,9.7, "Horses", col = "red")  
text(0.65,9, "Wild Bulls", col = "red") 
text(0.8,2, "Crickets", col = "red") 
#better use Excel of PPT for visual
testdata <- data #make a copy to protect the original
testdata <- scale(testdata) #to standadize (normalize) the variables
d = dist(testdata, method = "euclidean")
hcward = hclust(d, method="ward.D")
data$groups<-cutree(hcward,k=3) #assign the group to a new varible named "group"
library(lattice) # load the lattice package by using the library() function and passing it the name of the package you wish to load
xyplot(ADS~ CV,main = "After Clustering", type="p",pch=16,group=groups,data=data, # define the groups to be differentiated 
       auto.key=list(title="Group", space = "left", cex=1.0, just = 0.95), # to produce the legend we use the auto.key= list() 
       par.settings = list(superpose.line=list(pch = 0:18, cex=1)), # the par.settings argument allows us to pass a list of display settings
       col=c('blue','green', 'yellow')) # finally we choose the colour of our plotted points per group

#########################################################

##### HRCase ########
rm(list=ls(all=TRUE))
data=read.table('DATA_2.02_HR.csv',header = T,sep=',')
str(data)
data$Newborn <- as.factor(data$Newborn)
summary(data)
# S = satisfaction [0;1]
# LPE = Last project evaluation [0;1]
# NP = # of projects
# ANH = avg of work hours LTM
# TIC = time in company
# Newborn = 0 if no child in LTM

##### DO THE CLUSTERING #####
testdata=data[ ,c(1:5)] #take column 1 to 5 in "data",iow, remove the Newborn
testdata = scale(testdata)

d = dist(testdata, method = "euclidean")
hcward = hclust(d, method="ward.D") #ward.D =minimum within-cluster variance

data$groups = cutree(hcward,k=4)

##### PREPARE THE AGGREGATION OUTPUT #####

aggdata = aggregate(.~ groups, data=data, FUN=mean)
# . ~groups: aggregate all other variables by the "groups" 
# data = data" name of the dataset here is "data"
# FUN = the function we want to use to aggregate (the "groups" will be affected too)

proptemp=aggregate(S~ groups, data=data, FUN=length)
# create a new table which contain the # of observations in each group
# we count by "S" here but any other is fine
# length ( ) count the # of observations

aggdata$proportion = proptemp$S/sum(proptemp$S)
aggdata=aggdata[order(aggdata$proportion,decreasing=T), ]
# must have ", " at the end to show the proportion column in aggdata
# without the "' ", "proportion" will disappear after the sorting
# if enter other numbers, only the prorportion vector is shown

View(aggdata)

palette(rainbow(15, s = 0.6, v = 0.75)) # Select the colors to use
stars(aggdata[,2:(ncol(data))], len = 0.6, key.loc = c(11, 6),xlim=c(2,12),main = "Segments", draw.segments = TRUE,nrow = 2, cex = .75,labels=aggdata$groups)
#To export the output of our result, we execute the following line using the write.table() function
write.csv(aggdata, "HR_example_Numerical_Output.csv", row.names=FALSE)

##### Telecom Case #####
rm(list=ls(all=TRUE))
data=read.table('DATA_2.03_Telco.csv', header = T,sep=',')


#Clustering
testdata = data
testdata = scale(testdata)
d = dist(testdata, method = "euclidean")
hcward = hclust(d, method = "ward.D")
data$groups = cutree (hcward, k =5)
#plot(hcward) draw the dendrogram
#rect.hclust(hcward, n) draw retangular boundaries btw the n cluster
#rect.hclust(hcward,h= height) draw boundaries by height

#Aggregating
aggdata = aggregate( . ~ groups, data=data, FUN = mean)
proptemp = aggregate ( Calls ~groups, data=data, FUN = length)
aggdata$prop <- proptemp$Calls/sum(proptemp$Calls)
aggdata = aggdata[order(aggdata$prop,decreasing = T), ]

#Exporting
write.csv(aggdata, file = "TelecomClusteringOutput.csv", row.names =F)

#Visualizing
palette(rainbow(15, s = 0.6, v = 0.75)) # Select the colors to use
stars(aggdata[,2:(ncol(data))], len = 0.6, key.loc = c(11, 6),xlim=c(2,12),main = "Segments", draw.segments = TRUE,nrow = 2, cex = .75,labels=aggdata$groups)

cbind