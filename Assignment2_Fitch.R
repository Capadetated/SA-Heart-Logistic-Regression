# This program contains all the R code used to perform logistic regression analysis on the Wages dataset
# Data 630 Week 5 Module 3 Assignment 2
# Ted Fitch
# Last updated 22JUN21

# Section 1 - Upload and Explore File /////
# Set working directory
setwd("C:/Users/soari/Documents/Assignments/Data Analytics/UMGC/Summer 2021 Data 630/Assignment 2")

# Pull the data 
w=read.table("SAheart.csv",sep=",",header=TRUE, as.is = FALSE)


# Section 2 - EDA and Plotting /////
# Explore the data
str(w)
summary(w)
View(w)

# Turn numeric variable into factor
# Show type of variables all at once:
str(w)
# Transform variables
w$chd<-factor(w$chd)
# Show type of variables after conversion
summary(w$chd)
str(w)


# barplot of sex distribution
x<- w$chd
cnt <- table(x)
cnt
barplot (cnt,main="Distribution of CHD",
         xlab="CHD (1 = CHD)",
         ylab="Count",
         border="brown",
         col="brown", space =1.0,beside=TRUE,ylim=range(pretty(c(0, cnt))))

# barplot of marriage distribution
x<- w$famhist
cnt <- table(x)
cnt
barplot (cnt,main="Distribution of CHD in Family History",
         xlab="CHD in Family History",
         ylab="Count",
         border="brown",
         col="brown", space =1.0,beside=TRUE,ylim=range(pretty(c(0, cnt))))

# Histogram of alcohol
hist(w$alcohol,main="Distribution of Alcohol Consumption",
     xlab="Alcohol Consumption",
     ylab="Frequency",col="brown")

# Histogram of tobacco
hist(w$tobacco,main="Distribution of Tobacco Consumption",
     xlab="Tobacco Consumption (KG)",
     ylab="Frequency",col="brown")


# Section 3 - Preprocessing /////
# Check for null values:
Nw<-subset(w, complete.cases(w))
# Use square brackets
Nw<-w[complete.cases(w),]
# Use na.omit
Nw<-na.omit(w)
# Display the Rows with Missing Values
w[!complete.cases(w),]
nrow(w[!complete.cases(w),])
# Number of Missing Values in a Data Row
apply(w, 1, function (w) sum(is.na(w)))
# Number of Missing Values for Each Variable
apply(w, 2, function (w) sum(is.na(w)))


# Remove outliers
w<-w[w$tobacco < 25,]
w<-w[w$alcohol < 130,]
# Shows breakdown / that outlier removal worked
summary(w)


# Section 4 - Preparation for Logistic Regression /////
#make sure that the result is reproducible
set.seed(1234)
#split the data into a training and test set
ind <- sample(2, nrow(w), replace = TRUE, prob = c(0.7, 0.3))
train.data <- w [ind == 1, ]
test.data <- w [ind == 2, ]
# ensure splits worked
str(train.data)
str(test.data)

# Section 5 - Build the logistic regression model /////
#build the model and store in a variable model. Binomial = logistic regression. chd is the variable to predict. . means all other variables are used to predict CHD.
model1<-glm(chd~., family=binomial, data=train.data)
summary(model1)
# model 2: only significant independent variables added
model<-glm(chd~tobacco + ldl + famhist + typea + age, family=binomial, data=train.data)
#output the coefficients and Residual Deviance
print(model)
#output the coefficient, p value, and standard error for each independent variable and intercept
summary(model)
#output the coefficients and an intercept
exp(coef(model))

# Section 6 - Build Confusion Matrix /////
#first 10 estimated values
model$fitted.values[1:10]
#confusion matrix for the training set; need to round the estimated values
table(round(model$fitted.values), train.data$chd)
table(round(predict(model, train.data, type="response")), train.data$chd)

# Section 7 - Build Predictions Table /////
#display the first 10 estimated values for the test data
predict (model, test.data, type="response")[1:10]
#store the estimated values in a variable mypredictions; need to round the values
mypredictions<-round(predict (model, test.data, type="response"))
#confusion matrix for the test data
table (mypredictions, test.data$chd)


# Section 8 - Plot the residuals /////
plot(predict(model),residuals(model), col=c("blue"))
lines(lowess(predict(model),residuals(model)), col=c("black"), lwd=2)
abline(h=0, col="grey")

# Section 9 - MAM Comparison /////
#show minimal adequate model parameters
summary(step(model1))
# build MAM (same as model 2 but with obesity too)
mamodel<-glm(chd~tobacco + ldl + famhist + typea + obesity + age, family=binomial, data=train.data)
#show MAM
summary(mamodel)
#confusion matrix for the training set
table(round(predict(mamodel, train.data, type="response")), train.data$chd)
#confusion matrix for the test data
mypredictions<-round(predict (mamodel, test.data, type="response"))
table (mypredictions, test.data$chd)

# Section 10 - Original, Unedited Model Comparison /////
# build model (has all variables, is unedited)
ogmodel<-glm(chd~., family=binomial, data=train.data)
#confusion matrix for the training set
table(round(predict(ogmodel, train.data, type="response")), train.data$chd)
#confusion matrix for the test data
mypredictions<-round(predict (ogmodel, test.data, type="response"))
table (mypredictions, test.data$chd)


# End of script