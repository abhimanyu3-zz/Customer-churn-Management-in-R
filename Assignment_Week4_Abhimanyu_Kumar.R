
# title: "Assignment_Week4"
# author: "Abhimanyu Kumar"
# date: "2/4/2018"

## Customer churn 
# Customer churn occurs when a cutomer stop doing business with services or company. In industries like telecommunication or insurance, churn is really useful as customer has option to choose from multiple service providers based on different factor and geographical area.
# I have used a telecom data set which i have downloaded from IBM sample dataset.

# In the below code I have combined all the packages and libraries which I have used in the code below.

install.packages("plyr", repos = "http://cran.us.r-project.org")
library(plyr)
install.packages("corrplot", repos = "http://cran.us.r-project.org")
library(corrplot)
install.packages("ggplot2", repos = "http://cran.us.r-project.org")
library(ggplot2)
install.packages("gridExtra", repos = "http://cran.us.r-project.org")
library(gridExtra)
install.packages("ggthemes", repos = "http://cran.us.r-project.org")
library(ggthemes)
install.packages("caret", repos = "http://cran.us.r-project.org")
library(caret)
install.packages("MASS", repos = "http://cran.us.r-project.org")
library(MASS)
install.packages("randomForest", repos = "http://cran.us.r-project.org")
library(randomForest)
install.packages("party", repos = "http://cran.us.r-project.org")
library(party)
install.packages("e1071", repos = "http://cran.us.r-project.org")
library(e1071)
library(knitr)



## Reading the data set

churn <- read.csv('Customer_churn.csv')
str(churn)

# Here I am hvaing 7043 rows (customers) and 21 columns (features or variables). My main target is the churn column.

# I have used sapply below to check the number of missing values in each columns.
# I found 11 missing values in "TotalCharges" columns and I removed all the rows with missing value. 

sapply(churn, function(x) sum(is.na(x)))

churn <- churn[complete.cases(churn), ]


# After closely looking at the data i noticed that some wrangling is needed here.

 #1.) I substituted "No internet service to "No" for six columns and they are: 
# “OnlineSecurity”, “OnlineBackup”, “DeviceProtection”, “TechSupport”, “streamingTV”, “streamingMovies”.

cols_recode1 <- c(10:15)
for(i in 1:ncol(churn[,cols_recode1])) {
churn[,cols_recode1][,i] <- as.factor(mapvalues
(churn[,cols_recode1][,i], from =c("No internet service"),to=c("No")))


# 2.) “No phone service” to “No” for column “MultipleLines”

churn$MultipleLines <- as.factor(mapvalues(churn$MultipleLines, 
from=c("No phone service"),
to=c("No")))

# 3. Since the minimum tenure is 1 month and maximum tenure is 72 months, we can group them into five tenure groups: “0–12 Month”, “12–24 Month”, “24–48 Months”, “48–60 Month”, “> 60 Month”

min(churn$tenure); max(churn$tenure)

churn$tenure_group <-cut(churn$tenure, 
breaks = c(0, 12, 24, 48, 60, Inf),
labels = c("0–12 Month",
"12–24 Month",
"24–48 Months",
"48–60 Month",
"> 60 Month"))



# 4.)Change the values in column “SeniorCitizen” from 0 or 1 to “No” or “Yes”

churn$SeniorCitizen <- as.factor(mapvalues(churn$SeniorCitizen,
from=c("0","1"),
to=c("No", "Yes")))


# 5. Remove the columns we do not need for the analysis. 

churn$customerID <- NULL


## Exploratory data analysis and feature selection

# Correlation between numeric variables

numeric.var <- sapply(churn, is.numeric)
corr.matrix <- cor(churn[,numeric.var])
corrplot(corr.matrix, main="\n\nCorrelation Plot for Numerical Variables", method="number")


# The Monthly Charges and Total Charges are correlated. So one of them will be removed from the model. We remove Total Charges.

churn$TotalCharges <- NULL


# Bar plots of categorical variables

p1 <- ggplot(churn, aes(x=gender)) + ggtitle("Gender") + xlab("Gender") +
geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p2 <- ggplot(churn, aes(x=SeniorCitizen)) + ggtitle("Senior Citizen") + xlab("Senior Citizen") + 
geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p3 <- ggplot(churn, aes(x=Partner)) + ggtitle("Partner") + xlab("Partner") + 
geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p4 <- ggplot(churn, aes(x=Dependents)) + ggtitle("Dependents") + xlab("Dependents") +
geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
grid.arrange(p1, p2, p3, p4, ncol=2)

p5 <- ggplot(churn, aes(x=PhoneService)) + ggtitle("Phone Service") + xlab("Phone Service") +
geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p6 <- ggplot(churn, aes(x=MultipleLines)) + ggtitle("Multiple Lines") + xlab("Multiple Lines") + 
geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p7 <- ggplot(churn, aes(x=InternetService)) + ggtitle("Internet Service") + xlab("Internet Service") + 
geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p8 <- ggplot(churn, aes(x=OnlineSecurity)) + ggtitle("Online Security") + xlab("Online Security") +
geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
grid.arrange(p5, p6, p7, p8, ncol=2)

p9 <- ggplot(churn, aes(x=OnlineBackup)) + ggtitle("Online Backup") + xlab("Online Backup") +
geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p10 <- ggplot(churn, aes(x=DeviceProtection)) + ggtitle("Device Protection") + xlab("Device Protection") + 
geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p11 <- ggplot(churn, aes(x=TechSupport)) + ggtitle("Tech Support") + xlab("Tech Support") + 
geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p12 <- ggplot(churn, aes(x=StreamingTV)) + ggtitle("Streaming TV") + xlab("Streaming TV") +
geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
grid.arrange(p9, p10, p11, p12, ncol=2)

p13 <- ggplot(churn, aes(x=StreamingMovies)) + ggtitle("Streaming Movies") + xlab("Streaming Movies") +
geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p14 <- ggplot(churn, aes(x=Contract)) + ggtitle("Contract") + xlab("Contract") + 
geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p15 <- ggplot(churn, aes(x=PaperlessBilling)) + ggtitle("Paperless Billing") + xlab("Paperless Billing") + 
geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p16 <- ggplot(churn, aes(x=PaymentMethod)) + ggtitle("Payment Method") + xlab("Payment Method") +
geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p17 <- ggplot(churn, aes(x=tenure_group)) + ggtitle("Tenure Group") + xlab("Tenure Group") +
geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
grid.arrange(p13, p14, p15, p16, p17, ncol=2)


# As I can see all the categorical variables have a broad distribution, So, I will keep all of them for further analysis.

## Logistic Regression

# First, we split the data into training and testing sets

intrain<- createDataPartition(churn$Churn,p=0.7,list=FALSE)
set.seed(2017)
training<- churn[intrain,]
testing<- churn[-intrain,]


# After that I will Confirm, if  the splitting is correct or not??

dim(training); dim(testing)

# Once after confirming I Fitted the Logistic Regression Model

LogModel <- glm(Churn ~ .,family=binomial(link="logit"),data=training)
print(summary(LogModel))


##Feature Analysis
# The top three most-relevant features include Contract, tenure_group and PaperlessBilling.

anova(LogModel, test="Chisq")

# Analyzing the deviance table we can see the drop in deviance when adding each variable one at a time. Adding InternetService, Contract and tenure_group significantly reduces the residual deviance. The other variables such as PaymentMethod and Dependents seem to improve the model less even though they all have low p-values.

# Assessing the predictive ability of the Logistic Regression model

testing$Churn <- as.character(testing$Churn)
testing$Churn[testing$Churn=="No"] <- "0"
testing$Churn[testing$Churn=="Yes"] <- "1"
fitted.results <- predict(LogModel,newdata=testing,type='response')
#fitted.results  0.5,1,0)
fitted.results <- round(fitted.results, 0)
fitted.results <- as.character(fitted.results)
misClasificError <- mean(fitted.results != testing$Churn)
print(paste('Logistic Regression Accuracy',1-misClasificError))

# After that I created a Logistic Regression Confusion Matrix

print("Confusion Matrix for Logistic Regression"); table(testing$Churn, fitted.results > 0.5)


## Odds Ratio
# One of the interesting performance measurements in logistic regression is Odds Ratio.Basically, Odds ratio is what the odds of an event is happening.

exp(cbind(OR=coef(LogModel), confint(LogModel)))

## Decision Tree
## Decision Tree visualization For illustration purpose,I use only three variables for plotting Decision Trees, they are “Contract”, “tenure_group” and “PaperlessBilling”.

tree <- ctree(Churn~Contract+tenure_group+PaperlessBilling, training)
plot(tree, type='simple')


# 1. Out of three variables which I used, Contract is the most important variable to predict customer churn or not churn.
# 2. If a customer in a one-year or two-year contract, no matter he (she) has PapelessBilling or not, he (she) is less likely to churn.
# 3. On the other hand, if a customer is in a month-to-month contract, and in the tenure group of 0–12 month, and using PaperlessBilling, then this customer is more likely to churn.

## Decision Tree Confusion Matrix
# I am using all the variables to product confusion matrix table and make predictions.

pred_tree <- predict(tree, testing)
print("Confusion Matrix for Decision Tree"); table(Predicted = pred_tree, Actual = testing$Churn)`

## Decision Tree Accuracy

p1 <- predict(tree, training)
tab1 <- table(Predicted = p1, Actual = training$Churn)
tab2 <- table(Predicted = pred_tree, Actual = testing$Churn)
print(paste('Decision Tree Accuracy',sum(diag(tab2))/sum(tab2)))

# The accuracy for Decision Tree has hardly improved. So, I am going to try Random Foret to see if I can do it any better using Random forest.

## Random Forest

# Random Forest Initial Model

rfModel <- randomForest(Churn ~., data = training)
print(rfModel)


# After implementing the random forest I observed that the error rate is relatively low when predicting “No”, and the error rate is much higher when predicting “Yes”.

## Random Forest Prediction and Confusion Matrix

testing$Churn[testing$Churn == 0] <- "No"
testing$Churn[testing$Churn == 1] <- "Yes"

pred_rf <- predict(rfModel, testing)
caret::confusionMatrix(pred_rf, testing$Churn)


# After that I have plotted the Random Forest Error Rate below

plot(rfModel)

# I used this plot to determine the number of trees. I noticed that as the number of trees increases, OOB error rate decreases, and then becomes almost constant.I am not able to decrease the OOB error even after 100 to 200 trees.

## Tune Random Forest Model

t <- tuneRF(training[, -18], training[, 18], stepFactor = 0.5, plot = TRUE, ntreeTry = 200, trace = TRUE, improve = 0.05)


# I use the plot to get some idea on the number of mtry to choose. As the OOB error is lowest when mtry is 2 and so i choose the value oif mtry as 2.

# Fit the Random Forest Model After Tuning

rfModel_new <- randomForest(Churn ~., data = training, ntree = 200, mtry = 2, importance = TRUE, proximity = TRUE)
print(rfModel_new)

# Now i noticed that the OOB error rate decreased to 19.7% from 20.65% earlier.

# Random Forest Predictions and Confusion Matrix After Tuning

pred_rf_new <- predict(rfModel_new, testing)
caret::confusionMatrix(pred_rf_new, testing$Churn)


# Here what i have noticed that the accuracy did not increase but the sensitivity improved, compare with the initial Random Forest model.

# Random Forest Feature Importance

varImpPlot(rfModel_new, sort=T, n.var = 10, main = 'Top 10 Feature Importance')


# From the above example, I have noticed that logistic regression and random fores perfoms better than decission tree for churn analysis for this particular data set.

# Throughout the analysis, I noticed and  learned several things:
# 1. Features such as tenure_group, Contract, PaperlessBilling, MonthlyCharges and InternetService appear to play a role in customer churn.
# 2. I didnt noticed any relationship between churn and gender.
# 3. Customers in a month-to-month contract, with PaperlessBilling and are within 12 months tenure, are more likely to churn; On the other hand, customers with one or two year contract, with longer than 12 months tenure, that are not using PaperlessBilling, are less likely to churn.

# References:- 
# https://datascienceplus.com/predict-customer-churn-logistic-regression-decision-tree-and-random-forest/