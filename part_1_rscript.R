```{r}
advert <- read.csv("~/R/Introduction to R basics-EDA/advertising.csv")
head(advert)
```
## Explore The Dataset

```{r}
#summary of the dataset
summary(advert)
```
```{r}
# information about the dataset
str(advert)
```
## Observations
The dataset has 10 columns and 1000 rows
The output above showing the datatypes of the different columns. 
THe dtype for the City variable will be changed to string
he dtype for the timestamp will be changed to datetime

## Data Cleaning
### Completeness
Checking for Missing Values
```{r}
colSums(is.na(advert))
```
This dataset has no missing values. It is complete!.

### Consitency
Checking for Duplicates
```{r}
sum(duplicated(advert))
```
This dataset has no duplicates!!
  
  Checking for Unique Values
```{r, echo=TRUE, eval=TRUE}
unique(advert)
```
Checking for Outliers
# Display Column names
```{r} 
colnames(advert, do.NULL = TRUE, prefix = "col")
```

# Ploting box plots to show outliers
```{r, echo=TRUE, eval=TRUE}
num_cols <- advert[, c("Daily.Time.Spent.on.Site", "Age", "Area.Income", "Daily.Internet.Usage")]
boxplot(num_cols)
```

There are some outliers in the Area.Income column

Checking the outliers
```{r}
boxplot(advert$Area.Income,
        main = "Outliers in Area.Income",
        xlab = "Area.Income",
        col = "maroon",
        border = "orange",
        horizontal = TRUE,
        notch = TRUE
)
```
```{r}
#droping Ad.Topic.Line column
subset(advert, select = -c(Ad.Topic.Line))
```

EXPLORATORY DATA ANALYSIS
### Univariate Analysis
#### Measures of Central Tendencies
Mean

```{r}
# mean of the daily time spent on site
mean(advert$Daily.Time.Spent.on.Site)
```

```{r}
# Mean daily internet usage
mean(advert$Daily.Internet.Usage)
```
Median

```{r}
# median of the daily time spent on site
median(advert$Daily.Time.Spent.on.Site)
```
```{r}
# Median daily internet usage
median(advert$Daily.Internet.Usage)
```

Mode
```{r}
#creating the mode function
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
#mode of daily internet usage  
getmode(advert$Daily.Internet.Usage)
```
```{r}
#creating the mode function
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
#mode of daily time spent on site   
getmode(advert$Daily.Time.Spent.on.Site)
```
### Measures of Dispersion
#### Minimum and Maximum
```{r}
#minimum and maximum of age
min(advert$Age)
max(advert$Age)
```
```{r}
#minimum and maximum of area.income
min(advert$Area.Income)
max(advert$Area.Income)
```
#### Range
```{r}
#range in Daily.Internet.Usage
range(advert$Daily.Internet.Usage)
```
```{r}
#range in daily time spent on site
range(advert$Daily.Time.Spent.on.Site)
```

#### Quantiles
```{r}
#Quantiles in Age
quantile(advert$Age)
```
```{r}
#Quantiles in Area income
quantile(advert$Area.Income)
```
#### Variance
```{r}
# variance of internet usage
var(advert$Daily.Internet.Usage)
```
```{r}
# variance of Daily.Time.Spent.on.Site
var(advert$Daily.Time.Spent.on.Site)
```
#### Standard deviation
```{r}
#Sd in Age
sd(advert$Age)
```
```{r}
#sd in Area income
sd(advert$Area.Income)
```
Visualisations.
### Histograms

```{r}
# Histogram of daily time spent on site
hist(advert$Daily.Time.Spent.on.Site)
```
### Bar graph
```{r}
#Bar graph of daily internet usage
barplot(advert$Daily.Internet.Usage)
```
### Pie Chart

```{r}
#pie chart of gender
pie(table(advert$Male))
```
### Bivariate Analysis
### covariance and correlation

```{r}
# covariance between Daily.Time.Spent.on.Site AND clicked on ad
Time <- advert$Daily.Time.Spent.on.Site
ad <- advert$Clicked.on.Ad 
#getting the covariance between the two variables
cov(Time, ad )
#getting the ccorrelation coefficient 
cor(Time, ad)
```
#### Scatter plots
```{r}
plot(Time, ad, xlab="Time spent on the internet", ylab="clicked on ad")
```
##### Scatterplot
```{r}
# Scatterplot
timespent <- advert$Daily.Time.Spent.on.Site
internetusage<- advert$Daily.Internet.Usage
plot(timespent, internetusage, xlab="Daily time spent on site", ylab="Daily Internet Usage")
```
```{r}
plot(Area.Income ~ Daily.Internet.Usage, data = advert)
```
### Grouped Bar Plot
```{r, echo=TRUE, eval=TRUE}
counts = table(advert$Male, advert$Clicked.on.Ad)
barplot(counts, main="number of Clicks on an Ad as per each sex, 0=Female, 1=male",
        xlab="Ad Clicks", col=c("green","red"),
        legend = rownames(counts), beside=TRUE)
```
```{r, echo=TRUE, eval=TRUE}
counts = table(advert$Age, advert$Clicked.on.Ad)
barplot(counts, main="number of Clicks on an Ad as per Age",
        xlab="Ad Clicks", col=c("blue","yellow"),
        legend = rownames(counts), beside=TRUE)
```

###CORRELATION MATRIX

```{r}
col <- advert[, c("Daily.Time.Spent.on.Site", "Age", "Area.Income", "Daily.Internet.Usage","Clicked.on.Ad")]
cor<- cor(col, method = c("spearman"))
cor
```
```{r, echo=TRUE, eval=TRUE}
install.packages("Hmisc")
```

```{r, echo=TRUE, eval=TRUE}
install.packages("corrplot")
```

```{r, echo=TRUE, eval=TRUE}
library("corrplot")
corrplot(cor, method="color")
```
# IMPLEMENTING THE SOLUTION
## Multiple Linear Regression

```{r}
# subseting out data to include data that we will use for our modelling
ads = advert[, c(1,2,3,4,7,10)]
head(ads)
```
```{r}
# Applying the lm() function.
multiple_lm <- lm(Clicked.on.Ad ~ ., ads)
```

```{r}
# Generating the anova table
anova(multiple_lm)
```
```{r}
# Then performing our prediction 
pred_lm <- predict(multiple_lm, ads)
# Printing out our result
pred_lm
```
```{r}
# Printing the Accuracy

mean(advert$Clicked.on.Ad == pred_lm)
```

#### cross validation
```{r}
# installing packages
install.packages("caret") 
library(caret)
```
```{r}
#cross validation
set.seed(42)
multiple_lm <- train(Clicked.on.Ad ~ ., ads,
                     method = "lm", 
                     trControl = trainControl(method = "cv", 
                                              number = 10, 
                                              verboseIter = FALSE))
summary(multiple_lm)
multiple_lm
```
```{r}
# Printing the Accuracy

mean(ads$Clicked.on.Ad == pred_lm)
```

## DECISION TREES
```{r}
# We will use  Decision Trees to model.
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
```

```{r}
m <- rpart(Clicked.on.Ad ~ ., data =  ads,
           method = "class")
# Plotting our model
rpart.plot(m)
```
```{r}
# predicting
pred <- predict(m, ads, type = "class")
table(pred, ads$Clicked.on.Ad)
```
```{r}
# Printing the Accuracy

mean(advert$Clicked.on.Ad == pred)
```
Decision Trees has an accuracy of 95.7 %

### SVM
```{r}
#splitting the dataset into training and testing
intrain <- createDataPartition(y = ads$Clicked.on.Ad, p= 0.7, list = FALSE)
training <- ads[intrain,]
testing <- ads[-intrain,]
```

```{r}
# We check the dimensions of out training dataframe and testing dataframe
# ---
# 
dim(training); 
dim(testing);
```
```{r}
# The following code will convert the training data frame’s “clicked on ad” column to a factor variable.
# ---
# 
training[["Clicked.on.Ad"]] = factor(training[["Clicked.on.Ad"]])
```

```{r}
install.packages('e1071', dependencies=TRUE)
svm_Linear <- train(Clicked.on.Ad ~., data = training, method = "svmLinear",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)
```

```{r}
# We can then check the reult of our train() model as shown below
# ---
# 
svm_Linear
```

```{r}
# We can use the predict() method for predicting results as shown below. 
# We pass 2 arguements, our trained model and our testing data frame.
# ---
# 
test_pred <- predict(svm_Linear, newdata = testing)
test_pred
```

```{r}
# Now checking for our accuracy of our model by using a confusion matrix 
# ---
# 
confusionMatrix(table(test_pred, testing$Clicked.on.Ad))
```
