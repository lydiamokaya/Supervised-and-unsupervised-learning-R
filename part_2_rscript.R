# Data Sourcing
```{r}
shoppers <- read.csv("online_shoppers_intention.csv")
head(shoppers)
```
# Check the Data
```{r}
#summary of the dataset
summary(shoppers)
```
```{r}
# information about the dataset
str(shoppers)
```

#Data Cleaning
### Completeness
```{r}
#Checking for Missing Values
colSums(is.na(shoppers))
```
```{R}
# Filling the null columns with their respective mean
shoppers  = shoppers %>%
  mutate(Administrative =replace(Administrative,is.na(Administrative),mean(Administrative,na.rm=TRUE)))%>%
  mutate(Administrative_Duration =replace(Administrative_Duration,is.na(Administrative_Duration),mean(Administrative_Duration,na.rm=TRUE)))%>%
  mutate(Informational = replace(Informational, is.na(Informational), mean(Informational, na.rm = TRUE)))%>%
  mutate(Informational_Duration =replace(Informational_Duration,is.na(Informational_Duration),mean(Informational_Duration,na.rm=TRUE)))%>%
  mutate(ProductRelated =replace(ProductRelated,is.na(ProductRelated),mean(ProductRelated,na.rm=TRUE)))%>%
  mutate(ProductRelated_Duration = replace(ProductRelated_Duration, is.na(ProductRelated_Duration), mean(ProductRelated_Duration, na.rm = TRUE)))%>%
  mutate(BounceRates =replace(BounceRates, is.na(BounceRates),mean(BounceRates,na.rm=TRUE)))%>%
  mutate(ExitRates = replace(ExitRates, is.na(ExitRates), mean(ExitRates, na.rm = TRUE)))
```

```{r}
# Confirming that we have no null values
sum(colSums(is.na(shoppers)))
```
## Consitency

```{r}
# Checking for Duplicates
sum(duplicated(shoppers))
```
```{r}
# Removing duplicates
# pick the non-duplicated rows
data = shoppers[!duplicated(shoppers), ]
dim(data)
```
```{r}
# Confirming if Duplicates are still there
sum(duplicated(data))
```
```{r}
# plotting a boxplot for the product related duration outliers
boxplot(data$Administrative,
        main = "Administrative Boxplot",
        xlab = "Administrative",
        col = "orange",
        border = "brown",
        horizontal = TRUE,
        notch = TRUE
)
```
```{r}
# plotting a boxplot for the Administrative_Duration
boxplot(data$Administrative_Duration,
        main = "Administrative_Duration Boxplot",
        xlab = "Administrative_Duration",
        col = "blue",
        border = "brown",
        horizontal = TRUE,
        notch = TRUE
)
```
```{r}
# plotting a boxplot for Informational
boxplot(data$Informational,
        main = "Informational Boxplot",
        xlab = "Informational",
        col = "green",
        border = "brown",
        horizontal = TRUE,
        notch = TRUE
)
```
```{r}
# plotting a boxplot for the Informational_Duration
boxplot(data$Informational_Duration,
        main = "Informational_Duration boxplot",
        xlab = "Informational_Duration",
        col = "gold",
        border = "brown",
        horizontal = TRUE,
        notch = TRUE
)
```
```{r}
# plotting a boxplot for the product related
boxplot(data$ProductRelated,
        main = "Product Related Boxplot",
        xlab = "Product Related",
        col = "green",
        border = "brown",
        horizontal = TRUE,
        notch = TRUE
)
```



```{r}
# plotting a boxplot for the product related duration 
boxplot(data$ProductRelated_Duration,
        main = "Product Related Duration Boxplot",
        xlab = "Product Related Duration",
        col = "gold",
        border = "brown",
        horizontal = TRUE,
        notch = TRUE
)
```
```{r}
# plotting a boxplot for the BounceRates
boxplot(data$BounceRates,
        main = "BounceRates Boxplot",
        xlab = "BounceRates",
        col = "cyan",
        border = "brown",
        horizontal = TRUE,
        notch = TRUE
)
```
```{r}
# plotting a boxplot for theExitRates
boxplot(data$ExitRates,
        main = "ExitRates Boxplot",
        xlab = "ExitRates",
        col = "pink",
        border = "brown",
        horizontal = TRUE,
        notch = TRUE
)
```
```{r}
# plotting a boxplot for PageValues
boxplot(data$PageValues,
        main = "PageValues Boxplot",
        xlab = "PageValues",
        col = "maroon",
        border = "brown",
        horizontal = TRUE,
        notch = TRUE
)
```
# Univariate Analysis

```{r}
# statistical description of the dataset
summary(data)
```
```{r}
# Histogram 
hist(data$Administrative)
hist(data$Informational)
hist(data$ProductRelated)
hist(data$BounceRates)
hist(data$ExitRates)
hist(data$TrafficType)
```
Bar plots
```{r}
# Bar plots of the categorical/factor modes variables
par(mfrow=c(4,1))
for(i in 11:16) {
  counts <- table(data[,i])
  name <- names(data)[i]
  barplot(counts, main=name, col = heat.colors(20))}
```
# Bivariate analysis

```{r}
# Plotting a scatter plot using the plot() method
plot(ExitRates ~ BounceRates, data = data, 
     col = "black",
     main = "Bounce vs Exit Rates Scatter Plot")
```
From the scatter plot there is a strong positive correlation between Exit rates and Bounce rates.
```{r}
# Scatter plot using ggplots and fitting a line of best fit
ggplot(data, aes(x = Administrative_Duration, y = Informational_Duration)) + 
  geom_point(size = 2, color= "green", shape = 23)+ 
  geom_smooth(method=lm,  linetype="dashed",color="darkred", fill="blue")+
  labs(title = "Info Duration vs Adm Duration Scatter Plot")
```
The is a positive correlation between the two variables but not very strong.

```{r}
# Stacked bar chart: Visitor Type vs Month
data %>%
  ggplot(aes(Month)) +
  geom_bar(aes(fill = VisitorType))+
  labs(title = "Stacked Chart: Visitor Type by Month")
```
observations

May, Nov, March, and December in that order are the busy months.
During these months there is a higher number of new visitors which the company can attract using offers tailored for them to retain them.
Feb and June are the least busy months.
```{r}
# Stacked bar chart: Revenue vs Month
data %>%
  ggplot(aes(Revenue)) +
  geom_bar(aes(fill = Month))+
  labs(title = "Stacked Chart: Revenue by Month")
```
Nov, May and March are best months when the company makes most revenue.




```{r}
# Stacked bar chart: Revenue vs Day Type
data %>%
  ggplot(aes(Revenue)) +
  geom_bar(aes(fill = Weekend))+
  labs(title = "Stacked Chart: Revenue by Day Type")
```
About three quarters of the data shows that a visit to the page did not result to the company making revenue i.e. the customer did not make a purchase.
Of the quarter remaining, the company made revenue mostly during the weekdays.

##correlation plot
###install packages
```{r, echo=TRUE, eval=TRUE}
install.packages("Hmisc")
install.packages("corrplot")
library("corrplot")
#plot the correlation table
cor <- cor(data[,1:10])
corrplot(cor, method="color")
```
administrative,administrative ,informational, informational duration, product related, product duration are all strongly positively correlated.
bounce rates and exit rates  also have a strong positive correlation.
```{r}
# pairplots
pairs(data[,c(1,2,3,4,6,7,8,9,10)])
```
# Multivarite analysis
```{r}
#PCA
data.pca <- prcomp(data[,c(1,2,3,4,5,6,7,8,9,10)], center = TRUE,scale. = TRUE)
summary(data.pca)
```
```{R}
# Visualize PCA
install.packages("DataExplorer")
library(DataExplorer)
# Subset your data to numerical columns only
num <- data[, c(1,2,3,4,5,6,7,8,9,10)]
plot_prcomp(num, variance_cap = 0.9, nrow = 2L, ncol = 2L)
```


# IMPLEMENTING THE SOLUTION
K-MEANS CLUSTERING
###changing our factor variables to numeric
```{r}
b = c("OperatingSystems", "Browser", "Region", "TrafficType")
for (i in b) {
  data[, i] = as.factor(data[, i])
}
```

```{r}
g = c('Month','VisitorType', 'Weekend')
for (i in g){
  data[,i] = as.numeric(data[,i])
}
```

### data preprocessing

```{R}
kdata<- data[, c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)]
data.class<- data[, "Revenue"]
head(kdata)
```
```{R}
# Normalize the data
normalize <- function(x){
  return ((x-min(x)) / (max(x)-min(x)))
}
n = c(1,2,3,4,5,6,7,8,9,10)
for (i in n){
  kdata[,i] = normalize(kdata[,i])
}
head(kdata)
```
```{r}
# Applying the K-means clustering algorithm with no. of centroids(k)=3
# ---
# 
result <- kmeans(kdata,2)
```

```{r}
# Previewing the no. of records in each cluster
# 
result$size
```
```{r}
result$centers
```
```{r}
result$cluster
```
```{r}
k_data <- as.numeric(data.class)
table(result$cluster, k_data)
```
```{r}
mean(k_data == result$cluster)
```
HIERACHICAL CLUSTERING

```{r}
# Calculating euclidean distances of the independent variables.
d <- dist(kdata, method = "euclidean")
```

```{r}
# using the hclust clustering method.
h_model <- hclust(d, method = "ward.D2" )
```

```{r}
# plotting the dendogram
plot(h_model, cex = 0.6, hang = -1)
```
```{r}
# Cut tree into 2 groups.
sub_group = cutree (h_model, k = 2)
table (sub_group)
```
```{r}
table (sub_group, data.class)
```
```{r}
#checking accuracy
mean(sub_group == data.class)
```
# Challenging the solution
DBSCAN CLUSTERING
```{R}
install.packages('dbscan')
```
```{r}
# Loading the required library
library("dbscan")
# Applying our DBSCAN algorithm
# using a minimum of 4 points with in a distance of eps(0.4)
# 
l = c('OperatingSystems','Browser','Region','TrafficType')
for (i in l){
  kdata[,i] = as.numeric(kdata[,i])
}
db_model <- dbscan(kdata,eps=0.4,MinPts = 4)
```
```{r}
print(db_model)
```
```{r}
hullplot(kdata,db_model$cluster)
```