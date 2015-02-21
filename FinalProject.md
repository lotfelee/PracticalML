Title :  Practical Machine Learning Course Project
=========================================================

### Background

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here: http://groupware.les.inf.puc-rio.br/har (see the section on the Weight Lifting Exercise Dataset). 


### Getting , cleaning and Exploring the Data:

For cleaning this data and determining the useful parts of it i follow the below two steps:
* When i explorer this data i found a columns with too many NA , "#DIV/0!" and empty values ,so my strategy below was to 
delete any column where the missing values percentage is more than 90% , i ended up with 53 columns .
* The first 7 columns are more like identifier for the process rather than useful features(user name ,time stamp ,monitoring window identifier) so i git rid of them 


```r
library(ggplot2)
library(reshape2)
setwd("C:/Users/L/Desktop/Coursera/Practical Machine Learning/course project 1")
set.seed(333)
Data<-read.csv("pml-training.csv",header=TRUE)
training2<-data.frame(row.names = row.names(Data))
x<-vector();
j<-1
for(i in 1:ncol(Data)){
        
        x[i]<-(sum( is.na(Data[i]))+length(which(Data[i]=="#DIV/0!"))+length(which(Data[i]=="")))/nrow(Data)
        
        if (x[i]<.9){
                training2<-cbind(training2,Data[,i])
                colnames(training2)[j]<-colnames(Data)[i]
                j<-j+1
                }
        
        } 
Data_cleaned<-training2[,8:ncol(training2)]
Data_cleaned$classe<-as.factor(Data_cleaned$classe)
plot<-qplot(x=Var1, y=Var2, data=melt(cor(Data_cleaned[1:1000,1:52], use="p")), fill=value, geom="tile")
plot<-plot+scale_fill_gradient2(limits=c(-1, 1))+ggtitle("Final Cleaned Data Variables correlation ")
plot+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
```

<img src="figure/unnamed-chunk-1-1.png" title="plot of chunk unnamed-chunk-1" alt="plot of chunk unnamed-chunk-1" style="display: block; margin: auto;" />
### Slicing the data and extracting useful Features :

Here i sliced my data to training set (70%) and testing set (30%) and from the correlation figure we can see that this data is highly correlated so i used PCA on the training set keeping 90% variations on the data and ended up with 18 principles components ,so finally i projected both training set and testing set on these 18 principle components ,saving them on training_Pca and testing_Pca data frames .


```r
library(caret)
inTrain<-createDataPartition(Data_cleaned$classe,p=.7,list=FALSE)
training<-Data_cleaned[inTrain,]
testing<-Data_cleaned[-inTrain,]
obj<-preProcess(training[,-53],method = "pca",thresh = .9)
training_Pca<-predict(obj,training[,-53])
testing_Pca<-predict(obj,testing[,-53])
```

#### PCA object summary is :


```
## 
## Call:
## preProcess.default(x = training[, -53], method = "pca", thresh = 0.9)
## 
## Created from 13737 samples and 52 variables
## Pre-processing: principal component signal extraction, scaled, centered 
## 
## PCA needed 18 components to capture 90 percent of the variance
```

### Training and choosing the right model:

I trained three models as below:

1- Decision tree with rpart package .

2- Boosting with gbm package .

3- Random Forest with rf package .

Then i compared overall statistics after applying the three models in my test sets (30% of the data) then getting the relative statistics from their respective confusion matrices .


```r
model_rpart<-train(training$classe~.,data = training_Pca,method="rpart")
model_gbm<-train(training$classe~.,data = training_Pca,method="gbm")
model_rf<-train(training$classe~.,data = training_Pca,method="rf")
confusion_rpart<-confusionMatrix(testing$classe,predict(model_rpart,testing_Pca))
confusion_gbm<-confusionMatrix(testing$classe,predict(model_gbm,testing_Pca))
confusion_rf<-confusionMatrix(testing$classe,predict(model_rf,testing_Pca))
```

#### Models overall statitics comparison and out of sample error :


```
##       Accuracy Kappa AccuracyLower AccuracyUpper AccuracyNull AccuracyPValue McnemarPValue
## rpart     0.38  0.17          0.37          0.39         0.74              1           NaN
## gbm       0.81  0.76          0.80          0.82         0.30              0             0
## rf        0.97  0.97          0.97          0.98         0.29              0           NaN
```

*Random forest won ,as in the comparison table with 97% Accuracy(3% out of sample error) against 38%(62% out of sample error!!)  for rpart and 81 % for gbm (19 % out of sample error).*

#### Random forest model Confusion Matrix:


```
##           Reference
## Prediction    A    B    C    D    E
##          A 1657    8    5    4    0
##          B   17 1105   15    0    2
##          C    2   15  995   13    1
##          D    2    3   41  917    1
##          E    0    4    7    8 1063
```

#### Random forest model Statistics per class and error measures using the test set:


```
##          Sensitivity Specificity Pos Pred Value Neg Pred Value Prevalence Detection Rate Detection Prevalence Balanced Accuracy
## Class: A        0.99        1.00           0.99           1.00       0.29           0.28                 0.28              0.99
## Class: B        0.97        0.99           0.97           0.99       0.19           0.19                 0.19              0.98
## Class: C        0.94        0.99           0.97           0.99       0.18           0.17                 0.17              0.96
## Class: D        0.97        0.99           0.95           0.99       0.16           0.16                 0.16              0.98
## Class: E        1.00        1.00           0.98           1.00       0.18           0.18                 0.18              1.00
```
