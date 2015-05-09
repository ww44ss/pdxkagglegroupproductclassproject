## PDX DATA SCIENCE KAGGLE PROJECT
## Program thread 4
## In this thread continue RF adn GBM combos
##
## This is an exploratory look at some of the data in the kaggle product classification competition.
## the program provides for sampling the data (to get is small)
## and then looks at some cursory summary statistics

## GET TRAINING DATA

directory<- "/Users/winstonsaunders/Documents/pdxkagglegroupproductclassproject/"
file_name<- "train.csv"

train_data<-read.csv(paste0(directory,file_name))

print(head(train_data))

## LOAD REQUIRED PACKAGES

require(plyr)
require(ggplot2)
require(tidyr)
require(gbm)
require(randomForest)

## PRINT THE DIMENSIONS OF THE TRAINING DATA
## ensure proper read of data

cat(dim(train_data))

## SAMPLE DATA FOR QUICK LOOKS
##
## if sample_data == TRUE then creates a sample of overall data
sample_data<-TRUE

set.seed(8675309)
if (sample_data == TRUE){
        sample_rows <- sample(1:dim(train_data)[1], 2*dim(train_data)[1]/3)
        train_data<-train_data[sample_rows,]
        train_data2<-train_data[-sample_rows,]
        
}



## TRY GBM10 with an added RF
## USE {gbm} PACKAGE
## increase number of trees to 3000 since last run did not get validation down to lowest
## inch down shrinkage = 0.025
## explore increased depth to 3
## keep bag.fraction 0.031 which will make 3
## 

library(gbm)


#train_control<-tree.control(nobs=dim(train_data)[1], mindev=0.01/2)
#train_control<-tree.control(nobs=dim(train_data)[1], mindev=0.01/2)
## renumber rows
train<-train_data

set.seed(8675309)
gbm_fit<-gbm(target~.-id, data=train, 
             n.trees = 2000,
             distribution = "multinomial",
             shrinkage=0.025,
             interaction.depth=2,       ## interaction depth of 1 (normally between 1 and 3)
             train.fraction=0.5,
             bag.fraction=0.031,        ## out of 93 predictors select 3
             keep.data=TRUE,
             verbose=TRUE,
             cv.folds=3,                ## 3-fold cv 
             n.cores=1)                 ## avoid annoying bugs






## summary of the tree

fitsum <- summary(gbm_fit)
plot(fitsum)

plot(gbm_fit, i.var=11)
## plot and label
#names(gbm_fit)

gbm_plot<-as.data.frame(cbind(gbm_fit$train.error, gbm_fit$valid.error))
colnames(gbm_plot)<-c("train.error", "valid.error")
gbm_plot$iteration<-1:gbm_fit$n.tree

library(ggplot)
p<-ggplot(gbm_plot, aes(x=iteration, y = train.error))+geom_line()
p<-p+geom_line(aes(x=iteration, y =valid.error), color="red")

print(p)
# can see a slight divergence of training adn test
## still not over training

## make sure the test data is there 
# head(test_data)

td<-test_data

td_model<-predict(gbm_fit, newdata=td, type="response")
td_gbm_predict<-td_model[,,1]
td_gbm_predict<-as.data.frame(td_gbm_predict)

td_names<-colnames(td_model)
td_model<-as.data.frame(td_model)
colnames(td_model)<-td_names
td_predict <- as.factor(colnames(td_model)[max.col(td_model)])

table(td_predict, td$target)

#average_predict_single Class_1 Class_2 Class_3 Class_4 Class_5 Class_6 Class_7 Class_8 Class_9
#                       Class_1     280       3       2       1       1      23      23      42      33
#                       Class_2      38    4574    1382     357      24      46     127      38      48
#                       Class_3       2     567    1221     129       1       4      60       8       1
#                       Class_4       0      49      40     353       1       4      13       0       1
#                       Class_5       1      11       2       6     892       0       6       2       3
#                       Class_6      46      18       4      30       2    4434      79      79      66
#                       Class_7      27      40      44      15       0      62     574      22       8
#                       Class_8     119      15      17       2       2      90      57    2533      77
#                       Class_9     147      10       4       2       2      73       7      35    1465
#         
check<-table(td_predict ==test_data$target)
cat(check)

accuracy<-1-check[1]/(check[1]+check[2])

cat(accuracy)   ## 76.37%
## kaggle score 0.62881

## a small improvement 

## Add a Random Forest

library(randomForest)


## increase to 1000 trees and decrease node size to 3
td_rf_model <- randomForest(target~.-id, data=train, importance=TRUE, ntree=1000, nodesize=3)

td_rf_predict<-predict(td_rf_model, newdata=td, type="prob")


## average the two models
average_predict<-(td_rf_predict+td_gbm_predict)/2
## pick the max probability
average_predict_single <- as.factor(colnames(average_predict)[max.col(average_predict)])

table(average_predict_single, td$target)


#         average_predict_single Class_1 Class_2 Class_3 Class_4 Class_5 Class_6 Class_7 Class_8 Class_9
#         Class_1     280       3       2       1       1      23      23      42      33
#         Class_2      38    4574    1382     357      24      46     127      38      48
#         Class_3       2     567    1221     129       1       4      60       8       1
#         Class_4       0      49      40     353       1       4      13       0       1
#         Class_5       1      11       2       6     892       0       6       2       3
#         Class_6      46      18       4      30       2    4434      79      79      66
#         Class_7      27      40      44      15       0      62     574      22       8
#         Class_8     119      15      17       2       2      90      57    2533      77
#         Class_9     147      10       4       2       2      73       7      35    1465

## not a significant change....

check<-table(average_predict_single ==td$target)
cat(check)

accuracy<-1-check[1]/(check[1]+check[2])

cat(accuracy)

## accuracy of 79.15% (Very slight increase)

##MAKE SUBMISSION

## get the actual test data
directory<- "/Users/winstonsaunders/Documents/pdxkagglegroupproductclassproject/"
file_name<- "test.csv"

real_test_data<-read.csv(paste0(directory,file_name))
##just make sure its real
print(head(real_test_data))
## run the prediction
gbm_real_test_model<-predict(gbm_fit, newdata=real_test_data, type="response")
rf_real_test_model<-predict(td_rf_model, newdata=real_test_data, type="prob")
## get the data
a<-gbm_real_test_model[,,1]
a<-as.data.frame(a)
b<-rf_real_test_model
submission<-(a+b)/2
submission<-as.data.frame(submission)
## clean up the numbers
submission<-round(10000*submission,0)/10000.

## add ids back
submission<-cbind("id"=as.integer(real_test_data$id), submission)
submission<-as.data.frame(submission)
submission$id<-as.integer(submission$id)
## get rid of scientiic notation
options(scipen=10)
## check dimensions and data
dim(submission)
head(submission)
tail(submission)

## write csv
write.csv(submission, paste0(directory,"May082",".csv"), row.names=F, quote=F)

## this is the May081 run
## has a kaggle score of 0.56346 - right direction but marginal improvement

## TRY GBM of just 2, 3, and 4

train234<-train[train$target=="Class_2"|train$target=="Class_3"|train$target=="Class_3",]

gbm_fit<-gbm(target~.-id, data=train234, 
             n.trees = 200,
             distribution = "multinomial",
             shrinkage=0.05,
             interaction.depth=2,       ## interaction depth of 1 (normally between 1 and 3)
             train.fraction=0.5,
             bag.fraction=0.031,        ## out of 93 predictors select 3
             keep.data=TRUE,
             verbose=TRUE,
             cv.folds=3,                ## 3-fold cv 
             n.cores=1)                 ## avoid annoying bugs


## 