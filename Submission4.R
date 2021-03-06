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


require(ggplot2)
require(gbm)
require(randomForest)

## PRINT THE DIMENSIONS OF THE TRAINING DATA
## ensure proper read of data

cat("the dimensions of train_data are: ",dim(train_data)[1], " rows X ", dim(train_data)[2], "columns")

## SAMPLE DATA FOR QUICK LOOKS
##
## if sample_data == TRUE then creates a sample of overall data

sample_data<-TRUE

set.seed(8675309)
if (sample_data == TRUE){
        sample_rows <- sample(1:nrow(train_data), size=0.6*nrow(train_data))
        train_data<-train_data[sample_rows,]
        train_data2<-train_data[-sample_rows,]
        
        ## create a second smaller sample for intermediate evaluation
        sample_rows2<- sample(1:nrow(train_data2), size=0.75*nrow(train_data2))
        train_data2<-train_data2[sample_rows2,]
        eval_data<-train_data2[-sample_rows2,]
        
}

cat("the dimensions of train_data are: ",dim(train_data)[1], " rows X ", dim(train_data)[2], "columns")
cat("the dimensions of train_data2 are: ",dim(train_data2)[1], " rows X ", dim(train_data2)[2], "columns")
cat("the dimensions of eval_data are: ",dim(eval_data)[1], " rows X ", dim(eval_data)[2], "columns")

## TRY GBM10 with an added RF
## USE {gbm} PACKAGE
## increase number of trees to 3000 since last run did not get validation down to lowest
## inch down shrinkage = 0.025
## explore increased depth to 3
## keep bag.fraction 0.031 which will make 3
## 

library(gbm)


### GBM part

#train_control<-tree.control(nobs=dim(train_data)[1], mindev=0.01/2)
#train_control<-tree.control(nobs=dim(train_data)[1], mindev=0.01/2)
## renumber rows
train<-train_data

set.seed(8675309)
gbm_fit<-gbm(target~.-id, data=train, 
             n.trees = 100,
             distribution = "multinomial",
             shrinkage=0.06,
             interaction.depth=4,       ## interaction depth of 1 (normally between 1 and 3)
             train.fraction=0.5,
             bag.fraction=0.031,        ## out of 93 predictors select 3 to get weaker predictors
             keep.data=TRUE,
             verbose=TRUE,
             cv.folds=2,                ## 2-fold cv 
             n.cores=1)                 ## avoid annoying bugs


## summary of the tree

fitsum <- summary(gbm_fit)
plot(fitsum)

## plot errors to check convergence and overfitting

        gbm_plot<-as.data.frame(cbind(gbm_fit$train.error, gbm_fit$valid.error))
        colnames(gbm_plot)<-c("train.error", "valid.error")
        gbm_plot$iteration<-1:gbm_fit$n.tree

        p<-ggplot(gbm_plot, aes(x=iteration, y = train.error))+geom_line()
        p<-p+geom_line(aes(x=iteration, y =valid.error), color="red")

        print(p)

## evaluate performance of GBM
        ## assign eval_data
        td<-eval_data

        td_model<-predict(gbm_fit, newdata=td, type="response")

        td_gbm_predict<-td_model[,,1]
        td_gbm_predict<-as.data.frame(td_gbm_predict)

        td_names<-colnames(td_model)
        td_model<-as.data.frame(td_model)
        colnames(td_model)<-td_names
        td_predict <- as.factor(colnames(td_model)[max.col(td_model)])

        table(td_predict, td$target)

# td_predict    Class_1 Class_2 Class_3 Class_4 Class_5 Class_6 Class_7 Class_8 Class_9
#       Class_1      31       1       0       0       0       4       4       7      11
#       Class_2      11     605     199      63       9       8      23       9      15
#       Class_3       0      96     145      27       0       3      16       1       2
#       Class_4       0      15       6      26       0       2       0       0       0
#       Class_5       0       4       1       2      86       2       1       2       4
#       Class_6       8       1       1       0       0     589      10      13      10
#       Class_7       0       8      10       2       0       7      74       3       1
#       Class_8      15       4       0       0       0      11       6     321       7
#       Class_9      16       1       2       0       0      10       3       5     186       
        check<-table(td_predict ==eval_data$target)
        cat(check)

        accuracy<-1-check[1]/(check[1]+check[2])

        cat("The accuracy of the GBM Model is roughly ", round(100*accuracy,1), "%")   ## 74.6%
        

## DO INTERMEDIATE EVALUATION
        td<-train_data2

        ## predict and condition the data
        td_model<-predict(gbm_fit, newdata=td, type="response")

        td_gbm_predict<-td_model[,,1]
        td_gbm_predict<-as.data.frame(td_gbm_predict)


## RANDOM FOREST

        ## prep and condition the data

        ## get rid of target data and id data
        td<-td[, -which(names(td)=="id")]
        td<-td[, -which(names(td)=="target")]

        td_gbm_predict2 <- as.factor(colnames(td_gbm_predict)[max.col(td_gbm_predict)])
        
        ## increase to 1000 trees and decrease node size to 3
        rf_model <- randomForest(x=td, y = td_gbm_predict2, importance=TRUE, ntree=100)


## FINAL PREDICTION

## test on the eval data set

        final_predict<-predict(rf_model, newdata=eval_data, type="prob")
        eval_predict <- as.factor(colnames(final_predict)[max.col(final_predict)])

        table(eval_predict, eval_data$target)


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