## PDX DATA SCIENCE KAGGLE PROJECT
## Program thread 5
## In this thread continues RF and GBM combos
##

## this is a quick check of reversing the order of rf and gbm

## did not produce satisfactory results. 


## PARAMETER SPACE
## data split parameters
RF_sample<-0.6
## GBM_sample must be < 1.0 for there to be an eval data set. 
## currently the program does not control for a zero eval set 
GBM_sample<-0.95

## Random Forest Parameters
P_ntree<-250
P_nodesize<-1

## GBM Parameters
P_n.trees <- 400
P_shrinkage<-0.008
P_interaction.depth<-4
#P_n.minobsinnode <-10
P_train.fraction<-0.5
P_cv.folds<-2


## LOAD REQUIRED PACKAGES

require(ggplot2)
require(gbm)
require(randomForest)


## GET TRAINING DATA
## variable train_data

directory<- "/Users/winstonsaunders/Documents/pdxkagglegroupproductclassproject/"
file_name<- "train.csv"

train_data<-read.csv(paste0(directory,file_name))

## ensure proper read of data
cat("the dimensions of loaded train_data are: ",dim(train_data)[1], " rows X ", dim(train_data)[2], "columns","\n")

## SAMPLE DATA FOR QUICK LOOKS


sample_data<-TRUE

set.seed(8675309)
if (sample_data == TRUE){
        sample_rows <- sample(1:nrow(train_data), size=RF_sample*nrow(train_data))
        td<-train_data[sample_rows,]
        train_data2<-train_data[-sample_rows,]
        
        ## create a second smaller sample for intermediate evaluation
        sample_rows2<- sample(1:nrow(train_data2), size=GBM_sample*nrow(train_data2))
        train_data2<-train_data2[sample_rows2,]
        eval_data<-train_data2[-sample_rows2,]
        
        train_data<-td
        
}

cat("the dimensions of RF train_data are: ",dim(train_data)[1], " rows X ", dim(train_data)[2], "columns","\n")
cat("the dimensions of GBM train_data2 are: ",dim(train_data2)[1], " rows X ", dim(train_data2)[2], "columns","\n")
cat("the dimensions of eval_data are: ",dim(eval_data)[1], " rows X ", dim(eval_data)[2], "columns","\n")

## RANDOM FOREST


## assign td
td<-train_data

## Random forest Model
set.seed(8765309)
print("computing RF")
rf_model <-gbm(target~.-id,data=td, 
               n.trees = P_n.trees,
               distribution = "multinomial",
               shrinkage=P_shrinkage,
               interaction.depth=P_interaction.depth,
               #n.minobsinnode<-P_n.minobsinnode,
               train.fraction=P_train.fraction,
               keep.data=TRUE,
               verbose=TRUE,
               cv.folds=P_cv.folds,                ## 3-fold cv 
               n.cores=1)
        
# Iter   TrainDeviance   ValidDeviance   StepSize   Improve
# 1        2.1972          2.1972     0.0080    0.0594
# 2        2.1661          2.1661     0.0080    0.0569
# 3        2.1361          2.1360     0.0080    0.0535
# 4        2.1080          2.1079     0.0080    0.0512
# 5        2.0810          2.0809     0.0080    0.0487
# 6        2.0552          2.0551     0.0080    0.0475
# 7        2.0302          2.0301     0.0080    0.0456
# 8        2.0063          2.0062     0.0080    0.0435
# 9        1.9836          1.9834     0.0080    0.0413
# 10        1.9617          1.9615     0.0080    0.0397
# 20        1.7785          1.7786     0.0080    0.0292
# 40        1.5296          1.5300     0.0080    0.0185
# 60        1.3638          1.3645     0.0080    0.0128
# 80        1.2433          1.2443     0.0080    0.0093
# 100        1.1522          1.1540     0.0080    0.0070
# 120        1.0815          1.0840     0.0080    0.0057
# 140        1.0246          1.0276     0.0080    0.0048
# 160        0.9780          0.9817     0.0080    0.0038
# 180        0.9399          0.9446     0.0080    0.0030
# 200        0.9074          0.9131     0.0080    0.0025
# 220        0.8797          0.8865     0.0080    0.0020
# 240        0.8553          0.8635     0.0080    0.0021
# 260        0.8341          0.8437     0.0080    0.0016
# 280        0.8155          0.8262     0.0080    0.0014
# 300        0.7987          0.8106     0.0080    0.0011
# 320        0.7837          0.7969     0.0080    0.0010
# 340        0.7706          0.7850     0.0080    0.0009
# 360        0.7585          0.7741     0.0080    0.0008
# 380        0.7477          0.7644     0.0080    0.0007
# 400        0.7376          0.7554     0.0080    0.0006     
        

## test results against eval_data
predict_rf_input<-eval_data
rf_predict<-predict(rf_model, newdata=predict_rf_input, type="response")
rf_predict<-as.data.frame(rf_predict)
temp_predict<-rf_predict

names(rf_predict)<-gsub(".[0-9]{2,}","",names(rf_predict) )

rf_predict<- as.factor(colnames(rf_predict)[max.col(rf_predict)])

print(table(rf_predict, eval_data$target))


check<-table(rf_predict==eval_data$target)
accuracy<-1-check[1]/(check[1]+check[2])

cat("accuracy of rf is ", round(100*accuracy,2), "%","\n")


### GBM

## assign second partition of train data
td<-train_data2

##assign train_data and use to predict output

rf_predicted<-predict(rf_model, newdata=td, type="response")
rf_predicted<-as.data.frame(rf_predicted)
names(rf_predicted)<-gsub(".[0-9]{2,}","",names(rf_predicted) )
td_target<-train_data2$target

gbm_input<-cbind(rf_predicted, 'target'=td_target)

## use gbm fit to fit output of rf to data
## note that x = the rf_predicted values of class data
## The y are the actual target values of the data

set.seed(8675309)

gbm_fit<-randomForest(target~., data=gbm_input, importance=TRUE, ntree=P_ntree, nodesize=P_nodesize)

## summary of the fit

fitsum <- summary(gbm_fit)

## plot errors to check convergence and overfitting
# 
# gbm_plot<-as.data.frame(cbind(gbm_fit$train.error, gbm_fit$valid.error))
# colnames(gbm_plot)<-c("train.error", "valid.error")
# gbm_plot$iteration<-1:gbm_fit$n.tree
# 
# p<-ggplot(gbm_plot, aes(x=iteration, y = train.error))+geom_line()
# p<-p+geom_line(aes(x=iteration, y =valid.error), color="red")
# 
# print(p)

## EVALUATE PERFORMANCE OF RF->GBM
## assign eval_data
td<-eval_data

cat("the dimensions of the eval data are ", nrow(td), " X ", ncol(td),"\n")

##use to predict output

## fitrst run rf model
rf_predicted<-predict(rf_model, newdata=td, type="response")
rf_predicted<-as.data.frame(rf_predicted)
names(rf_predicted)<-gsub(".[0-9]{2,}","",names(rf_predicted) )
## then run gbm model
gbm_rf_predicted<-predict(gbm_fit, newdata=rf_predicted, type='prob')
gbm_rf_predicted<-as.data.frame(gbm_rf_predicted)
##check prediction
print(head(gbm_rf_predicted))

cat("the dimesions of the rf_predictions are ", nrow(gbm_rf_predicted), " X ", ncol(gbm_rf_predicted),"\n")

cat("and the first few rows are","\n")
print(head(gbm_rf_predicted,5))

## clean up names in predicted table
names(gbm_rf_predicted)<-gsub(".[0-9]{2,}","",names(gbm_rf_predicted) )

## Evaluate rf_predicted
eval_predict <- as.factor(colnames(rf_predicted)[max.col(rf_predicted)])
print(table(eval_predict, eval_data$target))

# eval_predict Class_1 Class_2 Class_3 Class_4 Class_5 Class_6 Class_7 Class_8 Class_9
# Class_1       3       0       0       0       0       0       1       2       3
# Class_2      10     256     107      32       3       9      15       7       6
# Class_3       0      35      46       7       0       0       1       0       0
# Class_4       0       0       0       9       0       0       1       0       0
# Class_5       1       2       0       0      45       0       0       0       0
# Class_6       3       0       0       3       0     247       4       3       5
# Class_7       1       1       3       2       0       0      36       0       0
# Class_8       3       0       2       0       0       6       3     158       7
# Class_9       9       0       0       0       0       2       0       2      74
eval_predict <- as.factor(colnames(gbm_rf_predicted)[max.col(gbm_rf_predicted)])
print(table(eval_predict, eval_data$target))

# eval_predict Class_1 Class_2 Class_3 Class_4 Class_5 Class_6 Class_7 Class_8 Class_9
# Class_1      30       0       0       0       0       0       0       0       0
# Class_2       0     294       0       0       0       0       0       0       0
# Class_3       0       0     157       0       0       0       0       0       0
# Class_4       0       0       1      53       0       0       0       0       0
# Class_5       0       0       0       0      48       0       0       0       0
# Class_6       0       0       0       0       0     264       0       0       0
# Class_7       0       0       0       0       0       0      61       0       0
# Class_8       0       0       0       0       0       0       0     172       0
# Class_9       0       0       0       0       0       0       0       0      95
check<-table(eval_predict==eval_data$target)
accuracy<-1-check[1]/(check[1]+check[2])

cat("accuracy of rf is ", round(100*accuracy,2), "%","\n")

## accuracy of rf is  99.91 % 

##MAKE SUBMISSION

## get the actual test data
directory<- "/Users/winstonsaunders/Documents/pdxkagglegroupproductclassproject/"
file_name<- "test.csv"

submission_test_data<-read.csv(paste0(directory,file_name))
##just make sure its real
print(submission_test_data[1:6,1:10])
## run the prediction

td<-submission_test_data

rf_predicted<-predict(rf_model, newdata=td, type="response")
rf_predicted<-as.data.frame(rf_predicted)
head(rf_predicted)
names(rf_predicted)<-gsub(".[0-9]{2,}","",names(rf_predicted) )
## then run gbm model
gbm_rf_predicted<-predict(gbm_fit, newdata=rf_predicted, type='prob')
gbm_rf_predicted<-as.data.frame(gbm_rf_predicted)

## clean up names in predicted table
names(rf_predicted)<-gsub(".[0-9]{2,}","",names(rf_predicted) )



cat("the dimesions of the submission are", nrow(rf_predicted), " X ", ncol(rf_predicted),"\n")

submission<-rf_predicted

## convert to data frame format
submission<-as.data.frame(submission)

## clean up the numbers
submission<-round(1000*submission,0)/1000.

## add ids back
submission<-cbind("id"=as.integer(submission_test_data$id), submission)
#submission<-as.data.frame(submission)
submission$id<-as.integer(submission$id)
## get rid of scientiic notation
options(scipen=10)
## check data
cat("and the first few rows are \n")
print(head(submission,50))

id Class_1 Class_2 Class_3 Class_4 Class_5 Class_6 Class_7 Class_8 Class_9
# 1     1   0.007   0.191   0.191   0.556   0.002   0.022   0.010   0.012   0.008
# 2     2   0.046   0.374   0.086   0.039   0.008   0.337   0.030   0.048   0.032
# 3     3   0.003   0.006   0.005   0.005   0.001   0.959   0.003   0.015   0.003
# 4     4   0.014   0.504   0.157   0.104   0.004   0.016   0.016   0.021   0.163
# 5     5   0.049   0.052   0.041   0.024   0.008   0.027   0.026   0.339   0.434
# 6     6   0.017   0.410   0.469   0.020   0.005   0.018   0.019   0.023   0.021
# 7     7   0.012   0.031   0.029   0.023   0.004   0.026   0.040   0.820   0.015
# 8     8   0.014   0.743   0.118   0.048   0.004   0.016   0.019   0.021   0.017
# 9     9   0.022   0.386   0.403   0.075   0.007   0.025   0.025   0.032   0.026
# 10   10   0.086   0.278   0.239   0.036   0.011   0.039   0.039   0.229   0.044
# 11   11   0.059   0.185   0.368   0.124   0.016   0.056   0.060   0.073   0.059
# 12   12   0.044   0.029   0.023   0.024   0.004   0.042   0.014   0.027   0.793
# 13   13   0.002   0.004   0.003   0.002   0.983   0.002   0.002   0.002   0.002
# 14   14   0.054   0.445   0.126   0.075   0.017   0.062   0.063   0.086   0.072
# 15   15   0.051   0.409   0.166   0.077   0.016   0.056   0.057   0.072   0.098
# 16   16   0.121   0.096   0.040   0.017   0.003   0.012   0.067   0.630   0.013
# 17   17   0.004   0.010   0.006   0.004   0.001   0.963   0.003   0.005   0.004
# 18   18   0.003   0.007   0.006   0.005   0.001   0.966   0.003   0.005   0.003
# 19   19   0.002   0.005   0.004   0.003   0.971   0.007   0.002   0.003   0.002
# 20   20   0.013   0.551   0.288   0.076   0.004   0.014   0.014   0.024   0.015
# 21   21   0.006   0.011   0.011   0.008   0.001   0.944   0.005   0.007   0.006
# 22   22   0.140   0.042   0.032   0.018   0.006   0.026   0.072   0.639   0.025
# 23   23   0.028   0.616   0.150   0.069   0.008   0.029   0.029   0.038   0.034
# 24   24   0.025   0.046   0.034   0.020   0.006   0.021   0.022   0.799   0.026
# 25   25   0.119   0.078   0.058   0.032   0.009   0.120   0.034   0.050   0.500
# 26   26   0.064   0.031   0.018   0.010   0.003   0.070   0.023   0.742   0.039
# 27   27   0.024   0.364   0.379   0.092   0.008   0.028   0.028   0.048   0.030
# 28   28   0.037   0.631   0.166   0.049   0.007   0.025   0.026   0.033   0.027
# 29   29   0.014   0.503   0.232   0.043   0.004   0.142   0.023   0.019   0.020
# 30   30   0.070   0.344   0.225   0.050   0.039   0.065   0.052   0.084   0.071
# 31   31   0.011   0.795   0.078   0.050   0.004   0.018   0.012   0.019   0.014
# 32   32   0.018   0.770   0.072   0.035   0.007   0.021   0.025   0.028   0.024
# 33   33   0.037   0.355   0.274   0.147   0.013   0.038   0.046   0.047   0.042
# 34   34   0.120   0.067   0.050   0.023   0.006   0.070   0.061   0.265   0.338
# 35   35   0.005   0.015   0.008   0.006   0.001   0.950   0.005   0.006   0.005
# 36   36   0.003   0.005   0.004   0.002   0.001   0.974   0.002   0.007   0.002
# 37   37   0.016   0.660   0.182   0.057   0.005   0.018   0.019   0.024   0.019
# 38   38   0.012   0.452   0.430   0.012   0.004   0.012   0.047   0.016   0.014
# 39   39   0.023   0.069   0.041   0.028   0.007   0.114   0.025   0.659   0.035
# 40   40   0.066   0.196   0.257   0.031   0.019   0.032   0.064   0.043   0.294
# 41   41   0.028   0.248   0.366   0.178   0.009   0.049   0.041   0.042   0.038
# 42   42   0.013   0.382   0.366   0.141   0.004   0.033   0.029   0.017   0.015
# 43   43   0.046   0.301   0.229   0.039   0.009   0.093   0.037   0.049   0.198
# 44   44   0.006   0.019   0.009   0.012   0.001   0.933   0.005   0.010   0.005
# 45   45   0.023   0.597   0.203   0.055   0.007   0.032   0.025   0.033   0.025
# 46   46   0.011   0.310   0.374   0.020   0.002   0.012   0.010   0.014   0.247
# 47   47   0.004   0.007   0.006   0.002   0.001   0.108   0.020   0.850   0.003
# 48   48   0.032   0.212   0.269   0.061   0.010   0.044   0.279   0.056   0.037
# 49   49   0.002   0.008   0.004   0.002   0.973   0.002   0.002   0.003   0.003
# 50   50   0.021   0.487   0.369   0.020   0.006   0.021   0.022   0.028   0.025
# 51   51   0.057   0.556   0.132   0.040   0.013   0.044   0.043   0.058   0.058
# 52   52   0.054   0.011   0.006   0.004   0.001   0.843   0.003   0.004   0.074
# 53   53   0.035   0.341   0.119   0.113   0.011   0.099   0.050   0.094   0.138
# 54   54   0.052   0.158   0.405   0.136   0.014   0.052   0.056   0.067   0.060
# 55   55   0.046   0.009   0.006   0.003   0.001   0.003   0.004   0.924   0.004
# 56   56   0.015   0.006   0.006   0.003   0.001   0.004   0.033   0.896   0.037
# 57   57   0.004   0.010   0.007   0.006   0.001   0.957   0.005   0.006   0.005
# 58   58   0.004   0.008   0.015   0.005   0.001   0.921   0.003   0.040   0.003
# 59   59   0.011   0.624   0.207   0.098   0.004   0.013   0.013   0.017   0.013
# 60   60   0.015   0.460   0.378   0.062   0.004   0.016   0.016   0.021   0.027
# 61   61   0.019   0.019   0.012   0.006   0.002   0.005   0.005   0.037   0.895
# 62   62   0.032   0.066   0.026   0.025   0.002   0.082   0.008   0.749   0.008
# 63   63   0.025   0.658   0.112   0.064   0.021   0.028   0.027   0.035   0.031
# 64   64   0.056   0.110   0.086   0.056   0.089   0.111   0.053   0.090   0.350
# 65   65   0.009   0.754   0.161   0.025   0.003   0.011   0.011   0.014   0.012
# 66   66   0.007   0.272   0.079   0.590   0.003   0.022   0.008   0.011   0.008
# 67   67   0.003   0.013   0.005   0.003   0.963   0.003   0.003   0.004   0.003
# 68   68   0.015   0.698   0.190   0.021   0.004   0.016   0.016   0.022   0.017
# 69   69   0.011   0.813   0.089   0.029   0.003   0.012   0.012   0.016   0.014
# 70   70   0.032   0.155   0.133   0.091   0.010   0.447   0.046   0.047   0.038
# 71   71   0.040   0.507   0.165   0.088   0.012   0.043   0.043   0.056   0.045
# 72   72   0.065   0.092   0.079   0.045   0.014   0.049   0.049   0.548   0.058
# 73   73   0.005   0.005   0.004   0.004   0.001   0.974   0.002   0.003   0.002
# 74   74   0.116   0.143   0.094   0.054   0.016   0.059   0.058   0.080   0.378
# 75   75   0.065   0.111   0.097   0.076   0.016   0.119   0.058   0.390   0.067
# 76   76   0.015   0.586   0.217   0.101   0.005   0.017   0.018   0.023   0.018
# 77   77   0.015   0.783   0.098   0.022   0.005   0.016   0.017   0.023   0.021
# 78   78   0.005   0.008   0.008   0.004   0.001   0.004   0.004   0.962   0.005
# 79   79   0.025   0.681   0.116   0.043   0.008   0.028   0.028   0.037   0.035
# 80   80   0.011   0.280   0.114   0.454   0.003   0.084   0.011   0.028   0.014
# 81   81   0.092   0.250   0.330   0.012   0.004   0.014   0.260   0.023   0.015
# 82   82   0.004   0.054   0.023   0.015   0.001   0.874   0.004   0.015   0.008
# 83   83   0.004   0.019   0.006   0.002   0.002   0.004   0.942   0.017   0.003
# 84   84   0.010   0.745   0.137   0.052   0.003   0.011   0.017   0.015   0.011
# 85   85   0.106   0.032   0.018   0.016   0.003   0.185   0.009   0.011   0.619
# 86   86   0.012   0.439   0.449   0.035   0.004   0.014   0.014   0.018   0.015
# 87   87   0.154   0.313   0.123   0.066   0.019   0.070   0.070   0.096   0.089
# 88   88   0.002   0.008   0.003   0.002   0.976   0.002   0.002   0.003   0.002
# 89   89   0.026   0.474   0.266   0.087   0.008   0.039   0.030   0.039   0.031
# 90   90   0.024   0.033   0.027   0.020   0.005   0.016   0.016   0.840   0.019
# 91   91   0.022   0.591   0.142   0.054   0.007   0.028   0.025   0.032   0.098
# 92   92   0.289   0.017   0.014   0.008   0.003   0.019   0.009   0.012   0.629
# 93   93   0.003   0.007   0.005   0.004   0.001   0.958   0.005   0.014   0.003
# 94   94   0.047   0.384   0.150   0.087   0.015   0.130   0.054   0.070   0.062
# 95   95   0.002   0.006   0.004   0.003   0.974   0.002   0.002   0.003   0.003
# 96   96   0.029   0.047   0.038   0.029   0.015   0.023   0.023   0.772   0.024
# 97   97   0.058   0.183   0.100   0.092   0.016   0.282   0.131   0.079   0.060
# 98   98   0.005   0.012   0.009   0.004   0.001   0.004   0.010   0.951   0.004
# 99   99   0.028   0.654   0.136   0.029   0.008   0.029   0.030   0.050   0.035
# 100 100   0.031   0.052   0.039   0.037   0.007   0.695   0.025   0.087   0.026

hist(submission$Class_4)
sub_check<-submission[,-1]
sub_check <- as.factor(colnames(sub_check)[max.col(sub_check)])
table(sub_check)

## write csv
write.csv(submission, paste0(directory,"May15b",".csv"), row.names=F, quote=F)
