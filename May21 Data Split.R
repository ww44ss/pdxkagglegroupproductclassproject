## PDX DATA SCIENCE KAGGLE PROJECT
## 
## In this thread continues RF and GBM combos
## SWITCH TO CARET GBM
## 


## LOAD REQUIRED PACKAGES

        require(ggplot2)
        require(gbm)
        require(randomForest)
        require(caret)
        require(plyr)


## PARAMETER SPACE
        ## data split parameters
        RF_sample<-0.6
        ## GBM_sample must be < 1.0 for there to be an eval data set. 
        ## currently the program does not control for a zero eval set 
        GBM_sample<-0.90
        
        ## Random Forest Parameters
        P_ntree<-50
        P_nodesize<-1
        
        ## GBM Parameters
        gbm_control<- trainControl(method="repeatedcv",
                                   number=3,
                                   repeats=2)
        gbm_grid <- expand.grid(
                .interaction.depth = (1:5)*2, 
                .n.trees = (2:4)*50, 
                .shrinkage = .1, 
                .n.minobsinnode=5)
      
        
## DATA PARTITION
        ## group Classes 3 4 and 5 into one class
        group_234<- function(datax){
                ## create additional factor column
                datax$group<-rep("group_1", nrow(datax))
                ## turn target into character
                datax$target<-as.character(datax$target)
                datax$group[datax$target=="Class_3"|datax$target=="Class_4"|datax$target=="Class_2"]<-"group_234"
                datax$target<-as.factor(datax$target)
                datax$group<-as.factor(datax$group)
                print(table(datax$group))
                return(datax)
        }
        
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

## RESERVE DATA
        train_data_full<-train_data
        train_data2_full<-train_data2
        eval_data_full<-eval_data
        
## GROUP DATA
        ## apply grouping function to full data sets
        train_data<-group_234(train_data)
        train_data2<-group_234(train_data2)
        eval_data<-group_234(eval_data)
        
## RANDOM FOREST

        ## assign td
        td<-train_data
      
        ## Random forest Model
        set.seed(8765309)
        cat("computing RF \n")
        rf_model <- randomForest(group~.-id-target, data=td, importance=TRUE, ntree=P_ntree, nodesize=P_nodesize)

                ## test results against eval_data
                predict_rf_input<-eval_data
                rf_predict<-predict(rf_model, newdata=predict_rf_input, type="prob")
                rf_predict<-as.data.frame(rf_predict)
                        
                
                rf_predict<- as.factor(colnames(rf_predict)[max.col(rf_predict)])

                print(table(rf_predict, eval_data$group))
   
                check<-table(rf_predict==eval_data$group)
                accuracy<-1-check[1]/(check[1]+check[2])

                cat("accuracy of rf is ", round(100*accuracy,2), "%","\n")

                ## accuracy of rf is  97.31 %

        
### SPLIT DATA ANALYSIS
        
        td<-train_data2
        
        
        ## make the predictions
        rf_predicted<-predict(rf_model, newdata=td, type="prob")
        
        rf_predicted<-as.data.frame(rf_predicted)
        
        ## predict group for data
        group_predict<- as.factor(colnames(rf_predicted)[max.col(rf_predicted)])
        
        
        ## split data into groups
        td_gr1<-td[group_predict=="group_1", ]
        td_gr234<-td[group_predict=="group_234", ]
        
### GBM

        
        #First try single model with additional factor

        gbm_input<-td_gr234
      

        set.seed(8675309)
        
        bootControl <- trainControl(number = 1)
        
        gbm_fit234 <- train(target~.-id,
                        data=gbm_input, 
                        method = "gbm", 
                        tuneLength = 5,
                        nTrain=0.8*nrow(gbm_input),
                        trControl = bootControl,
                        ##scaled = FALSE,
                        tuneGrid = gbm_grid 
                )
        
        
        
        gbm_input<-td_gr1
        
        
        set.seed(8675309)
        
        gbm_fit1 <- train(target~.-id,
                          data=gbm_input, 
                          method = "gbm", 
                          tuneLength = 5,
                          nTrain=0.8*nrow(gbm_input),
                          trControl = bootControl,
                          ##scaled = FALSE,
                          tuneGrid = gbm_grid 
        )

        print(gbm_fit234$bestTune)
        print(gbm_fit1$bestTune)
        
        
        plot(gbm_fit234)
        plot(gbm_fit1)
        
        
        plot(gbm_fit234, metric="Accuracy", plotType="level", scales=list(x=list(rot=90)))
        plot(gbm_fit1, metric="Accuracy", plotType="level", scales=list(x=list(rot=90)))

## EVALUATE PERFORMANCE OF RF->GBM
        ## assign eval_data
        td<-eval_data
        
        rownames(td)<-1:nrow(td)
        td$id<-as.numeric(rownames(td))

        cat("the dimensions of the eval data are ", nrow(td), " X ", ncol(td),"\n")

        td$group<-NULL
        
        ##use to predict output

        ## fitrst run rf model
        rf_predicted<-predict(rf_model, newdata=td, type="prob")
        rf_predicted<-as.data.frame(rf_predicted)
        
        group_predict<- as.factor(colnames(rf_predicted)[max.col(rf_predicted)])
        
        td<-cbind(td, "group"=group_predict)
        td_gr1<-td[td$group=="group_1", ]
        td_gr234<-td[td$group=="group_234", ]
        
        ## then run gbm model
        gbm_predict1<-predict(gbm_fit1, newdata=td_gr1, type='prob')
        ## add bookkeeping to ensure correct row order
        gbm_predict1<-cbind("id"=td_gr1$id, gbm_predict1)
        gbm_predict234<-predict(gbm_fit234, newdata=td_gr234, type='prob')
        ## add bookkeeping to ensure correct row order
        gbm_predict234<-cbind("id"=td_gr234$id, gbm_predict234)
        prediction_all<-rbind(gbm_predict1, gbm_predict234)
        ##check prediction
        
        
        prediction_all<-prediction_all[order(prediction_all$id),]
        print(head(prediction_all))

        cat("the dimesions of the rf_predictions are ", nrow(gbm_rf_predicted), " X ", ncol(gbm_rf_predicted),"\n")

        cat("and the first few rows are","\n")
        print(head(prediction_all,5))
        
        ## clean up names in predicted table
        names(prediction_all)<-gsub(".[0-9]{2,}","",names(prediction_all) )

        ## get rid of bookkeping
        prediction_all$id<-NULL
## Evaluate rf_predicted
        eval_predict <- as.factor(colnames(prediction_all)[max.col(prediction_all)])
        print(table(eval_predict, eval_data$target))

#         eval_predict Class_1 Class_2 Class_3 Class_4 Class_5 Class_6 Class_7 Class_8 Class_9
#         Class_1      46       1       0       0       0       0       2       0       4
#         Class_2       4     514     123      23       1       3       9       1       2
#         Class_3       1      41     165      15       0       0       6       1       0
#         Class_4       0       5       2      58       0       2       2       0       0
#         Class_5       1       3       0       0      86       0       0       0       0
#         Class_6       1       3       2       3       0     500       3       4       5
#         Class_7       1       5       6       1       0       0      80       1       1
#         Class_8       5       1       2       1       0       4       3     305       7
#         Class_9       9       1       1       0       0       4       1       3     148       

        check<-table(eval_predict==eval_data$target)
        accuracy<-1-check[1]/(check[1]+check[2])

        cat("accuracy of rf is ", round(100*accuracy,2), "%","\n")

        ## accuracy of rf is  85.22 %
   
        ## Best yet!     
        

##MAKE SUBMISSION

        ## get the actual test data
        directory<- "/Users/winstonsaunders/Documents/pdxkagglegroupproductclassproject/"
        file_name<- "test.csv"

        submission_test_data<-read.csv(paste0(directory,file_name))
        ##just make sure its real
        print(submission_test_data[1:6,1:10])
        ## run the prediction
        
        td<-submission_test_data
        
        rf_predicted<-predict(rf_model, newdata=td, type="prob")
        rf_predicted<-as.data.frame(rf_predicted)
        head(rf_predicted)
        ## then run gbm model
        gbm_rf_predicted<-predict(gbm_fit, newdata=rf_predicted, type='response')
        gbm_rf_predicted<-as.data.frame(gbm_rf_predicted)
        
        ## clean up names in predicted table
        names(gbm_rf_predicted)<-gsub(".[0-9]{2,}","",names(gbm_rf_predicted) )
        

        
        cat("the dimesions of the submission are", nrow(gbm_rf_predicted), " X ", ncol(gbm_rf_predicted),"\n")
        
        submission<-gbm_rf_predicted
        
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
        print(head(submission,5))

        ## write csv
        write.csv(submission, paste0(directory,"May17a",".csv"), row.names=F, quote=F)
        
