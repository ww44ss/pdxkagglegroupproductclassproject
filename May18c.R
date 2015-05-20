## PDX DATA SCIENCE KAGGLE PROJECT
## Program thread 5
## In this thread continues RF and GBM combos
## SWITCH TO CARET GBM
## Boost up some of the GBM parameters and decrease number of trees in RF (improved)
##
## score of 0.52819 (not an improvement)


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
        P_ntree<-100
        P_nodesize<-2
        
        ## GBM Parameters
        gbm_control<- trainControl(method="repeatedcv",
                                   number=5,
                                   repeats=2)
        gbm_grid <- expand.grid(
                .interaction.depth = (1:3)*2, 
                .n.trees = (1:3)*50, 
                .shrinkage = (1:2)*.01, 
                .n.minobsinnode=2)
        
        
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
        cat("computing RF \n")
        rf_model <- randomForest(target~.-id, data=td, importance=TRUE, ntree=P_ntree, nodesize=P_nodesize)

                ## test results against eval_data
                predict_rf_input<-eval_data
                rf_predict<-predict(rf_model, newdata=predict_rf_input, type="prob")
                rf_predict<-as.data.frame(rf_predict)
                        temp_predict<-rf_predict
                
                rf_predict<- as.factor(colnames(rf_predict)[max.col(rf_predict)])

                print(table(rf_predict, eval_data$target))
        
# #       rf_predict Class_1 Class_2 Class_3 Class_4 Class_5 Class_6 Class_7 Class_8 Class_9
#         Class_1      19       0       0       0       0       0       1       1       2
#         Class_2       7     516     152      45       3       5      18       3       1
#         Class_3       0      48     137      13       0       0       6       0       1
#         Class_4       0       0       2      36       0       1       3       0       0
#         Class_5       1       1       0       0      83       0       0       0       0
#         Class_6       4       2       0       7       1     490       9       7       7
#         Class_7       3       4       7       0       0       3      60       0       2
#         Class_8      14       3       2       0       0      11       9     302      11
#         Class_9      20       0       1       0       0       3       0       2     143
        
                check<-table(rf_predict==eval_data$target)
                accuracy<-1-check[1]/(check[1]+check[2])

                cat("accuracy of rf is ", round(100*accuracy,2), "%","\n")

                ## aaccuracy of rf is  80.02 % 


### GBM

        ## assign second partition of train data
        td<-train_data2
        
        ##assign train_data and use to predict output
        
        rf_predicted<-predict(rf_model, newdata=td, type="prob")
        rf_predicted<-as.data.frame(rf_predicted)

        td_target<-train_data2$target

        gbm_input<-cbind(rf_predicted, 'target'=td_target)
      
        ## use gbm fit to fit output of rf to data
        ## note that x = the rf_predicted values of class data
        ## The y are the actual target values of the data

        set.seed(8675309)
        
        
       
       
        bootControl <- trainControl(number = 1)
        
        gbm_fit <- train(target~.,
                        data=gbm_input, 
                        method = "gbm", 
                        trControl = gbm_control,
                        ##scaled = FALSE,
                        tuneGrid = gbm_grid,
                        verbose=FALSE
                )
        

        ## plot errors to check convergence and overfitting
        
        print(gbm_fit$bestTune)
        
        plot(gbm_fit)
        
        plot(gbm_fit, metric="Accuracy", plotType="level", scales=list(x=list(rot=90)))



## EVALUATE PERFORMANCE OF RF->GBM
        ## assign eval_data
        td<-eval_data

        cat("the dimensions of the eval data are ", nrow(td), " X ", ncol(td),"\n")

        ##use to predict output

        ## fitrst run rf model
        rf_predicted<-predict(rf_model, newdata=td, type="prob")
        rf_predicted<-as.data.frame(rf_predicted)
        ## then run gbm model
        gbm_rf_predicted<-predict(gbm_fit, newdata=rf_predicted, type='prob')
        gbm_rf_predicted<-as.data.frame(gbm_rf_predicted)
        ##check prediction

        cat("the dimesions of the rf_predictions are ", nrow(gbm_rf_predicted), " X ", ncol(gbm_rf_predicted),"\n")

        cat("and the first few rows are","\n")
        print(head(gbm_rf_predicted,5))
        
        ## clean up names in predicted table
        names(gbm_rf_predicted)<-gsub(".[0-9]{2,}","",names(gbm_rf_predicted) )

## Evaluate rf_predicted
        eval_predict <- as.factor(colnames(rf_predicted)[max.col(rf_predicted)])
        print(table(eval_predict, eval_data$target))

#         eval_predict Class_1 Class_2 Class_3 Class_4 Class_5 Class_6 Class_7 Class_8 Class_9
#         Class_1      19       0       0       0       0       0       1       1       2
#         Class_2       6     516     151      45       3       5      18       3       1
#         Class_3       0      48     138      13       0       0       6       0       0
#         Class_4       0       0       2      36       0       1       2       0       0
#         Class_5       1       1       0       0      83       0       0       0       0
#         Class_6       5       3       0       7       1     490      10       7       7
#         Class_7       3       4       7       0       0       3      60       0       2
#         Class_8      14       2       2       0       0      11       9     302      11
#         Class_9      20       0       1       0       0       3       0       2     144

        eval_predict <- as.factor(colnames(gbm_rf_predicted)[max.col(gbm_rf_predicted)])
        print(table(eval_predict, eval_data$target))

#         eval_predict Class_1 Class_2 Class_3 Class_4 Class_5 Class_6 Class_7 Class_8 Class_9
#         Class_1      26       0       0       0       0       0       3       3       6
#         Class_2       6     493     129      27       3       3       5       2       0
#         Class_3       0      58     151      12       0       0       8       0       1
#         Class_4       0      10       7      56       0       1       4       0       0
#         Class_5       1       1       0       0      84       0       0       0       0
#         Class_6       4       3       1       4       0     491       4       4       7
#         Class_7       4       8      10       2       0       4      75       1       2
#         Class_8      12       1       2       0       0      11       6     302      10
#         Class_9      15       0       1       0       0       3       1       3     141
        check<-table(eval_predict==eval_data$target)
        accuracy<-1-check[1]/(check[1]+check[2])

        cat("accuracy of rf is ", round(100*accuracy,2), "%","\n")

        ## accuracy of rf is  81.5 % 

##MAKE SUBMISSION

        ## get the actual test data
        directory<- "/Users/winstonsaunders/Documents/pdxkagglegroupproductclassproject/"
        file_name<- "test.csv"

        submission_test_data<-read.csv(paste0(directory,file_name))
        ##just make sure its real
        print(submission_test_data[1:6,1:10])
        dim(submission_test_data)
        
        ## run the prediction
        
        td<-submission_test_data
        
        rf_predicted<-predict(rf_model, newdata=td, type="prob")
        rf_predicted<-as.data.frame(rf_predicted)
        head(rf_predicted)
        ## then run gbm model
        gbm_rf_predicted<-predict(gbm_fit, newdata=rf_predicted, type='prob')
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
        write.csv(submission, paste0(directory,"May18c",".csv"), row.names=F, quote=F)
        

