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
                                   number=5,
                                   repeats=2)
        gbm_grid <- expand.grid(
                .interaction.depth = (1:5), 
                .n.trees = (1:3)*100, 
                .shrinkage = (1:3)*.005, 
                .n.minobsinnode=5)
      
        
## DATA PARTITION
        ## group Classes 3 4 and 5 into one class
        group_234<- function(datax){
                datax$target<-as.character(datax$target)
                datax$target[datax$target=="Class_3"|datax$target=="Class_4"|datax$target=="Class_2"]<-"Class_234"
                datax$target<-as.factor(datax$target)
                print(table(datax$target))
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
      
        str(td)

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
   
                check<-table(rf_predict==eval_data$target)
                accuracy<-1-check[1]/(check[1]+check[2])

                cat("accuracy of rf is ", round(100*accuracy,2), "%","\n")

                ## accuracy of rf %

        
### SPLIT DATA ANALYSIS
        
        td<-train_data2
        
        ## make the predictions
        rf_predicted<-predict(rf_model, newdata=td, type="prob")
        rf_predicted<-as.data.frame(rf_predicted)
        
        gbm_input<-cbind(rf_predicted, 'target'=train_data2$target)
        
        
        ## add an_index which will be later used to recombine rows
        rf$predicted$an_index<-1:nrow(rf_predicted)
        
        ## make target a character field
        rf_predicted$target<-as.character(rf_predicted$target)
        345_predicted<-rf_predicted[rf_predicted$target=="Class_345",]
        else_predicted<-rf_predicted[rf_predicted$target!="Class_345",]
        
        ## turn back into classification factors
        
        345_predicted$target<-as.factor(345_predicted$target)
        else_predicted$target<-as.factor(else_predicted$target)
        
        head(345_predicted)
        str(else_predicted)

### GBM

        ## assign second partition of train data
        td<-train_data2
        
        ##assign train_data and use to predict output
        
        rf_predicted<-predict(rf_model, newdata=td, type="prob")
        rf_predicted<-as.data.frame(rf_predicted)
        
        head(rf_predicted)
        
        ## This is where separate models are created
        ## requires careful bookkeeping
        
        

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
                        tuneLength = 5,
                        train.fraction=0.8,
                        trControl = bootControl,
                        ##scaled = FALSE,
                        tuneGrid = gbmGrid 
                )
        

#         gbm_fit<-gbm(target~.,data=gbm_input, 
#                      n.trees = P_n.trees,
#                      distribution = "multinomial",
#                      shrinkage=P_shrinkage,
#                      interaction.depth=P_interaction.depth,
#                      #n.minobsinnode<-P_n.minobsinnode,
#                      train.fraction=P_train.fraction,
#                      keep.data=TRUE,
#                      verbose=TRUE,
#                      cv.folds=P_cv.folds,                ## 3-fold cv 
#                      n.cores=1)
# 
        ## summary of the fit

        fitsum <- summary(gbm_fit)

        ## plot errors to check convergence and overfitting

        gbm_plot<-as.data.frame(cbind(gbm_fit$train.error, gbm_fit$valid.error))
        colnames(gbm_plot)<-c("train.error", "valid.error")
        gbm_plot$iteration<-1:gbm_fit$n.tree

        p<-ggplot(gbm_plot, aes(x=iteration, y = train.error))+geom_line()
        p<-p+geom_line(aes(x=iteration, y =valid.error), color="red")

        print(p)

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
        print(head(gbm_rf_predicted))

        cat("the dimesions of the rf_predictions are ", nrow(gbm_rf_predicted), " X ", ncol(gbm_rf_predicted),"\n")

        cat("and the first few rows are","\n")
        print(head(gbm_rf_predicted,5))
        
        ## clean up names in predicted table
        names(gbm_rf_predicted)<-gsub(".[0-9]{2,}","",names(gbm_rf_predicted) )

## Evaluate rf_predicted
        eval_predict <- as.factor(colnames(rf_predicted)[max.col(rf_predicted)])
        print(table(eval_predict, eval_data$target))

#         eval_predict Class_1 Class_2 Class_3 Class_4 Class_5 Class_6 Class_7 Class_8 Class_9
#         Class_1      17       0       0       0       0       1       1       1       3
#         Class_2       6     520     143      47       3       4      12       2       0
#         Class_3       0      45     146       9       0       0       7       1       1
#         Class_4       0       3       2      38       0       1       4       0       0
#         Class_5       1       1       0       0      83       0       0       0       0
#         Class_6       4       1       0       7       1     492      11       6       6
#         Class_7       2       3       7       0       0       1      60       0       1
#         Class_8      16       1       2       0       0      11      10     304      11
#         Class_9      22       0       1       0       0       3       1       1     145

        eval_predict <- as.factor(colnames(gbm_rf_predicted)[max.col(gbm_rf_predicted)])
        print(table(eval_predict, eval_data$target))

        #         eval_predict Class_1 Class_2 Class_3 Class_4 Class_5 Class_6 Class_7 Class_8 Class_9
        #         Class_1      38       1       0       0       0       2       3       3       7
        #         Class_2       6     486     116      28       1       1       2       2       0
        #         Class_3       0      72     170      14       0       0      12       0       0
        #         Class_4       1       7       5      52       0       1       3       0       0
        #         Class_5       1       1       0       0      86       0       0       0       0
        #         Class_6       4       1       0       5       0     494       3       6       7
        #         Class_7       2       6       7       2       0       2      76       0       2
        #         Class_8       6       0       2       0       0      11       7     301      10
        #         Class_9      10       0       1       0       0       2       0       3     141

        check<-table(eval_predict==eval_data$target)
        accuracy<-1-check[1]/(check[1]+check[2])

        cat("accuracy of rf is ", round(100*accuracy,2), "%","\n")

        ## accuracy of rf is  82.62 % 

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
        
