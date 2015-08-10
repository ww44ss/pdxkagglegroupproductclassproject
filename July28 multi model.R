## PDX DATA SCIENCE KAGGLE PROJECT

## 


## LOAD REQUIRED PACKAGES

        require(ggplot2)
        require(gbm)
        require(adabag)
        require(randomForest)
        require(caret)
        require(plyr)
        library(nnet)


## PARAMETER SPACE
        ## data split parameters
        RF_sample<-0.6
        ## GBM_sample must be < 1.0 for there to be an eval data set. 
        ## currently the program does not control for a zero eval set 
        GBM_sample<-0.90
        
        ## Random Forest Parameters
        P_ntree<-20
        P_nodesize<-5
        
        ## GBM Parameters
        gbm_control<- trainControl(method="repeatedcv",
                                   number=3,
                                   repeats=2)
        gbm_grid <- expand.grid(
                .interaction.depth = (2:3)*10, 
                .n.trees = (2:3)*100, 
                .shrinkage = .05, 
                .n.minobsinnode=3)
        
        ada_grid<- expand.grid(
                .iter= c(50,100),
                .maxdepth = c(4,8),
                .nu = c(0.1,1))
        
        cv_control <- trainControl(method= "repeatedcv", repeats = 3)
      
        
## FUNCTION FOR DATA BUNDLING
        ## group Classes 2, 3 4 into one class called group_234, all others into another called group_1
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
        
        ## group Classes 2 into group_2 and 3 4 into  group_34
        group_34<- function(datax){
            ## create additional factor column
            datax$group<-rep("group_2", nrow(datax))
            ## turn target into character
            datax$target<-as.character(datax$target)
            datax$group[datax$target=="Class_3"|datax$target=="Class_4"]<-"group_34"
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
        ## create two data sets
        td<-train_data[sample_rows,]
        train_data2<-train_data[-sample_rows,]
        
        ## create a third, smaller sample for intermediate evaluation
        sample_rows2<- sample(1:nrow(train_data2), size=GBM_sample*nrow(train_data2))
        ## create two data sets, preserving teh name of the first
        train_data2<-train_data2[sample_rows2,]
        eval_data<-train_data2[-sample_rows2,]
        
        ## reassign train data
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
        ## apply grouping function to full data sets to evaluate 
        train_data<-group_234(train_data)
        train_data2<-group_234(train_data2)
        eval_data<-group_234(eval_data)
        
## CATEGORIZATION MODEL (SPLIT INTO TWO GROUPS)

        ## assign td
        td<-train_data
      
        ## Random forest Model
        set.seed(8765309)
        start_time <- proc.time()
        cat("computing RF \n")
        categorization_model <- randomForest(group~.-id-target, data=td, importance=TRUE, ntree=P_ntree, nodesize=P_nodesize)
        finish_time<-proc.time()
        elapsed_time<-finish_time-start_time
        cat("elapsed time ", elapsed_time[3], " seconds \n")
        
        #elapsed time  51.105  seconds 
        
                ## test results against eval_data
                predict_categorization_input<-eval_data
                categorization_predict<-predict(categorization_model, newdata=predict_categorization_input, type="prob")
                categorization_predict<-as.data.frame(categorization_predict)
                        
                
                categorization_predict<- as.factor(colnames(categorization_predict)[max.col(categorization_predict)])

                print(table(categorization_predict, eval_data$group))
                
#                 categorization_predict group_1 group_234
#                 group_1      1225        28
#                 group_234      31       948
   
                check<-table(categorization_predict==eval_data$group)
                accuracy<-1-check[1]/(check[1]+check[2])

                cat("accuracy of rf is ", round(100*accuracy,2), "%","\n")

 #                  accuracy of rf is  97.36 % 

        
### MODELS FOR gr_234
    ##
    ## GET THE DATA
                
        
        td<-train_data
        
        ## split data into groups
        td_gr234<-td[td$group=="group_234", ]
        
        ## drop levels
        td_gr234<-droplevels(td_gr234)
        
        ## get rid of group designator
        td_gr234$group<-NULL
        ## explore data correlations
        #ggplot(td_gr234, aes(y=feat_67, x = target))+geom_jitter() 
        #ggplot(td_gr234, aes(y=feat_15, x = feat_15*feat_16, color = target))+geom_jitter() 
        #ggplot(td_gr234, aes(y=sqrt(feat_23*feat_24), x = sqrt(feat_22*feat_23), color = target))+geom_jitter() 
    
        
    log_normalize <- function(x){
        return( log(x+1)
                )
    }    
    
    ## create normalized data frame
    td_new<-as.data.frame(lapply(td_gr234[,2:(ncol(td_gr234)-1)], log_normalize))
    td_new<-cbind("id"=td_gr234$id, td_new, "target"=td_gr234$target)
    
    set.seed(8675309)
    start_time <- proc.time()
    cat("computing RF \n")
    model_234 <- nnet(target~.-id, data=td_new, entropy=TRUE, size = 5, rang = 0.05, maxit = 1000)
    print(model_234)
    
    finish_time<-proc.time()
    elapsed_time<-finish_time-start_time
    cat("user time ", elapsed_time[1], " seconds \n")
    cat("elapsed time ", elapsed_time[3], " seconds \n")
    
    eval_gr234<-eval_data[eval_data$group=="group_234",]
    
    predict_234<-predict(model_234, newdata=eval_gr234, type="raw")
    predict_234<-as.data.frame(predict_234)
    
    predict_234_nnet<-predict_234
    
    predict_234_single<- as.factor(colnames(predict_234)[max.col(predict_234)])
    
    print(table(predict_234_single, eval_gr234$target))

    ## Neural Network Model for gr_234
        
        set.seed(8675309)
        start_time <- proc.time()
        cat("computing RF \n")
        model_234 <- nnet(target~.-id, data=td_gr234, entropy=TRUE, size = 5, rang = 0.05, maxit = 1000)
        print(model_234)
    
        finish_time<-proc.time()
        elapsed_time<-finish_time-start_time
        cat("user time ", elapsed_time[1], " seconds \n")
        cat("elapsed time ", elapsed_time[3], " seconds \n")
        
        eval_gr234<-eval_data[eval_data$group=="group_234",]
        
        predict_234<-predict(model_234, newdata=eval_gr234, type="raw")
        predict_234<-as.data.frame(predict_234)
        
        predict_234_nnet<-predict_234
        
        predict_234_single<- as.factor(colnames(predict_234)[max.col(predict_234)])
        
        print(table(predict_234_single, eval_gr234$target))
        
#       model_234 <- nnet(target~.-id, data=td_gr234, entropy=TRUE, size = 5, rang = 0.05, maxit = 1000)
#         predict_234_single Class_1 Class_2 Class_3 Class_4 Class_5 Class_6 Class_7 Class_8 Class_9
#                   Class_2       0     465     166      43       0       0       0       0       0
#                   Class_3       0      99     130      11       0       0       0       0       0
#                   Class_4       0      10       5      47       0       0       0       0       0  
   
## RANDOM FOREST 
        
        #
        
        set.seed(8675309)
        start_time <- proc.time()
        cat("computing RF \n")
        
        model_234 <- randomForest(target~.-id, data=td_gr234, importance=TRUE, ntree=P_ntree, nodesize=P_nodesize)
        
        finish_time<-proc.time()
        elapsed_time<-finish_time-start_time
        cat("user time ", elapsed_time[1], " seconds \n")
        cat("elapsed time ", elapsed_time[3], " seconds \n")
        
        eval_gr234<-eval_data[eval_data$group=="group_234",]
        
        predict_234<-predict(model_234, newdata=eval_gr234, type="prob")
        predict_234<-as.data.frame(predict_234)
        
        predict_234_rf<-predict_234
        
        predict_234_single<- as.factor(colnames(predict_234)[max.col(predict_234)])
        
        print(table(predict_234_single, eval_gr234$target))
        
#         predict_234_single Class_1 Class_2 Class_3 Class_4 Class_5 Class_6 Class_7 Class_8 Class_9
#         Class_2       0     497     142      40       0       0       0       0       0
#         Class_3       0      74     153      20       0       0       0       0       0
#         Class_4       0       3       6      41       0       0       0       0       0       #         
        

    ## GBM 
        gbm_fit234 <- train(target~.-id,
                         data=td_gr234, 
                         method = "gbm", 
                         trControl = gbm_control,
                         ##scaled = FALSE,
                         tuneGrid = gbm_grid,
                         train.fraction=0.67,
                         verbose=TRUE
        )
        
       
      
        
        predict_234<-predict(gbm_fit234, newdata=eval_gr234, type="prob")
        predict_234<-as.data.frame(predict_234)
        
        predict_234_gbm<-predict_234
        
        
        predict_234_single<- as.factor(colnames(predict_234)[max.col(predict_234)])
        
        print(table(predict_234_single, eval_gr234$target))
        
        
#         predict_234_single Class_1 Class_2 Class_3 Class_4 Class_5 Class_6 Class_7 Class_8 Class_9
#         Class_2       0     493     146      31       0       0       0       0       0
#         Class_3       0      73     149      15       0       0       0       0       0
#         Class_4       0       8       6      55       0       0       0       0       0
       
        eval_gr234<-droplevels(eval_gr234)
        
        ## let's do some examination
        eval_temp<-eval_gr234
        eval_temp<-cbind(eval_temp, predict_234_single)
        misses<-eval_temp[eval_temp$target!=predict_234_single,]
        hits<-eval_temp[eval_temp$target==predict_234_single,]
        
        
    ## GBM  of Log
        
        set.seed(8675309)
        
        bb<-td_gr234[,c(-1, -ncol(td_gr234))]
        bb<-log10(bb+1)
        
        log_td_gr234<-cbind("id"=td_gr234[,1], bb, "target"=td_gr234[,ncol(td_gr234)])
        
        gbm_fit234 <- train(target~.-id,
                            data=log_td_gr234, 
                            method = "gbm", 
                            trControl = gbm_control,
                            ##scaled = FALSE,
                            tuneGrid = gbm_grid,
                            train.fraction=0.67,
                            verbose=TRUE
        )
        
        
        
        
        predict_234<-predict(gbm_fit234, newdata=eval_gr234, type="prob")
        predict_234<-as.data.frame(predict_234)
        
        predict_234_gbm_log<-predict_234
        
        
        predict_234_single<- as.factor(colnames(predict_234)[max.col(predict_234)])
        
        print(table(predict_234_single, eval_gr234$target))
        
        
#         predict_234_single Class_2 Class_3 Class_4
#         Class_2               169      36      10
#         Class_3               262     199      24
#         Class_4               143      66      67
        
        
        eval_gr234<-droplevels(eval_gr234)
        
        ## let's do some examination
        eval_temp<-eval_gr234
        eval_temp<-cbind(eval_temp, predict_234_single)
        misses<-eval_temp[eval_temp$target!=predict_234_single,]
        hits<-eval_temp[eval_temp$target==predict_234_single,]

## ADA
        
        require(adabag)
        require(ada)
        set.seed(8675309)
        
        control <- rpart.control(maxdepth = 2,cp = -1, minsplit = 0)
        ada_fit234 <- ada(target~.-id,
                          data=td_gr234, iter = 1000, type = "real", nu = 0.001,
                          bag.frac = 1, model.coef = FALSE, control = control)
        
        ada_fit234 <- train(target~.-id,
                            data=td_gr234,
                            method="ada",control=rpart.control
                            (maxdepth=30, cp=0.010000, minsplit=20, xval=10), iter=500)
        
        ada_fit234 <- train(target~.-id,
                            data=td_gr234,
                            method="ada",control=rpart.control
                            (maxdepth=30, cp=0.010000, minsplit=20, xval=10), iter=500)
        
        ada_fit234 <- train(target~.-id,
                            data=td_gr234,
                            method="ada",control=rpart.control
                            (maxdepth=30, cp=0.010000, minsplit=20, xval=10), iter=500)
        #         ,
#                             tuneGrid= ada_grid,
#                             trControl = cv_control,
#                             train.fraction=0.67
#         )
        
        predict_234<-predict(ada_fit234, newdata=eval_gr234, type="prob")
        predict_234<-as.data.frame(predict_234)
        
        predict_234_ada<-predict_234
    
        predict_234_single<- as.factor(colnames(predict_234)[max.col(predict_234)])
        
        print(table(predict_234_single, eval_gr234$target))
        
        eval_gr234<-droplevels(eval_gr234)
        
        ## let's do some examination
        eval_temp<-eval_gr234
        eval_temp<-cbind(eval_temp, predict_234_single)
        misses<-eval_temp[eval_temp$target!=predict_234_single,]
        hits<-eval_temp[eval_temp$target==predict_234_single,]
        
  
        

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
        
