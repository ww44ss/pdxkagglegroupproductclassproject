## PDX DATA SCIENCE KAGGLE PROJECT
## Program thread 5
## In this thread continues RF and GBM combos

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
        cat("the dimensions of loaded train_data are: ",dim(train_data)[1], " rows X ", dim(train_data)[2], "columns")

## SAMPLE DATA FOR QUICK LOOKS


sample_data<-TRUE

set.seed(8675309)
if (sample_data == TRUE){
        sample_rows <- sample(1:nrow(train_data), size=0.6*nrow(train_data))
        td<-train_data[sample_rows,]
        train_data2<-train_data[-sample_rows,]
        
        ## create a second smaller sample for intermediate evaluation
        sample_rows2<- sample(1:nrow(train_data2), size=0.75*nrow(train_data2))
        train_data2<-train_data2[sample_rows2,]
        eval_data<-train_data2[-sample_rows2,]
        
        train_data<-td
        
}

        cat("the dimensions of train_data are: ",dim(train_data)[1], " rows X ", dim(train_data)[2], "columns")
        cat("the dimensions of train_data2 are: ",dim(train_data2)[1], " rows X ", dim(train_data2)[2], "columns")
        cat("the dimensions of eval_data are: ",dim(eval_data)[1], " rows X ", dim(eval_data)[2], "columns")

## RANDOM FOREST


        ## assign td
        td<-train_data

        ## Random forest Model
        set.seed(8765309)
        rf_model <- randomForest(target~.-id, data=td, importance=TRUE, ntree=100, nodesize=10)

                ## test results against eval_data
                predict_rf_input<-eval_data
                rf_predict<-predict(rf_model, newdata=predict_rf_input, type="prob")
                rf_predict<-as.data.frame(rf_predict)
                        temp_predict<-rf_predict
                
                rf_predict<- as.factor(colnames(rf_predict)[max.col(rf_predict)])

                table(rf_predict, eval_data$target)
                check<-table(rf_predict==eval_data$target)
                accuracy<-1-check[1]/(check[1]+check[2])

                cat("accuracy of rf is ", round(100*accuracy,2), "%")

                ## accuracy of 79.7%


### GBM

        ## assign train data
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

        gbm_fit<-gbm(target~.,data=gbm_input, 
                     n.trees = 100,
                     distribution = "multinomial",
                     shrinkage=0.05,
                     interaction.depth=2,
                     train.fraction=0.5,
                     keep.data=TRUE,
                     verbose=TRUE,
                     cv.folds=3,                ## 3-fold cv 
                     n.cores=1)
#       gbm_fit<-gbm.fit(target~.,data=gbm_input 
#            n.trees = 200,
#            distribution = "multinomial",
#            shrinkage=0.1,
#            interaction.depth=2,       ## interaction depth of 1 (normally between 1 and 3)
#             bag.fraction=0.5,        ## out of 93 predictors select 3 to get weaker predictors
#             nTrain=2,
#            keep.data=TRUE,
#            verbose=TRUE,
#             #cv.folds=2,                ## 2-fold cv 
#             #n.cores=1                 ## avoid annoying bugs
#        )

#code from http://stackoverflow.com/questions/8722247/r-caret-and-gbm-cant-find-ntrees-input
#gbmGrid <- expand.grid(.interaction.depth = (1:5) * 2, .n.trees = (1:5)*50, .shrinkage = .1)
#+ bootControl <- trainControl(number = 1)#
#gbmFit <- train(prePrior1[,-c(2,60,61,161)], trainClass, method = "gbm", tuneLength = 5,
#+ trControl = bootControl
#+ ##, scaled = FALSE
#        + , tuneGrid = gbmGrid 
#+ )

        ## summary of the tree

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

        cat("the dimensions of the eval data are ", nrow(td), " X ", ncol(td))

        ##use to predict output

        ## fitrst run rf model
        rf_predicted<-predict(rf_model, newdata=td, type="prob")
        rf_predicted<-as.data.frame(rf_predicted)
        ## then run gbm model
        gbm_rf_predicted<-predict(gbm_fit, newdata=rf_predicted, type='response')
        gbm_rf_predicted<-as.data.frame(gbm_rf_predicted)

        cat("the dimesions of the rf_predictions are ", nrow(gbm_rf_predicted), " X ", ncol(gbm_rf_predicted))

        cat("and the first few rows are")
        head(gbm_rf_predicted,5)

## Evaluate rf_predicted
        eval_predict <- as.factor(colnames(rf_predicted)[max.col(rf_predicted)])
        table(eval_predict, eval_data$target)

# eval_predict Class_1 Class_2 Class_3 Class_4 Class_5 Class_6 Class_7 Class_8 Class_9
# Class_1      41       1       0       0       0       0       3       5       5
# Class_2      11    1028     318      90       4      11      31       8      12
# Class_3       0     102     274      25       0       2      11       2       0
# Class_4       0       4       4      65       0       1       2       0       0
# Class_5       0       1       0       0     182       1       0       0       0
# Class_6      19       6       0       7       2    1021      12      14      17
# Class_7       5       9       6       2       0       6     112       3       1
# Class_8      38       7       4       1       0      21      20     603      22
# Class_9      39       1       1       0       0       5       0       4     312

eval_predict <- as.factor(colnames(gbm_rf_predicted)[max.col(gbm_rf_predicted)])
table(eval_predict, eval_data$target)

# eval_predict  Class_1 Class_2 Class_3 Class_4 Class_5 Class_6 Class_7 Class_8 Class_9
# Class_1.100      78       1       0       1       0       3       6      14       9
# Class_2.100       8     961     246      55       4       9      17       4      10
# Class_3.100       0     146     331      37       0       3      17       6       1
# Class_4.100       0      20      14      88       0       3       3       0       0
# Class_5.100       0       1       0       0     183       1       0       0       0
# Class_6.100      14       4       1       6       1    1014       9      12      14
# Class_7.100       6      18      11       2       0      10     128       4       1
# Class_8.100      18       5       4       1       0      20      11     594      20
# Class_9.100      29       3       0       0       0       5       0       5     314

    ## fix levels and column names
        levels(eval_predict)<-gsub(".100", "", levels(eval_predict))

        check<-table(eval_predict==eval_data$target)
        accuracy<-1-check[1]/(check[1]+check[2])

        cat("accuracy of rf is ", round(100*accuracy,2), "%")

        ## improved to 80.9%



##MAKE SUBMISSION

        ## get the actual test data
        directory<- "/Users/winstonsaunders/Documents/pdxkagglegroupproductclassproject/"
        file_name<- "test.csv"

        submission_test_data<-read.csv(paste0(directory,file_name))
        ##just make sure its real
        print(head(submission_test_data))
        ## run the prediction
        
        td<-submission_test_data
        
        rf_predicted<-predict(rf_model, newdata=td, type="prob")
        rf_predicted<-as.data.frame(rf_predicted)
        ## then run gbm model
        gbm_rf_predicted<-predict(gbm_fit, newdata=rf_predicted, type='response')
        gbm_rf_predicted<-as.data.frame(gbm_rf_predicted)
        
        cat("the dimesions of the rf_predictions are ", nrow(gbm_rf_predicted), " X ", ncol(gbm_rf_predicted))
        
        cat("and the first few rows are")
        head(gbm_rf_predicted,5)
        
        submission<-gbm_rf_predicted
        
        ## convert to data frame format
        submission<-as.data.frame(submission)
        
        ## clean up the numbers
        submission<-round(10000*submission,0)/10000.

        ## add ids back
        submission<-cbind("id"=as.integer(submission_test_data$id), submission)
        #submission<-as.data.frame(submission)
        submission$id<-as.integer(submission$id)
        ## get rid of scientiic notation
        options(scipen=10)
        ## check dimensions and data
        dim(submission)
        head(submission)
        tail(submission)

        ## write csv
        write.csv(submission, paste0(directory,"May082",".csv"), row.names=F, quote=F)

