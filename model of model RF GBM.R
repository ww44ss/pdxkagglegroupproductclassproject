## Model of Model RF GBM 
## Here's an interesting approach from the kaggle website.
## from https://www.kaggle.com/users/54720/rudi-kruger/otto-group-product-classification-challenge/rf-gbm
## Rather than simple "average" look at "model of model" approach...
##
## Since I have essentially already developed all the machinery underneath, let's try it.

## GET TRAINING DATA

        directory<- "/Users/winstonsaunders/Documents/pdxkagglegroupproductclassproject/"
        file_name<- "train.csv"
        
        ## read the .csv
        train_data<-read.csv(paste0(directory,file_name))
        
        ## check the data
        print(head(train_data))

## LOAD REQUIRED PACKAGES

require(plyr)
require(ggplot2)
require(tidyr)
require(gbm)
require(randomForest)



## SPLIT THE TRAIN DATA INTO RF AND GBM PORTIONS
        ##
        ## if sample_data == TRUE then creates a sample of overall data
        sample_data<-TRUE

        ## create sample datasets
        set.seed(8675309)
        if (sample_data == TRUE){
                ## 2/3 1/3 split of data
                sample_rows <- sample(1:dim(train_data)[1], 2*dim(train_data)[1]/3)
                ## 
                train_data<-train_data[sample_rows,]
                train2_data<-train_data[-sample_rows,]
        }

## RANDOM FOREST (use {randomForest} package)

        ## assign train_rf data set
        train_rf<-train_data

        ## model
        td_rf_model <- randomForest(target~.-id, data=train_rf, importance=TRUE, ntree=200, nodesize=10)

        ## predict with second data set
        predict_rf_input<-train2_data
        rf_predict<-predict(td_rf_model, newdata=predict_rf_input, type="prob")

## GBM (use {gbm} package)

        rf_predict<-train2_data

        set.seed(8675309)
        gbm_fit<-gbm(target~.-id, data=rf_predict, 
             n.trees = 2000,
             distribution = "multinomial",
             shrinkage=0.03,
             interaction.depth=2,       ## interaction depth of 1 (normally between 1 and 3)
             train.fraction=0.5,
             bag.fraction=0.03,
             keep.data=TRUE,
             verbose=TRUE,
             cv.folds=3,                ## 3-fold cv 
             n.cores=1                  ## avoid annoying bugs
        )                 


## summary of the tree

        fitsum <- summary(gbm_fit)
        plot(fitsum)

## Plot GBM

        ## create plot data
        gbm_plot<-as.data.frame(cbind(gbm_fit$train.error, gbm_fit$valid.error))
        colnames(gbm_plot)<-c("train.error", "valid.error")
        gbm_plot$iteration<-1:gbm_fit$n.tree

        ## show train.error and cv.error
        p<-ggplot(gbm_plot, aes(x=iteration, y = train.error))+geom_line()
        p<-p+geom_line(aes(x=iteration, y =valid.error), color="red")

        print(p)

## use model to predict competition test data

        ## get the actual test data
        directory<- "/Users/winstonsaunders/Documents/pdxkagglegroupproductclassproject/"
        file_name<- "test.csv"

        competition_test_data<-read.csv(paste0(directory,file_name))

        ## run the prediction
        competition_prediction<-predict(gbm_fit, newdata=competition_test_data, type="response")
        
        ## get the data
        a<-competition_prediction[,,1]
        a<-as.data.frame(a)
        submission<-a

        ## clean up the numbers
        submission<-round(1000*submission,0)/1000.

        ## add ids back
        submission<-cbind("id"=as.integer(competition_test_data$id), submission)
        submission$id<-as.integer(submission$id)
        ## get rid of scientiic notation
        options(scipen=10)
        ## check dimensions and data
        dim(submission)
        head(submission)
        tail(submission)

        ## write csv
        write.csv(submission, paste0(directory,"May091",".csv"), row.names=F, quote=F)

        ## this is the May091 run
        ## has a kaggle score of 0.7... - horrible score

       
        
        
