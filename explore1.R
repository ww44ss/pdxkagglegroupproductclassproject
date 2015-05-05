## PDX DATA SCIENCE KAGGLE PROJECT
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

## PRINT THE DIMENSIONS OF THE TRAINING DATA
        ## ensure proper read of data
        
        print(dim(train_data))
   
## SAMPLE DATA FOR QUICK LOOKS
        ##
        ## if sample_data == TRUE then creates a sample of overall data
        sample_data<-TRUE
        
        set.seed(8675309)    
        if (sample_data == TRUE){
                feats<-colnames(train_data)
                feats<-feats[3:93]
        
                        sample_rows <- sample(1:dim(train_data)[1], 2*dim(train_data)[1]/3)
                        #sample_columns<-c("id","feat_1", sample(feats, 28), "feat_93", "target")
        
                        train_data_T <- train_data[sample_rows, ] #sample_columns]
                        
                        test_data<-train_data[-sample_rows,]
                        dim(train_data_T)
        
                        ## assign subset to data frame name temporarily
        
                        train_data<-train_data_T ## remove this for full analysis
        }    
        
## DATA MUNGING
        ## convert from wide to long format
        require(tidyr)
        ## use tidyr package to munge the data into a long format with the feature id as a feature variable
        long_train<-gather(train_data, feature, data, feat_1:feat_93)
        
        print(head(long_train,12))
        
              
        ## summarize using ddply to get means and standard deviations
        train_morph<-ddply(long_train, c("target", "feature"), summarize, mean_data = mean(data), sdev_data = sqrt(var(data)))
        ## calculate the CV
        train_morph$CV<-train_morph$sdev_data/(train_morph$mean_data+.00001)
        
        #head(train_morph)
        #str(train_morph)

## INFORMATIONAL PLOTS
        
        p<-ggplot(train_morph, aes(x=target, y=mean_data, color=feature))+geom_point()+ theme_bw()
        p <- p + ggtitle("means of several features versus class")
        p <- p + guides(color=guide_legend(nrow=15))
        print(p)
        
## TRY GETTING RID OF ZERO DATA
        
        long_train_nz <- long_train[long_train$data>0,]
        
        
        ## summarize using ddply to get means and standard deviations
        train_morph_nz<-ddply(long_train_nz, c("target", "feature"), summarize, mean_data = mean(data), sdev_data = sqrt(var(data)))
        ## calculate the CV
        train_morph_nz$CV<-train_morph_nz$sdev_data/(train_morph_nz$mean_data+.00001)
        
        head(train_morph_nz)
        str(train_morph_nz)
        
        ## INFORMATIONAL PLOTS
        
        p<-ggplot(train_morph_nz, aes(x=target, y=mean_data, color=feature))+geom_point()+ theme_bw()
        p <- p + ggtitle("means of features nz versus class")
        p <- p + guides(color=guide_legend(nrow=20))
        print(p)
        
        
        ## plot the coefficient of variation (CV)
        
        p<-ggplot(train_morph_nz, aes(x=target, y=CV, color=feature))+geom_point()+ theme_bw()
        p <- p + ggtitle("CV features nz versus class")
        p <- p + guides(color=guide_legend(nrow=20))
        print(p)
        
        p<-ggplot(train_morph, aes(x=target, y=CV, color=feature))+geom_point()+ theme_bw()
        p <- p + ggtitle("CV features nz versus class")
        p <- p + guides(color=guide_legend(nrow=15))
        print(p)
        
        ## can see quite a difference
        
## TREE MODEL
        
## USE {rpart} PACKAGE
        
        library(rpart)
        ###
        fit<-rpart(target~.-target-id, method="class", 
                   data=train_data)
        
        plot(fit)
        

        plot(fit, uniform=TRUE, 
             main="Classification Tree for Product Class")
        text(fit, use.n=TRUE, all=TRUE, cex=.5)  

## USE {tree} PACKAGE
        
        library(tree)
        
        train_control<-tree.control(nobs=dim(train_data)[1], mindev=0.01/2)
        
        train_control
        
        tree_fit<-tree(target~.-id, method="class", 
                       data=train_data, control = train_control)
        
        ## summary of the tree
        
        summary(tree_fit)
        
        ## plot and label
        plot(tree_fit)
        text(tree_fit, pretty=0)
        
        ## make sure the test data is there 
        #head(test_data)

        td<-test_data
        td$id<-NULL
        td$target<-NULL
        
        td<-td[1:10,]
        
        predicted_classification<-predict(tree_fit, data=td, type="class")
        
        
        table(predicted_classification)
        table(test_data$target)
        
        table(predicted_classification, test_data$id)
        
        length(test_data$target)
        length(predicted_classification)
        
        
        ## shows substantial error
        
## look at CV Tree for pruning
        
        set.seed(8675309)
        cv_tree_fit <- cv.tree(tree_fit, FUN=prune.misclass)
        
        names(cv_tree_fit)
        
        plot(cv_tree_fit$size, cv_tree_fit$dev, type="b")
        
 
## Let's check the confusion matrix
        
        table(predicted_classification, test_data$target)
        