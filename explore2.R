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
        
        cat(dim(train_data))
   
## SAMPLE DATA FOR QUICK LOOKS
        ##
        ## if sample_data == TRUE then creates a sample of overall data
        sample_data<-TRUE
        
        set.seed(8675309)
        if (sample_data == TRUE){
                sample_rows <- sample(1:dim(train_data)[1], 2*dim(train_data)[1]/3)
                training<-train_data[sample_rows,]
                testing<-train_data[-sample_rows,]
                
                train_data<-training
                test_data<-testing
        }

## DATA MUNGING
        ## convert from wide to long format
        ## useful for plotting etc.
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
        
## TREE MODELS
## Let's start predicting
        

## USE {tree} PACKAGE
        
        library(tree)
        
        #train_control<-tree.control(nobs=dim(train_data)[1], mindev=0.01/2)
        train_control<-tree.control(nobs=dim(train_data)[1], mindev=0.01/2)
        ## renumber rows
        row.names(train_data) <- 1:nrow(train_data)
        
        tree_fit<-tree(target~.-id, method="class", 
                       data=train_data)#, control = train_control)
        
        ## summary of the tree
        
        summary(tree_fit)
        
        ## plot and label
        plot(tree_fit)
        text(tree_fit, pretty=0)
        
        ## make sure the test data is there 
        # head(test_data)

        td<-test_data
  
        predicted_classification<-predict(tree_fit, newdata=td, type="class")
        
        table(predicted_classification, test_data$target)
        
        check<-table(predicted_classification ==test_data$target)
        cat(check)
        
        accuracy<-1-check[1]/(check[1]+check[2])
        
        cat(accuracy)   ## 58%
        
        ## not verry good. 
        ## missing several rows (Class_1, 3, 4, 7)
        
## TRY TWO SEPARATE TREES
        
        train<-train_data
        train_a<-train[train$target=="Class_2"|train$target=="Class_5"|train$target=="Class_6"|train$target=="Class_8"|train$target=="Class_9",]
        train_b<-train[train$target=="Class_1"|train$target=="Class_3"|train$target=="Class_4"|train$target=="Class_7",]
        
        tree_a<-tree(target~.-id, method="class", 
                       data=train_a)
        tree_b<-tree(target~.-id, method="class", data=train_b)
        
        
        plot(tree_b)
        text(tree_b, pretty=0)
        
        test<-test_data
        test_a<-test[test$target=="Class_2"|test$target=="Class_5"|test$target=="Class_6"|test$target=="Class_8"|test$target=="Class_9",]
        test_b<-test[test$target=="Class_1"|test$target=="Class_3"|test$target=="Class_4"|test$target=="Class_7",]
        
        
        predict_a<-predict(tree_a, newdata=test_a, type="class")
        predict_b<-predict(tree_b, newdata=test_b, type="class")
        
        head(predict_a)
        head(predict_b)
        
        table(test_a$target, predict_a)
        table(test_b$target, predict_b)
        
        ## put everything back together 
        ## this is a little convoluted
        
        result_a<-cbind(id=test_a$id, target=as.data.frame(predict_a))
        colnames(result_a)<-c("id", "target")
        result_b<-cbind(test_b$id, as.data.frame(predict_b))
        colnames(result_b)<-c("id", "target")
        result<-rbind(result_a, result_b)
        result<-result[order(result[,1]),]
        
        table(test_data$target, result$target)
        
        check<-table(test_data$target==result$target)
        cat(check)
        
        accuracy<-1-check[1]/(check[1]+check[2])
        
        cat(accuracy)   ## 78%
        
        ## a big improvement
        
### LETS TRY ONE MORE DISCREET BREAKUP
        train<-train_data
        train_a<-train[train$target=="Class_5"|train$target=="Class_6"|train$target=="Class_8"|train$target=="Class_9",]
        train_b<-train[train$target=="Class_3"|train$target=="Class_4"|train$target=="Class_7",]
        train_c<-train[train$target=="Class_1"|train$target=="Class_2",]
        
        tree_a<-tree(target~.-id, method="class", 
                     data=train_a)
        tree_b<-tree(target~.-id, method="class", data=train_b)
        tree_c<-tree(target~.-id, method="class", data=train_c)
        
        
        plot(tree_c)
        text(tree_c, pretty=0)
        
        test<-test_data
        test_a<-test[test$target=="Class_5"|test$target=="Class_6"|test$target=="Class_8"|test$target=="Class_9",]
        test_b<-test[test$target=="Class_3"|test$target=="Class_4"|test$target=="Class_7",]
        
        test_c<-test[test$target=="Class_1"|test$target=="Class_2",]
        
        
        predict_a<-predict(tree_a, newdata=test_a, type="class")
        predict_b<-predict(tree_b, newdata=test_b, type="class")
        predict_c<-predict(tree_c, newdata=test_c, type="class")
        
        head(predict_a)
        head(predict_b)
        head(predict_c)
        
        table(test_a$target, predict_a)
        table(test_b$target, predict_b)
        table(test_c$target, predict_c)
        
        ## put everything back together 
        ## this is a little convoluted
        
        result_a<-cbind(id=test_a$id, target=as.data.frame(predict_a))
        colnames(result_a)<-c("id", "target")
        result_b<-cbind(test_b$id, as.data.frame(predict_b))
        colnames(result_b)<-c("id", "target")
        result_c<-cbind(test_c$id, as.data.frame(predict_c))
        colnames(result_c)<-c("id", "target")
        result<-rbind(result_a, result_b, result_c)
        result<-result[order(result[,1]),]
        
        table(test_data$target, result$target)
        
        check<-table(test_data$target==result$target)
        cat(check)
        
        accuracy<-1-check[1]/(check[1]+check[2])
        
        cat(accuracy)   ## 85%
        
        ## False accuracy as i am presrting the outcome. 
        
        
### LETS GROUPING
        train<-train_data
        
        train$target<-gsub("Class_1", "Class_1347", train$target)
        train$target<-gsub("Class_2", "Class_25689", train$target)
        train$target<-gsub("Class_3", "Class_1347", train$target)
        train$target<-gsub("Class_4", "Class_1347", train$target)
        train$target<-gsub("Class_5", "Class_25689", train$target)
        train$target<-gsub("Class_6", "Class_25689", train$target)
        train$target<-gsub("Class_7", "Class_1347", train$target)
        train$target<-gsub("Class_8", "Class_25689", train$target)
        train$target<-gsub("Class_9", "Class_25689", train$target)
        
        train$target<-as.factor(train$target)
        
        tree_a<-tree(target~.-id, method="class", data=train,mindev=0.01/2)
        
        plot(tree_a)
        text(tree_a, pretty=0)
        
        test_a<-test_data
        
        test_a$target<-gsub("Class_1", "Class_1347", test_a$target)
        test_a$target<-gsub("Class_2", "Class_25689", test_a$target)
        test_a$target<-gsub("Class_3", "Class_1347", test_a$target)
        test_a$target<-gsub("Class_4", "Class_1347", test_a$target)
        test_a$target<-gsub("Class_5", "Class_25689", test_a$target)
        test_a$target<-gsub("Class_6", "Class_25689", test_a$target)
        test_a$target<-gsub("Class_7", "Class_1347", test_a$target)
        test_a$target<-gsub("Class_8", "Class_25689", test_a$target)
        test_a$target<-gsub("Class_9", "Class_25689", test_a$target)
        
        
        
        predict_a<-predict(tree_a, newdata=test_a, type="class")
        
        
        table(test_a$target, predict_a)

        
        ## THis produces
        

        check<-table(test_a$target==predict_a)
        cat(check)
        
        accuracy<-1-check[1]/(check[1]+check[2])
        cat(round(accuracy*100,3))
        
        ## this approach got 74% into 25689
        
     
##TRY SOME FEATURE ENGINEERING
        
        ### Since feat_11, 43 and 60 all indicate way from 25689 let's add
        train<-train_data
        
        ## combined the features
        feat_agg1<-train$feat_11+train$feat_43
        ## Null out individuals
        train$feat_11<-NULL
        train$feat_43<-NULL
        #train$feat_60<-NULL
        train<-cbind(train, feat_agg1)
        
        train$target<-gsub("Class_1", "Class_1347", train$target)
        train$target<-gsub("Class_2", "Class_25689", train$target)
        train$target<-gsub("Class_3", "Class_1347", train$target)
        train$target<-gsub("Class_4", "Class_1347", train$target)
        train$target<-gsub("Class_5", "Class_25689", train$target)
        train$target<-gsub("Class_6", "Class_25689", train$target)
        train$target<-gsub("Class_7", "Class_1347", train$target)
        train$target<-gsub("Class_8", "Class_25689", train$target)
        train$target<-gsub("Class_9", "Class_25689", train$target)
        
        train$target<-as.factor(train$target)
        
        tree_a<-tree(target~.-id, method="class", data=train,mindev=0.01/2)
        
        plot(tree_a)
        text(tree_a, pretty=0)
        
        test_a<-test_data
        
        ## combined the features
        feat_agg1<-test_a$feat_11+test_a$feat_43
        ## Null out individuals
        test_a$feat_11<-NULL
        test_a$feat_43<-NULL
        #test_a$feat_60<-NULL
        test_a<-cbind(test_a, feat_agg1)
        
        
        test_a$target<-gsub("Class_1", "Class_1347", test_a$target)
        test_a$target<-gsub("Class_2", "Class_25689", test_a$target)
        test_a$target<-gsub("Class_3", "Class_1347", test_a$target)
        test_a$target<-gsub("Class_4", "Class_1347", test_a$target)
        test_a$target<-gsub("Class_5", "Class_25689", test_a$target)
        test_a$target<-gsub("Class_6", "Class_25689", test_a$target)
        test_a$target<-gsub("Class_7", "Class_1347", test_a$target)
        test_a$target<-gsub("Class_8", "Class_25689", test_a$target)
        test_a$target<-gsub("Class_9", "Class_25689", test_a$target)
        
        
        
        predict_a<-predict(tree_a, newdata=test_a, type="class")
        
        
        table(test_a$target, predict_a)
        
        
        ## THis produces
        
        
        check<-table(test_a$target==predict_a)
        cat(check)
        
        accuracy<-1-check[1]/(check[1]+check[2])
        cat(round(accuracy*100,3))
        
        ## this approach got 76% into 25689 but is unsatisfactory overall.
        
### TRY GBM
        ## USE {gbm} PACKAGE
        
        library(gbm)
        
        #train_control<-tree.control(nobs=dim(train_data)[1], mindev=0.01/2)
        #train_control<-tree.control(nobs=dim(train_data)[1], mindev=0.01/2)
        ## renumber rows
        train<-train_data
        
        set.seed(8675309)
        gbm_fit<-gbm(target~.-id, data=train, 
                     n.trees = 100,
                     distribution = "multinomial",
                     shrinkage=0.05,
                     interaction.depth=2,
                     train.fraction=0.5,
                     keep.data=TRUE,
                     verbose=TRUE,
                     cv.folds=3,                ## 3-fold cv 
                     n.cores=1)
        
        ## summary of the tree
        
        fitsum <- summary(gbm_fit)
        
        plot(gbm_fit, i.var=59)
        ## plot and label
        names(gbm_fit)
        
        ## make sure the test data is there 
        # head(test_data)
        
        td<-test_data
        
        td_model<-predict(gbm_fit, newdata=td, type="response")
        
        td_names<-colnames(td_model)
        td_model<-as.data.frame(td_model)
        colnames(td_model)<-td_names
        td_predict <- as.factor(colnames(td_model)[max.col(td_model)])
        
        table(td_predict, td$target)
        
        check<-table(td_predict ==test_data$target)
        cat(check)
        
        accuracy<-1-check[1]/(check[1]+check[2])
        
        cat(accuracy)   ## 73%
        ## a significant improvement over tree 
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        