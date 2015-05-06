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
        
        
        
### TRY GBM2
        ## USE {gbm} PACKAGE
        
        ## from base above increase number of trees and reduce shrinkage by facgor of two.
        
        library(gbm)
        
        #train_control<-tree.control(nobs=dim(train_data)[1], mindev=0.01/2)
        #train_control<-tree.control(nobs=dim(train_data)[1], mindev=0.01/2)
        ## renumber rows
        train<-train_data
        
        set.seed(8675309)
        gbm_fit<-gbm(target~.-id, data=train, 
                     n.trees = 200,
                     distribution = "multinomial",
                     shrinkage=0.025,
                     interaction.depth=2,       ## interaction depth of 2 (normally between 1 and 3)
                     train.fraction=0.5,
                     keep.data=TRUE,
                     verbose=TRUE,
                     cv.folds=3,                ## 3-fold cv 
                     n.cores=1)                 ## avoid annoying bugs
        
        ## summary of the tree
        
        fitsum <- summary(gbm_fit)
        plot(fitsum)
        
        plot(gbm_fit, i.var=11)
        ## plot and label
        names(gbm_fit)
        
        gbm_plot<-as.data.frame(cbind(gbm_fit$train.error, gbm_fit$valid.error))
        colnames(gbm_plot)<-c("train.error", "valid.error")
        gbm_plot$iteration<-1:gbm_fit$n.tree
        
        
        p<-ggplot(gbm_plot, aes(x=iteration, y = train.error))+geom_line()
        p<-p+geom_line(aes(x=iteration, y =valid.error), color="red")
        
        print(p)
        # can see a slight divergence of training adn test
        
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
        
### TRY GBM3
        ## USE {gbm} PACKAGE
        
        ## from base above increase number of trees by factor of two and restore shrinkage .
        
        library(gbm)
        
        #train_control<-tree.control(nobs=dim(train_data)[1], mindev=0.01/2)
        #train_control<-tree.control(nobs=dim(train_data)[1], mindev=0.01/2)
        ## renumber rows
        train<-train_data
        
        set.seed(8675309)
        gbm_fit<-gbm(target~.-id, data=train, 
                     n.trees = 200,
                     distribution = "multinomial",
                     shrinkage=0.05,
                     interaction.depth=2,       ## interaction depth of 2 (normally between 1 and 3)
                     train.fraction=0.5,
                     keep.data=TRUE,
                     verbose=TRUE,
                     cv.folds=3,                ## 3-fold cv 
                     n.cores=1)                 ## avoid annoying bugs
        
        ## summary of the tree
        
        fitsum <- summary(gbm_fit)
        plot(fitsum)
        
        plot(gbm_fit, i.var=11)
        ## plot and label
        names(gbm_fit)
        
        gbm_plot<-as.data.frame(cbind(gbm_fit$train.error, gbm_fit$valid.error))
        colnames(gbm_plot)<-c("train.error", "valid.error")
        gbm_plot$iteration<-1:gbm_fit$n.tree
        
        
        p<-ggplot(gbm_plot, aes(x=iteration, y = train.error))+geom_line()
        p<-p+geom_line(aes(x=iteration, y =valid.error), color="red")
        
        print(p)
        # can see a slight divergence of training adn test
        
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
        
        cat(accuracy)   ## 75.46%
        ## significant misclassification of class_2
        
#         td_predict Class_1 Class_2 Class_3 Class_4 Class_5 Class_6 Class_7 Class_8 Class_9
#         Class_1     237       2       5       0       0      21      24      45      51
#         Class_2      81    4598    1695     501      35      91     185      85     106
#         Class_3       7     568     912     107       4      17      69      15       3
#         Class_4       0      21      20     219       0       1       6       0       2
#         Class_5       2      16       1       7     885       1       2       3       3
#         Class_6      50      15       3      36       1    4386      65      74      78
#         Class_7      27      47      60      19       0      50     524      26       9
#         Class_8     116      11      13       4       0      97      65    2452      98
#         Class_9     140       9       7       2       0      72       6      59    1352

        
        ## a significant improvement over tree 
        
        
### TRY GBM4
        ## USE {gbm} PACKAGE
        
        ## from base above increase number of trees to 400 of two and keep shrinkage = 0.05
        ## also decrease depth to 1
        
        library(gbm)
        
        #train_control<-tree.control(nobs=dim(train_data)[1], mindev=0.01/2)
        #train_control<-tree.control(nobs=dim(train_data)[1], mindev=0.01/2)
        ## renumber rows
        train<-train_data
        
        set.seed(8675309)
        gbm_fit<-gbm(target~.-id, data=train, 
                     n.trees = 400,
                     distribution = "multinomial",
                     shrinkage=0.05,
                     interaction.depth=1,       ## interaction depth of 2 (normally between 1 and 3)
                     train.fraction=0.5,
                     keep.data=TRUE,
                     verbose=TRUE,
                     cv.folds=3,                ## 3-fold cv 
                     n.cores=1)                 ## avoid annoying bugs
        
        ## summary of the tree
        
        fitsum <- summary(gbm_fit)
        plot(fitsum)
        
        plot(gbm_fit, i.var=11)
        ## plot and label
        names(gbm_fit)
        
        gbm_plot<-as.data.frame(cbind(gbm_fit$train.error, gbm_fit$valid.error))
        colnames(gbm_plot)<-c("train.error", "valid.error")
        gbm_plot$iteration<-1:gbm_fit$n.tree
        
        
        p<-ggplot(gbm_plot, aes(x=iteration, y = train.error))+geom_line()
        p<-p+geom_line(aes(x=iteration, y =valid.error), color="red")
        
        print(p)
        # can see a slight divergence of training adn test
        
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
        
        cat(accuracy)   ## 75.46%
        
        ## a significant improvement over tree 
        
        
        
        
        
        
        
        
        
        
        