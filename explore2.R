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
        ## still not over training
        
        ## make sure the test data is there 
        # head(test_data)
        
        td<-test_data
        
        td_model<-predict(gbm_fit, newdata=td, type="response")
        
        td_names<-colnames(td_model)
        td_model<-as.data.frame(td_model)
        colnames(td_model)<-td_names
        td_predict <- as.factor(colnames(td_model)[max.col(td_model)])
        
        table(td_predict, td$target)

        
#         td_predict Class_1 Class_2 Class_3 Class_4 Class_5 Class_6 Class_7 Class_8 Class_9
#         Class_1     223       1       6       0       0      22      20      47      65
#         Class_2      91    4587    1773     532      37      90     191      86     109
#         Class_3       7     582     835     124       0      15      64      13       5
#         Class_4       0      19       9     172       0       4       9       0       2
#         Class_5       3      15       2       6     888       1       2       1       2
#         Class_6      53      15       4      35       0    4375      65      73      80
#         Class_7      24      43      65      20       0      53     521      33       9
#         Class_8     121      17      16       4       0      97      68    2450     106
#         Class_9     138       8       6       2       0      79       6      56    1324
#         
        check<-table(td_predict ==test_data$target)
        cat(check)
        
        accuracy<-1-check[1]/(check[1]+check[2])
        
        cat(accuracy)   ## 74.54%
        
        ## not a significant change 

        
        
### TRY GBM5
        ## USE {gbm} PACKAGE
        
        ## keep number of trees to 400 of two and keep shrinkage = 0.05
        ## keep depth to 1
        ## bag.fraction to 0.1
        ## basis of May7 submission
        
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
                     bag.fraction=0.1,
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
        ## still not over training
        
        ## make sure the test data is there 
        # head(test_data)
        
        td<-test_data
        
        td_model<-predict(gbm_fit, newdata=td, type="response")
        
        td_names<-colnames(td_model)
        td_model<-as.data.frame(td_model)
        colnames(td_model)<-td_names
        td_predict <- as.factor(colnames(td_model)[max.col(td_model)])
        
        table(td_predict, td$target)
        
#         td_predict Class_1 Class_2 Class_3 Class_4 Class_5 Class_6 Class_7 Class_8 Class_9
#         Class_1     225       8       5       0       0      24      27      58      52
#         Class_2      85    4628    1808     535      39      87     204      83     116
#         Class_3       7     537     793     122       0      17      66      11       2
#         Class_4       0      22      15     170       0       3      10       0       1
#         Class_5       2      19       4       7     883       1       2       1       3
#         Class_6      49      12       2      39       1    4376      72      70      84
#         Class_7      28      40      66      16       0      50     499      32       9
#         Class_8     121      13      17       4       1     105      63    2446     105
#         Class_9     143       8       6       2       1      73       3      58    1330
#         
            
        check<-table(td_predict ==test_data$target)
        cat(check)
        
        accuracy<-1-check[1]/(check[1]+check[2])
        
        cat(accuracy)   ## 74.44%
        
        ## not a significant change 
 
### TRY GBM6
        ## USE {gbm} PACKAGE
        
        ## increase number of trees to 1000 of two and keep shrinkage = 0.05
        ## keep depth to 1
        ## reduce bag.fraction to 0.05
        ## 
        
        library(gbm)
        
        #train_control<-tree.control(nobs=dim(train_data)[1], mindev=0.01/2)
        #train_control<-tree.control(nobs=dim(train_data)[1], mindev=0.01/2)
        ## renumber rows
        train<-train_data
        
        set.seed(8675309)
        gbm_fit<-gbm(target~.-id, data=train, 
                     n.trees = 1000,
                     distribution = "multinomial",
                     shrinkage=0.05,
                     interaction.depth=1,       ## interaction depth of 1 (normally between 1 and 3)
                     train.fraction=0.5,
                     bag.fraction=0.05,
                     keep.data=TRUE,
                     verbose=TRUE,
                     cv.folds=3,                ## 3-fold cv 
                     n.cores=1)                 ## avoid annoying bugs
        
        ## summary of the tree
        
        fitsum <- summary(gbm_fit)
        plot(fitsum)
        
        plot(gbm_fit, i.var=11)
        ## plot and label
        #names(gbm_fit)
        
        gbm_plot<-as.data.frame(cbind(gbm_fit$train.error, gbm_fit$valid.error))
        colnames(gbm_plot)<-c("train.error", "valid.error")
        gbm_plot$iteration<-1:gbm_fit$n.tree
        
        
        p<-ggplot(gbm_plot, aes(x=iteration, y = train.error))+geom_line()
        p<-p+geom_line(aes(x=iteration, y =valid.error), color="red")
        
        print(p)
        # can see a slight divergence of training adn test
        ## still not over training
        
        ## make sure the test data is there 
        # head(test_data)
        
        td<-test_data
        
        td_model<-predict(gbm_fit, newdata=td, type="response")
        
        td_names<-colnames(td_model)
        td_model<-as.data.frame(td_model)
        colnames(td_model)<-td_names
        td_predict <- as.factor(colnames(td_model)[max.col(td_model)])
        
        table(td_predict, td$target)
        
#         td_predict Class_1 Class_2 Class_3 Class_4 Class_5 Class_6 Class_7 Class_8 Class_9
#         Class_1     211       7       3       0       0      30      33      65      47
#         Class_2      57    4504    1638     457      32      59     146      52      75
#         Class_3       3     622     945     125       0      11      67      16       6
#         Class_4       1      36      25     246       0       5      10       0       1
#         Class_5       2      17       4       7     883       1       3       0       2
#         Class_6      49      20       4      36       2    4377      78      83      88
#         Class_7      41      48      76      18       0      73     547      40      11
#         Class_8     133      14      15       4       7     102      53    2461      99
#         Class_9     163      19       6       2       1      78       9      42    1373
#         
        check<-table(td_predict ==test_data$target)
        cat(check)
        
        accuracy<-1-check[1]/(check[1]+check[2])
        
        cat(accuracy)   ## 75.37%
        ## kaggle score 0.65879
        
        ## a small improvement
        
        
### TRY GBM7 with an added RF
        ## USE {gbm} PACKAGE
        
        ## increase number of trees to 2000 of two and keep shrinkage = 0.03
        ## keep depth to 2
        ## reduce bag.fraction to 0.03
        ## 
        
        library(gbm)
        
        
        #train_control<-tree.control(nobs=dim(train_data)[1], mindev=0.01/2)
        #train_control<-tree.control(nobs=dim(train_data)[1], mindev=0.01/2)
        ## renumber rows
        train<-train_data
        
        set.seed(8675309)
        gbm_fit<-gbm(target~.-id, data=train, 
                     n.trees = 2000,
                     distribution = "multinomial",
                     shrinkage=0.03,
                     interaction.depth=2,       ## interaction depth of 1 (normally between 1 and 3)
                     train.fraction=0.5,
                     bag.fraction=0.03,
                     keep.data=TRUE,
                     verbose=TRUE,
                     cv.folds=3,                ## 3-fold cv 
                     n.cores=1)                 ## avoid annoying bugs
        
        
        
        
        
        
        ## summary of the tree
        
        fitsum <- summary(gbm_fit)
        plot(fitsum)
        
        plot(gbm_fit, i.var=11)
        ## plot and label
        #names(gbm_fit)
        
        gbm_plot<-as.data.frame(cbind(gbm_fit$train.error, gbm_fit$valid.error))
        colnames(gbm_plot)<-c("train.error", "valid.error")
        gbm_plot$iteration<-1:gbm_fit$n.tree
        
        
        p<-ggplot(gbm_plot, aes(x=iteration, y = train.error))+geom_line()
        p<-p+geom_line(aes(x=iteration, y =valid.error), color="red")
        
        print(p)
        # can see a slight divergence of training adn test
        ## still not over training
        
        ## make sure the test data is there 
        # head(test_data)
        
        td<-test_data
        
        td_model<-predict(gbm_fit, newdata=td, type="response")
        td_gbm_predict<-td_model[,,1]
        td_gbm_predict<-as.data.frame(td_gbm_predict)
        
        td_names<-colnames(td_model)
        td_model<-as.data.frame(td_model)
        colnames(td_model)<-td_names
        td_predict <- as.factor(colnames(td_model)[max.col(td_model)])
        
        table(td_predict, td$target)
        
#         td_predict Class_1 Class_2 Class_3 Class_4 Class_5 Class_6 Class_7 Class_8 Class_9
#         Class_1     265       7       5       2       0      29      35      75      67
#         Class_2      45    4348    1408     352      29      53     112      38      60
#         Class_3       3     697    1135     143       1       3      70       8       3
#         Class_4       1     106      68     336       2      11      17       0       3
#         Class_5       3      23       6       5     884       0      11       4       5
#         Class_6      54      19       4      32       2    4363      82      76      73
#         Class_7      23      51      64      20       0      82     560      37      10
#         Class_8     114      21      18       2       6     102      51    2473      94
#         Class_9     152      15       8       3       1      93       8      48    1387
#         
        check<-table(td_predict ==test_data$target)
        cat(check)
        
        accuracy<-1-check[1]/(check[1]+check[2])
        
        cat(accuracy)   ## 76.37%
        ## kaggle score 0.62881
        
        ## a small improvement 
        
        ## Add a Random Forest
        
        library(randomForest)
        
        td_rf_model <- randomForest(target~.-id, data=train, importance=TRUE, ntree=200, nodesize=10)
        
        td_rf_predict<-predict(td_rf_model, newdata=td, type="prob")
        
        
        ## average the two models
        average_predict<-(td_rf_predict+td_gbm_predict)/2
        ## pick the max probability
        average_predict_single <- as.factor(colnames(average_predict)[max.col(average_predict)])
        
        table(average_predict_single, td$target)
        
#         average_predict_single 
#                 Class_1 Class_2 Class_3 Class_4 Class_5 Class_6 Class_7 Class_8 Class_9
#         Class_1     224       0       1       0       0      13      21      28      19
#         Class_2      48    4713    1608     460      29      70     174      56      65
#         Class_3       2     478    1026     110       0       5      48       7       3
#         Class_4       0      14      18     264       0       1       6       0       0
#         Class_5       2      13       3       6     890       0       4       2       2
#         Class_6      56      14       3      37       3    4437      69      86      71
#         Class_7      19      38      39      15       0      50     536       9       9
#         Class_8     145      11      14       2       0      96      80    2530      86
#         Class_9     164       6       4       1       3      64       8      41    1447
#        
#       Note this still does not fix the class_2 class_3 mixup
        
        check<-table(average_predict_single ==td$target)
        cat(check)
        
        accuracy<-1-check[1]/(check[1]+check[2])
        
        cat(accuracy)
        
##MAKE SUBMISSION
        
        ## get the actual test data
        directory<- "/Users/winstonsaunders/Documents/pdxkagglegroupproductclassproject/"
        file_name<- "test.csv"
        
        real_test_data<-read.csv(paste0(directory,file_name))
        ##just make sure its real
        print(head(real_test_data))
        ## run the prediction
        gbm_real_test_model<-predict(gbm_fit, newdata=real_test_data, type="response")
        rf_real_test_model<-predict(td_rf_model, newdata=real_test_data, type="prob")
        ## get the data
        a<-gbm_real_test_model[,,1]
        a<-as.data.frame(a)
        b<-rf_real_test_model
        submission<-(a+b)/2
        submission<-as.data.frame(submission)
        ## clean up the numbers
        submission<-round(10000*submission,0)/10000.
        
        ## add ids back
        submission<-cbind("id"=as.integer(real_test_data$id), submission)
        submission<-as.data.frame(submission)
        submission$id<-as.integer(submission$id)
        options(scipen=10)
        dim(submission)
        head(submission)
        tail(submission)
        
        ## write csv
        write.csv(submission, paste0(directory,"May074",".csv"), row.names=F, quote=F)
        
        
#############################################
### TRY GBM8 with an added RF
        ## USE {gbm} PACKAGE
        
        ## increase number of trees to 2000 of two and inch down shrinkage = 0.025
        ## keep depth to 2
        ## reduce bag.fraction to 0.031 which will make 3
        ## 
        
        library(gbm)
        
        
        #train_control<-tree.control(nobs=dim(train_data)[1], mindev=0.01/2)
        #train_control<-tree.control(nobs=dim(train_data)[1], mindev=0.01/2)
        ## renumber rows
        train<-train_data
        
        set.seed(8675309)
        gbm_fit<-gbm(target~.-id, data=train, 
                     n.trees = 2000,
                     distribution = "multinomial",
                     shrinkage=0.025,
                     interaction.depth=2,       ## interaction depth of 1 (normally between 1 and 3)
                     train.fraction=0.5,
                     bag.fraction=0.031,
                     keep.data=TRUE,
                     verbose=TRUE,
                     cv.folds=3,                ## 3-fold cv 
                     n.cores=1)                 ## avoid annoying bugs
        
        
        
        
        
        
        ## summary of the tree
        
        fitsum <- summary(gbm_fit)
        plot(fitsum)
        
        plot(gbm_fit, i.var=11)
        ## plot and label
        #names(gbm_fit)
        
        gbm_plot<-as.data.frame(cbind(gbm_fit$train.error, gbm_fit$valid.error))
        colnames(gbm_plot)<-c("train.error", "valid.error")
        gbm_plot$iteration<-1:gbm_fit$n.tree
        
        
        p<-ggplot(gbm_plot, aes(x=iteration, y = train.error))+geom_line()
        p<-p+geom_line(aes(x=iteration, y =valid.error), color="red")
        
        print(p)
        # can see a slight divergence of training adn test
        ## still not over training
        
        ## make sure the test data is there 
        # head(test_data)
        
        td<-test_data
        
        td_model<-predict(gbm_fit, newdata=td, type="response")
        td_gbm_predict<-td_model[,,1]
        td_gbm_predict<-as.data.frame(td_gbm_predict)
        
        td_names<-colnames(td_model)
        td_model<-as.data.frame(td_model)
        colnames(td_model)<-td_names
        td_predict <- as.factor(colnames(td_model)[max.col(td_model)])
        
        table(td_predict, td$target)
        
        #         td_predict Class_1 Class_2 Class_3 Class_4 Class_5 Class_6 Class_7 Class_8 Class_9
        #         Class_1     265       7       5       2       0      29      35      75      67
        #         Class_2      45    4348    1408     352      29      53     112      38      60
        #         Class_3       3     697    1135     143       1       3      70       8       3
        #         Class_4       1     106      68     336       2      11      17       0       3
        #         Class_5       3      23       6       5     884       0      11       4       5
        #         Class_6      54      19       4      32       2    4363      82      76      73
        #         Class_7      23      51      64      20       0      82     560      37      10
        #         Class_8     114      21      18       2       6     102      51    2473      94
        #         Class_9     152      15       8       3       1      93       8      48    1387
        #         
        check<-table(td_predict ==test_data$target)
        cat(check)
        
        accuracy<-1-check[1]/(check[1]+check[2])
        
        cat(accuracy)   ## 76.37%
        ## kaggle score 0.62881
        
        ## a small improvement 
        
        ## Add a Random Forest
        
        library(randomForest)
        
        td_rf_model <- randomForest(target~.-id, data=train, importance=TRUE, ntree=500, nodesize=5)
        
        td_rf_predict<-predict(td_rf_model, newdata=td, type="prob")
        
        
        ## average the two models
        average_predict<-(td_rf_predict+td_gbm_predict)/2
        ## pick the max probability
        average_predict_single <- as.factor(colnames(average_predict)[max.col(average_predict)])
        
        table(average_predict_single, td$target)
        
#average_predict_single Class_1 Class_2 Class_3 Class_4 Class_5 Class_6 Class_7 Class_8 Class_9
#               Class_1     273       4       2       1       0      23      24      43      30
#               Class_2      37    4566    1379     362      27      45     132      38      47
#               Class_3       2     575    1220     129       1       4      60       8       2
#               Class_4       0      49      41     348       1       4      14       0       1
#               Class_5       1      11       2       6     889       0       5       2       3
#               Class_6      47      17       5      30       3    4432      77      79      65
#               Class_7      28      40      44      15       0      62     572      22       8
#               Class_8     120      15      19       2       2      92      56    2535      81
#               Class_9     152      10       4       2       2      74       6      32    1465
       
        #        
        #       Note this still does not fix the class_2 class_3 mixup
        
        check<-table(average_predict_single ==td$target)
        cat(check)
        
        accuracy<-1-check[1]/(check[1]+check[2])
        
        cat(accuracy)
        
        ## accuracy of 79.03%
        
        ##MAKE SUBMISSION
        
        ## get the actual test data
        directory<- "/Users/winstonsaunders/Documents/pdxkagglegroupproductclassproject/"
        file_name<- "test.csv"
        
        real_test_data<-read.csv(paste0(directory,file_name))
        ##just make sure its real
        print(head(real_test_data))
        ## run the prediction
        gbm_real_test_model<-predict(gbm_fit, newdata=real_test_data, type="response")
        rf_real_test_model<-predict(td_rf_model, newdata=real_test_data, type="prob")
        ## get the data
        a<-gbm_real_test_model[,,1]
        a<-as.data.frame(a)
        b<-rf_real_test_model
        submission<-(a+b)/2
        submission<-as.data.frame(submission)
        ## clean up the numbers
        submission<-round(10000*submission,0)/10000.
        
        ## add ids back
        submission<-cbind("id"=as.integer(real_test_data$id), submission)
        submission<-as.data.frame(submission)
        submission$id<-as.integer(submission$id)
        options(scipen=10)
        dim(submission)
        head(submission)
        tail(submission)
        
        ## write csv
        write.csv(submission, paste0(directory,"May081",".csv"), row.names=F, quote=F)
        
        
        
        
        
        
        
        