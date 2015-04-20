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
        
                        sample_rows <- sample(1:dim(train_data)[1], 1000)
                        sample_columns<-c("id","feat_1", sample(feats, 28), "feat_93", "target")
        
                        train_data_T <- train_data[sample_rows, sample_columns]
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
        ## calculate the z-stat
        train_morph$z_stat<-train_morph$mean_data/(train_morph$sdev_data+.00001)
        

## INFORMATIONAL PLOTS
        
        p<-ggplot(train_morph, aes(x=target, y=mean_data, color=feature))+geom_point()+ theme_bw()
        p <- p + ggtitle("means of several features versus class")
        p <- p + guides(color=guide_legend(nrow=10))
        print(p)
        
        p<-ggplot(train_morph, aes(x=feature, y=mean_data, color=target))+geom_point(size=3)+ theme_bw()
        p <- p + ggtitle("means of several features versus feature")
        p <- p + theme(axis.text.x = element_text(angle=90))
        print(p)
        
        p<-ggplot(train_morph, aes(x=target, y=z_stat, color=feature))+geom_point(size=3)+ theme_bw()
        p <- p + ggtitle("z_stat of several features versus class")
        p <- p + guides(color=guide_legend(nrow=10))
        print(p)
                
        ## used information from here: http://www.computerworld.com/article/2486425/business-intelligence-4-data-wrangling-tasks-in-r-for-advanced-beginners.html
        train_class <- ddply(train_morph, c("target"), transform, max=max(z_stat) )
        train_class <- train_class[train_class$z_stat==train_class$max & !is.na(train_class$max),]
        
        print(train_class)
        
        ## plot a histogram 
        hist(train_class$z_stat)
