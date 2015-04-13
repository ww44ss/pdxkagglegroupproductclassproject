## PDX DATA SCIENCE KAGGLE PROJECT
##

## Get trining data
##
        directory<- "/Users/winstonsaunders/Documents/pdxkagglegroupproductclassproject/"
        file_name<- "train.csv"
        
        train_data<-read.csv(paste0(directory,file_name))
        
        print(head(train_data))

        
        require(plyr)
        require(ggplot2)
        require(tidyr)
        ## create a subset for simpler debugging
        
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
        
        ## convert from wide to long format
        require(tidyr)
        long_train<-gather(train_data, feature, data, feat_1:feat_93)
        
              
        ## summarize using ddply
        train_morph<-ddply(long_train, c("target", "feature"), summarize, mean_data = mean(data), sdev_data = sqrt(var(data)))
        
        
        
        p<-ggplot(train_morph, aes(x=target, y=mean_data, color=feature))+geom_point()+ theme_bw()
        p <- p + ggtitle("means of several features versus class")
        p <- p + guides(color=guide_legend(nrow=10))
        print(p)
        
        p<-ggplot(train_morph, aes(x=feature, y=mean_data, color=target))+geom_point(size=3)+ theme_bw()
        p <- p + ggtitle("means of several features versus feature")
        p <- p + theme(axis.text.x = element_text(angle=90))
        print(p)
        
        p<-ggplot(train_morph, aes(x=target, y=sdev_data, color=feature))+geom_point()+ theme_bw()
        p <- p + ggtitle("standard deviations of several features versus class")
        p <- p + guides(color=guide_legend(nrow=10))
        print(p)
        
        
        
        
        ## used information from here: http://www.computerworld.com/article/2486425/business-intelligence-4-data-wrangling-tasks-in-r-for-advanced-beginners.html
        train_class <- ddply(train_morph, c("target"), transform, max=max(mean_data) )
        train_class <- train_class[train_class$mean_data==train_class$max,]
        
        print(train_class)
