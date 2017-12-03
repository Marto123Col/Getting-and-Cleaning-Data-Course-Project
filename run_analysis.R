#Run analysis.R
library(dplyr)
library(reshape2)

#Read Files

#Read lables file
        Lables<-read.table("UCI HAR Dataset/activity_labels.txt")
        #Convert from factors to characters 
        Activity_Lables<-as.character(Lables[,2])
        Activity_Levels<-as.character(Lables[,1])
#Read features file
        Features<-read.table("UCI HAR Dataset/features.txt")
        #Convert from factor to characters
        Features<-as.character(Features[,2])
#Read train files 
        #Read train activity - lable
        Traina<-read.table("UCI HAR Dataset/train/Y_train.txt")
        #Read subject number
        Trains<-read.table("UCI HAR Dataset/train/subject_train.txt")
        #Read train data
        Train<-read.table("UCI HAR Dataset/train/X_train.txt")
#Read test fies
        #Read test activity - lable
        Testa<-read.table("UCI HAR Dataset/test/Y_test.txt")
        #Read test subject data
        Tests<-read.table("UCI HAR Dataset/test/subject_test.txt")
        #Read test data
        Test<-read.table("UCI HAR Dataset/test/X_test.txt")

#Extract Features for Data
        #Extract vector with the mean and std features locations
        DataFeatures<-grep(".*mean.*|.*std.*",Features)
        #Extract vector with features names
        DataFeaturesNames<-Features[DataFeatures]
        #Clean the names        
        DataFeaturesNames<-gsub("-std", "Std",DataFeaturesNames)
        DataFeaturesNames<-gsub("-mean","Mean",DataFeaturesNames)
        DataFeaturesNames<-gsub("[-()]","",DataFeaturesNames)
#Extract Train and Test by the std and mean Features
        Test<-Test[DataFeatures]
        Train<-Train[DataFeatures]
#Combine each Test and Train Data sets-> by columm
        #Combine Test
        Test<-cbind(Testa,Tests,Test)
        #Insert Test Label
        #Combine Train
        Train<-cbind(Traina,Trains,Train)
        #Instert Train Label
#Combine Test and Train in one Data set-> by rows
        WCDataMeanStd<-rbind(Test,Train)
#Replace DF Names
        colnames(WCDataMeanStd)<-c("Activity","Subject",DataFeaturesNames)
#Convert Subjects and Activities into factors
        WCDataMeanStd$Subject<-as.factor(WCDataMeanStd$Subject)
        WCDataMeanStd$Activity<-factor(WCDataMeanStd$Activity,levels = Activity_Levels,labels=Activity_Lables)
#Summarize
       WcDataMean<-melt(WCDataMeanStd,id=c("Subject","Activity"))
       WcDataMean<-dcast(WcDataMean, Subject + Activity ~ variable,mean)
       View(WcDataMean)
 #Print Data
       write.table(WcDataMean,"tidy.txt",row.names = FALSE,quote = FALSE)
       
       
       
        
        

                

        
        
        
       
        
        
