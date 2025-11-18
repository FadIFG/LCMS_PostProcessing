#in your Data, samples must be in rows, features in columns. 
#features must be written like this: mz:269.149_rt:5.12 in order for the extraction of mz and RT to work



FilterCorRT <- function(Data, RTtol, CorTresh){
#RTol is RT tolerance in minutes, like 0.05
#CorTresh is the correlation threshold, above which the correlation of features must be to be clustered together.
  library(HiClimR)
  library(dplyr)
  
  before <- 2
  after <- 1
  #these values before and after are set to 2 and 1 just to get the loop started
  #the loop stops when the number of columns does not change anymore, 
  #meaning the remaining features do not correlate strongly.
  while(before > after) {
    before <- ncol(Data)
  
Cor <- fastCor(Data)
Cor<-as.data.frame(Cor)

# calculating Medians, samples in Rows, features in columns
Medians <- list()
for (i in 1:dim(Data)[2]){
  Med = median(Data[,i])
  Medians <- append(Medians, Med)}

#extracting all features
Features<- colnames(Data)
RT <- sub( ".*rt:","", Features)
RT <- as.numeric(sub( "_.*","", RT))
mz <- sub( ".*mz:","", Features)
mz <-as.numeric(sub( "_rt.*","", mz))
Medians <- as.numeric(Medians)
FeaturesMedians<-cbind(Features,mz,RT,Medians) 


#here we adding the 4 columns from FeaturesMedians to the far right side of Cor
CorRaw <- cbind(Cor,FeaturesMedians)  

#here a for-loop to extract the representative feature (Highest abundance) within an RT tolerance and above certain correlation Value.
#RTtol = 0.05  # set the desired retention time tolerance for grouping features
#CorTresh = 0.9 # set the desired tolerance Threshold for grouping features


x <- nrow(CorRaw) #defining the number of features, to be used in the for loop
RepFeatures<-data.frame()  #creating an empty data frame to be appended in the loop
for(i in 1:x){
  CorRaw<-CorRaw[order(CorRaw[,i],decreasing = TRUE),] #sorting the correlation of the specific feature i in descending order
  Feat <- CorRaw[,c(x+1,x+2,x+3,x+4,i)] #extracting the feature and it correlations from CorRaw
  r<-as.numeric(Feat[1,3]); L<-r-RTtol; U<-r+RTtol  #extracting the RT and setting lower and upper limit
  Feat<- Feat %>% filter(RT > L & RT<U , Feat[,5] > CorTresh) # filtering the features to RT and Correlation values
  if (nrow(Feat)>5) {Feat<-Feat[1:5,]} #keeping the features with the top 5 correlations, given there are more than 5 features
  Feat[,4]<-as.numeric(Feat[,4]) #converting the Medians column to numeric so we can sort it
  Feat<-Feat[order(Feat[,4],decreasing = TRUE),] #sorting to Medians 
  Feat<-Feat[1,] # keeping only the features with the highest abundance (Median)
  colnames(Feat)[5]<-"Correl." #setting a unified column name so we can bind the rows of different features while looping
  RepFeatures <- rbind(RepFeatures, Feat)} #Ending the loop with each feature being appended to RepFeatures

RepFeatures <- RepFeatures[!duplicated(RepFeatures$Features),] #deleting duplicate features 
toMerge<- cbind(Features, t(Data))    #Adding a Features column to data so "merge" can use it to find the mutual features.
Filtered<-merge(RepFeatures,toMerge)   #finding the overlap between the data matrix and the representative features
Filtered<-Filtered[order(Filtered[,3],decreasing = FALSE),]  #sorting to RT
#Filtered is the output. It is a data set with the most abundant features for a certain RT.
#However to get rid of redundant features, the following is a second iteration



FeaturesMedians2 <- Filtered[,1:4]

Data2 <- Filtered[,-c(2:5)]
rownames(Data2) <- Data2[,1]
Data2<-Data2[,-1]
Data2<- t(Data2)
Features<- colnames(Data2)
RNsamples<-rownames(Data2) #row names, which are the samples names.
Data <-apply(Data2, 2 ,as.numeric)
rownames(Data)<-RNsamples


after <- ncol(Data)}

return(Data)
  
now<-Sys.time()
write.csv(Data, file = paste(format(now, "%Y%m%d_%H%M%S_"),"FilterCorRT.csv",sep = ""))
}

