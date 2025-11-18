'
                                      ~Fingerprinting Workflow in R~
  This code includes many manipulation steps of LC-MS fingerprinting Data, from imputation and plotting PCAs,
                      to calculatiing correlations, filtering and statistical analysis.
                      
The following libraries and functions are necessary for the code to run. 
Please notice that the code only calls the libraries, assuming they are already installed.
If a library is not already installed, use the command install.packages("libraryname") to install it.' 

library("openxlsx")    # to write xslx files instead of csv
library("HiClimR")      # to calculate correlations fast
library("matrixTests")  # for more control over t.tests in matrices
library("ggplot2")      #for Plots
library("ggrepel")      #for Plots
library("readxl")       #to read xslx files
library("dplyr")        #many functions,but here especially necessary for filtering as in filter(RT > L & RT<U)
library("stats")        #to adjust for multiple tests




wd <- dirname(rstudioapi::getActiveDocumentContext()$path) #extracting the current path of the folder where this R file is located
setwd(wd)  # setting wd as the working directory
# The folowing are functions needed in the code and are included in the folder "Functions"
source("Functions/PCA.R")                            #for PCA score plots
source("Functions/Normalization_PQN_SOB.R")           #for PQN Normalization
source("Functions/ImputeTenth.R")                    #for Imputing 1/10 of the smallest peak in the sample
source("Functions/Combine_Matrices_Dif_Sizes.R")     #to combine two matrices of different dimensions
source("Functions/FilterCorRT.R")                    #to filter based on the RT and the correlations between features (see https://github.com/FadIFG/FilterCorRT)

'
Data structure:
For the code to work properly, your Data Structure must be as follow:
Excel table with a sheet called "inputR" where your data are. 
In "inputR" features must be in rows and samples in columns.
You need to have your groups-assignment in another sheet called "Groups" in a column called "Groups"
In this column "Groups", it is enough to only have the group namesin the same sequence as the corresponding samples. 
Having another column with the sample names in it is recommeneded for better overview in Excel.

The values of mz and RT will be automatically extracted for several purposes in this code. 
For that to work,The features must be Written in this format:
       mz:123.456_rt:12.34_x
                             The x is optional, it is usually the name of the metabolite
                             The number of decimals in mz or rt values is irrelevant
If you follow the pre-processing workflow in Excel (Macro workflow) you will end up with this format anyway.
'








########################################################################################################################


'                                       ##############################
                                        #####PARAMETERS TO CHANGE#####
'                                       ##############################


wd <- dirname(rstudioapi::getActiveDocumentContext()$path) #extracting the current path of the folder where this R file is located
setwd(wd)  # setting wd as the working directory

OData <- read_excel("Example_Dataset.xlsx", sheet = "inputR") #loading the dataset (in the example Dataset are Features in Rows and Samples in columns, which must be transposed later)
Groups <- read_excel("Example_Dataset.xlsx", sheet = "Groups") #loading the sample group
Groups <- Groups$Groups
GrNames <- Groups[!duplicated(Groups)] #deleting duplicate group assignment to get a string of general groups names

#Write project name here 
title<-"Example_Dataset"

#Take a look at the element GrNames, and choose the groups you want to
#compare by writing the index of this groups as it appears in the sequence in GrNames.
#As you see in the example dataset, "WT" has an index of 1 and "KO" an index of 2.
#Therefore if you do not change the indices below, by default you will compare the first
#and second groups appearing in "GrNames"
TTestGroup1 <- GrNames[1]
TTestGroup2 <- GrNames[2]

#here is the adjusted P value maximum (maxPAdj)and the minimum Log2FC (minFC) that will be used to
#extract the features with the lowest adjusted p value highest fold change. The same FC value would be
#used for negative FC too. 
maxPAdj <- 5e-2
minFC <- 2




############################################################################################################################





'
from here on, you only need to introduce changes in the code if you:
      Have urine samples and want to perform Creatinine normalization (scroll down to find the code block)
or    Want to change the parameters for filtering by correlatoins , RT tolerance (default set to 0.05) and Coffecient Treshold (default set to 0.9)
or    Change the ttest from unpaired (Default) to paired (scroll down to activate paired and deactivate unpaired)
'




Data<-data.frame(OData)
rownames(Data)<-Data[,1]
Data<-Data[, -1]
Data<-as.matrix(Data)
Data<-t(Data)

#Imputing
Imputed <- ImputeTenth(Data)
Data<- Imputed
now<-Sys.time()
write.xlsx(as.data.frame(t(Data)), file = paste(format(now, "%Y%m%d_%H%M%S_"),title,"_imputed_only.xlsx",sep = ""),rowNames=TRUE)



norm_method<-"_No-Norm"
#Creating PCAs
a<-PCA.SCPlot(Data,Groups,paste(title,norm_method,sep = ""))
a

jpeg(paste(format(now, "%Y%m%d_%H%M%S_"),title,norm_method,".jpg"), width = 1200, height = 720)
# 2. Create the plot
a
# 3. Close the file
dev.off()



#Calculating correlations #library("HiClimR")   
#samples in rows features in columns
Cor <- fastCor(Data)
Cor<-as.data.frame(Cor)

now<-Sys.time()
write.xlsx(as.data.frame(Cor), file = paste(format(now, "%Y%m%d_%H%M%S_"),title,"_all Correlations.xlsx",sep = ""),rowNames=TRUE)



'                                ~ Filter by correlation~
samples must be in rows, features in columns.
features must be written like this: mz:269.149_rt:5.12_ in order for the extraction of mz and RT to work
The numbers in the fucntion are the recommended RT tolerence between two correlating features (0.05 minutes)
And the minimum value for pearson correlation coefficient between features to be considered correlated (0.9). 
'
Filtered <- FilterCorRT(Data, 0.05, 0.9)
Data <- Filtered



#Normalize using PQN
#for PQN each row a feature, each column a sample. That is why we take t(Data)
PQN <- pqn(t(Data))
Data <- t(PQN)
norm_method<-"_PQN_after_FilterCorRT"

now<-Sys.time()
write.xlsx(as.data.frame(t(Data)), file = paste(format(now, "%Y%m%d_%H%M%S_"),title,norm_method,".xlsx",sep = ""),rowNames=TRUE)




'                              ~Crea Normalization~
# in case of Crea Norm, ,you have to have a sheet called Crea with a column called Creatinine 
Crea <- read_excel("xxx.xlsx" , sheet = "Crea")
Creatinine <- Crea$Creatinine 
#Norm to Crea
CreaData <- Data/Creatinine
Data <- CreaData
norm_method<-"_Norm_to_Creatinine"
#Creating PCAs
#Samples in Rows, features in columns.
b<-PCA.SCPlot(Data,Groups,paste(title,norm_method,sep = ""))
b
#saving the PCA
jpeg(paste(title,norm_method,".jpg",sep = ""), width = 1200, height = 720)
# 2. Create the plot
b
# 3. Close the file
dev.off()
now<-Sys.time()
write.xlsx(as.data.frame(t(CreaData)), file = paste(format(now, "%Y%m%d_%H%M%S_"),title,"_CorrFiltered_NormCrea.xlsx",sep = ""),rowNames=TRUE)
'





'                                                ~Statistics~
                                  t-test between TTestGroup1 and TTestGroup2
'

#library(matrixTests)
x1<-Data
FTs <-colnames(x1)

'
By default in this code, the t-test is unpaired. However,to perform paired t-tests on two groups with the same size,
the samples in both groups should simply be in the same sequence
, as the test would pair the first sample in TTestGroup1 to the first sample in TTestGroup2.
then you can activate the paired t-test code
'
#paired
#ttest <- col_t_paired(x1[which(Groups==TTestGroup1),], x1[which(Groups==TTestGroup2),], alternative = "two.sided",  conf.level = 0.95)

#unpaired
ttest <- col_t_equalvar(x1[which(Groups==TTestGroup1),], x1[which(Groups==TTestGroup2),], alternative = "two.sided",  conf.level = 0.95)

#library(stats)
p_values<- ttest$pvalue #extracting p-value
mean_groupgx<-ttest$mean.x #extracting the mean of both groups
mean_groupgy<-ttest$mean.y
FC<- mean_groupgy/mean_groupgx
pAdjustBH<-p.adjust(p_values, method=c("BH"))
resultsBH_FC <- data.frame(FTs, p_values,pAdjustBH,mean_groupgx,mean_groupgy,FC)
resultsBH_FC<-resultsBH_FC[order(resultsBH_FC$pAdjustBH),] #(sort to adjustedP)#
Gmean <- c(paste0("mean_",TTestGroup1),paste0("mean_",TTestGroup2),paste0("FC_",TTestGroup2,"/",TTestGroup1))
colnames(resultsBH_FC)[4:6]<- Gmean[1:3]

#exporting all features
Features<-resultsBH_FC[,1]
RT <- (sub( ".*rt:","", Features))
RT <- as.numeric(sub( "_.*","", RT))
mz <- (sub( ".*mz:","", Features))
mz <-as.numeric(sub( "_rt.*","", mz))
log2_FC<-log2(resultsBH_FC$FC)
log10_adjP<- -log10(resultsBH_FC$pAdjustBH)
resultsBH_FC_All<-cbind(resultsBH_FC,log2_FC,log10_adjP,RT,mz) 
now<-Sys.time()
write.xlsx(as.data.frame(resultsBH_FC_All), file = paste(format(now, "%Y%m%d_%H%M%S_"),title,"_t-test_All-features.xlsx",sep = ""),rowNames=TRUE)



#volcano plot

metab<-resultsBH_FC_All[,c(1,7,2,3)]

colnames(metab)<-cbind("metab","log2FoldChange","pvalue", "padj")
metab$Significant <- ifelse(metab$padj < 0.05, "FDR < 0.05", "Not significant")
#library(ggplot2) 
#library(ggrepel)


#volcano plot labeled based on fold change and P value
#defining range
A<-subset(metab, log2FoldChange > minFC)
B<-subset(metab, log2FoldChange < -minFC)
FC<-rbind(A,B)
P<-subset(metab, padj < maxPAdj)
Interest<-merge(FC,P)
Interest <- Interest[order(Interest[,4]),]

v <- ggplot(metab, aes(x = log2FoldChange, y = -log10(padj))) +
  geom_point(aes(color = Significant)) +
  scale_color_manual(values = c("red", "black")) +
  theme_bw(base_size = 12) + theme(legend.position = "bottom") +
  geom_text_repel(
    data = Interest,
    aes(label = metab),
    size = 3,  
    box.padding = unit(0.35, "lines"),
    point.padding = unit(0.3, "lines")
  )

v # showing the Volcano Plot

#saving the Volcano Plot
now<-Sys.time()
jpeg(paste(format(now, "%Y%m%d_%H%M%S_"),title,"_VolcanoPlot.jpg",sep = ""), width = 1000, height = 720)
# 2. Create the plot
v
# 3. Close the file
dev.off()

####################################################

#exporting only significant features
SigOnly<-length(which(resultsBH_FC$pAdjustBH < 0.05))
resultsBH_FC<-resultsBH_FC[1:SigOnly,]

Features<-resultsBH_FC[,1]
RT <- (sub( ".*rt:","", Features))
RT <- as.numeric(sub( "_.*","", RT))
mz <- (sub( ".*mz:","", Features))
mz <-as.numeric(sub( "_rt.*","", mz))
log2_FC<-log2(resultsBH_FC$FC)

resultsBH_FC<-cbind(resultsBH_FC,log2_FC,RT,mz) 

now<-Sys.time()
write.xlsx(as.data.frame(resultsBH_FC), file = paste(format(now, "%Y%m%d_%H%M%S_"),title,"_t-test_Signifcant Features Only.xlsx",sep = ""),rowNames=TRUE)






#defining the features with the highest fold change and lowest p value
#defining range
A<-subset(resultsBH_FC_All, log2_FC > minFC)
B<-subset(resultsBH_FC_All, log2_FC < -minFC)
FC<-rbind(A,B)
P<-subset(resultsBH_FC_All, pAdjustBH < maxPAdj)
FC2<-cbind(rownames(FC),FC)
P2<-cbind(rownames(P),P)
Interest<-merge(FC2,P2)      # finding the intersect between both. The feature merge automatically find the common column if there is one.
rownames(Interest) <- Interest[,c(12)]
Interest <- Interest[,-c(11,12)]
Interest <- Interest[order(Interest[,3]),]

now<-Sys.time()
write.xlsx(as.data.frame(Interest), file = paste(format(now, "%Y%m%d_%H%M%S_"),title,"_Top_Metabolites.xlsx",sep = ""),rowNames=TRUE)







#Exporting Correlations of significant features

#x is the desired number of significant features to be considered

options(scipen = 999) # this disables scientific notation of powers with "E", This allows avoiding problems when these get recognized as letters
CorFeat<- cbind(rownames(Cor), Cor) # setting the row names as a first column, preparing for matching features 
Features<-rownames(CorFeat) #extracting the features Identifier
RT <- (sub( ".*rt:","", Features))
RT <- as.numeric(sub( "_.*","", RT)) # extracting retention times of the features
mz <- (sub( ".*mz:","", Features))
mz <-as.numeric(sub( "_rt.*","", mz)) #extracting m/z of the features

colnames(CorFeat)[1] <- "FTs"       #Changing the column name to FTs , for the merge function to recognize at the merging column
SigFeatCor <- merge(CorFeat, Interest)
rownames(SigFeatCor) <- SigFeatCor[,1]
SigFeatCor <- SigFeatCor[,-1]
SigFeatCor <- t(SigFeatCor)

L=length(Features)
SigFeatCor <- SigFeatCor[-c((L+1):(L+9)),]   # deleting the p-values and mz and other rows from the end of the SigFeatCor matrix
SigFeatCor<-cbind(SigFeatCor,Features,RT,mz) #Significant features with all their correlations, mz, and RT.
SigFeatCor<- as.data.frame(SigFeatCor) #here the significant features with their correlations to all other features and the mz and rt of all features. 

tFiltered <- t(Filtered)                         # using the transposed filtered data for extracting the abundance of significant features
SigFeatAbund  <- cbind(rownames(tFiltered ),tFiltered) 
colnames(SigFeatAbund)[1] <- "FTs"             #assigning a feature column to have a common column with "Interest"
SigFeatAbund <- merge(SigFeatAbund , Interest) #extracting the abundance of significant features only
SigFeatAbund <- t(SigFeatAbund)
colnames(SigFeatAbund) <- SigFeatAbund[1,]
SigFeatAbund <- cbind(rownames(SigFeatAbund),SigFeatAbund) #having row names and colnames as part of the table

a<-length(Groups)+2   #a is a number defined based on the number of samples (which is the length of the vector "Groups)
b<-nrow(SigFeatAbund) #b is used in combination with "a" to determine the range where the statistical information to the feature are
SigFeatStat <- SigFeatAbund[a:b,]  #isolating the stats of the significant features in a table

SigFeatAbund <- SigFeatAbund[-(a:b),] #removing the stats to keep abundance only





#here a for-loop to extract the top 100 significant significant feature with its abundance, statistics and correlations. 
#for each feature i

library(dplyr)
x <- length(Interest[,1])
SigFeatSummary <- as.matrix(c(1:100)) 

for(i in 1:x){
  SigCorEach<-SigFeatCor[order(SigFeatCor[,i],decreasing = TRUE),]
  SigCorEach<-SigCorEach[,c(x+1,x+2,x+3,i)]
  r<-as.numeric(SigCorEach[1,2]); L<-r-0.1; U<-r+0.1
  SigCorEach<- SigCorEach %>% filter(RT > L & RT<U)
  SigCorEach <- rbind(colnames(SigCorEach),SigCorEach)
  SigCorEach<-rbind(NA,SigCorEach)
  SigCorEach[1,1] <- i
  SigCorEach[1,4] <- "Correlations"
  SigAbunEach<-SigFeatAbund[,c(1,i+1)]
  SigAbunEach<-rbind(NA,SigAbunEach)
  SigAbunEach[1,2] <- "Abundance"
  SigStatEach<-SigFeatStat[,c(1,i+1)]
  SigStatEach<-rbind(NA,SigStatEach)
  SigStatEach[1,2] <- "Statistics"
  SigCorEach<-as.matrix(SigCorEach)
  SigAbunEach<-as.matrix(SigAbunEach)
  SigStatEach<-as.matrix(SigStatEach)
  SigFeatSummary<-combine.mat(SigFeatSummary,SigCorEach, by = "column")
  SigFeatSummary<-combine.mat(SigFeatSummary,SigAbunEach, by = "column")
  SigFeatSummary<-combine.mat(SigFeatSummary,SigStatEach, by = "column")
  SigFeatSummary<-cbind(SigFeatSummary,NA,NA) 
}

SigFeatSummary <- as.data.frame(SigFeatSummary[,-1])
library("openxlsx") #necessary to export all features in one excel file of multiple sheets
now<-Sys.time()
write.xlsx(SigFeatSummary, file = paste(format(now, "%Y%m%d_%H%M%S_"),title,"_Significant_Features_Summary.xlsx",sep = ""), colNames= FALSE)
