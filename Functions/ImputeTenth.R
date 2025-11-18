#Function to impute 1/10 of the smallest peak instead of 0


ImputeTenth <- function(Data){
  #imputen (mit 1/10xmin) wenn 0 statt NA:
  NA_matrix <-matrix(NA, dim(Data), length((Data)[1,])) 
  colnames(NA_matrix) <- colnames(Data)
  rownames(NA_matrix) <- rownames(Data)
  for(i in 1:dim(Data)[2]) {
    for(j in 1:dim(Data)[1]) {
      if(Data[j,i]==0){
        NA_matrix[j,i] <-NA
      } else NA_matrix[j,i] <- Data[j,i]
    }
  }
  
  imputed <-matrix(NA, dim(Data), length((Data)[1,])) 
  colnames(imputed)<-colnames(Data)
  rownames(imputed) <- rownames(Data)
  
  for(i in 1:dim(NA_matrix)[2]) {
    for(j in 1:dim(NA_matrix)[1]) {
      if(is.na(NA_matrix[j,i])){
        imputed[j,i] <- min(NA_matrix[j,], na.rm=TRUE)/10
      } else imputed[j,i] <- NA_matrix[j,i]
    }
  }
  return(imputed)
}