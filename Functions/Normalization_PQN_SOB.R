
####PQN:
#each row a metabolite, each column a sample
pqn<-function(data){
  data<-as.matrix(data)
  reference <- apply(data,1,median) #Berechnung median,Funktion auf Zeilen (MARGIN=1), Spalten (MARGIN=2)
  quotient <- data/reference
  quotient.median <- apply(quotient,2,median) #Berechnung PQN Faktor
  pqn.data <- t(t(data)/quotient.median) #Normalisierung der Daten (Divide by factor)
}




####Sum of bucket 
#each row a metabolite, each column a sample
sumB.norm<-function(data){
  data<-as.matrix(data)
  Fsum <- apply(data,2,sum) #Berechnung median,Funktion auf Zeilen (MARGIN=1), Spalten (MARGIN=2)
  SumBucket <- t((t(data)/Fsum)*1000)
}


