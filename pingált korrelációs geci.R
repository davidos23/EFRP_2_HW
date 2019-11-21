kezdo_datum = "2010-01-02"
veg_datum = "2016-12-31"
kezdo_datum = as.Date(kezdo_datum)
veg_datum = as.Date(veg_datum)
WTI2 <- readxl::read_excel("WTI2.xlsx")
WTI2 <<-return_maker(WTI2)
adat_kezdo=as.Date(WTI2[1,1])

kezdo_datum_num=as.numeric(adat_kezdo-kezdo_datum)
vegso=nrow(WTI2)
ablak_meret=20
kesleltet=0
the_data=WTI2
n<- ncol(WTI2) -1 #-1 because first column kept for the date
m<- vegso - ablak_meret-kezdo_datum_num
vegso=nrow(WTI2)-1 #one column is for the date vector

CorMatrixCol=n*(n-1)+2# this is the number of columns that contains correlations +1 as date vector [first one]
pairedCorrelation=matrix(nrow=m,ncol=CorMatrixCol)

z=2
for(i in 1:n){
  for (j in 1:n){
    if(i!=j){
      for(k in 1:m){
        pairedCorrelation[k,z] = cor(the_data[[1 + i]][(k-1+kezdo_datum_num):(k-1+kezdo_datum_num+ablak_meret)],
           the_data[[1 + j]][(k-1+kezdo_datum_num+kesleltet):(k-1+kezdo_datum_num+ablak_meret+kesleltet)])
          
     
      }
      z=z+1
    }
    
  }
 
} # here we correlate each asset with each other
for(k in 1:m){
  if(kezdo_datum_num!=0){
    pairedCorrelation[k,1]=the_data[k-1+kezdo_datum_num,1]
  }else{
    pairedCorrelation[k,1]=the_data[k,1]
  }
  
} # here we fill up the corr-matrix with dates

#cor(the_data[[1 + i]][(k-1+kezdo_datum_num):(k-1+kezdo_datum_num+ablak_meret)],
 #   the_data[[1 + j]][(k-1+kezdo_datum_num+kesleltet):(k-1+kezdo_datum_num+ablak_meret+kesleltet)])
if(kezdo_datum_num!=0){
  pairedCorrelation[,1]=WTI2[[1]][kezdo_datum_num:nrow(WTI2)]
}else{
  pairedCorrelation[,1]=as.character(WTI2[[1]][1:m])
}
