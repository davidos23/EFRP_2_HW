

corrmatrix <- as.data.frame(pairedCorrelation)

for(i in m){
  corrmatrix['V1'] = the_data['Date'] + ablak_meret*86400
}

col_list = list()
for(i in (1:23)){
  for(j in (i+1):24){
    colname = paste("CL", i ,"-CL", j)
    col_list[(i-1)*24+j] = colname
  }
}
col_list[sapply(col_list, is.null)] <- NULL

colnames(corrmatrix) = c("Date", col_list)

#atirni az oszlopneveket, utana kinyerni belole adott napokra a halozatot es abrazolni


random_nap = corrmatrix[250,]
random_nap_matrix = matrix(nrow=24, ncol=24)

list=list()
for(i in (1:24)){
  label=paste("CL", i)
  list[i]=label
}

rownames(random_nap_matrix) = list
colnames(random_nap_matrix) = list


#csak a felsoharomszogmatrix kene
for(i in (1:23)){
  for(j in (i+1):24){
      random_nap_matrix[i,j] = random_nap[1,(i-1)*(24-i)+(j-i)+1]
      random_nap_matrix[j,i] = random_nap[1,(i-1)*(24-i)+(j-i)+1]
  }
}












