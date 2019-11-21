#  this is the final code

return_maker <- function(WTI_fut)
{
  ##the next function converts the raw dataset into a new one by making the returns from day n to day n + 1
  ##the first column remains the same as it is our date vector, and the first row is empty as we don't have data before the first day
  ## -!-this function is copied from out pprevious work because this code calculated the correlation with prices
  ## in this code i have changed ret_WTI_fut output name to WTI2 because this is the basic parameter in the original code
  #this function imports the dataset as well 
  WTI_fut <- readxl::read_excel("WTI2.xlsx")
  n <- nrow(WTI_fut)
  m <- ncol(WTI_fut)
  ret_WTI_fut <- matrix(nrow = n, ncol = m)
  for (i in 2:n)
  {
    for (j in 2:m)
    {
      ret_WTI_fut[i,j] <- (WTI_fut[[i,j]] / WTI_fut[[i-1,j]]) - 1
    }
  }
  
  ret_WTI_fut <- cbind(WTI_fut[,1], ret_WTI_fut)
  WTI2 <<- ret_WTI_fut[-1,-2]
  colnames(WTI2) = colnames(WTI_fut)
  # View(ret_WTI_fut)
  return(WTI2)
}

add_parameters <- function(startDate,kesleltet,ablak_meret){
  kezdo_datum  <<- starDate
  kesleltet <<- kesleltet
  ablak_meret <<- ablak_meret
}
check_parameters <-
  function(kezdo_datum,
           kesleltet,
           ablak_meret) {
    adat_kezdo=WTI2[1,1]
    # check the type of the start date
    if (typeof(kezdo_datum) != "double") {
      print("Karakter formátumba adja meg a kezdő és végdátumokat pl: \"2010-01-01\"")
      return(FALSE)
      
      # Ha karakterek, akkor megnézzük, hogy a fájlban megadott intervallumba esnek-e
    } else if (kezdo_datum < adat_kezdo ) {
      print(
        paste(
          "Kérjük olyan dátumot adjon meg, ami az elemzés intervallumába beleesik:"
        )
      )
      return(FALSE)
      
      # Leellenőrizzük, hogy a többi paramétert egész szám formátumban adta meg
    } else if (typeof(kesleltet) != "double" ||
               typeof(ablak_meret) != "double") {
      print("Kérjük a dátumokon kívüli paramétereket egész számok formájában adja meg.")
      return(FALSE)
      
      # check the non-negativity of the the lagg/kesletet
    } else if (kesleltet < 0 ) {
      print("Kérjük megfelelő intevallumban adja meg a paramétereket (pl. a késleltetés ne legyen negatív).")
      return(FALSE)
      
    } 
    return(TRUE)
  } 

calculate_correlation <-
  function(ablak_meret = 100,
           kesleltet = 0,
           the_data = WTI2) {  
    vegso=nrow(WTI2)-1 #-1 as one column kept for the date vector 
    kezdo_datum_num=as.numeric(adat_kezdo-kezdo_datum)
    #insert adat_kezdo as i forgot it
    m<- vegso - ablak_meret-kezdo_datum_num
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
    
    MinAvgMax<<-matrix(nrow = m,ncol=4)
    for(i in 1:CorMatrixCol){ #here we fill the average,min,max vectors to the matrix. First column is date
      MinAvgMax[i,2]=min(pairedCorrelation[i,-1])
      MinAvgMax[i,3]=mean(pairedCorrelation[i,-1])
      MinAvgMax[i,4]=max(pairedCorrelation[i,-1])
    }
  }

