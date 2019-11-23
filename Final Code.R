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
  # for ciklus 2-tol megy, alapbol a ret matrix a 2 oszlopbol tolti fel. a cbind pedig a détumot berakja a ret elé, de lesz egy ures oszlop
  ret_WTI_fut <- cbind(WTI_fut[,1], ret_WTI_fut)
  WTI2 <<- data.frame(ret_WTI_fut[-1,-2])
  colnames(WTI2) = colnames(WTI_fut)
  adat_kezdo<<-WTI2[1,1]
  # View(ret_WTI_fut)
  return(WTI2)
}

add_parameters <- function(startDate,kesleltet,ablak_meret){
  kezdo_datum  <<- startDate
  kesleltet <<- kesleltet
  ablak_meret <<- ablak_meret
}
check_parameters <-
  function() {
    
    # check the type of the start date
    if (typeof(kezdo_datum) != "character") {
      print("Character formátumba adja meg a kezdő és végdátumokat pl: \"2010-01-01\"")
      return(FALSE)
      
      # Ha karakterek, akkor megnézzük, hogy a fájlban megadott intervallumba esnek-e
    } else if (as.numeric(as.Date(kezdo_datum)) < as.numeric(as.Date(adat_kezdo)) ) {
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
      print("Negatív a késleltetés")
      return(FALSE)
      
    }
    return(TRUE)
  }

calculate_correlation <-
  function() {
    the_data <- WTI2
    vegso<-nrow(WTI2)-1 #-1 as one column kept for the date vector
    kezdo_datum_num=as.numeric(as.Date(kezdo_datum))-as.numeric(as.Date(adat_kezdo))
    #insert adat_kezdo as i forgot it
    m<- vegso - ablak_meret-kezdo_datum_num
    n <- ncol(WTI2)-1
    CorMatrixCol=n*(n-1)# this is the number of columns that contains correlations +1 as date vector [first one]
    pairedCorrelation <<- matrix(nrow=m,ncol=CorMatrixCol)
    z=1
    for(i in 1:n){
      for (j in 1:n){
        if(i!=j){
          for(k in 1:m){
            pairedCorrelation[k,z] <<- cor(the_data[[1 + i]][(k-1+kezdo_datum_num):(k-1+kezdo_datum_num+ablak_meret)],
                                           the_data[[1 + j]][(k-1+kezdo_datum_num+kesleltet):(k-1+kezdo_datum_num+ablak_meret+kesleltet)])
          }
          z=z+1
        }
      }
    } # here we correlate each asset with each other
    TimeVector <<- vector(length=m)
    for(k in 1:m){
      if(kezdo_datum_num!=0){
        TimeVector[k]<<-as.Date(the_data[k+kezdo_datum_num+ablak_meret,1])
      }else{
        TimeVector[k]<<-as.Date(the_data[k,1])
      }
      class(TimeVector) <<- "Date"
    } # here we fill up the corr-matrix with dates
    CorrelationMatrix <<- data.frame(TimeVector,pairedCorrelation)
    MinAvgMaxVal<-matrix(nrow = m,ncol=3)
    for(i in 1:m){ #here we fill the average,min,max vectors to the matrix. First column is date
      MinAvgMaxVal[i,1]<-min(pairedCorrelation[i,])
      MinAvgMaxVal[i,2]<-mean(pairedCorrelation[i,])
      MinAvgMaxVal[i,3]<-max(pairedCorrelation[i,])
    }
    MinAvgMax<<-data.frame(TimeVector,MinAvgMaxVal)
    return()
  }


return_maker()
add_parameters("2011-01-30",10,100)
check_parameters()
calculate_correlation()