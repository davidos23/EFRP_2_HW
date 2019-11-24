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
      print("Character formátumba adja meg a kezdõ és végdátumokat pl: \"2010-01-01\"")
      return(FALSE)

      # Ha karakterek, akkor megnézzük, hogy a fájlban megadott intervallumba esnek-e
    } else if (as.numeric(as.Date(kezdo_datum)) < as.numeric(as.Date(adat_kezdo)) ) {
      print(
        paste(
          "Kérjük olyan dátumot adjon meg, ami az elemzés intervallumába beleesik:"
        )
      )
      return(FALSE)

      # Leellenõrizzük, hogy a többi paramétert egész szám formátumban adta meg
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
    CorMatrixCol=n*(n-1)/2# this is the number of columns that contains correlations +1 as date vector [first one]
    pairedCorrelation <- matrix(nrow=m,ncol=CorMatrixCol)
    z=1
    for(i in 1:(n-1)){
      for (j in (i+1):n){
          for(k in 1:m){
            pairedCorrelation[k,z] <- cor(the_data[[1 + i]][(k-1+kezdo_datum_num):(k-1+kezdo_datum_num+ablak_meret)],
                                          the_data[[1 + j]][(k-1+kezdo_datum_num+kesleltet):(k-1+kezdo_datum_num+ablak_meret+kesleltet)])
          }
          z=z+1

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
    MinAvgMaxVal <- matrix(nrow = m,ncol=3)
    for(i in 1:m){ #here we fill the average,min,max vectors to the matrix. First column is date
      MinAvgMaxVal[i,1]<-min(pairedCorrelation[i,])
      MinAvgMaxVal[i,2]<-mean(pairedCorrelation[i,])
      MinAvgMaxVal[i,3]<-max(pairedCorrelation[i,])
    }

    MinAvgMax<<-data.frame(TimeVector,MinAvgMaxVal)
    rm(MinAvgMaxVal,pairedCorrelation)
    return()
  }

graph_plot <- function(day_num){

  col_list = list()
  for(i in (1:23)){
    for(j in (i+1):24){
      colname = paste("CL", i ,"-CL", j)
      col_list[(i-1)*24+j] = colname
    }
  }
  col_list[sapply(col_list, is.null)] <- NULL

  colnames(CorrelationMatrix) = c("Date", col_list)

  #atirni az oszlopneveket, utana kinyerni belole adott napokra a halozatot es abrazolni

  random_nap = CorrelationMatrix[day_num,]
  random_nap_matrix = matrix(nrow=24, ncol=24)

  list=list()
  for(i in (1:24)){
    label=paste("CL", i)
    list[i]=label
  }

  rownames(random_nap_matrix) = list
  colnames(random_nap_matrix) = list

  for(i in (1:23)){
    for(j in (i+1):24){
      random_nap_matrix[i,j] = random_nap[1,(i-1)*(24-i)+(j-i)+1]
      random_nap_matrix[j,i] = random_nap[1,(i-1)*(24-i)+(j-i)+1]
    }
  }

  network_plot(random_nap_matrix)

}

heatmap <- function(day_num) {
  col_list = list()
  for(i in (1:23)){
    for(j in (i+1):24){
      colname = paste("CL", i ,"-CL", j)
      col_list[(i-1)*24+j] = colname
    }
  }
  col_list[sapply(col_list, is.null)] <- NULL

  colnames(CorrelationMatrix) = c("Date", col_list)

  #atirni az oszlopneveket, utana kinyerni belole adott napokra a halozatot es abrazolni

  random_nap = CorrelationMatrix[day_num,]
  random_nap_matrix = matrix(nrow=24, ncol=24)

  list=list()
  for(i in (1:24)){
    label=paste("CL", i)
    list[i]=label
  }

  rownames(random_nap_matrix) = list
  colnames(random_nap_matrix) = list

  for(i in (1:23)){
    for(j in (i+1):24){
      random_nap_matrix[i,j] = random_nap[1,(i-1)*(24-i)+(j-i)+1]
      random_nap_matrix[j,i] = random_nap[1,(i-1)*(24-i)+(j-i)+1]
    }
  }


  melted_cormat <- melt(random_nap_matrix, na.rm = TRUE)
  ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
    geom_tile(color = "white")+
    scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                         midpoint = 0, limit = c(-1,1), space = "Lab",
                         name="Correlation") +
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                     size = 12, hjust = 1))+
    coord_fixed()

}


return_maker()
add_parameters("2011-01-30",10,20)
check_parameters()
calculate_correlation()
graph_plot(100)
heatmap(100)

plot(MinAvgMax[,1], MinAvgMax[,2], "l", col = "red", xlab = "Time", ylab = "Variables", main = "Mean,Minimum,Maximum")

lines(MinAvgMax[,1], MinAvgMax[,3], "l", col = "blue")

lines(MinAvgMax[,1], MinAvgMax[,4], "l", col="green")

legend("bottomleft", legend = c("Minimum","Average","Maximum"),fill=c("red","blue","green"))


library(corrr)
library(reshape2)
library(ggplot2)
