corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  ## NOTE: Do not round the result!

  files_full <- list.files(directory, full.names=TRUE)
  antall_filer <- length(files_full)
  resultat <- vector("numeric", length = 0)
  for(i in 1:antall_filer) {
    fil <- read.csv(files_full[i])
    cc <- complete.cases(fil)
    if(sum(cc) >= threshold) {
      ## her skal vi beregne korrelasjonen for disse complete casene
      korrelasjon = cor(fil[cc, 2], fil[cc, 3])
      if(!is.na(korrelasjon)) {
        resultat <- c(resultat, as.numeric(korrelasjon))
      }
    }
  }
  
  
##  if(pollutant == "sulfate") {
##    mean(dat[,2], na.rm = TRUE)
##  } else if(pollutant == "nitrate") {
##    mean(dat[,3], na.rm = TRUE)
##  }
  resultat
}