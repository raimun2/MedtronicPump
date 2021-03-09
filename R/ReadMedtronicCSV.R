#readcsv medtronic
read_medtronic_csv <- function(archivo, ...){
  require(janitor)
  archivo <- readLines(archivo)
  
  blancas <- which(archivo == "")
  
  nomcols <- read.table(text=archivo[1], header=FALSE, sep=",")
  subject <- read.table(text=archivo[2], header=FALSE, sep=",")
  colnames(subject) <- nomcols[1:ncol(subject)]
  
  bomba1 <- archivo[(blancas[2]+2):(blancas[3]-1)]
  bomba2 <- archivo[(blancas[3]+2):(blancas[4]-1)]
  
  bomba1 <- read.table(text=bomba1, header=TRUE, ...)
  bomba1 <- bomba1 %>%  remove_empty("cols")
  
  bomba2 <- read.table(text=bomba2, header=TRUE, ...)
  bomba2 <- bomba2 %>%  remove_empty("cols")
  
  return(list("subject" = subject, 
              "transactions" = bomba1, 
              "readings" = bomba2))
}

