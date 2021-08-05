#readcsv medtronic
read_medtronic_csv <- function(archivo, ...){
  require(janitor)
  require(lubridate)
  
  archivo <- readLines(archivo)
  sep <- substr(archivo[1],10,10)
  blancas <- which(archivo == "")
  
  nomcols <- read.table(text=archivo[1], header=FALSE, sep=sep)
  subject <- read.table(text=archivo[2], header=FALSE, sep=sep)
  colnames(subject) <- nomcols[1:ncol(subject)]
  
  ### extract transactions with the pump
  transactions <- archivo[(blancas[2]+2):(blancas[3]-1)]
  
  transactions <- read.table(text=transactions, header=TRUE, sep=sep)
  transactions <- transactions %>% remove_empty("cols")
  transactions$Date    <- transactions$Date %>% ymd()
  #transactions$Time    <- transactions$Time %>% hms()
  transactions$Datetime  <- transactions$Date + ( transactions$Time %>% hms())
  transactions$Basal.Rate..U.h. <- gsub(",", ".", transactions$Basal.Rate..U.h.) %>% as.numeric()
  transactions$Bolus.Volume.Delivered..U. <- gsub(",", ".", transactions$Bolus.Volume.Delivered..U.) %>% as.numeric()
  transactions$BWZ.Carb.Input..grams.<- gsub(",", ".",transactions$BWZ.Carb.Input..grams.) %>% as.numeric()
  
  basal_rate <- transactions %>% 
    select(Date, Time, Datetime, Basal.Rate..U.h.) %>% 
    filter(!is.na(Basal.Rate..U.h.)) %>% 
    arrange(Datetime) 
  
  basal_rate$delta_time <- c(diff(basal_rate$Datetime),0)/3600
  basal_rate$basal.units <- basal_rate$delta_time * basal_rate$Basal.Rate..U.h.
  
  bolus <- transactions %>% 
    select(Date, Time, Datetime, Bolus.Volume.Delivered..U.) %>% 
    filter(!is.na(Bolus.Volume.Delivered..U.)) %>% 
    arrange(Datetime) 
  
  temps <-  NULL
  if(!is.null(transactions$Temp.Basal.Duration..h.mm.ss.)) {
    temps <- transactions %>% 
      select(Date, Time, Datetime, Temp.Basal.Duration..h.mm.ss., Temp.Basal.Amount) %>% 
      filter(!is.na(Temp.Basal.Amount)) %>% 
      arrange(Datetime) 
    
    temps$Temp.Basal.Duration..h.mm.ss. <- temps$Temp.Basal.Duration..h.mm.ss. %>%  as.difftime(units = "mins")
  }
  

  ### extract glicemic readings
  readings <- archivo[(blancas[3]+2):(blancas[4]-1)]
  readings <- read.table(text=readings, header=TRUE, sep=sep)
  readings <- readings %>% remove_empty("cols")
  readings$Date    <- readings$Date %>% ymd()
  #readings$Time    <- readings$Time %>% hms()
  readings$Datetime  <- readings$Date + (readings$Time %>% hms())
  readings$Sensor.Glucose..mg.dL. <- as.numeric(gsub(",",".",readings$Sensor.Glucose..mg.dL.))
  
  
  return(list("subject" = subject, 
              "transactions" = transactions, 
              "basal" = basal_rate,
              "bolus" = bolus,
              "temps" = temps,
              "readings" = readings))
}

