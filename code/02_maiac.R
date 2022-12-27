
#In this code, an average of AERONET measurements is made for 
# a given time interval centered  at satellite overpass to compare 
# it with the average of MAIAC retrievals


time_correlation <- function(path_aeronet,path_maiac,time_buffer){
   #path_aeronet AERONET file path
   # path_maiac MAIAC file path
   #time_buffer Time window considered in minutes. 
   #According to the literature: 15 min - 30min - 60min - 90min - 120min
  
  # Open AERONET data
   data_aeronet <- read.csv(path_aeronet, header=TRUE, sep=",", dec=".", na.strings = "NA", stringsAsFactors = FALSE)
   # Date formats
   data_aeronet$date <- as.POSIXct(strptime(data_aeronet$date, format = "%Y-%m-%d %H:%M", "GMT"))
   # Open MAIAC data
   data_sat <- read.csv(path_maiac, header=TRUE, sep=",",dec=".", stringsAsFactors = FALSE, na.strings = "NA")
   #NAs are removed
   data_maiac <- data_sat[complete.cases(data_sat$AOD_055),]
   # Date formats
   data_maiac$date  <- strptime(data_maiac$date, tz= "GMT", format = "%Y%j")
   data_maiac$hour  <- strptime(data_maiac$timestamp, tz= "GMT", format = "%Y%j%H%M")
  
  MODIS_aeronet <- data.frame()
  AOD <- data.frame()

  for (i in 1: nrow(data_maiac)){ 
    if (i %% 50 == 0) {
      print (i)
    }
    #Day-month-year agreement between AERONET and MAIAC is sought.
    table_aeronet<- data_aeronet 
    eq_year <- which(year(table_aeronet$date) == year(data_maiac[i,]$date))
    
    table_aeronet<- table_aeronet[eq_year,] 
    
    eq_month <- which(month(table_aeronet$date) == month(data_maiac[i,]$date))
    table_aeronet<- table_aeronet[eq_month,] 
    
    eq_day <- which(day(table_aeronet$date) == day(data_maiac[i,]$date))
    table_aeronet<- table_aeronet[eq_day,]
    dim_table <- dim(table_aeronet)
    
    if(dim_table[1] == 0){
      out_data <- data.frame(NA, NA, NA, NA,NA,NA,NA,NA,NA,NA)   
      
    }else{ 
      #If there is a match, the AERONET time window is searched.
      table_dif <-data.frame()
      
      
      mach <- which(abs(difftime(table_aeronet$date, data_maiac[i,]$hour,units = "mins")) <time_buffer)
      
      
      table_dif <- table_aeronet[mach,]
      dim_table <- dim(table_dif)
      if(dim_table[1] == 0){  
        df <- data.frame()
        df <- data.frame(NA, NA,NA, NA, NA,NA,NA,NA,NA,NA,NA)
        names(df) <- c("Date_MODIS","timestamp", "satellite","AOD_470","AOD_550_maiac","uncert", "date_AERO", "AOD_550_AER_mean","AOD_550_AER_median","AOD_550_AER_sd","AOD_550_AER_dim")
        
      }else{
        #The output file is created with co-located MAIAC and AERONET data.
        out_data <- data.frame(mean(table_dif[,5],  na.rm=TRUE),
                             median(table_dif[,5],  na.rm=TRUE),
                             sd(table_dif[,5], na.rm=TRUE), (dim_table[1]))
        names(out_data) <- c("mean", "mediana","sd","dim")
        df <- data.frame() 
        df <- data.frame(data_maiac[i,2],data_maiac[i,16], data_maiac[i,10:13], substr(table_dif[1,1],1,10),out_data[,1:4])
        names(df) <- c("Date_MODIS","timestamp", "satellite","AOD_470","AOD_550_maiac","uncert", "date_AERO", "AOD_550_AER_mean","AOD_550_AER_median","AOD_550_AER_sd","AOD_550_AER_dim")
      }
      AOD <- rbind(AOD, df)
      
      names(AOD) <- c("Date_MODIS","timestamp", "satellite","AOD_470","AOD_550_maiac","uncert", "date_AERO", "AOD_550_AER_mean","AOD_550_AER_median","AOD_550_AER_sd","AOD_550_AER_dim")
      AOD <- AOD[complete.cases(AOD),]
    }
  }
  return(AOD)
}


######     -------  EXAMPLE for one station     -------  ######

buffer_time <- 60 #minutes
# BA
data_maiac_BA <- "D:/Josefina/papers_escritos/MAIAC_paper/datasets/maiac/BA-25KM-MAIAC.csv"
data_aeronet_BA <-"D:/Josefina/papers_escritos/MAIAC_paper/datasets/aeronet/datasets_interp_s/BA_2015-2022_interp-s.csv"
combinate_BA <- time_correlation (path_aeronet=data_aeronet_BA,path_maiac=data_maiac_BA,time_buffer=buffer_time)
# Save the file with co-located data from AERONET and MAIAC on local path
write.csv (combinate_BA,"D:/Josefina/papers_escritos/MAIAC_paper/datasets/processed/BA-25KM-MAIAC-60-AER.csv")

# SP
data_maiac_SP <- "D:/Josefina/papers_escritos/MAIAC_paper/datasets/maiac/SP-25KM-MAIAC.csv"
data_aeronet_SP <-"D:/Josefina/papers_escritos/MAIAC_paper/datasets/aeronet/datasets_interp_s/SP_2015-2022_interp-s.csv"
combinate_SP <- time_correlation (path_aeronet=data_aeronet_SP,path_maiac=data_maiac_SP,time_buffer=buffer_time)
write.csv (combinate_SP,"D:/Josefina/papers_escritos/MAIAC_paper/datasets/processed/SP-25KM-MAIAC-60-AER.csv")


# ST
data_maiac_ST <- "D:/Josefina/papers_escritos/MAIAC_paper/datasets/maiac/ST-25KM-MAIAC.csv"
data_aeronet_ST <-"D:/Josefina/papers_escritos/MAIAC_paper/datasets/aeronet/datasets_interp_s/ST_2015-2022_interp-s.csv"
combinate_ST <- time_correlation (path_aeronet=data_aeronet_ST,path_maiac=data_maiac_ST,time_buffer=buffer_time)
write.csv (combinate_ST,"D:/Josefina/papers_escritos/MAIAC_paper/datasets/processed/ST-25KM-MAIAC-60-AER.csv")

# MD
data_maiac_MD <- "D:/Josefina/papers_escritos/MAIAC_paper/datasets/maiac/MD-25KM-MAIAC.csv"
data_aeronet_MD <-"D:/Josefina/papers_escritos/MAIAC_paper/datasets/aeronet/datasets_interp_s/MD_2015-2022_interp-s.csv"
combinate_MD <- time_correlation (path_aeronet=data_aeronet_MD,path_maiac=data_maiac_MD,time_buffer=buffer_time)
write.csv (combinate_MD,"D:/Josefina/papers_escritos/MAIAC_paper/datasets/processed/MD-25KM-MAIAC-60-AER.csv")


# LP
data_maiac_LP <- "D:/Josefina/papers_escritos/MAIAC_paper/datasets/maiac/LP-25KM-MAIAC.csv"
data_aeronet_LP <-"D:/Josefina/papers_escritos/MAIAC_paper/datasets/aeronet/datasets_interp_s/LP_2015-2022_interp-s.csv"
combinate_LP <- time_correlation (path_aeronet=data_aeronet_LP,path_maiac=data_maiac_LP,time_buffer=buffer_time)
write.csv (combinate_LP,"D:/Josefina/papers_escritos/MAIAC_paper/datasets/processed/LP-25KM-MAIAC-60-AER.csv")


# MX
data_maiac_MX <- "D:/Josefina/papers_escritos/MAIAC_paper/datasets/maiac/MX-25KM-MAIAC.csv"
data_aeronet_MX <-"D:/Josefina/papers_escritos/MAIAC_paper/datasets/aeronet/datasets_interp_s/MX_2015-2022_interp-s.csv"
combinate_MX <- time_correlation (path_aeronet=data_aeronet_MX,path_maiac=data_maiac_MX,time_buffer=buffer_time)
write.csv (combinate_MX,"D:/Josefina/papers_escritos/MAIAC_paper/datasets/processed/MX-25KM-MAIAC-60-AER.csv")
