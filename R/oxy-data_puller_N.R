#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param file_data PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname data_puller_N
#' @export 
data_puller_N <- function(file_data){
  # file_data = file_data
  ##############################################################################
  ## Pulls names from the folder
  file_names <- list.files(path = file_data)
  file_names <- file_names[!file_names %in% c("cache")]
  
  library(isoreader)
  # Drop the extra columns!
  keeps <- c("Rt", 
             "Ampl.28",
             "BGD.28",
             "Intensity.All",
             "d.15N.14N",
             "file_name")
  
  # Read in all files, bind them togehter
  # setwd(file_data)
  df_all <- iso_read_continuous_flow(paste0(file_data, "/", file_names[1]))
  df_all <- data.frame(df_all$vendor_data_table)
  df_all$file_name <- file_names[1]
  df_all <- df_all[,names(df_all) %in% keeps]
  
  numerics <- c("Rt", "Ampl.28", "BGD.28", "Intensity.All", "d.15N.14N")
  for(i in 1:length(numerics)){
    df_all[,numerics[i]] <- as.numeric(df_all[,numerics[i]])
  }
  for(i in 2:length(file_names)){
    print(i)
    df <- iso_read_continuous_flow(paste0(file_data, "/", file_names[i]))
    df <- data.frame(df$vendor_data_table)
    df$file_name <- file_names[i]
    df <- df[,names(df) %in% keeps]
    
    numerics <- c("Rt", "Ampl.28", "BGD.28", "Intensity.All", "d.15N.14N")
    for(i in 1:length(numerics)){
      df[,numerics[i]] <- as.numeric(df[,numerics[i]])
    }
    
    df_all <- rbind(df_all, df)
    rm(df)
  }
  
  # names(df_all) <- c("Rt..s.", "Ampl..44..mV.", "BGD.44..mV.", "Area.All..Vs.", "d.13C.12C..per.mil..vs..VPDB", "file_name")
  names(df_all) <- c("Rt.s.", "Ampl.28", "BGD.28", "Area.All.Vs.",  "d.15N.14N", "file_name")
  
  
  # Split the sample name off
  df_all$date <- NA
  df_all$time <- NA
  df_all$garb_1 <- NA
  df_all$smp <- NA
  df_all$CN <- NA
  df_all$run <- NA
  df_all$last_name <- NA
  df_all$notes <- NA
  
  
  for(i in 1:nrow(df_all)){
    df_all[i,(ncol(df_all)-7):ncol(df_all)] <- unlist(strsplit(df_all$file_name,'_',fixed=TRUE)[i])
  }
  
  # # Clean the run number and the CN id 
  # for(i in 1:nrow(df_all)){
  #   df_all$CN[i] <- unlist(strsplit(df_all$CN[i],'.',fixed=TRUE))[1]
  # }
  
  for(i in 1:nrow(df_all)){
    df_all$run[i] <- unlist(strsplit(df_all$run[i],'R',fixed=TRUE))[2]
  }
  df_all$run <- as.numeric(df_all$run)
  
  CN_id <- df_all$CN[1]
  df_all <- df_all[,!names(df_all) %in% c("CN")]
  

    
  
  
  return(list(df_all = df_all, 
              CN_id = CN_id))
}

