#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param df_all PARAM_DESCRIPTION
#' @param df_AAstd PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname id_peaks_in_smps
#' @export 
id_peaks_in_smps <- function(df_all, df_AAstd){
  #################################################################################
  #################################################################################
  ## First tag the on and off runs using the timing that they happen at
  ## Put an ID number on each run to count how many peaks there are!
  
  
  #######################
  ## Tag the on and off runs using the timming (first five and last three)
  ## Mabye give every thing an id number, not just each AA...
  # Could give the standard the ID number
  
  
  smps <- unique(df_all$smp)
  runs <- unique(df_all$run)
  df_all$AAs <-NA
  for(i in 1:length(smps)){
    for(j in 1:length(runs)){
      # i <- 1
      # j <- 1
      df <- df_all[df_all$smp == smps[i] & df_all$run == runs[j],]
      if(nrow(df) == 0) next  ## Some of the indexes are intentionaly empty to make the loop simplar
      df$AAs[1:5] <- c("C1", "C2", "C3", "C4", "C5")
      df$AAs[(nrow(df)-2):nrow(df)] <- c("C6", "C7", "C8") 
      df_all[df_all$smp == smps[i] & df_all$run == runs[j],] <-  df
      if(nrow(df) > 23) print(df[nrow(df),])
      rm(df)
    }
  }
  
  ### Check for NA value in run time to know that vendor table is wrong
  for(i in 1:length(smps)){
    for(j in 1:length(runs)){
      # i <- 1
      # j <- 1
      df <- df_all[df_all$smp == smps[i] & df_all$run == runs[j],]
      if(nrow(df) == 0) next  ## Some of the indexes are intentionaly empty to make the loop simplar
      if(any(is.na(df$Rt.s.))){
        stop(paste0("vendor table corrupt reset isodat chromatogram and correct for sample: ",smps[i],", run: ", runs[j] ))   
      }
      
      rm(df)
    }
  }
  
  
  #################################################
  ## Take the diff of just middle peaks
  df_peaks <- df_all[is.na(df_all$AAs),]
  ## Take the diff of time between peaks
  df_peaks<- df_peaks[order(df_peaks$smp, df_peaks$run, df_peaks$Rt.s.),]
  df_peaks$diff <- c(-1,diff(df_peaks$Rt.s.))
  df_peaks$diff[df_peaks$diff < 0] <- -1
  
  
  #################################################
  ## Take the diff of just middle peaks
  C_runs <- c("C1", "C2", "C3", "C4", "C5","C6", "C7", "C8")
  df_AAstd <- df_AAstd[!df_AAstd$AAs%in%C_runs,]
  ## Take the diff of time between peaks
  df_AAstd<- df_AAstd[order(df_AAstd$smp, df_AAstd$run, df_AAstd$Rt.s.),]
  df_AAstd$diff <- c(-1,diff(df_AAstd$Rt.s.))
  df_AAstd$diff[df_AAstd$diff < 0] <- -1

  df_AA_diff <- aggregate(df_AAstd[,names(df_AAstd) %in% c("diff", "Rt.s.")], by = list(AAs = df_AAstd$AAs), FUN = "mean")
  df_AA_std <- aggregate(df_AAstd[,names(df_AAstd) %in% c("diff", "Rt.s.")], by = list(AAs = df_AAstd$AAs), FUN = "sd")
  df_AA_std <- df_AA_std[order(df_AA_std$Rt.s.),]
  
  df_AA_diff <- df_AA_diff[order(df_AA_diff$Rt.s.),]
  
  # df_AA_std <- aggregate(df_peaks$diff, by = list(AAs = df_peaks$AAs), FUN = "sd")
  

  df_peaks_test <-df_peaks[1,]
  df_peaks_test[] <- NA

  smps <- unique(df_peaks$smp)
  runs <- unique(df_peaks$run)
  runs <- runs[order(runs)]
  for(i in 1:length(smps)){
    for(j in 1:length(runs)){
      # i <- 3
      # j <- 1
      df <- df_peaks[df_peaks$smp == smps[i] & df_peaks$run == runs[j],]
      if(nrow(df) == 0) next  ## Some of the indexes are intentionaly empty to make the loop simplar
      
      if(nrow(df) == nrow(df_AA_diff)){
        df$AAs <- df_AA_diff$AAs
        
      }else{
        if(nrow(df) > nrow(df_AA_diff) ) stop(paste0("your sample: ", smps[i], " run: ", runs[j], " has more peaks than your standard. Go check the chromatoagram before proceding."))
      
        for(k in 1:nrow(df_AA_diff)){
          # k = 15
          if(k > nrow(df)){
            df_row <- df[1,]
            df_row[,c(1:9,12:15)] <- NA
            df <- rbind(df[1:k-1,], df_row, df[(k):nrow(df),])
            df$AAs[k] <- df_AA_diff$AAs[k]
            df$diff[k+1] <- df$diff[k+1] - df_AA_diff$diff[k]
            
          }else if(df$diff[k] < df_AA_diff$diff[k] + 10 &  df$diff[k] > df_AA_diff$diff[k] - 10){   ## Ten is a bit high for the space between lys and arg, this number could be changed (needs to be big enough for diff from aastd run to samp run, but small enough that it is bigger than the time diff between two aa next to each other in the run )
            df$AAs[k] <- df_AA_diff$AAs[k]
          }else{
            df_row <- df[1,]
            df_row[,c(1:9,12:15)] <- NA
            df <- rbind(df[1:k-1,], df_row, df[(k):nrow(df),])
            df$AAs[k] <- df_AA_diff$AAs[k]
            df$diff[k+1] <- df$diff[k+1] - df_AA_diff$diff[k]
          } #else
          if(nrow(df) == nrow(df_AA_diff)){
            df$AAs <- df_AA_diff$AAs
            break #for
          } #if
        } #for
      } #else
      df_peaks_test <- rbind(df_peaks_test, df)

      # df_peaks[df_peaks$smp == smps[i] & df_peaks$run == runs[j],] <-  df
      if(nrow(df) > 23) print(df[nrow(df),])
      rm(df)
    }
  }
  df_peaks <- unique(df_peaks_test) ## this is because when arg is missing lys gets duplicated at the end 
  # df_peaks <- df_peaks_test
  return(df_peaks)
}
