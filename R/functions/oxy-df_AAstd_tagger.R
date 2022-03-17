#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param df PARAM_DESCRIPTION
#' @param AAStd_name PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname df_AAstd_tagger
#' @export 
df_AAstd_tagger <- function(df, AAStd_name){
  # df = df_all
  
  ####
  # Make sure things are in order
  df <- df[order(df$smp, df$run, df$Rt.s.),]
  df <- df[!is.na(df$Rt.s.),]
  
  #################################################################################
  ## Tag the AA's
  testing <- diff(df$Rt.s.)
  # testing[testing<0] <- NA
  
  min(testing, na.rm = TRUE) #smallest distance between peaks
  
  df_AAstd <- df[df$smp == AAStd_name,]
  
  AAs <- c("C1", "C2", "C3", "C4", "C5",AA_list,  "C6", "C7", "C8")
  runs <- unique(df_AAstd$run)
  for(i in 1:length(runs)){
    print(nrow(df_AAstd[df_AAstd$run == i,]) )
   if(nrow(df_AAstd[df_AAstd$run == runs[i],]) != length(AAs)) stop(paste0("Your list of aa in the standard run is not consistant with the number of peaks in your aa standard file. Go check your peaks, or adjust the variable: AA_list. Run: ", i))
    
  }
  
  df_AAstd$AAs <- rep(AAs, length(unique(df_AAstd$run)))

  return(df_AAstd)
}

