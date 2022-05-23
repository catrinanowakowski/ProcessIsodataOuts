#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param df_AAstd_int PARAM_DESCRIPTION
#' @param df_peaks_internal PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname nitrogen_corrections
#' @export 
nitrogen_corrections <- function(df_AAstd_int, df_peaks_internal){
  # df_AAstd_int <- df_AAstd
  # df_peaks_internal <- df_peaks

  drops <- c("date", "time", "garb_1", "last_name", "notes")
  df_peaks_internal <- df_peaks_internal[,!names(df_peaks_internal)%in%drops]
  
  df_AAstd_int <- df_AAstd_int[df_AAstd_int$AAs %in% AA_list,]
  
  
  Std_df <- data.frame(mix_standard = c("Ala", "Gly", "Thr", "Ser", "Val", "Leu", "Ile", "NLeu", "Pro", "Asp", "Met", "Glu", "Phe", "Lys", "Arg"),
                       n = max(df_AAstd_int$run),
                       # Mean = as.numeric(NA),
                       # Stdev = as.numeric(NA) ,
                       Accepted_delta_15_N = c(-0.54607398761019500000, 1.94867295542031000000, -1.24059300375791000000, -3.24949603620891000000, -0.01889041407098630000, 7.47911452175828000000,  -1.48951415502080000000, NA, -6.79674950772015000000, -8.44544738463509000000, -1.09863912190419000000, -3.00768635729947000000, 3.14250687925614000000, 1.98681288360346000000, -7.12917230404501000000), 
                       correction_factor = NA)
  

  
  means <- aggregate(df_AAstd_int$d.15N.14N, by = list(df_AAstd_int$AAs), FUN = "mean", na.rm = TRUE)
  names(means) <- c("mix_standard", "Mean")
  Std_df <- merge(Std_df, means, by = c("mix_standard"), all = TRUE)
  
  stdvs <- aggregate(df_AAstd_int$d.15N.14N, by = list(df_AAstd_int$AAs), FUN = "sd", na.rm = TRUE)
  names(stdvs) <- c("mix_standard", "Stdev")
  Std_df <- merge(Std_df, stdvs, by = c("mix_standard"))
  

  Std_df$correction_factor <- Std_df$Accepted_delta_15_N - Std_df$Mean 
  
  
  smps<- unique(df_peaks_internal$smp)
  smps <- smps[!smps%in%AAStd_name]
  smps <- smps[!is.na(smps)]
  
  df_peaks_internal$Mean <- NA
  df_peaks_internal$Stdev <- NA
  
  
  samp_mean_std <- function(smp, df_peaks_internal){
    # smp <- "CY3"
    df <- df_peaks_internal[df_peaks_internal$smp == smp,]
    df_means <- aggregate(df$d.15N.14N, by = list(df$AAs), FUN = "mean", na.rm = TRUE)
    names(df_means) <- c("AAs", "Mean")
    df <- merge(df, df_means, by = c("AAs"))
    
    df_stdv <- aggregate(df$d.15N.14N, by = list(df$AAs), FUN = "sd", na.rm = TRUE)
    names(df_stdv) <- c("AAs", "Stdev")
    df <- merge(df, df_stdv, by = c("AAs"))
    
    drops <- c("Stdev.x", "Mean.x")
    df <- df[,!names(df)%in%drops]
    names(df)[(ncol(df)-1):ncol(df)] <- c("Mean", "Stdev")
    
    drops <- c("id",  "Rt.s.", "Ampl.28", "BGD.28", "Area.All.Vs.",  "d.15N.14N",  "file_name", "diff" )
    df <- df[df$run == max(df$run),!names(df)%in%drops]
    names(df) <- c( "AAs", "smp", "n", "Mean", "Stdev")
    return(df)
  }
  
  df_names <- rep(NA, length(smps))
  for(i in 1:length(smps)){
    df_names[i] <- paste0("df_",smps[i])
    assign(paste0("df_",smps[i]), samp_mean_std(smps[i], df_peaks_internal = df_peaks_internal))
  }
  
  
  
  correct_smps <- function(df,Std_df ){
    df$Corrected_delta_15_N <- NA
    # aa <- unique(df$AAs)
    aa <- AA_list
    
    i <- 1
    for(i in 1:length(aa)){
      print(i)
      print(aa[i])
      df$Corrected_delta_15_N[df$AAs == aa[i]] <-  df$Mean[df$AAs == aa[i]] + Std_df$correction_factor[Std_df$mix_standard == aa[i]]
    }
    return(df)
  }
  
  
  
  
  df_cor_names <- rep(NA, length(smps))
  for(i in 1:length(smps)){
    df_cor_names[i] <- paste0("df_cor_",smps[i])
    assign(paste0("df_cor_",smps[i]), correct_smps(get(df_names[i]), Std_df = Std_df))
  }
  
  if(length(smps) == 1){
    fn_df <- get(df_cor_names[1])
  }else if(length(smps) == 2){
    fn_df <- rbind(get(df_cor_names[1]), get(df_cor_names[2]))
  }else{
    fn_df <- rbind(get(df_cor_names[1]), get(df_cor_names[2]))
    for(i in 3:length(smps)){
      fn_df<- rbind(fn_df,get(df_cor_names[i]))
    }
  }
  
  return(list(fn_df = fn_df, 
              Std_df = Std_df))
}
