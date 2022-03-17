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
#' @rdname carbon_corrections
#' @export 
carbon_corrections <- function(df_AAstd_int, df_peaks_internal){
  # df_AAstd_int <- df_AAstd
  # df_peaks_internal <- df_peaks
  
  drops <- c("date", "time", "garb_1", "last_name", "notes")
  df_peaks_internal <- df_peaks_internal[,!names(df_peaks_internal)%in%drops]
  
  df_AAstd_int <- df_AAstd_int[df_AAstd_int$AAs %in% AA_list,]
  
  
  Std_df <- data.frame(mix_standard = c("Ala", "Gly", "Thr", "Ser", "Val", "Leu", "Ile", "NLeu", "Pro", "Asp", "Met", "Glu", "Phe", "Lys", "Arg"),
                       n = max(df_AAstd_int$run),
                       # Mean = as.numeric(NA),
                       # Stdev = as.numeric(NA) ,
                       Accepted_delta_13_C = c(-12.9085491752533, -41.7183853640363, -12.1544191391024, -7.6921820952706, -12.3374024435250, -28.2836744188465,-13.4006134521285, -34.2066688412487, -12.5292807232169, -32.6903205817577, -32.8228744011490,-29.1996007579595, -12.2933312258567, -27.3927976899589, -23.2411002062929), 
                       Frac_AA_C = c(0.375, 0.286, 0.444, 0.375, 0.500, 0.545, 0.545, NA, 0.500, 0.333, 0.500, 0.385, 0.643, 0.462, 0.550), 
                       Frac_der_C = c(0.625, 0.714, 0.556, 0.625, 0.500, 0.455, 0.455, NA, 0.500, 0.667, 0.500, 0.615, 0.357, 0.538, 0.450), 
                       correction_factor = NA)
  

  
  # means <- aggregate(df_AAstd_int$d.13C.12C.per.mil.vs..VPDB, by = list(df_AAstd_int$AAs), FUN = "mean", na.action = na.omit)
  means <- aggregate(df_AAstd_int$d.13C.12C.per.mil.vs..VPDB, by = list(df_AAstd_int$AAs), FUN = "mean", na.rm = TRUE)
  names(means) <- c("mix_standard", "Mean")
  Std_df <- merge(Std_df, means, by = c("mix_standard"), all = TRUE)
  
  stdvs <- aggregate(df_AAstd_int$d.13C.12C.per.mil.vs..VPDB, by = list(df_AAstd_int$AAs), FUN = "sd", na.rm = TRUE)
  names(stdvs) <- c("mix_standard", "Stdev")
  Std_df <- merge(Std_df, stdvs, by = c("mix_standard"))
  
  # 
  # drops <- c("Stdev.x", "Mean.x")
  # Std_df <- Std_df[,!names(Std_df)%in%drops]
  # names(Std_df)[(ncol(Std_df)-1):ncol(Std_df)] <- c("Mean", "Stdev")
  
  
  Std_df$correction_factor <- (Std_df$Mean - (Std_df$Accepted_delta_13_C*Std_df$Frac_AA_C))/Std_df$Frac_der_C
  
  
  smps<- unique(df_peaks_internal$smp)
  smps <- smps[!smps%in%AAStd_name]
  df_peaks_internal$Mean <- NA
  df_peaks_internal$Stdev <- NA
  smps <- smps[!is.na(smps)]
  
  
  samp_mean_std <- function(smp, df_peaks_internal){
    # smp <- "CY3"
    df <- df_peaks_internal[df_peaks_internal$smp == smp,]
    # df_means <- aggregate(df$d.13C.12C.per.mil.vs..VPDB, by = list(df$AAs), FUN = "mean", na.action = na.omit)
    df_means <- aggregate(df$d.13C.12C.per.mil.vs..VPDB, by = list(df$AAs), FUN = "mean", na.rm = TRUE)
    names(df_means) <- c("AAs", "Mean")
    df <- merge(df, df_means, by = c("AAs"))
    
    # df_stdv <- aggregate(df$d.13C.12C.per.mil.vs..VPDB, by = list(df$AAs), FUN = "sd", na.action = na.omit)
    df_stdv <- aggregate(df$d.13C.12C.per.mil.vs..VPDB, by = list(df$AAs), FUN = "sd", na.rm = TRUE)
    names(df_stdv) <- c("AAs", "Stdev")
    df <- merge(df, df_stdv, by = c("AAs"))
    
    drops <- c("Stdev.x", "Mean.x")
    df <- df[,!names(df)%in%drops]
    names(df)[(ncol(df)-1):ncol(df)] <- c("Mean", "Stdev")
    
    drops <- c("id",  "Rt.s.", "Ampl..44.mV.", "BGD.44.mV.", "Area.All.Vs.", "d.13C.12C.per.mil.vs..VPDB",  "file_name", "diff" )
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
    df$Corrected_delta_13_c <- NA
    # aa <- unique(df$AAs)
    aa <- AA_list
    i <- 1
    for(i in 1:length(aa)){
      print(i)
      print(aa[i])
      df$Corrected_delta_13_c[df$AAs == aa[i]] <- (   df$Mean[df$AAs == aa[i]] - ( Std_df$Frac_der_C[Std_df$mix_standard == aa[i]]*Std_df$correction_factor[Std_df$mix_standard == aa[i]] )   ) / Std_df$Frac_AA_C[Std_df$mix_standard == aa[i]]
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
