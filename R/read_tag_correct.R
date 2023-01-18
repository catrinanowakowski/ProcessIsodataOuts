#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param C_N PARAM_DESCRIPTION
#' @param file_sorc PARAM_DESCRIPTION
#' @param file_data PARAM_DESCRIPTION
#' @param AAStd_name PARAM_DESCRIPTION
#' @param drift_corr PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname read_tag_correct
#' @export 
read_tag_correct <- function(C_N, file_sorc, file_data, AAStd_name, drift_corr){
  ## Read all of the files in! 
  if(C_N == "C"){
    # source(paste0(file_sorc, "/data_puller_C.R"))
    outs <- data_puller_C(file_data = file_data)
    df_all <- outs$df_all
    # CN_id <- outs$CN_id
    rm(outs)
  }else if(C_N == "N"){
    # source(paste0(file_sorc, "/data_puller_N.R"))
    outs <- data_puller_N(file_data = file_data)
    df_all <- outs$df_all
    rm(outs)
  }
  
  ## Tag the names to the AA std runs
  # source(paste0(file_sorc, "/df_AAstd_tagger.R"))
  df_AAstd <- df_AAstd_tagger(df = df_all, AAStd_name = AAStd_name)
  
  ## Tag the names to the sample peaks 
  # source(paste0(file_sorc, "/id_peaks_in_smps.R"))
  df_peaks <- id_peaks_in_smps(df_all = df_all,  df_AAstd = df_AAstd)
  
  #####################################
  ## Add a drift correction here ######
 if(drift_corr == 1){
   run_times <- df_peaks[,c("date", "time")]
   run_times <- run_times[!is.na(run_times$date),]
   run_times <- unique(run_times)
   run_times <- run_times[order(run_times$date, run_times$time),]
   run_times$inj_num <- 1:nrow(run_times)
   
   df_peaks_test <- merge(df_peaks, run_times, by = c("date", "time"), all = TRUE)
   
   ggplot() +
     geom_point(data = df_peaks_test[df_peaks_test$AAs == "Ala",], aes(x = inj_num, y = d.15N.14N))
   
   
   # drops <- c("date", "time", "garb_1", "last_name", "notes")
   # df_peaks_test <- df_peaks_test[,!names(df_peaks_test)%in%drops]
   
   df_AAstd_int <- df_AAstd
   df_AAstd_int <- merge(df_AAstd_int, run_times, by = c("date", "time"), all = TRUE)
   df_AAstd_int <- df_AAstd_int[df_AAstd_int$AAs %in% AA_list,]
   
   
   ggplot() +
     geom_point(data = df_AAstd_int[df_AAstd_int$AAs == "Ala",], aes(x = inj_num, y = d.15N.14N))
   
   
   
   Std_df <- data.frame(mix_standard = c("Ala", "Gly", "Thr", "Ser", "Val", "Leu", "Ile", "NLeu", "Pro", "Asp", "Met", "Glu", "Phe", "Lys", "Arg"),
                        n = max(df_AAstd_int$run),
                        # Mean = as.numeric(NA),
                        # Stdev = as.numeric(NA) ,
                        Accepted_delta_15_N = c(-0.54607398761019500000, 1.94867295542031000000, -1.24059300375791000000, -3.24949603620891000000, -0.01889041407098630000, 7.47911452175828000000,  -1.48951415502080000000, NA, -6.79674950772015000000, -8.44544738463509000000, -1.09863912190419000000, -3.00768635729947000000, 3.14250687925614000000, 1.98681288360346000000, -7.12917230404501000000), 
                        correction_factor = NA)
   
   AA_nms <- unique(df_AAstd_int$AAs)
   for(i in 1:length(AA_nms)){
     df_AAstd_int$diff[df_AAstd_int$AAs == AA_nms[i]] <- df_AAstd_int$d.15N.14N[df_AAstd_int$AAs == AA_nms[i]] - Std_df$Accepted_delta_15_N[Std_df$mix_standard == AA_nms[i]]
     
   }
   ggplot() +
     geom_point(data = df_AAstd_int[df_AAstd_int$AAs == "Ala",], aes(x = inj_num, y = diff))
   ggplot() +
     geom_point(data = df_AAstd_int, aes(x = inj_num, y = diff))
   
   df_peaks_test <- df_peaks_test[!is.na(df_peaks_test$d.15N.14N),]
   # df_peaks_test$driftcor_d15N <- NA
   AA_nms <- AA_nms[!AA_nms%in%c("NLeu")]
   for(i in 1:length(AA_nms)){
     AA_mod <- lm(data = df_AAstd_int[df_AAstd_int$AAs == AA_nms[i],],diff~ inj_num)
     df_peaks_test$d.15N.14N[df_peaks_test$AAs ==  AA_nms[i]] <- df_peaks_test$d.15N.14N[df_peaks_test$AAs ==  AA_nms[i]] - (AA_mod$coefficients[1] + AA_mod$coefficients[2]*df_peaks_test$inj_num[df_peaks_test$AAs ==  AA_nms[i]])
   }
   
   names(df_peaks_test)[!names(df_peaks_test)%in%names(df_peaks)]
   df_peaks<- df_peaks_test[,!names(df_peaks_test)%in%c("inj_num")]
   
   df_AAstd <- df_peaks[df_peaks$smp == AAStd_name,]
 }
  # #####################################
  # ## Add a drift correction here ######
  
  

  ## Apply corrections 
  if(C_N == "C"){
    # source(paste0(file_sorc, "/carbon_corrections.R"))
    outs <- carbon_corrections(df_AAstd_int = df_AAstd, df_peaks_int = df_peaks)
    fn_df <- outs$fn_df
    Std_df <- outs$Std_df
    rm(outs)
  }else if(C_N == "N"){
    # source(paste0(file_sorc, "/nitrogen_corrections.R"))
    outs <- nitrogen_corrections(df_AAstd_int = df_AAstd, df_peaks_int = df_peaks)
    fn_df <- outs$fn_df
    Std_df <- outs$Std_df
    rm(outs)
  }
  
  outs <- list(Std_df = Std_df, 
               fn_df = fn_df, 
               df_peaks = df_peaks, 
               df_AAstd = df_AAstd, 
               df_all = df_all)
  return(outs)
}
