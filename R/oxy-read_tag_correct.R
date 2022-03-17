#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param C_N PARAM_DESCRIPTION
#' @param file_sorc PARAM_DESCRIPTION
#' @param file_data PARAM_DESCRIPTION
#' @param AAStd_name PARAM_DESCRIPTION
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
read_tag_correct <- function(C_N, file_sorc, file_data, AAStd_name){
  ## Read all of the files in! 
  if(C_N == "C"){
    source(paste0(file_sorc, "/data_puller_C.R"))
    outs <- data_puller_C(file_data = file_data)
    df_all <- outs$df_all
    # CN_id <- outs$CN_id
    rm(outs)
  }else if(C_N == "N"){
    source(paste0(file_sorc, "/data_puller_N.R"))
    outs <- data_puller_N(file_data = file_data)
    df_all <- outs$df_all
    rm(outs)
  }
  
  ## Tag the names to the AA std runs
  source(paste0(file_sorc, "/df_AAstd_tagger.R"))
  df_AAstd <- df_AAstd_tagger(df = df_all, AAStd_name = AAStd_name)
  
  ## Tag the names to the sample peaks 
  source(paste0(file_sorc, "/id_peaks_in_smps.R"))
  df_peaks <- id_peaks_in_smps(df_all = df_all,  df_AAstd = df_AAstd)
  
  
  ## Apply corrections 
  if(C_N == "C"){
    source(paste0(file_sorc, "/carbon_corrections.R"))
    outs <- carbon_corrections(df_AAstd_int = df_AAstd, df_peaks_int = df_peaks)
    fn_df <- outs$fn_df
    Std_df <- outs$Std_df
    rm(outs)
  }else if(C_N == "N"){
    source(paste0(file_sorc, "/nitrogen_corrections.R"))
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
