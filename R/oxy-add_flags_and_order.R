#' @title add_flags_and_order
#' @description Adds flags to know what has a high std, and orders the files in more user based structure. 
#' @param fn_df final version of samples
#' @param Std_df final version of standards
#' @param df_peaks raw peak data
#' @param df_AAstd raw std data
#' @return returns files with full information
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname add_flags_and_order
#' @export 
add_flags_and_order <- function(fn_df, Std_df, df_peaks, df_AAstd){

  fn_df$flag_Stdev <- 0
  fn_df$flag_Stdev[fn_df$Stdev >.5] <- .5
  fn_df$flag_Stdev[fn_df$Stdev >1] <- 1
  
  Std_df$flag_Stdev <- 0
  Std_df$flag_Stdev[Std_df$Stdev >.5] <- .5
  Std_df$flag_Stdev[Std_df$Stdev > 1] <- 1
  
  
  add_blank_aa <- function(df){
    if(nrow(df) == 0){
      return(df)
    }else{
      AAs <- c("Ala", "Gly", "Thr", "Ser", "Val", "Leu", "Ile", "NLeu", "Pro", "Asp", "Met", "Glu", "Phe", "Lys", "Arg")
      AAs <- AAs[!AAs %in% unique(as.character(df$AAs)) ]
      if(length(AAs) > 0){
        blank <- df[1: (( length(AAs)*length(unique(df$smp) )) ),]
        blank[,] <- NA
        blank$AAs <- AAs
        blank$smp <- unique(df$smp)
        if(C_N == "C"){
          df <- merge(df, blank, by = c("AAs",  "smp", "n", "Mean", "Stdev", "Corrected_delta_13_c", "flag_Stdev"), all = TRUE)
        }else if(C_N == "N"){
          df <- merge(df, blank, by = c("AAs",  "smp", "n", "Mean", "Stdev", "Corrected_delta_15_N", "flag_Stdev"), all = TRUE)
        }
        return(df)
      }
    }#else
    return(df)
  }
  
  fl_lst <- c("fn_df")
  for(i in 1:length(fl_lst)){
    assign(fl_lst[i], add_blank_aa(get(fl_lst[i])))
  }
  
  fn_df <- fn_df[order(fn_df$smp, fn_df$AAs),]
  fn_df <- unique(fn_df)
  
  
  ## Add in the run time order
  aa_order <- c("Ala", "Gly", "Thr", "Ser", "Val", "Leu", "Ile", "NLeu", "Pro", "Asp", "Met", "Glu", "Phe", "Lys", "Arg")
  fn_df$AAs_time_order <- factor(fn_df$AAs, levels = aa_order, labels = c(1:length(aa_order)) )
  fn_df <- fn_df[order(fn_df$smp, fn_df$AAs_time_order),]
  
  ## Bring in more meta data to the output 
  if(C_N =="C"){
    df_vals <-df_peaks[,names(df_peaks) %in% c("Rt.s.", "Ampl..44.mV.", "BGD.44.mV." , "d.13C.12C.per.mil.vs..VPDB", "smp", "run", "AAs")]
  }else if(C_N == "N"){
    df_vals <-df_peaks[,names(df_peaks) %in% c("Rt.s.", "Ampl.28", "BGD.28" , "d.15N.14N", "smp", "run", "AAs")]
  }
  
  all_fn_df_1 <- merge(fn_df, df_vals[df_vals$run == 1,], by = c("smp", "AAs"))
  all_fn_df_2 <- merge(fn_df, df_vals[df_vals$run == 2,], by = c("smp", "AAs"))
  all_fn_df_3 <- merge(fn_df, df_vals[df_vals$run == 3,], by = c("smp", "AAs"))
  
  all_fn_df <- rbind(all_fn_df_1, all_fn_df_2, all_fn_df_3)
  # all_fn_df <- all_fn_df[,!names(all_fn_df) %in% drops]
  all_fn_df <- all_fn_df[order(all_fn_df$smp, all_fn_df$AAs_time_order, all_fn_df$run), ]
  
  ## Add the run order to the std file
  Std_df$AAs_time_order <-  factor(Std_df$mix_standar, levels = aa_order, labels = c(1:length(aa_order)) )
  Std_df <- Std_df[order(Std_df$AAs_time_order),]
  
  ## Bring in more meta data to the output 
  if(C_N =="C"){
    names(df_AAstd)
    df_vals <-df_AAstd[,names(df_AAstd) %in% c("Rt.s.", "Ampl..44.mV.", "BGD.44.mV." , "d.13C.12C.per.mil.vs..VPDB", "smp", "run", "AAs")]
    
  }else if(C_N == "N"){
    df_vals <-df_AAstd[,names(df_AAstd) %in% c("Rt.s.", "Ampl.28", "BGD.28" , "d.15N.14N", "smp", "run", "AAs")]
  }
  names(Std_df)[names(Std_df)%in%c("mix_standard")] <- "AAs"
  
  runs <- max(Std_df$n, na.rm = TRUE)
  
  all_fn_df_1 <- merge(Std_df, df_vals[df_vals$run == 1,], by = c("AAs"))
  all_fn_std_df <- all_fn_df_1
  for(i in 2:runs){
    all_fn_df_n <- merge(Std_df, df_vals[df_vals$run == i,], by = c("AAs"))
    all_fn_std_df <- rbind(all_fn_std_df, all_fn_df_n)
  }
  
  
  
  
  
  if(C_N == "N"){
    col_nms <- c("Mean", "Stdev", "d.15N.14N", "Corrected_delta_15_N", "Rt.s.", "Ampl.28", "BGD.28")
    for(i in 1:length(col_nms)){
      all_fn_df[,col_nms[i]] <- round(all_fn_df[,col_nms[i]], digits = 2)
    }
    
    col_nms <- c( "Mean", "Stdev", "flag_Stdev", "d.15N.14N", "Accepted_delta_15_N", "correction_factor", "Rt.s.", "Ampl.28", "BGD.28")
    for(i in 1:length(col_nms)){
      all_fn_std_df[,col_nms[i]] <- round(all_fn_std_df[,col_nms[i]], digits = 2)
    }
    
    ## Nitrogen names
    # names(all_fn_df)
    all_fn_df_order <- c("n", "run", "smp", "AAs","AAs_time_order",  "Corrected_delta_15_N",  "Stdev", "flag_Stdev", "Mean", "Rt.s.", "Ampl.28", "BGD.28", "d.15N.14N") 
    all_fn_df <- all_fn_df[,all_fn_df_order]
    all_fn_df <- all_fn_df[order(all_fn_df$smp, all_fn_df$AAs_time_order),]
    
    # names(all_fn_std_df)
    all_fn_std_df_order <- c("n", "run", "smp", "AAs","AAs_time_order",  "Mean",                  "Stdev", "flag_Stdev", "Rt.s.", "Ampl.28", "BGD.28", "d.15N.14N", "Accepted_delta_15_N", "correction_factor")  
    all_fn_std_df <- all_fn_std_df[,all_fn_std_df_order]
    all_fn_std_df <- all_fn_std_df[order(all_fn_std_df$smp, all_fn_std_df$AAs_time_order),]
    
    
  }else if(C_N =="C"){
    
    col_nms <- c("Mean", "Stdev", "d.13C.12C.per.mil.vs..VPDB", "Corrected_delta_13_c", "Rt.s.", "Ampl..44.mV.", "BGD.44.mV.")
    for(i in 1:length(col_nms)){
      all_fn_df[,col_nms[i]] <- round(all_fn_df[,col_nms[i]], digits = 2)
    }
    
    col_nms <- c( "Mean", "Stdev", "flag_Stdev", "d.13C.12C.per.mil.vs..VPDB", "Accepted_delta_13_C", "correction_factor", "Rt.s.", "Ampl..44.mV.", "BGD.44.mV.")
    for(i in 1:length(col_nms)){
      all_fn_std_df[,col_nms[i]] <- round(all_fn_std_df[,col_nms[i]], digits = 2)
    }
## NEEDS UPDATED WITH CARBON NAMES!!! ## leaving in wrong code for now so I will fix it later 
    # names(all_fn_df)
    all_fn_df_order <- c("n", "run", "smp", "AAs","AAs_time_order",  "Corrected_delta_13_c",  "Stdev", "flag_Stdev", "Mean", "Rt.s.", "Ampl..44.mV.", "BGD.44.mV.", "d.13C.12C.per.mil.vs..VPDB") 
    all_fn_df <- all_fn_df[,all_fn_df_order]
    all_fn_df <- all_fn_df[order(all_fn_df$smp, all_fn_df$AAs_time_order),]
    
    # names(all_fn_std_df)
    all_fn_std_df_order <- c("n", "run", "smp", "AAs","AAs_time_order",  "Mean",                  "Stdev", "flag_Stdev", "Rt.s.", "Ampl..44.mV.", "BGD.44.mV.", "d.13C.12C.per.mil.vs..VPDB", "Accepted_delta_13_C", "correction_factor")  
    all_fn_std_df <- all_fn_std_df[,all_fn_std_df_order]
    all_fn_std_df <- all_fn_std_df[order(all_fn_std_df$smp, all_fn_std_df$AAs_time_order),]
  }
  
  
  outs <- list(fn_df = fn_df, 
               Std_df = Std_df, 
               all_fn_df = all_fn_df, 
               all_fn_std_df = all_fn_std_df)
  return(outs)
}
