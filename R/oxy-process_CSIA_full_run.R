#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION

#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname process_CSIA_full_run
#' @export 
process_CSIA_full_run <- function(){

  ##############################################################################
  ##############################################################################
  # source(paste0(file_sorc, "/read_tag_correct.R"))     
  outs <- read_tag_correct(C_N = C_N, file_sorc = file_sorc, file_data = file_data, AAStd_name = AAStd_name)
  
  Std_df = outs$Std_df # Summary table of the AA std runs and the data used in correction factors
  fn_df = outs$fn_df # All corrected sample data. Mean of Isodat values, std of isodat values, corrected final value
  df_peaks = outs$df_peaks # Not corrected data for the sample runs and lab stds. AAs tagged, file names and isodat info stored here
  df_AAstd = outs$df_AAstd # All of the data from the AA std runs including file names and amplitudes
  df_all = outs$df_all  # All of the data from isodat, all samples all aa stds and all lab standards -- not corrected, AA not tagged yet
  rm(outs)
  
  
  ## Sometimes for N when correcting peak st and end it gets confused and does not read vendor data table correctly -- need to reset the dxf file
  
  
  ##########################################################
  ## Add flags 
  # source(paste0(file_sorc, "/add_flags_and_order.R"))     
  outs <- add_flags_and_order(fn_df = fn_df, Std_df = Std_df, df_peaks = df_peaks, df_AAstd = df_AAstd)
  
  fn_df <- outs$fn_df
  Std_df <- outs$Std_df
  all_fn_df <- outs$all_fn_df
  all_fn_std_df <- outs$all_fn_std_df
  
  ##########################################################
  ## Write out the lab standards to a separate folder for plotting later
  
  if(C_N =="N"){
    file_save_stds <- file_save_stds_N
    file_save_AAstds <- file_save_AAstds_N
    if(!is.na(cyano)|!is.na(fish_muscle)){
      write.csv(fn_df[fn_df$smp %in% c(cyano, fish_muscle),], paste0(file_save_stds, "/",fl_nm,"_",cyano,"_",fish_muscle, ".csv"))
    }
    write.csv(all_fn_std_df, paste0(file_save_AAstds, "/",fl_nm,"_AA_Std.csv"))
  }else if(C_N == "C"){
    file_save_stds <- file_save_stds_C
    file_save_AAstds <- file_save_AAstds_C
    if(!is.na(cyano)|!is.na(fish_muscle)){
      write.csv(fn_df[fn_df$smp %in% c(cyano, fish_muscle),], paste0(file_save_stds, "/",fl_nm,"_",cyano,"_",fish_muscle, ".csv"))
    }
    write.csv(all_fn_std_df, paste0(file_save_AAstds, "/",fl_nm,"_AA_Std.csv"))
  }
  
  ##########################################################
  ## Write out the final data from today
  
  write.csv(fn_df, paste0(file_save, "/fn_df_",fl_nm,"_",Sys.Date(), ".csv"))
  write.csv(all_fn_df, paste0(file_save, "/all_fn_df_",fl_nm,"_",Sys.Date(), ".csv"))
  write.csv(all_fn_std_df, paste0(file_save, "/all_fn_std_df_",fl_nm,"_",Sys.Date(), ".csv"))
  
  
  ##########################################################
  
  if(order_AAs == "Run"){
    order_AA <- data.frame(AA  =  c("Ala", "Gly", "Thr", "Ser", "Val", "Leu", "Ile", "NLeu", "Pro", "Asp", "Met", "Glu", "Phe", "Lys", "Arg"), 
                           order = c(1:15) )
    
    Std_df$AAs <- factor(Std_df$AAs, levels = order_AA$AA)
    fn_df$AAs <- factor(fn_df$AAs, levels = order_AA$AA)
  }else if (order_AAs == "Type"){
    if(C_N == "N"){
      order_AA <- data.frame(AA  = c("Ala", "Asp", "Glu", "Ile", "Leu", "Pro", "Val",  "Gly", "Lys", "Phe", "Ser", "Thr", "Tyr",    "Arg", "Met", "NLeu"), 
                             type = c("T", "T",    "T",   "T",   "T",   "T",   "T",     "S",  "S",   "S",   "S",   "S",  "S",       "S",    "S" , "C"), 
                             order = c(1,   2,      3,     4,     5,     6,     7,       8,    9,    10,    11,    12,    13,        14 ,   15,  16) ) 
    }else if(C_N == "C"){
      order_AA <- data.frame(AA  = c("Thr", "Val", "Leu", "Ile", "Phe", "Lys",    "Ala", "Gly", "Ser", "Pro", "Asp", "Met", "Glu",  "Arg", "NLeu"  ), 
                             type = c("E", "E",    "E",   "E",   "E",   "E",      "N",    "N",  "N",   "N",   "N",   "N",    "N",    "N" ,   "C"), 
                             order = c(1,   2,      3,     4,     5,     6,        7,      8,    9,    10,    11,    12,    13,        14 , 15) ) 
    }
    Std_df$AAs <- factor(Std_df$AAs, levels = order_AA$AA)
    fn_df$AAs <- factor(fn_df$AAs, levels = order_AA$AA)
  }
  order_AA <- data.frame(AA  =  c("Ala", "Gly", "Thr", "Ser", "Val", "Leu", "Ile", "NLeu", "Pro", "Asp", "Met", "Glu", "Phe", "Lys", "Arg"), 
                         order = c(1:15) )
  all_fn_std_df$AAs <-  factor(all_fn_std_df$AAs, levels = order_AA$AA)
  all_fn_std_df$run <- as.factor(all_fn_std_df$run)
  
  
  
  ## What prelim figures to add to this to quick assess 
  library(ggplot2)
  library(gridExtra)
  if(C_N == "C"){
    std_plt <- ggplot() +
      geom_point(data = all_fn_std_df, aes(x = AAs , y = d.13C.12C.per.mil.vs..VPDB, fill = run, alpha = flag_Stdev), size = 5,pch=21, color = "black", position =position_dodge(width=0.5)) +
      geom_point(data = Std_df, aes(x = AAs , y = Mean), color = "black", size = 4) +
      geom_errorbar(data = Std_df, aes(x = AAs,
                                       ymin=Mean-Stdev,
                                       ymax=Mean+Stdev), width=.2)  +
      guides(alpha = "none") +
      ggtitle(fl_nm) +
      theme_bw() +
      theme(legend.position="bottom")
    
    smp_plt <- ggplot() +
      geom_point(data = fn_df, aes(x = AAs, 
                                   y = Corrected_delta_13_c, 
                                   fill = smp,
                                   alpha = flag_Stdev), size = 5, pch=21, color = "black",
                 position =position_dodge(width=0.2), stat = "identity") +
      geom_errorbar(data = fn_df, 
                    aes(x = AAs, 
                        ymin=Corrected_delta_13_c-Stdev, 
                        ymax=Corrected_delta_13_c+Stdev, 
                        color = smp), 
                    position = position_dodge(width=0.2))  +
      guides(alpha = "none") +
      theme_bw() +
      theme(legend.position="bottom") 
    
    
  }else if(C_N =="N"){
    std_plt <- ggplot() +
      geom_point(data = all_fn_std_df, aes(x = AAs , y = d.15N.14N, fill = run, alpha = flag_Stdev), size = 5,pch=21, color = "black", position =position_dodge(width=0.5)) +
      geom_point(data = Std_df, aes(x = AAs , y = Mean), color = "black", size = 4) +
      geom_errorbar(data = Std_df, aes(x = AAs,
                                       ymin=Mean-Stdev,
                                       ymax=Mean+Stdev), width=.2)  +
      guides(alpha = "none") +
      ggtitle(fl_nm) +
      theme_bw() +
      theme(legend.position="bottom")
    
    
    
    smp_plt <- ggplot() +
      geom_point(data = fn_df, aes(x = AAs, 
                                   y = Corrected_delta_15_N, 
                                   fill = smp,
                                   alpha = flag_Stdev), size = 5, pch=21, color = "black",
                 position =position_dodge(width=0.2), stat = "identity") +
      geom_errorbar(data = fn_df, 
                    aes(x = AAs, 
                        ymin=Corrected_delta_15_N-Stdev, 
                        ymax=Corrected_delta_15_N+Stdev, 
                        color = smp), 
                    position = position_dodge(width=0.2))  +
      guides(alpha = "none") +
      theme_bw() +
      theme(legend.position="bottom") #+
    #ylim(-10, 40)
  }
  grid.arrange(std_plt, smp_plt)
  
  if(C_N == "C"){
    # source(paste(file_sorc,"Lab_Std_plots_C.R", sep = "/"))
    lab_aa_plt <- Plt_Lab_AAStds_C(file_save_AAstds_C = file_save_AAstds_C)+
      geom_point(data = all_fn_std_df, aes(x = AAs,
                                           y = Mean), size = 5, color = "black") +
      ggtitle(fl_nm) 
    
    lab_std_plt <- Plt_Lab_Stds_C(file_save_stds_C = file_save_stds_C) +
      geom_point(data = fn_df[fn_df$smp == cyano,], aes(x = AAs,
                                                        y = Corrected_delta_13_c), size = 5, color = "black") + 
      geom_point(data = fn_df[fn_df$smp == fish_muscle,], aes(x = AAs,
                                                              y = Corrected_delta_13_c), size = 5, color = "darkgray")
  }else if(C_N == "N"){
    # source(paste(file_sorc,"Lab_Std_plots_N.R", sep = "/"))
    lab_aa_plt <- Plt_Lab_AAStds_N(file_save_AAstds_N = file_save_AAstds_N)+
      geom_point(data = all_fn_std_df, aes(x = AAs,
                                           y = Mean), size = 5, color = "black") +
      ggtitle(fl_nm) 
    
    lab_std_plt <- Plt_Lab_Stds_N(file_save_stds_N = file_save_stds_N) +
      geom_point(data = fn_df[fn_df$smp == cyano,], aes(x = AAs,
                                                        y = Corrected_delta_15_N), size = 5, color = "black") + 
      geom_point(data = fn_df[fn_df$smp == fish_muscle,], aes(x = AAs,
                                                              y = Corrected_delta_15_N), size = 5, color = "darkgray")
  }
  grid.arrange(lab_aa_plt, lab_std_plt)
  
  
  all_smp_plts <- list(std_plt, smp_plt)
  lab_compair_plts <- list(lab_aa_plt, lab_std_plt)
  
  grid.arrange(arrangeGrob(grobs = all_smp_plts, ncol = 1), 
               arrangeGrob(grobs = lab_compair_plts, ncol = 1), 
               ncol = 2)
  outs <- list(all_fn_std_df = all_fn_std_df, 
               all_fn_df = all_fn_df)
  
  return(outs)
}
