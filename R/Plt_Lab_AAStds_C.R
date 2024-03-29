#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param file_save_AAstds_C PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname Plt_Lab_AAStds_C
#' @export 
Plt_Lab_AAStds_C <- function(file_save_AAstds_C){
  file_save_AAstds <- file_save_AAstds_C
  
  
  file_names <- list.files(path = file_save_AAstds)
  file_names <- file_names[!file_names %in% c("cache")]
  
  
  setwd(file_save_AAstds)
  df_LabStds <- read.csv(file = file_names[1], header = TRUE, stringsAsFactors = FALSE)
  df_LabStds$file_name <- file_names[1]
  drops <- c("flag_R1_peaksz", "flag_R2_peaksz", "flag_R3_peaksz", "diff", "AAs_time_order.1", "Frac_AA_C", "Frac_der_C")
  df_LabStds <- df_LabStds[,!names(df_LabStds)%in%drops]
  
  if(length(file_names) > 1){
    for(i in 2:length(file_names)){
      df <- read.csv(file = file_names[i], header = TRUE, stringsAsFactors = FALSE)
      df$file_name <- file_names[i]
      df <- df[,!names(df)%in%drops]
      
      df_LabStds <- rbind(df_LabStds, df)
      
      rm(df)
    }
  }
  
  order_AA <- data.frame(AA  = c("Thr", "Val", "Leu", "Ile", "Phe", "Lys",    "Ala", "Gly", "Ser", "Pro", "Asp", "Met", "Glu",  "Arg", "NLeu"  ), 
                         type = c("E", "E",    "E",   "E",   "E",   "E",      "N",    "N",  "N",   "N",   "N",   "N",    "N",    "N" ,   "C"), 
                         order = c(1,   2,      3,     4,     5,     6,        7,      8,    9,    10,    11,    12,    13,        14 , 15) ) 
  # order_AA <- data.frame(AA  = c("Ala", "Asp", "Glu", "Ile", "Leu", "Pro", "Val",  "Gly", "Lys", "Phe", "Ser", "Thr", "Tyr",    "Arg", "Met", "NLeu"), 
  #                        type = c("T",  "T",    "T",   "T",   "T",   "T",   "T",     "S",  "S",   "S",   "S",   "S",  "S",       "S",    "S" , "C"), 
  #                        order = c(1,   2,      3,     4,     5,     6,     7,       8,    9,    10,    11,    12,    13,        14 ,   15,  16) ) 
  df_LabStds$AAs <- factor(df_LabStds$AAs, levels = order_AA$AA)
  
  df_LabStds_mean <- aggregate(df_LabStds$Mean, by = list(AAs = df_LabStds$AAs), FUN = "mean")
  df_LabStds_sd <- aggregate(df_LabStds$Mean, by = list(AAs = df_LabStds$AAs), FUN = "sd")
  names(df_LabStds_mean) <- c("AAs", "mean")
  names(df_LabStds_sd) <- c("AAs", "sd")
  df_LabStds_agg <- merge(df_LabStds_mean,df_LabStds_sd,by = c("AAs"))
  df_LabStds_agg$std_alpha <- NA 
  df_LabStds_agg$std_alpha[df_LabStds_agg$sd > .5] <- 1
  df_LabStds_agg$std_alpha[df_LabStds_agg$sd <= .5] <- .75
  
  plt <- ggplot() +
    geom_point(data = df_LabStds, aes(x = AAs, 
                                      y = Mean, 
                                      fill = smp), size = 5, pch=21, color = "black",
               position =position_dodge(width=0.2), stat = "identity") +
    geom_errorbar(data = df_LabStds_agg, 
                  aes(x = AAs, 
                      ymin=mean-sd, 
                      ymax=mean+sd, 
                      alpha = std_alpha), 
                  position = position_dodge(width=0.2))  +
    guides(alpha = "none") +
    theme_bw() +
    theme(legend.position="bottom")
  
  return(plt)
}

