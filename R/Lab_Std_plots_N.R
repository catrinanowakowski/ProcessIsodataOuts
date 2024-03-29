#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param file_save_stds_N PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname Plt_Lab_Stds_N
#' @export 
Plt_Lab_Stds_N <- function(file_save_stds_N){
  file_save_stds <- file_save_stds_N
  
  
  file_names <- list.files(path = file_save_stds)
  file_names <- file_names[!file_names %in% c("cache")]
  
  
  setwd(file_save_stds)
  df_LabStds <- read.csv(file = file_names[1], header = TRUE, stringsAsFactors = FALSE)
  df_LabStds$file_name <- file_names[1]
  drops <- c("flag_R1_peaksz", "flag_R2_peaksz", "flag_R3_peaksz")
  df_LabStds <- df_LabStds[,!names(df_LabStds)%in%drops]
  
  for(i in 2:length(file_names)){
    df <- read.csv(file = file_names[i], header = TRUE, stringsAsFactors = FALSE)
    df$file_name <- file_names[i]
    df <- df[,!names(df)%in%drops]
    df_LabStds <- rbind(df_LabStds, df)
    rm(df)
  }
  
  order_AA <- data.frame(AA  = c("Ala", "Asp", "Glu", "Ile", "Leu", "Pro", "Val",  "Gly", "Lys", "Phe", "Ser", "Thr", "Tyr",    "Arg", "Met", "NLeu"), 
                         type = c("T", "T",    "T",   "T",   "T",   "T",   "T",     "S",  "S",   "S",   "S",   "S",  "S",       "S",    "S" , "C"), 
                         order = c(1,   2,      3,     4,     5,     6,     7,       8,    9,    10,    11,    12,    13,        14 ,   15,  16) ) 
  df_LabStds$AAs <- factor(df_LabStds$AAs, levels = order_AA$AA)
  
  plt <- ggplot() +
    geom_point(data = df_LabStds, aes(x = AAs, 
                                      y = Corrected_delta_15_N, 
                                      fill = smp), size = 5, pch=21, color = "black",
               position =position_dodge(width=0.2), stat = "identity") +
    geom_errorbar(data = df_LabStds, 
                  aes(x = AAs, 
                      ymin=Corrected_delta_15_N-Stdev, 
                      ymax=Corrected_delta_15_N+Stdev, 
                      color = smp), 
                  position = position_dodge(width=0.2))  +
    guides(alpha = "none") +
    theme_bw() +
    theme(legend.position="bottom")
  
  return(plt)
}


