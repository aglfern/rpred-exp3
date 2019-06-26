# This global constant must be defined by the main project function
# It defines the log level that should be generated.
# The lower the number the less number of log output is generated.
LOG_LEVEL <- 2

#' Generates log
#'
#' @param texto
#' @param nivel
#'
#' @return
#' @export
#'
#' @examples
generate_log <- function(texto,nivel=1) {
   if (nivel <= LOG_LEVEL )
      print(paste(Sys.time(), ":", texto))
}




#' Function used to calculate the RMSPE for some executions that were done before
#' this error metric being introduced in the core algorithm
#'
#' @param filename
#'
#' @return
#' @export
#'
#' @examples
calculate_rmspe_comm <- function(d,saveas,appendLines=FALSE) {
   training <- d[which(d$type=="T"),]
   validation <- d[which(d$type=="V"),]
   r1 <- calculate_rmspe_comm_per_type(training, type="T")
   r2 <- calculate_rmspe_comm_per_type(validation, type="V")
   rdf <- rbind(r1,r2)

   if (!is.null(saveas)) {
      write.table(rdf, file=saveas, row.names=FALSE, col.names = FALSE, sep=";", dec=",", quote = FALSE, append = appendLines)
   }
   return(rdf)
}


#' Function used to calculate the RMSPE for some executions that were done before
#' this error metric being introduced in the core algorithm
#'
#' @param events_anot
#' @param type
#' @param horizon
#'
#' @return
#' @export
#'
#' @examples
calculate_rmspe_comm_per_type <- function(events_anot, type)
{
   #events_anot <- read_delim(f, ";", escape_double = FALSE, trim_ws = TRUE,
   #                          col_types = cols(updated_at = col_datetime(format = "%Y-%m-%d %H:%M:%S")))

   events_anot_filtered <- na.omit(events_anot)
   events_anot_filtered <- events_anot_filtered[events_anot_filtered$remaining_stc > 0,]

   #RMSPE(y_pred, y_true)
   rmspe_val <- c(
      RMSPE(events_anot_filtered$committee_set_remaining, events_anot_filtered$remaining_stc),
      RMSPE(events_anot_filtered$committee_mset_remaining, events_anot_filtered$remaining_stc),
      RMSPE(events_anot_filtered$committee_seq_remaining, events_anot_filtered$remaining_stc)
   )
   names(rmspe_val) <- c("rmspe_comm_set", "rmspe_comm_mset", "rmspe_comm_seq")

   rmspe_val2 <- c(
      RMSPE(events_anot_filtered$committee_set_remaining2, events_anot_filtered$remaining_stc),
      RMSPE(events_anot_filtered$committee_mset_remaining2, events_anot_filtered$remaining_stc),
      RMSPE(events_anot_filtered$committee_seq_remaining2, events_anot_filtered$remaining_stc)
   )
   names(rmspe_val2) <- c("rmspe_comm_set2", "rmspe_comm_mset2", "rmspe_comm_seq2")

   # # filtro para eventos com fit
   # events_anot_filtered_fitted_set <- events_anot_filtered[events_anot_filtered$set_state_id != NIL_STATE,]
   # events_anot_filtered_fitted_mset <- events_anot_filtered[events_anot_filtered$mset_state_id != NIL_STATE,]
   # events_anot_filtered_fitted_seq <- events_anot_filtered[events_anot_filtered$seq_state_id != NIL_STATE,]
   #
   # rmspe_val1 <- c(
   #    RMSPE(events_anot_filtered_fitted_set$remaining_stc_pset_mean, events_anot_filtered_fitted_set$remaining_stc),
   #    RMSPE(events_anot_filtered_fitted_mset$remaining_stc_pmset_mean, events_anot_filtered_fitted_mset$remaining_stc),
   #    RMSPE(events_anot_filtered_fitted_seq$remaining_stc_pseq_mean, events_anot_filtered_fitted_seq$remaining_stc)
   # )
   # names(rmspe_val1) <- c("val_rmspe_pset_mean1", "val_rmspe_pmset_mean1", "val_rmspe_pseq_mean1")


   # returns only the summarized results for the given fold and horizon
   rmspe_result <- c(fold=type, rmspe_val, rmspe_val2)

   return(rmspe_result)

}



load_prediction <- function(filename)
{
   d <- read_delim(filename, ";", escape_double = FALSE, trim_ws = TRUE,
                   col_types = cols(horiz = col_character(),
                                    fold = col_skip(), incident_state = col_skip(),
                                    number = col_character(), type = col_character(),
                                    updated_at = col_datetime(format = "%Y-%m-%d %H:%M:%S"),
                                    #updated_at = col_skip(),
                                    sojourn_set_stc = col_double(),
                                    sojourn_mset_stc = col_double(),
                                    sojourn_seq_stc = col_double(),
                                    sojourn_set_state_mean = col_double(),
                                    sojourn_mset_state_mean = col_double(),
                                    sojourn_seq_state_mean = col_double(),
                                    remaining_stc_mset_state_mean = col_double(),
                                    remaining_stc_pmset_mean = col_double(),
                                    remaining_stc_pseq_mean = col_double(),
                                    remaining_stc_pset_mean = col_double(),
                                    remaining_stc_seq_state_mean = col_double(),
                                    remaining_stc_set_state_mean = col_double()
                   ))
   return(d)
}



library(corrplot)
library(plyr) # antes de Hmisc e dplyr
library(Hmisc) # antes de dplyr e fields
library(tidyr) # antes de sets
library(sets) # antes de dplyr e data.table
library(dplyr) # precisa vir antes do data.table
library(data.table)
library(doParallel)
library(fields)
library(lsr)
library(MLmetrics)
library(nortest)
library(readr)
library(writexl)
library(stringdist)