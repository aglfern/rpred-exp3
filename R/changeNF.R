# objetivo: alterar o código de construção do ATS para tratar melhor os NFs
# data: 05/06/2019


# make sure that all the important constants are set before start
TRACE_ID_COLUMN_NAME <- "number"
STATUS_COLUMN_NAME <- "incident_state"
EVENT_TIMESTAMP_COLUMN_NAME <- "updated_at"
CLOSED_STATUS_VALUE <- "Closed"
REMOVE_LONGER_CASES <- FALSE
INCLUDE_SOJOURN_IN_PREDICTION <- FALSE


EXECUTION_DESCRIPTION <- " Testes para alteração do ATS para tratamento dos non-fitting"

HORIZONS <- c(1,3,5,6,7,Inf)

sel_attributes <- c("caller_id", "assigned_to")

test_nf <- exec_all_horizons(s=sel_attributes,runId=1)


# ----- MAIN FUNCTION ------


# ----- função para executar para uma lista de horizontes -----
exec_all_horizons <- function(h=HORIZONS,s,runId)
{
   results <- NULL
   for(i in 1:length(h))
   {
      results <- rbind(results, EXP2_NF(horizon=h[i],sel_attributes=s,runId))
   }
   return(results)
}

EXP2_NF <- function(horizon=selected_horizon, sel_attributes=selected_attributes, runId,
                        training_fn="fold1_train.csv",validation_fn="fold1_test.csv")
{
   generate_log(" ************* Initiating EXP 2 - handling the NF *************", 1)
   generate_log(EXECUTION_DESCRIPTION)
   generate_log(paste(" REMOVE_LONGER_CASES == ",REMOVE_LONGER_CASES))
   generate_log(paste(" INCLUDE_SOJOURN_IN_PREDICTION == ",INCLUDE_SOJOURN_IN_PREDICTION))
   generate_log(paste("Training file: ",training_fn,"Validation file:",validation_fn))

   startTime <- Sys.time()

   # the base name for the files that will store all the results
   statsFile <- paste("results_EXP2_NF_",runId,"_",format(startTime, "%Y%m%d-%H%M"),sep="")

   model <- NULL
   predict <- NULL
   eval_stats_arr <- NULL

   trainingFold <- read.csv(file=file.path("data", training_fn)) #,nrows = 104)

   #option to remove from the training the outliers with elapsed time much bigger
   if ( REMOVE_LONGER_CASES == TRUE ) {
      q <- quantile(trainingFold$elapsed_stc,0.99)
      onePerc <- trainingFold[trainingFold$elapsed_stc > q,c("number","elapsed_stc")]
      onePercDist <- distinct(onePerc,onePerc$number)
      colnames(onePercDist) <- c("number")
      generate_log(paste("Removing ",nrow(onePercDist)," traces that have elapsed times bigger than [",q,"] seconds"))
      '%ni%' <- Negate('%in%')
      trainingFold <- trainingFold[trainingFold$number %ni% onePercDist$number,]
   }

   rfn <- file.path("data/test",paste(statsFile,"_pred.csv",sep=""))

   # builds the transition system
   model <- build_ats(trainingFold,horizon,sel_attributes)
   # anotates the transition system
   training_stats <- annotate_model(trainingFold, rfn, "T", 0, horizon)

   # prediction over the validation data set
   testingFold <- read.csv(file=file.path("data", validation_fn)) #,nrows = 104)

   # builds the transition system for teh testing fold
   predict <- build_prediction(testingFold,model)
   validation_stats <- annotate_model(testingFold, rfn, "V", 0, horizon)

   eval_stats_arr <- rbind(training_stats, validation_stats)


   eval_stats_df1 <- data.frame(eval_stats_arr)

   string_attrib <- paste(unlist(sel_attributes),collapse=",")
   eval_stats_df1 <- cbind(start=format(startTime, "%Y-%m-%d %H:%M:%S"),end=format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                           at="laptop", attributes=string_attrib,eval_stats_df1)

   sfilen <- file.path("data/test",paste(statsFile,"_STATS.csv",sep=""))
   write.table(eval_stats_df1, file=sfilen, row.names=FALSE, col.names = TRUE, sep=";", dec=",")

   generate_log(paste("Stat file generated: [",statsFile,"_STATS.csv]",sep=""))
   generate_log("Step completed successfully.",2)

   return(eval_stats_df1)

}




# ----- outros testes de nf -----


training_fn="fold1_test.csv"

aevents <- read.csv(file=file.path("data", training_fn)) #,nrows = 104)
horiz <- 3
sel_attributes <- "incident_state"

#model <- build_ats(trainingFold,horizon,sel_attributes)

#build_ats <- function(aevents, horiz, sel_attributes)


activity_fields <- do.call(paste, as.data.frame(traceEvents[,sel_attributes]))

p <- NULL
for ( i in 1:2 )
{
   p <- paste(p,as.numeric(traceEvents[,sel_attributes[i]]))
}
p

#INC0000519
aev <- aevents[108:113,]
horizon <- Inf
sel_attributes <- c("incident_state","active")
model <- build_ats(aev,horizon,sel_attributes)

match('New true, Active true', model$seq_state_list, NIL_STATE)




# ---- TESTES com métodos de distância ------------

install.packages("stringdist")
library("stringdist")

#stringsim(a, b, method = c("osa", "lv", "dl", "hamming", "lcs", "qgram",
#                          "cosine", "jaccard", "jw", "soundex"), useBytes = FALSE, q = 1, ...)

# MODEL 1
#[1] "New true"
#[2] "New true, New true"
#[3] "New true, New true, Active true"
#[4] "New true, New true, Active true, Active true"
#[5] "New true, New true, Active true, Active true, Resolved true"
#[6] "New true, New true, Active true, Active true, Resolved true, Closed false"

test_distance_methods('New true', model$seq_state_list)
# 1

test_distance_methods('New true, Active true', model$seq_state_list)
# 2 "New true, New true"

test_distance_methods('New true, Active true, Waiting false', model$seq_state_list)
# 3 "New true, New true, Active true"

# se assumir o mais próximo do estado anterior para continuar a busca
test_distance_methods('New true, New true, Waiting false', model$seq_state_list)
# 3 "New true, New true, Active true"
# não deu diferença

test_distance_methods('New true, Active true, Waiting false, Active true', model$seq_state_list)
#[4] "New true, New true, Active true, Active true"

# se assumir o mais próximo do estado anterior para buscar
test_distance_methods('New true, New true, Active true, Active true', model$seq_state_list)
#[4] "New true, New true, Active true, Active true"


# MODEL 1 - com números
num <- NULL
num[1] <- "1 1"
num[2] <- "1 1, 1 1"
num[3] <- "1 1, 1 1, 2 1"
num[4] <- "1 1, 1 1, 2 1, 2 1"
num[5] <- "1 1, 1 1, 2 1, 2 1, 4 1"
num[6] <- "1 1, 1 1, 2 1, 2 1, 4 1, 5 2"

test_distance_methods('1 1', num)
# [1]

test_distance_methods('1 1, 2 1', num)
# [2] "1 1, 1 1"  menos Jaccard, que indicou [3] e [4] com match 1

test_distance_methods('1 1, 2 1, 3 2', num)
# [3] "1 1, 1 1, 2 1" menos Jaccard que indicou empate entre [3] e [4]

# se assumir o mais próximo do estado anterior para continuar a busca
test_distance_methods('1 1, 1 1, 3 2', num)
# [3] "1 1, 1 1, 2 1" menos Jaccard que indicou empate entre [3] e [4]
# não deu diferença

test_distance_methods('1 1, 2 1, 3 2, 2 1', num)
#[4] "1 1, 1 1, 2 1, 2 1" mas Jaccard indicou empate com [3]

# se assumir o mais próximo do estado anterior para buscar
test_distance_methods('1 1, 1 1, 2 1, 2 1', num)
#[4] "1 1, 1 1, 2 1, 2 1"


# MODEL 2 - com números
num <- NULL
num[1] <- "1 1"
num[2] <- "1 1, 1 1"
num[3] <- "1 1, 1 1, 2 1"
num[4] <- "1 1, 1 1, 2 1, 2 1"
num[5] <- "1 1, 1 1, 2 1, 2 1, 4 1"
num[6] <- "1 1, 1 1, 2 1, 2 1, 4 1, 5 2"
num[7] <- "1 1, 2 1"
num[8] <- "1 1, 2 1, 3 2"
num[9] <- "1 1, 2 1, 3 2, 2 1"
num[10] <- "1 1, 2 1, 3 2, 2 1, 4 1"
num[11] <- "1 1, 2 1, 3 2, 2 1, 4 1, 5 2"

# EVENTO 1
test_distance_methods('1 1', num)
# [1]

# EVENTO 2
test_distance_methods('1 1, 2 2', num)
# [7] "1 1, 2 1"
# Jaccard: [3], [4], [7] com match 1

# EVENTO 3
test_distance_methods('1 1, 2 2, 2 1', num)
# [3] "1 1, 1 1, 2 1"
# Jaccard: [3], [4], [7] com match 1

# se assumir o mais próximo do estado anterior [7] para continuar a busca
test_distance_methods('1 1, 2 1, 2 1', num)
# [3] "1 1, 1 1, 2 1"
# Jaccard: [3], [4], [7] com match 1
# não deu diferença

# MAS SE LIMITAR PARA MANTER NA BUSCA SOMENTE OS ESTADOS QUE PRESERVAREM A SELEÇÃO ANTERIOR [7],
# DAÍ O SELECIONADO PASSA A SER O [8]
# [8] "1 1, 2 1, 3 2
# Jaccard: [8] e [9]

# EVENTO 4
test_distance_methods('1 1, 2 2, 2 1, 3 2', num)
#[4] "1 1, 1 1, 2 1, 2 1"
# Jaccard: [8], [9] com match 1

# se assumir o mais próximo do estado anterior [3] para buscar
test_distance_methods('1 1, 1 1, 2 1, 3 2', num)
#[4] "1 1, 1 1, 2 1, 2 1"
# Jaccard: [8], [9] com match 1

# MAS SE LIMITAR PARA MANTER NA BUSCA SOMENTE OS ESTADOS QUE PRESERVAREM A SELEÇÃO ANTERIOR [8],
# DAÍ O SELECIONADO PASSA A SER O [9]
# [9] "1 1, 2 1, 3 2, 2 1"


# EVENTO 5
test_distance_methods('1 1, 2 2, 2 1, 3 2, 2 1', num)
# Empate entre [5] "1 1, 1 1, 2 1, 2 1, 4 1"
# e            [9] "1 1, 2 1, 3 2, 2 1"
# Jaccard: [8], [9] com match 1


# se assumir o mais próximo do estado anterior [4] para buscar
test_distance_methods('1 1, 1 1, 2 1, 2 1, 2 1', num)
#[5] "1 1, 1 1, 2 1, 2 1, 4 1"
# Jaccard: [3], [4], [7] com match 1



# se assumir o mais próximo do estado anterior [9] para buscar
test_distance_methods('1 1, 2 1, 3 2, 2 1, 2 1', num)
#[10] '1 1, 2 1, 3 2, 2 1, 4 1'
# Jaccard: [8], [9] com match 1



# EVENTO 6
test_distance_methods('1 1, 2 2, 2 1, 3 2, 2 1, 4 1', num)
# [10] "1 1, 2 1, 3 2, 2 1, 4 1"
# Jaccard: [10] com match 1


# se assumir o mais próximo do estado anterior [5] para buscar
test_distance_methods('1 1, 1 1, 2 1, 2 1, 2 1, 4 1, 4 1', num)
#[6] "1 1, 1 1, 2 1, 2 1, 4 1, 5 2"
# Jaccard: [5] com match 1



# se assumir o mais próximo do estado anterior [10] para buscar
test_distance_methods('1 1, 2 1, 3 2, 2 1, 4 1, 4 1', num)
#[11] '1 1, 2 1, 3 2, 2 1, 4 1, 5 2'
# Jaccard: [10] com match 1



#' Title
#'
#' @param target
#' @param listValues
#'
#' @return
#' @export
#'
#' @examples
test_distance_methods <- function(target, listValues)
{
   # c("osa", "lv", "dl", "hamming", "lcs", "qgram", "cosine", "jaccard", "jw", "soundex")
   unlisted <- unlist(listValues, recursive = FALSE)
   dist <- NULL
   dist <- rbind(dist, c("osa", test_distance_method_str(target, unlisted, "osa")))
   dist <- rbind(dist, c("lv", test_distance_method_str(target, unlisted, "lv")))
   dist <- rbind(dist, c("dl", test_distance_method_str(target, unlisted, "dl")))
   dist <- rbind(dist, c("hamming", test_distance_method_str(target, unlisted, "hamming")))
   dist <- rbind(dist, c("lcs", test_distance_method_str(target, unlisted, "lcs")))
   # jaccard tem que ser um por um porque dá erro ao tentar incluir o parâmetro adicional sapply...
   jacc <- NULL
   for(i in 1:length(unlisted))
   {
      jacc <- cbind(jacc, stringsim(a=target, b=unlisted, method="jaccard", useBytes=FALSE, q=1))
   }
   dist <- rbind(dist, c("jaccard", jacc))
   dist <- format(is.numeric(dist),digits=2)
   return(dist)
}

test_distance_method_str <- function(target, listValues, method)
{
   return(sapply(target, stringsim, a=target, b=listValues, method=method, useBytes=FALSE))
}

stringsim(a="A C", b="A B C", method="dl", useBytes=FALSE)

stringsim(a="C A", b="C A B", method="jaccard", useBytes=FALSE)

SET = 




test_distance_method_str('New true, Active true', model$seq_state_list, method = "osa")

test_distance_method_str('New true, Active true', model$seq_state_list, method = "jaccard")


stringsim('New true, Active true', "New true, New true", method = "osa", q = NULL)

# sapply(m, test_distance_method_str, target='New true, Active true', listValues=model$seq_state_list, method=m)

# stringsim
# # Returns a vector with similarities, which are values between 0 and 1 where 1 corresponds to perfect similarity (distance 0)
# and 0 to complete dissimilarity. NA is returned when stringdist returns NA. Distances equal to Inf are truncated to a similarity of 0.
# stringsim('New true, Active true', "New true", method = "osa")
# stringsim('New true, Active true', "New true, New true", method = "osa")
# stringsim('New true, Active true', "New true, New true, Active true", method = "osa")
# stringsim('New true, Active true', "New true, New true, Active true, Active true", method = "osa")
# stringsim('New true, Active true', "New true, New true, Active true, Active true, Resolved true", method = "osa")
# stringsim('New true, Active true', "New true, New true, Active true, Active true, Resolved true, Closed false", method = "osa")
#
# stringsim('New true, Active true', model$seq_state_list, method = "osa")
#
# u <- unlist(model$seq_state_list, recursive = FALSE)
# sapply("New true, Active true", stringsim, a='New true, Active true', b=u, method="osa", useBytes=FALSE)
qgr <- 2
stringsim('New true, Active true', "New true", method = "jaccard", q = qgr)
stringsim('New true, Active true', "New true, New true", method = "jaccard")
stringsim('New true, Active true', "New true, New true, Active true", method = "jaccard", q = qgr)
stringsim('New true, Active true', "New true, New true, Active true, Active true", method = "jaccard", q = qgr)
stringsim('New true, Active true', "New true, New true, Active true, Active true, Resolved true", method = "jaccard", q = qgr)
stringsim('New true, Active true', "New true, New true, Active true, Active true, Resolved true, Closed false", method = "jaccard", q = qgr)

# não funciona...
sapply('New true, Active true', stringsim, a='New true, Active true', b=unlist(model$seq_state_list, recursive = FALSE), method="jaccard", q=2, useBytes=FALSE)


qgr <- 3
stringsim('New true, Active true', "New true", method = "cosine", q = qgr)
stringsim('New true, Active true', "New true, New true", method = "cosine")
stringsim('New true, Active true', "New true, New true, Active true", method = "cosine", q = qgr)
stringsim('New true, Active true', "New true, New true, Active true, Active true", method = "cosine", q = qgr)
stringsim('New true, Active true', "New true, New true, Active true, Active true, Resolved true", method = "cosine", q = qgr)
stringsim('New true, Active true', "New true, New true, Active true, Active true, Resolved true, Closed false", method = "cosine", q = qgr)

qgr <- 3
stringsim('New true, Active true', "New true", method = "jw", q = qgr)
stringsim('New true, Active true', "New true, New true", method = "jw")
stringsim('New true, Active true', "New true, New true, Active true", method = "jw", q = qgr)
stringsim('New true, Active true', "New true, New true, Active true, Active true", method = "jw", q = qgr)
stringsim('New true, Active true', "New true, New true, Active true, Active true, Resolved true", method = "jw", q = qgr)
stringsim('New true, Active true', "New true, New true, Active true, Active true, Resolved true, Closed false", method = "jw", q = qgr)





# MODEL 1 - com números
num <- NULL
num[1] <- "0001 0001"
num[2] <- "0001 0001, 0001 0001"
num[3] <- "0001 0001, 0001 0001, 0002 0001"
num[4] <- "0001 0001, 0001 0001, 0002 0001, 0002 0001"
num[5] <- "0001 0001, 0001 0001, 0002 0001, 0002 0001, 0004 0001"
num[6] <- "0001 0001, 0001 0001, 0002 0001, 0002 0001, 0004 0001, 0005 0002"

test_distance_methods('0001 0001', num)
# [1]

# [,1]      [,2] [,3]                [,4]                [,5]                [,6]                [,7]
# [1,] "osa"     "1"  "0.45"              "0.290322580645161" "0.214285714285714" "0.169811320754717" "0.140625"
# [2,] "lv"      "1"  "0.45"              "0.290322580645161" "0.214285714285714" "0.169811320754717" "0.140625"
# [3,] "dl"      "1"  "0.45"              "0.290322580645161" "0.214285714285714" "0.169811320754717" "0.140625"
# [4,] "hamming" "1"  "0"                 "0"                 "0"                 "0"                 "0"
# [5,] "lcs"     "1"  "0.620689655172414" "0.45"              "0.352941176470588" "0.290322580645161" "0.246575342465753"
# [6,] "jaccard" "1"  "0.75"              "0.6"               "0.6"               "0.5"               "0.428571428571429"
#
#

test_distance_methods('1 1', num)
# [,1]      [,2] [,3]                [,4]                [,5]                [,6]                [,7]
# [1,] "osa"     "1"  "0.375"             "0.230769230769231" "0.166666666666667" "0.130434782608696" "0.107142857142857"
# [2,] "lv"      "1"  "0.375"             "0.230769230769231" "0.166666666666667" "0.130434782608696" "0.107142857142857"
# [3,] "dl"      "1"  "0.375"             "0.230769230769231" "0.166666666666667" "0.130434782608696" "0.107142857142857"
# [4,] "hamming" "1"  "0"                 "0"                 "0"                 "0"                 "0"
# [5,] "lcs"     "1"  "0.545454545454545" "0.375"             "0.285714285714286" "0.230769230769231" "0.193548387096774"
# [6,] "jaccard" "1"  "0.666666666666667" "0.5"               "0.5"               "0.4"               "0.333333333333333"


num[7] <- "1 1, 1 1"

d <- stringsim(a='1 1, 2 1', b=num, method="dl", useBytes=FALSE)
which.max(d)

open_num <- num[which(startsWith(num,"1 1"))]

