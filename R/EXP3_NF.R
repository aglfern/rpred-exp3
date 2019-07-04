# objetivo: avaliar soluções para tratar non-fitting


# make sure that all the important constants are set before start
TRACE_ID_COLUMN_NAME <- "number"
STATUS_COLUMN_NAME <- "incident_state"
EVENT_TIMESTAMP_COLUMN_NAME <- "updated_at"
CLOSED_STATUS_VALUE <- "Closed"
REMOVE_LONGER_CASES <- FALSE
#INCLUDE_SOJOURN_IN_PREDICTION <- FALSE

SEARCH_SIMILAR <- TRUE
# possible methods are dl, jaccard
SIM_METHOD_SET <- "jaccard" 
SIM_METHOD_MSET <- "jaccard"
SIM_METHOD_SEQ <- "dl"

SAVE_MODEL <- TRUE
LAST_RUN_MODEL <- NULL
LAST_RUN_PREDICT <- NULL

EXECUTION_DESCRIPTION <- " Testes para alteração do ATS para tratamento dos non-fitting"

HORIZONS <- c(1,3,5,6,7,Inf)

# these were the best horizons selected according to the tests using the
# incident_state, category and priority (expert selection)
#selected_horizon <- c(5,6,7)

#selected_attributes <- c("incident_state", "category", "priority") # expert

# ---- Execuções anteriores ----

# id=1: baseline
# id=2: dl, dl, dl
# id=3: jaccard, jaccard, dl
# id=5: jaccard, jaccard, jaccard
# id=6: dl, dl, dl (bug fixed)

# id=4: 20k, id3

# execuções em "usp"
# id=1: nova baseline
# id=3: com bugfix

id <- 3

selected_attributes <- c("caller_id", "assigned_to")

test_nf3 <- exec_all_horizons(s=selected_attributes,runId=id)

#test_nf2 <- exec_all_horizons(h=c(3,5,6,7,Inf),selected_attributes,id)

#test_h1 <- main(horizon=1,sel_attributes=selected_attributes,runId=id)

#test_h3 <- main(horizon=3,sel_attributes=selected_attributes,runId=id)

# ---- executar somente horizonte 1 após correção de bug na predição para h1 e métodos diferentes ---
SEARCH_SIMILAR <- TRUE
id <- 3
SIM_METHOD_SET <- "jaccard" 
SIM_METHOD_MSET <- "jaccard"
SIM_METHOD_SEQ <- "dl"
test_h1 <- main(horizon=1,sel_attributes=selected_attributes,runId=id)



id <- 4
test_h1 <- main(horizon=1,sel_attributes=selected_attributes,runId=id,
                training_fn="preproc_ds1and2.csv",validation_fn="preproc_ds3.csv")

# nova variação, vou executar usando jaccard para todos.
test_nf3 <- exec_all_horizons(h=HORIZONS,selected_attributes,5)

#selected_attributes <- c("caller_id", "assigned_to", "assignment_group", "sys_updated_by")


# ---- nova execução de todos -----
# id=1: baseline
# id=2: dl, dl, dl
# id=3: jaccard, jaccard, dl
# id=4: jaccard, jaccard, jaccard

set_log_file("data/test/log_runId1.txt")

id <- 1
SEARCH_SIMILAR <- FALSE
test_nf1 <- exec_all_horizons(h=HORIZONS,selected_attributes,id)
test_nf1 <- exec_all_horizons(h=c(3,5,6,7,Inf),selected_attributes,id)

close_log_file()


SAVE_MODEL <- FALSE

set_log_file("data/test/log_runId2.txt")

id <- 2
SEARCH_SIMILAR <- TRUE
SIM_METHOD_SET <- "dl" 
SIM_METHOD_MSET <- "dl"
SIM_METHOD_SEQ <- "dl"
test_nf2 <- exec_all_horizons(h=HORIZONS,selected_attributes,id)

close_log_file()


set_log_file("data/test/log_runId3.txt")

id <- 3
SEARCH_SIMILAR <- TRUE
SIM_METHOD_SET <- "jaccard" 
SIM_METHOD_MSET <- "jaccard"
SIM_METHOD_SEQ <- "dl"
test_nf3 <- exec_all_horizons(h=HORIZONS,selected_attributes,id)

close_log_file()


set_log_file("data/test/log_runId4.txt")

id <- 4
SEARCH_SIMILAR <- TRUE
SIM_METHOD_SET <- "jaccard" 
SIM_METHOD_MSET <- "jaccard"
SIM_METHOD_SEQ <- "jaccard"
test_nf4 <- exec_all_horizons(h=HORIZONS,selected_attributes,id)

close_log_file()



# ----- função para executar para uma lista de horizontes -----


exec_all_horizons <- function(h=HORIZONS,s,runId)
{
   results <- NULL
   for(i in 1:length(h))
   {
      results <- rbind(results, main(horizon=h[i],sel_attributes=s,runId))
   }
   return(results)
}

# ----- MAIN FUNCTION ------


main <- function(horizon=selected_horizon, sel_attributes=selected_attributes, runId,
                 training_fn="fold1_train.csv",validation_fn="fold1_test.csv")
{
   generate_log(" ************* Initiating EXP 3 for handling the Non-Fitting (OPT Version) *************", 1)
   generate_log(EXECUTION_DESCRIPTION)
   generate_log(paste(" REMOVE_LONGER_CASES == ",REMOVE_LONGER_CASES))
   #generate_log(paste(" INCLUDE_SOJOURN_IN_PREDICTION == ",INCLUDE_SOJOURN_IN_PREDICTION))
   if ( SEARCH_SIMILAR )
      generate_log(paste(" SIMILARITY FOR HANDLING NON-FITTING: set=", SIM_METHOD_SET, "mset=", SIM_METHOD_MSET, "seq=", SIM_METHOD_SEQ))
   else
      generate_log(paste(" SEARCH FOR SIMILARITY = ", SEARCH_SIMILAR, " (searching only exact matches)"))
   generate_log(paste("Training file: ",training_fn,"Validation file:",validation_fn))
   
   startTime <- Sys.time()
   
   # the base name for the files that will store all the results
   statsFile <- paste("results_EXP3_NF_",runId,"_",format(startTime, "%Y%m%d-%H%M"),sep="")
   
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
   training_stats <- annotate_model(trainingFold, rfn, "T", 0, horizon, model)
   
   # prediction over the validation data set
   testingFold <- read.csv(file=file.path("data", validation_fn)) #,nrows = 104)
   
   # builds the transition system for teh testing fold
   predict <- build_prediction_optimized(testingFold,model)
   validation_stats <- annotate_model(testingFold, rfn, "V", 0, horizon, model)
   
   eval_stats_arr <- rbind(training_stats, validation_stats)
   
   eval_stats_df1 <- data.frame(eval_stats_arr)
   
   string_attrib <- paste(unlist(sel_attributes),collapse=",")
   eval_stats_df1 <- cbind(start=format(startTime, "%Y-%m-%d %H:%M:%S"),end=format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                           at="laptop", attributes=string_attrib,eval_stats_df1)
   
   sfilen <- file.path("data/test",paste(statsFile,"_STATS.csv",sep=""))
   write.table(eval_stats_df1, file=sfilen, row.names=FALSE, col.names = TRUE, sep=";", dec=",")
   
   generate_log(paste("Stat file generated: [",statsFile,"_STATS.csv]",sep=""))
   
   if ( SAVE_MODEL )
      save_model(model, startTime, "data/test", statsFile)
   
   generate_log("Step completed successfully.",2)
   
   eval.parent(substitute(LAST_RUN_MODEL<-model))
   eval.parent(substitute(LAST_RUN_PREDICT<-predict))
   
   return(eval_stats_df1)
   
}









MODEL_H1 <- model
PREDICTION_H1 <- predict
TESTING_DATA <- testingFold

MODEL_H2 <- model
PREDICTION_H2 <- predict

# cenário 1: exact search, horizon 1
# cenário 2: exact search, horizon > 1
# cenário 3: similar search, horizon 1

SEARCH_SIMILAR <- TRUE
# possible methods are dl, jaccard
SIM_METHOD_SET <- "dl" 
SIM_METHOD_MSET <- "dl"
SIM_METHOD_SEQ <- "dl"

# testing
training_fn="fold1_train.csv"
validation_fn="fold1_test.csv"

aevents <- read.csv(file=file.path("data", training_fn),nrows = 104)
horiz <- 3
#sel_attributes <- "incident_state"
sel_attributes <- c("incident_state","active")

model <- build_ats(aevents,horiz,sel_attributes)

rfn <- file.path("data/test","test_pred3.csv")

training_stats <- annotate_model(aevents, rfn, "T", 0, horiz, model)

a_testingFold <- read.csv(file=file.path("data", validation_fn),nrows = 107)
a_predict <- build_prediction(a_testingFold,model)
a_testing_stats <- annotate_model(a_testingFold, rfn, "V", 0, horiz, model)

a_testingFoldOpt <- read.csv(file=file.path("data", validation_fn),nrows = 107)
a_predictOpt <- build_prediction_optimized(a_testingFoldOpt,model)
a_testingStatsOpt <- annotate_model(a_testingFoldOpt, rfn, "V", 0, horiz, model)





# --- testing performance ---

install.packages("microbenchmark")

library(microbenchmark)

SEARCH_SIMILAR <- FALSE
# possible methods are dl, jaccard
SIM_METHOD_SET <- "dl" 
SIM_METHOD_MSET <- "dl"
SIM_METHOD_SEQ <- "dl"

horiz <- 1
sel_attributes <- c("incident_state","active")

# testing
training_fn="fold1_train.csv"
validation_fn="fold1_test.csv"
rfn <- file.path("data/test","test_perf.csv")

aevents <- read.csv(file=file.path("data", training_fn),nrows = 1007)
model <- build_ats(aevents,horiz,sel_attributes)
training_stats <- annotate_model(aevents, rfn, "T", 0, horiz, model)

a_testingFold <- read.csv(file=file.path("data", validation_fn),nrows = 508)
a_perf1 <- microbenchmark(build_prediction(a_testingFold,model),times = 10)
#a_testing_stats <- annotate_model(a_testingFold, rfn, "V", 0, horiz, model)

a_testingFoldOpt <- read.csv(file=file.path("data", validation_fn),nrows = 508)
a_perf2 <- microbenchmark(build_prediction_optimized(a_testingFoldOpt,model),times = 10)
#a_testingStatsOpt <- annotate_model(a_testingFoldOpt, rfn, "V", 0, horiz, model)

SEARCH_SIMILAR <- FALSE
horiz <- 1

# > mean(a_perf2$time)
# [1] 2267680877
# > mean(a_perf1$time)
# [1] 2266240667

horiz <- 5

# > mean(a_perf1$time)
# [1] 3195302780
# > mean(a_perf2$time)
# [1] 2726507523

SEARCH_SIMILAR <- TRUE
horiz <- 1

# > mean(a_perf1$time)
# [1] 2262954575
# > mean(a_perf2$time)
# [1] 2257147251

horiz <- 5

# > mean(a_perf1$time)
# [1] 3711528307
# > mean(a_perf2$time)
# [1] 3722370742
