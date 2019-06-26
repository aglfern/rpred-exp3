# Novo algoritmo de geracaoo dos datasets
#

# ******************* Testing area ***************************
testing <- function()
{
   dataset_size_list <- c(5, 8, 12);
   r <- dataset_splitting_indexes(dataset_size_list)
   r
   rm(r,dataset_size_list)

   xtable <- read.table(file.path('data','novo_log_sara.csv'), quote = "\"", sep = ",", dec = ".",
                        header = T, encoding = "utf-8", stringsAsFactors = T, nrow = 100)
   require(dplyr)
   xdistinct <- distinct(xtable,number)
   nrow(xdistinct)
   xdsize <- c(3, 4, 10)
   xsplitted <- dataset_split(xdistinct,xdsize)
   sum(xdsize[1:2])
   xdistinct[c(2,4,6),]
   as.list(xsplitted[[1]])
   rm(xtable,xdistinct,xsplitted)

   create_kfold_files(xtable,5,1)
}



# ******************* Testing area ***************************



#' dataset_splitting_indexes
#' Returns randomized lists of indexes, based on the number and sizes indicated
#' @param datasets_sizes_list: a list containing the desired sizes of the splitted datasets
#'
#' @return
#' @export
#'
#' @examples
dataset_splitting_indexes <- function(ds_sizes_list)
{
   generate_log(paste("Generating random indexes for ", length(ds_sizes_list), " smaller datasets."),1)
   start <- 1; end <- 0
   random_indexes <- sample(sum(ds_sizes_list))
   datasets_indexes <- vector("list",length(ds_sizes_list))
   for(i in 1:length(ds_sizes_list))
   {
      end = end + ds_sizes_list[i]
      generate_log(paste("start: ",start," end: ", end),3)
      datasets_indexes[[i]] <- random_indexes[start:end]
      start = start + ds_sizes_list[i]
   }
   return(datasets_indexes)
}

#' Splits a dataset into smaller datasets according to the sizes requested
#'
#' @param dataset_to_split
#' @param splitting_sizes
#'
#' @return
#' @export
#'
#' @examples
dataset_split <- function(dataset_to_split, splitting_sizes)
{
   ds_size <- nrow(dataset_to_split)
   number_of_splits <- length(splitting_sizes)
   generate_log(paste("Splitting dataset of size ",ds_size),1)
   if(sum(splitting_sizes)>ds_size)
   {
      x <- sum(splitting_sizes[1:(number_of_splits-1)])
      splitting_sizes[number_of_splits] <- ds_size - x
      generate_log(paste("Size of last subset must not be greater than ", splitting_sizes[number_of_splits]))
   }
   indexes <- dataset_splitting_indexes(splitting_sizes)
   generate_log(paste("Indexes: ",indexes),3)
   splitted_datasets <- vector("list",number_of_splits)
   for(i in 1:number_of_splits)
   {
      splitted_datasets[[i]] <- dataset_to_split[indexes[[i]],]
   }
   return(splitted_datasets)
}

#' Function pre-processes the given dataset
#' This function is specific for a dataset format, as it references directly the attribute names
#' and perform actions that relies on previous knowledge of the input data.
#'
#' Version1 handles file versions 1 and 2:
#'  file version 1: original source file from Claudio's work
#'  file version 2: source file edited by Sara
#'
#' @param ds given dataset
#' @param fileVersion int value = 1 for file version 1, int value = 2 for file version 2
#'
#' @return a new ordered dataset with new columns and pre-processed data
#' @export
#'
#' @examples
dataset_preproc_version1 <- function(ds, fileVersion)
{
   require(data.table)
   dateFormat <- "%d/%m/%Y %H:%M:%S"
   if ( fileVersion == 1 )
   {
      # arquivo original Claudio
      # principais colunas
      date_columns = c(created = "sys_created_on", opened = "opened_at", updated = "sys_updated_on", resolved = "resolved_at", closed = "closed_at")
      ds[,"activity_due"] <- as.POSIXct(strptime(ds[,"activity_due"], "%d/%m/%Y %H:%M:%S"))
      ds[,"vendor_closed_at"] <- as.POSIXct(strptime(ds[,"vendor_closed_at"], "%d/%m/%Y %H:%M:%S"))
      ds[,"vendor_opened_at"] <- as.POSIXct(strptime(ds[,"vendor_opened_at"], "%d/%m/%Y %H:%M:%S"))
      ds[,"vendor_resolved_at"] <- as.POSIXct(strptime(ds[,"vendor_resolved_at"], "%d/%m/%Y %H:%M:%S"))
   } else if ( fileVersion == 2 ) {
      # arquivo novo sara
      # principais colunas
      date_columns = c(created = "sys_created_at", opened = "opened_at", updated = "sys_updated_at", resolved = "resolved_at", closed = "closed_at")
      dateFormat <- "%d/%m/%Y %H:%M"
      # outras colunas especificas desse arquivo

   }

   # faz o mesmo set acima, para as colunas principais
   for(i in 1:length(date_columns)) {
      ds[,date_columns[i]] <- as.POSIXct(strptime(ds[,date_columns[i]], dateFormat))
   }

   #  cria nova coluna para deixar generico no codigo
   ds$updated_at <- ds$sys_updated_at

   # cria campos com valor inteiro para a contagem do tempo
   ds$created_at_stc <- as.integer(ds[, date_columns["created"]])
   ds$updated_at_stc <- as.integer(ds[, date_columns["updated"]])
   ds$opened_at_stc <- as.integer(ds[, date_columns["opened"]])
   ds$resolved_at_stc <- as.integer(ds[, date_columns["resolved"]])
   ds$closed_at_stc <- as.integer(ds[, date_columns["closed"]])

   # gera contadores para modelo MTA
   # elapsed: opcao 1 = updated - opened; opcao 2 = updated - created
   ds$elapsed_stc <- ds$updated_at_stc - ds$opened_at_stc
   #incidentevtlog$elapsed_stc <- incidentevtlog$updated_at_stc - incidentevtlog$created_at_stc

   # remaining: opcao 1 = closed - updated; opcao 2 = resolved - updated
   ds$remaining_stc <- ds$closed_at_stc - ds$updated_at_stc
   #incidentevtlog$remaining_stc <- incidentevtlog$resolved_at_stc - incidentevtlog$updated_at_stc

   # cria e inicializa - tempo de sojourn a ser preenchido na construcao do modelo
   ds$sojourn_set_stc <-  NULL
   ds$sojourn_mset_stc <-  NULL
   ds$sojourn_seq_stc <-  NULL
   ds$sojourn_set_stc <-  0
   ds$sojourn_mset_stc <-  0
   ds$sojourn_seq_stc <-  0

   #cria colunas para os estados
   ds$seq_state_id <- NULL
   ds$set_state_id <- NULL
   ds$mset_state_id <- NULL
   ds$seq_state_id <- 0
   ds$set_state_id <- 0
   ds$mset_state_id <- 0

   # reorders the dataset by the given columns (it is optimized for memory, does not create a copy)
   setorder(ds,number,updated_at_stc)

   dt <- as.data.table(ds)
   setkey(dt,number)
   dt_ord <- dt[,list(closedat=max(closed_at_stc),closedcode=first(closed_code)),by=number]

   # replica tempo de conclusao para todos os registros das variaveis target
   dt$closed_at_stc <- dt_ord$closedat[match(dt$number,dt_ord$number)]
   dt$closed_code <- dt_ord$closedcode[match(dt$number,dt_ord$number)]

   # campos "resolved" não estão sendo atualizados
#   incidentevtlog$resolved_at <- DTUNIQR$updated_at[match(incidentevtlog$number, DTUNIQR$number)]
#   incidentevtlog$resolved_by <- DTUNIQR$resolved_by[match(incidentevtlog$number, DTUNIQR$number)]


   return (ds)
}

#' Title
#'
#' @param ds
#' @param k
#' @param numberOfTimes
#'
#' @return
#' @export
#'
#' @examples
generate_kfolds <- function(ds,k,numberOfTimes=1)
{
   ds_<-as.data.table(ds[sample(nrow(ds)),])

   #Create k equally size folds
   folds <- cut(seq(1,nrow(ds_)),breaks=k,labels=FALSE)

   # Saves the k fold files
   training_set <- vector("list",5)
   testing_set <- vector("list",5)
   for(i in 1:k){
      #Segment your data by fold using the which() function
      testIndexes <- which(folds==i,arr.ind=TRUE)
      testData <- ds_[testIndexes, ]
      trainData <- ds_[-testIndexes, ]
      training_set[i] <- trainData
      testing_set[i] <- testData
   }
   folds_list <- list(trainingSet = training_set, testingSet = testing_set)
   return(folds_list)
}

