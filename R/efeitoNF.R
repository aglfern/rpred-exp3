# testing effects of removing the prediction on NIL_STATE (not fitted)

basePath <- "C:/Mestrado/NovaVersao/Resultados/Exp2-Passo2-RankingCorrel/"

# load all the prediction files in the basePath.

theFolders <- list.files(path = basePath, include.dirs = TRUE, no.. = FALSE)
predFileNames <- NULL
for( i in 1:length(theFolders) ) {
   predFileNames <- rbind(predFileNames, cbind(theFolders[i],
                     list.files(path = paste(basePath,theFolders[i],"/", sep = ""),
                                pattern = "*_pred.csv", all.files = FALSE, full.names = TRUE,
                                recursive = FALSE, ignore.case = FALSE, include.dirs = FALSE, no.. = TRUE)))

}

previousErrorTable <- errorTable

errorTable <- NULL
for ( i in 1:nrow(predFileNames) )
{
   predFile <- load_prediction((predFileNames[i,2]))
   currentLine <- cbind(predFileNames[i,1],predFileNames[i,2])
   erros <- calculate_errors_without_non_fitting(predFile,predFileNames[i,2])
   currentLine <- cbind(currentLine, erros)
   errorTable <- rbind(errorTable, currentLine)
}

write.table(errorTable, file="Error_table_RNF_v3.csv", row.names=FALSE, col.names = TRUE, sep=";", dec=",", quote = FALSE, append = FALSE)


# Remove eventos que não fizeram match com nenhum estado do modelo criado do cálculo
# do erro da predição, gerando como saída uma linha com as seguintes colunas
# RNF RMSPE SET 	 RNF RMSPE MSET 	 RNF RMSPE SEQ 	 RNF MAPE SET% 	 RNF MAPE MSET% 	 RNF MAPE SEQ% CROSSCHECK
# A última coluna CROSSCHECK é a soma dos erros RMSPE das predições originais de SET, MSET e SEQ
calculate_errors_without_non_fitting <- function(efeitoNF,fileName)
{
   generate_log(paste("Calculando erro sem non-fitting para arquivo [",fileName,"]"))

   # Tabela final deve ter este formato
   # RNF RMSPE SET 	 RNF RMSPE MSET 	 RNF RMSPE SEQ 	 RNF MAPE SET% 	 RNF MAPE MSET% 	 RNF MAPE SEQ%
   efeitoNF_v <- efeitoNF[which(efeitoNF$type=="V" & efeitoNF$remaining_stc>0),]

   # este efeito está sendo calculado sobre os eventos do data set de validação, excluindo-se
   # o último evento do case, pois nesse caso o remaining real é zero.
   efeitoNF_fitted_set <- efeitoNF_v[which(efeitoNF_v$set_state_id!=NIL_STATE),]
   efeitoNF_fitted_mset <- efeitoNF_v[which(efeitoNF_v$mset_state_id!=NIL_STATE),]
   efeitoNF_fitted_seq <- efeitoNF_v[which(efeitoNF_v$seq_state_id!=NIL_STATE),]

   tabela_RNF <- NULL  # inicializa

   tabela_RNF <- cbind(tabela_RNF,
                       "RNF_RMSPE_SET"=RMSPE(efeitoNF_fitted_set$remaining_stc_pset_mean,efeitoNF_fitted_set$remaining_stc),
                       "RNF_RMSPE_MSET"=RMSPE(efeitoNF_fitted_mset$remaining_stc_pmset_mean,efeitoNF_fitted_mset$remaining_stc),
                       "RNF_RMSPE_SEQ"=RMSPE(efeitoNF_fitted_seq$remaining_stc_pseq_mean,efeitoNF_fitted_seq$remaining_stc))

   tabela_RNF <- cbind(tabela_RNF,
                       "RNF_MAPE_SET"=100*MAPE(efeitoNF_fitted_set$remaining_stc_pset_mean,efeitoNF_fitted_set$remaining_stc),
                       "RNF_MAPE_MSET%"=100*MAPE(efeitoNF_fitted_mset$remaining_stc_pmset_mean,efeitoNF_fitted_mset$remaining_stc),
                       "RNF_MAPE_SEQ%"=100*MAPE(efeitoNF_fitted_seq$remaining_stc_pseq_mean,efeitoNF_fitted_seq$remaining_stc))

   # crosscheck - vou somar os erros RMSPE das predições originais para comparar no final e garantir que são da mesma execução
   erro_pred_orig <- RMSPE(efeitoNF_v$remaining_stc_pset_mean, efeitoNF_v$remaining_stc) +
                     RMSPE(efeitoNF_v$remaining_stc_pmset_mean, efeitoNF_v$remaining_stc) +
                     RMSPE(efeitoNF_v$remaining_stc_pseq_mean, efeitoNF_v$remaining_stc)

   tabela_RNF <- cbind(tabela_RNF,"CROSSCHECK"=erro_pred_orig)

   # este efeito está sendo calculado sobre os eventos do data set de validação, excluindo-se
   # o último evento do case, pois nesse caso o remaining real é zero.
   efeitoNF_nonfitted_set <- efeitoNF_v[which(efeitoNF_v$set_state_id==NIL_STATE),]
   efeitoNF_nonfitted_mset <- efeitoNF_v[which(efeitoNF_v$mset_state_id==NIL_STATE),]
   efeitoNF_nonfitted_seq <- efeitoNF_v[which(efeitoNF_v$seq_state_id==NIL_STATE),]

   tabela_RNF <- cbind(tabela_RNF,
                       "NFO_RMSPE_SET"=RMSPE(efeitoNF_nonfitted_set$remaining_stc_pset_mean,efeitoNF_nonfitted_set$remaining_stc),
                       "NFO_RMSPE_MSET"=RMSPE(efeitoNF_nonfitted_mset$remaining_stc_pmset_mean,efeitoNF_nonfitted_mset$remaining_stc),
                       "NFO_RMSPE_SEQ"=RMSPE(efeitoNF_nonfitted_seq$remaining_stc_pseq_mean,efeitoNF_nonfitted_seq$remaining_stc))

   # additional statistic information about the non-fitting events
   # gets all valitation events
   efeitoNF_v <- efeitoNF[which(efeitoNF$type=="V"),]

   nfstats_set <- calculate_nf_stats(efeitoNF_v, "number", "set_state_id")
   nfstats_mset <- calculate_nf_stats(efeitoNF_v, "number", "mset_state_id")
   nfstats_seq <- calculate_nf_stats(efeitoNF_v, "number", "seq_state_id")

   tabela_RNF <- cbind(tabela_RNF, "SET_NF_END2END"=nfstats_set[1], "SET_NF_MIXED"=nfstats_set[2],
                                   "MSET_NF_END2END"=nfstats_mset[1], "MSET_NF_MIXED"=nfstats_mset[2],
                                   "SEQ_NF_END2END"=nfstats_seq[1], "SEQ_NF_MIXED"=nfstats_seq[2])


   return(tabela_RNF)

}

# --- testing the calculate nf stats function
file1 <- load_prediction("C:/Mestrado/NovaVersao/Resultados/Exp2-Passo2-RankingCorrel/04/results_EXP2_20K_S02_4_20190418-1213_pred.csv")
file1_v <- file1[which(file1$type=="V"),]
st1 <- calculate_nf_stats(file1_v, "number", "set_state_id")

file2 <- load_prediction("C:/Mestrado/NovaVersao/Resultados/Exp2-Passo2-RankingCorrel/14/results_EXP2_20K_S02_14_20190428-1738_pred.csv")
file2_v <- file2[which(file2$type=="V"),]
st2 <- calculate_nf_stats(file2_v, "number", "set_state_id")

rm(file1, file1_v, st1, file2, file2_v, st2)

# --- calculate nf stats function
calculate_nf_stats <- function(dataset, grouping, var, DROP=TRUE, NA.RM=TRUE)
{
   nfstats <- ddply(dataset, grouping, .drop=DROP,
                    .fun = function(xx, col) {
                       c( sum   = ceiling(sum (xx[[col]], na.rm=NA.RM)),
                          mean  = ceiling(mean(xx[[col]], na.rm=NA.RM)),
                          min   = ceiling(min (xx[[col]], na.rm=NA.RM)),
                          max   = ceiling(max (xx[[col]], na.rm=NA.RM)),
                          count = length(xx[[col]])
                       )
                    },
                    var
   )
   nf_endtoend <- nrow(nfstats[which(nfstats$min==-1 & nfstats$max==-1),])
   nf_mixed <- nrow(nfstats[which(nfstats$min==-1 & nfstats$max>0),])

   return(c(nf_endtoend,nf_mixed))
}



# Função que calcula médias a partir dos eventos finais dos cases, para criar um
# benchmark de predição somente por média. Retorna os erros da predição sobre o
# o conjunto de treinamento, usando RMSPE e MAPE.
# Também calcula removendo outliers, que são classificados como sendo os cases
# que estão na faixa acima de 99% do tempo total do case.
# Em teoria esta função poderia ser executada somente uma vez já que os dados
# de treinamento usados foram sempre os mesmos.
calcula_medias_treinamentos <- function(dadosExecucao, percOutliers=0.99)
{
   generate_log("Iniciando cálculo de dados de predição por média simples")
   TABELA_ERROS <- NULL

   # calcular média histórica usando a base de treinamento
   # seleciona apenas os eventos finais (remaining = 0) e que tiveram duração maior que zero
   eventosFinais <- dadosExecucao[which(dadosExecucao$type=="T" &
                                           dadosExecucao$remaining_stc==0 &
                                           dadosExecucao$elapsed_stc>0),]

   generate_log(paste("Total de cases do conjunto de dados: [",nrow(eventosFinais),"]"))

   summary(eventosFinais$elapsed_stc)
   sd(eventosFinais$elapsed_stc)
   quantile(eventosFinais$elapsed_stc,.90) #  1.919.928
   quantile(eventosFinais$elapsed_stc,.95) #  3.117.276
   quantile(eventosFinais$elapsed_stc,.96) #  3.737.148
   quantile(eventosFinais$elapsed_stc,.97) #  4.865.009
   quantile(eventosFinais$elapsed_stc,.98) #  6.645.703
   quantile(eventosFinais$elapsed_stc,.99) # 10.520.472

   media <- mean(eventosFinais$elapsed_stc)
   RMSPE_media <- RMSPE(media,eventosFinais$elapsed_stc)
   MAPE_media <- MAPE(media,eventosFinais$elapsed_stc) * 100

   TABELA_ERROS <- rbind(TABELA_ERROS,
                         rmspe_pred_por_media=c(set=RMSPE_media,mset=RMSPE_media,seq=RMSPE_media))
   TABELA_ERROS <- rbind(TABELA_ERROS,
                         mape_pred_por_media=c(MAPE_media, MAPE_media, MAPE_media))

   limiteOutlier <- quantile(eventosFinais$elapsed_stc,percOutliers)
   outliers <- eventosFinais[which(eventosFinais$elapsed_stc > limiteOutlier),] # 201
   eventosFinaisSemOutliers <- eventosFinais[which(eventosFinais$elapsed_stc < limiteOutlier),] # 19836
   generate_log(paste("Removendo ",(1-percOutliers)*100,"% dos casos, que são os com tempo total de duração > ",limiteOutlier,"."))
   generate_log(paste("Removidos um total de [", nrow(outliers), "] cases."))

   media <- mean(eventosFinaisSemOutliers$elapsed_stc)
   RMSPE_media <- RMSPE(media,eventosFinaisSemOutliers$elapsed_stc)
   MAPE_media <- MAPE(media,eventosFinaisSemOutliers$elapsed_stc) * 100

   TABELA_ERROS <- rbind(TABELA_ERROS,
                         rmspe_pred_por_media_sem_out=c(set=RMSPE_media,mset=RMSPE_media,seq=RMSPE_media))
   TABELA_ERROS <- rbind(TABELA_ERROS,
                         mape_pred_por_media_sem_out=c(MAPE_media, MAPE_media, MAPE_media))

   generate_log("Tabela de erros inicializada.")
   return(TABELA_ERROS)
}


# ------ gráfico benchmark -----
#  gera gráfico da distribuição da duração dos casos
#
# seleciona apenas os eventos finais (remaining = 0) e que tiveram duração maior que zero
eventosFinais <- efeitoNF[which(efeitoNF$type=="T" &
                                        efeitoNF$remaining_stc==0 &
                                        efeitoNF$elapsed_stc>0),]

x <- as.numeric(rownames(eventosFinais))
y <- NULL
for(i in 1:nrow(eventosFinais))
   y <- rbind(y,RMSPE(media,eventosFinais$elapsed_stc[i]))

summary(y)

qplot(x=x, y=y,  main = "Scattered MEAN Prediction Error - training data",
      xlab = "Cases", ylab = "Individual RMSPE", ylim = c(0,10))
# 10.000 contém todos
# 1.000, elimina 10 linhas
# 100, elimina 23 linhas
# 10, elimina 59 linhas

mean(y) # [1] 3.359938
sd(y) # [1] 117.826
RMSPE(media,eventosFinais$elapsed_stc) # [1] 117.826 (!!!!)


# fica claro que os outliers afetam demais o RMSPE
# será que têm o mesmo efeito para o MAPE? RESP: Não

ym <- NULL
for(i in 1:nrow(eventosFinais))
   ym <- rbind(ym,MAPE(media,eventosFinais$elapsed_stc[i]))

summary(ym)
sd(ym) # [1] 117.8274
MAPE(media,eventosFinais$elapsed_stc) # [1] 3.359938


#
# ----- Testes - Cálculos das predições original e com remoção dos NF -------



efeitoNF_v <- efeitoNF[which(efeitoNF$type=="V" & efeitoNF$remaining_stc>0),]
erro_pred_orig <- cbind(set= RMSPE(efeitoNF_v$remaining_stc_pset_mean, efeitoNF_v$remaining_stc),
                        mset=RMSPE(efeitoNF_v$remaining_stc_pmset_mean, efeitoNF_v$remaining_stc),
                        seq= RMSPE(efeitoNF_v$remaining_stc_pseq_mean, efeitoNF_v$remaining_stc))

# linha três da tabela de erros
tabela_erros <- rbind(tabela_erros,erro_pred_orig)
#rownames(tabela_erros) <- c("rmspe_pred_orig")

erro_pred_orig <- cbind(set= MAPE(efeitoNF_v$remaining_stc_pset_mean, efeitoNF_v$remaining_stc),
                        mset=MAPE(efeitoNF_v$remaining_stc_pmset_mean, efeitoNF_v$remaining_stc),
                        seq= MAPE(efeitoNF_v$remaining_stc_pseq_mean, efeitoNF_v$remaining_stc))

# linha três da tabela de erros
tabela_erros <- rbind(tabela_erros,erro_pred_orig)
tabela_erros[4,] <- tabela_erros[4,]*100

rownames(tabela_erros) <- c("rmspe_pred_por_media","mape_pred_por_media","rmspe_pred_orig","rmspe_pred_orig")

# este efeito está sendo calculado sobre os eventos do data set de validação, excluindo-se o último evento do case,
# pois nesse caso o remaining real é zero.
efeitoNF_fitted_set <- efeitoNF_v[which(efeitoNF_v$set_state_id!=NIL_STATE),]

#RMSPE(y_pred, y_true)
tabela_erros <- rbind(tabela_erros,rmspe_pred_efeitoNF=
                         c(set=RMSPE(efeitoNF_fitted_set$remaining_stc_pset_mean,efeitoNF_fitted_set$remaining_stc),
                           mset=RMSPE(efeitoNF_fitted_set$remaining_stc_pmset_mean,efeitoNF_fitted_set$remaining_stc),
                           seq=RMSPE(efeitoNF_fitted_set$remaining_stc_pseq_mean,efeitoNF_fitted_set$remaining_stc)))

tabela_erros <- rbind(tabela_erros,mape_pred_efeitoNF=
                         c(set=MAPE(efeitoNF_fitted_set$remaining_stc_pset_mean,efeitoNF_fitted_set$remaining_stc),
                           mset=MAPE(efeitoNF_fitted_set$remaining_stc_pmset_mean,efeitoNF_fitted_set$remaining_stc),
                           seq=MAPE(efeitoNF_fitted_set$remaining_stc_pseq_mean,efeitoNF_fitted_set$remaining_stc)))

tabela_erros[6,] <- tabela_erros[6,]*100

RMSPE(efeitoNF_fitted_set$remaining_stc_pset_mean,efeitoNF_fitted_set$remaining_stc)
MAPE(efeitoNF_fitted_set$remaining_stc_pset_mean,efeitoNF_fitted_set$remaining_stc) * 100

x <- as.numeric(rownames(efeitoNF_v))
y <- NULL
for(i in 1:nrow(efeitoNF_v))
   y <- rbind(y,RMSPE(efeitoNF_v$remaining_stc_pset_mean[i],efeitoNF_v$remaining_stc[i]))


# confirmar: será que a média que está sendo gerada para o estado -1 é mesmo a do data set de validação?
#

# graphics
plot(x, y, main = "Scattered SET Prediction Error - caller_id,assigned_to - HORIZON 1",
     xlab = "Events", ylab = "Individual SET RMSPE",
     pch = 1, frame = FALSE, ylim = c(0,100), y)

library(ggplot2)

qplot(x=x, y=y,  main = "Scattered SET Prediction Error - caller_id,assigned_to - HORIZON 1",
      xlab = "Events", ylab = "Individual SET RMSPE", ylim = c(0,1000))
# 4 outliers

qplot(x=x, y=y,  main = "Scattered SET Prediction Error - caller_id,assigned_to - HORIZON 1",
      xlab = "Events", ylab = "Individual SET RMSPE", ylim = c(0,100))
# 24 outliers

qplot(x=x, y=y,  main = "Scattered SET Prediction Error - caller_id,assigned_to - HORIZON 1",
      xlab = "Events", ylab = "Individual SET RMSPE", ylim = c(0,10))
# 74 outliers







# ------  testes iniciais com um caso específico (reunião) ------

# carregar um que tenha muito NF
#"usp";"caller_id,assigned_to,assignment_group,sys_updated_by,u_symptom";"T";"1"
fn <- "C:/Mestrado/NovaVersao/Resultados/Exp2-Passo2-RankingCorrel/05/results_EXP2_20K_S02_5_20190418-2353_pred.csv"
efeitoNF <- load_prediction(fn)


efeitoNF_v <- efeitoNF[which(efeitoNF$type=="V" & efeitoNF$remaining_stc>0),]
summary(efeitoNF_v$remaining_stc)

efeitoNF_fitted_set <- efeitoNF_v[which(efeitoNF_v$set_state_id!=NIL_STATE),]


#RMSPE(y_pred, y_true)
RMSPE(efeitoNF_fitted_set$remaining_stc_pset_mean,efeitoNF_fitted_set$remaining_stc)
MAPE(efeitoNF_fitted_set$remaining_stc_pset_mean,efeitoNF_fitted_set$remaining_stc) * 100

summary(efeitoNF_fitted_set$remaining_stc_pset_mean)
summary(efeitoNF_fitted_set$remaining_stc)

d1 <- distinct(efeitoNF_v,efeitoNF_v$number)  # 4915 cases
d2 <- distinct(efeitoNF_fitted_set,efeitoNF_fitted_set$number) # 1021 cases
1021/4915 # 20,7%



# ----- iniciando modelo mais sistemático -----

# "usp";"caller_id,assigned_to";"1"

fn <- "C:/Mestrado/NovaVersao/Resultados/Exp2-Passo2-RankingCorrel/02/results_EXP2_20K_S02_2_20190415-2340_pred.csv"
efeitoNF <- load_prediction(fn)

efeitoNF_t <- efeitoNF[which(efeitoNF$type=="T" & efeitoNF$remaining_stc > 0),]
mean(efeitoNF_t$remaining_stc)

RMSPE(1103056,efeitoNF_t$remaining_stc) # [1] 252.0569%

efeitoNF_v <- efeitoNF[which(efeitoNF$type=="V" & efeitoNF$remaining_stc > 0),]
nrow(efeitoNF_v)
RMSPE(1103056,efeitoNF_v$remaining_stc) # [1] 179.7905

eventosFinais <- efeitoNF[which(efeitoNF$type=="T" &
                                   efeitoNF$remaining_stc==0 &
                                   efeitoNF$elapsed_stc>0),]
nrow(eventosFinais)
media <- mean(eventosFinais$elapsed_stc)
media # [1] 1.133.348

efeitoNF_v2 <- efeitoNF[which(efeitoNF$type=="V" & efeitoNF$elapsed_stc > 0),]

RMSPE_media <- RMSPE(media,efeitoNF_v2$elapsed_stc) # [1] 2845.672

RMSPE(1133348,efeitoNF_v$remaining_stc) # [1] 184.7284


#[1] 1103056

# inicializa a tabela com os dados fundamentais de benchmarking
TABELA_ERROS <- calcula_medias_treinamentos(efeitoNF,0.98)
# [1] "2019-05-19 14:08:38 : Iniciando cálculo de dados de predição por média simples"
# [1] "2019-05-19 14:08:38 : Total de cases do conjunto de dados: [ 20037 ]"
# [1] "2019-05-19 14:08:38 : Removendo  2 % dos casos, que são os com tempo total de duração >  6645703.2 ."
# [1] "2019-05-19 14:08:38 : Removidos um total de [ 401 ] cases."
# [1] "2019-05-19 14:08:38 : Tabela de erros inicializada."

# "usp";"caller_id,assigned_to,assignment_group,sys_updated_by,u_symptom";"5"
fn <- "C:/Mestrado/NovaVersao/Resultados/Exp2-Passo2-RankingCorrel/05/results_EXP2_20K_S02_5_20190419-0654_pred.csv"
efeitoNF <- load_prediction(fn)

# inicializa a tabela com os dados fundamentais de benchmarking
TABELA_ERROS2 <- calcula_medias_treinamentos(efeitoNF,0.90)  # validando
# [1] "2019-05-19 14:09:50 : Iniciando cálculo de dados de predição por média simples"
# [1] "2019-05-19 14:09:50 : Total de cases do conjunto de dados: [ 20037 ]"
# [1] "2019-05-19 14:09:50 : Removendo  10 % dos casos, que são os com tempo total de duração >  1919928 ."
# [1] "2019-05-19 14:09:50 : Removidos um total de [ 2004 ] cases."
# [1] "2019-05-19 14:09:50 : Tabela de erros inicializada."

outliers <- eventosFinais[which(eventosFinais$elapsed_stc > 1919928),]


# testes para entender o comportamento dos non-fitting

efeitoNF_v <- efeitoNF[which(efeitoNF$type=="V"),]
efeitoNF_t <- efeitoNF[which(efeitoNF$type=="T"),]

cases <- distinct(efeitoNF_v,efeitoNF_v$number)

NA.RM <- TRUE
nfstats <- ddply(efeitoNF_v, "number", .drop=TRUE,
               .fun = function(xx, col) {
                  c( sum   = ceiling(sum (xx[[col]], na.rm=NA.RM)),
                     mean  = ceiling(mean(xx[[col]], na.rm=NA.RM)),
                     min   = ceiling(min (xx[[col]], na.rm=NA.RM)),
                     max   = ceiling(max (xx[[col]], na.rm=NA.RM)),
                     count = length(xx[[col]])
                  )
               },
               "set_state_id"
)

nfstats <- calculate_nf_stats(efeitoNF_v, "number", "set_state_id")

nf_endtoend <- nfstats[which(nfstats$min==-1 & nfstats$max==-1),]
nf_mixed <- nfstats[which(nfstats$min==-1 & nfstats$max>0),]



#efeitoNF_endtoend <- efeitoNF[which(efeitoNF$)]
