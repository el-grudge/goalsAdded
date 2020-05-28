# Introduction
library(dplyr)
library(StatsBombR)
library(randomForest)
Comp <- FreeCompetitions()
Matches <- FreeMatches(Comp)
FAWSL <- filter(Matches, competition.competition_name == 'FA Women\'s Super League')

# Get common columns
col_counter <- data.frame(x=character(0), y=numeric(0), stringsAsFactors=FALSE)
colnames(col_counter) <- c('colname', 'colcount')
`%notin%` <- Negate(`%in%`)

for (i in FAWSL$match_id){
  print(i)
  colz <- c(colnames(get.matchFree(filter(FAWSL, match_id == i))))
  for (j in 1:length(colz)){
    if(colz[j] %notin% col_counter$colname){
      col_counter[nrow(col_counter) + 1,] = list(colz[j], 1)
    } else {
      col_counter$colcount[col_counter$colname == colz[j]] <- col_counter$colcount[col_counter$colname == colz[j]]+1
    }
  }
}

# Get match events
colkeys <- col_counter$colname[col_counter$colcount == 194]

FAWSL_event <- data.frame()
for (i in FAWSL$match_id){
  print(i)
  event <- select(get.matchFree(filter(FAWSL, match_id == i)), all_of(colkeys))
  event$match_name <- paste(filter(FAWSL, match_id == i)$home_team.home_team_name,
                            'v',
                            filter(FAWSL, match_id == i)$away_team.away_team_name) 
  event$match_date <- filter(FAWSL, match_id == i)$match_date
  FAWSL_event <- rbind(FAWSL_event, event)
}

lapply(FAWSL_event, class)

FAWSLEvents <- FAWSL_event

FAWSL_event$related_events <- as.character(FAWSL_event$related_events)
FAWSL_event$location <- as.character(FAWSL_event$location)
FAWSL_event$pass.end_location <- as.character(FAWSL_event$pass.end_location)
FAWSL_event$shot.end_location <- as.character(FAWSL_event$shot.end_location)
FAWSL_event$carry.end_location <- as.character(FAWSL_event$carry.end_location)
FAWSL_event$goalkeeper.end_location <- as.character(FAWSL_event$goalkeeper.end_location)

write.table(dplyr::select(FAWSL_event, 
                          -tactics.lineup, 
                          -shot.freeze_frame,
                          ), file='FAWSL_event_2.csv')


# building xGChains:
# 1- get id of all shot ending sequences  -> maxPossessionId_M
# 2- filter out shots without key passes -> maxPossessionIdKeyPass_M
# 3- propagate xG to previous keypass as xA -> FAWSLEvents$xA
# 4- build a model using this data
# 4.5- build a clustering model to find passes that are similar to key passes
# 5- use model to calculate xA potential value for passes that didn't end in shot

# step 1
maxPossessionId_M <- data.frame()
for (i in unique(FAWSLEvents$match_id)){
  tmp_M <- FAWSLEvents[FAWSLEvents$match_id==i,]
  possessionIds <- sapply(
    unique(tmp_M$possession),
    FUN=function(X){
      return(filter(tmp_M[tmp_M$possession==X,],index==max(index))$index)
    }
  )
  maxPossessionId_M <- rbind(maxPossessionId_M, tmp_M[tmp_M$index %in% possessionIds & !is.na(tmp_M$shot.statsbomb_xg),])
}
View(maxPossessionId_M)

# step 2
maxPossessionIdKeyPass_M <- maxPossessionId_M[!is.na(maxPossessionId_M$shot.key_pass_id),]
View(maxPossessionIdKeyPass_M)

# step 3 - look for a more efficient way
FAWSLEvents$xA <- NA
# FAWSLEvents[FAWSLEvents$id %in% maxPossessionIdKeyPass_M$shot.key_pass_id,'xA']
# maxPossessionIdKeyPass_M[,c('shot.statsbomb_xg','shot.key_pass_id','id')]
# inner_join(
#   FAWSLEvents,
#   maxPossessionIdKeyPass_M,
#   by=c('id'='shot.key_pass_id')
# )
# match(FAWSLEvents$id, maxPossessionIdKeyPass_M$shot.key_pass_id)
for (i in c(1:nrow(FAWSLEvents))){
  for (j in c(1:nrow(maxPossessionIdKeyPass_M))){
    if (FAWSLEvents[i,'id'] == maxPossessionIdKeyPass_M[j,'shot.key_pass_id']){
      FAWSLEvents[i,'xA'] = maxPossessionIdKeyPass_M[j,'shot.statsbomb_xg']
    }
  }
}
FAWSLEvents[!is.na(FAWSLEvents$xA),]

# step 4
xADataset_M <- FAWSLEvents[!is.na(FAWSLEvents$xA),]
# variable selection
xADataset_M <- select(xADataset_M,
                      xA,
                      location,
                      under_pressure,
                      counterpress,
                      play_pattern.name,
                      starts_with('pass'),
                      -pass.assisted_shot_id,
                      -pass.shot_assist,
                      -pass.recipient.id,
                      -pass.recipient.name,
                      -pass.height.id,
                      -pass.type.id,
                      -pass.body_part.id,
                      -pass.outcome.id
                      )
       
xADataset_M$start.X <- NA
xADataset_M$start.Y <- NA
xADataset_M$end.X <- NA
xADataset_M$end.Y <- NA
for (i in c(1:nrow(xADataset_M))){
  xADataset_M[i, 'start.X'] <- unlist(xADataset_M[i,'location'])[1]
  xADataset_M[i, 'start.Y'] <- unlist(xADataset_M[i,'location'])[2]
  xADataset_M[i, 'end.X'] <- unlist(xADataset_M[i,'pass.end_location'])[1]
  xADataset_M[i, 'end.Y'] <- unlist(xADataset_M[i,'pass.end_location'])[2]
}

xADataset_M <- select(xADataset_M, -location, -pass.end_location)
summary(xADataset_M)

xADataset_M <- select(xADataset_M, -under_pressure, -counterpress, -pass.cross, -pass.switch, -pass.type.name, -pass.outcome.name)

# splitting the datasets to train (75%) & test (25%)
set.seed(2)
trainXA_M=sample(1:nrow(xADataset_M), 0.75*nrow(xADataset_M))
testXA_M=xADataset_M[-trainXA_M,]
real_target=xADataset_M$xA[-trainXA_M]

# missing values

apply(is.na(xADataset_M), 2, sum)
xADataset_M[is.na(xADataset_M$pass.body_part.name),'pass.body_part.name'] <- 'Other'

############################################################
# Random forest
# randomforest, uses less variables so mtry should be smaller
# mtry (number of featuers considered by model): for regression p=p/3
# mtry (number of featuers considered by model): for classification p=sqrt(p)
# !!! handle categorical columns !!!
xA_rf_model=randomForest(xA~.,
                    data=xADataset_M,
                    subset=trainXA_M,
                    mtry=length(colnames(xADataset_M))/3,
                    importance=TRUE)

predictedXA_M=predict(xA_rf_model, newdata=xADataset_M[-trainXA_M,])
confusionMatrix_rf <- table(predictedXA_M, real_target)
confusionMatrix_rf[1]+confusionMatrix_rf[4]/sum(confusionMatrix_rf) # a higher accuracy of 71 %

# importance to see importance of each variable
importance(xA_rf_model)

# plotting importance
varImpPlot(xA_rf_model)