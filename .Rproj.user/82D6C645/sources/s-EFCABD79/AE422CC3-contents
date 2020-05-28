FAWSLEvents$location[FAWSLEvents$location != 'NULL']

FAWSLEvents$location[FAWSLEvents$location != 'NULL'][1]

unlist(FAWSLEvents$location[FAWSLEvents$location != 'NULL'][1])

unlist(FAWSLEvents$location[FAWSLEvents$location != 'NULL'][1])[1]

locationList <- FAWSLEvents$location[FAWSLEvents$location != 'NULL']

locationList_X <- c()
locationList_Y <- c()
for (i in c(1:length(locationList))){
  locationList_X <- c(locationList_X, unlist(locationList[i])[1])
  locationList_Y <- c(locationList_Y, unlist(locationList[i])[2])
}

unique(locationList_X)
max(locationList_X)
min(locationList_X)

unique(locationList_Y)
max(locationList_Y)
min(locationList_Y)

sampleMatch_ArsLiv <- FAWSLEvents[FAWSLEvents$match_id==19717,]
unique(sampleMatch_ArsLiv$type.name)

first100 <- sampleMatch_ArsLiv[1:100,]

first100$chainId <- c(1,rep(NA,nrow(first100)-1))
for (i in c(2:nrow(first100))){
  if(first100$play_pattern.id[i]==first100$play_pattern.id[i-1] & first100$possession_team.id[i]==first100$possession_team.id[i-1]){
    first100$chainId[i]=first100$chainId[i-1]
  } else {
    first100$chainId[i]=first100$chainId[i-1]+1
  }
}

View(dplyr::select(first100,chainId,minute,second,type.name,possession_team.name,play_pattern.name))

filter(sampleMatch_ArsLiv, index==max(index))

maxPossessionId <- sampleMatch_ArsLiv[
sampleMatch_ArsLiv$id %in% sapply(
  unique(sampleMatch_ArsLiv$possession),
  FUN=function(X){
    return (filter(sampleMatch_ArsLiv[sampleMatch_ArsLiv$possession==X,],index==max(index)))
  }  ),c('possession', 'index', 'timestamp', 'shot.statsbomb_xg')]

!is.na(maxPossessionId$shot.statsbomb_xg)

possessionCount <- FAWSLEvents %>% group_by(match_id) %>% summarise(possessionCount=n_distinct(possession))


print(possessionIds)

maxPossessionIds_V <- sapply(
  unique(FAWSLEvents$possession),
  FUN=function(X){
    return(filter(FAWSLEvents[FAWSLEvents$possession==X,],index==max(index[shot.statsbomb_xg])))
  }
)


for (i in c(1:nrow(xADataset_M))){
  # xADataset_M$start.X <- unlist(xADataset_M[i,'location'])[1]
  # xADataset_M$start.Y <- unlist(xADataset_M[i,'location'])[2]
  # xADataset_M$end.X <- unlist(xADataset_M[i,'pass.end_location'])[1]
  # xADataset_M$end.Y <- unlist(xADataset_M[i,'pass.end_location'])[2]
  print(paste(unlist(xADataset_M[i,'location'])[1], unlist(xADataset_M[i,'location'])[2], sep=' '))
}
