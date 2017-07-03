

preFlopD <- function(hand, numPlayers, JamesPos){
  lineNum <- grep("Pre Flop:", hand)
  
  line <- strsplit(hand[lineNum], " ")
  
  pot <- line[[1]][4]
  
  JamesCard1 <- line[[1]][8]
  JamesCard2 <- line[[1]][9]
  
  actionCount <- length(grep("Hero", hand[lineNum + 1]))

    
  actionData <- list()
  
  for(i in actionCount + 1){
    actionData[[i]] <- data.frame()
  }

  return(list(pot = pot, JamesCard1 = JamesCard1, JamesCard2 = JamesCard2))
}

setPlayerNames <- function(df, numPlayers, JamesPos){
  
  df[1,c(18,28,38,48,58)] <- "None"
  
  if(numPlayers == 6){
    if(JamesPos == "UTG"){
      df$Op1Pos <- "BB"
      df$Op2Pos <- "SB"
      df$Op3Pos <- "BTN"
      df$Op4Pos <- "CO"
      df$Op5Pos <- "MP"
    }else if(JamesPos == "MP"){
      df$Op1Pos <- "UTG"
      df$Op2Pos <- "BB"
      df$Op3Pos <- "SB"
      df$Op4Pos <- "BTN"
      df$Op5Pos <- "CO"
    }else if(JamesPos == "CO"){
      df$Op1Pos <- "MP"
      df$Op2Pos <- "UTG"
      df$Op3Pos <- "BB"
      df$Op4Pos <- "SB"
      df$Op5Pos <- "BTN"
    }else if(JamesPos == "BTN"){
      df$Op1Pos <- "CO"
      df$Op2Pos <- "MP"
      df$Op3Pos <- "UTG"
      df$Op4Pos <- "BB"
      df$Op5Pos <- "SB"
    }else if(JamesPos == "SB"){
      df$Op1Pos <- "BTN"
      df$Op2Pos <- "CO"
      df$Op3Pos <- "MP"
      df$Op4Pos <- "UTG"
      df$Op5Pos <- "BB"
    }else{
      df$Op1Pos <- "SB"
      df$Op2Pos <- "BTN"
      df$Op3Pos <- "CO"
      df$Op4Pos <- "MP"
      df$Op5Pos <- "UTG"
    }
  }else if(numPlayers == 5){
    if(JamesPos == "UTG"){
      df$Op1Pos <- "BB"
      df$Op2Pos <- "SB"
      df$Op3Pos <- "BTN"
      df$Op4Pos <- "CO"
    }else if(JamesPos == "CO"){
      df$Op1Pos <- "UTG"
      df$Op2Pos <- "BB"
      df$Op3Pos <- "SB"
      df$Op4Pos <- "BTN"
    }else if(JamesPos == "BTN"){
      df$Op1Pos <- "CO"
      df$Op2Pos <- "UTG"
      df$Op3Pos <- "BB"
      df$Op4Pos <- "SB"
    }else if(JamesPos == "SB"){
      df$Op1Pos <- "BTN"
      df$Op2Pos <- "CO"
      df$Op3Pos <- "UTG"
      df$Op4Pos <- "BB"
    }else{
      df$Op1Pos <- "SB"
      df$Op2Pos <- "BTN"
      df$Op3Pos <- "CO"
      df$Op4Pos <- "UTG"
    }
  }else if(numPlayers == 4){
    if(JamesPos == "CO"){
      df$Op1Pos <- "BB"
      df$Op2Pos <- "SB"
      df$Op3Pos <- "BTN"
    }else if(JamesPos == "BTN"){
      df$Op1Pos <- "CO"
      df$Op2Pos <- "BB"
      df$Op3Pos <- "SB"
    }else if(JamesPos == "SB"){
      df$Op1Pos <- "BTN"
      df$Op2Pos <- "CO"
      df$Op3Pos <- "BB"
    }else{
      df$Op1Pos <- "SB"
      df$Op2Pos <- "BTN"
      df$Op3Pos <- "CO"
    }
  }else if(numPlayers == 3){
    if(JamesPos == "BTN"){
      df$Op1Pos <- "BB"
      df$Op2Pos <- "SB"
    }else if(JamesPos == "SB"){
      df$Op1Pos <- "BB"
      df$Op2Pos <- "BTN"
    }else{
      df$Op1Pos <- "BTN"
      df$Op2Pos <- "SB"
    }
  }else {
    if(JamesPos == "SB"){
      df$Op1Pos <- "BB"
    }else{
      df$Op1Pos <- "SB"
    }
  }
  
  return(df)
}

preProcess1 <- function(hand, breakPoints) {
  JamesPos <- strsplit(hand[grep("Hero", hand, fixed = T)[1]], " ")[[1]][2]
  JamesPos <- substring(JamesPos, 2)
  JamesPos <- strtrim(JamesPos, nchar(JamesPos) - 2)
  
  JamesStack <- as.numeric(strsplit(hand[grep("Hero", hand, fixed = T)[1]], " ")[[1]][3])
  
  bbAmount <- unique(na.omit(as.numeric(unlist(strsplit(unlist(hand[1]), "[^0-9]+")))))[1]
  
  intro <- hand[grep("VPIP:", hand, fixed = T)]
  
  nums <- unique(na.omit(as.numeric(unlist(strsplit(unlist(hand[1]), "[^0-9]+")))))
  
  numPlayers <- nums[length(nums)]

  BBline <- grep("BB:", intro)
  SBline <- grep("SB:", intro)
  BTNline <- grep("BTN:", intro)
  COline <- grep("CO:", intro)
  MPline <- grep("MP:", intro)
  UTGline <- grep("UTG:", intro)
  
  BBInfo <- NULL
  SBInfo <- NULL
  BTNInfo <- NULL
  COInfo <- NULL
  MPInfo <- NULL
  UTGInfo <- NULL
  
  if(length(BBline) > 0){
    BBInfo <- processPlayer1(intro[BBline])
  }
  
  if(length(SBline) > 0){
    SBInfo <- processPlayer1(intro[SBline])
  }
  
  if(length(BTNline) > 0){
    BTNInfo <- processPlayer1(intro[BTNline])
  }
  
  if(length(COline) > 0){
    COInfo <- processPlayer1(intro[COline])
  }
  
  if(length(MPline) > 0){
    MPInfo <- processPlayer1(intro[MPline])
  }
  
  if(length(UTGline) > 0){
    UTGInfo <- processPlayer1(intro[UTGline])
  }

  return(list(numPlayers = numPlayers, JamesPos = JamesPos, JamesStack = JamesStack,
              BBInfo = BBInfo, SBInfo = SBInfo, BTNInfo = BTNInfo, COInfo = COInfo,
              MPInfo = MPInfo, UTGInfo = UTGInfo, bbAmount = bbAmount))
}

processPlayer1 <- function(line) {
  
  playerInfo <- list(chipCounts = numeric(0), VPIP = numeric(0), PFR = numeric(0),
                     ThreeBet = numeric(0), hands = integer(0))
  
  line <- strsplit(line, " ")
  
  playerInfo$chipCounts <- as.numeric(line[[1]][2])
  playerInfo$VPIP <- as.numeric(strtrim(line[[1]][5], nchar(line[[1]][5])-1))
  playerInfo$PFR <- as.numeric(strtrim(line[[1]][7], nchar(line[[1]][7])-1))
  playerInfo$ThreeBet <- as.numeric(strtrim(line[[1]][10], nchar(line[[1]][10])-1))
  playerInfo$hands <- as.numeric(strtrim(line[[1]][12], nchar(line[[1]][12])-1))
  
  return(playerInfo)
} 

################################################################################
# Long hand functions
################################################################################


updatePot <- function(df){

  for(i in 2:nrow(df)){
    totStack1 <- sum(df[i-1,c(6,21,31,41,51,61)], na.rm = T)
    totStack2 <- sum(df[i,c(6,21,31,41,51,61)], na.rm = T)
    
    df[i,11] <- round(as.numeric(df[i-1,11]) + (totStack1 - totStack2),6)
  }
  
  return(df)
}


hasCards <- function(df, playerInfo, preflopFinal = NA, flopFinal = NA, turnFinal = NA){
  
  # start by making it all true then go through and update 
  for(i in 1:5){
    if(i < nrow(playerInfo)){
      df[,i*10 + 16] <- T
    } else {
      df[,i*10 + 16] <- F
    }
  }
  
  
  
  if(!is.na(preflopFinal) && length(which(df[,10] == "flop")) > 0){
    if("folds" %in% preflopFinal[,2]){
      index1 <- which(preflopFinal[,2] == "folds")
      for(i in index1){
        # had to add tail funciton because it was picking up the big blind player index
        index2 <- tail(which(df[1,] == preflopFinal[i,1]), n=1)
        flopIndex <- which(df[,10] == "flop")[1]
        df[flopIndex, index2 + 8] <- F
      }
    }
  }
  
  
  if(!is.na(flopFinal) && length(which(df[,10] == "turn")) > 0){
    if("folds" %in% flopFinal[,2]){
      index1 <- which(flopFinal[,2] == "folds")
      for(i in index1){
        index2 <- tail(which(df[1,] == flopFinal[i,1]), n=1)
        turnIndex <- which(df[,10] == "turn")[1]
        df[turnIndex, index2 + 8] <- F
      }
    }
  }
  
  if(!is.na(turnFinal) && length(which(df[,10] == "river")) > 0){
    if("folds" %in% turnFinal[,2]){
      index1 <- which(turnFinal[,2] == "folds")
      for(i in index1){
        index2 <- tail(which(df[1,] == turnFinal[2,1]), n=1)
        riverIndex <- which(df[,10] == "river")[1]
        df[riverIndex, index2 + 8] <- F
      }
    }
  }
  
  
  for(i in 2:(nrow(df) - 1)){
    for(j in 1:5){
      
      if(!is.na(df[i, 10*j + 9])){
        if(df[i, 10*j + 16] && df[i, 10*j + 9] == "folds"){
          df[i + 1, 10*j + 16] <- F
        } else if (!df[i, 10*j + 16]){
          df[i + 1, 10*j + 16] <- F
        }
      } else if (!df[i, 10*j + 16]){
        df[i + 1, 10*j + 16] <- F
      }
      
    }
  }
  
  return(df)
}

updateStacks <- function(df, playerInfo, preflopFinal = NA, flopFinal = NA,
                         turnFinal = NA, allinPre, allinFlop, allinTurn){
  
  actStackPairs <- matrix(c(8, 6,
                            19, 21,
                            29, 31,
                            39, 41,
                            49, 51,
                            59, 61),nrow = 6, byrow = T)
  
  for(i in 2:nrow(df)){
    for(j in 1:6){
      if(!is.na(df[i-1, actStackPairs[j,1]])){
        if(df[i-1, actStackPairs[j,1]] == "calls" || df[i-1, actStackPairs[j,1]] == "bets" ||  df[i-1, actStackPairs[j,1]] == "raises" ){
          df[i, actStackPairs[j,2]] <- df[i - 1, actStackPairs[j,2]] - df[i - 1, actStackPairs[j,1] + 1]
        } else {
          df[i, actStackPairs[j,2]] <- df[i - 1, actStackPairs[j,2]]
        }
      } else {
        df[i, actStackPairs[j,2]] <- df[i - 1, actStackPairs[j,2]]
      }
    }
  }    
  
  
  breaks <- c(which(df[,10] == "flop")[1], which(df[,10] == "turn")[1], which(df[,10] == "river")[1])
  
  if(!is.na(preflopFinal) && !is.na(sum(breaks))){
    for(i in 1:nrow(preflopFinal)){
      if(preflopFinal[i,2] == "calls" && !allinPre){
        df[breaks[1], which(df[1,] == preflopFinal[i,1]) + 3] <- df[breaks[1] - 1, which(df[1,] == preflopFinal[i,1]) + 3] - as.numeric(preflopFinal[i,3])
      }
    }
  }
  
  if(!is.na(flopFinal)){
    for(i in 1:nrow(flopFinal)){
      if(flopFinal[i,2] == "calls" && !allinFlop){
        df[breaks[1], which(df[1,] == flopFinal[i,1]) + 3] <- df[breaks[1] - 1, which(df[1,] == flopFinal[i,1]) + 3] - as.numeric(flopFinal[i,3])
      }
    }
  }
  
  if(!is.na(turnFinal)){
    for(i in 1:nrow(turnFinal)){
      if(turnFinal[i,2] == "calls" && !allinTurn){
        df[breaks[1], which(df[1,] == turnFinal[i,1]) + 3] <- df[breaks[1] - 1, which(df[1,] == turnFinal[i,1]) + 3] - as.numeric(turnFinal[i,3])
      }
    }
  }
  
  return(df)
}




cleanLong <- function(handLong){
  if(length(grep("has timed out", handLong, fixed = T)) > 0){
    handLong <- handLong[-grep("has timed out", handLong, fixed = T)]
  }
  if(length(grep("leaves the table", handLong, fixed = T)) > 0){
    handLong <- handLong[-grep("leaves the table", handLong, fixed = T)]
  }
  if(length(grep("joins the table", handLong, fixed = T)) > 0){
    handLong <- handLong[-grep("joins the table", handLong, fixed = T)]
  }
  if(length(grep(" said, ", handLong, fixed = T)) > 0){
    handLong <- handLong[-grep(" said, ", handLong, fixed = T)]
  }
  if(length(grep("Uncalled bet ", handLong, fixed = T)) > 0){
    handLong <- handLong[-grep("Uncalled bet ", handLong, fixed = T)]
  }
  if(length(grep(" collected", handLong, fixed = T)) > 0){
    handLong <- handLong[-grep(" collected", handLong, fixed = T)]
  }
  if(length(grep(" is disconnected", handLong, fixed = T)) > 0){
    handLong <- handLong[-grep(" is disconnected", handLong, fixed = T)]
  }
  if(length(grep(" is sitting out", handLong, fixed = T)) > 0){
    handLong <- handLong[-grep(" is sitting out", handLong, fixed = T)]
  }
  if(length(grep("was removed from the table", handLong, fixed = T)) > 0){
    handLong <- handLong[-grep("was removed from the table", handLong, fixed = T)]
  }
  if(length(grep("is connected", handLong, fixed = T)) > 0){
    handLong <- handLong[-grep("is connected", handLong, fixed = T)]
  }
  
  return(handLong)
}

getBreakpoints <- function(hand) {
  breakPoints <- grep("***", hand, fixed = T)
  return(breakPoints)
}

preProcess2 <- function(hand, breakPoints) {
  intro <- hand[1:(breakPoints[1] - 1)]
  handNum <- as.numeric(substring(strsplit(hand[1], " ")[[1]][3], 2, last = 13))
  seats <- grep("Seat [[:digit:]]", intro)
  playerInfo <- processPlayer2(intro, seats)
  smallBlindInd <- grep("small blind", intro, fixed = T)[1]
  if(!is.na(smallBlindInd)) {
    smallBlind <- strsplit(intro[smallBlindInd], ":")[[1]][1]
  }
  bigBlindInd <- grep("big blind", intro, fixed = T)[1]
  bigBlind <- strsplit(intro[bigBlindInd], ":")
  bigBlindPlayer <- bigBlind[[1]][1]
  bigBlindAmt <- as.numeric(gsub("[^0-9]+", "", bigBlind[[1]][2]))
  pot <- 1.5 * bigBlindAmt
  for (i in 1:nrow(playerInfo)) {
    if (playerInfo[i, 1] == smallBlind) {
      playerInfo[i, 2] <- playerInfo[i, 2] - (bigBlindAmt / 2)
    }
    if (playerInfo[i, 1] == bigBlindPlayer) {
      playerInfo[i, 2] <- playerInfo[i, 2] - bigBlindAmt
    }
  }
  return(list(handNum = handNum, pot = pot, playerInfo = playerInfo, bigBlindPlayer = bigBlindPlayer, bigBlindAmt = bigBlindAmt))
}

processPlayer2 <- function(intro, seats) {
  seatsText <- intro[seats]
  jamesPos <- grep(paste(screenName), seatsText, fixed = T)
  playerInfo <- data.frame(names = character(0), chipCounts = integer(0), stringsAsFactors = F)
  if(jamesPos > 1){
    seatsText <- c(seatsText[jamesPos:length(seatsText)], seatsText[1:(jamesPos - 1)])
  }
  split <- strsplit(seatsText, " \\(")
  for (i in 1:length(seatsText)) {
    playerInfo[i, 1] <- substr(split[[i]][1], 9, nchar(split[[i]][1]))
    playerInfo[i, 2] <- as.numeric(gsub("[^0-9]+", "", split[[i]][2]))
  }
  return(playerInfo)
}

riverFun <- function(hand, df, breakPoints, allin = F) {
  adjAmt <- nrow(df)
  card <- hand[(breakPoints[4])]
  card <- substr(card, 30, 31)
  
  river <- hand[(breakPoints[4] + 1):(breakPoints[5] - 1)]
  actionPoints <- c(0, grep(paste0(screenName, ":"), river))
  numActions <- length(actionPoints) - 1
  for (j in 1:numActions) {
    triplets <- data.frame(name = character(0), action = character(0), amt = numeric(0),
                           allin = logical(0), stringsAsFactors = F)
    for (i in (actionPoints[j] + 1):actionPoints[j + 1]) {
      triplet <- processLine(river[i], bbsize = df$BBSize)
      triplets[i, 1] <- triplet$name
      triplets[i, 2] <- triplet$action
      triplets[i, 3] <- triplet$amt
      triplets[i, 4] <- triplet$allin
    }
    # Update data frame
    df[j + adjAmt, c(7:10, 16)] <- c(triplets[nrow(triplets), 1:3], "river", card[[1]][1])
    if(triplets[nrow(triplets), 4]){
      allin <- T
    }
    df[j + adjAmt, 12:15] <- df[j + adjAmt -1, 12:15]
    if (is.na(triplets[1, 1])) {
      triplets <- triplets[2:nrow(triplets),]
    }
    triplets <- triplets[rowSums(is.na(triplets)) < 2, ]
    if(nrow(triplets) != 1){
      for (k in 1:(nrow(triplets) - 1)) {
        index <- which(df[1,] == triplets[k,1]) 
        df[j + adjAmt, c(index:(index+2))] <- triplets[k, 1:3]
      }
    }
  }
  if (df[nrow(df), 8] != "folds") {
    if (actionPoints[length(actionPoints)] != length(river)) {
      finalTriplets <- data.frame(name = character(0), action = character(0), amt = numeric(0),
                                  allin = logical(0), stringsAsFactors = F)
      for (i in (actionPoints[length(actionPoints)] + 1):length(river)) {
        triplet <- processLine(river[i], bbsize = df$BBSize)
        finalTriplets[i - actionPoints[length(actionPoints)], 1] <- triplet$name
        finalTriplets[i - actionPoints[length(actionPoints)], 2] <- triplet$action
        finalTriplets[i - actionPoints[length(actionPoints)], 3] <- triplet$amt
        finalTriplets[i - actionPoints[length(actionPoints)], 4] <- triplet$allin
      }
    } else {
      finalTriplets = NA
    }
  } else {
    finalTriplets = NA
  }
  return(list(df = df, finalTriplets = finalTriplets, turnCard = card, allin = allin))
}

turnFun <- function(hand, df, breakPoints, allin = F) {
  adjAmt <- nrow(df)
  card <- hand[(breakPoints[3])]
  card <- substr(card, 26, 27)
  
  turn <- hand[(breakPoints[3] + 1):(breakPoints[4] - 1)]
  actionPoints <- c(0, grep(paste0(screenName, ":"), turn))
  numActions <- length(actionPoints) - 1
  for (j in 1:numActions) {
    triplets <- data.frame(name = character(0), action = character(0), amt = numeric(0),
                           allin = logical(0), stringsAsFactors = F)
    for (i in (actionPoints[j] + 1):actionPoints[j + 1]) {
      triplet <- processLine(turn[i], bbsize = df$BBSize)
      triplets[i, 1] <- triplet$name
      triplets[i, 2] <- triplet$action
      triplets[i, 3] <- triplet$amt
      triplets[i, 4] <- triplet$allin
    }
    
    # Update data frame
    df[j + adjAmt, c(7:10, 15)] <- c(triplets[nrow(triplets), 1:3], "turn", card[[1]][1])
    if(triplets[nrow(triplets), 4]){
      allin <- T
    }
    df[j + adjAmt, 12:14] <- df[j + adjAmt -1, 12:14]
    if (is.na(triplets[1, 1])) {
      triplets <- triplets[2:nrow(triplets),]
    }
    triplets <- triplets[rowSums(is.na(triplets)) < 2, ]
    if(nrow(triplets) != 1){
      for (k in 1:(nrow(triplets) - 1)) {
        
        index <- which(df[1,] == triplets[k,1]) 
        df[j + adjAmt, c(index:(index+2))] <- triplets[k, 1:3]
      }
    }
  }
  
  if (df[nrow(df), 8] != "folds") {
    if (actionPoints[length(actionPoints)] != length(turn)) {
      finalTriplets <- data.frame(name = character(0), action = character(0), amt = numeric(0),
                                  allin = logical(0), stringsAsFactors = F)
      for (i in (actionPoints[length(actionPoints)] + 1):length(turn)) {
        triplet <- processLine(turn[i], bbsize = df$BBSize)
        finalTriplets[i - actionPoints[length(actionPoints)], 1] <- triplet$name
        finalTriplets[i - actionPoints[length(actionPoints)], 2] <- triplet$action
        finalTriplets[i - actionPoints[length(actionPoints)], 3] <- triplet$amt
        finalTriplets[i - actionPoints[length(actionPoints)], 4] <- triplet$allin
      }
    } else {
      finalTriplets = NA
    }
  } else {
    finalTriplets = NA
  }
  return(list(df = df, finalTriplets = finalTriplets, turnCard = card, allin = allin))
}

flopFun <- function(hand, df, breakPoints, allin = F) {
  adjAmt <- nrow(df)
  cards <- hand[(breakPoints[2])]
  cards <- substr(cards, 15, 22)
  cards <- strsplit(cards, " ")
  
  flop <- hand[(breakPoints[2] + 1):(breakPoints[3] - 1)]
  actionPoints <- c(0, grep(paste0(screenName, ":"), flop))
  numActions <- length(actionPoints) - 1
  for (j in 1:numActions) {
    triplets <- data.frame(name = character(0), action = character(0), amt = numeric(0),
                           allin = logical(0), stringsAsFactors = F)
    for (i in (actionPoints[j] + 1):actionPoints[j + 1]) {
      triplet <- processLine(flop[i], bbsize = df$BBSize)
      triplets[i, 1] <- triplet$name
      triplets[i, 2] <- triplet$action
      triplets[i, 3] <- triplet$amt
      triplets[i, 4] <- triplet$allin
    }
    # Update data frame
    df[j + adjAmt, c(7:10, 12:14)] <- c(triplets[nrow(triplets), 1:3], "flop", cards[[1]][1], cards[[1]][2], cards[[1]][3])
    if(triplets$allin[nrow(triplets)]){
      allin <- T
    }
    if (is.na(triplets[1, 1])) {
      triplets <- triplets[2:nrow(triplets),]
    }
    
    triplets <- triplets[rowSums(is.na(triplets)) < 2, ]
    if(nrow(triplets) != 1){
      for (k in 1:(nrow(triplets) - 1)) {
        
        index <- which(df[1,] == triplets[k,1]) 
        df[j + adjAmt, c(index:(index+2))] <- triplets[k, 1:3]
      }
    }
  }
  
  if (df[nrow(df), 8] != "folds") {
    if (actionPoints[length(actionPoints)] != length(flop)) {
      finalTriplets <- data.frame(name = character(0), action = character(0), amt = numeric(0),
                                  allin = logical(0), stringsAsFactors = F)
      for (i in (actionPoints[length(actionPoints)] + 1):length(flop)) {
        triplet <- processLine(flop[i], bbsize = df$BBSize)
        finalTriplets[i - actionPoints[length(actionPoints)], 1] <- triplet$name
        finalTriplets[i - actionPoints[length(actionPoints)], 2] <- triplet$action
        finalTriplets[i - actionPoints[length(actionPoints)], 3] <- triplet$amt
        finalTriplets[i - actionPoints[length(actionPoints)], 4] <- triplet$allin
      }
    } else {
      finalTriplets = NA
    }
  } else {
    finalTriplets = NA
  }
  return(list(df = df, finalTriplets = finalTriplets, flopCards = cards, allin = allin))
}

preFlopFun <- function(hand, df, breakPoints, allin = F) {
  preFlop <- hand[(breakPoints[1] + 1):(breakPoints[2] - 1)]
  actionPoints <- c(1, grep(paste0(screenName, ":"), preFlop))
  numActions <- length(actionPoints) - 1
  for (j in 1:numActions) {
    triplets <- data.frame(name = character(0), action = character(0), amt = numeric(0),
                           allin = logical(0), stringsAsFactors = F)
    for (i in (actionPoints[j] + 1):actionPoints[j + 1]) {
      triplet <- processLine(preFlop[i], bbsize = df$BBSize)
      triplets[i - 1, 1] <- triplet$name
      triplets[i - 1, 2] <- triplet$action
      triplets[i - 1, 3] <- triplet$amt
      triplets[i - 1, 4] <- triplet$allin
    }
    # Update data frame
    df[j, 7:10] <- c(triplets[nrow(triplets), 1:3], "preflop")
    if(triplets[nrow(triplets), 4]){
      allin <- T
    }
    if (nrow(triplets) != 1) {
      for (k in 1:(nrow(triplets) - 1)) {
        index <- which(df[1,] == triplets[k,1]) 
        df[j, c(index + 1, index + 2)] <- triplets[k,2:3]
      }
    }
  }
  # Check if last action point was a fold.  If that's false, check if last action point is
  # length(preflop). Otherwise, we need to send back triplets.
  if (df[nrow(df), 7] != "folds") {
    if (actionPoints[length(actionPoints)] != length(preFlop)) {
      finalTriplets <- data.frame(name = character(0), action = character(0), amt = numeric(0),
                                  allin = logical(0), stringsAsFactors = F)
      for (i in (actionPoints[length(actionPoints)] + 1):length(preFlop)) {
        triplet <- processLine(preFlop[i], bbsize = df$BBSize)
        finalTriplets[i - actionPoints[length(actionPoints)], 1] <- triplet$name
        finalTriplets[i - actionPoints[length(actionPoints)], 2] <- triplet$action[1]
        finalTriplets[i - actionPoints[length(actionPoints)], 3] <- triplet$amt
        finalTriplets[i - actionPoints[length(actionPoints)], 4] <- triplet$allin
      }
    } else {
      finalTriplets = NA
    }
  } else {
    finalTriplets = NA
  }
  return(list(df = df, finalTriplets = finalTriplets, allin = allin))
}

### processLine will return name, action, amt triplet. For loop will combine these into a frame.
### processLine should be general for any time a line can be processed.
processLine <- function(line, bbsize) {
  allin <- F
  line <- strsplit(line, ":")
  name <- line[[1]][1]
  actionAmt <- line[[1]][2]
  actionAmt <- trimws(actionAmt)
  actionAmt <- strsplit(actionAmt, " ")
  action <- actionAmt[[1]][1]
  if (length(actionAmt[[1]]) == 1) {
    amt <- NA
    allin <- F
  } else if(actionAmt[[1]][length(actionAmt[[1]])] == "all-in"){
    if(length(actionAmt[[1]] == 5)){
      amt <- as.numeric(actionAmt[[1]][2])
    }else{
      amt <- as.numeric(actionAmt[[1]][4])
    }
    allin <- T
  }  else {
    amt <- as.numeric(tail(actionAmt[[1]][2]), n=1)
    allin <- F
  }
  
  amt <- amt / bbsize[1]
  
  return(list(name = name, action = action, amt = amt, allin = allin))
}

setStartVals <- function(df, playerInfo, numPlayers){
  # start has cards to false for last 4 players since there must always be 2
  df[1,c(26)] <- T
  df[1,c(46,56,66)] <- F
  
  posPlaces <- c(21)
  
  if (numPlayers > 2){
    df[1,36] <- T
    posPlaces <- c(posPlaces, 31)
  }
  if (numPlayers > 3){
    df[1,46] <- T
    posPlaces <- c(posPlaces, 41)
  }
  if (numPlayers > 4){
    df[1,56] <- T
    posPlaces <- c(posPlaces, 51)
  }
  if (numPlayers > 5){
    df[1,66] <- T
    posPlaces <- c(posPlaces, 61)
  }
  
  # setting my stack size to start the hand
  df[1,6] <- playerInfo$JamesStack
  # setting each players stack size and that they have cards to start
  for(i in posPlaces){
    if(df[1,i - 4] == "BB" & !is.null(playerInfo$BBInfo)){
      df[1,i] <- playerInfo$BBInfo$chipCounts
      df[1,i + 1] <- playerInfo$BBInfo$VPIP
      df[1,i + 2] <- playerInfo$BBInfo$PFR
      df[1,i + 3] <- playerInfo$BBInfo$ThreeBet
      df[1,i + 4] <- playerInfo$BBInfo$hands
    } else if(df[1,i - 4] == "SB" & !is.null(playerInfo$SBInfo)){
      df[1,i] <- playerInfo$SBInfo$chipCounts
      df[1,i + 1] <- playerInfo$SBInfo$VPIP
      df[1,i + 2] <- playerInfo$SBInfo$PFR
      df[1,i + 3] <- playerInfo$SBInfo$ThreeBet
      df[1,i + 4] <- playerInfo$SBInfo$hands
    } else if(df[1,i - 4] == "BTN" & !is.null(playerInfo$BTNInfo)){
      df[1,i] <- playerInfo$BTNInfo$chipCounts
      df[1,i + 1] <- playerInfo$BTNInfo$VPIP
      df[1,i + 2] <- playerInfo$BTNInfo$PFR
      df[1,i + 3] <- playerInfo$BTNInfo$ThreeBet
      df[1,i + 4] <- playerInfo$BTNInfo$hands
    } else if(df[1,i - 4] == "CO" & !is.null(playerInfo$COInfo)){
      df[1,i] <- playerInfo$COInfo$chipCounts
      df[1,i + 1] <- playerInfo$COInfo$VPIP
      df[1,i + 2] <- playerInfo$COInfo$PFR
      df[1,i + 3] <- playerInfo$COInfo$ThreeBet
      df[1,i + 4] <- playerInfo$COInfo$hands
    } else if(df[1,i - 4] == "MP" & !is.null(playerInfo$MPInfo)){
      df[1,i] <- playerInfo$MPInfo$chipCounts
      df[1,i + 1] <- playerInfo$MPInfo$VPIP
      df[1,i + 2] <- playerInfo$MPInfo$PFR
      df[1,i + 3] <- playerInfo$MPInfo$ThreeBet
      df[1,i + 4] <- playerInfo$MPInfo$hands
    } else if(df[1,i - 4] == "UTG" & !is.null(playerInfo$UTGInfo)){
      df[1,i] <- playerInfo$UTGInfo$chipCounts
      df[1,i + 1] <- playerInfo$UTGInfo$VPIP
      df[1,i + 2] <- playerInfo$UTGInfo$PFR
      df[1,i + 3] <- playerInfo$UTGInfo$ThreeBet
      df[1,i + 4] <- playerInfo$UTGInfo$hands
    }
  }
  return(df)
}

setNames <- function(df, names){
  for(i in 1:(length(names)-1)){
    df[1,8 + i*10] <- names[i+1]
  }
  return(df)
}

