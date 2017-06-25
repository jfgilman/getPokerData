rm(list = ls())

setwd("~/Desktop/PokerStars")

processFile = function(filepath) {
  con = file(filepath, "r")
  hands <- list()
  data <- c()
  i <- 1
  j <- 1
  
  while ( TRUE ) {
    line = readLines(con, n = 1)
    if ( length(line) == 0 ) {
      break
    }
    data[i] <- line
    
    # print(line)
    
    if(data[i] == ""){
      if(data[i-1] == ""){
        hands[[j]] <- data
        j <- j + 1
        i <- 1
        data <- c()
      } else {
        i <- i + 1 
      }
    } else {
      i <- i + 1 
    }
  }
  
  close(con)
  
  return(hands)
}

# hands <- processFile("Landi - P100 NL (6 max) - 201702212117.txt")



filelist = list.files(pattern = ".*.txt")

screenName <- "LL-Poker1"

dfAll <- data.frame(hand = integer(0), BBPlayer = character(0), JamesCard1 = character(0),
                 JamesCard2 = character(0), JamesStack = numeric(0), JamesName = character(0),
                 JamesAction = character(0), JamesAmount = integer(0),
                 street = character(0), pot = numeric(0), comCard1 = character(0),
                 comCard2 = character(0), comCard3 = character(0),
                 comCard4 = character(0), comCard5 = character(0), Op1Name = character(0), 
                 Op1Action = character(0), OP1Amount = integer(0), Op1Stack = numeric(0), 
                 Op1HasCards = logical(0), Op2Name = character(0), Op2Action = character(0),
                 OP2Amount = integer(0), Op2Stack = numeric(0),
                 Op2HasCards = logical(0), Op3Name = character(0), Op3Action = character(0),
                 OP3Amount = integer(0), Op3Stack = numeric(0),
                 Op3HasCards = logical(0),  Op4Name = character(0), Op4Action = character(0),
                 OP4Amount = integer(0), Op4Stack = numeric(0),
                 Op4HasCards = logical(0),  Op5Name = character(0), Op5Action = character(0),
                 OP5Amount = integer(0), Op5Stack = numeric(0),
                 Op5HasCards = logical(0),  bigBlindAmt = integer(0),
                 stringsAsFactors=FALSE)


for(f in 1:length(filelist)){
  
  hands <- processFile(filelist[f])
  
  dfBig <- data.frame(hand = integer(0), BBPlayer = character(0), JamesCard1 = character(0),
                      JamesCard2 = character(0), JamesStack = numeric(0), JamesName = character(0),
                      JamesAction = character(0), JamesAmount = integer(0),
                      street = character(0), pot = numeric(0), comCard1 = character(0),
                      comCard2 = character(0), comCard3 = character(0),
                      comCard4 = character(0), comCard5 = character(0), Op1Name = character(0), 
                      Op1Action = character(0), OP1Amount = integer(0), Op1Stack = numeric(0), 
                      Op1HasCards = logical(0), Op2Name = character(0), Op2Action = character(0),
                      OP2Amount = integer(0), Op2Stack = numeric(0),
                      Op2HasCards = logical(0), Op3Name = character(0), Op3Action = character(0),
                      OP3Amount = integer(0), Op3Stack = numeric(0),
                      Op3HasCards = logical(0),  Op4Name = character(0), Op4Action = character(0),
                      OP4Amount = integer(0), Op4Stack = numeric(0),
                      Op4HasCards = logical(0),  Op5Name = character(0), Op5Action = character(0),
                      OP5Amount = integer(0), Op5Stack = numeric(0),
                      Op5HasCards = logical(0),  bigBlindAmt = integer(0),
                      stringsAsFactors=FALSE)
  
  for(p in 1:length(hands)){
    
    if(length(grep(paste(screenName), hands[[p]], fixed = T)) > 0){
      
      hand <- hands[[p]]
      if(length(grep("has timed out", hand, fixed = T)) > 0){
        hand <- hand[-grep("has timed out", hand, fixed = T)]
      }
      if(length(grep("leaves the table", hand, fixed = T)) > 0){
        hand <- hand[-grep("leaves the table", hand, fixed = T)]
      }
      if(length(grep("joins the table", hand, fixed = T)) > 0){
        hand <- hand[-grep("joins the table", hand, fixed = T)]
      }
      if(length(grep(" said, ", hand, fixed = T)) > 0){
        hand <- hand[-grep(" said, ", hand, fixed = T)]
      }
      if(length(grep("Uncalled bet ", hand, fixed = T)) > 0){
        hand <- hand[-grep("Uncalled bet ", hand, fixed = T)]
      }
      if(length(grep(" collected", hand, fixed = T)) > 0){
        hand <- hand[-grep(" collected", hand, fixed = T)]
      }
      if(length(grep(" is disconnected", hand, fixed = T)) > 0){
        hand <- hand[-grep(" is disconnected", hand, fixed = T)]
      }
      if(length(grep(" is sitting out", hand, fixed = T)) > 0){
        hand <- hand[-grep(" is sitting out", hand, fixed = T)]
      }
      if(length(grep("was removed from the table", hand, fixed = T)) > 0){
        hand <- hand[-grep("was removed from the table", hand, fixed = T)]
      }
      if(length(grep("is connected", hand, fixed = T)) > 0){
        hand <- hand[-grep("is connected", hand, fixed = T)]
      }
      
      df <- data.frame(hand = integer(0), BBPlayer = character(0), JamesCard1 = character(0),
                       JamesCard2 = character(0), JamesStack = numeric(0), JamesName = character(0),
                       JamesAction = character(0), JamesAmount = integer(0),
                       street = character(0), pot = numeric(0), comCard1 = character(0),
                       comCard2 = character(0), comCard3 = character(0),
                       comCard4 = character(0), comCard5 = character(0), Op1Name = character(0), 
                       Op1Action = character(0), OP1Amount = integer(0), Op1Stack = numeric(0), 
                       Op1HasCards = logical(0), Op2Name = character(0), Op2Action = character(0),
                       OP2Amount = integer(0), Op2Stack = numeric(0),
                       Op2HasCards = logical(0), Op3Name = character(0), Op3Action = character(0),
                       OP3Amount = integer(0), Op3Stack = numeric(0),
                       Op3HasCards = logical(0),  Op4Name = character(0), Op4Action = character(0),
                       OP4Amount = integer(0), Op4Stack = numeric(0),
                       Op4HasCards = logical(0),  Op5Name = character(0), Op5Action = character(0),
                       OP5Amount = integer(0), Op5Stack = numeric(0),
                       Op5HasCards = logical(0),  bigBlindAmt = integer(0),
                       stringsAsFactors=FALSE)
      
      
      breakPoints <- getBreakpoints(hand)
      preProcessData <- preProcess(hand, breakPoints)
      df <- setStartVals(df, preProcessData$playerInfo)
      holdCards <- getHoldCards(hand)
      preFlop <- preFlopFun(hand, df, breakPoints)
      df <- preFlop[[1]]
      df[1,41] <- preProcessData$bigBlindAmt
      df[1,10] <- preProcessData$bigBlindAmt*1.5
      
      if(length(breakPoints) > 2){
        if(df$JamesAction[nrow(df)] != "folds" && df$JamesAction[nrow(df)] != "doesn't"
           && !preFlop$allin && (breakPoints[3] - breakPoints[2]) > 1){
          flop <- flopFun(hand, df, breakPoints)
          df <- flop[[1]]
        } else {
          flop <- list(df = df, finalTriplets = NA, allin = F)
        }
      } else {
        flop <- list(df = df, finalTriplets = NA, allin = F)
      }
      
      if(length(breakPoints) > 3){
        if(df$JamesAction[nrow(df)] != "folds" && df$JamesAction[nrow(df)] != "doesn't"
           && !preFlop$allin && !flop$allin && (breakPoints[4] - breakPoints[3]) > 1){
          turn <- turnFun(hand, df, breakPoints)
          df <- turn[[1]]
        } else {
          turn <- list(df = df, finalTriplets = NA, allin = F)
        }
      } else {
        turn <- list(df = df, finalTriplets = NA, allin = F)
      }
      
      if(length(breakPoints) > 4){
        if(df$JamesAction[nrow(df)] != "folds" && df$JamesAction[nrow(df)] != "doesn't"
           && !preFlop$allin && !flop$allin && !turn$allin && (breakPoints[5] - breakPoints[4]) > 1){
          river <- riverFun(hand, df, breakPoints)
          df <- river[[1]]
        } else {
          river <- list(df = df, finalTriplets = NA, allin = F)
        }
      }else {
        river <- list(df = df, finalTriplets = NA, allin = F)
      }
      
      df[,1] <- preProcessData$handNum
      df[,2] <- preProcessData$bigBlindPlayer
      df[,3] <- holdCards$card1
      df[,4] <- holdCards$card2
      df[,41] <- preProcessData$bigBlindAmt
      
      df[,c(16,21,26,31,36)] <- df[1,c(16,21,26,31,36)]
      
      
      if(nrow(df) > 1){
        
        options(warn=-1)
        
        df <- hasCards(df, preProcessData$playerInfo, preFlop$finalTriplets, flop$finalTriplets,
                       turn$finalTriplets)
        
        df <- updateStacks(df, preProcessData$playerInfo, preFlop$finalTriplets, flop$finalTriplets,
                           turn$finalTriplets, preFlop$allin, flop$allin, turn$allin)
        
        options(warn=0)
        
        df <- updatePot(df)
      }
      
      dfBig <- rbind(dfBig, df) 
    }
  }
  
  dfAll <- rbind(dfAll, dfBig)
}



final <- dfAll[!is.na(dfAll$hand),]

save(final, file = "pokerData.RData")




##################################################################################################

updatePot <- function(df){
  df[1,10] <- as.numeric(df$bigBlindAmt[1]*1.5)
  
  for(i in 2:nrow(df)){
    totStack1 <- sum(df[i-1,c(5,19,24,29,34,39)], na.rm = T)
    totStack2 <- sum(df[i,c(5,19,24,29,34,39)], na.rm = T)
    
    df[i,10] <- df[i-1,10] + (totStack1 - totStack2)
  }
  
  return(df)
}

preflopFinal <- preFlop$finalTriplets
playerInfo <- preProcessData$playerInfo


updateStacks <- function(df, playerInfo, preflopFinal = NA, flopFinal = NA,
                         turnFinal = NA, allinPre, allinFlop, allinTurn){

  actStackPairs <- matrix(c(7, 5,
                            17, 19,
                            22, 24,
                            27, 29,
                            32, 34,
                            37, 39),nrow = 6, byrow = T)
  
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
  

  breaks <- c(which(df[,9] == "flop")[1], which(df[,9] == "turn")[1], which(df[,9] == "river")[1])
  
  if(!is.na(preflopFinal)){
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


hasCards <- function(df, playerInfo, preflopFinal = NA, flopFinal = NA, turnFinal = NA){
  
  # start by making it all true then go through and update 
  for(i in 1:5){
    if(i < nrow(playerInfo)){
      df[,i*5 + 15] <- T
    } else {
      df[,i*5 + 15] <- F
    }
  }
  
  
  
  if(!is.na(preflopFinal) && length(which(df[,9] == "flop")) > 0){
    if("folds" %in% preflopFinal[,2]){
      index1 <- which(preflopFinal[,2] == "folds")
      for(i in index1){
        # had to add tail funciton because it was picking up the big blind player index
        index2 <- tail(which(df[1,] == preflopFinal[i,1]), n=1)
        flopIndex <- which(df[,9] == "flop")[1]
        df[flopIndex, index2 + 4] <- F
      }
    }
  }


  if(!is.na(flopFinal) && length(which(df[,9] == "turn")) > 0){
    if("folds" %in% flopFinal[,2]){
      index1 <- which(flopFinal[,2] == "folds")
      for(i in index1){
        index2 <- tail(which(df[1,] == flopFinal[i,1]), n=1)
        turnIndex <- which(df[,9] == "turn")[1]
        df[turnIndex, index2 + 4] <- F
      }
    }
  }

  if(!is.na(turnFinal) && length(which(df[,9] == "river")) > 0){
    if("folds" %in% turnFinal[,2]){
      index1 <- which(turnFinal[,2] == "folds")
      for(i in index1){
        index2 <- tail(which(df[1,] == turnFinal[2,1]), n=1)
        riverIndex <- which(df[,9] == "river")[1]
        df[riverIndex, index2 + 4] <- F
      }
    }
  }


  for(i in 2:(nrow(df) - 1)){
    for(j in 1:5){
      
      if(!is.na(df[i, 5*j + 12])){
        if(df[i, 5*j + 15] && df[i, 5*j + 12] == "folds"){
          df[i + 1, 5*j + 15] <- F
        } else if (!df[i, 5*j + 15]){
          df[i + 1, 5*j + 15] <- F
        }
      } else if (!df[i, 5*j + 15]){
        df[i + 1, 5*j + 15] <- F
      }

    }
  }
  
  return(df)
}



setStartVals <- function(df, playerInfo){
  # start has cards to false for last 4 players since there must always be 2
  df[1,c(30,35,40)] <- F
  # always setting preflop pot to 1 big and 1 small blind for now
  df[1,10] <- df[1,41]*1.5
  # setting my stack size to start the hand 
  df[1,5] <- playerInfo[1,2]
  # setting each players stack size and that they have cards to start
  for(i in 1:(nrow(playerInfo) - 1)){
    df[1,i*5 + 11] <- playerInfo[i + 1, 1]
    df[1,i*5 + 14] <- playerInfo[i + 1, 2]
    df[1,i*5 + 15] <- T
  }
  return(df)
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
      triplet <- processLine(river[i])
      triplets[i, 1] <- triplet$name
      triplets[i, 2] <- triplet$action
      triplets[i, 3] <- triplet$amt
      triplets[i, 4] <- triplet$allin
    }
    # Update data frame
    df[j + adjAmt, c(6:9, 15)] <- c(triplets[nrow(triplets), 1:3], "river", card[[1]][1])
    if(triplets[nrow(triplets), 4]){
      allin <- T
    }
    df[j + adjAmt, 11:14] <- df[j + adjAmt -1, 11:14]
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
  if (df[nrow(df), 7] != "folds") {
    if (actionPoints[length(actionPoints)] != length(river)) {
      finalTriplets <- data.frame(name = character(0), action = character(0), amt = numeric(0),
                                  allin = logical(0), stringsAsFactors = F)
      for (i in (actionPoints[length(actionPoints)] + 1):length(river)) {
        triplet <- processLine(river[i])
        finalTriplets[i - actionPoints[length(actionPoints)], 1] <- triplet$name
        finalTriplets[i - actionPoints[length(actionPoints)], 2] <- triplet$action
        finalTriplets[i - actionPoints[length(actionPoints)], 3] <- triplet$amt
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
      triplet <- processLine(turn[i])
      triplets[i, 1] <- triplet$name
      triplets[i, 2] <- triplet$action
      triplets[i, 3] <- triplet$amt
      triplets[i, 4] <- triplet$allin
    }
    
    # Update data frame
    df[j + adjAmt, c(6:9, 14)] <- c(triplets[nrow(triplets), 1:3], "turn", card[[1]][1])
    if(triplets[nrow(triplets), 4]){
      allin <- T
    }
    df[j + adjAmt, 11:13] <- df[j + adjAmt -1, 11:13]
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
  
  if (df[nrow(df), 7] != "folds") {
    if (actionPoints[length(actionPoints)] != length(turn)) {
      finalTriplets <- data.frame(name = character(0), action = character(0), amt = numeric(0),
                                  allin = logical(0), stringsAsFactors = F)
      for (i in (actionPoints[length(actionPoints)] + 1):length(turn)) {
        triplet <- processLine(turn[i])
        finalTriplets[i - actionPoints[length(actionPoints)], 1] <- triplet$name
        finalTriplets[i - actionPoints[length(actionPoints)], 2] <- triplet$action
        finalTriplets[i - actionPoints[length(actionPoints)], 3] <- triplet$amt
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
      triplet <- processLine(flop[i])
      triplets[i, 1] <- triplet$name
      triplets[i, 2] <- triplet$action
      triplets[i, 3] <- triplet$amt
      triplets[i, 4] <- triplet$allin
    }
    # Update data frame
    df[j + adjAmt, c(6:9, 11:13)] <- c(triplets[nrow(triplets), 1:3], "flop", cards[[1]][1], cards[[1]][2], cards[[1]][3])
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
  
  if (df[nrow(df), 7] != "folds") {
    if (actionPoints[length(actionPoints)] != length(flop)) {
      finalTriplets <- data.frame(name = character(0), action = character(0), amt = numeric(0),
                                  allin = logical(0), stringsAsFactors = F)
      for (i in (actionPoints[length(actionPoints)] + 1):length(flop)) {
        triplet <- processLine(flop[i])
        finalTriplets[i - actionPoints[length(actionPoints)], 1] <- triplet$name
        finalTriplets[i - actionPoints[length(actionPoints)], 2] <- triplet$action
        finalTriplets[i - actionPoints[length(actionPoints)], 3] <- triplet$amt
      }
    } else {
      finalTriplets = NA
    }
  } else {
    finalTriplets = NA
  }
  return(list(df = df, finalTriplets = finalTriplets, flopCards = cards, allin = allin))
}

getOpponentOrder <- function(df) {
  return(Opponents = df[1, c(16, 21, 26, 31, 36)])
}

preFlopFun <- function(hand, df, breakPoints, allin = F) {
  preFlop <- hand[(breakPoints[1] + 1):(breakPoints[2] - 1)]
  actionPoints <- c(1, grep(paste0(screenName, ":"), preFlop))
  numActions <- length(actionPoints) - 1
  for (j in 1:numActions) {
    triplets <- data.frame(name = character(0), action = character(0), amt = numeric(0),
                           allin = logical(0), stringsAsFactors = F)
    for (i in (actionPoints[j] + 1):actionPoints[j + 1]) {
      triplet <- processLine(preFlop[i])
      triplets[i - 1, 1] <- triplet$name
      triplets[i - 1, 2] <- triplet$action
      triplets[i - 1, 3] <- triplet$amt
      triplets[i - 1, 4] <- triplet$allin
    }
    # Update data frame
    df[j, 6:9] <- c(triplets[nrow(triplets), 1:3], "preflop")
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
        triplet <- processLine(preFlop[i])
        finalTriplets[i - actionPoints[length(actionPoints)], 1] <- triplet$name
        finalTriplets[i - actionPoints[length(actionPoints)], 2] <- triplet$action
        finalTriplets[i - actionPoints[length(actionPoints)], 3] <- triplet$amt
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
processLine <- function(line, allin = F) {
  line <- strsplit(line, ":")
  name <- line[[1]][1]
  actionAmt <- line[[1]][2]
  actionAmt <- trimws(actionAmt)
  actionAmt <- strsplit(actionAmt, " ")
  action <- actionAmt[[1]][1]
  if (length(actionAmt[[1]]) == 1) {
    amt <- NA
  } else if(actionAmt[[1]][length(actionAmt[[1]])] == "all-in"){
    if(length(actionAmt[[1]] == 5)){
      amt <- as.numeric(actionAmt[[1]][2])
    }else{
      amt <- as.numeric(actionAmt[[1]][4])
    }
    allin <- T
  }  else {
    amt <- as.numeric(tail(actionAmt[[1]][2]), n=1)
  }
  
  return(list(name = name, action = action, amt = amt, allin = allin))
}

# found line number with the hold cards 
getHoldCards <- function(hand) {
  lineNum <- grep(paste("Dealt to", screenName), hand)
  card1 <- substring(hand[lineNum], 21, 22)
  card2 <- substring(hand[lineNum], 24, 25)
  return(list(card1 = card1, card2 = card2))
}


getBreakpoints <- function(hand) {
  breakPoints <- grep("***", hand, fixed = T)
  return(breakPoints)
}

preProcess <- function(hand, breakPoints) {
  intro <- hand[1:(breakPoints[1] - 1)]
  handNum <- as.numeric(substring(strsplit(hand[1], " ")[[1]][3], 2, last = 13))
  seats <- grep("Seat [[:digit:]]", intro)
  playerInfo <- processPlayer(intro, seats)
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

processPlayer <- function(intro, seats) {
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




