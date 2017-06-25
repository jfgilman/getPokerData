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

hands <- processFile("Landi - P100 NL (6 max) - 201702212117.txt")


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

screenName <- "LL-Poker1"

breakPoints <- getBreakpoints(hands[[1]])
preProcessData <- preProcess(hands[[1]], breakPoints)
holdCards <- getHoldCards(hands[[1]])
preFlop <- preFlopFun(hands[[1]], df, breakPoints)
df <- preFlop[[1]]
df[1:nrow(df), 9] <- "preFlop"
flop <- flopFun(hands[[1]], df, breakPoints)
df <- flop[[1]]
turn <- turnFun(hands[[1]], df, breakPoints)
df <- turn[[1]]
river <- riverFun(hands[[1]], df, breakPoints)
df <- river[[1]]

df[,1] <- preProcessData$handNum
df[,2] <- preProcessData$bigBlindPlayer
df[,3] <- holdCards$card1
df[,4] <- holdCards$card2
df[,41] <- preProcessData$bigBlindAmt

df <- setStartVals(df, preProcessData$playerInfo)









setStartVals <- function(df, playerInfo){
  # start has cards to false for last 4 players since there must always be 2
  df[1,c(30,35,40)] <- F
  # always setting preflop pot to 1 big and 1 small blind for now
  df[1,10] <- df[1,41]*1.5
  # setting my stack size to start the hand 
  df[1,5] <- playerInfo[1,2]
  # setting each players stack size and that they have cards to start
  for(i in 1:(nrow(playerInfo) - 1)){
    df[1,i*5 + 14] <- playerInfo[i + 1, 2]
    df[1,i*5 + 15] <- T
  }
  return(df)
}

riverFun <- function(hand, df, breakPoints) {
  adjAmt <- nrow(df)
  card <- hand[(breakPoints[4])]
  card <- substr(card, 30, 31)

  river <- hand[(breakPoints[4] + 1):(breakPoints[5] - 1)]
  actionPoints <- c(0, grep(paste0(screenName, ":"), river))
  numActions <- length(actionPoints) - 1
  for (j in 1:numActions) {
    triplets <- data.frame(name = character(0), action = character(0), amt = numeric(0), stringsAsFactors = F)
    for (i in (actionPoints[j] + 1):actionPoints[j + 1]) {
      triplet <- processLine(river[i])
      triplets[i, 1] <- triplet$name
      triplets[i, 2] <- triplet$action
      triplets[i, 3] <- triplet$amt
    }
    # Update data frame
    df[j + adjAmt, 6:8] <- triplets[nrow(triplets), 1:3]
    if (is.na(triplets[1, 1])) {
      triplets <- triplets[2:nrow(triplets),]
    }
    if (nrow(triplets) != 1 && j != 1) {
      triplets <- rbind(triplets[2:(nrow(triplets) - 1),], triplets[1,], triplets[nrow(triplets),])
      for (k in 1:(nrow(triplets) - 1)) {
        k <- k * 5
        df[j + adjAmt, (k+11):(k+13)] <- triplets[(k/5), 1:3]
      }
    } else if (nrow(triplets) != 1) {
      for (k in 1:(nrow(triplets) - 1)) {
        opponent <- triplets[k, 1]
        opponentOrder <- getOpponentOrder(df)
        for (ind in 1:length(opponentOrder)) {
          if (opponent == opponentOrder[ind]) {
            break
          }
        }
        ind <- ind * 5
        if (ind == 5) {
          df[j + adjAmt, 16:18] <- triplets[k, 1:3]
        } else {
          df[j + adjAmt, (ind+11):(ind+13)] <- triplets[k, 1:3]
        }
      }
    }
  }
  
  if (df[nrow(df), 7] != "folds") {
    if (actionPoints[length(actionPoints)] != length(river)) {
      finalTriplets <- data.frame(name = character(0), action = character(0), amt = numeric(0), stringsAsFactors = F)
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
  return(list(df = df, finalTriplets = finalTriplets, turnCard = card))
}


turnFun <- function(hand, df, breakPoints) {
  adjAmt <- nrow(df)
  card <- hand[(breakPoints[3])]
  card <- substr(card, 26, 27)

  turn <- hand[(breakPoints[3] + 1):(breakPoints[4] - 1)]
  actionPoints <- c(0, grep(paste0(screenName, ":"), turn))
  numActions <- length(actionPoints) - 1
  for (j in 1:numActions) {
    triplets <- data.frame(name = character(0), action = character(0), amt = numeric(0), stringsAsFactors = F)
    for (i in (actionPoints[j] + 1):actionPoints[j + 1]) {
      triplet <- processLine(turn[i])
      triplets[i, 1] <- triplet$name
      triplets[i, 2] <- triplet$action
      triplets[i, 3] <- triplet$amt
    }
    # Update data frame
    df[j + adjAmt, 6:8] <- triplets[nrow(triplets), 1:3]
    if (is.na(triplets[1, 1])) {
      triplets <- triplets[2:nrow(triplets),]
    }
    if (nrow(triplets) != 1 && j != 1) {
      triplets <- rbind(triplets[2:(nrow(triplets) - 1),], triplets[1,], triplets[nrow(triplets),])
      for (k in 1:(nrow(triplets) - 1)) {
        k <- k * 5
        df[j + adjAmt, (k+11):(k+13)] <- triplets[(k/5), 1:3]
      }
    } else if (nrow(triplets) != 1) {
      for (k in 1:(nrow(triplets) - 1)) {
        opponent <- triplets[k, 1]
        opponentOrder <- getOpponentOrder(df)
        for (ind in 1:length(opponentOrder)) {
          if (opponent == opponentOrder[ind]) {
            break
          }
        }
        ind <- ind * 5
        if (ind == 5) {
          df[j + adjAmt, 16:18] <- triplets[k, 1:3]
        } else {
          df[j + adjAmt, (ind+11):(ind+13)] <- triplets[k, 1:3]
        }
      }
    }
  }
  
  if (df[nrow(df), 7] != "folds") {
    if (actionPoints[length(actionPoints)] != length(turn)) {
      finalTriplets <- data.frame(name = character(0), action = character(0), amt = numeric(0), stringsAsFactors = F)
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
  return(list(df = df, finalTriplets = finalTriplets, turnCard = card))
}



flopFun <- function(hand, df, breakPoints) {
  adjAmt <- nrow(df)
  cards <- hand[(breakPoints[2])]
  cards <- substr(cards, 15, 22)
  cards <- strsplit(cards, " ")

  flop <- hand[(breakPoints[2] + 1):(breakPoints[3] - 1)]
  actionPoints <- c(0, grep(paste0(screenName, ":"), flop))
  numActions <- length(actionPoints) - 1
  for (j in 1:numActions) {
    triplets <- data.frame(name = character(0), action = character(0), amt = numeric(0), stringsAsFactors = F)
    for (i in (actionPoints[j] + 1):actionPoints[j + 1]) {
      triplet <- processLine(flop[i])
      triplets[i, 1] <- triplet$name
      triplets[i, 2] <- triplet$action
      triplets[i, 3] <- triplet$amt
    }
    # Update data frame
    df[j + adjAmt, 6:8] <- triplets[nrow(triplets), 1:3]
    if (is.na(triplets[1, 1])) {
      triplets <- triplets[2:nrow(triplets),]
    }
    if (nrow(triplets) != 1 && j != 1) {
      triplets <- rbind(triplets[2:(nrow(triplets) - 1),], triplets[1,], triplets[nrow(triplets),])
      for (k in 1:(nrow(triplets) - 1)) {
        k <- k * 5
        df[j + adjAmt, (k+11):(k+13)] <- triplets[(k/5), 1:3]
      }
    } else if (nrow(triplets) != 1) {
      for (k in 1:(nrow(triplets) - 1)) {
        opponent <- triplets[k, 1]
        opponentOrder <- getOpponentOrder(df)
        for (ind in 1:length(opponentOrder)) {
          if (opponent == opponentOrder[ind]) {
            break
          }
        }
        ind <- ind * 5
        if (ind == 5) {
          df[j + adjAmt, 16:18] <- triplets[k, 1:3]
        } else {
          df[j + adjAmt, (ind+11):(ind+13)] <- triplets[k, 1:3]
        }
      }
    }
  }
  
  if (df[nrow(df), 7] != "folds") {
    if (actionPoints[length(actionPoints)] != length(flop)) {
      finalTriplets <- data.frame(name = character(0), action = character(0), amt = numeric(0), stringsAsFactors = F)
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
  return(list(df = df, finalTriplets = finalTriplets, flopCards = cards))
}

getOpponentOrder <- function(df) {
  return(Opponents = df[1, c(16, 21, 26, 31, 36)])
}

preFlopFun <- function(hand, df, breakPoints) {
  preFlop <- hand[(breakPoints[1] + 1):(breakPoints[2] - 1)]
  actionPoints <- c(1, grep(paste0(screenName, ":"), preFlop))
  numActions <- length(actionPoints) - 1
  for (j in 1:numActions) {
    triplets <- data.frame(name = character(0), action = character(0), amt = numeric(0), stringsAsFactors = F)
    for (i in (actionPoints[j] + 1):actionPoints[j + 1]) {
      triplet <- processLine(preFlop[i])
      triplets[i - 1, 1] <- triplet$name
      triplets[i - 1, 2] <- triplet$action
      triplets[i - 1, 3] <- triplet$amt
    }
    # Update data frame
    df[j, 6:8] <- triplets[nrow(triplets), 1:3]
    if (nrow(triplets) != 1) {
      for (k in 1:(nrow(triplets) - 1)) {
       k <- k * 5
       if (k == 5) {
         df[j, 16:18] <- triplets[(k/5), 1:3]
       } else {
         df[j, (k+11):(k+13)] <- triplets[(k/5), 1:3]
       }
     }
    }
  }
  # Check if last action point was a fold.  If that's false, check if last action point is
  # length(preflop). Otherwise, we need to send back triplets.
  if (df[nrow(df), 7] != "folds") {
    if (actionPoints[length(actionPoints)] != length(preFlop)) {
      finalTriplets <- data.frame(name = character(0), action = character(0), amt = numeric(0), stringsAsFactors = F)
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
  return(list(df = df, finalTriplets = finalTriplets))
}

### processLine will return name, action, amt triplet. For loop will combine these into a frame.
### processLine should be general for any time a line can be processed.
processLine <- function(line) {
  line <- strsplit(line, ":")
  name <- line[[1]][1]
  actionAmt <- line[[1]][2]
  actionAmt <- trimws(actionAmt)
  actionAmt <- strsplit(actionAmt, " ")
  action <- actionAmt[[1]][1]
  if (length(actionAmt[[1]]) == 1) {
    amt <- NA
  } else {
    amt <- as.numeric(actionAmt[[1]][2])
  }
  return(list(name = name, action = action, amt = amt))
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
  seatsText <- c(seatsText[jamesPos:length(seatsText)], seatsText[1:(jamesPos - 1)])
  split <- strsplit(seatsText, " \\(")
  for (i in 1:length(seatsText)) {
    playerInfo[i, 1] <- substr(split[[i]][1], 9, nchar(split[[i]][1]))
    playerInfo[i, 2] <- as.numeric(gsub("[^0-9]+", "", split[[i]][2]))
  }
  return(playerInfo)
} 




