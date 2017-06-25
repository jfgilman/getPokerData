

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



preFlop <- function(hand, numPlayers, JamesPos){
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

setPlayerNames <- function(hand, numPlayers, JamesPos){
  
  playerList <- list(Op1Name = character(0), Op2Name = character(0),
                     Op3Name = character(0), Op4Name = character(0),
                     Op5Name = character(0))
  
  if(numPlayers == 6){
    if(JamesPos == "UTG"){
      playerList$Op1Name <- "BB"
      playerList$Op2Name <- "SB"
      playerList$Op3Name <- "BTN"
      playerList$Op4Name <- "CO"
      playerList$Op5Name <- "MP"
    }else if(JamesPos == "MP"){
      playerList$Op1Name <- "UTG"
      playerList$Op2Name <- "BB"
      playerList$Op3Name <- "SB"
      playerList$Op4Name <- "BTN"
      playerList$Op5Name <- "CO"
    }else if(JamesPos == "CO"){
      playerList$Op1Name <- "MP"
      playerList$Op2Name <- "UTG"
      playerList$Op3Name <- "BB"
      playerList$Op4Name <- "SB"
      playerList$Op5Name <- "BTN"
    }else if(JamesPos == "BTN"){
      playerList$Op1Name <- "CO"
      playerList$Op2Name <- "MP"
      playerList$Op3Name <- "UTG"
      playerList$Op4Name <- "BB"
      playerList$Op5Name <- "SB"
    }else if(JamesPos == "SB"){
      playerList$Op1Name <- "BTN"
      playerList$Op2Name <- "CO"
      playerList$Op3Name <- "MP"
      playerList$Op4Name <- "UTG"
      playerList$Op5Name <- "BB"
    }else{
      playerList$Op1Name <- "SB"
      playerList$Op2Name <- "BTN"
      playerList$Op3Name <- "CO"
      playerList$Op4Name <- "MP"
      playerList$Op5Name <- "UTG"
    }
  }else if(numPlayers == 5){
    if(JamesPos == "UTG"){
      playerList$Op1Name <- "BB"
      playerList$Op2Name <- "SB"
      playerList$Op3Name <- "BTN"
      playerList$Op4Name <- "CO"
      playerList$Op5Name <- NULL
    }else if(JamesPos == "CO"){
      playerList$Op1Name <- "UTG"
      playerList$Op2Name <- "BB"
      playerList$Op3Name <- "SB"
      playerList$Op4Name <- "BTN"
      playerList$Op5Name <- NULL
    }else if(JamesPos == "BTN"){
      playerList$Op1Name <- "CO"
      playerList$Op2Name <- "UTG"
      playerList$Op3Name <- "BB"
      playerList$Op4Name <- "SB"
      playerList$Op5Name <- NULL
    }else if(JamesPos == "SB"){
      playerList$Op1Name <- "BTN"
      playerList$Op2Name <- "CO"
      playerList$Op3Name <- "UTG"
      playerList$Op4Name <- "BB"
      playerList$Op5Name <- NULL
    }else{
      playerList$Op1Name <- "SB"
      playerList$Op2Name <- "BTN"
      playerList$Op3Name <- "CO"
      playerList$Op4Name <- "UTG"
      playerList$Op5Name <- NULL
    }
  }else if(numPlayers == 4){
    if(JamesPos == "CO"){
      playerList$Op1Name <- "BB"
      playerList$Op2Name <- "SB"
      playerList$Op3Name <- "BTN"
      playerList$Op4Name <- NULL
      playerList$Op5Name <- NULL
    }else if(JamesPos == "BTN"){
      playerList$Op1Name <- "CO"
      playerList$Op2Name <- "BB"
      playerList$Op3Name <- "SB"
      playerList$Op4Name <- NULL
      playerList$Op5Name <- NULL
    }else if(JamesPos == "SB"){
      playerList$Op1Name <- "BTN"
      playerList$Op2Name <- "CO"
      playerList$Op3Name <- "BB"
      playerList$Op4Name <- NULL
      playerList$Op5Name <- NULL
    }else{
      playerList$Op1Name <- "SB"
      playerList$Op2Name <- "BTN"
      playerList$Op3Name <- "CO"
      playerList$Op4Name <- NULL
      playerList$Op5Name <- NULL
    }
  }else if(numPlayers == 3){
    if(JamesPos == "BTN"){
      playerList$Op1Name <- "BB"
      playerList$Op2Name <- "SB"
      playerList$Op3Name <- NULL
      playerList$Op4Name <- NULL
      playerList$Op5Name <- NULL
    }else if(JamesPos == "SB"){
      playerList$Op1Name <- "BB"
      playerList$Op2Name <- "BTN"
      playerList$Op3Name <- NULL
      playerList$Op4Name <- NULL
      playerList$Op5Name <- NULL
    }else{
      playerList$Op1Name <- "BTN"
      playerList$Op2Name <- "SB"
      playerList$Op3Name <- NULL
      playerList$Op4Name <- NULL
      playerList$Op5Name <- NULL
    }
  }else {
    if(JamesPos == "SB"){
      playerList$Op1Name <- "BB"
      playerList$Op2Name <- NULL
      playerList$Op3Name <- NULL
      playerList$Op4Name <- NULL
      playerList$Op5Name <- NULL
    }else{
      playerList$Op1Name <- "SB"
      playerList$Op2Name <- NULL
      playerList$Op3Name <- NULL
      playerList$Op4Name <- NULL
      playerList$Op5Name <- NULL
    }
  }
  
  return(playerList)
}


preProcess1 <- function(hand, breakPoints) {
  JamesPos <- strsplit(hand[grep("Hero", hand, fixed = T)[1]], " ")[[1]][2]
  JamesPos <- substring(JamesPos, 2)
  JamesPos <- strtrim(JamesPos, nchar(JamesPos) - 2)
  
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

  return(list(numPlayers = numPlayers, JamesPos = JamesPos, BBInfo = BBInfo,
              SBInfo = SBInfo, BTNInfo = BTNInfo, COInfo = COInfo,
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

# setStartVals <- function(df, playerInfo){
#   # start has cards to false for last 4 players since there must always be 2
#   df[1,c(30,35,40)] <- F
#   # always setting preflop pot to 1 big and 1 small blind for now
#   df[1,10] <- df[1,41]*1.5
#   # setting my stack size to start the hand 
#   df[1,5] <- playerInfo[1,2]
#   # setting each players stack size and that they have cards to start
#   for(i in 1:(nrow(playerInfo) - 1)){
#     df[1,i*5 + 11] <- playerInfo[i + 1, 1]
#     df[1,i*5 + 14] <- playerInfo[i + 1, 2]
#     df[1,i*5 + 15] <- T
#   }
#   return(df)
# }
