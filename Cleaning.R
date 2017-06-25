rm(list = ls())

setwd("~/Desktop/Hands")

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

hands <- processFile("hands-6_18_17.txt")


df <- data.frame(hand = integer(0), BBSize = integer(0), JamesPos = character(0), JamesCard1 = character(0),
           JamesCard2 = character(0), JamesStack = numeric(0),
           JamesAction = character(0), JamesAmount = integer(0),
           street = character(0), pot = numeric(0), comCard1 = character(0),
           comCard2 = character(0), comCard3 = character(0),
           comCard4 = character(0), comCard5 = character(0), Op1Name = character(0), 
           Op1Action = character(0), OP1Amount = integer(0), Op1Stack = numeric(0),
           Op1VPIP = numeric(0), Op1PFR = numeric(0), Op13Bet = numeric(0), Op1NumHands = integer(0),
           Op1HasCards = logical(0), Op2Name = character(0), Op2Action = character(0),
           OP2Amount = integer(0), Op2Stack = numeric(0),
           Op2VPIP = numeric(0), Op2PFR = numeric(0), Op23Bet = numeric(0), Op2NumHands = integer(0),
           Op2HasCards = logical(0), Op3Name = character(0), Op3Action = character(0),
           OP3Amount = integer(0), Op3Stack = numeric(0),
           Op3VPIP = numeric(0), Op3PFR = numeric(0), Op33Bet = numeric(0), Op3NumHands = integer(0),
           Op3HasCards = logical(0),  Op4Name = character(0), Op4Action = character(0),
           OP4Amount = integer(0), Op4Stack = numeric(0),
           Op4VPIP = numeric(0), Op4PFR = numeric(0), Op43Bet = numeric(0), Op4NumHands = integer(0),
           Op4HasCards = logical(0),  Op5Name = character(0), Op5Action = character(0),
           OP5Amount = integer(0), Op5Stack = numeric(0),
           Op5VPIP = numeric(0), Op5PFR = numeric(0), Op53Bet = numeric(0), Op5NumHands = integer(0),
           Op5HasCards = logical(0), stringsAsFactors=FALSE)

################################################################################

preProcessData <- preProcess(hands[[1]])
proFlopData <- preFlop(hands[[1]], numPlayers, JamesPos)
playerNames <- setPlayerNames(hands[[1]],numPlayers, JamesPos)

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


preProcess <- function(hand, breakPoints) {
  JamesPos <- strsplit(hand[grep("Hero", hand, fixed = T)[1]], " ")[[1]][2]
  JamesPos <- substring(JamesPos, 2)
  JamesPos <- strtrim(JamesPos, nchar(JamesPos) - 2)
  
  bbAmount <- unique(na.omit(as.numeric(unlist(strsplit(unlist(hand[1]), "[^0-9]+")))))[1]
  
  intro <- hand[grep("VPIP:", hand, fixed = T)]
  
  numPlayers <- length(intro) + 1

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
    BBInfo <- processPlayer(intro[BBline])
  }
  
  if(length(SBline) > 0){
    SBInfo <- processPlayer(intro[SBline])
  }
  
  if(length(BTNline) > 0){
    BTNInfo <- processPlayer(intro[BTNline])
  }
  
  if(length(COline) > 0){
    COInfo <- processPlayer(intro[COline])
  }
  
  if(length(MPline) > 0){
    MPInfo <- processPlayer(intro[MPline])
  }
  
  if(length(UTGline) > 0){
    UTGInfo <- processPlayer(intro[UTGline])
  }

  return(list(numPlayers = numPlayers, BBInfo = BBInfo, SBInfo = SBInfo,
              BTNInfo = BTNInfo, COInfo = COInfo, MPInfo = MPInfo,
              UTGInfo = UTGInfo, bbAmount = bbAmount))
}

processPlayer <- function(line) {
  
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
