rm(list = ls())

# Windows
setwd("C:/Users/mjahja/Desktop/Research/Poker/Data")

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

hands <- processFile("100NL.txt")
screenName <- "LL-Poker1"

handLong <- hands[[1]][grep("PokerStars Hand #",hands[[1]]):length(hands[[1]])]
handShort <- hands[[1]][1:(grep("PokerStars Hand #",hands[[1]])-2)]

handLong <- cleanLong(handLong)

#Windows
setwd("C:/Users/mjahja/Desktop/Research/Poker/getPokerData")

source("Cleaning.R")

df <- data.frame(handNum = integer(0), BBSize = integer(0), JamesPos = character(0), JamesCard1 = character(0),
                 JamesCard2 = character(0), JamesStack = numeric(0), JamesName = character(0),
                 JamesAction = character(0), JamesAmount = integer(0),
                 street = character(0), pot = numeric(0), comCard1 = character(0),
                 comCard2 = character(0), comCard3 = character(0),
                 comCard4 = character(0), comCard5 = character(0), Op1Pos = character(0),  Op1Name = character(0),
                 Op1Action = character(0), OP1Amount = integer(0), Op1Stack = numeric(0),
                 Op1VPIP = numeric(0), Op1PFR = numeric(0), Op13Bet = numeric(0), Op1NumHands = integer(0),
                 Op1HasCards = logical(0), Op2Pos = character(0),  Op2Name = character(0), Op2Action = character(0),
                 OP2Amount = integer(0), Op2Stack = numeric(0),
                 Op2VPIP = numeric(0), Op2PFR = numeric(0), Op23Bet = numeric(0), Op2NumHands = integer(0),
                 Op2HasCards = logical(0), Op3Pos = character(0),  Op3Name = character(0), Op3Action = character(0),
                 OP3Amount = integer(0), Op3Stack = numeric(0),
                 Op3VPIP = numeric(0), Op3PFR = numeric(0), Op33Bet = numeric(0), Op3NumHands = integer(0),
                 Op3HasCards = logical(0), Op4Pos = character(0),  Op4Name = character(0), Op4Action = character(0),
                 OP4Amount = integer(0), Op4Stack = numeric(0),
                 Op4VPIP = numeric(0), Op4PFR = numeric(0), Op43Bet = numeric(0), Op4NumHands = integer(0),
                 Op4HasCards = logical(0), Op5Pos = character(0),  Op5Name = character(0), Op5Action = character(0),
                 OP5Amount = integer(0), Op5Stack = numeric(0),
                 Op5VPIP = numeric(0), Op5PFR = numeric(0), Op53Bet = numeric(0), Op5NumHands = integer(0),
                 Op5HasCards = logical(0), stringsAsFactors=FALSE)

preProcessData1 <- preProcess1(handShort)
proFlopData <- preFlop(handShort, preProcessData$numPlayers, preProcessData$JamesPos)
playerNames <- setPlayerNames(handShort, preProcessData$numPlayers, preProcessData$JamesPos)
breakPoints <- getBreakpoints(handLong)
# maybe get player order function 
# df <- setStartVals(df, preProcessData1)
preProcessData2 <- preProcess2(handLong, breakPoints)
preFlop <- preFlopFun(handLong, df, breakPoints)
df <- preFlop[[1]]

