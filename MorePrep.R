
rm(list = ls())

load("/Users/jfgilman/Desktop/pokerData.RData")

final <- final[which(final$JamesAction != "doesn't"),]

colnames(final)


# make Big Blind matrix ---------------------------------------------------

BBMat <- cbind(final$BBPlayer, final$JamesName, final$Op1Name, final$Op2Name, final$Op3Name, final$Op4Name, final$Op5Name)
BBMat <- cbind(as.numeric(BBMat[,1] == BBMat[,2]), as.numeric(BBMat[,1] == BBMat[,3]),
               as.numeric(BBMat[,1] == BBMat[,4]), as.numeric(BBMat[,1] == BBMat[,5]),
               as.numeric(BBMat[,1] == BBMat[,6]), as.numeric(BBMat[,1] == BBMat[,7]))
BBMat[is.na(BBMat)] <- 0
colnames(BBMat) <- c("jamesBB", "Op1BB", "Op2BB", "Op3BB", "Op4BB", "Op5BB")
dim(BBMat)



# make card matrix --------------------------------------------------------

c1 <- do.call(rbind, strsplit(final$JamesCard1, ""))
c2 <- do.call(rbind, strsplit(final$JamesCard1, ""))

myCards <- cbind(c1,c2)
myCards[is.na(myCards)] <- "M"
colnames(myCards) <- c("C1Val", "C1Suit", "C2Val", "C2Suit")
myCards <- as.data.frame(myCards)

myCardsMat <- model.matrix(~C1Val + C1Suit + C2Val + C2Suit + 0, data = myCards)
dim(myCardsMat)
colnames(myCardsMat)

# Community card #1
comC1 <- do.call(rbind, strsplit(final$comCard1, ""))
comC1[is.na(comC1)] <- "M"

colnames(comC1) <- c("Com1Val", "Com1Suit")
comC1 <- as.data.frame(comC1)

comC1Mat <- model.matrix(~Com1Val + Com1Suit + 0, data = comC1)
dim(comC1Mat)

# Community card #2
comC2 <- do.call(rbind, strsplit(final$comCard2, ""))
comC2[is.na(comC2)] <- "M"

colnames(comC2) <- c("Com2Val", "Com2Suit")
comC2 <- as.data.frame(comC2)

comC2Mat <- model.matrix(~Com2Val + Com2Suit + 0, data = comC2)

# Community card #3
comC3 <- do.call(rbind, strsplit(final$comCard3, ""))
comC3[is.na(comC3)] <- "M"

colnames(comC3) <- c("Com3Val", "Com3Suit")
comC3 <- as.data.frame(comC3)

comC3Mat <- model.matrix(~Com3Val + Com3Suit + 0, data = comC3)

# Community card #4
comC4 <- do.call(rbind, strsplit(final$comCard4, ""))
comC4[is.na(comC4)] <- "M"

colnames(comC4) <- c("Com4Val", "Com4Suit")
comC4 <- as.data.frame(comC4)

comC4Mat <- model.matrix(~Com4Val + Com4Suit + 0, data = comC4)

# Community card #5
comC5 <- do.call(rbind, strsplit(final$comCard5, ""))
comC5[is.na(comC5)] <- "M"

colnames(comC5) <- c("Com5Val", "Com5Suit")
comC5 <- as.data.frame(comC5)

comC5Mat <- model.matrix(~Com5Val + Com5Suit + 0, data = comC5)


# street and hasCards Matrix ----------------------------------------------

SandHCs <- cbind(final$street, final$Op1HasCards, final$Op2HasCards, final$Op3HasCards, final$Op4HasCards, final$Op5HasCards)
SandHCs <- as.data.frame(SandHCs)
head(SandHCs)
colnames(SandHCs) <- c("s", "Op1HC", "Op2HC", "Op3HC", "Op4HC", "Op5HC")

SandHCs$Op2HC[is.na(SandHCs$Op2HC)] <- FALSE

SandHCsMat <- model.matrix(~s + Op1HC + Op2HC + Op3HC + Op4HC + Op5HC + 0, data = SandHCs)
dim(SandHCsMat)

head(SandHCsMat)

# Standardize by BB and Actions matrix ------------------------------------

M <- final[,c(5,7,8,10,17:19,22:24,27:29,32:34,37:39)]

M[is.na(M)] <- 0

M[,c(1,3,4,6,7,9,10,12,13,15,16,18,19)] <- M[,c(1,3,4,6,7,9,10,12,13,15,16,18,19)]/final$bigBlindAmt

getAction <- function(action, amount){
  if(action == "folds"){
    return("folds")
  } else if(action == "calls" || action == "checks"){
    return("check/call")
  } else {
    if(amount <= 4){
      return("bets<4")
    } else if(amount <= 9){
      return("bets[4,9]")
    } else if(amount <= 15){
      return("bets[9,15]")
    } else if(amount <= 28){
      return("bets[15,28]")
    } else{
      return("bets>28")
    }
  }
}

JAct <- c()
for(i in 1:length(M$JamesAction)){
  JAct[i] <- getAction(M$JamesAction[i], M$JamesAmount[i])
}
M$OP1Amount[is.na(M$OP1Amount)] <- 0
Op1Act <- c()
for(i in 1:length(M$Op1Action)){
  Op1Act[i] <- getAction(M$Op1Action[i], M$OP1Amount[i])
}
M$OP2Amount[is.na(M$OP2Amount)] <- 0
Op2Act <- c()
for(i in 1:length(M$Op2Action)){
  Op2Act[i] <- getAction(M$Op2Action[i], M$OP2Amount[i])
}
M$OP3Amount[is.na(M$OP3Amount)] <- 0
Op3Act <- c()
for(i in 1:length(M$Op3Action)){
  Op3Act[i] <- getAction(M$Op3Action[i], M$OP3Amount[i])
}
M$OP4Amount[is.na(M$OP4Amount)] <- 0
Op4Act <- c()
for(i in 1:length(M$Op4Action)){
  Op4Act[i] <- getAction(M$Op4Action[i], M$OP4Amount[i])
}
M$OP5Amount[is.na(M$OP5Amount)] <- 0
Op5Act <- c()
for(i in 1:length(M$Op5Action)){
  Op5Act[i] <- getAction(M$Op5Action[i], M$OP5Amount[i])
}

opActMat <- cbind(Op1Act, Op2Act, Op3Act, Op4Act, Op5Act)
opActMat <- as.data.frame(opActMat)

opActMat <- model.matrix(~ Op1Act + Op2Act + Op3Act + Op4Act + Op5Act + 0, data = opActMat)
dim(opActMat)

JAct <- as.data.frame(JAct)

JActMat <- model.matrix(~JAct + 0, data = JAct)
dim(JActMat)

# now Stacks and pot matrix

getPotSize <- function(amount){
  if(amount <= 3){
    return("small")
  } else if(amount <= 10) {
    return("small/mid")
  } else if(amount <= 25){
    return("mid")
  } else if(amount <= 60) {
    return("mid/large")
  } else if(amount <= 100) {
    return("large")
  } else {
    return("Vlarge")
  }
}

getStackSize <- function(amount){
  if(amount <= 15){
    return("small")
  } else if(amount <= 30) {
    return("small/mid")
  } else if(amount <= 75){
    return("mid")
  } else if(amount <= 120) {
    return("mid/large")
  } else if(amount <= 200) {
    return("large")
  } else {
    return("Vlarge")
  }
}

M$pot[is.na(M$pot)] <- 0
potSize <- c()
for(i in 1:length(M$pot)){
  potSize[i] <- getPotSize(M$pot[i])
}

M$JamesStack[is.na(M$JamesStack)] <- 0
JStack <- c()
for(i in 1:length(M$JamesStack)){
  JStack[i] <- getStackSize(M$JamesStack[i])
}

M$Op1Stack[is.na(M$Op1Stack)] <- 0
Op1Stack <- c()
for(i in 1:length(M$Op1Stack)){
  Op1Stack[i] <- getStackSize(M$Op1Stack[i])
}

M$Op2Stack[is.na(M$Op2Stack)] <- 0
Op2Stack <- c()
for(i in 1:length(M$Op2Stack)){
  Op2Stack[i] <- getStackSize(M$Op2Stack[i])
}

M$Op3Stack[is.na(M$Op3Stack)] <- 0
Op3Stack <- c()
for(i in 1:length(M$Op3Stack)){
  Op3Stack[i] <- getStackSize(M$Op3Stack[i])
}

M$Op4Stack[is.na(M$Op4Stack)] <- 0
Op4Stack <- c()
for(i in 1:length(M$Op4Stack)){
  Op4Stack[i] <- getStackSize(M$Op4Stack[i])
}

M$Op5Stack[is.na(M$Op5Stack)] <- 0
Op5Stack <- c()
for(i in 1:length(M$Op5Stack)){
  Op5Stack[i] <- getStackSize(M$Op5Stack[i])
}

SandP <- cbind(JStack, Op1Stack, Op2Stack, Op5Stack, Op3Stack, Op4Stack, Op5Stack)
SandP <- as.data.frame(SandP)

SandPMat <- model.matrix(~JStack + Op1Stack + Op2Stack + Op5Stack + Op3Stack + Op4Stack + Op5Stack + 0, data = SandP)
dim(SandPMat)


binaryX <- cbind(final$hand, BBMat, myCardsMat, comC1Mat, comC2Mat, comC3Mat, comC4Mat, comC5Mat,
                     SandHCsMat, opActMat, SandPMat)

head(binaryX)

binaryY <- cbind(final$hand, JActMat)
head(binaryY)

dim(binaryX)
dim(binaryY)


newX <- binaryX[1:3,]
newY <- binaryY[1:3,]

z199 <- matrix(0, ncol = 199, nrow = 5)
z8 <- matrix(0, ncol = 8, nrow = 5)

newX <- rbind(newX, z199)
newY <- rbind(newY, z8)

handIndex <- c()
handIndex[1] <- 1
for(i in 1:(nrow(binaryX)-1)){
  handNum <- binaryX[i,1]
  if(handNum == binaryX[i+1,1]){
    handIndex[i+1] <- 0
  } else {
    handIndex[i+1] <- 1
  }
}

for(i in 4:length(binaryX)){
  
  newX <- rbind(newX, binaryX[i,])
  newY <- rbind(newY, binaryY[i,])
  if(handIndex[i] == 1){
    
  }
}


length(table(binaryX[,1]))







pokerBinary <- list(X = binaryX, Y = binaryY)

save(pokerBinary, file = "pokerBinary.RData")
