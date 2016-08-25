## PREPARATION
setwd("~/Zipcode/WEST")
board = read.csv("ALL_WEST_SITES__v2.csv")
board = board[board$POPs!='#N/A' & board$throughput.Score!='#NUM!',]
board$Network.KPI.Scores = as.numeric(as.character(board$Network.KPI.Scores))
board$throughput.Score = as.numeric(as.character(board$throughput.Score))
board$POPs= as.numeric(as.character(board$POPs))
board$Pops.Growth.Values= as.numeric(as.character(board$Pops.Growth.Values))

## descriptive
hist(board$Network.KPI.Scores)
hist(board$X2G.3G.4G.Voice.DCR)
hist(board$X2G.3G.4G.Voice.DCR.Value...)
hist(board$X4G.VOLTE.ACCESS.FAIL.RATE)
hist(board$X4G.VOLTE.ACCESS.FAIL.Value...)
hist(board$throughput.Score)
hist(board$thpt.Value.kbps.)
hist(board$LTE.Leakage)
hist(board$LTE.Leakage.Value...)
hist(board$POPs)
hist(board$Churn.Values)
hist(board$ATT_VZ_Port..Vaues)
hist(board$Market_Share.Values)
hist(board$X.IBC.Value)
hist(board$Congested.LTE)
hist(board$VoLTE.AFR.Value...)
hist(board$VoLTE.DCR.Value...)
# correlation:
library(plotly)
library(ggplot2)

# CHURN
q = qplot(data = board, y=Churn.Values, x=VoLTE.DCR.Value...)
q = qplot(data = board, y=Churn.Values, x=VoLTE.AFR.Value...)
q = qplot(data = board, y=Churn.Values, x=thpt.Value.kbps.)
q = qplot(data = board, y=Churn.Values, x=LTE.Leakage.Value...  )
q = qplot(data = board, y=Churn.Values, x= X.IBC.Value )
ggplotly(q)

list = data.frame(
  cor(board$Churn.Values, board$Network.KPI.Scores), #-0.066
  cor(board$Churn.Values, board$VoLTE.DCR.Value...), # 0.066
  cor(board$Churn.Values, board$VoLTE.AFR.Value...), # 0.1415
  cor(board$Churn.Values, board$thpt.Value.kbps.), # -0.0458
  cor(board$Churn.Values, board$LTE.Leakage.Value...), # -0.0108
  cor(board$Churn.Values, board$ATT_VZ_Port..Vaues), # 0.1554
  cor(board$Churn.Values, board$POPs), # -0.1069
  cor(board$Churn.Values, board$X.IBC.Value), #-0.1148
  cor(board$Churn.Values, board$Congested.LTE.Value), # -0.0189
  cor(board$Churn.Values, board$X.avaliability.Values)) # -0.0140

write.csv(list, "correlation_churn.csv")

# Market penetration
q = qplot(data = board, y=Market_Share.Values, x=VoLTE.DCR.Value...)
q = qplot(data = board, y=Market_Share.Values, x=VoLTE.AFR.Value...)
q = qplot(data = board, y=Market_Share.Values, x=thpt.Value.kbps.)
q = qplot(data = board, y=Market_Share.Values, x=LTE.Leakage.Value...  )
q = qplot(data = board, y=Market_Share.Values, x= X.IBC.Value )
ggplotly(q)

list1 = data.frame(
  cor(board$Market_Share.Values, board$Network.KPI.Scores),#0.25
  cor(board$Market_Share.Values, board$VoLTE.DCR.Value...), # -0.21
  cor(board$Market_Share.Values, board$VoLTE.AFR.Value...), # -0.21
  cor(board$Market_Share.Values, board$thpt.Value.kbps.), # -0.0458
  cor(board$Market_Share.Values, board$LTE.Leakage.Value...), # -0.0108
  cor(board$Market_Share.Values, board$ATT_VZ_Port..Vaues), # 0.1554
  cor(board$Market_Share.Values, board$POPs), # -0.1069
  cor(board$Market_Share.Values, board$X.IBC.Value), #0.478
  cor(board$Market_Share.Values, board$Congested.LTE.Value), # -0.0189
  cor(board$Market_Share.Values, board$X.avaliability.Values)) # -0.0140

cor.mat = cor(board[,-1])
write.csv(cor.mat,"cor.mat.csv")
write.csv(list1, "correlation_market_penetration.csv")
# regression
churn = lm(data = board,
           Churn.Values~X2G.3G.4G.Voice.DCR.Value...
           +X4G.VOLTE.ACCESS.FAIL.Value...
           +thpt.Value.kbps.
           +LTE.Leakage.Value...
           +X.IBC.Value
           +POPs
           +Congested.LTE.Value
           +X.avaliability.Values)
summary(churn)
anova(churn)
# K-fold cross-validation
library(DAAG)
cv=cv.lm(data=board, churn, m=3) # 3 fold cross-validation
stepwise(churn)
plot(churn)

#
library(MASS)
step <- stepAIC(churn, direction="both")
step$anova # display results