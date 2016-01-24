##################################################
## Projet Méthodes Statistiques pour la Finance ##
##  BELKHIRIA Mohamed Malek &                   ##
##                         LOPEZ RUIZ Gabriela  ##
##################################################

############################
# Récupération des données #
############################

## Installe et charge la bibliothèque nécessaire à la récupération des données
install.packages("quantmod")
library(quantmod)

## Charge la liste des composantes du Dow Jones
## Source : https://finance.yahoo.com/q/cp?s=%5EDJI
TickerList = read.csv("TickerList.csv", sep = ';')
TickerList
Tickers = as.vector(as.vector(TickerList$Ticker[1:30]))

## Récupération des données historiques grace à la Bibliothèque Quantmod
## Attention ! : Nécessite une connection à Internet
getSymbols(Tickers)
RawDataList=list()
RawDataXts = xts()
AdjXts = xts()
for(Name in Tickers) {
  RawDataList[[Name]]=get(Name)
  RawDataXts = merge.xts(RawDataXts, RawDataList[[Name]])
  if(Name==Tickers[1])
    AdjXTs = RawDataList[[Name]][,6]
  else
    AdjXTs = merge.xts(AdjXTs, RawDataList[[Name]][,6])
}
remove(list = Tickers) #Nettoie l'environnement
remove(Name)