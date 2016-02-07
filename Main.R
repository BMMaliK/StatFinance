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
  AdjXts = merge.xts(AdjXts, RawDataList[[Name]][,6])
}
remove(list = Tickers) #Nettoie l'environnement
remove(Name)

names(AdjXts) = Tickers
AdjXts = AdjXts["2011/"]
ReturnsXts = ROC(AdjXts) ## Calcul des returns
ReturnsXts = na.omit(ReturnsXts)
l = length(ReturnsXts[,1])

## ACP

install.packages("FactoMineR")
library(FactoMineR)
x11()
par(mfrow=c(1,2))
PCAres = PCA(ReturnsXts, scale.unit = T, ncp = 3, graph = T)

plot.PCA(PCAres, axes = c(1,2), choix="ind", habillage = "none", select = (l-20):l, unselect = .98)
plot.PCA(PCAres, axes = c(1,2), choix="var")
dimdesc(PCAres, axes=c(1,2))

## Markowitz
PastXts = ReturnsXts["/2014"]
FutureXts = ReturnsXts["2015/"]

NPast=nrow(PastXts)
pPast=ncol(PastXts)
PastMeans = colMeans(PastXts)
