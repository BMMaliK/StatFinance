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

## Nettoyage de la matrice de corrélation
ReturnsXts = ReturnsXts+1
PastXts = ReturnsXts["/2014"]
FutureXts = ReturnsXts["2015/"]

NPast=nrow(PastXts)
p=ncol(ReturnsXts)
N = nrow(ReturnsXts)

PastMeans = colMeans(PastXts)
PastCor = cor(PastXts)

Z = svd(PastCor)
q = p/NPast
Lambda_plus = 1+sqrt(q)
Z$d[Z$d<Lambda_plus] = 0
a = (p-sum(Z$d))/p
CorClean = Z$u %*% diag(Z$d) %*% t(Z$v)
CorClean = CorClean + a*diag(p)
