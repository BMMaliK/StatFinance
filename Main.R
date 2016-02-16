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
Tickers = as.vector(TickerList$Ticker)


## Récupération des données historiques grace à la Bibliothèque Quantmod
## Attention ! : Nécessite une connection à Internet
getSymbols(Tickers)
Tickers[31] = "DJI"
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

names(AdjXts) = Tickers ## Formattage des noms de colonnes
AdjXts = AdjXts["2011/"] ## Données à partir de début 2011
ReturnsXts = ROC(AdjXts) ## Calcul des returns
ReturnsXts = na.omit(ReturnsXts) ## Suppression des lignes contenant des données manquantes 
l = length(ReturnsXts[,1]) ##Nombre de dates restantes
l

#Journées Bull/Bear
stateXts = ReturnsXts[,"DJI"] 
names(stateXts) = "DayState"
stateXts[,"DayState"]=(stateXts[,"DayState"]>0)
ReturnsXts = merge.xts(ReturnsXts, stateXts)

############################
#           ACP            #
############################

install.packages("FactoMineR") ## Installe le package nécessaire pour l'ACP
library(FactoMineR) ## Charge la bibliothèque
x11() ## Nouvelle Fenêtre graphique
par(mfrow=c(1,2)) ## Formattage

PCAres = PCA(ReturnsXts, quanti.sup = 31, quali.sup = 32, scale.unit = T, ncp = 3, graph = T) ## ACP

## Projection des individus
# Rouge : DJI > 0 # Noir : DJI <= 0
plot.PCA(PCAres, axes = c(1,2), choix="ind", habillage = 32, select = (1286-20):1286, unselect = .98, xlim = c(-10,10), ylim = c(-5,5))
## Projection des 
plot.PCA(PCAres, axes = c(1,2), choix="var")
## Corrélation des variables aux deux premières composantes principales
dimdesc(PCAres, axes=c(1,2))

## Journée Bull
mean(ReturnsXts["20160122",1:30])
ReturnsXts["20160122","DJI"]

# Journée Bear
mean(ReturnsXts["20160211",1:30])
ReturnsXts["20160211","DJI"]

###########################################
# Nettoyage de la matrice de corrélation  #
###########################################

ReturnsXts = ReturnsXts+1
ReturnsXts[,"DayState"] = ReturnsXts[,"DayState"] - 1
PastXts = ReturnsXts["/2014"] #Données jusqu'à fin 2014
FutureXts = ReturnsXts["2015/"] #Données à partir de début 2015

NPast=nrow(PastXts) #Nombre de dates passées
p=ncol(ReturnsXts[,1:30]) #Dimension
N = nrow(ReturnsXts) #Nombre total de dates

PastMeans = colMeans(PastXts[1:30]) #Moyenne
PastCor = cor(PastXts[,1:30]) #Estimateur empirique de la matrice de corrélation
x11() ## Nouvelle Fenêtre graphique
par(mfrow=c(1,2)) ## Formattage
image(PastCor, xlab = "Corrélation empirique")

Z = svd(PastCor) #Digonalisation
q = p/NPast 
lambda.plus = 1+sqrt(q) #cut-off
Z$d[Z$d<lambda.plus] = 0 # troncature des valeurs propres
a = (p-sum(Z$d))/p
Z$d[Z$d<lambda.plus] = a #préservation de la trace
CorClean = Z$u %*% diag(Z$d) %*% t(Z$v) #reconstitution de la matrice
diags = diag(CorClean) %o% rep(1,p)
diags = diags*t(diags)
CorClean = CorClean / sqrt(diags) #renormalisation pour avoir une matrice de corrélation
image(CorClean, xlab = "Corrélation nettoyée")

###############
#  Markowitz  #
###############

# Chargement de la bibliothèque
install.packages("tseries")
library(tseries)

ret.mat = PastXts[,1:30] #Données

# Mu et Sigma (en utilisant les corrélations nettoyées)
mu.hat = PastMeans[1:30]
cov.hat = var(PastXts[,1:30])
vols = sqrt(diag(cov.hat))
cov.hat = diag(vols) %*% CorClean %*% diag(vols)

source("portfolio.r") #Chargement des fonctions nécessaires

# Calcule le Portefeuille efficient avec un objectif de rendement égal à la moyenne
ep = efficient.portfolio(mu.hat, cov.hat, mu.hat[1])
ep
# Portefeuille tangent
tp = tangency.portfolio(mu.hat, cov.hat, 0.005)
tp
# Portefeuille du minimum global de variance
gp = globalMin.portfolio(mu.hat, cov.hat)
gp

# Frontière efficiente
ef = efficient.frontier(mu.hat, cov.hat, 
                        alpha.min=0, alpha.max=1)

# Portefeuille efficient avec portfolio.optim
ep2 = portfolio.optim(ret.mat, pm = mu.hat[1], shorts=T)
names(ep2)
ep2$pm
ep2$ps
ep2$pw

# Portefeuille efficient sans vente à découvert
ep2.ns = portfolio.optim(ret.mat, shorts=F)
ep2.ns$pm
ep2.ns$ps
ep2.ns$pw

# Portefeuille tangent
tp2 = portfolio.optim(ret.mat, pm = tp$er, shorts=T)
tp2$pm
tp2$ps
tp2$pw

# Frontière efficiente avec vente à découvert

results.mat = matrix(0, 20, ncol(PastXts[,1:30])+2)
colnames(results.mat) = c("er","sd",colnames(ret.mat))
targetRet.vec = seq(from=1.0001, to=1.001, length=20)
for (i in 1:20) {
  tmp.p = portfolio.optim(ret.mat, pm = targetRet.vec[i], 
                          shorts=F)
  results.mat[i,] = c(tmp.p$pm,tmp.p$ps,tmp.p$pw)
} 

# Trace la frontière efficiente
x11() ## Nouvelle Fenêtre graphique
plot(ef$sd, ef$er, type="b", col="blue", main="Efficient Frontier",
     xlab="portfolio sd", ylab="portfolio er")
points(results.mat[,"sd"], results.mat[,"er"], type="b",
       col="red")
legend(x="topleft", legend=c("Short sales", "No short sales"),
       lty=c(1,1), pch=c("o","o"), col=c("blue","red"))