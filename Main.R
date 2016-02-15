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

#####################
#  GARCH - COPULES  #
#####################

# install.packages("xlsx")
# library(xlsx)
selectedReturns<-cbind(ReturnsXts[,1],ReturnsXts[,12],ReturnsXts[,13])
head(selectedReturns)
dat<-selectedReturns[,1:3]

#ARIMA
Time=as.Date(index(ReturnsXts),"%Y-%m-%d")
selectedReturnsAAPL=plot(Time,dat[,1],type="l",ylab="Apple, weekly log returns",ylim=range(selectedReturns[,2:4]))
selectedReturnsIBM=plot(Time,dat[,2],type="l",ylab="IBM, weekly log returns",ylim=range(selectedReturns[,2:4]))
selectedReturnsINTC=plot(Time,dat[,3],type="l",ylab="Intel Corporation, weekly log returns",ylim=range(selectedReturns[,2:4]))
# 3 figures arranged in 1 row and 3 columns
attach(mtcars)
par(mfrow=c(1,3))
selectedReturnsAAPL=plot(Time,dat[,1],type="l",ylab="Apple, weekly log returns",ylim=range(selectedReturns[,2:4]))
selectedReturnsIBM=plot(Time,dat[,2],type="l",ylab="IBM, weekly log returns",ylim=range(selectedReturns[,2:4]))
selectedReturnsINTC=plot(Time,dat[,3],type="l",ylab="Intel Corporation, weekly log returns",ylim=range(selectedReturns[,2:4]))

fit1=arima(x=dat[,1],order=c(2,0,1))
fit2=arima(x=dat[,2],order=c(1,0,1))
fit3=arima(x=dat[,3],order=c(1,0,1))
dat_arma <- cbind(residuals(fit1),residuals(fit2),residuals(fit3))
m <- apply(dat_arma, 2, mean)
v <- apply(dat_arma, 2, var)
dat_arma_std <- t((t(dat_arma)-m)/sqrt(v))

#GARCH
fit1=garchFit(formula = ~ arma(2,1)+garch(1, 1),data=dat[,1],cond.dist ="std")
fit2=garchFit(formula = ~ arma(1,1)+garch(1, 1),data=dat[,2],cond.dist ="std")
fit3=garchFit(formula = ~ arma(1,1)+garch(1, 1),data=dat[,3],cond.dist ="std")
dat_res<-cbind(residuals(fit1),residuals(fit2),residuals(fit3))
m_res <- apply(dat_res, 2, mean)
v_res <- apply(dat_res, 2, var)
dat_res_std =cbind((dat_res[,1]-m_res[1])/sqrt(v_res[1]),(dat_res[,2]-m_res[2])/sqrt(v_res[2]),(dat_res[,3]-m_res[3])/sqrt(v_res[3]))
#EMWA
ewma=EWMAvol(dat_res_std, lambda = 0.96)


#To visualize the volatility, use

emwa_series_vol=function(i=1){
  plot(Time,ewma$Sigma.t[,1],type="l",
       col="white",ylim=c(0,45))
  lines(Time,dat_arma[,i]+20,col="grey")
  j=1
  if(i==2) j=5
  if(i==3) j=9
  lines(Time,ewma$Sigma.t[,j],lwd=1.5)
}

for(i in 1:3) emwa_series_vol(i)


# 3 figures arranged in 1 row and 3 columns
attach(mtcars)
par(mfrow=c(1,3))
for(i in 1:3) emwa_series_vol(i)

#The implied correlation is here

emwa_series_cor=function(i=1,j=2){
  if((min(i,j)==1)&(max(i,j)==2)){
    a=1;     b=5;     ab=2}
  if((min(i,j)==1)&(max(i,j)==3)){
    a=1;     b=9;     ab=3}
  if((min(i,j)==2)&(max(i,j)==3)){
    a=5;     b=9;     ab=6}
  r=ewma$Sigma.t[,ab]/sqrt(ewma$Sigma.t[,a]*
                             ewma$Sigma.t[,b])
  plot(Time,r,type="l",ylim=c(0,1))
}

# 3 figures arranged in 1 row and 3 columns
attach(mtcars)
par(mfrow=c(1,3))
emwa_series_cor(1,2)
emwa_series_cor(2,3)
emwa_series_cor(3,1)

install.packages("MTS")
library(MTS)
bekk=BEKK11(dat_arma)
bekk_series_vol=function(i=1){
  plot(Time,bekk$Sigma.t[,1],type="l",
       ylab=names(dat)[i],col="white",ylim=c(0,80))
  lines(Time,dat_arma[,i]+40,col="grey")
  j=1
  if(i==2) j=5
  if(i==3) j=9
  lines(Time,bekk$Sigma.t[,j],lwd=1.5)
}

# 3 figures arranged in 1 row and 3 columns
attach(mtcars)
par(mfrow=c(1,3))
bekk_series_vol(1)
bekk_series_vol(2)
bekk_series_vol(3)


bekk_series_cor=function(i=1,j=2){
  if((min(i,j)==1)&(max(i,j)==2)){
    a=1;     b=5;     ab=2}
  if((min(i,j)==1)&(max(i,j)==3)){
    a=1;     b=9;     ab=3}
  if((min(i,j)==2)&(max(i,j)==3)){
    a=5;     b=9;     ab=6}
  r=bekk$Sigma.t[,ab]/sqrt(bekk$Sigma.t[,a]*
                             bekk$Sigma.t[,b])
  plot(Time,r,type="l",
       ylim=c(0,1))
}

attach(mtcars)
par(mfrow=c(1,3))
bekk_series_cor(1,2)
bekk_series_cor(2,3)
bekk_series_cor(1,3)

dat_rjoint12=joint(dat_res[,1],dat_res[,2])
dat_rjoint23=joint(dat_res[,2],dat_res[,3])
dat_rjoint31=joint(dat_res[,3],dat_res[,1])

hist(dat_res[,1], freq=FALSE, xlab="Brent", breaks=6, col="gray90",
     main="Histogram and density estimate")
lines(density(dat_res[,1]), lwd=2)
rug(jitter(dat_res[,1]))
lines(density(dat_res[,2]), lwd=2, type="s")
rug(jitter(dat_res[,2]))
#lines(density(dat_res[,1]), lwd=2, type="b")
#rug(jitter(dat_res[,1]))

copula_NP=function(i=1,j=2){
  uv=dat_res_std[,c(i,j)]
  n=nrow(uv)
  uv=cbind(rank(uv[,1]),rank(uv[,2]))/(n+1)
  xy=qnorm(uv)
  s=0.3
  dc=Vectorize(function(x,y) mean(dnorm(rep(qnorm(x),n),xy[,1],s)*dnorm(rep(qnorm(y),n),xy[,2],s))/
                 dnorm(qnorm(x))/dnorm(qnorm(y)))
  vx=seq(1/30,29/30,by=1/30)
  vz=outer(vx,vx,dc)
  zl=c(0,8)
  persp(vx,vx,vz,theta=20,phi=10,col="yellow",shade=TRUE,xlab=names(dat)[j],
        ylab=names(dat)[j],zlab="",ticktype="detailed",zlim=zl)
  
  library(copula)
  norm.cop <- normalCopula(0.5)
  norm.cop <- normalCopula(fitCopula(norm.cop,uv)@estimate)
  dc=function(x,y) dCopula(cbind(x,y), norm.cop)
  vz=outer(vx,vx,dc)
  persp(vx,vx,vz,theta=20,phi=10,col="yellow",shade=TRUE,xlab=names(dat)[j],
        ylab=names(dat)[j],zlab="copule Gaussienne",ticktype="detailed",zlim=zl)
  
  t.cop <- tCopula(0.5,df=3)
  t.fit <- fitCopula(t.cop,uv)@estimate
  t.cop <- tCopula(t.fit[1],df=t.fit[2])
  dc=function(x,y) dCopula(cbind(x,y), t.cop)
  vz=outer(vx,vx,dc)
  persp(vx,vx,vz,theta=20,phi=10,col="yellow",shade=TRUE,xlab=names(dat)[j],
        ylab=names(dat)[j],zlab="copule de Student",ticktype="detailed",zlim=zl)
}

copula_NP(1,2)
copula_NP(2,3)
copula_NP(3,1)

lambda=function(C){
  l=function(u) pcopula(C,cbind(u,u))/u
  u=seq(.001,.5,by=.001)
  v=Vectorize(l)(u)
  return(c(v,rev(v)))
}

graph_lambda=function(i,j){
  X=dat_res
  U=rank(X[,i])/(nrow(X)+1)
  V=rank(X[,j])/(nrow(X)+1)
  library(copula)
  normal.cop <- normalCopula(.5,dim=2)
  t.cop <- tCopula(.5,dim=2,df=3)
  gumbel.cop <- gumbelCopula(1.5,dim=2)
  fit1=fitCopula(normal.cop, cbind(U,V), method="ml")
  fit2=fitCopula(t.cop, cbind(U,V), method="ml")
  C1=normalCopula(fit1@copula@parameters,dim=2)
  C2=tCopula(fit2@copula@parameters[1],dim=2,df=trunc(fit2@copula@parameters[2]))
  
  Lemp=function(z) sum((U<=z)&(V<=z))/sum(U<=z)
  Remp=function(z) sum((U>=1-z)&(V>=1-z))/sum(U>=1-z)
  u=seq(.001,.5,by=.001)
  L=Vectorize(Lemp)(u)
  R=Vectorize(Remp)(rev(u))
  nom=paste("Indice de dépendance de queue ",names(dat)[i]," vs. ",names(dat)[j],sep="")
  plot(c(u,u+.5-u[1]),c(L,R),type="l",
       ylim=0:1,lwd=2,
       xlab="",ylab=nom)
  lines(c(u,u+.5-u[1]),lambda(C1),lty=2)
  lines(c(u,u+.5-u[1]),lambda(C2),col=gray.colors(1,.4))
}

time_varying_correl_2=function(i=1,j=2,
                               nom_arg="Pearson"){
  uv=dat_arma[,c(i,j)]
  n=nrow(uv)
  correlation=function(t){
    sub_uv=uv[t+(-26):26,]
    cor(sub_uv,method = tolower(nom_arg))[1,2]
  }
  ic_correlation=function(t){
    b=1000
    sub_uv=uv[t+(-26):26,]
    cr=rep(NA,b)
    for(sim in 1:b){
      sub_uv_b=sub_uv[sample(1:53,size=53,replace=TRUE),]+matrix(rnorm(106),53,2)/10000
      cr[sim]=cor(sub_uv_b,method = tolower(nom_arg))[1,2]}
    quantile(cr,c(.05,.95))
  }
  nom=paste("Correlation de ",nom_arg, " ",names(dat)[i]," vs. ",names(dat)[j],sep="")
  C=sapply(26:(n-26),correlation)
  IC_C=sapply(27:(n-27),ic_correlation)
  plot(Time[26:(n-26)],C,type="l",xlab="Time",ylab=nom,ylim=c(.3,1),col="white")
  polygon(c(Time[27:(n-27)],rev(Time[27:(n-27)])),c(IC_C[1,],rev(IC_C[2,])),col="light blue",border=NA)  
  lines(Time[26:(n-26)],C)  
  abline(h=cor(uv,method = tolower(nom_arg))[1,2],lty=2,col="red")
}

time_varying_correl_2(1,2)
time_varying_correl_2(1,2, "spearman")
time_varying_correl_2(1,2,"kendall")

library(MTS)
m2=dccFit(dat_res_std)
m3=dccFit(dat_res_std,type="Engle")
R2=m2$rho.t
R3=m3$rho.t

install.packages("rmgarch")
install.packages("rugarch")
library(rmgarch)
library(rugarch)
garch11.spec = ugarchspec(mean.model = list(armaOrder = c(2,1)),variance.model = list(garchOrder = c(1,1), model = "GARCH"), distribution.model = "norm")
dcc.garch11.spec = dccspec(uspec = multispec( replicate(3, garch11.spec) ), dccOrder = c(1,1),
                           distribution = "mvnorm")
dcc.fit= dccfit(dcc.garch11.spec,data = dat)
fcst=dccforecast(dcc.fit,n.ahead=200)
plot(fcst)
