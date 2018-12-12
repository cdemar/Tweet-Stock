# Programers: Cory De Mar & Alexandria Allarde
# There is a 44 day data set.
# Trumps tweets are on # 22 & 24
library(quantmod)

# The start and end days we are looking at
start <- as.Date('2018-08-05')
end <- as.Date('2018-10-07')

# Nike
getSymbols("NKE",src="yahoo",from=start,to=end)
NKE.a <- adjustOHLC(NKE)
# uses the adjusted column in Yahoo for Open to Close prices
NKE.uA <- adjustOHLC(NKE, use.Adjusted=TRUE)
# converts Open to Close data to decimal
NKE.decimal <- cbind(OpCl(NKE),OpCl(NKE.a),OpCl(NKE.uA))

# Adidas
getSymbols("ADDYY",src="yahoo",from=start,to=end)
ADDYY.a <- adjustOHLC(ADDYY)
ADDYY.uA <- adjustOHLC(ADDYY, use.Adjusted=TRUE)
ADDYY.decimal <- cbind(OpCl(ADDYY),OpCl(ADDYY.a),OpCl(ADDYY.uA))

# Puma
getSymbols("PMMAF",src="yahoo",from=start,to=end)
PMMAF.a <- adjustOHLC(PMMAF)
PMMAF.uA <- adjustOHLC(PMMAF,use.Adjusted=TRUE)
PMMAF.decimal <- cbind(OpCl(PMMAF),OpCl(PMMAF.a),OpCl(PMMAF.uA))

# Under Armour
getSymbols("UAA",src="yahoo",from=start,to=end)
UAA.a <- adjustOHLC(UAA)
UAA.uA <- adjustOHLC(UAA,use.Adjusted=TRUE)
UAA.decimal <- cbind(OpCl(UAA),OpCl(UAA.a),OpCl(UAA.uA))

# Skechers
getSymbols("SKX",src="yahoo",from=start,to=end)
SKX.a <- adjustOHLC(SKX)
SKX.uA <- adjustOHLC(SKX,use.Adjusted=TRUE)
SKX.decimal <- cbind(OpCl(SKX),OpCl(SKX.a),OpCl(SKX.uA))

# Li-Ning
getSymbols("LNNGF",src="yahoo",from=start,to=end)
LNNGF.a <- adjustOHLC(LNNGF)
LNNGF.uA <- adjustOHLC(LNNGF,use.Adjusted=TRUE)
LNNGF.decimal <- cbind(OpCl(LNNGF),OpCl(LNNGF.a),OpCl(LNNGF.uA))

# Looking at non-normalized data in decimal form
plot(NKE.decimal,ylim=range(-.09,.07),type='l',
     main="2 Months: All Companies",ylab="Decimal",xlab="Days")
lines(ADDYY.decimal,col='red') 
lines(PMMAF.decimal,col='green')
lines(UAA.decimal,col='blue')
lines(SKX.decimal,col='purple')
lines(LNNGF.decimal,col='orange')
#legend(2,.05,legend=c("Nike","Adidas","Puma","Under Armour",
#              "Skechers","Li-Ning"), col=c('black','red','green','blue',
#              'purple','orange'),lty=1:2,cex=0.8)


# This is looking at a Aug. 22 to Sep. 20

# This is to create a moving window.
NKEClose <- function(x) {mean(NKE.decimal$OpCl.NKE[x-5:x+5])}
ADDYYClose <- function(x) {mean(ADDYY.decimal$OpCl.ADDYY[x-5:x+5])}
PMMAFClose <- function(x) {mean(PMMAF.decimal$OpCl.PMMAF[x-5:x+5])}
UAAClose <- function(x) {mean(UAA.decimal$OpCl.UAA[x-5:x+5])}
SKXClose <- function(x) {mean(SKX.decimal$OpCl.SKX[x-5:x+5])}
LNNGFClose <- function(x) {mean(LNNGF.decimal$OpCl.LNNGF[x-5:x+5])}

# This is averaging the decimal of all componies into one portfolio.
ALLClose <- function(x) { mean <- mean(ADDYY.decimal$OpCl.ADDYY[x-5:x+5]+
          PMMAF.decimal$OpCl.PMMAF[x-5:x+5]+UAA.decimal$OpCl.UAA[x-5:x+5]+
          SKX.decimal$OpCl.SKX[x-5:x+5]+LNNGF.decimal$OpCl.LNNGF[x-5:x+5])
          avg <- mean/5
          avg }

# Ploting the moving window of all componies
plot(13:33,lapply(13:33,NKEClose),ylim=range(-.004,.005),type='l',
     main="21 Days of Data: All Companies",ylab="Decimal",xlab="Days")
lines(13:33,lapply(13:33,ADDYYClose),col='red')
lines(13:33,lapply(13:33,PMMAFClose),col='green')
lines(13:33,lapply(13:33,UAAClose),col='blue')
lines(13:33,lapply(13:33,SKXClose),col='purple')
lines(13:33,lapply(13:33,LNNGFClose),col='orange')
legend(25,.005,legend=c("Nike","Adidas","Puma","Under Armour","Skechers","Li-Ning")
       ,col=c('black','red','green','blue','purple','orange'),lty=1:2,cex=0.8)

# Turning into a list so that we can use it for aggregate
mac <- mean(sapply(13:33,ALLClose))
mnk <- mean(sapply(13:33,NKEClose))
sf <- mac/mnk

# Plotting the moving window of Nike and the Portfolio
plot(13:33,lapply(13:33,ALLClose),ylim=range(-.004,.004),type='l',
     main="21 Days of Data: Nike Vs. Portfolio",ylab="Decimal",xlab="Days")
lines(13:33,lapply(13:33,function(x) {NKEClose(x)*sf}),col='blue',type='b')
legend(25,.004,legend=c("Portfolio","Nike"),col=c('black','blue'),lty=1:2,cex=0.8)


# This is looking at a Aug. 31 to Sep. 11
NKEClose2 <- function(x) {mean(NKE.decimal$OpCl.NKE[x-5:x+5])}
ADDYYClose2 <- function(x) {mean(ADDYY.decimal$OpCl.ADDYY[x-5:x+5])}
PMMAFClose2 <- function(x) {mean(PMMAF.decimal$OpCl.PMMAF[x-5:x+5])}
UAAClose2 <- function(x) {mean(UAA.decimal$OpCl.UAA[x-5:x+5])}
SKXClose2 <- function(x) {mean(SKX.decimal$OpCl.SKX[x-5:x+5])}
LNNGFClose2 <- function(x) {mean(LNNGF.decimal$OpCl.LNNGF[x-5:x+5])}

ALLClose2 <- function(x) { mean <- mean(ADDYY.decimal$OpCl.ADDYY[x-5:x+5]+
          PMMAF.decimal$OpCl.PMMAF[x-5:x+5]+UAA.decimal$OpCl.UAA[x-5:x+5]+
          SKX.decimal$OpCl.SKX[x-5:x+5]+LNNGF.decimal$OpCl.LNNGF[x-5:x+5])
          avg <- mean/5
          avg }

plot(20:26,lapply(20:26,NKEClose2),ylim=range(-.003,.003),type='l',
     main="7 Days of Data: All Companies",ylab="Decimal",xlab="Days")
lines(20:26,lapply(20:26,ADDYYClose2),col='red')
lines(20:26,lapply(20:26,PMMAFClose2),col='green')
lines(20:26,lapply(20:26,UAAClose2),col='blue')
lines(20:26,lapply(20:26,SKXClose2),col='purple')
lines(20:26,lapply(20:26,LNNGFClose2),col='orange')
legend(24,.003,legend=c("Nike","Adidas","Puma","Under Armour","Skechers","Li-Ning")
       ,col=c('black','red','green','blue','purple','orange'),lty=1:2,cex=0.8)

mac <- mean(sapply(20:26,ALLClose2))
mnk <- mean(sapply(20:26,NKEClose2))
sf <- mac/mnk

plot(20:26,lapply(20:26,ALLClose2),ylim=range(-.002,.002),type='l',
     main="7 Days of Data: Nike Vs. Portfolio",ylab="Decimal",xlab="Days")
lines(20:26,lapply(20:26,function(x) {NKEClose2(x)*sf}),col='blue',type='b')
legend(24,.002,legend=c("Portfolio","Nike"),col=c('black','blue'),lty=1:2,cex=0.8)
