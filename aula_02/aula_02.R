corridas<-500000
GG<-matrix(0, corridas, 1)
U<-lm(Dados_SAT$SAT~Dados_SAT$Renda)
eB0=U$coefficients[1]
eB1=U$coefficients[2]

dp<-summary(U)$sigma
W<-summary(U)
dpb1<-W$coefficients[4]
Tobs<-eB1/dpb1

for(i in 1:corridas){
  #Sob H0 temos B1=0
  Y<-eB0+0*Dados_SAT$Renda+rnorm(18, 0, dp)
  Reg<-lm(Y~Dados_SAT$Renda)
  W1<-summary(Reg)
  dpb1e<-W1$coefficients[4]
  GG[i,1]=Reg$coefficients[2]/dpb1e
}
LimitesA<-quantile(GG[,1], c(.025, .975))
pvalue=2*mean(GG[,1]>Tobs)
LimitesA
pvalue

# --------------------------------------------
# Nao normais -> Bootstrap nao parametrico
# Aula 02
corridas<-500000
GG<-matrix(0, corridas, 1)
U<-lm(Dados_SAT$SAT~Dados_SAT$Renda)
eB0=U$coefficients[1]
eB1=U$coefficients[2]

dp<-summary(U)$sigma

for(i in 1:corridas){
  #Sob H0 temos B1=0
  Y<-eB0+0*Dados_SAT$Renda+rnorm(18, 0, dp)
  Reg<-lm(Y~Dados_SAT$Renda)
  eB1e <- Reg$coefficients[2]
  GG[i,1]=eB1e
}
LimitesA<-quantile(GG[,1], c(.025, .975))
pvalue=2*mean(GG[,1]>eb1)
LimitesA
pvalue

#----------------------------------------
U<-lm(Dados_SAT$SAT~Dados_SAT$Renda)
Res<-U$residuals

# install.packages("nortest")
# install.packages("car")
# install.packages("lmtest")
T<-nortest::ad.test(Res)
car::qqPlot(Res)

s <- U$fitted.values
Res <- U$residuals
Res
plot(s, Res)

homo <- lmtest::bptest(U,~fitted(U)+I(fitted(U)^2)) # Teste de Wright
homo
# Breusch-Godfrey test
cor<-lmtest::bgtest(U , order=1)
cor

cor<-lmtest::bgtest(U , order=2)
cor





