a1 <- lm(Ascombe$y_1~Ascombe$x_1)
a1
summary(a1)

a2 <- lm(Ascombe$y_2~Ascombe$x_2)
a2
summary(a2)

par(mfrow=c(2,2))
plot(a1$fitted.values, a1$residuals)
plot(a1)

plot(a2$fitted.values, a2$residuals)
plot(a2)

a3 <- lm(Ascombe$y_4~Ascombe$x_4)
a3
summary(a3)
plot(a3$fitted.values, a3$residuals)
plot(a3)

View(exe)
S <- lm(exe$co~exe$Velo)
S
summary(S)
plot(S$fitted.values, S$residuals)

XX <- 1/exe$Velo
S <- lm(exe$co~XX)
summary(S)
plot(S$fitted.values, S$residuals)
plot(exe$Velo, exe$co)
plot(XX, exe$co)

wines = read_excel('/Users/rpinheir/Projects/personal/statistic/regresao_linear/aula_03/exe.xlsx', sheet = 'D')
View(wines)

w <- lm(wines$Qua~wines$Clari+wines$Aroma+wines$Corpo+wines$Sabor+wines$Afina)
summary(w)
w$residuals
