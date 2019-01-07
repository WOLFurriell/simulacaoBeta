library(shape)

x <- seq(0,20,l=10000)
y <- dnorm(x, mean=10, sd=2)
plot(x, y, type="l", ylim=c(0, 0.21), xlim=c(4,16), bty="l", axes = F,
     xlab = "", ylab = "")
axis(side=1,labels = F,lwd.ticks = 0)
axis(side=2,labels = F,lwd.ticks = 0)
mtext(text = expression(theta), side=1, at = 16, font=3,family='serif',
      cex = 1.2)
mtext(text = expression(paste("l(",theta,"|x)")), side=2,at = 0.21,las = 1,
      font = 3,family='serif',cex=1.2)
box(bty="l",lwd=1,xaxs='d')


abline(h = 0.12, lty=3)
abline(h = 0.2, lty=3)
abline(v = 10, lty=2)
abline(v = 12, lty=2)
# Teste da Razão de Verossimilhança ---------------------------------------
Arrows(x0 = 5 ,y0 = 0.13, x1 = 5, y1 = 0.19, code = 3,
       arr.width = .1,arr.length = .15)
text(x = 6.8, y = 0.21, labels = "Teste da Razão de Verossimilhança",
     font=3,family='serif', cex = 1.2)
Arrows(x0 = 6.2 ,y0 = 0.203, x1 = 5.1, y1 = 0.16, code = 2,
       arr.width = .1,arr.length = .15)

# Teste de Wald -----------------------------------------------------------
Arrows(x0 = 10.1 ,y0 = 0.05, x1 = 11.9, y1 = 0.05, code = 3,
       arr.width = .1,arr.length = .15)
text(x = 8.6, y = 0.01, labels = "Teste de Wald",font=3,family='serif',
     cex = 1.2)
Arrows(x0 = 9.3 ,y0 = 0.01, x1 = 10.6, y1 = 0.045, code = 2,
       arr.width = .1,arr.length = .15)
mtext(text = expression(hat(theta)), side = 1, at=10, padj = 0.3,
      font=3,family='serif', cex = 1.2)
mtext(text = expression(theta[0]), side = 1, at=12,padj = 0.3,
      font=3,family='serif', cex = 1.2)

# Teste Escore ------------------------------------------------------------
spl <- smooth.spline(y ~ x)
newx <- 12
pred0 <- predict(spl, x=newx, deriv=0)
pred1 <- predict(spl, x=newx, deriv=1)
yint <- pred0$y - (pred1$y*newx)
xint <- -yint/pred1$y
lines(x, yint + pred1$y*x, col="blue")

text(x = 14, y = 0.15, labels = "Teste Escore",font=3,family='serif', cex = 1.2)
Arrows(x0 = 13.3 ,y0 = 0.145, x1 = 12.2, y1 = 0.125, code = 2,
       arr.width = .1,arr.length = .15)
#Salvar 10 x 6
