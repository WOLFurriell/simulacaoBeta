rm(list=ls())

library(maxLik)
library(MASS)
library(stats4)
library(tidyverse)
library(reshape)
library(plyr)

# Definindo os intervalos para as repetições
vmu<-c(0.05,0.25,0.50,0.75,0.95)
vphi<-c(5,15,50)

# Definindo os parâmetros
B<-100
k<-1
p.LR<-c()
p.Wd<-c()
p.Es<-c()
out<-matrix(nrow=B*75,ncol=11)

testes<-list()
for (mu in vmu){
  for (phi in vphi){
      for (enes in seq(20,100,20)){
        set.seed(1299)
          for (n in enes){
            for (i in 1:B){
              mu<-mu
              phi<-phi
              theta<-c(mu,phi)
              y<-betar(enes,mu,phi)
              emv<-maxLik(logbeta,start = c(mu=0.5,phi=15));emv
              theta.hat<-emv$estimate
              IFisher <- emv$hessian
              
              # Teste da Razão de verossimilhança ------------------------------------------
              LR <- 2*(logbeta(theta.hat)-logbeta(theta))
              p.LR[k] <- 1-pchisq(q = LR, df = 2)
              # Teste de Wald -----------------------------------------------------------
              diff <- matrix(c(theta.hat - theta),nrow=2)
              Wd <- t(diff)%*%(-IFisher)%*%diff
              p.Wd[k] <- 1-pchisq(q = Wd, df = 2)
              # Teste Escore ------------------------------------------------------------
              Es <- t(U(theta))%*%solve(-IFisher)%*%U(theta)
              p.Es[k] <- 1-pchisq(q = Es, df = 2)
              #----------------------------------------------
              out[k,1]<-k
              out[k,2]<-mu
              out[k,3]<-phi
              out[k,4]<-enes
              out[k,5]<-i
              out[k,6]<-p.LR[k]
              out[k,7]<-p.Wd[k]
              out[k,8]<-p.Es[k]
              out[k,9]<-ifelse(out[k,6]>0.05,1,0)
              out[k,10]<-ifelse(out[k,7]>0.05,1,0)
              out[k,11]<-ifelse(out[k,8]>0.05,1,0)
              
              cat(k,mu,phi,enes,i,"\n")
              k<-k+1
            }
          }
      }
  }
}

resultados<-as.data.frame(out)  
names(resultados)<-c("id","mu","phi","n","B","TRV","Wd","Es",
                     "TRV_0.05","Wd_0.05","Es_0.05")
head(resultados)      

resultado<-melt(resultados,c("id","mu","phi","n","B","TRV_0.05","Wd_0.05","Es_0.05"))
names(resultado)
resultado$TRV_0.05 <- NULL
resultado$Wd_0.05 <- NULL
resultado$Es_0.05 <- NULL
resultado<-rename(resultado,c(variable="testes",value="valor.p"))
resultado<-melt(resultado,c("id","mu","phi","n","B","testes","valor.p"))


resultado<-rename(resultado,c(value="class_p"))
head(resultado)

na.omit(resultado)
resultado%>% filter(phi==15)
resultado$phi<-resultado$phi%>%as.factor()

count(resultado$class_p, "mu")

p2<-resultado%>% filter(phi==15)%>%
  ggplot(aes(n, alpha, col = teste))+
  facet_wrap(~mu,labeller = label_parsed, nrow = 3) +
  geom_line()+geom_point()+ 
  scale_colour_brewer(palette = "Set1",name="Testes de\n Hipótese")+
  scale_y_continuous(breaks=seq(0,0.13,0.01))+
  expand_limits(y = 0.04) +
  geom_hline(yintercept = 0.05,linetype="dashed")+
  labs(x="\nTamanho da amostra", y="Taxa do Erro Tipo I\n", 
       title=expression(paste(phi, " = 15"))) +
  theme(legend.position="none") + tema
phi15_simul<-p2 + legenda
p2
