
# Valores pseudo-aleatórios da beta ---------------------------------------
betar<-function(n,mu,phi){
  alpha<-mu*phi
  beta<-(1-mu)*phi
  rbeta(n, alpha, beta)}

# LogMV ------------------------------------------------------------------
logbeta<-function(theta){
  mu<-theta[1]
  phi<-theta[2]
  loggamma1<-(phi-0.5)*log(phi)-phi+0.5*log(2*pi) 
  loggamma2<-((mu*phi)-0.5)*log((mu*phi))-(mu*phi)+0.5*log(2*pi)
  loggamma3<-((phi-mu*phi)-0.5)*log((phi-mu*phi))-(phi-mu*phi)+0.5*log(2*pi)
  mle<-n*loggamma1-n*loggamma2-n*loggamma3+
    (mu*phi-1)*sum(log(y))+(phi-mu*phi-1)*sum(log(1-y))
  return(mle)}

# Função score -------------------------------------------------------------
U<-function(theta){
  mu<-theta[1]
  phi<-theta[2]
  Ad<-digamma(mu*phi)
  Bd<-digamma(phi-phi*mu)
  Es<-c()
  Es[1]<-n*phi*Bd-n*phi*Ad + phi*sum(log(y))-phi*sum(log(1-y));
  Es[2]<-n*digamma(phi)+(n*(mu-1))*Bd-n*mu*Ad+mu*sum(log(y))+(1-mu)*sum(log(1-y))
  return(Es)}
