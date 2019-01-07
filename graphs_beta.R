library(gcookbook)
library(scales)
library(ggplot2)
library(dplyr)
##------------------------------------------------------------------------
cbbPalette<-c("#006400","#2E5894","#800020","#2E5894","#006400")
teste4<-ggplot(dados,aes(x=.,colour=mu)) +
  geom_density(size=1.2,alpha=.4) +
  scale_colour_manual(values=cbbPalette,name=NULL,
                        guide=guide_legend(expression(paste(mu)))) +
  ggtitle(expression(paste(phi, " = 50")))

##------------------------------------------------------------------------
dir<-"D:/BETA_TRABALHO/Banco/teste4.jpeg"
ggsave(dir, plot = teste4, width = 8, height = 6, dpi = 300)
##------------------------------------------------------------------------
##------------------------------------------------------------------------
phi50<-ggplot(dados,aes(x=.,group=mu)) +
  geom_density(size=1.2) +    
  scale_fill_manual(name=NULL) +
  ggtitle(expression(paste(phi, " = 50"))) +
  ylab("") +
  xlab("y") +  
  theme(axis.text.x = element_text(hjust = 1,color="black",size=15),
        axis.text.y = element_text(hjust = 1,color="black",size=15),
        axis.title.x = element_text(size = rel(1.6)),
        plot.title = element_text(hjust = 0.5,size = 18,face="bold")) +
  annotate("text",x =0.5,y=6.6,label=paste("µ","=0.5",""),colour="black",size=6) +
  annotate("text",x =0.25,y=7.8,label=paste("µ","=0.25",""),colour="black",size=6) +
  annotate("text",x =0.75,y=7.8,label=paste("µ","=0.75",""),colour="black",size=6) +
  annotate("text",x =0.95,y=16,label=paste("µ","=0.95",""),colour="black",size=6) +
  annotate("text",x =0.05,y=16,label=paste("µ","=0.05",""),colour="black",size=6) 
phi50
##------------------------------------------------------------------------
dir<-"D:/BETA_TRABALHO/Graphs/phi50.pdf"
ggsave(dir, plot = phi50, width = 8, height = 8, dpi = 300)
##------------------------------------------------------------------------
##------------------------------------------------------------------------
phi15<-ggplot(dados2,aes(x=.,group=mu)) +
  geom_density(size=1.2) +
  scale_fill_manual(name=NULL) +
  ylab("") +
  xlab("y") +
  ggtitle(expression(paste(phi, " = 15"))) +
  theme(axis.text.x = element_text(hjust = 1,color="black",size=15),
        axis.text.y = element_text(hjust = 1,color="black",size=15),
        axis.title.x = element_text(size = rel(1.6)),
        plot.title = element_text(hjust = 0.5,size = 18,face="bold")) +
  annotate("text",x =0.5,y=4.3,label=paste("µ","=0.5",""),colour="black",size=6) +
  annotate("text",x =0.25,y=5.2,label=paste("µ","=0.25",""),colour="black",size=6) +
  annotate("text",x =0.75,y=5.2,label=paste("µ","=0.75",""),colour="black",size=6) +
  annotate("text",x =0.90,y=15.8,label=paste("µ","=0.95",""),colour="black",size=6) +
  annotate("text",x =0.10,y=15.8,label=paste("µ","=0.05",""),colour="black",size=6) 
phi15
##------------------------------------------------------------------------
dir<-"D:/BETA_TRABALHO/Graphs/phi15.pdf"
ggsave(dir, plot = phi15, width = 8, height = 8, dpi = 300)
##------------------------------------------------------------------------
##------------------------------------------------------------------------
phi5<-ggplot(dados3,aes(x=.,group=mu)) +
  geom_density(size=1.2) +
  scale_fill_manual(name=NULL) +
  ylab("Densidade") +
  xlab("y") +
  ggtitle(expression(paste(phi, " = 5"))) +
  theme(axis.text.x = element_text(hjust = 1,color="black",size=15),
        axis.title.y = element_text(size = rel(1.8)),
        axis.title.x = element_text(size = rel(1.6)),
        axis.text.y = element_text(hjust = 1,color="black",size=15),
        plot.title = element_text(hjust = 0.5,size = 18,face="bold")) +
  annotate("text",x =0.5,y=4.3,label=paste("µ","=0.5",""),colour="black",size=6) +
  annotate("text",x =0.25,y=5.2,label=paste("µ","=0.25",""),colour="black",size=6) +
  annotate("text",x =0.75,y=5.2,label=paste("µ","=0.75",""),colour="black",size=6) +
  annotate("text",x =0.90,y=15.8,label=paste("µ","=0.95",""),colour="black",size=6) +
  annotate("text",x =0.10,y=15.8,label=paste("µ","=0.05",""),colour="black",size=6) 
phi5
##------------------------------------------------------------------------
dir<-"D:/BETA_TRABALHO/Graphs/phi5.pdf"
ggsave(dir, plot = phi5, width = 8, height = 8, dpi = 300)
##------------------------------------------------------------------------

##------------------------------------------------------------------------
allbeta<-grid.arrange(phi15,phi50,nrow=2)
dir<-"D:/BETA_TRABALHO/Banco/graph_beta.jpeg"
ggsave(dir, plot = allbeta, width = 6, height = 8, dpi = 300)

teste5<-ggplot(dados,aes(x=.,group=mu)) +
  geom_density(size=1.2) +
  scale_fill_manual(name=NULL) + xlab(expression(paste(mu))) +
  ggtitle(expression(paste(phi, " = 50")))
teste5

dir<-"D:/BETA_TRABALHO/Banco/teste5.jpeg"
ggsave(dir, plot = teste5, width = 8, height = 6, dpi = 300)

