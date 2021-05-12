library(shiny)
library(splines)
library(ggplot2)


shinyServer(function(input, output) {

  
  output$plocik<-renderPlot({
    wezel<- 10/(input$wezly+1)*(1:input$wezly)
    prawda<-data.frame(a=seq(0,10,by=0.01),b=seq(0,10,by=0.01)*sin(seq(0,10,by=0.01)))
    set.seed(33)
    x<-runif(40,0,10)
    gr<-c()
    for(i in 1:40){
      gr<-c(gr,sum(x[i]>wezel))
    }
    dane<-data.frame(x=x,y=x*sin(x)+rnorm(40,0,1),gr=gr)
    dane1<-data.frame(x=x,y=predict(smooth.spline(x,y=dane$y,df=input$df),x)$y)
    set.seed(45)
    test<-data.frame(x1=(x1=runif(10,0,10)),y1=x1*sin(x1)+rnorm(10,0,1))
    if(input$model==1){
      ggplot(dane,aes(x=x,y=y))+geom_point(cex=4)+stat_smooth(method = "lm",se=F,lwd=2)+
        geom_line(data=prawda,aes(group=1,x=a,y=b),lwd=2)+
        theme_light()+geom_point(data=test,aes(x=x1,y=y1),cex=4,color="red")
    }
    else if(input$model==2){
      ggplot(dane,aes(x=x,y=y))+geom_point(cex=4)+
        geom_line(data=prawda,aes(group=1,x=a,y=b),lwd=2)+
        theme_light()+geom_point(data=test,aes(x=x1,y=y1),cex=4,color="red")+
        stat_smooth(col="darkgreen",se=F,lwd=2,span = input$span)
    }
    else if(input$model==3){
      if(input$stopien==0){
        formu<-as.formula("y~1")
      }
      else{
        formu<-as.formula("y~bs(x,degree=input$stopien,knots=wezel)")
      }
      ggplot(dane,aes(x=x,y=y))+geom_point(cex=4)+
        geom_line(data=prawda,aes(group=1,x=a,y=b),lwd=2)+
        theme_light()+geom_point(data=test,aes(x=x1,y=y1),cex=4,color="red")+
        stat_smooth(method = "lm",formula = formu,col="red",lwd=2,se=F)+geom_vline(xintercept = wezel,lty=2,lwd=2)
    }
    else if(input$model==4){
      if(input$stopien==0){
        formu<-as.formula("y~1")
      }
      else{
        formu<-as.formula("y~poly(x,input$stopien)")
      }
      ggplot(dane,aes(x=x,y=y,group=gr))+geom_point(cex=4)+
        geom_line(data=prawda,aes(group=1,x=a,y=b),lwd=2)+
        theme_light()+geom_point(data=test,aes(x=x1,y=y1,group=2),cex=4,color="red")+
        stat_smooth(method = "lm", formula = formu, col="purple",se=F,lwd=2)+geom_vline(xintercept = wezel,lty=2,lwd=2)
    }
    else if(input$model==5){
      if(input$stopien==0){
        formu<-as.formula("y~1")
      }
      else{
        formu<-as.formula("y~ns(x,Boundary.knots=wezel[c(1,length(wezel))],knots=wezel[-c(1,length(wezel))])")
      }
      ggplot(dane,aes(x=x,y=y))+geom_point(cex=4)+
        geom_line(data=prawda,aes(group=1,x=a,y=b),lwd=2)+
        theme_light()+geom_point(data=test,aes(x=x1,y=y1),cex=4,color="red")+
        stat_smooth(method = "lm",formula = formu,col="brown",lwd=2,se=F)+geom_vline(xintercept = wezel,lty=2,lwd=2)
    }
    else if(input$model==6){
      
      ggplot(dane,aes(x=x,y=y))+geom_point(cex=4)+
        geom_line(data=prawda,aes(group=1,x=a,y=b),lwd=2)+
        theme_light()+geom_point(data=test,aes(x=x1,y=y1),cex=4,color="red")+
        geom_line(data=dane1,aes(x=x,y=y,group=1),col="purple",lwd=2)+geom_vline(xintercept = wezel,lty=2,lwd=2)
    }
    
  },width=900,height=700)
  
  output$bazy<-renderPlot({
    wezel<- 10/(input$wezly+1)*(1:input$wezly)
    a<-seq(0,10,by=0.01)
    if(input$model==1){
      bazowe<-data.frame(x=c(a,a),y=c(rep(1,length(a)),a),gr=c(rep(1,length(a)),rep(2,length(a))))
      ggplot(bazowe,aes(x=x,y=y,group=gr,color=factor(gr)))+geom_line(lwd=2)+theme_light()+
        guides(colour=FALSE)
    }
    else if(input$model==3){
      x<-rep(a,(input$stopien+1+input$wezly))
      y<-c()
      gr<-c()
           for(i in 0:input$stopien){
             y<-c(y,a^i)
             gr<-c(gr,rep(i,length(a)))
           }
           for(j in 1:input$wezly){
             y<-c(y,sapply(a,function(x){max(0,x-wezel[j])^input$stopien},simplify=T))
             gr<-c(gr,rep(input$stopien+1+j,length(a)))
           }
      
      bazowe<-data.frame(x=x,y=y, gr=gr)
      ggplot(bazowe,aes(x=x,y=y,group=gr,color=factor(gr)))+geom_line(lwd=2)+theme_light()+
        guides(colour=FALSE)+coord_cartesian(ylim = c(0,10))+geom_vline(xintercept = wezel,lty=2,lwd=2)
    }
    else if(input$model==4){
      x<-rep(a,(input$stopien+1))
      y<-c()
      gr1<-c()
      gr<-c()
      for(i in 1:length(a)){
        gr1<-c(gr1,sum(a[i]>wezel))
      }
      for(i in 0:input$stopien){
        y<-c(y,a^i)
        gr<-c(gr,gr1+i*100)
      }
      bazowe<-data.frame(x=x,y=y, gr=gr)
      ggplot(bazowe,aes(x=x,y=y,group=gr,color=factor(gr)))+geom_line(lwd=2)+theme_light()+
        guides(colour=FALSE)+coord_cartesian(ylim = c(0,10))+geom_vline(xintercept = wezel,lty=2,lwd=2)
    }
    else if(input$model==5){
      x<-rep(a,input$wezly)
      y<-c(rep(1,length(a)),a)
      gr<-c(rep(1,length(a)),rep(2,length(a)))
      dk1<-(sapply(a,function(x){max(0,x-wezel[length(wezel)-1])^3},simplify=T)-sapply(a,function(x){max(0,x-wezel[length(wezel)])^3},simplify=T))/(wezel[length(wezel)]-wezel[length(wezel)-1])
      for(i in 1:(input$wezly-2)){
        y<-c(y, 
             (sapply(a,function(x){max(0,x-wezel[i])^3},simplify=T)-sapply(a,function(x){max(0,x-wezel[length(wezel)])^3},simplify=T))/(wezel[length(wezel)]-wezel[i])-dk1 )
        gr<-c(gr,rep(i+2,length(a)))
      }
      bazowe<-data.frame(x=x,y=y, gr=gr)
      ggplot(bazowe,aes(x=x,y=y,group=gr,color=factor(gr)))+geom_line(lwd=2)+theme_light()+
        guides(colour=FALSE)+coord_cartesian(ylim = c(0,50))+geom_vline(xintercept = wezel,lty=2,lwd=2)
    }
  },width=900,height=700)
  
})