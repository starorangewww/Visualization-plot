library(shiny)
library(rlang)
library(httpuv)
library(Rcpp)
library(rsconnect)
library(varhandle)
library(stats)
#library(ropls) bio
library(car)
library(ggplot2)
library(e1071)
library(neuralnet) 
library(nnet)     
library(caret)
library(randomForest)
options(shiny.maxRequestSize=30*1024^2) 

shinyServer(
  function(input,output){
    
    datasetInputRaw<-reactive({
      input$goButton
      inFile<-isolate(input$file)
      validate(need(inFile!= "", "Please select a data set"))
      if(is.null(inFile)){return("No Data")}else{
        data.plot<-read.csv(inFile$datapath)
      }
      return(list(Data.plot=data.plot))
    })
    
    datasetInput<-reactive({
      input$goButton
      y.type<-isolate(as.character(input$Ty));Cor.type<-isolate(as.character(input$Cor.1))
      y.IS<-isolate(as.numeric(input$IS));y.PD<-isolate(as.numeric(input$PD));
      y.group<-isolate(as.numeric(input$G.S));y.group.eahsize<-isolate(as.numeric(input$R.S));
      y.Cor.cut<-isolate(as.numeric(input$Cor))
      x.Cor.cut<-isolate(input$Method)
      
      #Data set
      data.plot<-datasetInputRaw()[[1]]
      #dim(data.plot)
      data.plot<-data.plot[-c(y.IS),]
      data.plot.feature<-data.plot[,c(1,2)]
      data.plot.con<-data.plot[,-c(1,2)]
      #parent drug
      data.plot.con.p<-data.plot.con[c(y.PD),]
      #--------Y-correlation-----------
      Y.axis<-sapply(1:dim(data.plot.con)[1],function(i)cor(as.numeric(data.plot.con.p),as.numeric(data.plot.con[i,])))
      Y.axis.p<-c()
      for(i in 1:dim(data.plot.con)[1]){
        x<-cor.test(as.numeric(data.plot.con.p),as.numeric(data.plot.con[i,]),method=c(Cor.type))
        Y.axis.p<-c(Y.axis.p,x$p.value)}
      Y.axis.M<-data.frame(Cor=Y.axis,Cor.p=Y.axis.p)
      #------------------------------- 
      #ANOVA F: if eual using ANOVA, vice versa using welch
      durg.c<-rep(1:y.group,each=y.group.eahsize)
      durg.c<-factor(durg.c)
      #VPPM
      if(y.type=="Univariate:Parametric"){
        X.axis.p<-c()
        L.1<-c()
        for(j in 1:dim(data.plot.con)[1]){
          my.dataframe<-data.frame(Drug.level=durg.c,Con=as.numeric(data.plot.con[j,]))
          L<-leveneTest(Con~Drug.level,my.dataframe)
          L.1<-c(L.1,L$`Pr(>F)`[1])
          if(L$`Pr(>F)`[1]>0.05){ANOVA.f<-oneway.test(Con~Drug.level,my.dataframe,var.equal=T);X.axis.p<-c(X.axis.p,ANOVA.f$p.value)}else{
            ANOVA.f<-oneway.test(Con~Drug.level,my.dataframe,var.equal=F);X.axis.p<-c(X.axis.p,ANOVA.f$p.value)
          }}
        X.axis.fdr<-p.adjust(X.axis.p,method="BH")
        X.axis.fdr.10<-(-log(X.axis.fdr,base=10))
        X.axis.p.10<-(-log(X.axis.p,base=10))
      }else{if(y.type=="Univariate:Non-parametric"){
        X.axis.kw.p<-c()
        for(j in 1:dim(data.plot.con)[1]){
          my.dataframe<-data.frame(Drug.level=durg.c,Con=as.numeric(data.plot.con[j,]))
          K<-kruskal.test(Con~Drug.level,my.dataframe)
          X.axis.kw.p<-c(X.axis.kw.p,K$p.value)}
        X.axis.p.10<-(-log(X.axis.kw.p,base=10))}else{
          data.plot.con.t<-t(data.plot.con)
          colnames(data.plot.con.t)<-paste0("f",".",data.plot.feature[,2])
          data.plot.con.t.1<-cbind(durg.c,data.plot.con.t)
          data.plot.con.t.1<-as.data.frame(data.plot.con.t.1)
          data.plot.con.t.1[,1]<-as.factor(data.plot.con.t.1[,1])
          RL<-randomForest(durg.c~.,data.plot.con.t.1)
          U<-RL$importance
          X.axis.imp<-U[,1]
        }
        
      }
      #plot 
      if(x.Cor.cut=="BH_FDR"){
      D1<-data.frame(X.P=X.axis.p.10,COR=Y.axis,X.fdr=X.axis.fdr.10)}else{
        if(y.type=="Multivairate:Importance wetight"){D1<-data.frame(X.P=X.axis.imp,COR=Y.axis)}else{
          
          D1<-data.frame(X.P=X.axis.p.10,COR=Y.axis)}}
      if(length(which(D1$X.P=="NaN"))>0){
        D1.1<-D1[-which(D1$X.P=="NaN"),]
        D1.1$ID<-ifelse(data.plot.feature[-which(D1$X.P=="NaN"),1]!="","Y","N")}else{
          D1.1<-D1
          D1.1$ID<-ifelse(data.plot.feature[,1]!="","Y","N")}
      
      return(list(Data.plot=D1.1,Y.Cut=y.Cor.cut,X.Cut=x.Cor.cut,Y.type=y.type,Feature=data.plot.feature))
    })
    #--------------------cutting plot
    datasetInput.2<-reactive({
      D1.1<-datasetInput()[[1]]
      x.C<-datasetInput()[[3]]
      if(x.C=="Null"){x.p<-0}else{
        if(x.C=="Bonferroni's adjustment"){x.p<-c(-log(0.05/dim(D1.1)[1],10))}else{if(x.C=="BH_FDR"){
          x.p<-c(-log(0.05,10))}else{
          if(x.C=="Logistic regression"){x.1<-glm(relevel(factor(ID),ref="N")~X.P+COR,data=D1.1,family=binomial(link="logit"))
          D1.1$Predict<-ifelse(x.1$fitted.values>0.5,1,0)
          x.p<-min(D1.1[which(D1.1$Predict==1),1])}else{
            if(x.C=="Support vector machine"){
              x.svm<-svm(factor(ID)~X.P+COR,data=D1.1)
              train.pred<-predict(x.svm,D1.1)
              x.p<-min(D1.1[which(train.pred=="Y"),1])}else{
                if(x.C=="Arifical neural network"){
                  bpn<-neuralnet(formula=factor(ID)~X.P+COR,data=D1.1)
                  pred<-compute(bpn,D1.1[,1:2])  
                  x.p4<-round(pred$net.result)
                  x.p4.nn<-ifelse(x.p4[,1]==1,"N","Y")
                  x.p<-min(D1.1[which(x.p4.nn=="Y"),1])
                }else{
                  RL<-randomForest(factor(ID)~X.P+COR,data=D1.1)
                  meta.pred<-predict(RL,D1.1)
                  x.p<-min(D1.1[which(meta.pred=="Y"),1])}
              }
          }
        }
      }}
      return(X.p=x.p)
    })
    
    #====================================
    #Output
    output$distPlot.1<-renderPlot({
      y.C<-datasetInput()[[2]]
      x.C<-datasetInput()[[3]]
      V<-datasetInput()[[1]]
      x.p<-datasetInput.2()[[1]]
      y.type<-isolate(as.character(input$Ty))
      if(y.C==99&x.C=="Null"){
        if(y.type!="Multivairate:Importance wetight"){
          V.1<-ggplot(V,aes(x=X.P,y=COR))+geom_point()+
            labs(x=expression(-log[10]*p.value),y="Correlation")+
            theme(axis.text=element_text(size=25),axis.title=element_text(size=20,face="bold"))
        }else{
          V.1<-ggplot(V,aes(x=X.P,y=COR))+geom_point()+
            labs(x="importance value by random forest",y="Correlation")+
            theme(axis.text=element_text(size=25),axis.title=element_text(size=20,face="bold"))}
      }else{
        if(x.C=="BH_FDR"){
          V.1<-ggplot(V,aes(x=X.fdr,y=COR))+geom_point()+
            labs(x=expression(-log[10]*FDR),y="Correlation")+
            theme(axis.text=element_text(size=25),axis.title=element_text(size=20,face="bold"))+geom_hline(aes(yintercept=y.C))+
            geom_vline(aes(xintercept=x.p))
        }else{
        V.1<-ggplot(V,aes(x=X.P,y=COR))+geom_point()+
          labs(x=expression(-log[10]*p.value),y="Correlation")+
          theme(axis.text=element_text(size=25),axis.title=element_text(size=20,face="bold"))+geom_hline(aes(yintercept=y.C))+
          geom_vline(aes(xintercept=x.p))}
      }
      return(V.1)
    })
    output$XY<-renderPrint({
      if(datasetInput.2()[[1]]==0){W<-c("Null")}else{
        W<-datasetInput.2()[[1]]}
      return(W)
    })
    
    output$table.1<-renderTable({
      y.C<-datasetInput()[[2]]
      x.C<-datasetInput()[[3]]
      x.p<-datasetInput.2()[[1]]
      V<-datasetInput()[[1]]
      V.f<-datasetInput()[[5]]
      if(y.C==99&x.C=="Null"){
        V.11<-c("NULL")}else{
          V.11<-V[which(V$COR>y.C&V$X.P>=x.p),]
          V.f.1<-V.f[which(V$COR>y.C&V$X.P>=x.p),]
          V.11<-cbind(V.f.1[,2],V.11)
        }
      return(V.11)
    })
    
  }
)