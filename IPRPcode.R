#UTF-8 encoding

setwd("E:\\10分文章新训练集\\代码上传\\IPRP")####please change this path to the "IPRP" file!
load(".\\IPRPmodel.RData")
library(limma)
library(survival)
library(survivalROC)
library(survminer)
namedata=c("37642train","37642test","tcga","12417","106291","beat","fimm")# 7 test sets
pvaluesurvival=c() ###the p values in K-M curves
for (ID in namedata) {
  cox_gene_list=read.table("cox_gene_list.txt", header=T, sep="\t", check.names=F)
  cox_pair_list=read.table("cox_pair_list.txt", header=T, sep="\t", check.names=F)
  expdata=read.table(paste0(ID,"data68.txt"), header=T, sep="\t", check.names=F,row.names = 1)
  out=data.frame()
  for(i in 1:nrow(cox_gene_list))
  {
    num=which(row.names(expdata)[]==cox_gene_list[i,1])
    out=rbind(out,expdata[num,])
    
  }
  #expression data is in "out", then we are going to pair these genes
  tcgaPair=data.frame()
  rt=out
  sampleNum=ncol(rt)
  for(i in 1:(nrow(rt)-1)){
    for(j in (i+1):nrow(rt)){
      pair=ifelse(rt[i,]>rt[j,], 1, 0)
      rownames(pair)=paste0(rownames(rt)[i],"|",rownames(rt)[j])
      tcgaPair=rbind(tcgaPair, pair)
    }
  }
  tcgaOut=rbind(ID=colnames(tcgaPair), tcgaPair)
  tcgaOut=tcgaOut[which(rownames(tcgaOut)[]%in%cox_pair_list[,1]),]
  tcgaOut=t(tcgaOut)
  cli=read.table(paste0(ID,"cli.txt"), header=T, sep="\t", check.names=F) ####input survival data of these sets
  intername=intersect(cli[,1],row.names(tcgaOut))
  cli=cli[which(cli[,1]%in%intername),]
  tcgaOut=tcgaOut[intername,]
  fustat=vector(length = nrow(tcgaOut))
  futime=vector(length = nrow(tcgaOut))
  for(i in 1:nrow(tcgaOut))
  {
    
    
    num=which(cli[,1]==rownames(tcgaOut)[i])
    fustat[i]=cli[num,3]
    futime[i]=cli[num,2]
    
  }
  tcgaOut=cbind(tcgaOut,fustat,futime)
  tcgaOut[,"futime"]=as.numeric(tcgaOut[,"futime"])/365 ####we used "year" in survival time
  tcgaOut[,"fustat"]=as.numeric(tcgaOut[,"fustat"])
  tcgaOut=as.data.frame(tcgaOut)
  for(i in 1:ncol(tcgaOut))
  {
    tcgaOut[,i]=as.numeric(as.character(tcgaOut[,i]))
  }
  riskScoreTest=predict(multiCox,type="risk",newdata=tcgaOut)      #we obtained the IPRP risk sore of this test set
  medianTrainRisk=0.6844
  riskTest=as.vector(ifelse(riskScoreTest>medianTrainRisk,"high","low"))
  riskfile=cbind(id=rownames(cbind(tcgaOut,riskScoreTest,riskTest)),cbind(tcgaOut,riskScore=riskScoreTest,risk=riskTest))
  write.table(riskfile,
              file=paste0(".\\IPRP score\\",ID,"riskTest.txt"),
              sep="\t",
              quote=F,
              row.names=F)#save the IPRP risk score of this test set in "IPRP score" file.
  
  

  #to draw K-M curves using below codes
  rt=riskfile
  diff=survdiff(Surv(futime, fustat) ~risk,data = rt)
  pValue=1-pchisq(diff$chisq,df=1)
  pValue=signif(pValue,4)
  pValue=format(pValue, scientific = TRUE)
  fit <- survfit(Surv(futime, fustat) ~ risk, data = rt)
  
  
  
  
  pdf(file=paste0(".\\survival\\",ID,"survivalTest.pdf"),width=5.5,height=5)
  plot(fit, 
       lwd=2,
       col=c("red","blue"),
       xlab="Time (year)",
       ylab="Survival rate",
       main=paste("Survival curve (p=", pValue ,")",sep=""),
       mark.time=T)
  legend("topright", 
         c("high risk", "low risk"),
         lwd=2,
         col=c("red","blue"))
  dev.off()
  
  
  pvaluesurvival=c(pvaluesurvival,pValue)
  
  
  #to draw ROC curves using below codes
  rocCol=rainbow(3)
  aucText=c()
  
  pdf(file=paste0(".\\ROC\\",ID,"multiROC.pdf"),width=6,height=6)
  
  par(oma=c(0.5,1,0,1),font.lab=1.5,font.axis=1.5)
  
  roc=survivalROC(Stime=rt$futime, status=rt$fustat, marker = rt$riskScore, predict.time =1, method="KM")
  plot(roc$FP, roc$TP, type="l", xlim=c(0,1), ylim=c(0,1),col=rocCol[1], 
       xlab="False positive rate", ylab="True positive rate",
       main=paste0("IPRP score in", ID),
       lwd = 2, cex.main=1.3, cex.lab=1.2, cex.axis=1.2, font=1.2)
  aucText=c(aucText,paste0("1st year"," (AUC=",sprintf("%.3f",roc$AUC),")"))
  
  
  roc=survivalROC(Stime=rt$futime, status=rt$fustat, marker = rt$riskScore, predict.time =2, method="KM")
  lines(roc$FP, roc$TP, type="l", xlim=c(0,1), ylim=c(0,1),col=rocCol[2],
        lwd = 2)
  aucText=c(aucText,paste0("2nd year"," (AUC=",sprintf("%.3f",roc$AUC),")"))
  
  
  roc=survivalROC(Stime=rt$futime, status=rt$fustat, marker = rt$riskScore, predict.time =3, method="KM")
  lines(roc$FP, roc$TP, type="l", xlim=c(0,1), ylim=c(0,1),col=rocCol[3],
        lwd = 2)
  aucText=c(aucText,paste0("3rd year"," (AUC=",sprintf("%.3f",roc$AUC),")"))
  
  
  
  abline(0,1)
  
  
  legend("bottomright", aucText,lwd=2,bty="n",col=rocCol)
  dev.off()
  print(ID)

}
outdata=cbind(namedata,pvaluesurvival)
fix(outdata)

