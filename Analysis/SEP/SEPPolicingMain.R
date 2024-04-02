#==================================================================================================
# Title:                 Policing Main (SEP)
# Date:                  14 March 2024
# Weather Scenario:      - Deterministic 20 Minutes
# BLACK Model:           - Base case for cities: AR1
#                        - Base case for states: CT2
# CITATIONS Model:       - Base case for cities: CA5
#                        - Base case for states: CT2
#==================================================================================================
rm(list=ls())
library(ggplot2)
library(margins)
library(marginaleffects)
library(openxlsx)
library(stargazer)
root                     = "D:/Research/Live Manuscripts/Policing"
setwd(paste(root,"/Analysis/SEP",sep=""))
load(paste(root,"/Analysis/DATA/ALLPolicingRegData.RData",sep=""))
glmblack                 = dget("FunGLMBlack.R")
glmcitation              = dget("FunGLMCitation.R")
dfcity$ctime             = floor(dfcity$clocktime)
dfstate$ctime            = floor(dfstate$clocktime)
dfcity                   = dfcity[c("year","citation","black","white","hispanic","jcode",
                                    "night","tempk","ctime","det","weekday","months")]
dfstate                  = dfstate[c("year","citation","black","white","hispanic","jcode",
                                     "night","tempk","ctime","det","weekday","months")]
jurisdictions            = readWorkbook(paste(root,"/Analysis/Data/Policing.xlsx",sep=""),
                                        sheet="JURISDICTIONS")
#==================================================================================================
# BLACK: City and State Logit Model
#==================================================================================================
meblack                  = list()
dfcity1                  = subset(dfcity,hispanic==0)
dfstate1                 = subset(dfstate,hispanic==0)
df                       = rbind(dfcity1,dfstate1)
rm(dfcity1,dfstate1)
#--------------------------------------------------------------------------------------------------
for(i in unique(df$jcode)){
     dftemp              = subset(df,jcode==i)
     dftemp$ctime        = factor(dftemp$ctime)
     dftemp$months       = factor(dftemp$months)
     dftemp$weekday      = factor(dftemp$weekday)
     maxyear             = ifelse(length(unique(dftemp$year))>1,max(dftemp$year)-1,
                                  max(dftemp$year))
     if(length(unique(dftemp$year))>1){
          dftemp$year    = factor(dftemp$year)}
     b1                  = glmblack("det",dftemp)
     b2                  = glmblack("det+night:det",dftemp)
     b3                  = glmblack("det+tempk",dftemp)
     b4                  = glmblack("tempk",dftemp)
     b5                  = glmblack("det+tempk+det:tempk",dftemp)
     b6                  = glmblack("det+tempk+det:tempk+night:det",dftemp)
     stargazer(b1,b2,b3,b4,b5,b6,type="latex",no.space=TRUE,
               omit=c("ctime","weekday","months","year"),out=paste("black",i,".tex",sep=""))
     newdf               = unique(dftemp[c("year","weekday","months","night","ctime")])
     newdf               = subset(newdf,weekday=="Wednesday" & months=="June" &
                                        ctime==12 & year==maxyear)
     if(i=="NY1"){
          newdf          = unique(dftemp[c("year","weekday","months","night","ctime")])
          newdf          = subset(newdf,weekday=="Wednesday" & months=="June" &
                                            ctime==11 & year==maxyear)}
     tempk               = quantile(dftemp$tempk,probs=c(0.25,0.5,0.75))
     tempk               = unname(tempk)
     tempk               = data.frame(tempk=tempk,templabels=c("Q1","Q2","Q3"))
     det                 = dftemp$det[which(dftemp$det>0.1)]
     det                 = det[!det %in% boxplot.stats(det)$out]
     det                 = summary(det)
     det                 = unname(det)
     det                 = as.numeric(det[c(2,3,5,6)])
     det                 = data.frame(det=det,detlabels=c("Q1","Q2","Q3","Max"))
     newdf1              = merge(det,tempk)
     newdf               = merge(newdf,newdf1)
     fitted              = predict(b6,newdata=newdf,type="response",se.fit=TRUE)
     newdf$fitted        = fitted$fit
     newdf$se            = fitted$se.fit
     dfggplot            = subset(newdf,ctime==12 & months=="June")
     if(i=="NY1"){
          dfggplot            = subset(newdf,ctime==11 & months=="June")}
     me                  = slopes(b6,newdata=newdf,variables=c("det","tempk"))
     me                  = me[c("term","estimate","p.value","conf.low","conf.high","year",
                                "weekday","months","night","ctime","det","detlabels","tempk",
                                "templabels")]
     me$jcode            = i
     jname               = jurisdictions$jurisdiction[which(jurisdictions$jcode==i)]
     me$jurisdiction     = jname
     meblack[[i]]        = me
     ggplot(dfggplot,aes(x=factor(detlabels,c("Q1","Q2","Q3","Max")),color=templabels))+
          geom_point(aes(y=fitted),position=position_dodge(width=0.25))+
          geom_errorbar(aes(ymin=fitted-1.96*se,ymax=fitted+1.96*se),width=.01,
                        position=position_dodge(width=0.25))+
          facet_wrap(vars(night))+theme_bw()+xlab("Precipitation in mm per 20 Minutes")+
          ylab("Predicted Probability (Black=1)")+ylim(0,1)+ggtitle(jname)+
          theme(legend.position="bottom")+labs(color="Temperature Quartile:") 
     ggsave(paste(root,"/Supplemental Information/black",i,".pdf",sep=""),width=6.5,height=4)}
#--------------------------------------------------------------------------------------------------
meblack                  = do.call("rbind",meblack)
#==================================================================================================
# CITATION: City and State Logit Model
#==================================================================================================
mecitation               = list()
dfcity$nonwhite          = dfcity$black+dfcity$hispanic
dfstate$nonwhite         = dfstate$black+dfstate$hispanic
citylist                 = c("CA5","CA8","CT1","KY1","LA1","MN1","NC1","NC2","NC3","NC4","NC5",
                             "NC7","ND1","OH2","WI1")
statelist                = c("CT2","MI1","NH1","TX8","WI2")
dfcity                   = subset(dfcity,jcode %in% citylist)
dfstate                  = subset(dfstate,jcode %in% statelist)
df                       = rbind(dfcity,dfstate)
#--------------------------------------------------------------------------------------------------
for(i in unique(df$jcode)){
     dftemp              = subset(df,jcode==i)
     dftemp$ctime        = factor(dftemp$ctime)
     dftemp$months       = factor(dftemp$months)
     dftemp$weekday      = factor(dftemp$weekday)
     maxyear             = ifelse(length(unique(dftemp$year))>1,max(dftemp$year)-1,
                                  max(dftemp$year))
     if(length(unique(dftemp$year))>1){
          dftemp$year    = factor(dftemp$year)}
     c1                  = glmcitation("det",dftemp)
     c2                  = glmcitation("det+night:det",dftemp)
     c3                  = glmcitation("det+tempk",dftemp)
     c4                  = glmcitation("tempk",dftemp)
     c5                  = glmcitation("det+tempk+det:tempk",dftemp)
     c6                  = glmcitation("det+tempk+det:tempk+night:det",dftemp)
     stargazer(c1,c2,c3,c4,c5,c6,type="latex",no.space=TRUE,
               omit=c("ctime","weekday","months","year"),out=paste("citation",i,".tex",sep=""))
     newdf               = unique(dftemp[c("year","weekday","months","night","ctime","nonwhite")])
     newdf               = subset(newdf,weekday=="Wednesday" & months=="June" &
                                       ctime==12 & year==maxyear)
     tempk               = quantile(dftemp$tempk,probs=c(0.25,0.5,0.75))
     tempk               = unname(tempk)
     tempk               = data.frame(tempk=tempk,templabels=c("Q1","Q2","Q3"))
     det                 = dftemp$det[which(dftemp$det>0.1)]
     det                 = det[!det %in% boxplot.stats(det)$out]
     det                 = summary(det)
     det                 = unname(det)
     det                 = as.numeric(det[c(2,3,5,6)])
     det                 = data.frame(det=det,detlabels=c("Q1","Q2","Q3","Max"))
     newdf1              = merge(det,tempk)
     newdf               = merge(newdf,newdf1)
     fitted              = predict(c6,newdata=newdf,type="response",se.fit=TRUE)
     newdf$fitted        = fitted$fit
     newdf$se            = fitted$se.fit
     dfggplot            = subset(newdf,ctime==12 & months=="June")
     dfggplot$night      = ifelse(dfggplot$night==1,"Night","Day")
     dfggplot$nonwhite   = ifelse(dfggplot$nonwhite==1,"Nonwhite","White")
     me                  = slopes(c6,newdata=newdf,variables=c("det","tempk"))
     me                  = me[c("term","estimate","p.value","conf.low","conf.high","year",
                                "weekday","months","night","ctime","det","detlabels","tempk",
                                "templabels","nonwhite")]
     me$jcode            = i
     jname               = jurisdictions$jurisdiction[which(jurisdictions$jcode==i)]
     me$jurisdiction     = jname
     mecitation[[i]]     = me
     ggplot(dfggplot,aes(x=factor(detlabels,c("Q1","Q2","Q3","Max")),color=templabels))+
          geom_point(aes(y=fitted),position=position_dodge(width=0.25))+
          geom_errorbar(aes(ymin=fitted-1.96*se,ymax=fitted+1.96*se),width=.01,
                        position=position_dodge(width=0.25))+
          facet_grid(vars(night),vars(nonwhite))+theme_bw()+xlab("Precipitation in mm per 20 Minutes")+
          ylab("Predicted Probability (Citation=1)")+ylim(0,1)+ggtitle(jname)+
          theme(legend.position="bottom")+labs(color="Temperature Quartile:") 
     ggsave(paste(root,"/Supplemental Information/citation",i,".pdf",sep=""),width=6.5,height=4)}
#--------------------------------------------------------------------------------------------------
mecitation               = do.call("rbind",mecitation)
#--------------------------------------------------------------------------------------------------
rm(list=setdiff(ls(),c("mecitation","meblack","root")))
save.image("marginaleffects.RData")
#==================================================================================================
# Plotting Marginal Effects
#==================================================================================================
df                       = subset(meblack,term=="det" & detlabels=="Q3")
ggplot(df,aes(x=jurisdiction,color=templabels))+
     geom_point(mapping=aes(y=estimate),position=position_dodge(width=0.5))+
     geom_errorbar(aes(ymin=conf.low,ymax=conf.high),width=.1,
                   position=position_dodge(width=0.5))+theme_bw()+
     theme(axis.text.x = element_text(angle=90,vjust=0.5,hjust=1))+
     ylab("Marginal Probability")+ggtitle("Marginal Probability: Black")+
     theme(legend.position="bottom",axis.title.x=element_blank())+
     labs(color="Temperature Quartile:")
ggsave(paste(root,"/Manuscript/ALLmeblack.pdf",sep=""),width=11.5,height=7)
#--------------------------------------------------------------------------------------------------
df                       = subset(mecitation,term=="det" & detlabels=="Q3" & nonwhite==1)
ggplot(df,aes(x=jurisdiction,color=templabels))+
     geom_point(mapping=aes(y=estimate),position=position_dodge(width=0.5))+
     geom_errorbar(aes(ymin=conf.low,ymax=conf.high),width=.1,
                   position=position_dodge(width=0.5))+theme_bw()+
     theme(axis.text.x = element_text(angle=90,vjust=0.5,hjust=1))+
     ylab("Marginal Probability")+ggtitle("Marginal Probability: Citation")+
     theme(legend.position="bottom",axis.title.x=element_blank())+
     labs(color="Temperature Quartile:")
ggsave(paste(root,"/Manuscript/ALLmecitation.pdf",sep=""),width=11.5,height=7)
#--------------------------------------------------------------------------------------------------
# All additional marginal effects for the Supplemental Information
#--------------------------------------------------------------------------------------------------
listme                                            = unique(meblack[c("term","detlabels")])
listme$termname                                   = "Temperature"
listme$termname[which(listme$term=="det")]        = "Precipitation"
listme$detname                                    = "First Quartile"
listme$detname[which(listme$detlabels=="Q2")]     = "Second Quartile"
listme$detname[which(listme$detlabels=="Q3")]     = "Third Quartile"
listme$detname[which(listme$detlabels=="Max")]    = "Maximum"
listme$title             = paste("ME w.r.t ",listme$termname,
                                 " (Precipitation at the ",listme$detname,")",sep="")
#--------------------------------------------------------------------------------------------------
temp                   = subset(listme,term=="det")
for(i in 1:nrow(temp)){
     df   = subset(meblack,term==temp$term[i] & detlabels==temp$detlabels[i])
     ggplot(df,aes(x=jurisdiction,color=templabels))+
          geom_point(mapping=aes(y=estimate),position=position_dodge(width=0.5))+
          geom_errorbar(aes(ymin=conf.low,ymax=conf.high),width=.1,
                        position=position_dodge(width=0.5))+theme_bw()+
          theme(axis.text.x = element_text(angle=90,vjust=0.5,hjust=1))+
          ylab("Marginal Probability")+ggtitle(temp$title[i])+ylim(-0.8,0.8)+
          theme(legend.position="bottom",axis.title.x=element_blank())+
          labs(color="Temperature Quartile:")
     ggsave(paste(root,"/Supplemental Information/meb",temp$term[i],temp$detlabels[i],
                  ".pdf",sep=""),width=11.5,height=7)}
#--------------------------------------------------------------------------------------------------
temp                   = subset(listme,term=="tempk")
for(i in 1:nrow(temp)){
     df   = subset(meblack,term==temp$term[i] & detlabels==temp$detlabels[i])
     ggplot(df,aes(x=jurisdiction,color=templabels))+
          geom_point(mapping=aes(y=estimate),position=position_dodge(width=0.5))+
          geom_errorbar(aes(ymin=conf.low,ymax=conf.high),width=.1,
                        position=position_dodge(width=0.5))+theme_bw()+
          theme(axis.text.x = element_text(angle=90,vjust=0.5,hjust=1))+
          ylab("Marginal Probability")+ggtitle(temp$title[i])+ylim(-0.1,0.05)+
          theme(legend.position="bottom",axis.title.x=element_blank())+
          labs(color="Temperature Quartile:")
     ggsave(paste(root,"/Supplemental Information/meb",temp$term[i],temp$detlabels[i],
                  ".pdf",sep=""),width=11.5,height=7)}
#--------------------------------------------------------------------------------------------------
listme                                            = unique(mecitation[c("term","detlabels",
                                                                        "nonwhite")])
listme$termname                                   = "Temperature"
listme$termname[which(listme$term=="det")]        = "Precipitation"
listme$detname                                    = "First Quartile"
listme$detname[which(listme$detlabels=="Q2")]     = "Second Quartile"
listme$detname[which(listme$detlabels=="Q3")]     = "Third Quartile"
listme$detname[which(listme$detlabels=="Max")]    = "Maximum"
listme$nonwhite          = ifelse(listme$nonwhite==1,"Nonwhite","White")
listme$title             = paste("ME w.r.t. ",listme$termname,
                                 " (Precipitation at the ",listme$detname,")",sep="")
listme                   = subset(listme,nonwhite=="Nonwhite")
#--------------------------------------------------------------------------------------------------
temp                   = subset(listme,term=="det")
for(i in 1:nrow(temp)){
     df   = subset(mecitation,term==temp$term[i] & detlabels==temp$detlabels[i] & nonwhite==1)
     ggplot(df,aes(x=jurisdiction,color=templabels))+
          geom_point(mapping=aes(y=estimate),position=position_dodge(width=0.5))+
          geom_errorbar(aes(ymin=conf.low,ymax=conf.high),width=.1,
                        position=position_dodge(width=0.5))+theme_bw()+
          theme(axis.text.x = element_text(angle=90,vjust=0.5,hjust=1))+
          ylab("Marginal Probability")+ggtitle(temp$title[i])+ylim(-0.5,0.25)+
          theme(legend.position="bottom",axis.title.x=element_blank())+
          labs(color="Temperature Quartile:")
     ggsave(paste(root,"/Supplemental Information/mec",temp$term[i],temp$detlabels[i],
                  ".pdf",sep=""),width=11.5,height=7)}
#--------------------------------------------------------------------------------------------------
temp                   = subset(listme,term=="tempk")
for(i in 1:nrow(temp)){
     df   = subset(mecitation,term==temp$term[i] & detlabels==temp$detlabels[i] & nonwhite==1)
     ggplot(df,aes(x=jurisdiction,color=templabels))+
          geom_point(mapping=aes(y=estimate),position=position_dodge(width=0.5))+
          geom_errorbar(aes(ymin=conf.low,ymax=conf.high),width=.1,
                        position=position_dodge(width=0.5))+theme_bw()+
          theme(axis.text.x = element_text(angle=90,vjust=0.5,hjust=1))+
          ylab("Marginal Probability")+ggtitle(temp$title[i])+ylim(-0.025,0.025)+
          theme(legend.position="bottom",axis.title.x=element_blank())+
          labs(color="Temperature Quartile:")
     ggsave(paste(root,"/Supplemental Information/mec",temp$term[i],temp$detlabels[i],
                  ".pdf",sep=""),width=11.5,height=7)}
#==================================================================================================
# End of File
#==================================================================================================