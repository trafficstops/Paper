#==================================================================================================
# Title:                 Policing Main (VOD)
# Date:                  1 April 2024
# Weather Scenario:      - Deterministic 20 Minutes
#                        - Stochastic 20 Minutes (T=3)
#                        - Stochastic 15 Minutes (T=4)
#                        - Stochastic 10 Minutes (T=6)
# BLACK Model:           - Base case for cities: AR1
#                        - Base case for states: CT2
# CITATIONS Model:       - Base case for cities: CA5
#                        - Base case for states: CT2
#==================================================================================================
rm(list=ls())
library(ggplot2)
library(ggpubr)
library(marginaleffects)
library(margins)
library(stargazer)
root                     = "D:/Research/Live Manuscripts/Policing"
setwd(paste(root,"/Analysis/VOD",sep=""))
load(paste(root,"/Analysis/DATA/VODPolicingRegData.RData",sep=""))
glmblack                 = dget("FunGLMBlack.R")
glmcitation              = dget("FunGLMCitation.R")
dfcity$ctime             = floor(dfcity$clocktime)
dfstate$ctime            = floor(dfstate$clocktime)
dfcity$nonwhite          = dfcity$black+dfcity$hispanic
dfstate$nonwhite         = dfstate$black+dfstate$hispanic
#--------------------------------------------------------------------------------------------------
dfcity$ctime             = factor(dfcity$ctime)
dfcity$weekday           = factor(dfcity$weekday)
dfcity$jcode             = factor(dfcity$jcode)
dfcity$year              = factor(dfcity$year)
#--------------------------------------------------------------------------------------------------
dfstate$ctime            = factor(dfstate$ctime)
dfstate$weekday          = factor(dfstate$weekday)
dfstate$jcode            = factor(dfstate$jcode)
dfstate$year             = factor(dfstate$year)
#==================================================================================================
# BLACK: City and State Logit Model
#==================================================================================================
dfcity1                  = subset(dfcity,hispanic==0)
dfstate1                 = subset(dfstate,hispanic==0)
#--------------------------------------------------------------------------------------------------
bc0                      = glmblack(NULL,dfcity1)
bc1                      = glmblack("det",dfcity1)
bc2                      = glmblack("det+night:det",dfcity1)
bc3                      = glmblack("det+tempk",dfcity1)
bc4                      = glmblack("tempk",dfcity1)
bc5                      = glmblack("det+tempk+det:tempk",dfcity1)
bc6                      = glmblack("det+tempk+det:tempk+night:det",dfcity1)
#--------------------------------------------------------------------------------------------------
bs0                      = glmblack(NULL,dfstate1)
bs1                      = glmblack("det",dfstate1)
bs2                      = glmblack("det+night:det",dfstate1)
bs3                      = glmblack("det+tempk",dfstate1)
bs4                      = glmblack("tempk",dfstate1)
bs5                      = glmblack("det+tempk+det:tempk",dfstate1)
bs6                      = glmblack("det+tempk+det:tempk+night:det",dfstate1)
#--------------------------------------------------------------------------------------------------
omitvar                  = c("jcode","ctime","weekday","year","dststart")
stargazer(bc0,bc1,bc2,bc3,bc4,bc5,bc6,type="text",no.space=TRUE,omit=omitvar)
stargazer(bs0,bs1,bs2,bs3,bs4,bs5,bs6,type="text",no.space=TRUE,omit=omitvar)
rm(dfcity1,dfstate1)
#==================================================================================================
# CITATION: City and State Logit Model
#==================================================================================================
citylist                 = c("CA5","CA8","CT1","KY1","LA1","MN1","NC1","NC2","NC3","NC4","NC5",
                             "NC7","ND1","OH2","WI1")
statelist                = c("CT2","MI1","NH1","TX8","WI2")
dfcity1                  = subset(dfcity,jcode %in% citylist)
dfstate1                 = subset(dfstate,jcode %in% statelist)
#--------------------------------------------------------------------------------------------------
cc0                      = glmcitation(NULL,dfcity1)
cc1                      = glmcitation("det",dfcity1)
cc2                      = glmcitation("det+night:det",dfcity1)
cc3                      = glmcitation("det+tempk",dfcity1)
cc4                      = glmcitation("tempk",dfcity1)
cc5                      = glmcitation("det+tempk+tempk:det",dfcity1)
cc6                      = glmcitation("det+tempk+tempk:det+night:det",dfcity1)
#--------------------------------------------------------------------------------------------------
cs0                      = glmcitation(NULL,dfstate1)
cs1                      = glmcitation("det",dfstate1)
cs2                      = glmcitation("det+night:det",dfstate1)
cs3                      = glmcitation("det+tempk",dfstate1)
cs4                      = glmcitation("tempk",dfstate1)
cs5                      = glmcitation("det+tempk+tempk:det",dfstate1)
cs6                      = glmcitation("det+tempk+tempk:det+night:det",dfstate1)
#--------------------------------------------------------------------------------------------------
omitvar                  = c("jcode","ctime","weekday","year","dststart")
stargazer(cc0,cc1,cc2,cc3,cc4,cc5,cc6,no.space=TRUE,type="text",omit=omitvar)
stargazer(cs0,cs1,cs2,cs3,cs4,cs5,cs6,no.space=TRUE,type="text",omit=omitvar)
#--------------------------------------------------------------------------------------------------
rm(dfcity1,dfstate1,glmcitation,glmblack,statelist,citylist)
#==================================================================================================
# Predicted Probabilities
#==================================================================================================
det                      = data.frame(det=seq(0,3,0.5))
tempk                    = seq(-10,30,10)+273.15
tempk                    = data.frame(tempk=tempk,templabels=c("-10C","0C","10C","20C","30C"))
#--------------------------------------------------------------------------------------------------
# Black City
#--------------------------------------------------------------------------------------------------
newdfcity                = unique(dfcity[c("jcode","year","weekday","months","night","ctime")])
newdfcity                = subset(newdfcity,jcode=="AR1" & weekday=="Wednesday" & ctime==18 &
                                            months=="March")
newdfcity                = merge(newdfcity,det)
newdfcity                = merge(newdfcity,tempk)
newdfcity$dststart       = 1
fitted                   = predict(bc6,newdata=newdfcity,type="response",se.fit=TRUE)
meblackcity              = slopes(bc6,newdata=newdfcity,variables=c("det","tempk"))
newdfcity$fitted         = fitted$fit
newdfcity$se             = fitted$se.fit
newdfcity$item           = "City"
#--------------------------------------------------------------------------------------------------
# Black State
#--------------------------------------------------------------------------------------------------
newdfstate               = unique(dfstate[c("jcode","year","weekday","months","night","ctime")])
newdfstate               = subset(newdfstate,jcode=="CT2" & weekday=="Wednesday" & ctime==18 &
                                             months=="March" & year==2015)
newdfstate               = merge(newdfstate,det)
newdfstate               = merge(newdfstate,tempk)
newdfstate$dststart      = 1
fitted                   = predict(bs6,newdata=newdfstate,type="response",se.fit=TRUE)
meblackstate             = slopes(bs6,newdata=newdfstate,variables=c("det","tempk"))
newdfstate$fitted        = fitted$fit
newdfstate$se            = fitted$se.fit
newdfstate$item          = "State"
#--------------------------------------------------------------------------------------------------
# Black: New Data Frame
#--------------------------------------------------------------------------------------------------
newdfblack               = rbind(newdfcity,newdfstate)
newdfblack$tempc         = newdfblack$tempk-273.15
newdfblack$night         = ifelse(newdfblack$night==1,"Night","Day")
#--------------------------------------------------------------------------------------------------
# Citation City
#--------------------------------------------------------------------------------------------------
newdfcity                = unique(dfcity[c("jcode","year","weekday","months","night","ctime",
                                           "nonwhite")])
newdfcity                = subset(newdfcity,jcode=="CA5" & weekday=="Wednesday" & ctime==18 &
                                       months=="March" & year==2017)
newdfcity                = merge(newdfcity,det)
newdfcity                = merge(newdfcity,tempk)
newdfcity$dststart       = 1
fitted                   = predict(cc6,newdata=newdfcity,type="response",se.fit=TRUE)
mecitationcity           = slopes(cc6,newdata=newdfcity,variables=c("det","tempk"))
newdfcity$fitted         = fitted$fit
newdfcity$se             = fitted$se.fit
newdfcity$item           = "City"
#--------------------------------------------------------------------------------------------------
# Citation State
#--------------------------------------------------------------------------------------------------
newdfstate               = unique(dfstate[c("jcode","year","weekday","months","night","ctime",
                                            "nonwhite")])
newdfstate               = subset(newdfstate,jcode=="CT2" & weekday=="Wednesday" & ctime==18 &
                                       months=="March" & year==2015)
newdfstate               = merge(newdfstate,det)
newdfstate               = merge(newdfstate,tempk)
newdfstate$dststart      = 1
fitted                   = predict(cs6,newdata=newdfstate,type="response",se.fit=TRUE)
mecitationstate          = slopes(cs6,newdata=newdfstate,variables=c("det","tempk"))
newdfstate$fitted        = fitted$fit
newdfstate$se            = fitted$se.fit
newdfstate$item          = "State"
#--------------------------------------------------------------------------------------------------
# Citation: New Data Frame
#--------------------------------------------------------------------------------------------------
newdfcitation            = rbind(newdfcity,newdfstate)
newdfcitation$tempc      = newdfcitation$tempk-273.15
newdfcitation$night      = ifelse(newdfcitation$night==1,"Night","Day")
newdfcitation$nonwhite   = ifelse(newdfcitation$nonwhite==1,"Nonwhite","White")
#==================================================================================================
# Plotting Predicted Probabilities
#==================================================================================================
fs                       = 15
#--------------------------------------------------------------------------------------------------
p1 = ggplot(newdfblack,aes(x=det,color=factor(templabels,c("-10C","0C","10C","20C","30C"))))+
     geom_point(aes(y=fitted),position=position_dodge(width=0.25))+
     geom_errorbar(aes(ymin=fitted-1.96*se,ymax=fitted+1.96*se),width=.01,
                   position=position_dodge(width=0.25))+
     facet_grid(vars(night),vars(item))+theme_bw()+xlab("Precipitation in mm per 20 Minutes")+
     ylab("Predicted Probability (Black=1)")+ylim(0,1)+
     ggtitle("(a) Predicted Probability: Black")+
     theme(legend.title=element_blank(),legend.position="bottom",text=element_text(size=fs))+
     scale_color_brewer(type="qual",labels=c("-10C","0C","10C","20C","30C"),
                        palette="Spectral",direction=-1)
#--------------------------------------------------------------------------------------------------
p2 = ggplot(subset(newdfcitation,item=="City"),
       aes(x=det,color=factor(templabels,c("-10C","0C","10C","20C","30C"))))+
     geom_point(aes(y=fitted),position=position_dodge(width=0.25))+
     geom_errorbar(aes(ymin=fitted-1.96*se,ymax=fitted+1.96*se),width=.01,
                   position=position_dodge(width=0.25))+
     facet_grid(vars(night),vars(nonwhite))+theme_bw()+xlab("Precipitation in mm per 20 Minutes")+
     ylab("Predicted Probability (Citation=1)")+
     ggtitle("(a) Predicted Probability: Citation (City)")+ylim(0,1)+
     theme(legend.title=element_blank(),legend.position="bottom",text=element_text(size=fs))+
     scale_color_brewer(type="qual",labels=c("-10C","0C","10C","20C","30C"),
                        palette="Spectral",direction=-1)
#--------------------------------------------------------------------------------------------------
p3 = ggplot(subset(newdfcitation,item=="State"),
       aes(x=det,color=factor(templabels,c("-10C","0C","10C","20C","30C"))))+
     geom_point(aes(y=fitted),position=position_dodge(width=0.25))+
     geom_errorbar(aes(ymin=fitted-1.96*se,ymax=fitted+1.96*se),width=.01,
                   position=position_dodge(width=0.25))+
     facet_grid(vars(night),vars(nonwhite))+theme_bw()+xlab("Precipitation in mm per 20 Minutes")+
     ylab("Predicted Probability (Citation=1)")+
     ggtitle("(b) Predicted Probability: Citation (State)")+ylim(0,1)+
     theme(legend.title=element_blank(),legend.position="bottom",text=element_text(size=fs))+
     scale_color_brewer(type="qual",labels=c("-10C","0C","10C","20C","30C"),
                        palette="Spectral",direction=-1)
#==================================================================================================
# Marginal Effects
#==================================================================================================
temp                = c("term","estimate","p.value","conf.low","conf.high","night","det",
                        "templabels")
meblackcity         = meblackcity[temp]
meblackcity$item    = "City"
meblackstate        = meblackstate[temp]
meblackstate$item   = "State"
meblack             = rbind(meblackcity,meblackstate)
meblack$night       = ifelse(meblack$night==1,"Night","Day")
#--------------------------------------------------------------------------------------------------
temp                = c("term","estimate","p.value","conf.low","conf.high","night","det",
                        "templabels","nonwhite")
mecitationcity      = mecitationcity[temp]
mecitationcity$item = "City"
mecitationstate     = mecitationstate[temp]
mecitationstate$item= "State"
mecitation          = rbind(mecitationcity,mecitationstate)
mecitation$night    = ifelse(mecitation$night==1,"Night","Day")
mecitation$nonwhite = ifelse(mecitation$nonwhite==1,"Nonwhite","White")
#--------------------------------------------------------------------------------------------------
p4 = ggplot(subset(meblack,term=="det"),
       aes(x=det,color=factor(templabels,c("-10C","0C","10C","20C","30C"))))+
     geom_point(aes(y=estimate),position=position_dodge(width=0.25))+
     geom_errorbar(aes(ymin=conf.low,ymax=conf.high),width=.01,
                   position=position_dodge(width=0.25))+
     facet_grid(vars(night),vars(item))+theme_bw()+xlab("Precipitation in mm per 20 Minutes")+
     ylab("Marginal Probability")+ylim(-0.15,0.25)+
     ggtitle("(b) Marginal Probability: Black")+
     theme(legend.title=element_blank(),legend.position="bottom",text=element_text(size=fs))+
     scale_color_brewer(type="qual",labels=c("-10C","0C","10C","20C","30C"),
                        palette="Spectral",direction=-1)
#--------------------------------------------------------------------------------------------------
p5 = ggplot(subset(mecitation,term=="det" & item=="City"),
       aes(x=det,color=factor(templabels,c("-10C","0C","10C","20C","30C"))))+
     geom_point(aes(y=estimate),position=position_dodge(width=0.25))+
     geom_errorbar(aes(ymin=conf.low,ymax=conf.high),width=.01,
                   position=position_dodge(width=0.25))+
     facet_grid(vars(night),vars(nonwhite))+theme_bw()+xlab("Precipitation in mm per 20 Minutes")+
     ylab("Marginal Probability")+ylim(-0.15,0.25)+
     ggtitle("(c) Marginal Probability: Citation (City)")+
     theme(legend.title=element_blank(),legend.position="bottom",text=element_text(size=fs))+
     scale_color_brewer(type="qual",labels=c("-10C","0C","10C","20C","30C"),
                        palette="Spectral",direction=-1)
#--------------------------------------------------------------------------------------------------
p6 = ggplot(subset(mecitation,term=="det" & item=="State"),
       aes(x=det,color=factor(templabels,c("-10C","0C","10C","20C","30C"))))+
     geom_point(aes(y=estimate),position=position_dodge(width=0.25))+
     geom_errorbar(aes(ymin=conf.low,ymax=conf.high),width=.01,
                   position=position_dodge(width=0.25))+
     facet_grid(vars(night),vars(nonwhite))+theme_bw()+xlab("Precipitation in mm per 20 Minutes")+
     ylab("Marginal Probability")+ylim(-0.15,0.25)+
     ggtitle("(d) Marginal Probability: Citation (State)")+
     theme(legend.title=element_blank(),legend.position="bottom",text=element_text(size=fs))+
     scale_color_brewer(type="qual",labels=c("-10C","0C","10C","20C","30C"),
                        palette="Spectral",direction=-1)
#--------------------------------------------------------------------------------------------------
ggarrange(p1,p4,common.legend=TRUE,legend="bottom")
ggsave(paste(root,"/Manuscript/VODblack.pdf",sep=""),width=13,height=6)
#--------------------------------------------------------------------------------------------------
ggarrange(p2,p3,p5,p6,common.legend=TRUE,legend="bottom")
ggsave(paste(root,"/Manuscript/VODcitation.pdf",sep=""),width=13,height=12)
#==================================================================================================
# End of File
#==================================================================================================