#==================================================================================================
# Title:                 Policing Main
# Date:                  3 April 2022
# Weather Scenario:      - Deterministic 20 Minutes
#                        - Stochastic 20 Minutes (T=3)
#                        - Stochastic 15 Minutes (T=4)
#                        - Stochastic 10 Minutes (T=6)
#==================================================================================================
rm(list=ls())
library(splines)
library(ggplot2)
library(mlogit)
library(stargazer)
library(margins)
memory.limit(size=30000)
setwd("G:/My Drive/Research/Live Manuscripts/Policing/Analysis")
load("PolicingData.RData")
#--------------------------------------------------------------------------------------------------
# Functions
#--------------------------------------------------------------------------------------------------
funprorain               = dget("FunProRain.R")
funormsbeesto            = dget("FunOrmsbeeStochastic.R")
funormsbeedet            = dget("FunOrmsbeeDeterministic.R")
#--------------------------------------------------------------------------------------------------
# City: NLDAS Data Availability
#--------------------------------------------------------------------------------------------------
dfcity                   = subset(policing,statepatrol==0)
rainvar                  = dfcity[c("nldasrainVm1","nldasrainV","nldasrainVp1")]
rainvar                  = is.na(rainvar)
rainvar                  = (rainvar[,1]+rainvar[,2]+rainvar[,3])==0
dfcity                   = dfcity[rainvar,]
#--------------------------------------------------------------------------------------------------
# City: Calculating NLDAS Rain Variables
#--------------------------------------------------------------------------------------------------
input                    = dfcity[c("nldasrainVm1","nldasrainV","nldasrainVp1","clocktime")]
input                    = as.matrix(input)
input[,4]                = input[,4]-floor(input[,4])
#--------------------------------------------------------------------------------------------------
df                       = apply(input,1,funormsbeedet)
df                       = as.data.frame(df)
colnames(df)             = c("det")
dfcity                   = cbind(dfcity,df)
#--------------------------------------------------------------------------------------------------
df                       = apply(input,1,funormsbeesto,T=3)
df                       = as.data.frame(df)
colnames(df)             = c("sto20")
dfcity                   = cbind(dfcity,df)
#--------------------------------------------------------------------------------------------------
df                       = apply(input,1,funormsbeesto,T=4)
df                       = as.data.frame(df)
colnames(df)             = c("sto15")
dfcity                   = cbind(dfcity,df)
#--------------------------------------------------------------------------------------------------
df                       = apply(input,1,funormsbeesto,T=6)
df                       = as.data.frame(df)
colnames(df)             = c("sto10")
dfcity                   = cbind(dfcity,df)
#--------------------------------------------------------------------------------------------------
# State: NLDAS Data Availability
#--------------------------------------------------------------------------------------------------
dfstate                  = subset(policing,statepatrol==1)
rainvar                  = dfstate[c("nldasrainVm1","nldasrainV","nldasrainVp1")]
rainvar                  = is.na(rainvar)
rainvar                  = (rainvar[,1]+rainvar[,2]+rainvar[,3])==0
dfstate                  = dfstate[rainvar,]
#--------------------------------------------------------------------------------------------------
# State: Calculating NLDAS Rain Variables
#--------------------------------------------------------------------------------------------------
input                    = dfstate[c("nldasrainVm1","nldasrainV","nldasrainVp1","clocktime")]
input                    = as.matrix(input)
input[,4]                = input[,4]-floor(input[,4])
#--------------------------------------------------------------------------------------------------
df                       = apply(input,1,funormsbeedet)
df                       = as.data.frame(df)
colnames(df)             = c("det")
dfstate                  = cbind(dfstate,df)
#--------------------------------------------------------------------------------------------------
df                       = apply(input,1,funormsbeesto,T=3)
df                       = as.data.frame(df)
colnames(df)             = c("sto20")
dfstate                  = cbind(dfstate,df)
#--------------------------------------------------------------------------------------------------
df                       = apply(input,1,funormsbeesto,T=4)
df                       = as.data.frame(df)
colnames(df)             = c("sto15")
dfstate                  = cbind(dfstate,df)
#--------------------------------------------------------------------------------------------------
df                       = apply(input,1,funormsbeesto,T=6)
df                       = as.data.frame(df)
colnames(df)             = c("sto10")
dfstate                  = cbind(dfstate,df)
#--------------------------------------------------------------------------------------------------
dfcity                   = dfcity[,!names(dfcity) %in% c("nldasrainV","nldasrainVm1",
                                                         "nldasrainVp1")]
dfstate                  = dfstate[,!names(dfstate) %in% c("nldasrainV","nldasrainVm1",
                                                           "nldasrainVp1")]
rm(df,input,policing,rainvar,funormsbeedet,funormsbeesto,funprorain)
#--------------------------------------------------------------------------------------------------
names(dfcity)[names(dfcity) == "nldastemperature"]     = "tempk"
names(dfstate)[names(dfstate) == "nldastemperature"]   = "tempk"
#--------------------------------------------------------------------------------------------------
save.image("PolicingRegData.RData")
#==================================================================================================
# Paper Main Results
#==================================================================================================
load("PolicingRegData.RData")
glmblack                 = dget("FunGLMBlack.R")
glmcitation              = dget("FunGLMCitation.R")
dfcity1                  = subset(dfcity,hispanic==0)
dfstate1                 = subset(dfstate,hispanic==0)
#==================================================================================================
# BLACK: City and State Regression Analysis
#==================================================================================================
black_c_p                = glmblack("det",dfcity1)
black_c_t                = glmblack("tempk",dfcity1)
black_c_pt               = glmblack("tempk+det+I(tempk*det)",dfcity1)
#--------------------------------------------------------------------------------------------------
black_s_p                = glmblack("det",dfstate1)
black_s_t                = glmblack("tempk",dfstate1)
black_s_pt               = glmblack("tempk+det+I(tempk*det)",dfstate1)
#--------------------------------------------------------------------------------------------------
stargazer(black_c_p,black_c_t,black_c_pt,black_s_p,black_s_t,black_s_pt,
          type="text",omit=c("jcode","clocktime","dststart"),no.space=TRUE)
#--------------------------------------------------------------------------------------------------
rm(dfcity1,dfstate1)
#==================================================================================================
# CITATION: City and State Regression Analysis
#==================================================================================================
dfcity$nonwhite          = dfcity$black+dfcity$hispanic
dfstate$nonwhite         = dfstate$black+dfstate$hispanic
#--------------------------------------------------------------------------------------------------
dfcity                   = subset(dfcity,!(jcode %in% c("AR1","AZ2","CA2","CO1","KS1","KY2","NV1",
                                                        "OK1","TX3","TX7")))
dfstate                  = subset(dfstate,!(jcode %in% c("TN2")))
#--------------------------------------------------------------------------------------------------
citation_c_p             = glmcitation("det",dfcity)
citation_c_t             = glmcitation("tempk",dfcity)
citation_c_pt            = glmcitation("tempk+det+I(tempk*det)",dfcity)
#--------------------------------------------------------------------------------------------------
citation_s_p             = glmcitation("det",dfstate)
citation_s_t             = glmcitation("tempk",dfstate)
citation_s_pt            = glmcitation("tempk+det+I(tempk*det)",dfstate)
#--------------------------------------------------------------------------------------------------
stargazer(citation_c_p,citation_c_t,citation_c_pt,citation_s_p,citation_s_t,citation_s_pt,
          type="text",omit=c("jcode","clocktime","dststart"),no.space=TRUE)
#--------------------------------------------------------------------------------------------------
rm(dfcity,dfstate,glmcitation)
#==================================================================================================
# Predicted Probabilities (Sheet "Predicted Probabilities" in Policing.xlsx)
#==================================================================================================
temperature              = data.frame(temperature=c(0,10,20,30))
precipitation            = data.frame(det=c(2.5/3,7.6/3,12.6/3),
                                      detname=c("Light rain","Moderate rain","Heavy rain"))
#--------------------------------------------------------------------------------------------------
newdf                    = data.frame(night=0,jcode="AR1",clocktime=18,dststart=0,year=2018,
                                      jurisdiction="City")
newdf                    = merge(newdf,temperature)
newdf$tempk              = newdf$temperature+273.15
newdf                    = merge(newdf,precipitation)
cityrain                 = newdf
cityrain$model           = "Rain"
cityrain$predprob        = predict(black_c_p,newdata=newdf,type="response")
citytemprain             = newdf
citytemprain$model       = "Interaction Rain Temperature"
citytemprain$predprob    = predict(black_c_pt,newdata=newdf,type="response")
#--------------------------------------------------------------------------------------------------
newdf                    = data.frame(night=0,jcode="CT2",clocktime=18,dststart=0,year=2018,
                                      jurisdiction="State")
newdf                    = merge(newdf,temperature)
newdf$tempk              = newdf$temperature+273.15
newdf                    = merge(newdf,precipitation)
staterain                = newdf
staterain$model          = "Rain"
staterain$predprob       = predict(black_s_p,newdata=newdf,type="response")
statetemprain            = newdf
statetemprain$model      = "Interaction Rain Temperature"
statetemprain$predprob   = predict(black_s_pt,newdata=newdf,type="response")
#--------------------------------------------------------------------------------------------------
newdf                    = rbind(cityrain,citytemprain,staterain,statetemprain)
newdf                    = newdf[c("jurisdiction","temperature","det","detname","model",
                                   "predprob")]
#--------------------------------------------------------------------------------------------------
newdfinteraction         = subset(newdf,model=="Interaction Rain Temperature")
levelorder               = c("Light rain","Moderate rain","Heavy rain")
ggplot(data=newdfinteraction,aes(x=factor(detname,levels=levelorder),
                                 y=predprob,fill=as.character(temperature)))+
     geom_bar(stat="identity",position=position_dodge())+theme_bw()+
     facet_wrap(vars(jurisdiction))+scale_fill_brewer(palette="Reds")+
     ylab("Predicted Probability")+
     theme(axis.title.x=element_blank(),
           legend.position="bottom",legend.title=element_blank())
ggsave("predictedprobabilities.pdf",width=6,height=3)
#==================================================================================================
# End of File
#==================================================================================================