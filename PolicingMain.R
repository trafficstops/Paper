#==================================================================================================
# Title:                 Policing Main
# Date:                  3 January 2023
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
library(fastDummies)
library(ggplot2)
library(margins)
library(splines)
library(stargazer)
memory.limit(size=30000)
root                     = "G:/My Drive/Research/Live Manuscripts/Policing"
setwd(paste(root,"/Analysis",sep=""))
#--------------------------------------------------------------------------------------------------
# Data Load and Functions
#--------------------------------------------------------------------------------------------------
load("PolicingData.RData")
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
citylist                 = c("AR1","AZ2","CA2","CA4","CA5","CA7","CA8","CA9","CO1","CT1","KS1",
                             "KY1","KY2","LA1","MN1","NC1","NC2","NC3","NC4","NC5","NC7","ND1",
                             "NJ1","NV1","NY1","OH1","OH2","OK1","OK2","PA1","TN1","TX1","TX3",
                             "TX6","TX7","VT1","WI1")
statelist                = c("CT2","FL2","GA1","MI1","ND2","NH1","NY2","OH3","TN2","TX8","WI2")
nslist                   = c("ns1","ns2","ns3","ns4","ns5","ns6")
#==================================================================================================
# BLACK: City and State Logit Model
#==================================================================================================
dfcity1                  = subset(dfcity,hispanic==0)
dfstate1                 = subset(dfstate,hispanic==0)
nsdf6                    = ns(dfcity1$clocktime,df=6)
colnames(nsdf6)          = nslist
dfcity1                  = cbind(dfcity1,nsdf6)
dfcity1                  = dummy_cols(dfcity1,select_columns="jcode")
names(dfcity1)           = sub("jcode_","",names(dfcity1))
#--------------------------------------------------------------------------------------------------
black_c_p                = glmblack("det",dfcity1,"City")
black_c_p_np             = glmblack("det+night:det",dfcity1,"City")
black_c_p_t              = glmblack("det+tempk",dfcity1,"City")
black_c_p_t_pt           = glmblack("det+tempk+det:tempk",dfcity1,"City")
black_c_p_t_pt_np        = glmblack("det+tempk+det:tempk+night:det",dfcity1,"City")
#--------------------------------------------------------------------------------------------------
nsdf6                    = ns(dfstate1$clocktime,df=6)
colnames(nsdf6)          = nslist
dfstate1                 = cbind(dfstate1,nsdf6)
dfstate1                 = dummy_cols(dfstate1,select_columns="jcode")
names(dfstate1)          = sub("jcode_","",names(dfstate1))
#--------------------------------------------------------------------------------------------------
black_s_p                = glmblack("det",dfstate1,"State")
black_s_p_np             = glmblack("det+night:det",dfstate1,"State")
black_s_p_t              = glmblack("det+tempk",dfstate1,"State")
black_s_p_t_pt           = glmblack("det+tempk+det:tempk",dfstate1,"State")
black_s_p_t_pt_np        = glmblack("det+tempk+det:tempk+night:det",dfstate1,"State")
#--------------------------------------------------------------------------------------------------
stargazer(black_c_p,black_c_p_np,black_c_p_t,black_c_p_t_pt,black_c_p_t_pt_np,
          type="text",no.space=TRUE,omit=c(citylist,statelist,nslist,"clocktime","dststart"))
stargazer(black_s_p,black_s_p_np,black_s_p_t,black_s_p_t_pt,black_s_p_t_pt_np,
          type="text",no.space=TRUE,omit=c(citylist,statelist,nslist,"clocktime","dststart"))
rm(dfcity1,dfstate1,nsdf6)
#==================================================================================================
# CITATION: City and State Logit Model
#==================================================================================================
dfcity$nonwhite          = dfcity$black+dfcity$hispanic
dfstate$nonwhite         = dfstate$black+dfstate$hispanic
citylist                 = c("CA5","CA8","CT1","KY1","LA1","MN1","NC1","NC2","NC3","NC4","NC5",
                             "NC7","ND1","OH2","WI1")
statelist                = c("CT2","MI1","NH1","TX8","WI2")
dfcity                   = subset(dfcity,jcode %in% citylist)
dfstate                  = subset(dfstate,jcode %in% statelist)
#--------------------------------------------------------------------------------------------------
nsdf6                    = ns(dfcity$clocktime,df=6)
colnames(nsdf6)          = nslist
dfcity                   = cbind(dfcity,nsdf6)
dfcity                   = dummy_cols(dfcity,select_columns="jcode")
names(dfcity)            = sub("jcode_","",names(dfcity))
#--------------------------------------------------------------------------------------------------
citation_c_p             = glmcitation("det",dfcity,"City")
citation_c_p_np          = glmcitation("det+night:det",dfcity,"City")
citation_c_p_t           = glmcitation("det+tempk",dfcity,"City")
citation_c_p_t_pt        = glmcitation("det+tempk+tempk:det",dfcity,"City")
citation_c_p_t_pt_np     = glmcitation("det+tempk+tempk:det+night:det",dfcity,"City")
#--------------------------------------------------------------------------------------------------
nsdf6                    = ns(dfstate$clocktime,df=6)
colnames(nsdf6)          = nslist
dfstate                  = cbind(dfstate,nsdf6)
dfstate                  = dummy_cols(dfstate,select_columns="jcode")
names(dfstate)           = sub("jcode_","",names(dfstate))
#--------------------------------------------------------------------------------------------------
citation_s_p             = glmcitation("det",dfstate,"State")
citation_s_p_np          = glmcitation("det+night:det",dfstate,"State")
citation_s_p_t           = glmcitation("det+tempk",dfstate,"State")
citation_s_p_t_pt        = glmcitation("det+tempk+tempk:det",dfstate,"State")
citation_s_p_t_pt_np     = glmcitation("det+tempk+tempk:det+night:det",dfstate,"State")
#--------------------------------------------------------------------------------------------------
stargazer(citation_c_p,citation_c_p_np,citation_c_p_t,citation_c_p_t_pt,citation_c_p_t_pt_np,
          no.space=TRUE,type="text",omit=c(citylist,statelist,nslist,"clocktime","dststart"))
stargazer(citation_s_p,citation_s_p_np,citation_s_p_t,citation_s_p_t_pt,citation_s_p_t_pt_np,
          no.space=TRUE,type="text",omit=c(citylist,statelist,nslist,"clocktime","dststart"))
#--------------------------------------------------------------------------------------------------
rm(dfcity,dfstate,glmcitation,glmblack,statelist,citylist,nsdf6,nslist)
#==================================================================================================
# Predicted Probabilities and Marginal Effects: Black
#==================================================================================================
rainfall                 = seq(from=0,to=3,by=0.5)
temperature              = seq(from=-10,to=30,by=10)+273.15
newdfcity                = black_c_p_t_pt_np$model
newdfcity                = newdfcity[0,]
dftemplate               = newdfcity
dftemplate[1,]           = colMeans(black_c_p_t_pt_np$model)
#--------------------------------------------------------------------------------------------------
for(daylight in c(0,1)){
     for(kelvin in temperature){
          for(rain in rainfall){
               df        = dftemplate
               df$night  = daylight
               df$tempk  = kelvin
               df$det    = rain
               newdfcity = rbind(newdfcity,df)}}}
#--------------------------------------------------------------------------------------------------
newdfstate               = black_s_p_t_pt_np$model
newdfstate               = newdfstate[0,]
dftemplate               = newdfstate
dftemplate[1,]           = colMeans(black_s_p_t_pt_np$model)
#--------------------------------------------------------------------------------------------------
for(daylight in c(0,1)){
     for(kelvin in temperature){
          for(rain in rainfall){
               df        = dftemplate
               df$night  = daylight
               df$tempk  = kelvin
               df$det    = rain
               newdfstate= rbind(newdfstate,df)}}}
#--------------------------------------------------------------------------------------------------
rm(daylight,kelvin,rain,rainfall,temperature,df,dftemplate)
#--------------------------------------------------------------------------------------------------
ppr_black_c              = margins(model=black_c_p_t_pt_np,data=newdfcity,type="response")
ppr_black_c              = ppr_black_c[c("night","tempk","det","fitted","se.fitted")]
ppr_black_c$item         = "City"
ppr_black_c$model        = "C5"
temp                     = margins(model=black_c_p,data=newdfcity,type="response")
temp                     = temp[c("night","tempk","det","fitted","se.fitted")]
temp$item                = "City"
temp$model               = "C1"
ppr_black_c              = rbind(ppr_black_c,temp)
temp                     = margins(model=black_c_p_np,data=newdfcity,type="response")
temp                     = temp[c("night","tempk","det","fitted","se.fitted")]
temp$item                = "City"
temp$model               = "C2"
ppr_black_c              = rbind(ppr_black_c,temp)
temp                     = margins(model=black_c_p_t,data=newdfcity,type="response")
temp                     = temp[c("night","tempk","det","fitted","se.fitted")]
temp$item                = "City"
temp$model               = "C3"
ppr_black_c              = rbind(ppr_black_c,temp)
temp                     = margins(model=black_c_p_t_pt,data=newdfcity,type="response")
temp                     = temp[c("night","tempk","det","fitted","se.fitted")]
temp$item                = "City"
temp$model               = "C4"
ppr_black_c              = rbind(ppr_black_c,temp)
ppr_black_s              = margins(model=black_s_p_t_pt_np,data=newdfstate,type="response")
ppr_black_s              = ppr_black_s[c("night","tempk","det","fitted","se.fitted")]
ppr_black_s$item         = "State"
ppr_black_s$model        = "S5"
temp                     = margins(model=black_s_p,data=newdfstate,type="response")
temp                     = temp[c("night","tempk","det","fitted","se.fitted")]
temp$item                = "State"
temp$model               = "S1"
ppr_black_s              = rbind(ppr_black_s,temp)
temp                     = margins(model=black_s_p_np,data=newdfstate,type="response")
temp                     = temp[c("night","tempk","det","fitted","se.fitted")]
temp$item                = "State"
temp$model               = "S2"
ppr_black_s              = rbind(ppr_black_s,temp)
temp                     = margins(model=black_s_p_t,data=newdfstate,type="response")
temp                     = temp[c("night","tempk","det","fitted","se.fitted")]
temp$item                = "State"
temp$model               = "S3"
ppr_black_s              = rbind(ppr_black_s,temp)
temp                     = margins(model=black_s_p_t_pt,data=newdfstate,type="response")
temp                     = temp[c("night","tempk","det","fitted","se.fitted")]
temp$item                = "State"
temp$model               = "S4"
ppr_black_s              = rbind(ppr_black_s,temp)
ppr_black                = rbind(ppr_black_c,ppr_black_s)
ppr_black$night          = ifelse(ppr_black$night==1,"Night","Day")
labels                   = c("-10C","0C","10C","20C","30C")
breaks                   = seq(from=-15,to=35,by=10)+273.15
#--------------------------------------------------------------------------------------------------
ggplot(subset(ppr_black,model %in% c("C5","S5")),
              aes(x=det,color=cut(tempk,breaks=breaks,include.lowest=TRUE)))+
     geom_point(aes(y=fitted),position=position_dodge(width=0.25))+
     geom_errorbar(aes(ymin=fitted-1.96*se.fitted,ymax=fitted+1.96*se.fitted),width=.01,
                   position=position_dodge(width=0.25))+
     facet_grid(vars(night),vars(item))+theme_bw()+xlab("Precipitation in mL per 20 Minutes")+
     ylab("Predicted Probability (Black=1)")+ylim(0,1)+
     theme(legend.title=element_blank(),legend.position="bottom")+
     scale_color_brewer(type="qual",labels=labels,palette="Spectral",direction=-1)
ggsave(paste(root,"/Manuscript/pprblack.pdf",sep=""),width=7,height=5)
#--------------------------------------------------------------------------------------------------
rm(black_c_p,black_c_p_np,black_c_p_t,black_c_p_t_pt,black_c_p_t_pt_np,
   black_s_p,black_s_p_np,black_s_p_t,black_s_p_t_pt,black_s_p_t_pt_np,
   ppr_black_c,ppr_black_s)
#--------------------------------------------------------------------------------------------------
ppr_black                = ppr_black[c("night","tempk","det","fitted","item","model")]
ppr_black$temperature    = ppr_black$tempk-273.15
#==================================================================================================
# Predicted Probabilities: Citation
#==================================================================================================
rainfall                 = seq(from=0,to=3,by=0.5)
temperature              = seq(from=-10,to=30,by=10)+273.15
newdfcity                = citation_c_p_t_pt_np$model
newdfcity                = newdfcity[0,]
dftemplate               = newdfcity
dftemplate[1,]           = colMeans(citation_c_p_t_pt_np$model)
#--------------------------------------------------------------------------------------------------
for(race in c(0,1)){
     for(daylight in c(0,1)){
          for(kelvin in temperature){
               for(rain in rainfall){
                    df             = dftemplate
                    df$night       = daylight
                    df$tempk       = kelvin
                    df$det         = rain
                    df$nonwhite    = race
                    newdfcity      = rbind(newdfcity,df)}}}}
#--------------------------------------------------------------------------------------------------
newdfstate               = citation_s_p_t_pt_np$model
newdfstate               = newdfstate[0,]
dftemplate               = newdfstate
dftemplate[1,]           = colMeans(citation_s_p_t_pt_np$model)
#--------------------------------------------------------------------------------------------------
for(race in c(0,1)){
     for(daylight in c(0,1)){
          for(kelvin in temperature){
               for(rain in rainfall){
                    df             = dftemplate
                    df$night       = daylight
                    df$tempk       = kelvin
                    df$det         = rain
                    df$nonwhite    = race
                    newdfstate     = rbind(newdfstate,df)}}}}
#--------------------------------------------------------------------------------------------------
rm(daylight,kelvin,rain,rainfall,temperature,df,dftemplate)
#--------------------------------------------------------------------------------------------------
ppr_citation_c           = margins(model=citation_c_p_t_pt_np,data=newdfcity,type="response",
                                   variables=c("nonwhite","det","night","tempk"))
ppr_citation_c           = ppr_citation_c[c("nonwhite","night","tempk","det","fitted","se.fitted")]
ppr_citation_c$model     = "C5"
temp                     = margins(model=citation_c_p,data=newdfcity,type="response")
temp                     = temp[c("nonwhite","night","tempk","det","fitted","se.fitted")]
temp$model               = "C1"
ppr_citation_c           = rbind(ppr_citation_c,temp)
temp                     = margins(model=citation_c_p_np,data=newdfcity,type="response")
temp                     = temp[c("nonwhite","night","tempk","det","fitted","se.fitted")]
temp$model               = "C2"
ppr_citation_c           = rbind(ppr_citation_c,temp)
temp                     = margins(model=citation_c_p_t,data=newdfcity,type="response")
temp                     = temp[c("nonwhite","night","tempk","det","fitted","se.fitted")]
temp$model               = "C3"
ppr_citation_c           = rbind(ppr_citation_c,temp)
temp                     = margins(model=citation_c_p_t_pt,data=newdfcity,type="response")
temp                     = temp[c("nonwhite","night","tempk","det","fitted","se.fitted")]
temp$model               = "C4"
ppr_citation_c           = rbind(ppr_citation_c,temp)
ppr_citation_c$night     = ifelse(ppr_citation_c$night==1,"Night","Day")
ppr_citation_c$nonwhite  = ifelse(ppr_citation_c$nonwhite==1,"Nonwhite","White")
ppr_citation_c$tempc     = ppr_citation_c$tempk-273.15
#--------------------------------------------------------------------------------------------------
ppr_citation_s           = margins(model=citation_s_p_t_pt_np,data=newdfstate,type="response")
ppr_citation_s           = ppr_citation_s[c("nonwhite","night","tempk","det","fitted","se.fitted")]
ppr_citation_s$model     = "S5"
temp                     = margins(model=citation_s_p,data=newdfstate,type="response")
temp                     = temp[c("nonwhite","night","tempk","det","fitted","se.fitted")]
temp$model               = "S1"
ppr_citation_s           = rbind(ppr_citation_s,temp)
temp                     = margins(model=citation_s_p_np,data=newdfstate,type="response")
temp                     = temp[c("nonwhite","night","tempk","det","fitted","se.fitted")]
temp$model               = "S2"
ppr_citation_s           = rbind(ppr_citation_s,temp)
temp                     = margins(model=citation_s_p_t,data=newdfstate,type="response")
temp                     = temp[c("nonwhite","night","tempk","det","fitted","se.fitted")]
temp$model               = "S3"
ppr_citation_s           = rbind(ppr_citation_s,temp)
temp                     = margins(model=citation_s_p_t_pt,data=newdfstate,type="response")
temp                     = temp[c("nonwhite","night","tempk","det","fitted","se.fitted")]
temp$model               = "S4"
ppr_citation_s           = rbind(ppr_citation_s,temp)
ppr_citation_s$night     = ifelse(ppr_citation_s$night==1,"Night","Day")
ppr_citation_s$nonwhite  = ifelse(ppr_citation_s$nonwhite==1,"Nonwhite","White")
ppr_citation_s$tempc     = ppr_citation_s$tempk-273.15
#--------------------------------------------------------------------------------------------------
labels                   = c("-10C","0C","10C","20C","30C")
breaks                   = seq(from=-15,to=35,by=10)+273.15
ggplot(subset(ppr_citation_c,model==c("C5")),
       aes(x=det,color=cut(tempk,breaks=breaks,include.lowest=TRUE)))+
     geom_point(aes(y=fitted),position=position_dodge(width=0.25))+
     geom_errorbar(aes(ymin=fitted-1.96*se.fitted,ymax=fitted+1.96*se.fitted),width=.01,
                   position=position_dodge(width=0.25))+
     facet_grid(vars(night),vars(nonwhite))+theme_bw()+xlab("Precipitation in mL per 20 Minutes")+
     ylab("Predicted Probability (Citation=1)")+ggtitle("City")+ylim(0,1)+
     theme(legend.title=element_blank(),legend.position="bottom")+
     scale_color_brewer(type="qual",labels=labels,palette="Spectral",direction=-1)
ggsave(paste(root,"/Manuscript/pprcitationc.pdf",sep=""),width=7,height=5)
#--------------------------------------------------------------------------------------------------
labels                   = c("-10C","0C","10C","20C","30C")
breaks                   = seq(from=-15,to=35,by=10)+273.15
ggplot(subset(ppr_citation_s,model==c("S5")),
       aes(x=det,color=cut(tempk,breaks=breaks,include.lowest=TRUE)))+
     geom_point(aes(y=fitted),position=position_dodge(width=0.25))+
     geom_errorbar(aes(ymin=fitted-1.96*se.fitted,ymax=fitted+1.96*se.fitted),width=.01,
                   position=position_dodge(width=0.25))+
     facet_grid(vars(night),vars(nonwhite))+theme_bw()+xlab("Precipitation in mL per 20 Minutes")+
     ylab("Predicted Probability (Citation=1)")+ggtitle("State")+ylim(0,1)+
     theme(legend.title=element_blank(),legend.position="bottom")+
     scale_color_brewer(type="qual",labels=labels,palette="Spectral",direction=-1)
ggsave(paste(root,"/Manuscript/pprcitations.pdf",sep=""),width=7,height=5)
#==================================================================================================
# End of File
#==================================================================================================