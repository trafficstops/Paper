function(input=NULL,dataset){
     base = "black~night+factor(jcode)+ns(clocktime,df=6)+dststart+year"
     if (is.null(input)){
          eq = base 
     } else {
          eq  = paste(base,input,sep="+")
     }
     out  = glm(eq,data=dataset,family=binomial(link="logit"))
     return(out)}