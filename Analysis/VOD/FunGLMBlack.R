function(input=NULL,dataset){
     base      = "black~night+jcode+ctime+weekday+year+dststart"
     if (is.null(input)){
          eq = base 
     } else {
          eq  = paste(base,input,sep="+")
     }
     out  = glm(eq,data=dataset,family=binomial(link="logit"))
     return(out)}