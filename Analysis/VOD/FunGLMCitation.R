function(input=NULL,dataset,jurisdiction){
     base      = "citation~nonwhite+night+jcode+ctime+weekday+year"
     if (is.null(input)){
          eq = base 
     } else {
          eq  = paste(base,input,sep="+")
     }
     out  = glm(eq,data=dataset,family=binomial(link="logit"))
     return(out)}