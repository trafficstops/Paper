function(input=NULL,dataset){
     base      = "black~night+ctime+months+weekday+year"
     if(length(unique(dataset$year))==1){
          base = "black~night+ctime+months+weekday"}
     if (is.null(input)){
          eq = base 
     } else {
          eq  = paste(base,input,sep="+")
     }
     out  = glm(eq,data=dataset,family=binomial(link="logit"))
     return(out)}