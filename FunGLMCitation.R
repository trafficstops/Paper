function(input=NULL,dataset,jurisdiction){
     base      = "citation~nonwhite+night+ns1+ns2+ns3+ns4+ns5+ns6+dststart+year"
     cities    = paste("CA8","CT1","KY1","LA1","MN1","NC1","NC2","NC3","NC4","NC5",
                       "NC7","ND1","OH2","WI1",sep="+")
     states    = paste("MI1","NH1","TX8","WI2",sep="+") 
     if (is.null(input)){
          eq = base 
     } else {
          eq  = paste(base,input,sep="+")
     }
     if (jurisdiction=="City"){
          eq = paste(eq,cities,sep="+")
     } else if (jurisdiction=="State"){
          eq = paste(eq,states,sep="+")
     }
     out  = glm(eq,data=dataset,family=binomial(link="logit"))
     return(out)}