function(input=NULL,dataset,jurisdiction){
     base      = "black~night+ns1+ns2+ns3+ns4+ns5+ns6+dststart+year"
     cities    = paste("AZ2","CA2","CA4","CA5","CA7","CA8","CA9","CO1","CT1","KS1","KY1","KY2",
                       "LA1","MN1","NC1","NC2","NC3","NC4","NC5","NC7","ND1","NJ1","NV1","NY1",
                       "OH1","OH2","OK1","OK2","PA1","TN1","TX1","TX3","TX6","TX7","VT1","WI1",
                       sep="+")
     states    = paste("FL2","GA1","MI1","ND2","NH1","NY2","OH3","TN2","TX8","WI2",sep="+") 
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