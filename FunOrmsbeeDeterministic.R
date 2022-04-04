function(input){
     Vm1       = input[1]
     V         = input[2]
     Vp1       = input[3]
     input[4]  = (input[4]-floor(input[4]))*60
     Vsum      = Vm1+V+Vp1
     tseq      = seq(0,60,60/3)
     idpos     = min(which(tseq>input[4]))-1
     tseq      = c(V*Vm1/Vsum,V*V/Vsum,V*Vp1/Vsum)
     output    = tseq[idpos]
     output[is.na(output)] = 0
     return(output)} 