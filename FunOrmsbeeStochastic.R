function(input,T){
     Vm1       = input[1]
     V         = input[2]
     Vp1       = input[3]
     input[4]  = (input[4]-floor(input[4]))*60
     tseq      = seq(0,60,60/T)
     idpos     = min(which(tseq>input[4]))-1
     if(Vm1 > V & V > Vp1){
          tstar    = 60*(Vm1-V)/(Vm1-Vp1)
     } else if (Vm1 < V & V < Vp1) {
          tstar    = 60*(V-Vm1)/(Vp1-Vm1)
     } else if (Vm1 > V & Vp1 > V) {
          tstar    = 60*(Vm1-V)/(Vm1+Vp1-2*V)
     } else if (Vm1 < V & Vp1 < V) {
          tstar    = 60*(V-Vm1)/(2*V-Vm1-Vp1)
     } else  {
          tstar    = 30
     }
     Vstar    = 30*(V+Vp1)-0.5*tstar*(Vp1-Vm1)
     tseq     = mapply(funprorain,tseq,Vm1=Vm1,
                       V=V,Vp1=Vp1,Vstar=Vstar,tstar=tstar)
     tseq     = diff(tseq)*V
     output   = tseq[idpos]
     output[is.na(output)] = 0
     return(output)}