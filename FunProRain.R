function(t,Vm1,V,Vp1,Vstar,tstar){(t<tstar)*(Vm1*t/Vstar-((Vm1-V)*t^2)/
                                                  (2*Vstar*tstar))+(t>=tstar)*((V+Vm1)*tstar/(2*Vstar)+V*(t-tstar)/
                                                                                    Vstar-((V-Vp1)*(t-tstar)^2)/(2*Vstar*(60-tstar)))}