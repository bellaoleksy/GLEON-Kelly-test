#Function to run 'huisman' (Kelly model)

#State variables (with initial values y) are mixed layer concentrations of:
# A     Algal biomass, mg C m-3
# P     Phosphorus, mg P m-3
# DOC   Dissolved organic carbon, g C m-3

#Parameters (params) are:
# kDOC
# kA
# lA
# pA
# hA
# mA
# decay
# cA
# rec

#Additional inputs are:
# SA    Surface are of the lake, km2
# zMax  Maximum depth of the lake, m
# zMean Mean depth of the lake, m
# Qin   Hydrologic inflow, m3 day-1 #########IAO corrected to m3 day-1 from m3 year-1
# I0    Average incident light at the surface, umol photons m-2 s-1
# Pin   Concentration of dissolved phosphorus in hydrologic inputs, mg P m-3
# DOCin Concentration of dissolved organic carbon in hydrologic inputs, g C m-3


huisman<-function(t,y,params,SA,zMax,zMean,Qin,I0,Pin,DOCin){
  with(as.list(c(y,params)),{
    
    # using Morris paper relationship, but symbols from Huisman
    kD=kA*A+kDOC*DOC-0.05	#m-1; based on Morris paper
    
    # from a published paper -> I'll refind the paper eventually
    zMix=10^(-0.515*log10(DOC)+0.115*log10(2*sqrt(SA/pi))+0.991)
    if(zMix>zMax){zMix=zMax}
    
    # V=SA*1e6*zMean
    
    IZMix=lightAtten(z=zMix,I0=I0,kD=kD)
    
    # biomass specific growth integrated across mixed layer
    prod=(pA/(kD*zMix))*log((hA+I0)/(hA+IZMix))*(P/(P+mA))	# d-1
    
    dA.dt=A*prod-lA*A-v/zMix*A-Qin/(zMix*SA*1e6)*A	# mg C m-3
    dP.dt=Qin/(zMix*SA*1e6)*(Pin-P)+cA*lA*A*rec-cA*A*prod		# mg P m-3
    dDOC.dt=(Qin/(zMix*SA*1e6))*(DOCin-DOC)-decay*DOC				# g C m-3
    
    return(list(c(dA.dt,dP.dt,dDOC.dt)))
  })
}
