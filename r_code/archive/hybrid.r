# function for huisman model of phytoplankton productivity taken from Vasconcelos et al. 2017 and from Jager and Diehl 2014
hybrid<-function(t,y,params){
  with(as.list(params),{
    #zs=seq(0.1,zmax,0.1)
    
    DOC=y[1]						# g C m-3
    A=y[2]							# mg C m-3
    Rpel=y[3]						# mg P m-3
    Bs=y[4:(3+length(zs))]
    Rbents=y[(4+length(zs)):(3+2*length(zs))]
    
    # using Morris paper relationship, but symbols from Huisman
    kD=kA*A+kDOC*DOC		#m-1; based on Morris paper
    
    # from a published paper -> I'll refind the paper eventually
    zmix=10^(-0.515*log10(DOC)+0.115*log10(2*sqrt(SA/pi))+0.991)
    if(zmix>zmax){zmix=zmax}
    
    # fully mixed
    #zmix=zmax
    
    Izmix=lightAtten(z=zmix,I0=I0,kD=kD)
    Izbents=lightAtten(z=zs,I0=I0,kD=kD)
    Izseds=lightAtten(z=zbent,I0=Izbents,kD=kB*Bs+kDOC*DOC)
    
    # biomass specific growth integrated across mixed layer
    PAt=(pA/(kD*zmix))*log((hA+I0)/(hA+Izmix))*(Rpel/(Rpel+mA))	# d-1
    PBts=(pB/((kB*Bs+kDOC*DOC)*zbent))*log((hB+Izbents)/(hB+Izseds))*(Rbents/(Rbents+mB)) # d-1
    
    dDOC.dt=Qin*(zmix/zmax)/(zmix*SA*1e6)*(DOCin-DOC)-decay*DOC
    #dDOC.dt=Qin/V*(DOCin-DOC)-decay*DOC
    dA.dt=A*PAt-lA*A-v/zmix*A-Qin*(zmix/zmax)/(zmix*SA*1e6)*A
    #dA.dt=A*PAt-lA*A-v/zmix*A-Qin/V*A
    dRpel.dt=Qin*(zmix/zmax)/(zmix*SA*1e6)*(Rin-Rpel)+sum(Dpel/zs[zs<=zmix]*(Rbents[zs<=zmix]-Rpel))/sum(zs<=zmix)+cA*lA*A*rec-cA*A*PAt
    #dRpel.dt=Qin/V*(Rin-Rpel)+sum(Dpel/zs[zs<=zmix]*(Rbents[zs<=zmix]-Rpel))/sum(zs<=zmix)+cA*lA*A*rec-cA*A*PAt
    dBs.dt=Bs*PBts-lB*Bs
    dRbents.dt=Dbent/zbent*(Rsed-Rbents)-Dpel/zbent*(Rbents-Rpel)+cB*fB*lB*Bs-cB*Bs*PBts
    
    # need to scale areas????
    #	-trying this by dividing by number of benthic layers; in some ways this counts for the "area" effect...
    
    return(list(c(dDOC.dt,dA.dt,dRpel.dt,dBs.dt,dRbents.dt)))
  })
}
