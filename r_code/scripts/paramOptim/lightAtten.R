#Function to calculate and return light at depth given depths z, light at surface I0, light attenuation coef kD

lightAtten<-function(z,I0,kD){
  Iz=I0*exp(-kD*z)
  return(Iz)
}