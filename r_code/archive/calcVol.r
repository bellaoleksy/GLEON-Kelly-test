#define function for calculating volumes give depth-area data
calcVol<-function(data){
  z=approxfun(x=data[,1],y=data[,2])
  return(integrate(z,min(data[,1]),max(data[,1]))$value)
}
