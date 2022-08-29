standard<-function(xi,yi){
  xiv<-xi/max(xi)
  yiv<-yi/(max(yi))
  list(xiv=xiv,yiv=yiv)
}

sl<-function(df,pos){
  slopes<-(df[pos,2]-df[(pos-1),2])/(df[pos,1]-df[(pos-1),1])
  slopes<-unique(round(slopes,10))
  return(is.unsorted(slopes))
}

pe<-function(data=list(),version = "proportional",plot_pe=TRUE, correct=TRUE)
{
  if(length(which(is.na(data$pi)))>0){
    data<-data[-which(is.na(data$pi)),]; warning("NAs in pi column")
  }

  if(length(which(is.na(data$ai)))>0){
    data<-data[-which(is.na(data$ai)),]; warning("NAs in ai column")
  } 
  
  N<-length(data$pi)
  pi<-data$pi
  ai<-data$ai
  pi_ai<-pi/ai
  order1<-order(pi_ai) 
  order2<-order(pi)
  
  if(sum(data$pi)==0){
    PE<-NA
    geom<-NA
    unsorted_slope<-NA
     warning("All pi equal 0- cannot compute PE")
  }
  
    
  if(sum(data$pi)>0){    
    # version 1 #
    if (version=="proportional")
    {
      sec_half_v1<-rep(0,N-1)
      order<-order1  # ordering by pi/ai
      xi<-c(0,cumsum(rep(1/N,N))) # x-axis is 1/n
      yi<-c(0,cumsum(pi_ai[order])) # y-axis is pi/ai
      
      xiv<-standard(xi,yi)$xiv
      yiv<-standard(xi,yi)$yiv
      
      for(k in 1:(N-1))
      {
        sec_half_v1[k]<-pi_ai[order][k]*(N-k)
      }
      PE<-(1/N*(1/2*cumsum(pi_ai[order])[N] + sum(sec_half_v1)))/(1/2*cumsum(pi_ai[order])[N])
    }  
    
    
    if(version=="fixed") #used to be version 3
    {
      sec_half_v3<-rep(0,N-1)
      order<-order2 # ordering by pi
      xi<-c(0,cumsum(rep(1/N,N))) # x-axis is 1/N
      yi<-c(0,cumsum(pi[order])) # y-axis is pi (ha)
      
      xiv<-standard(xi,yi)$xiv
      yiv<-standard(xi,yi)$yiv
      
      for(k in 1:(N-1))
      {
        sec_half_v3[k]<-pi[order][k]*(N-k)
      }
      PE<-(1/N*(1/2*cumsum(pi[order])[N] + sum(sec_half_v3)))/(1/2*cumsum(pi[order])[N])
    }
    
    if(correct==TRUE)PE<-(PE-(1/N))*(N/(N-1))
    
    geom<-(yiv<=xiv)
    unsorted_slope<-sl(data.frame(x=xiv,y=yiv),2:(length(xiv)))
    
    if(plot_pe==TRUE)
    {
      par(mar=c(2.5,1,1,6))
      plot(yiv~xiv,xlim=c(0,max(xiv,na.rm=T)),ylim=c(0,max(yiv,na.rm=T)),type="b",xaxt="n",yaxt="n",bty="n",xaxs="i",yaxs="i",pch=19,ylab="",xlab="")
      axis(4,yiv,c("0",paste("y",c(1:N),sep="")),las=1)
      axis(1,xiv,c("0",paste("x",c(1:N),sep="")))
      slope<-(max(yiv)-0)/(max(xiv)-0)
      abline(0,slope,col="grey50")
    }
    
  }else{
    xiv<-yiv<-NA
  } 
  list(PE=PE,
       version=version,
       N=N,
       Geom=ifelse(is.na(geom[1]),NA,(ifelse(length(which(geom==FALSE))>0,"Bad- wrong geometry","Good- no problem with geometry"))),
       Slope=ifelse(is.na(unsorted_slope),NA,(ifelse(unsorted_slope==TRUE,"Bad- unsorted slopes!","Good-sorted slopes!"))),
       Sorted_stand_xaxis=xiv,
       Sorted_stand_yaxis=yiv)   
  
}


