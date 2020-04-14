library(foreign)
library(rdd)

m <- read.csv("islamic.csv")

# a
  m$islamicwin<-ifelse(m$iwm94>=0,1,0)

# b
  mean(m$hischshr1520f[m$islamicwin==1],na.rm=T) - 
  mean(m$hischshr1520f[m$islamicwin==0],na.rm=T)
  
# c
  band <- IKbandwidth(m$iwm94, m$hischshr1520f) 

# d
  rdd_est <- m[m$iwm94<band & m$iwm94>-band,]

# e
  summary(lm(hischshr1520f~iwm94 + islamicwin, data=rdd_est))
  
# f
  rdest <- RDestimate(hischshr1520f~iwm94,bw=band, data=m)  
  summary(rdest)

# g
  plot(rdest, range=c(-0.6,0.6))
  abline(v=0)

# h
  summary(RDestimate(hischshr1520f~iwm94,bw=band,cutpoint=-0.1,data=m)) 
  summary(RDestimate(hischshr1520f~iwm94,bw=band,cutpoint=-0.05,data=m))
  summary(RDestimate(hischshr1520f~iwm94,bw=band,cutpoint= 0.05,data=m))
  summary(RDestimate(hischshr1520f~iwm94,bw=band,cutpoint= 0.1,data=m))


# i
  summary(RDestimate(sexr~iwm94,bw=band, data=m))
  summary(RDestimate(lpop1994~iwm94,bw=band, data=m))
  summary(RDestimate(lareapre~iwm94,bw=band, data=m))
  
# j
  DCdensity(m$iwm94,verbose=T)
  abline(v=0)
  
# k
  rdests=rdci.up=rdci.down <- c()
  thresholds <- seq(from=0.05,to=0.6,by=0.005)
  
  for(i in 1:length(thresholds)){
    rdest <- RDestimate(hischshr1520f~iwm94,bw=thresholds[i], data=m)
    rdests[i] <- rdest$est[1]
    rdci.up[i] <- rdests[i] + 1.96*rdest$se[1]
    rdci.down[i] <- rdests[i] -1.96*rdest$se[1]
  }
  
  plot(rdests,
       type="l",
       lwd=2,
       ylim=c(-0.1,0.1),
       xaxt="n",
       xlab="Threshold",
       ylab="Estimate")
  axis(1,
       at=c(1,31,51,71,91,111),
       labels=c(0.05,0.2,0.3,0.4,0.5,0.6))
  abline(h=0)
  lines(rdci.up,
        lty=3)
  lines(rdci.down,
        lty=3)
  legend("topright",
         c("RD Estimate","95% Confidence Interval"),
         lty=c(1,3))

       
  
  
  
  
