library(foreign)

f<-read.csv("fishdata.csv")

# a
  sum(f$MUSLIM)  # i
  sum(f$MUSLIM)/length(f$MUSLIM) # ii
  length(f$GDP90LGN[f$GDP90LGN>3]) # iii
  length(f$MUSLIM[f$MUSLIM==1 & f$BRITCOL==1]) # iv
  length(f$GDP90LGN[f$GDP90LGN>2.5 | f$GRW7598P>0.6]) # v 
  f.new <- f[f$MUSLIM==1 & f$OPEC==1,]  # vi

# b
  mean(f$FHREVERS[f$MUSLIM==1]) - mean(f$FHREVERS[f$MUSLIM==0])
  summary(lm(FHREVERS ~ MUSLIM, data=f))
  
# d
  t.test(f$FHREVERS[f$MUSLIM==1], f$FHREVERS[f$MUSLIM==0])
  
# e
  d <- mean(f$FHREVERS[f$MUSLIM==1]) - mean(f$FHREVERS[f$MUSLIM==0])
  
  se <-   sqrt( 
              var(f$FHREVERS[f$MUSLIM==1])/length(f$FHREVERS[f$MUSLIM==1]) +
              var(f$FHREVERS[f$MUSLIM==0])/length(f$FHREVERS[f$MUSLIM==0]) 
              )
  
  d/se
  
# f
  length(f$MUSLIM[f$BRITCOL==1 & f$MUSLIM==1]) / sum(f$MUSLIM)
  length(f$MUSLIM[f$BRITCOL==1 & f$MUSLIM==0]) / (length(f$MUSLIM)-sum(f$MUSLIM))
  
  cor(f$BRITCOL,f$FHREVERS)

# g  
  summary(lm(FHREVERS ~ MUSLIM + BRITCOL , data=f))
  
  
# h
  length(f$MUSLIM[f$OPEC==1 & f$MUSLIM==1]) / sum(f$MUSLIM)
  length(f$MUSLIM[f$OPEC==1 & f$MUSLIM==0]) / (length(f$MUSLIM)-sum(f$MUSLIM))
  cor(f$OPEC,f$FHREVERS)
                                               
  cor(f$GRW7598P,f$MUSLIM)
  cor(f$GRW7598P,f$FHREVERS)
  
  cor(f$GDP90LGN,f$MUSLIM)
  cor(f$GDP90LGN,f$FHREVERS)
  

# i
  summary(lm(FHREVERS ~ MUSLIM + BRITCOL + OPEC + GRW7598P + GDP90LGN , data=f))
  
  
  