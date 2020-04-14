# Question 1

load("experiment.Rda")

# a
  mean(a$y1-a$y0)

# b
  set.seed(1)
  a$rand <- sample(c(1:100))
  a <- a[order(a$rand),]
  a$tr <- c(rep(1,50),rep(0,50))

# c
  t.test(a$y0[a$tr==1], a$y0[a$tr==0])

# d
  mean(a$y1[a$tr==1]) - mean(a$y0[a$tr==0])
  
# e
  experiment.sim <- function(a){
    a$rand <- sample(c(1:100))
    a <- a[order(a$rand),]
    a$tr <- c(rep(1,50),rep(0,50))
    
    mean(a$y1[a$tr==1]) - mean(a$y0[a$tr==0])
  }

  sims <- replicate(10000,experiment.sim(a))
  mean(sims)

# f
  experiment.sim2 <- function(a){
    a$rand <- sample(c(1:100))
    a <- a[order(a$rand),]
    a$tr <- c(rep(1,50),rep(0,50))
    
    mean(a$y0[a$tr==1] - a$y0[a$tr==0])
  }
  
  sims2 <- replicate(10000,experiment.sim2(a))
  mean(sims2)


#############################################
# Question 2

load("gerber.Rda")

# (a)

  mean(g$voting[g$civicduty==1]) - mean(g$voting[g$control==1])
  t.test(g$voting[g$civicduty==1], g$voting[g$control==1])

  mean(g$voting[g$neighbors==1]) - mean(g$voting[g$control==1])
  t.test(g$voting[g$neighbors==1], g$voting[g$control==1])
  

# (b)
  
  t.test(g$sex[g$civicduty==1],g$sex[g$control==1])
  t.test(g$yob[g$civicduty==1],g$yob[g$control==1])
  t.test(g$p2004[g$civicduty==1],g$p2004[g$control==1])
  
  t.test(g$sex[g$neighbors==1],g$sex[g$control==1])
  t.test(g$yob[g$neighbors==1],g$yob[g$control==1])
  t.test(g$p2004[g$neighbors==1],g$p2004[g$control==1])
  
 
# (c)
  
  # Subset to only neighbors and control observations
  g.reg <- g[g$neighbors==1|g$control==1,]
  
  #i)
  summary(lm(voting ~ neighbors,data=g.reg))
  
  #ii)
  summary(lm(voting ~ neighbors + sex + yob + p2004,data=g.reg))
  
  
  
