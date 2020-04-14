library(foreign)
library(lmtest)
library(multiwayvcov)
library(plm)

##### Q1: Malesky #######
load("malesky.Rda")

# (a) create the time dummy and the interaction term for diff in diff
  m3 <- m[m$year>2006,]
  m3$time <- ifelse(m3$year==2010,1,0)
  
  m3$time_treat <- m3$time*m3$treatment

# (b)  do diff-in-diff, no covariates
  summary(lm(infra ~ time + treatment + time_treat,data=m3))

# (c) diff-in-diff, covariates
  model1 <- lm(infra ~ time + treatment + time_treat + lnpopden,data=m3)
  summary(model1)

# (d) cluster the standard error at the district level
  coeftest(model1, cluster.vcov(model1,m3$district))

# (f) parallel trends pic
  
    # means, treated
  means.t <- c(mean(m$infra[m$year==2006&m$treatment==1]),
  mean(m$infra[m$year==2008&m$treatment==1]),
  mean(m$infra[m$year==2010&m$treatment==1]))
  
    #means,control
  means.c <- c(mean(m$infra[m$year==2006&m$treatment==0]),
  mean(m$infra[m$year==2008&m$treatment==0]),
  mean(m$infra[m$year==2010&m$treatment==0]))
  
    # plot
  plot(means.t,
       ylim=c(2.6,3.6),
       type="o",
       pch=16,
       col="red",
       xaxt="n",
       xlab="Year",
       ylab="Infrastructure Index")
  lines(means.c,type="o",pch=15,col="blue")
  axis(1,at=c(1,2,3),lab=c(2006,2008,2010))
  legend("topleft",
         c("Treated","Control"),
         col=c("red","blue"),
         pch=c(16,15),
         lty=c(1,1))

# (g) placebo diff-in-diff [can't cluster because district IDs aren't provided pre the shift]
  m4 <- m[m$year<2010,]
  m4$time <- ifelse(m4$year==2008,1,0)
  m4$time_treat <- m4$time*m4$treatment
  
  summary(lm(infra ~ time + treatment + time_treat + lnpopden ,data=m4))

  
  
###############################
  # Fixed effects Question 2

  load("Stokes_fe.Rda")
  
# b) 
  mod_fe1 <- plm(perc_lib ~ op + factor(master_id)*year, data = s,
                index = c("master_id", "year"),effect = "twoways")
  coeftest(mod_fe1, vcov=vcovHC(mod_fe1, cluster="group", type="HC1"))
  
  # probably better to include all covariates (all vary over time)
  mod_fe2 <- plm(perc_lib ~ op + p_uni_degree + log_pop_denc + unemploy_rate + log_median_inc + p_immigrant, 
                data = s, index = c("master_id", "year"), effect = "twoways")
  coeftest(mod_fe2, vcov=vcovHC(mod_fe2, cluster="group", type="HC1"))

# d)
  # means, treated
  means.t <- c(mean(s$perc_lib[s$year==2003&s$treat_o==1]),
               mean(s$perc_lib[s$year==2007&s$treat_o==1]),
               mean(s$perc_lib[s$year==2011&s$treat_o==1]))
  
  #means,control
  means.c <- c(mean(s$perc_lib[s$year==2003&s$treat_o==0]),
               mean(s$perc_lib[s$year==2007&s$treat_o==0]),
               mean(s$perc_lib[s$year==2011&s$treat_o==0]))
  
  # plot
  plot(means.t,
       ylim=c(0.2,0.6),
       type="o",
       pch=16,
       col="red",
       xaxt="n",
       xlab="Year",
       ylab="Incumbent Vote Share")
  lines(means.c,type="o",pch=15,col="blue")
  axis(1,at=c(1,2,3),lab=c(2003,2007,2011))
  legend("topright",
         c("Treated","Control"),
         col=c("red","blue"),
         pch=c(16,15),
         lty=c(1,1))

  
  
  
  
  
  
  
  
  
  
  

