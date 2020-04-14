load("benin.Rda")

# (a)
#regress treatment on the covariate
  summary(lm(treatment ~ reg.voters,data=b))


# (b) 
  summary(lm(vote.pop~treatment,data=b))
  
  # same
  t.test(b$vote.pop[b$treatment==1],b$vote.pop[b$treatment==0])

# (c)
  summary(lm(vote.pop ~ treatment + factor(block), data=b))


##################################################################

# Question 2

load("star.Rda")

# (a) 
  s$treat <- ifelse(s$gkclasstype=="SMALL CLASS",1,0)
  
# (b)
  t.test(s$gender[s$treat==1],s$gender[s$treat==0])
  t.test(s$race[s$treat==1],s$race[s$treat==0])
  t.test(s$gkfreelunch[s$treat==1],s$gkfreelunch[s$treat==0])

  
# (c)
  ate.reading1 <- lm(gktreadss ~ treat,data=s)
  summary(ate.reading1)$coef[2,]
  ate.reading2 <- lm(gktreadss ~ treat + factor(gkschid),data=s)
  summary(ate.reading2)$coef[2,]
  
  ate.maths1 <- lm(gktmathss ~ treat,data=s)
  summary(ate.maths1)$coef[2,]
  ate.maths2 <- lm(gktmathss ~ treat + factor(gkschid),data=s)
  summary(ate.maths2)$coef[2,]


# (d)
  s$missing <- ifelse( is.na(s$g1classtype) | is.na(s$g1treadss) | is.na(s$g1tmathss) ,1,0)
  sum(s$missing) / length(s$missing)
  
  s$missing <- ifelse( (is.na(s$g1classtype)) | (is.na(s$g1treadss) & is.na(s$g1tmathss)) ,1,0)
  sum(s$missing) / length(s$missing)
  
  
  
# (e)
  s$treat2 <- ifelse(s$g1classtype=="SMALL CLASS",1,0)
  
  ate.reading.p2 <- lm(g1treadss ~ treat2 + factor(gkschid),data=s)
  summary(ate.reading.p2)$coef[2,]
  
  ate.maths.p2 <- lm(g1tmathss ~ treat2 + factor(gkschid),data=s)
  summary(ate.maths.p2)$coef[2,]
  
  
   
# (f)  
  length(s$g1classtype[s$g1classtype=="SMALL CLASS" & s$gkclasstype=="REGULAR CLASS" &
                         !is.na(s$g1classtype)]) /
    length(s$gkclasstype[s$gkclasstype=="REGULAR CLASS"])
  
  length(s$g1classtype[s$g1classtype=="REGULAR CLASS" & s$gkclasstype=="SMALL CLASS" &
                         !is.na(s$g1classtype)]) /
    length(s$gkclasstype[s$gkclasstype=="SMALL CLASS"])
  
  
# (g)

  # Upper bound
    # start by creating a new dataset with new treatment variable, filling in missing values
  s.upper <- s
  s.upper$treat3 <- ifelse(is.na(s.upper$treat2),s.upper$treat,s.upper$treat2)
  
    # assign max score to treated units, min score to control units
  for(i in 1:length(s.upper$treat3)){
    if(is.na(s.upper$g1treadss[i]) & s.upper$treat3[i]==0){
      s.upper$g1treadss[i] <- min(na.omit(s$g1treadss))
      s.upper$g1tmathss[i] <- min(na.omit(s$g1tmathss))
    }
    
    else if(is.na(s.upper$g1treadss[i]) & s.upper$treat3[i]==1){
      s.upper$g1treadss[i] <- max(na.omit(s$g1treadss))
      s.upper$g1tmathss[i] <- max(na.omit(s$g1tmathss))
    } 
  }


  ate.reading.upper <- lm(g1treadss ~ treat3 + factor(gkschid),data=s.upper)
  summary(ate.reading.upper)$coef[2,]
  
  ate.maths.upper <- lm(g1tmathss ~ treat3 + factor(gkschid),data=s.upper)
  summary(ate.maths.upper)$coef[2,]
  
  
  # Lower bound
    # start by creating a new dataset with new treatment variable, filling in missing values
  s.lower <- s
  s.lower$treat3 <- ifelse(is.na(s.lower$treat2),s.lower$treat,s.lower$treat2)
  
    # assign min score to treated units, max score to control units
  for(i in 1:length(s.lower$treat3)){
    if(is.na(s.lower$g1treadss[i]) & s.lower$treat3[i]==0){
      s.lower$g1treadss[i] <- max(na.omit(s$g1treadss))
      s.lower$g1tmathss[i] <- max(na.omit(s$g1tmathss))
    }
    
    else if(is.na(s.lower$g1treadss[i]) & s.lower$treat3[i]==1){
      s.lower$g1treadss[i] <- min(na.omit(s$g1treadss))
      s.lower$g1tmathss[i] <- min(na.omit(s$g1tmathss))
    } 
  }
  
  ate.reading.lower <- lm(g1treadss ~ treat3 + factor(gkschid),data=s.lower)
  summary(ate.reading.lower)$coef[2,]
  
  ate.maths.lower <- lm(g1tmathss ~ treat3 + factor(gkschid),data=s.lower)
  summary(ate.maths.lower)$coef[2,]
  

  
  
