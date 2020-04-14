library(MatchIt)
library(Matching)

ldata <- MatchIt::lalonde


# (a) 
m1 <- lm(re78 ~ treat +hispan + educ + married, data = ldata)
summary(m1) 

# (b) 
matching.vars1 <- cbind(ldata$hispan,ldata$educ, ldata$married)
m1 <- Match(Y=ldata$re78, Tr=ldata$treat, X=matching.vars1, exact=TRUE)
summary.Match(m1)

# (c)
mbal1  <- MatchBalance(treat ~ hispan + educ + married, match.out=m1, data=ldata)

# (d)
matching.vars2 <- cbind(ldata$hispan,ldata$educ, ldata$married, ldata$re74, ldata$re75)
m2 <- Match(Y=ldata$re78, Tr=ldata$treat, X=matching.vars2,Weight=2)
summary.Match(m2)
mbal2  <- MatchBalance(treat ~ hispan + educ + married + re74 + re75, match.out=m2, data=ldata)


# (e)
propscores.reg <- glm(treat~age+educ+black+hispan+married+nodegree+re74+re75, family=binomial, data=ldata)
propscores <- fitted.values(propscores.reg)   
ldata <- cbind(ldata,propscores)

# (f)
m3 <- Match(Y=ldata$re78, Tr=ldata$treat, X=propscores,Weight=2)
summary.Match(m3)
mbal3  <- MatchBalance(treat ~ age+educ+black+hispan+married+nodegree+re74+re75, match.out=m3, data=ldata)


# (g)
m4 <- Match(Y=ldata$re78, Tr=ldata$treat, X=propscores,Weight=2,ties=F)
summary.Match(m4)

ldata.tr <- ldata[m4$index.treated,]
ldata.con <- ldata[m4$index.control,]
ldata.reg <- rbind(ldata.tr,ldata.con)

summary(lm(re78 ~ treat + propscores,data=ldata.reg))







