## Question 1 ##

library("ri")
load("t4data.Rda")

# a): ATE, report p-value
  reg1 <- lm(Y~tr,data=a)
  summary(reg1)
  ate <- reg1$coef[2]

# b) 
  perms <- genperms(a$tr)

# c) 
  ates <- c()
  for(i in 1:ncol(perms)){
    ates[i] <- mean(a$Y[perms[,i]==1]) - mean(a$Y[perms[,i]==0])
  }

# d) calculate exact p-value: two-tailed
  (length(ates[ates<=ate]) + length(ates[ates>=-ate])) / ncol(perms)  

# e) histogram
  plot(density(ates),
       xlab="Average Treatment Effects",
       main="Distribution of ATEs Under the\n Sharp Null Hypothesis")
  abline(v=ate,col="red",lwd=2)
  abline(v=quantile(ates,0.025),col="blue",lty=2)
  abline(v=quantile(ates,0.975),col="blue",lty=2)
  legend("topright",c("ATE","Critical\n Values"),lty=c(1,2),col=c("red","blue"),ncol=1)


# f) using software
  Y_null <- genouts(a$Y,a$tr,ate=0)
  distout <- gendist(Y_null,perms)
  dispdist(distout,ate)

# g) 
  reg2 <- lm(Y~tr + factor(block),data=a)
  summary(reg2)
  ate <- reg2$coef[2]

# h) how many perms now?
  perms <- genperms(a$tr,blockvar=a$block)

# i) using software: what do you notice about sampling dist compared to before
  Y_null <- genouts(a$Y,a$tr,ate=0)
  distout <- gendist(Y_null,perms)
  dispdist(distout,ate)
  









