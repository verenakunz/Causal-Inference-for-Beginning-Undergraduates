load("Stokes.Rda")
library(lmtest)
library(AER)

# a)  
  summary(lm(avg_pwr_log ~ factor(ed_id) + longitude + latitude + mindistlake + mindistlake_sq,data=s))

# b)
  summary(lm(prop_3km ~ avg_pwr_log, data=s))
  
# d)
  summary(lm(prop_3km ~ avg_pwr_log + factor(ed_id) + longitude + latitude + mindistlake + mindistlake_sq , data=s))

# e)
  mod1 <- lm(prop_3km ~ longitude + latitude + mindistlake + mindistlake_sq + as.factor(ed_id), data=s) # Run one model without the instrument.
  mod2 <- lm(prop_3km ~ avg_pwr_log + longitude + latitude + mindistlake + mindistlake_sq + as.factor(ed_id), data=s) # Run a second model with the instrument.
  waldtest(mod2, mod1) #Compare the two using a wald test

# f)
  summary(ivreg(chng_lib ~ prop_3km + mindistlake + mindistlake_sq + longitude + latitude + as.factor(ed_id) 
                | avg_pwr_log + mindistlake + mindistlake_sq + longitude + latitude + as.factor(ed_id), data = s))

# g)
  first.stage <- lm(prop_3km ~ avg_pwr_log + factor(ed_id) + longitude + latitude + mindistlake + mindistlake_sq , data=s)
  s$fitted.first <- fitted.values(first.stage)
  
  summary(lm(chng_lib ~ fitted.first + factor(ed_id) + longitude + latitude + mindistlake + mindistlake_sq , data=s))





