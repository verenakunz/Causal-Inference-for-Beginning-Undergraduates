# Question 1

load("guangreen.Rda")

# a) 
  # assigned to control, contacted
  length(g$contact[g$contact==1&g$treat2==0])
  # assigned to treatment, not contacted
  length(g$contact[g$contact==0&g$treat2==1])

# c) 
  prop.c <- sum(g$contact)/sum(g$treat2)

# d) 
  itt <- mean(g$turnout[g$treat2==1]) - mean(g$turnout[g$treat2==0])

# e) 
  cace <- itt/prop.c


#############################################################

### QUESTION 2: HAJJ PILGRIMAGE ###

load("hajjdata.Rda")

# (a)
  # assigned to control, went on hajj
  length(h$hajj2006[h$success==0&h$hajj2006==1])/length(h$hajj2006[h$success==0])
  # assigned to treatment, didn't go on hajj
  length(h$hajj2006[h$success==1&h$hajj2006==0])/length(h$hajj2006[h$success==1])

# (c)
  itt <- mean(h$moderacy[h$success==1]) - mean(h$moderacy[h$success==0])
  
  prop.c <- sum(h$hajj2006[h$success==1])/length(h$hajj2006[h$success==1]) - 
            sum(h$hajj2006[h$success==0])/length(h$hajj2006[h$success==0])
  
  cace <- itt/prop.c

# (d)
  summary(lm(h$hajj2006 ~ h$success))

# (e)
  library(AER)
  summary(ivreg(moderacy ~ hajj2006 | success,data=h))

# (f)
  summary(ivreg(moderacy ~ hajj2006+age+literate+urban | success+age+literate+urban,data=h))







