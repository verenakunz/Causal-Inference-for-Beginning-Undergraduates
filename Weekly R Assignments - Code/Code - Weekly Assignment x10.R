load("africa.Rda")
library(Synth)

# a) prepare data for synth

  mali.data <- dataprep(a,
                        predictors=c(#"lngdpmad",
                                     "lnpop",
                                     "ki",
                                     "openk",
                                     "civwar",
                                     "civwarend",
                                     "pwt_xrate",
                                     "eximdiff",
                                     "wbank",
                                     "imfadj"),
                         dependent="lngdpmad",
                         unit.variable="wbcode2",
                         unit.names.variable="Country",
                         time.variable="year",
                         treatment.identifier="Mali",
                         controls.identifier=c(unique(a$Country[a$Country!="Mali"])),
                         time.predictors.prior=c(1975:1990),
                         time.optimize.ssr=c(1975:1991),
                         time.plot=c(1975:2008))
  
  

# b) estimate 
  set.seed(123)
  synth.mali <- synth(mali.data)


# c) plot treatment vs. synthetic control
  pdf(file="ps8_1.pdf")
  path.plot(synth.res=synth.mali, dataprep.res=mali.data,
            tr.intake=1991,Ylab="Log GDP per capita",Xlab="")
  dev.off()

# d) describe which units are included in the control
  mali.weights <- synth.mali$solution.w

# e) describe which variables are included in the synthetic control estimate
  mali.vars <- synth.mali$solution.v

# f) plot the treatment effect (requires matrix algebra to get the synth fitted values)
  synthetic.mali <- mali.data$Y0plot %*% mali.weights
  actual.mali <- a$lngdpmad[a$Country=="Mali"&a$year>1974]
  
  treatment.effect <- actual.mali - synthetic.mali
  
  plot(treatment.effect,
       type="l",
       lwd=2,
       ylim=c(-0.5,1.2),
       xlab="",
       ylab="Log GDP per Capita",
       xaxt="n")
  axis(1,at=c(1,8,15,22,29),labels=c(1975,1983,1990,1997,2004))
  abline(v=16,lty=3)
  abline(h=0)

# g) do the synth again but for a control. 
  a.angola <- subset(a,a$Country!="Mali")
  angola.data <- dataprep(a.angola,
                        predictors=c("lngdpmad",
                                     "lnpop",
                                     "ki",
                                     "openk",
                                     "civwar",
                                     "civwarend",
                                     "pwt_xrate",
                                     "eximdiff",
                                     "wbank",
                                     "imfadj"),
                        dependent="lngdpmad",
                        unit.variable="wbcode2",
                        unit.names.variable="Country",
                        time.variable="year",
                        treatment.identifier="Angola",
                        controls.identifier=c(unique(a.angola$Country[a.angola$Country!="Angola"])),
                        time.predictors.prior=c(1975:1990),
                        time.optimize.ssr=c(1975:1991),
                        time.plot=c(1975:2008))
  
  synth.angola <- synth(angola.data)
  angola.weights <- synth.angola$solution.w
  
  synthetic.angola <- angola.data$Y0plot %*% angola.weights
  actual.angola <- a$lngdpmad[a$Country=="Angola" & a$year>1974]
  treatment.effect.angola <- actual.angola - synthetic.angola
  
  plot(treatment.effect,
       type="l",
       lwd=2,
       ylim=c(-0.5,1.2),
       xlab="",
       ylab="Log GDP per Capita",
       xaxt="n")
  axis(1,at=c(1,8,15,22,29),labels=c(1975,1983,1990,1997,2004))
  abline(v=16,lty=3)
  abline(h=0)
  lines(treatment.effect.angola,lty=2,lwd=1)
  legend("topright",c("Mali","Placebo"),lty=c(1,2))


  
# h) use a for loop combined with the unique() command to do it for all of them
  
  plot(treatment.effect,
       type="l",
       lwd=2,
       ylim=c(-0.5,1.2),
       xlab="",
       ylab="Log GDP per Capita",
       xaxt="n")
  axis(1,at=c(1,8,15,22,29),labels=c(1975,1983,1990,1997,2004))
  abline(v=16,lty=3)
  abline(h=0)
  legend("topright",c("Mali","Placebo"),lty=c(1,2))
  
  
  a.controls <- subset(a,a$Country!="Mali")
  for(i in 1:length(unique(a.controls$Country))){
    my.country <- unique(a.controls$Country)[i]
    
    controls.data <- dataprep(a.controls,
                            predictors=c("lngdpmad",
                                         "lnpop",
                                         "ki",
                                         "openk",
                                         "civwar",
                                         "civwarend",
                                         "pwt_xrate",
                                         "eximdiff",
                                         "wbank",
                                         "imfadj"),
                            dependent="lngdpmad",
                            unit.variable="wbcode2",
                            unit.names.variable="Country",
                            time.variable="year",
                            treatment.identifier=my.country,
                            controls.identifier=c(unique(a.controls$Country[a.controls$Country!=my.country])),
                            time.predictors.prior=c(1975:1990),
                            time.optimize.ssr=c(1975:1991),
                            time.plot=c(1975:2008))
    
    synth.control <- synth(controls.data)
    control.weights <- synth.control$solution.w
    
    synthetic.control <- controls.data$Y0plot %*% control.weights
    actual.control <- a$lngdpmad[a$Country==my.country & a$year>1974]
    treatment.effect.control <- actual.control - synthetic.control
    
    lines(treatment.effect.control,lty=2,lwd=1)
  }

  
