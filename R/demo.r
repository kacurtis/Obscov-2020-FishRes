# Part 1: Demo of tool

## load packages
library(ObsCovgTools)

## Demo plots

### (a) Probability of observing any bycatch

#### plot output
tiff(filename="Fig1_demo_ppos.tiff", width=120, height=75, units="mm", pointsize=9, 
     res=1000, compression="lzw")   # resize to 65% for 85mm across
opar=par()
par(mar=c(4, 4, 2, 1), cex.main=1)
plot_probposobs(te = 10000, bpue = 0.0005, d = 2)
dev.off()
par <- opar

#### text output
#The probability that any bycatch occurs in the given total effort is 96.9%.
#Minimum observer coverage to achieve at least 95% probability of observing 
#bycatch when total bycatch is positive is 73% (7300 trips or sets).
#Please review the caveats in the associated documentation.


### (b) Upper confidence limit given no bycatch observed

#### plot output
tiff(filename="Fig2_demo_ucl.tiff", width=120, height=75, units="mm", pointsize=9,
     res=1000, compression="lzw")
opar = par()
par(mar=c(4, 4, 2, 1), cex.main=1)
plot_uclnegobs(te = 10000, d = 2, targetucl = 2, fixedoc = 20)
dev.off()
par <- opar

#### text output
#Minimum observer coverage to ensure that the upper confidence limit of 2 is not exceeded when no bycatch is observed is 84.8% (8470 trips or sets).
#Upper confidence limit for bycatch given none observed in 20% (2000 trips or sets) coverage is 19.4.

### (c) Estimation CV
simlist <- sim_cv_obscov(te = 10000, bpue = 0.0005, d = 2)

#### plot output for obscov vs cv
tiff(filename="Fig3_demo_cv.tiff", width=120, height=75, units="mm", pointsize=9, 
     res=1000, compression="lzw")   # resize to 65% for 85mm across
opar=par()
par(mar=c(4, 4, 2, 1), cex.main=1)
plot_cv_obscov(simlist, targetcv = 0.3)
dev.off()
par <- opar

#### text output
#Minimum observer coverage to achieve CV ≤ 0.3 is 82% (8200 trips or sets).
#Please review the caveats in the associated documentation.
#Note that results are interpolated from simulation-based projections and may vary slightly 
#with repetition.
