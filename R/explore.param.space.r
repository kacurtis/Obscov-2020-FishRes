# Part 2: Variation of observer coverage recommendation across parameter space

## load packages
library(ObsCovgTools)

## define common parameters
vte <- seq(0.5,20,0.5)*1000
te <- 10000
vbpue <- seq(0.0001, 0.0101, 0.0002)
bpue <- 0.001
vd <- seq(1,5,0.1)
d <- 2

## Projections

### (a) Probability of observing any bycatch
target.ppos <- 95

#### (a)(i) Variation with total effort
df.vte.ppos <- data.frame(te = vte, pobs = NA, nobs = NA)
for (i in 1:nrow(df.vte.ppos)) {
  temp <- plot_probposobs(te=df.vte.ppos$te[i], bpue, d, silent=T, showplot=F)
  df.vte.ppos$pobs[i] <- temp$pobs
  df.vte.ppos$nobs[i] <- temp$nobs
}
rm(i, temp)

#### (a)(ii) Variation with bycatch rate
df.vbpue.ppos <- data.frame(bpue = vbpue, pobs = NA, nobs = NA)
for (i in 1:nrow(df.vbpue.ppos)) {
  temp <- plot_probposobs(te, bpue=df.vbpue.ppos$bpue[i], d, silent=T, showplot=F)
  df.vbpue.ppos$pobs[i] <- temp$pobs
  df.vbpue.ppos$nobs[i] <- temp$nobs
}
rm(i, temp)

#### (a)(iii) Variation with dispersion
df.vd.ppos <- data.frame(d = vd, pobs = NA, nobs = NA)
for (i in 1:nrow(df.vd.ppos)) {
  temp <- plot_probposobs(te, bpue=bpue, d=df.vd.ppos$d[i], silent=T, showplot=F)
  df.vd.ppos$pobs[i] <- temp$pobs
  df.vd.ppos$nobs[i] <- temp$nobs
}
rm(i, temp)


### (b) Probability Upper confidence limit
targetucl <- 2

#### (b)(i) Variation with total effort
df.vte.ucl <- data.frame(te = vte, pobs = NA, nobs = NA)
for (i in 1:nrow(df.vte.ucl)) {
  temp <- plot_uclnegobs(te=df.vte.ucl$te[i], d, targetucl=targetucl, silent=T, showplot=F)
  df.vte.ucl$pobs[i] <- temp$targetoc
  df.vte.ucl$nobs[i] <- temp$targetnoc
}
rm(i, temp)

#### (b)(ii) Variation with bycatch rate
# skip for ucl

#### (b)(iii) Variation with dispersion
df.vd.ucl <- data.frame(d = vd, pobs = NA, nobs = NA)
for (i in 1:nrow(df.vd.ucl)) {
  temp <- plot_uclnegobs(te, d=df.vd.ucl$d[i], targetucl=targetucl, silent=T, showplot=F)
  df.vd.ucl$pobs[i] <- temp$targetoc
  df.vd.ucl$nobs[i] <- temp$targetnoc
}
rm(i, temp)


### (c) Estimation CV
target.cv <- 0.3

#### (c)(i) Variation with total effort
df.vte.cv <- data.frame(te = vte, pobs = NA, nobs = NA)
for (i in 1:nrow(df.vte.cv)) {
  temp1 <- sim_cv_obscov(te=df.vte.cv$te[i], bpue, d)
  temp2 <- plot_cv_obscov(temp1, targetcv=target.cv, showplot=F, silent=T)
  df.vte.cv$pobs[i] <- temp2$pobs
  df.vte.cv$nobs[i] <- temp2$nobs
}
rm(i, temp1, temp2)

#### (c)(ii) Variation with bycatch rate
df.vbpue.cv <- data.frame(bpue = vbpue, obscov = NA, nobsets = NA)
for (i in 1:nrow(df.vbpue.cv)) {
  temp1 <- sim_cv_obscov(te=te, bpue=df.vbpue.cv$bpue[i], d)
  temp2 <- plot_cv_obscov(temp1, targetcv=target.cv, showplot=F, silent=T)
  df.vbpue.cv$pobs[i] <- temp2$pobs
  df.vbpue.cv$nobs[i] <- temp2$nobs
}
rm(i, temp1, temp2)

#### (c)(iii) Variation with dispersion
df.vd.cv <- data.frame(d = vd, obscov = NA, nobsets = NA)
for (i in 1:nrow(df.vd.cv)) {
  temp1 <- sim_cv_obscov(te=te, bpue=bpue, d=df.vd.cv$d[i])
  temp2 <- plot_cv_obscov(temp1, targetcv=target.cv, showplot=F, silent=T)
  df.vd.cv$pobs[i] <- temp2$pobs
  df.vd.cv$nobs[i] <- temp2$nobs
}
rm(i, temp1, temp2)

## save results
save(vte, te, vbpue, bpue, vd, d, 
     target.ppos, targetucl, target.cv, 
     df.vte.ppos, df.vbpue.ppos, df.vd.ppos,
     df.vte.ucl, df.vd.ucl,
     df.vte.cv, df.vbpue.cv, df.vd.cv,
     file = "param.space.rdata")

## clean up
rm(vte, te, vbpue, bpue, vd, d, 
   target.ppos, targetucl, target.cv, 
   df.vte.ppos, df.vbpue.ppos, df.vd.ppos,
   df.vte.ucl, df.vd.ucl,
   df.vte.cv, df.vbpue.cv, df.vd.cv)


#######################################

## plot

attach("param.space.rdata")

### open graphical device
tiff(filename="Fig4_ExploreParamSpace.tiff", width=17, height=15, units="cm", pointsize=9, 
     res=1000, compression="lzw")

### plot prep
opar=par()
par(mar=c(4.1, 2.5, 1, 2.5), mfcol=c(3,3), oma=c(0,2,0,2))

### (a)(i) Variation with total effort
plot(df.vte.ppos$te, df.vte.ppos$pobs, 
     pch=1,lwd=2, xlim=c(0,20000), ylim=c(0,100),xaxs="i", yaxs="i", 
     xlab = "Total Effort", ylab = "",
     cex.lab=1.5, cex.axis=1.3)
text(20000,10,"BPUE = 0.001, d=2",cex=1.5, pos=2)
par(new = T)
plot(df.vte.ppos$te, df.vte.ppos$nobs, 
     pch=1, lwd=2, col="gray70", xlim=c(0,20000), ylim=c(0,7000),xaxs="i", yaxs="i", 
     axes=F, xlab = "", ylab = "",
     cex.axis=1.3)
axis(side = 4, col="gray70", col.ticks="gray70", col.axis="gray70", cex.axis=1.3)
abline(v=20000, col="gray70")

### (a)(ii) Variation with bycatch rate
plot(df.vbpue.ppos$bpue, df.vbpue.ppos$pobs, 
     pch=1,lwd=2, xlim=c(0,0.01), ylim=c(0,100),xaxs="i", yaxs="i", 
     xlab = "Bycatch Per Unit Effort", ylab = "",
     cex.lab=1.5, cex.axis=1.3)
text(0.01,90,"TE = 10000, d=2",cex=1.5, pos=2)

### (a)(iii) Variation with dispersion
plot(df.vd.ppos$d, df.vd.ppos$pobs, 
     pch=1,lwd=2, xlim=c(1,5), ylim=c(0,100),xaxs="i", yaxs="i", 
     xlab = "Dispersion", ylab = "",
     cex.lab=1.5, cex.axis=1.3)
text(5,10,"TE = 10000, BPUE=0.001",cex=1.5, pos=2)


### (b)(i) Variation with total effort
plot(df.vte.ucl$te, df.vte.ucl$pobs, 
     pch=5,lwd=2, xlim=c(0,20000), ylim=c(0,100),xaxs="i", yaxs="i", 
     xlab = "Total Effort", ylab = "",
     cex.lab=1.5, cex.axis=1.3)
par(new = T)
plot(df.vte.ucl$te, df.vte.ucl$nobs, 
     pch=5, lwd=2, col="gray70", xlim=c(0,20000), ylim=c(0,18000),xaxs="i", yaxs="i", 
     axes=F, xlab = "", ylab = "",
     cex.axis=1.3)
axis(side = 4, col="gray70", col.ticks="gray70", col.axis="gray70", cex.axis=1.3)
text(20000,2000,"d=2",cex=1.5, pos=2)
abline(v=20000, col="gray70")

### (b)(ii) Variation with bycatch rate
# skip plot
plot(0,type='n',axes=FALSE,ann=FALSE)

### (b)(iii) Variation with dispersion
plot(df.vd.ucl$d, df.vd.ucl$pobs, 
     pch=5,lwd=2, xlim=c(1,5), ylim=c(0,100),xaxs="i", yaxs="i", 
     xlab = "Dispersion", ylab = "",
     cex.lab=1.5, cex.axis=1.3)
text(5,10,"TE = 10000",cex=1.5, pos=2)


### (c)(i) Variation with total effort
plot(df.vte.cv$te, df.vte.cv$pobs, 
     pch=2,lwd=2, xlim=c(0,20000), ylim=c(0,100),xaxs="i", yaxs="i", 
     xlab = "Total Effort", ylab = "",
     cex.lab=1.5, cex.axis=1.3)
par(new = T)
plot(df.vte.cv$te, df.vte.cv$nobs, 
     pch=2, lwd=2, col="gray70", xlim=c(0,20000), ylim=c(0,13000),xaxs="i", yaxs="i", 
     axes=F, xlab = "", ylab = "",
     cex.axis=1.3)
axis(side = 4, col="gray70", col.ticks="gray70",col.axis="gray70", cex.axis=1.3)
text(20000,1500,"BPUE = 0.001, d=2",cex=1.5, pos=2)
abline(v=20000, col="gray70")

### (c)(ii) Variation with bycatch rate
plot(df.vbpue.cv$bpue, df.vbpue.cv$pobs, 
     pch=2,lwd=2, xlim=c(0,0.01), ylim=c(0,100),xaxs="i", yaxs="i", 
     xlab = "Bycatch Per Unit Effort", ylab = "",
     cex.lab=1.5, cex.axis=1.3)
text(0.01,90,"TE = 10000, d=2",cex=1.5, pos=2)

### (c)(iii) Variation with dispersion
plot(df.vd.cv$d, df.vd.cv$pobs, 
     pch=2,lwd=2, xlim=c(1,5), ylim=c(0,100),xaxs="i", yaxs="i", 
     xlab = "Dispersion", ylab = "",
     cex.lab=1.5, cex.axis=1.3)
text(5,10,"TE = 10000, BPUE=0.001",cex=1.5, pos=2)

### outer axis labels
ylab1 = "Minimum Observer Coverage (%)"
ylab2 = "Minimum Effort to Observe"
mtext(side = 2, outer=TRUE, line = 0.8, ylab1)
mtext(side = 4, outer=TRUE, line = 0.8, adj = 1, ylab2, col="gray70")

dev.off()

# return to original plot parameters
par <- opar
