# Part 3: Case study with CDGN protected species

## load packages
library(tidyverse)
library(magrittr)
library(ObsCovgTools)

## import and edit CDGN data (omit set.type, PingTy, Envr, Ornt, Dist)
### import data
dat.dgn <- read_csv("../../../data/DGN.Current.Data.csv", 
                    col_types="diddddddcc-iiii---iiiiiiidiiiiiiidiiiiiiiii-iidddciiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii--") 
### remove columns for species with zero bycatch, add date column
dat.dgn %<>% arrange(year,trip,Set) %>% 
  gather(key="Sp",value="Num",BA:UC,factor_key=TRUE) %>%
  group_by(Sp) %>% 
  mutate(spsum=sum(Num)) %>% 
  ungroup() %>% 
  filter(spsum>0) %>% 
  droplevels() %>% 
  mutate(date=as.POSIXct(paste(year,mm,dd,sep="-")))
### select years post-DC closure, post-pinger (2002-2016), drop non-specific ID
dat.dgn.filter <- dat.dgn %>% filter(year>=2002 & !(Sp %in% c("aSP","aVE","UW")))
### summary stats, select species with non-zero bycatch for this period
dat.dgn.summary2 <- dat.dgn.filter %>% group_by(Sp, trip) %>% 
  summarize(tnum=sum(Num)) %>% ungroup() %>% group_by(Sp) %>% 
  summarize(num=sum(tnum), tx=mean(tnum), tv=var(tnum), tv2tx=tv/tx, n=n()) %>% 
  filter(!is.na(tv2tx)) %>% mutate(Sp=as.character(Sp))
### match species codes to names
sptable <- read_csv("../../../data/CDGN SpNames table.txt")
dat.dgn.summary %<>% left_join(sptable, by = c("Sp" = "spcode"))
rm(sptable)
### add PBR for each species to table
df.pbr <- read_csv("PBR table.csv")
dat.dgn.summary %<>%  left_join(df.pbr, by = "spname")
rm(df.pbr)

# how many sets in a trip?
length(unique(dat.dgn.filter$trip.set))/length(unique(dat.dgn.filter$trip))  #[1] 5.7
# convert typical effort in sets to trips
500/6   # 83 trips/yr

## Estimate target observer coverages
### set parameters
yrs <- c(1,5,10)
te <- yrs*80   # effort in one, five, ten years
### edit var2mean ratio to be at least one
dat.dgn.summary %<>% mutate(tv2tx = ifelse(tv2tx<1, 1, tv2tx))
### initialize results columns
dat.dgn.summary %<>% mutate(pobs.ppos.1 = NA, nobs.ppos.1 = NA, pobs.ucl.1 = NA, nobs.ucl.1 = NA, pobs.cv.1 = NA, nobs.cv.1 = NA,
                            pobs.ppos.5 = NA, nobs.ppos.5 = NA, pobs.ucl.5 = NA, nobs.ucl.5 = NA, pobs.cv.5 = NA, nobs.cv.5 = NA,
                            pobs.ppos.10 = NA, nobs.ppos.10 = NA, pobs.ucl.10 = NA, nobs.ucl.10 = NA, pobs.cv.10 = NA, nobs.cv.10 = NA)

for (i in 1:nrow(dat.dgn.summary)) {
  # Conditional probability of observing bycatch
  ## one year
  temp <- plot_probposobs(te=te[1], bpue=dat.dgn.summary$tx[i], d=dat.dgn.summary$tv2tx[i], silent=T, showplot=F)
  dat.dgn.summary$pobs.ppos.1[i] <- temp$pobs
  dat.dgn.summary$nobs.ppos.1[i] <- temp$nobs
  ## five years
  temp <- plot_probposobs(te=te[2], bpue=dat.dgn.summary$tx[i], d=dat.dgn.summary$tv2tx[i], silent=T, showplot=F)
  dat.dgn.summary$pobs.ppos.5[i] <- temp$pobs
  dat.dgn.summary$nobs.ppos.5[i] <- temp$nobs
  ## ten years
  temp <- plot_probposobs(te=te[3], bpue=dat.dgn.summary$tx[i], d=dat.dgn.summary$tv2tx[i], silent=T, showplot=F)
  dat.dgn.summary$pobs.ppos.10[i] <- temp$pobs
  dat.dgn.summary$nobs.ppos.10[i] <- temp$nobs
  # Upper confidence limit given no bycatch observed
  ## one year
  temp <- plot_uclnegobs(te=te[1], d=dat.dgn.summary$tv2tx[i], targetucl=yrs[1]*dat.dgn.summary$pbr[i], silent=T, showplot=F)
  dat.dgn.summary$pobs.ucl.1[i] <- temp$targetoc
  dat.dgn.summary$nobs.ucl.1[i] <- temp$targetnoc
  ## five years
  temp <- plot_uclnegobs(te=te[1], d=dat.dgn.summary$tv2tx[i], targetucl=yrs[2]*dat.dgn.summary$pbr[i], silent=T, showplot=F)
  dat.dgn.summary$pobs.ucl.5[i] <- temp$targetoc
  dat.dgn.summary$nobs.ucl.5[i] <- temp$targetnoc
  ## ten years
  temp <- plot_uclnegobs(te=te[1], d=dat.dgn.summary$tv2tx[i], targetucl=yrs[3]*dat.dgn.summary$pbr[i], silent=T, showplot=F)
  dat.dgn.summary$pobs.ucl.10[i] <- temp$targetoc
  dat.dgn.summary$nobs.ucl.10[i] <- temp$targetnoc
  # Bycatch estimation CV
  ## one year
  temp1 <- sim_cv_obscov(te=te[1], bpue=dat.dgn.summary$tx[i], d=dat.dgn.summary$tv2tx[i])
  temp2 <- plot_cv_obscov(temp1, showplot=F, silent=T)
  dat.dgn.summary$pobs.cv.1[i] <- temp2$pobs
  dat.dgn.summary$nobs.cv.1[i] <- temp2$nobs
  ## five years
  temp1 <- sim_cv_obscov(te=te[2], bpue=dat.dgn.summary$tx[i], dat.dgn.summary$tv2tx[i])
  temp2 <- plot_cv_obscov(temp1, showplot=F, silent=T)
  dat.dgn.summary$pobs.cv.5[i] <- temp2$pobs
  dat.dgn.summary$nobs.cv.5[i] <- temp2$nobs
  ## ten years
  temp1 <- sim_cv_obscov(te=te[3], bpue=dat.dgn.summary$tx[i], dat.dgn.summary$tv2tx[i])
  temp2 <- plot_cv_obscov(temp1, showplot=F, silent=T)
  dat.dgn.summary$pobs.cv.10[i] <- temp2$pobs
  dat.dgn.summary$nobs.cv.10[i] <- temp2$nobs
}
rm(i, temp, temp1, temp2)

## arrange table
dat.dgn.summary %<>% arrange(tx, tv2tx, pbr, substring(dat.dgn.summary$spname,1,1), substring(dat.dgn.summary$spname,2,2))

dat.dgn.summary.ppos <- dat.dgn.summary %>% select(Sp, num, tx, tv2tx, pbr, 
                                                   pobs.ppos.1, nobs.ppos.1, pobs.ppos.5, nobs.ppos.5, pobs.ppos.10, nobs.ppos.10) %>% 
  mutate(tx=round(tx, 3), tv2tx=round(tv2tx, 1), OC.ppos.1=paste0(round(pobs.ppos.1), " (", nobs.ppos.1, ")"),
         OC.ppos.5=paste0(round(pobs.ppos.5), " (", nobs.ppos.5, ")"),
         OC.ppos.10=paste0(round(pobs.ppos.10), " (", nobs.ppos.10, ")")) %>% 
  select(-pobs.ppos.1, -nobs.ppos.1, -pobs.ppos.5, -nobs.ppos.5, -pobs.ppos.10, -nobs.ppos.10)
write.csv(dat.dgn.summary.ppos, file="DGNsummary_ppos.csv", row.names=F)

dat.dgn.summary.ucl <- dat.dgn.summary %>% select(Sp, num, tx, tv2tx, pbr, 
                                                   pobs.ucl.1, nobs.ucl.1, pobs.ucl.5, nobs.ucl.5, pobs.ucl.10, nobs.ucl.10) %>% 
  mutate(tx=round(tx, 3), tv2tx=round(tv2tx, 1), OC.ucl.1=paste0(round(pobs.ucl.1), " (", nobs.ucl.1, ")"),
         OC.ucl.5=paste0(round(pobs.ucl.5), " (", nobs.ucl.5, ")"),
         OC.ucl.10=paste0(round(pobs.ucl.10), " (", nobs.ucl.10, ")")) %>% 
  select(-pobs.ucl.1, -nobs.ucl.1, -pobs.ucl.5, -nobs.ucl.5, -pobs.ucl.10, -nobs.ucl.10)
write.csv(dat.dgn.summary.ucl, file="DGNsummary_ucl.csv", row.names=F)

dat.dgn.summary.cv <- dat.dgn.summary %>% select(Sp, num, tx, tv2tx, pbr, 
                                                  pobs.cv.1, nobs.cv.1, pobs.cv.5, nobs.cv.5, pobs.cv.10, nobs.cv.10) %>% 
  mutate(tx=round(tx, 3), tv2tx=round(tv2tx, 1), OC.cv.1=paste0(round(pobs.cv.1), " (", nobs.cv.1, ")"),
         OC.cv.5=paste0(round(pobs.cv.5), " (", nobs.cv.5, ")"),
         OC.cv.10=paste0(round(pobs.cv.10), " (", nobs.cv.10, ")")) %>% 
  select(-pobs.cv.1, -nobs.cv.1, -pobs.cv.5, -nobs.cv.5, -pobs.cv.10, -nobs.cv.10)
write.csv(dat.dgn.summary.cv, file="DGNsummary_cv.csv", row.names=F)

## save results
save(dat.dgn.summary, te, file="case.study.rdata")
