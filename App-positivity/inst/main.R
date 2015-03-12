###############################################################################
# Description: Master file that calls in all other functions 
# 
# Author: Linh Tran <tranlm@berkeley.edu>
# Title: Handling positivity violations in longitudinal data
# Date: 2015-01-08
###############################################################################
rm(list = ls())

#################
## DESCRIPTION ##
#################
# This study simulates positivity violations in a longitudinal setting and tries 
# out four different approaches at handling it: 
#
# 1) Choose regimens where not a problem (eg switch immediately or never) 
# 2) Dynamic regimes, eg switch as soon as you come in after your assigned time
# 3) Joint (partially stochastic) intervention on both 
#	(i.e. leave visits random until time of assigned switch and then force someone to come in)
# 4) Don't adjust for the covariate causing the problem (ie dont adjust for whether a patient comes in) 
#	and could show that this might or might not cause a problem depending on whether coming to 
#	clinic has a direct effect on outcome not via switch.


#############
## OPTIONS ##
#############
ncores = 8
setwd("~/Dropbox/Studies/positivity")
options("mc.cores" = ncores, stringsAsFactors=FALSE, digits=4)
words = function(...) paste(substitute(list(...)))[-1]


#############
## LIBRARY ##
#############
load.packages = words(foreach, iterators, snow, doSNOW)
lapply(load.packages, require, character.only=T)


#########################
## SIMULATION SETTINGS ##
#########################
n = 1000
sim = 1000
time.pt = 10
ZaffectsY = TRUE


########################
## TRUE DISTRIBUTIONS ##
########################
#Time ordering: W, Y(t), L(t), Z(t), A(t), C(t) : W=(W1,W2) and L(t) = (L2(t),L1(t))
#n.b. Within L(t) there is no implied time-ordering...i.e. either of L2(t) or L1(t) can go first
rexpit  = function(x) rbinom(n=length(x), size=1, prob=x)
QW1     = function(n) rnorm(n, mean=0, sd=1)
QW2     = function(n) rep(plogis(.2), n)
QL1.t   = function(y, 	   w1, 	   prev_l1, prev_l2, prev_a) ifelse(y==1, prev_l1, 0.1 + 0.4*w1 + 0.6*prev_l1 - 0.7*prev_l2 - 0.45*prev_a - rnorm(length(w1), sd=0.5))
QL2.t   = function(y, 	   w1, w2, prev_l1, prev_l2, prev_a) ifelse(y==1, prev_l2, -0.55 + 0.5*w1 + 0.75*w2 + 0.1*prev_l1 + 0.3*prev_l2 - 0.75*prev_a - rnorm(length(w1), sd=0.5))
gZ.t	= function(y, 	   w1, w2, l1, l2, prev_a) 			ifelse(y==1, 0, plogis(2.8 - 0.5*w1 + 0.6*w2 + 0.7*l1 + 0.7*l2))
gA.t    = function(y, 	   w1, w2, l1, l2, prev_a, z) 		ifelse(y==1, prev_a, ifelse(z==0, prev_a, ifelse(prev_a==1, 1, plogis(-1.5 - 1.5*w1 + 0.75*w2 + 0.8*l1 + 0.8*l2))))
if(ZaffectsY) {
	QY.t    = function(prev_y, w1, w2, prev_l1, prev_l2, prev_a, prev_z) ifelse(prev_y==1, 1, plogis(-1.8 + 1.2*w1 - 2.4*w2 - 1.6*prev_l1 - 1*prev_l2 - 1.9*prev_a - 1.25*prev_z))
} else {
	QY.t    = function(prev_y, w1, w2, prev_l1, prev_l2, prev_a, prev_z) ifelse(prev_y==1, 1, plogis(-1.8 + 1.2*w1 - 2.4*w2 - 1.6*prev_l1 - 1*prev_l2 - 1.9*prev_a + 0*prev_z))
}
# nb. Distribution is set up such that: 
#	Y(0)=0 for everyone, ie. Everyone is alive at the beginning of follow-up
# 	if Y(t)=1, then they don't come in for a visit (ie. Z(t)=0))
#	if Y(t)=1, then all remaining covariate last values get carried forward
#	if Z(t)=0, then A(t-1) gets carried forward
#	if A(t-1)=1 then A(t)=1 


#############
## FOLDERS ##
#############
subdirectories = list.dirs()
if(!"./data" %in% subdirectories) system("mkdir ./data")
if(!"./inst" %in% subdirectories) system("mkdir ./inst") 
if(!"./inst/results" %in% subdirectories) system("mkdir ./inst/results") 


###########
## CODES ##
###########
source("./R/generateData.R")
source("./R/generatePsi.R")
source("./R/plotPsi.R")


###############################################################################
############################# TRUE PARAMETER VALUES ###########################
###############################################################################

skip = function() {

## STARTS PARALLEL CLUSTER ##
set.seed(1989)
superman <- makeCluster(ncores, type="SOCK")
registerDoSNOW(superman)
clusterExport(cl = superman, c("time.pt", "generate.data", "rexpit", "QW1", "QW2", "QL1.t", "QL2.t", "gZ.t", "gA.t", "QY.t"))
cat("Cluster summary\n"); getDoParRegistered(); getDoParName(); getDoParWorkers(); getDoParVersion()

## CALCULATES TRUE VALUES ##
truePsi = generatePsi()
save(truePsi, file=paste0("./data/truePsi_", ifelse(ZaffectsY,"ZaffectsY","noZaffectsY"), ".Rda"))

## STOPS CLUSTER ##
stopCluster(superman)

#######################
## PLOTS TRUE VALUES ##
#######################
pdf(paste0("./inst/results/truePsi_", ifelse(ZaffectsY,"ZaffectsY","noZaffectsY"), ".pdf"), height=6, width=6)
plotPsi(main=ifelse(ZaffectsY,"(a)","(b)"))
dev.off()

}

## LOADS PREVIOUSLY COMPUTED/SAVED VALUES ##
load(file=paste0("./data/truePsi_", ifelse(ZaffectsY,"ZaffectsY","noZaffectsY"), ".Rda"))



###############################################################################
################################## SIMULATION #################################
###############################################################################
set.seed(1)







