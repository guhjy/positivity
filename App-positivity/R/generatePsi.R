###############################################################################
# Description: Generates true parameter values
# 
# Author: Linh Tran <tranlm@berkeley.edu>
# Date: January 9, 2015
###############################################################################


generatePsi = function(n.psi = 1000000) {

	###############
	## OPTION #1 ##
	###############
	# Go for regimes not affected (and possibly interpolate)
	cat("Generating psi for Option #1\n")
	O.abar_t0 = foreach(n = rep(n.psi, 8), .combine = rbind) %dopar%
			generate.data(n=n, abar=rep(1,time.pt), time.pt=time.pt, dynamic=TRUE)
	psi.abar_t0 = mean(O.abar_t0[,ncol(O.abar_t0)])
	assign(paste0("O.abar_t", time.pt), 
		foreach(n = rep(n.psi, 8), .combine = rbind) %dopar%
			generate.data(n=n, abar=rep(0,time.pt), time.pt=time.pt, dynamic=TRUE)
	)
	assign(paste0("psi.abar_t", time.pt), 
		mean(get(paste0("O.abar_t", time.pt))[,ncol(O.abar_t0)])
	)
	rm(list=paste0("O.abar_t", time.pt))
	
	
	###############
	## OPTION #2 ##
	###############
	# Dynamic regime (for all other time points)
	cat("Generating psi for Option #2\n")
	for(i in 1:(time.pt-1)) {
		assign(paste0("O.abar_t", i),
			foreach(n = rep(n.psi, 8), .combine = rbind) %dopar%
				generate.data(n=n, abar=c(rep(0,i),rep(1,time.pt-i)), time.pt=time.pt, dynamic=TRUE)
		)
		assign(paste0("psi.abar_t", i), 
			mean(get(paste0("O.abar_t", i))[,ncol(O.abar_t0)])
		)
		rm(list=paste0("O.abar_t", i))
	}
	
	
	###############
	## OPTION #3 ##
	###############
	# Joint intervention
	cat("Generating psi for Option #3\n")
	for(i in 1:(time.pt-1)) {
		assign(paste0("O.z1.abar_t", i),
			foreach(n = rep(n.psi, 8), .combine = rbind) %dopar%
				generate.data(n=n, abar=c(rep(0,i),rep(1,time.pt-i)), time.pt=time.pt, dynamic=FALSE)
		)
		assign(paste0("psi.z1.abar_t", i), 
				mean(get(paste0("O.z1.abar_t", i))[,ncol(O.abar_t0)])
		)
		rm(list=paste0("O.z1.abar_t", i))
	}
	
	return(list(option1 = c(psi.abar_t0, psi.abar_t10), 
				option2 = c(psi.abar_t1, psi.abar_t2, psi.abar_t3, psi.abar_t4, psi.abar_t5, psi.abar_t6, psi.abar_t7, psi.abar_t8, psi.abar_t9),
				option3 = c(psi.z1.abar_t1, psi.z1.abar_t2, psi.z1.abar_t3, psi.z1.abar_t4, psi.z1.abar_t5, psi.z1.abar_t6, psi.z1.abar_t7, psi.z1.abar_t8, psi.z1.abar_t9)))

}


