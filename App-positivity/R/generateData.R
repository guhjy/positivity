###############################################################################
# Description: Generates simulation data
# 
# Author: Linh Tran
# Date: January 8, 2015
###############################################################################

generate.data = function(n, time.pt, abar=NULL, dynamic=TRUE) {
	
	## CHECKS ##
	if(time.pt==0) stop("time.pt has to be greater than 0")
	if(any(cummax(abar)!=abar)) stop("A is a counting process & cannot decrease")
	if(!is.null(abar) & length(abar) != time.pt) stop("abar has to be NULL or length of time.pt")
	
	## INITIALIZATION ##
	o.names = NULL
	for(i in 0:time.pt){
		if(i<time.pt) {
			o.names = c(o.names, paste0(c("Y", "L1","L2", "Z", "A"), ".", i))
		} else {
			o.names = c(o.names, paste0(c("Y"), ".", i))
		}	
	}
	O = matrix(nrow=n, ncol=5*time.pt+3, dimnames=list(NULL, c("W1", "W2", o.names)))
	
	## OBSERVED VALUES ##
	O[,"W1"] = QW1(n)
	O[,"W2"] = rexpit(QW2(n))
	for(i in 0:time.pt){
		#nb. "prev" values are set to 0 for t=0
		if(i==0) {
			#Y(t)
			O[,"Y.0"] = rep(0,n)
			#L1(t)
			O[,"L1.0"] = QL1.t(y=O[,"Y.0"], w1=O[,"W1"], 			 prev_l1=rep(0, n), prev_l2=rep(0, n), prev_a=rep(0, n))
			#L2(t)
			O[,"L2.0"] = QL2.t(y=O[,"Y.0"], w1=O[,"W1"], w2=O[,"W2"], prev_l1=rep(0, n), prev_l2=rep(0, n), prev_a=rep(0, n))
			#Z(t)
			O[,"Z.0"]  = rep(1,n)
			#A(t)
			if(is.null(abar)) {
				O[,"A.0"] = rexpit(gA.t(y=O[,"Y.0"], w1=O[,"W1"], w2=O[,"W2"], l1=O[,"L1.0"], l2=O[,"L2.0"], prev_a=rep(0, n), z=O[,"Z.0"]))
			} else {
				O[,"A.0"] = rep(abar[i+1], n)
			}
		} else if (i<time.pt) {			
			#Y(t)
			O[,paste0("Y.",i)] = rexpit(QY.t(prev_y=O[,paste0("Y.",i-1)], w1=O[,"W1"], w2=O[,"W2"], prev_l1=O[,paste0("L1.",i-1)], prev_l2=O[,paste0("L2.",i-1)], prev_a=O[,paste0("A.",i-1)], prev_z=O[,paste0("Z.",i-1)]))
			#L1(t)
			O[,paste0("L1.",i)] = QL1.t(y=O[,paste0("Y.",i)], w1=O[,"W1"], prev_l1=O[,paste0("L1.",i-1)], prev_l2=O[,paste0("L2.",i-1)], prev_a=O[,paste0("A.",i-1)])
			#L2(t)
			O[,paste0("L2.",i)] = QL2.t(y=O[,paste0("Y.",i)], w1=O[,"W1"], w2=O[,"W2"], prev_l1=O[,paste0("L1.",i-1)], prev_l2=O[,paste0("L2.",i-1)], prev_a=O[,paste0("A.",i-1)])
			#Z(t)
			O[,paste0("Z.",i)]  = rexpit(gZ.t(y=O[,paste0("Y.",i)], w1=O[,"W1"], w2=O[,"W2"], l1=O[,"L1.0"], l2=O[,"L2.0"], prev_a=O[,paste0("A.",i-1)]))
			#A(t)
			if(is.null(abar)) {
				O[,paste0("A.",i)] = rexpit(gA.t(y=O[,paste0("Y.",i)], w1=O[,"W1"], w2=O[,"W2"], l1=O[,paste0("L1.",i)], l2=O[,paste0("L2.",i)], prev_a=O[,paste0("A.",i-1)], z=O[,paste0("Z.",i)]))
			} else {
				if(dynamic) {
					O[,paste0("A.",i)] = ifelse(O[,paste0("Y.",i)]==0 & O[,paste0("Z.",i)]==1, abar[i+1], O[,paste0("A.",i-1)])
				} else {
					## Joint intervention ##
					if(abar[i+1]==1) {
						O[,paste0("Z.",i)] = ifelse(O[,paste0("Y.",i)]==1, 0, 1)
						O[,paste0("A.",i)] = ifelse(O[,paste0("Y.",i)]==1, O[,paste0("A.",i-1)], abar[i+1])
					} else {
						O[,paste0("A.",i)] = ifelse(O[,paste0("Y.",i)]==1, O[,paste0("A.",i-1)], abar[i+1])
					}
				}
			}
		} else if (i==time.pt) {
			#Y(t)
			O[,paste0("Y.",i)] = rexpit(QY.t(prev_y=O[,paste0("Y.",i-1)], w1=O[,"W1"], w2=O[,"W2"], prev_l1=O[,paste0("L1.",i-1)], prev_l2=O[,paste0("L2.",i-1)], prev_a=O[,paste0("A.",i-1)], prev_z=O[,paste0("Z.",i-1)]))
		}
	}
	## CONVERTS CENSORING TO FACTORS ##
	O = data.frame(O)
	
	return(O)

}
