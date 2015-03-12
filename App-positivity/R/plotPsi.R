###############################################################################
# Description: Creates plots
# 
# Author: Linh Tran <tranlm@berkeley.edu>
# Date: Jan 15, 2015
###############################################################################


plotPsi = function(main="") {
	par(oma=c(0,1,0,0), mar=c(5,5,4,3))
	xlim = c(0,10); ylim = c(-2.5,0)
	plot(NULL, main=main, xlab="Time switched ART", ylab="logit Pr(Y=1)", xlim=xlim, ylim=ylim, cex.lab=1.5, cex.axis=1.75, cex.main=2, yaxt="n", xaxt="n")
	rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "gray90")
	axis(1, seq(min(xlim),max(xlim),1), label=TRUE)
	axis(2, seq(min(ylim),max(ylim),.5), label=TRUE, las=1)
	abline(v = seq(min(xlim),max(xlim),2), col = "white", lwd = 1.5)
	abline(v = seq(min(xlim),max(xlim),1), col = "white", lwd = .75)
	abline(h = seq(min(ylim),max(ylim),.5), col = "white", lwd = 1.5)
	abline(h = seq(min(ylim),max(ylim),.5/2), col = "white", lwd = .75)
	points(qlogis(truePsi$option1) ~ c(0, 10), pch=19, col="red")
	lines(qlogis(truePsi$option1) ~ c(0, 10), lty=2, lwd=1.5, col="red")
	points(qlogis(truePsi$option2) ~ c(1:9), pch=17, col="blue")
	points(qlogis(truePsi$option3) ~ c(1:9), pch=15, col="green")
	legend(0, 0, c("Option #1", "Option #2", "Option #3"), pch = c(19,17,15), col=c("red", "blue", "green"), bg="white")
}

