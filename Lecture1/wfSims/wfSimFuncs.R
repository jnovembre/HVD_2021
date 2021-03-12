wfBinom <- function(N,ngens,reps,p0,mu=0){
    freq.list <- list()
    freq.list[[1]] <- rep(p0,reps)
    for(i in 2:ngens){
        freq.list[[i]] <- rbinom(reps,2*N,freq.list[[i-1]])/(2*N)
    }
    my.freqs <- do.call(cbind,freq.list)
    my.freqs
}
het <- function(x) {
    tbl <- table(x)
    1 - sum((tbl/sum(tbl))^2)
}
freqPlot <- function(sims,ngens,p0){
    ##recover()

    plot(type="n",y=c(0,1),x=c(0,ngens),xlab="Time, generations",ylab="Frequency, p", cex.lab=1.4,cex.axis=1.2)
    abline(h=p0,col='blue',lty=2)
    matplot(
        t(sims),
        type='l',
        lty=1,
        lwd=1,
        col=adjustcolor("black",0.3),
        add=T
    )
    lines(sims[1,],col='red',lwd=2)
    lines(colMeans(sims),col='blue',lwd=2)
    legend(
        x="topright",
        legend=c("1 simulation","Mean across simulations","Expectation"),
        col=c("red","blue","blue"),
        lty=c(1,1,2),
        bg="white"
    )

}

hetPlot <- function(sims,ngens,p0,N){


    plot(type="n",y=c(0,0.5),x=c(0,ngens),xlab="Time, generations",ylab="Heterozygosity", cex.lab=1.4,cex.axis=1.2)
    hets <- 2*sims*(1-sims)

    matplot(
        t(hets),
        type='l',
        lty=1,
        lwd=1,
        col=adjustcolor('black',0.3),
        add=T
    )
    lines(hets[1,],col='red',lwd=2)
    lines(0:ngens,2*p0*(1-p0)*(1-1/(2*N))^(0:ngens),col="blue",lty=3,lwd=2)
    lines(colMeans(hets),col='blue',lwd=2)

}

