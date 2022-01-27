wfBinom <- function(N,ngens,reps,p0,mu=0){
    freq.list <- list()
    freq.list[[1]] <- rep(p0,reps)
    for(i in 2:ngens){
        freq.list[[i]] <- rbinom(reps,2*N,freq.list[[i-1]])/(2*N)
    }
    freq.list
}
fst <- function(x){
    l <- length(x)
    my.fst <- numeric()
    xbar <- mean(x)
    meanfst <- var(x)/(xbar*(1-xbar))
    meanfst
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
fstPlot <- function(mean.fsts,ngens,pop.size){
    plot(
        NA,
        type='n',
        ylim=c(0,1),
        xlim=c(0,ngens),
        xlab='Time, generations',
        ylab='Fst',
        cex.lab=1.4,
        cex.axis=1.2
    )
    lines(
        mean.fsts,
        lwd=2,
        lty=1,
        col='black'
    )
    lines(
        1-exp(-(1:ngens)/(2*pop.size)),
        col='blue',
        lwd=2,
        lty=2
    )
}
