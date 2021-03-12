wfCondBinom <- function(N,reps,p0){
    freq.list <- list()
    freq.list[[1]] <- rep(p0,reps)
    i=2
    while(any(freq.list[[i-1]]!=0)){
        freq.list[[i]] <- rbinom(reps,2*N,freq.list[[i-1]]*(1-1/(2*N)))/(2*N)
        i=i+1
    }
    tmp.freqs <- do.call(rbind,freq.list)
    my.freqs <- t(apply(tmp.freqs,2,rev))
    my.freqs
}
getAges <- function(sims){
    ncols <- ncol(sims)
    ages <- apply(sims,1,function(X) ncols-sum(X==0) )
    ages
}
freqPlot <- function(sims,N,p0){
    ## recover()

    ## ages <- getAges(sims)
    ## sages <- sort(ages)
    ## oldest <- max(ages)
    ## if(is.null(ngens))
    ##     ngens <- sages[length(ages)*0.9]

    sims.to.plot <- sims[1:50,]
    ages <- getAges(sims.to.plot)
    sages <- sort(ages)
    dist.this <- ages==sages[length(ages)/2]
    ngens <- ncol(sims)

    plot(NA,type="n",xlim=c(ngens-5*N,ngens),ylim=c(0,1),xlab="Generations Before Present",ylab="Frequency, p", cex.lab=1.4,cex.axis=1.2,bty='n',xaxt='n')
    ## abline(h=p0,col='blue',lty=2)
    matplot(
        t(sims.to.plot),
        type='l',
        lty=1,
        lwd=1,
        col=adjustcolor("black",0.3),
        add=T
    )
    axis(1,at=seq(ngens-5*N,ngens,by=N),labels=seq(5*N,0,by=-N))
    lines(colMeans(sims),col='blue',lwd=2,lty=1)
    lines(sims.to.plot[dist.this,],col='red',lwd=2)

    legend(
        x="topleft",
        legend=c("1 simulation","Mean across simulations"),
        col=c("red","blue"),
        lty=c(1,1),
        bg="white"
    )


}

ageHistPlot <- function(ages,N){
    mean.age <- mean(ages)
    left.limit <- 20*N
    my.hist <- hist(ages,breaks=50,xlim=c(0,left.limit),xlab="Allele Age",freq=F,main='')
    abline(v=mean.age,lty=2,lwd=2)
    text(x=left.limit*0.6,y=tail(sort(my.hist$density),2)[1],labels=paste('Mean Allele Age =',mean.age,'generations'))
}


