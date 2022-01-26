wfBinomWMig <- function(N,ngens,reps,pCont,pIsle,m=0.01){
    freq.list <- list()
    freq.list[[1]] <- rep(pIsle,each=reps)
    for(i in 2:ngens){
        ptemp <- freq.list[[i-1]]*(1-m)+pCont*m
        freq.list[[i]] <- rbinom(reps,2*N,ptemp)/(2*N)
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
freqPlot <- function(sims,pCont,ngens=length(my.sims)){
    rando <- sample(1:length(sims[[1]]),1)
    freq.mat <- do.call(rbind,sims)
    freq.mat.mean <- rowMeans(freq.mat)

    plot(type='n',y=c(0,1),x=c(0,ngens),xlab='Time, generations',ylab='Frequency, p', cex.lab=1.4,cex.axis=1.2)
    abline(h=pCont,lty=2,lwd=2,col='blue')
    lines(freq.mat.mean,col='orange',lwd=2,lty=2)
    lines(freq.mat[,rando],col='red',lwd=1.5,lty=1)
    matplot(
        freq.mat,
        type='l',
        lty=1,
        lwd=3/4,
        col=adjustcolor('red',0.05),
        add=T
    )

    legend(
        x='topleft',
        legend=c(
            '1 simulation from island population',
            'Island mean',
            'Continent mean'
            ),
        col=c('red','orange','blue'),
        lty=c(1,2,2),
        lwd=rep(1.5,2,2),
        bg='white'
    )

}


fstPlot <- function(mean.fsts,ngens,scaled.mig.rate){

    my.ymax <- ifelse(max(mean.fsts)<0.3,1/2,1)

    plot(
        NA,
        type='n',
        ylim=c(0,my.ymax),
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
        col='blue'
    )
    abline(
        h=1/(1+scaled.mig.rate),
        lwd=1.5,
        lty=2
    )

}
