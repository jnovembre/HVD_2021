getNextFreq <- function(p,w){
    het <- p*(1-p)
    g <- c(p^2,2*het,(1-p)^2)
    wdiff <- rev(diff(rev(w)))
    wbar <- sum(w*g)
    p+het*(p*wdiff[1]+(1-p)*wdiff[2])/wbar
}
getPStar <- function(w){
    (w[3]-w[2])/(w[1]+w[3]-2*w[2])
}
getDetTrajectory <- function(w,p.start){
    if(w[1]==w[2] & w[2]==w[3] ) {
        ## if actually neutral
        ## just return 1000 generations
        ## constant at intitial frequency
        freqs <- rep(p.start,1000)
        return(freqs)
    } else if( w[1] >= w[2] & w[2] >= w[3] ) {
        ## ###
        ## positive selection ##
        ## ###
        freqs <- list()
        freqs[[1]] <- p.start
        i <- 1
        gr99 <- FALSE
        go <- TRUE
        trigger.gen <- NULL
        while(go) {
            freqs[[i+1]] <- getNextFreq(freqs[[i]],w)
            if( !gr99 & freqs[[i+1]] > 0.99 ){
                ## check if frequency greater than 99%
                ## if so, set flag to true, and figure
                ## out how many more gens to calc
                gr99 <- TRUE
                if(i*1.5<1000)
                    trigger.gen <- 1000
                else
                    trigger.gen <- i*1.5

            }
            if(!is.null(trigger.gen)){
                ## exit while loop if enough gens have
                ## been calc'd
                if(i>trigger.gen)
                    go <- FALSE
            }
            i <- i+1
        }
        return(unlist(freqs))
    } else if( w[1]>w[2] & w[3]>w[2] ) {
        ## ###
        ## underdominance ##
        ## ###
        p.star <- getPStar(w)
        if(p.start==p.star) {
            ## if right at the unstable equilibrium
            ## just return 1000 generations holding
            ## constant at that frequency
            return(rep(p.start,1000))
        }
        freqs <- list()
        freqs[[1]] <- p.start
        i <- 1
        go <- TRUE
        hit.thr <- FALSE
        trigger.gen <- NULL
        while(go) {
            freqs[[i+1]] <- getNextFreq(freqs[[i]],w)
            if(!hit.thr) {
                if(any(abs(freqs[[i+1]]-c(0,1))<0.01)) {
                    hit.thr <- TRUE
                    if(i*1.5<1000)
                        trigger.gen <- 1000
                    else
                        trigger.gen <- i*1.5
                }
            } else if(hit.thr) {
                if(i>trigger.gen) {
                    go <- FALSE
                }
            }
            i <- i+1
        }
        return(unlist(freqs))
    } else if( w[1]<w[2] & w[3]<w[2] ) {
        ## ###
        ## overdominance ##
        ## ###
        p.star <- getPStar(w)
        if(p.start==p.star) {
            ## if right at the stable equilibrium
            ## just return 1000 generations holding
            ## constant at that frequency
            return(rep(p.start,1000))
        }
        freqs <- list()
        freqs[[1]] <- p.start
        i <- 1
        go <- TRUE
        hit.thr <- FALSE
        trigger.gen <- NULL
        while(go) {
            freqs[[i+1]] <- getNextFreq(freqs[[i]],w)
            if(!hit.thr) {
                if(any(abs(freqs[[i+1]]-p.star)<0.01)) {
                    hit.thr <- TRUE
                    if(i*1.5<1000)
                        trigger.gen <- 1000
                    else
                        trigger.gen <- i*1.5
                }
            } else if(hit.thr) {
                if(i>trigger.gen) {
                    go <- FALSE
                }
            }
            i <- i+1
        }
        return(unlist(freqs))
    }

}

wfBinomPosSel <- function(N,w,ngens,reps,p0,mu=0){
    recover()
    getNextFreq
    freq.list <- list()
    freq.list[[1]] <- rep(p0,reps)
    for(i in 1:(ngens-1)){
        freq.list[[i+1]] <- rbinom(reps,2*N,freq.list[[i]])/(2*N)
    }
    my.freqs <- do.call(cbind,freq.list)
    my.freqs
}

trajPlot <- function(traj,w){
    ngens <- length(traj)

    plot(type="n",y=c(0,1),x=c(0,ngens),xlab="Time, generations",ylab="Frequency, p", cex.lab=1.4,cex.axis=1.2,bty='n')
    lines(traj,col='black',lwd=2)
    if( w[1]>w[2] & w[3]>w[2] ){
        p.star <- getPStar(w)
        abline(h=p.star,col='red',lty=2,lwd=2)
    } else if( w[1]<w[2] & w[3]<w[2] ){
        p.star <- getPStar(w)
        abline(h=p.star,col='blue',lty=2,lwd=2)
    }

}


het <- function(x) {
    tbl <- table(x)
    1 - sum((tbl/sum(tbl))^2)
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

if(FALSE){

    traj <- getDetTrajectory(c(0.99,1,0.99),1/200)
    trajPlot(traj)


}
