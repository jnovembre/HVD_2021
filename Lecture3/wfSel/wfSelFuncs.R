getNextFreq <- function(p.cur,w){
    g <- sapply(p.cur,function(P)c(P^2,2*P*(1-P),(1-P)^2))
    wdiff <- rev(diff(rev(w)))
    wbar <- t(g)%*%w
    p.new <- numeric(length(p.cur))
    for(j in seq_along(p.cur)){
        p.new[j] <- p.cur[j] + g[2,j]/2*(p.cur[j]*wdiff[1] + (1-p.cur[j])*wdiff[2])/wbar[j]
    }
    p.new
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
    ##recover()

    freq.list <- list()
    freq.list[[1]] <- rep(p0,reps)
    for(i in 1:(ngens-1)){
        ## if(FALSE){
        ##     freq.list[[i+1]] <- ifelse(
        ##         freq.list[[i]]>0|freq.list[[i]]<1,
        ##         {
        ##             p <- getNextFreq(freq.list[[i]],w)
        ##             rbinom(reps,2*N,p)/(2*N)
        ##         },
        ##         freq.list[[i]]
        ##     )
        ## }
        p <- getNextFreq(freq.list[[i]],w)
        freq.list[[i+1]] <- rbinom(reps,2*N,p)/(2*N)
    }
    my.freqs <- do.call(rbind,freq.list)
    my.freqs
}

trajPlot <- function(traj,w){

    ngens <- length(traj)
    plot(type="n",y=c(0,1),x=c(0,ngens),xlab="Time, generations",ylab="Frequency, p", cex.lab=1.4,cex.axis=1.2,bty='n')


}
trajPlotLines <- function(traj,w){
    ngens <- length(traj)
    lines(traj,col='black',lwd=2)
    if( w[1]>w[2] & w[3]>w[2] ){
        p.star <- getPStar(w)
        abline(h=p.star,col='red',lty=2,lwd=2)
    } else if( w[1]<w[2] & w[3]<w[2] ){
        p.star <- getPStar(w)
        abline(h=p.star,col='blue',lty=2,lwd=2)
    }

}
if(FALSE){

    w <- c(1,0.99,0.98)
    Ne <- 10000
    p0 <- 0.05

    traj <- getDetTrajectory(w,p0)
    ngens <- length(traj)
    reps <- 500

    t1 <- Sys.time()
    stoch.trajs <- wfBinomPosSel(Ne,w,ngens,reps,p0,mu=0)
    t2 <- Sys.time()
    t2-t1

    mean.traj <- rowMeans(stoch.trajs)

    trajPlot(traj,w)
    matplot(stoch.trajs,type='l',lty=1,lwd=0.6,add=TRUE,col=adjustcolor("grey",0.4))
    trajPlotLines(traj,w)
    lines(mean.traj,col='darkgreen',lwd=2)

}
