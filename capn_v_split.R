#########################################################################
#this holds important functions for the (repeat) stopping version of capn
#Eli Fenichel, Yale University
#Updated 7/6/2018
#########################################################################

###############################################################
#### FUNCTIONS
###############################################################
### generalized inverse function
gen.inverse <- function(x , y){
  return(solve(t(x) %*% x) %*% t(x) %*% y)
}
################################################################
################################################################


### split function
split.v.approx<-function(aproxspace, sdata, split.node, split.marg.p, split.time = NULL){
  #setup basis functions for V when waiting
  nodes <- sdata[,1]
  hv <- nodes[split.node]
  
  #discount rate
  delta <- aproxspace$delta
  
  phi.wait <- chebbasisgen(nodes,
                            aproxspace$degree,
                            aproxspace$lowerB,
                            aproxspace$upperB)
  
  #setup basis functions for V_s when waiting
  phi.s.wait <- chebbasisgen(sdata[,1],
                             aproxspace$degree,
                             aproxspace$lowerB,
                             aproxspace$upperB, 
                            dorder = 1)
  
  #setup basis functions for V right after harvest. There is option here to use a discounting approach or the first node.
  #The discounting approach, which requires the harvest time "split.time," should be used if the data are not close to the lowest 
  #node like in our toy model with Conrad's Douglas Fir data. 
  #phi.stop
  if(!is.null(split.time)) {
    #this is an alternative way of getting the V(s(0)) or really \Phi(S(0)) that is consistent
    #with the brute force approach to the Faustmann rotation
    #phi.stop <-t(phi.wait[split.node,] %*% exp(-delta * split.time))
    phi.stop <-t(phi.wait[split.node,] * exp(-delta * split.time))
    } else {
    #this is the standard approach 
    phi.stop <- chebbasisgen(sdata[1,1],
                               aproxspace$degree,
                               aproxspace$lowerB,
                               aproxspace$upperB)
    }
  
  #setup basis functions for the V_s at harvest
  phi.s.stop <- chebbasisgen(sdata[1,1],
                           aproxspace$degree,
                           aproxspace$lowerB,
                           aproxspace$upperB, 
                           dorder = 1)
  

  #the idea is that it does not matter if you wait or harvest the structure is
  # QB - qB - y, were Q is one nxn matrix, q is another nxn matrix, and y is nx1 vector
  #B is standing in for beta.  The goal is to solve for B in an over determined system.
  #Because we know when harvest happens from the economic program. It is just a matter
  #arranging the correct basis functions and values for y. 
  
  gmat1 <- t(delta*phi.wait[1:split.node,])
  gmat2 <- t(phi.wait[(split.node + 1):length(nodes),]) 
  gmat <- t(cbind(gmat1,gmat2))
  
  hmat1a <- diag(sdata[,2]) %*% phi.s.wait
  hmat1 <- t(hmat1a[1:split.node,])
  num.stop <- size(gmat2)[[2]]
  hmat2 <- (kronecker(
    matrix(1,1,num.stop),
    t(phi.stop)
    ))
  hmat <- t(cbind(hmat1,hmat2))
  ymat <- gmat - hmat
  
  #the next section of code (until #*#*#*) is used to find the correct w and resolve for the correct beta
  find.w <- function(w){
    k <- c(nodes[1:split.node]*w, sdata[(split.node+1):length(nodes),3])
    beta <- gen.inverse(ymat, k)
    vs1 <- phi.s.wait[split.node-1,] %*% beta
    tol.obj <- (vs1-split.marg.p)^2
    return(tol.obj)
  }
  
  w.out <- optim(par = 0,
                     fn = find.w,
                     control = list(maxit = 5000),
                     method = "BFGS")
  
  w <- w.out$par #final first-order approximation to the ammenity flow income 
  k <- c(nodes[1:split.node]*w, sdata[(split.node+1):length(nodes),3])
  
  beta <- gen.inverse(ymat, k)
  #*#*#*
  
  sm <- phi.s.wait[split.node-1,] %*% beta #shadow value at the stopping point. 
  
  ## results list
  res <- list(degree = aproxspace$degree,
              lowerB = aproxspace$lowerB,
              upperB = aproxspace$upperB,
              delta = delta,
              coefficient = beta,
              shadow.stop = sm,
              income.w = w
              )
  return(res)
}

#####################################################################################


