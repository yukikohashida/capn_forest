# function analyze forest data
if (!require("pacman")) install.packages("pacman")
p_load(foreach, doParallel)  #http://blog.aicry.com/r-parallel-computing-in-5-minutes/index.html
                             #https://cran.r-project.org/web/packages/doParallel/doParallel.pdf 
registerDoParallel()
#########################################################################################
## run approximations
##########################################################################################

analyze.forests <- function(harv.data,
                            site.c,
                            parAll = paramAll,
                            orl = oregon.reg.list,
                            Snds = Snodes,
                            get.dr = NULL,
                            fguess = 20){
  
  #Assign site we are working with 
  para <- paramAll[site.c,]
  siteclass <- orl[[site.c]] 
  
  tprice.All <- as.data.frame(cbind(Snds, timberprice(Snds),
                                    profitF(Snds, Z = para, der = TRUE)))
  colnames(tprice.All) <- c("stock", "price","marg.prof") 
  
  #get the Faustmann result
  fvol.temp <- faust.func(guess = fguess, 
                          parameter = para,
                          siteclass = siteclass
                          )
  fvol = fvol.temp[3]
  
  ############### this from apply_capn_v_split.R ######################################
  crit.vol <- harv.data$Pdvol
  
  # node that corresponds to the observed harvest volume
  hnode1 <- min(which(Snodes > crit.vol)) 
  
  # node that corresponds to the Faustmann harvest volume
  fvolx <- fvol
  hnode1f <- min(which(Snodes>fvolx))
  
  p.temp <- sapply(Snodes, profitF, Z = para)
  p.temp <- replace(p.temp, Snodes < Snodes[hnode1], 0)
  simuDataV <- as.data.frame(cbind(Snodes,
                                   sdot.no.h(Snodes,para) - replace(Snodes, Snodes < Snodes[hnode1], 0),
                                   p.temp, 
                                   replace(Snodes, Snodes < Snodes[hnode1], 0)
  ))
  colnames(simuDataV) <- c("vol", "growth", "profit", "harvest")
  
  p.temp <- sapply(Snodes, profitF, Z = para)
  p.tempf <- replace(p.temp,Snodes < Snodes[hnode1f], 0)
  simuDataVf <- as.data.frame(cbind(Snodes,
                                    sdot.no.h(Snodes,para) - 
                                      replace(Snodes, Snodes < Snodes[hnode1f], 0),
                                    p.tempf, 
                                    replace(Snodes, Snodes < Snodes[hnode1f], 0)))
  colnames(simuDataVf) <- c("vol", "growth", "profit", "harvest")
  
  
  vC2 <- split.v.approx(Aspace, simuDataV, hnode1, 
                        profitF(Snodes[hnode1], Z = para, der = TRUE), 
                        t.of.s(Snodes[hnode1],siteclass)$t.of.s)
  vC2f <- split.v.approx(Aspace, simuDataVf, hnode1f, 
                         profitF(Snodes[hnode1f], para, der = TRUE), 
                         t.of.s(Snodes[hnode1f],siteclass)$t.of.s)
  
  ForestSimV2 <- vsim(vC2,as.matrix(simuDataV[,1],ncol=1),profitF(Snodes,para))
  fdata12 <- as.data.frame(ForestSimV2)
  
  ForestSimV2f <- vsim(vC2f,as.matrix(simuDataVf[,1],ncol=1),profitF(Snodes,para))
  fdata12f <- as.data.frame(ForestSimV2f)
  ##########################################################################################################
  econ.sprice <- fdata12$acc.price1
  econ.vfun <- fdata12$vfun
  faust.sprice <- fdata12f$acc.price1
  faust.vfunF <- fdata12f$vfun 
  
  faust.age <- fvol[1] #Faust.out$par
  faust.vol <- fvol   #assigns the Faustmann volume associate with the established discount rate
  faust.node <- hnode1f
  amenity <- vC2$income.w
  econ.vol <- Snodes[hnode1]
  econ.age <- t.of.s(Snodes[hnode1],siteclass)$t.of.s
  econ.node <- hnode1
  tprice <- timberprice(Snodes[hnode1])
  
  
  param.temp <- para
  fvol.temp <- fvol  # need to check that this correct
  
  # Discount rate start  
  if(get.dr == TRUE){
    
    delta.now <- 0.07
    start.guess <- econ.age
    while (Snodes[hnode1]-fvol.temp > 0 & delta.now > 0.0){
      param.temp$delta <- param.temp$delta - 0.001
      # garbage <- as.data.frame(-99)
      # colnames(garbage) <- "par"
      
      # f.temp <- tryCatch({
      f.temp <-
        optim(par = start.guess,
              fn = trueFaust,
              parameter = param.temp,
              siteclass = siteclass,
              control = list(maxit = 5000),
              method = "BFGS")
      # },
      # error = function(e){garbage}
      
      delta.now <- param.temp$delta
      
      fvol.temp <- treeVol(f.temp$par,siteclass)
      delta.now <- f.temp$par
    }
    
    #faust.delta1 <- max(param.temp$delta, delta.now)
    faust.delta1 <- param.temp$delta
    
    while (Snodes[hnode1]-fvol.temp < 0 & delta.now < 2.0){
      param.temp$delta <- param.temp$delta + 0.001
      # garbage <- as.data.frame(99)
      # colnames(garbage) <- "par"
      
      # f.temp <- tryCatch({
      f.temp <- optim(par = start.guess,
                      fn = trueFaust,
                      parameter = param.temp,
                      siteclass = siteclass,
                      control = list(maxit = 5000),
                      method = "BFGS")
      
      delta.now <- param.temp$delta
      fvol.temp <- treeVol(f.temp$par,siteclass)  
    }
    # error = function(e){garbage}
    # )
    
    
  }
  
  faust.delta2 <- min(param.temp$delta, delta.now)
  
  if(para$delta < param.temp$delta) {
    r.delta <- faust.delta2} else {
      r.delta <- faust.delta1
    }
  # } else {
  #   r.delta <- -99
  # }
  
  # Convert to data frame
  econ.sprice <- as.data.frame(econ.sprice)
  econ.vfun <- as.data.frame(econ.vfun)
  faust.sprice <- as.data.frame(faust.sprice)
  faust.vfunF <- as.data.frame(faust.vfunF)
  faust.age <- as.data.frame(faust.age)
  faust.vol <- as.data.frame(faust.vol) 
  faust.node <- as.data.frame(faust.node)
  amenity <- as.data.frame(amenity)
  econ.vol <- as.data.frame(econ.vol)
  econ.age <- as.data.frame(econ.age)
  econ.node <- as.data.frame(econ.node)
  tprice <- as.data.frame(tprice)
  r.delta <- as.data.frame(r.delta)
  
  # Calculate mean age, volume, amenity flow by taking averages across rows (plots)
  mean.econ.vol <- colMeans(econ.vol)
  mean.faust.vol <- colMeans(faust.vol)
  mean.econ.age <- colMeans(econ.age)
  median.econ.age <- apply(econ.age,2,median)
  mean.faust.age <- colMeans(faust.age)
  mean.amenity <- colMeans(amenity)
  mean.tprice <- colMeans(tprice)
  mean.r.delta <- colMeans(r.delta)
  
  
  # Calculate mean shadow prices by taking averages across columns (plots)
  mean.faust.sprice <- rowMeans(faust.sprice)
  
  # Create a shadow price data set that only includes pre-harvest nodes (to avoid averaging across kinks)
  econ.sprice.pre <- as.data.frame(matrix(0,ncol=nrow(harv.data),nrow=parAll$nodes))
  
  #this recreates the economic shadow price curve with NA after the econ harvest node.
  for (m in 1: nrow(harv.data)) {
    econ.sprice.pre[,m] <- c(econ.sprice[1:econ.node[m,]-1,m], rep(NA,400-(econ.node[m,]-1)))
    
  }
  
  mean.econ.sprice.pre <- rowMeans(econ.sprice.pre, na.rm = TRUE)
  
  # Sum of value function and mean value function
  sum.econ.vfun <- rowSums(econ.vfun)
  sum.faust.vfun <- rowSums(faust.vfunF)
  
  mean.econ.vfun <- rowMeans(econ.vfun)
  mean.faust.vfun <- rowMeans(faust.vfunF)
  
  simres <- cbind(fdata12$stock, 
                  mean.econ.sprice.pre, 
                  mean.faust.sprice,
                  mean.econ.vfun, 
                  mean.faust.vfun,
                  tprice.All$marg.prof)
  
  colnames(simres) <- c("stock",
                        "mean.econ.price",
                        "mean.faust.price",
                        "mean.econ.vfun",
                        "mean.faust.vfun",
                        "marg.prof")
  
  output <- list(simres, siteclass, fdata12, 
                 faust.node, parAll, econ.sprice, econ.node, r.delta, amenity )
  names(output) <-c("results", "siteclass", "fdata12", 
                    "faust.node", "param", "econ.sprice", "econ.node", "r.delta", "amenity")
  
  return(output)
}


##########################################################################################
## prep for graphs

prep.graphs <- function(focal.result, Snds = Snodes){
  results <- as.data.frame(focal.result$results)
  siteclass <- focal.result$siteclass
  para2 <- focal.result$param
  fdata12 <- as.data.frame(focal.result$fdata12)
  faust.node <- focal.result$faust.node
  
  
    #view from the smallest stock
  times <- t.of.s(stock = fdata12$stock,
                  sc = siteclass)$t.of.s
  
  at.t <- as.data.frame(cbind(Snds,exp(-(times-times[1])*para2$delta)*results$mean.econ.price))
  colnames(at.t) <- c("stock", "price")   
  
  at.t.F <- as.data.frame(cbind(Snds,exp(-(times-times[1])*para2$delta)*results$mean.faust.price))
  colnames(at.t.F) <- c("stock", "price") 
  
  
  v.at.t <- as.data.frame(cbind(Snds,exp(-(times-times[1])*para2$delta)*results$mean.econ.vfun)) # PV value function
  colnames(v.at.t) <- c("stock", "value") 
  
  v.at.t.F <- as.data.frame(cbind(Snds,exp(-(times-times[1])*para2$delta)*results$mean.faust.vfun))
  colnames(v.at.t.F) <- c("stock", "value") 
  
  output <- list(results, at.t, at.t.F, faust.node, v.at.t, v.at.t.F)
  names(output) <- c("results", "at.t", "at.t.F", "faust.node", "v.at.t", "v.at.t.F")
  
  return(output)
  
  
}


