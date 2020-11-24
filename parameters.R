######################################################################################
# defines parameters for growth, price, profit functions.
# Updated: 2/13/2019
# By: Yukiko Hashida
######################################################################################

if (!require("pacman")) install.packages("pacman")
p_load(readxl, capn, reshape2, dplyr, reshape)

########################### GROWTH MODEL ##########################################
# First read FIA volume and stand age data for the western Oregon Douglas-fir plots. 
# We will fit the data using volume = exp(b1+b2*1/age+b3*1/age^2).
# Because of limited data points for site class 1 and 7, we combine seven classes to 
# site group 1 (sc1/2), 2 (sc3), 3 (sc4), 4 (sc5), and 5 (sc6/7).

regdata <- data.frame(read_xlsx("vol_age_df_oregon.xlsx","data")) # volume in thousand board feet 

# Take a log of volume and invert age, elevation, and interaction terms
regdata_log <- as.data.frame(cbind(log(regdata$acrevol),1/regdata$FLDAGE,1/(regdata$FLDAGE^2), 
                                   1/regdata$ELEV,1/(regdata$ELEV*regdata$FLDAGE),1/(regdata$owner),
                                   1/(regdata$ageowner),regdata$SITECLCD))
colnames(regdata_log) <- c("lnQ","invt","invt2","invelev","invelevt", "invpriv","invprivage", "siteclcd")

# replace inf with zero in invpriv and invprivage columns
regdata_log$invpriv[which(!is.finite(regdata_log$invpriv))] <- 0
regdata_log$invprivage[which(!is.finite(regdata_log$invprivage))] <- 0

# Drop rows with inf
apply(regdata_log, 2, function(x) any(is.na(x)|is.infinite(x)))

regdata_log <- regdata_log[!is.infinite(rowSums(regdata_log)),]

# Subset to private plots and group the data into site groups
regdata.1 <- subset(regdata_log, invpriv==1 & (siteclcd==1|siteclcd==2))
regdata.2 <- subset(regdata_log, invpriv==1 & siteclcd==3)
regdata.3 <- subset(regdata_log, invpriv==1 & siteclcd==4)
regdata.4 <- subset(regdata_log, invpriv==1 & siteclcd==5)
regdata.5 <- subset(regdata_log, invpriv==1 & (siteclcd==6|siteclcd==7))

regdata.1$ID <- seq.int(nrow(regdata.1))
regdata.2$ID <- seq.int(nrow(regdata.2))
regdata.3$ID <- seq.int(nrow(regdata.3))
regdata.4$ID <- seq.int(nrow(regdata.4))
regdata.5$ID <- seq.int(nrow(regdata.5))

# Drop the outliers based on the regression results 
regdata.1 <- regdata.1[-c(93,157,173),]
regdata.2 <- regdata.2[-c(18,115,259),]

# Fit the growth inverse curves

#Site group 1 
oregon.reg1 <-  lm(lnQ ~ invt + invt2, data = regdata.1)
summary(oregon.reg1)
sigma2_1 <- sigma(oregon.reg1)^2

#Site group 2
oregon.reg2 <-  lm(lnQ ~ invt + invt2, data = regdata.2)
summary(oregon.reg2)
sigma2_2 <- sigma(oregon.reg2)^2

#Site group 3
oregon.reg3 <-  lm(lnQ ~ invt + invt2, data = regdata.3)
summary(oregon.reg3)
sigma2_3 <- sigma(oregon.reg3)^2

#Site group 4
oregon.reg4 <-  lm(lnQ ~ invt + invt2, data = regdata.4)
summary(oregon.reg4)
sigma2_4 <- sigma(oregon.reg4)^2

#Site group 5
oregon.reg5 <-  lm(lnQ ~ invt + invt2, data = regdata.5)
summary(oregon.reg5)
sigma2_5 <- sigma(oregon.reg5)^2

#Make a big list of regression results
oregon.reg.list <- list(oregon.reg1,oregon.reg2,oregon.reg3,oregon.reg4,oregon.reg5)

### this is the tree growth function. It is used later. 
treeVol <- function(t, siteclass){
  sigma2 <- sigma(siteclass)^2
  t.list <- as.data.frame(cbind(1/t, 1/t^2))
  colnames(t.list) <- c("invt", "invt2")
  return(exp(sigma2/2 + predict(siteclass, newdata = t.list)))
}
##########################################################################################
#create differential equation

t.list <- seq(from = 10, to = 100, by = 0.25)
trees.temp1 <- as.data.frame(cbind(t.list,sapply(t.list,treeVol, siteclass=oregon.reg1)))
trees.temp2 <- as.data.frame(cbind(t.list,sapply(t.list,treeVol, siteclass=oregon.reg2)))
trees.temp3 <- as.data.frame(cbind(t.list,sapply(t.list,treeVol, siteclass=oregon.reg3)))
trees.temp4 <- as.data.frame(cbind(t.list,sapply(t.list,treeVol, siteclass=oregon.reg4)))
trees.temp5 <- as.data.frame(cbind(t.list,sapply(t.list,treeVol, siteclass=oregon.reg5)))

colnames(trees.temp1)<-c("time", "vol")
colnames(trees.temp2)<-c("time", "vol")
colnames(trees.temp3)<-c("time", "vol")
colnames(trees.temp4)<-c("time", "vol")
colnames(trees.temp5)<-c("time", "vol")

trees.grow1 <- as.data.frame(cbind(trees.temp1$vol[1:dim(trees.temp1)[1]-1],
                                   (trees.temp1$vol[2:dim(trees.temp1)[1]]-trees.temp1$vol[1:dim(trees.temp1)[1]-1])/0.25))
trees.grow2 <- as.data.frame(cbind(trees.temp2$vol[1:dim(trees.temp2)[1]-1],
                                   (trees.temp2$vol[2:dim(trees.temp2)[1]]-trees.temp2$vol[1:dim(trees.temp2)[1]-1])/0.25))
trees.grow3 <- as.data.frame(cbind(trees.temp3$vol[1:dim(trees.temp3)[1]-1],
                                   (trees.temp3$vol[2:dim(trees.temp3)[1]]-trees.temp3$vol[1:dim(trees.temp3)[1]-1])/0.25))
trees.grow4 <- as.data.frame(cbind(trees.temp4$vol[1:dim(trees.temp4)[1]-1],
                                   (trees.temp4$vol[2:dim(trees.temp4)[1]]-trees.temp4$vol[1:dim(trees.temp4)[1]-1])/0.25))
trees.grow5 <- as.data.frame(cbind(trees.temp5$vol[1:dim(trees.temp5)[1]-1],
                                   (trees.temp5$vol[2:dim(trees.temp5)[1]]-trees.temp5$vol[1:dim(trees.temp5)[1]-1])/0.25))

colnames(trees.grow1)<-c("vol", "dvol")
colnames(trees.grow2)<-c("vol", "dvol")
colnames(trees.grow3)<-c("vol", "dvol")
colnames(trees.grow4)<-c("vol", "dvol")
colnames(trees.grow5)<-c("vol", "dvol")

plot(trees.grow1$vol, trees.grow1$dvol, type = "l", main = "grow1")
y1 <- trees.grow1$dvol
x1 <- trees.grow1$vol

plot(trees.grow2$vol, trees.grow2$dvol, type = "l", main = "grow2")
y2 <- trees.grow2$dvol
x2 <- trees.grow2$vol

plot(trees.grow3$vol, trees.grow3$dvol, type = "l", main = "grow3")
y3 <- trees.grow3$dvol
x3 <- trees.grow3$vol

plot(trees.grow4$vol, trees.grow4$dvol, type = "l", main = "grow4")
y4 <- trees.grow4$dvol
x4 <- trees.grow4$vol

plot(trees.grow5$vol, trees.grow5$dvol, type = "l", main = "grow5")
y5 <- trees.grow5$dvol
x5 <- trees.grow5$vol

########### Fit the growth curve with polynomial for each site class
grow_sc1 <- lm(y1 ~ poly(x1, 4,raw=TRUE))
grow_sc2 <- lm(y2 ~ poly(x2, 4,raw=TRUE))
grow_sc3 <- lm(y3 ~ poly(x3, 4,raw=TRUE))
grow_sc4 <- lm(y4 ~ poly(x4, 4,raw=TRUE))
grow_sc5 <- lm(y5 ~ poly(x5, 4,raw=TRUE))

############### Combine the parameters ##########################
# param will be a matrix of site class in rows and variable in colums

sc1coeff <- as.data.frame(t(grow_sc1[["coefficients"]]))
sc2coeff <- as.data.frame(t(grow_sc2[["coefficients"]]))
sc3coeff <- as.data.frame(t(grow_sc3[["coefficients"]]))
sc4coeff <- as.data.frame(t(grow_sc4[["coefficients"]]))
sc5coeff <- as.data.frame(t(grow_sc5[["coefficients"]]))

colnames(sc1coeff) <- c("a1", "a2", "a3", "a4", "a5")
colnames(sc2coeff) <- c("a1", "a2", "a3", "a4", "a5")
colnames(sc3coeff) <- c("a1", "a2", "a3", "a4", "a5")
colnames(sc4coeff) <- c("a1", "a2", "a3", "a4", "a5")
colnames(sc5coeff) <- c("a1", "a2", "a3", "a4", "a5")

#check the model fits visually
temp <- as.data.frame(cbind(x1, predict(grow_sc1)))
colnames(temp) <- c("vol", "dvol.hat")
plot(temp$vol, temp$dvol.hat, type = "l", col = "blue", main = "grow1", sub = "blue fitted")
lines(trees.grow1$vol, trees.grow1$dvol)

temp <- as.data.frame(cbind(x2, predict(grow_sc2)))
colnames(temp) <- c("vol", "dvol.hat")
plot(temp$vol, temp$dvol.hat, type = "l", col = "blue")
lines(trees.grow2$vol, trees.grow2$dvol)

temp <- as.data.frame(cbind(x3, predict(grow_sc3)))
colnames(temp) <- c("vol", "dvol.hat")
plot(temp$vol, temp$dvol.hat, type = "l", col = "blue")
lines(trees.grow3$vol, trees.grow3$dvol)

temp <- as.data.frame(cbind(x4, predict(grow_sc4)))
colnames(temp) <- c("vol", "dvol.hat")
plot(temp$vol, temp$dvol.hat, type = "l", col = "blue")
lines(trees.grow4$vol, trees.grow4$dvol)

temp <- as.data.frame(cbind(x5, predict(grow_sc5)))
colnames(temp) <- c("vol", "dvol.hat")
plot(temp$vol, temp$dvol.hat, type = "l", col = "blue", main = "grow5", sub = "blue fitted")
lines(trees.grow5$vol, trees.grow1$dvol)

paramAll <- rbind(sc1coeff,sc2coeff,sc3coeff,sc4coeff,sc5coeff)
rownames(paramAll) <- c("sc1","sc2","sc3","sc4", "sc5")
rm('sc1coeff', 'sc2coeff', 'sc3coeff','sc4coeff', 'sc5coeff')

########################################################################
####################    CREATE MODEL FUNCTIONS #########################
########################################################################

#### price function
# price and minimum volume combination comes from 10-year average timber prices at each size grade and the minimum volume
# calculated with diameter requirement, height table (Douglas-fir, western Oregon), and growth equation that converts DBH
# and height to volume per acre. Volume per acre for each diameter class varies across site classes.

tpricedata <- as.data.frame(read_xlsx("econprog_files/timber_prices_05.xlsx","regress", col_names = TRUE)) #ten-year average prices (2005-2014) in 2014$ for each grade and minimum MBF volume required to fetch that grade price (q(t)). Volumes are from the estimated across ages 30-50 years. 

# Regress timber price on volume 
a_start <- 1
b_start <- 1
P <- nls(price ~ a*log(q) + b, data=tpricedata, start=list(a=a_start, b=b_start))

Pcoeff <- as.data.frame(coef(P))

# price depends on volume at harvest
timberprice <- function(s,der = FALSE){
  res <- if(der == FALSE) {
  Pcoeff[1,1]*log(s)+Pcoeff[2,1]} else {
    Pcoeff[1,1]/s
  }
  return(res)
}


##### profit functions

# profit function, inclusive of economic program: per-acre profit after tax
# equals harvest revenue minus harvest tax minus severance tax minus costs minus property tax

#general profit function for a discrete harvest.
#set der = 1 for first derivative of harvest function with respect to stock
profitF <- function(s, Z, der = FALSE){
  res <- if(der == FALSE) {
    s * (timberprice(s) - Z$cost - Z$tax1 - Z$tax2) - Z$ptax1} else {
      (timberprice(s) - Z$cost - Z$tax1 - Z$tax2) + s * timberprice(s, der = TRUE)
    }
  return(res)
}


##### Growth function

sdot.no.h <- function(s,Z, der = 0){
  #this is the growth function with no harvest, der = 0 is the function.
  #write der = 1 for the first derivative
  res <- if(der == 0) {
    Z$a1 + Z$a2*s + Z$a3*(s^2) + Z$a4*(s^3) + Z$a5*(s^4)} else {
      Z$a2 + 2*Z$a3*(s^1) + 3*Z$a4*(s^2) + 4*Z$a5*(s^3)}
  return(res)
}


##### inverse of the growth function
## t(s) and t'(s)
t.of.s <- function(stock, sc){ 
  #siteclass: corresponding inverse growth equation oregon.reg1 through oregon.reg5
  #coeff should be entered intercept, slope, quadratic
  coeffp <- sc$coefficients
  sigma2 <- sigma(sc)^2 
  a <- coeffp[1] 
  b <- coeffp[2]
  c <- coeffp[3]
  res1 <- (-b + sqrt(b^2 - 4*a*c- 2*c*sigma2 + 4*c*log(stock)))/(2*a+sigma2-2*log(stock))
  res2 <- 2*c/(stock*(2*a+sigma2-2*log(stock))*sqrt(b^2-4*a*c-2*c*sigma2+4*c*log(stock))) +
    res1*2/(stock*(2*a+sigma2-2*log(stock)))
  
  #try this
  #res2 <- 1/sdot.no.h(stock, sc, 0)
  
  res.out <- list(t.of.s = res1, t.prime.s = res2)
  return(res.out)
}

########################################################################################
#Faustmann function might be better here. 
trueFaust <- function(t, parameter, siteclass){
  return(-profitF(treeVol(t, siteclass), Z = parameter)/
           (exp(parameter$delta*t)-1))
}

#######################################################################################
faust.func <- function(guess, parameter, siteclass){
  out <- optim(par = guess,
               fn = trueFaust,
               parameter = parameter,
               siteclass = siteclass,
               control = list(maxit = 5000),
               method = "BFGS")
  fvol <- treeVol(out$par,siteclass)
  temp <- c(out$par, -out$val, fvol)
  names(temp) <- c("age", "value", "volume")
  return(temp)
}

###########################################################################
faust.results <- function(sc.now){
  para.temp<- paramAll[sc.now,]
  faust.temp <- optim(par = 40,
                      fn = trueFaust,
                      parameter = para.temp,
                      siteclass = oregon.reg.list[[sc.now]],
                      control = list(maxit = 5000),
                      method = "BFGS")
  
  result7 <- c(faust.temp$par,
               treeVol(faust.temp$par, oregon.reg.list[[sc.now]]),
               -faust.temp$value
  )
  
  para.temp$delta <- 0.03
  faust.temp <- optim(par = 40,
                      fn = trueFaust,
                      parameter = para.temp,
                      siteclass = oregon.reg.list[[sc.now]],
                      control = list(maxit = 5000),
                      method = "BFGS")
  result3 <- c(faust.temp$par,
               treeVol(faust.temp$par, oregon.reg.list[[sc.now]]),
               -faust.temp$value
  )
  
  result <- as.data.frame(rbind(result7, result3))
  colnames(result) <- c("age", " volume", "value")
  rownames(result) <- c("7percent", "3percent")
  return(result)
}
