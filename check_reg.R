sc.temp <- 1
rawdata <- as.data.frame(cbind(1/regdata.1$invt, 
                               exp(regdata.1$lnQ),
                               exp(predict(oregon.reg.list[[sc.temp]]))))
colnames(rawdata) <- c("time", "vol", "v.hat")


treepred <- as.data.frame(cbind(
  c(1:800),
  sapply(c(1:800), treeVol, siteclass = oregon.reg.list[[sc.temp]])
))
colnames(treepred) <- c("time", "vol")

tout <- as.data.frame(sapply(c(1:150), 
                             t.of.s, 
                             sc = oregon.reg.list[[sc.temp]]))

pred.t <- as.data.frame(t(
  rbind(unlist(tout[1,]),
      c(1:150))))
colnames(pred.t) <- c("time", "stock")


rp1<-ggplot(data = rawdata, aes(x = time, y =vol)) +
  geom_point(size = 2, shape = 1)+
  geom_line(data = treepred, aes(x = time, y =vol), 
            color = 'blue')+
  geom_line(data = pred.t, aes(x = time, y = stock), 
          color = 'red', linetype = 'dashed')+
  xlim(min(c(0,rawdata$time)),max(rawdata$time))+
  ylim(min(c(0,rawdata$vol)), max(rawdata$vol))+
  labs(x="Years", y="Volume")+
  theme(  #http://ggplot2.tidyverse.org/reference/theme.html
    axis.line = element_line(color = "black"), 
    panel.background = element_rect(fill = "transparent",color = NA),
    plot.background = element_rect(fill = "transparent",color = NA)
  )
  
sc.temp <- 2
rawdata <- as.data.frame(cbind(1/regdata.2$invt, 
                               exp(regdata.2$lnQ),
                               exp(predict(oregon.reg.list[[sc.temp]]))))
colnames(rawdata) <- c("time", "vol", "v.hat")


treepred <- as.data.frame(cbind(
  c(1:800),
  sapply(c(1:800), treeVol, siteclass = oregon.reg.list[[sc.temp]])
))
colnames(treepred) <- c("time", "vol")

tout <- as.data.frame(sapply(c(1:150), 
                             t.of.s, 
                             sc = oregon.reg.list[[sc.temp]]))

pred.t <- as.data.frame(t(
  rbind(unlist(tout[1,]),
        c(1:150))))
colnames(pred.t) <- c("time", "stock")


rp2<-ggplot(data = rawdata, aes(x = time, y =vol)) +
  geom_point(size = 2, shape = 1)+
  geom_line(data = treepred, aes(x = time, y =vol), 
            color = 'blue')+
  geom_line(data = pred.t, aes(x = time, y = stock), 
            color = 'red', linetype = 'dashed')+
  labs(x="Years", y="Volume")+
  xlim(min(c(0,rawdata$time)),max(rawdata$time))+
  ylim(min(c(0,rawdata$vol)), max(rawdata$vol))

sc.temp <- 3
rawdata <- as.data.frame(cbind(1/regdata.3$invt, 
                               exp(regdata.3$lnQ),
                               exp(predict(oregon.reg.list[[sc.temp]]))))
colnames(rawdata) <- c("time", "vol", "v.hat")


treepred <- as.data.frame(cbind(
  c(1:800),
  sapply(c(1:800), treeVol, siteclass = oregon.reg.list[[sc.temp]])
))
colnames(treepred) <- c("time", "vol")

tout <- as.data.frame(sapply(c(1:150), 
                             t.of.s, 
                             sc = oregon.reg.list[[sc.temp]]))

pred.t <- as.data.frame(t(
  rbind(unlist(tout[1,]),
        c(1:150))))
colnames(pred.t) <- c("time", "stock")


rp3<-ggplot(data = rawdata, aes(x = time, y =vol)) +
  geom_point(size = 2, shape = 1)+
  geom_line(data = treepred, aes(x = time, y =vol), 
            color = 'blue')+
  geom_line(data = pred.t, aes(x = time, y = stock), 
            color = 'red', linetype = 'dashed')+
  labs(x="Years", y="Volume")+
  xlim(min(c(0,rawdata$time)),max(rawdata$time))+
  ylim(min(c(0,rawdata$vol)), max(rawdata$vol))

sc.temp <- 4
rawdata <- as.data.frame(cbind(1/regdata.4$invt, 
                               exp(regdata.4$lnQ),
                               exp(predict(oregon.reg.list[[sc.temp]]))))
colnames(rawdata) <- c("time", "vol", "v.hat")


treepred <- as.data.frame(cbind(
  c(1:800),
  sapply(c(1:800), treeVol, siteclass = oregon.reg.list[[sc.temp]])
))
colnames(treepred) <- c("time", "vol")

tout <- as.data.frame(sapply(c(1:150), 
                             t.of.s, 
                             sc = oregon.reg.list[[sc.temp]]))

pred.t <- as.data.frame(t(
  rbind(unlist(tout[1,]),
        c(1:150))))
colnames(pred.t) <- c("time", "stock")


rp4<-ggplot(data = rawdata, aes(x = time, y =vol)) +
  geom_point(size = 2, shape = 1)+
  geom_line(data = treepred, aes(x = time, y =vol), 
            color = 'blue')+
  geom_line(data = pred.t, aes(x = time, y = stock), 
            color = 'red', linetype = 'dashed')+
  labs(x="Years", y="Volume")+
  xlim(min(c(0,rawdata$time)),max(rawdata$time))+
  ylim(min(c(0,rawdata$vol)), max(rawdata$vol))

plot_grid(rp1, rp2, rp3, rp4, ncol = 2)