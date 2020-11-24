## Read in and organize plot data
#load data
load("hvst_panel_07_R4.RData")
load("hvst_panel_07_R5.RData")

# select the harvested plots only
hvsted_07_R4 <- hvst_panel_07_R4 %>% #Individual
  filter(hvst == 1)
hvsted_07_R5 <- hvst_panel_07_R5 %>% #Corporate
  filter(hvst == 1)

# drop site class 6 and 7 because we don't use them and they give us NAN in ages.out values
hvsted_07_R4 <- hvsted_07_R4 %>% #Individual
  filter(siteclcd ==1|siteclcd ==2|siteclcd ==3|siteclcd ==4|siteclcd ==5)
hvsted_07_R5 <- hvsted_07_R5  %>% #Corporate
  filter(siteclcd ==1|siteclcd ==2|siteclcd ==3|siteclcd ==4|siteclcd ==5)

# throw away unusually small volume of less than 1 Pdvol
hvsted_07_R4 <- hvsted_07_R4 %>% 
  filter(Pdvol > 1)
hvsted_07_R5 <- hvsted_07_R5 %>% 
  filter(Pdvol > 1)

#calculate ages for plots
ages.out <- rep(0, size(hvsted_07_R4)[1])
prices.out <- rep(0, size(hvsted_07_R4)[1])
for(j in 1:size(hvsted_07_R4)[1]){
  sc.temp <- hvsted_07_R4$sitegroup[j]
  ages.out[j] <- t.of.s(hvsted_07_R4$Pdvol[j],
                        oregon.reg.list[[sc.temp]])$t.of.s
  prices.out[j] <- timberprice(hvsted_07_R4$Pdvol[j])
}
hvsted_07_R4 <- cbind(hvsted_07_R4, ages.out, prices.out)

ages.out <- rep(0, size(hvsted_07_R5)[1])
prices.out <- rep(0, size(hvsted_07_R5)[1])
for(j in 1:size(hvsted_07_R5)[1]){
  sc.temp <- hvsted_07_R5$sitegroup[j]
  ages.out[j] <- t.of.s(hvsted_07_R5$Pdvol[j],
                        oregon.reg.list[[sc.temp]])$t.of.s
  prices.out[j] <- timberprice(hvsted_07_R5$Pdvol[j])
}
hvsted_07_R5 <- cbind(hvsted_07_R5, ages.out, prices.out)

quantile(hvsted_07_R4$ages.out, probs = c(0, 0.005, 0.01, 0.5, 0.8, 0.9, 0.95, 0.99, 0.995, 1))
quantile(hvsted_07_R5$ages.out, probs = c(0, 0.005, 0.01, 0.5, 0.8, 0.9, 0.95, 0.99, 0.995, 1))

#drop the bottom and top half percent of observations

hvsted_07_R4 <- hvsted_07_R4 %>% #Individual site group 1
  filter(ages.out > quantile(hvsted_07_R4$ages.out, probs = 0.005))
hvsted_07_R4 <- hvsted_07_R4 %>% #Individual site group 1
  filter(ages.out < quantile(hvsted_07_R4$ages.out, probs = 0.995))

hvsted_07_R5 <- hvsted_07_R5 %>% #Individual site group 1
  filter(ages.out > quantile(hvsted_07_R5$ages.out, probs = 0.005))
hvsted_07_R5 <- hvsted_07_R5 %>% #Individual site group 1
  filter(ages.out < quantile(hvsted_07_R4$ages.out, probs = 0.995))

# subset it further into the site groups 
hvsted_R4_1 <- hvsted_07_R4 %>% #Individual site group 1
  filter(sitegroup == 1)
hvsted_R4_2 <- hvsted_07_R4 %>% #Individual site group 2
  filter(sitegroup == 2)
hvsted_R4_3 <- hvsted_07_R4 %>% #Individual site group 3
  filter(sitegroup == 3)
hvsted_R4_4 <- hvsted_07_R4 %>% #Individual site group 4
  filter(sitegroup == 4)
hvsted_R5_1 <- hvsted_07_R5 %>% #Corporate site group 1
  filter(sitegroup == 1)
hvsted_R5_2 <- hvsted_07_R5 %>% #Corporate site group 2
  filter(sitegroup == 2)
hvsted_R5_3 <- hvsted_07_R5 %>% #Corporate site group 3
  filter(sitegroup == 3)
hvsted_R5_4 <- hvsted_07_R5 %>% #Corporate site group 4
  filter(sitegroup == 4)


#find mean Pdvol
R4.1mvol <- as.data.frame(matrix(ncol  = 1, nrow = 1, mean(hvsted_R4_1$Pdvol)))
colnames(R4.1mvol) <- "Pdvol"
R4.2mvol <- as.data.frame(matrix(ncol  = 1, nrow = 1, mean(hvsted_R4_2$Pdvol)))
colnames(R4.2mvol) <- "Pdvol"
R4.3mvol <- as.data.frame(matrix(ncol  = 1, nrow = 1, mean(hvsted_R4_3$Pdvol)))
colnames(R4.3mvol) <- "Pdvol"
R4.4mvol <- as.data.frame(matrix(ncol  = 1, nrow = 1, mean(hvsted_R4_4$Pdvol)))
colnames(R4.4mvol) <- "Pdvol"
R5.1mvol <- as.data.frame(matrix(ncol  = 1, nrow = 1, mean(hvsted_R5_1$Pdvol)))
colnames(R5.1mvol) <- "Pdvol"
R5.2mvol <- as.data.frame(matrix(ncol  = 1, nrow = 1, mean(hvsted_R5_2$Pdvol)))
colnames(R5.2mvol) <- "Pdvol"
R5.3mvol <- as.data.frame(matrix(ncol  = 1, nrow = 1, mean(hvsted_R5_3$Pdvol)))
colnames(R5.3mvol) <- "Pdvol"
R5.4mvol <- as.data.frame(matrix(ncol  = 1, nrow = 1, mean(hvsted_R5_4$Pdvol)))
colnames(R5.4mvol) <- "Pdvol"


source("summary_stats.R")

