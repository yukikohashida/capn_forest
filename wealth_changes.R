#First consider changes in the amenity flow 
stands.at.age <- as.data.frame(read_xlsx("oreg_data.xlsx", col_names = TRUE))  #read in FIA data

site.sums <- as.data.frame(cbind(
  c(rep("Non-industrial", 4), rep("Industrial", 4)),
  as.numeric(c(1,2,3,4,1,2,3,4)),
  rep(0,8),
  rep(0,8)))
colnames(site.sums) <- c("owner.type", "Site.group", "vol.01.06", "vol.11.16")
site.sums$Site.group <- as.numeric(site.sums$Site.group)
site.sums$vol.01.06<- as.numeric(site.sums$vol.01.06)
site.sums$vol.11.16<- as.numeric(site.sums$vol.11.16)

for(j in 1: size(site.sums)[1]){
  temp <- subset(stands.at.age, Site.group ==  site.sums[j,2] & owner.type == site.sums[j,1])
  site.sums[j,3:4] <- colSums(temp[,6:7])
}

site.sums$vol.01.06 <- site.sums$vol.01.06/1000 # since the excel oreg_data volume is in board feet, convert to thousand board feet
site.sums$vol.11.16 <- site.sums$vol.11.16/1000

stargazer(site.sums, type = "text", summary = FALSE, 
          flip = FALSE, rownames = TRUE,
          title = "Sums of standing volume") 

#### Compute amenity value and total wealth for 1) mean harvest volume during 2001-2006 and 
#### 2) mean harvest volume during 2011-2016. Redo the run_forests with these new harvest volumes.

# Find 2001-2006 mean harvest volume
R4.1.01.06mvol <- as.data.frame(matrix(ncol  = 1, nrow = 1, mean(hvsted_R4_1[(hvsted_R4_1$year<2007
                                                                             &hvsted_R4_1$year>2000),"Pdvol"])))
colnames(R4.1.01.06mvol) <- "Pdvol"

R4.2.01.06mvol <- as.data.frame(matrix(ncol  = 1, nrow = 1, mean(hvsted_R4_2[(hvsted_R4_2$year<2007
                                                                              &hvsted_R4_2$year>2000),"Pdvol"])))
colnames(R4.2.01.06mvol) <- "Pdvol"

R4.3.01.06mvol <- as.data.frame(matrix(ncol  = 1, nrow = 1, mean(hvsted_R4_3[(hvsted_R4_3$year<2007
                                                                              &hvsted_R4_3$year>2000),"Pdvol"])))
colnames(R4.3.01.06mvol) <- "Pdvol"

R4.4.01.06mvol <- as.data.frame(matrix(ncol  = 1, nrow = 1, mean(hvsted_R4_4[(hvsted_R4_4$year<2007
                                                                              &hvsted_R4_4$year>2000),"Pdvol"])))
colnames(R4.4.01.06mvol) <- "Pdvol"

R5.1.01.06mvol <- as.data.frame(matrix(ncol  = 1, nrow = 1, mean(hvsted_R5_1[(hvsted_R5_1$year<2007
                                                                              &hvsted_R5_1$year>2000),"Pdvol"])))
colnames(R5.1.01.06mvol) <- "Pdvol"

R5.2.01.06mvol <- as.data.frame(matrix(ncol  = 1, nrow = 1, mean(hvsted_R5_2[(hvsted_R5_2$year<2007
                                                                              &hvsted_R5_2$year>2000),"Pdvol"])))
colnames(R5.2.01.06mvol) <- "Pdvol"

R5.3.01.06mvol <- as.data.frame(matrix(ncol  = 1, nrow = 1, mean(hvsted_R5_3[(hvsted_R5_3$year<2007
                                                                              &hvsted_R5_3$year>2000),"Pdvol"])))
colnames(R5.3.01.06mvol) <- "Pdvol"

R5.4.01.06mvol <- as.data.frame(matrix(ncol  = 1, nrow = 1, mean(hvsted_R5_4[(hvsted_R5_4$year<2007
                                                                              &hvsted_R5_4$year>2000),"Pdvol"])))
colnames(R5.4.01.06mvol) <- "Pdvol"

# Find 2011-2016 mean harvest volume
R4.1.11.16mvol <- as.data.frame(matrix(ncol  = 1, nrow = 1, mean(hvsted_R4_1[(hvsted_R4_1$year>2010),"Pdvol"])))
colnames(R4.1.11.16mvol) <- "Pdvol"

R4.2.11.16mvol <- as.data.frame(matrix(ncol  = 1, nrow = 1, mean(hvsted_R4_2[(hvsted_R4_2$year>2010),"Pdvol"])))
colnames(R4.2.11.16mvol) <- "Pdvol"

R4.3.11.16mvol <- as.data.frame(matrix(ncol  = 1, nrow = 1, mean(hvsted_R4_3[(hvsted_R4_3$year>2010),"Pdvol"])))
colnames(R4.3.11.16mvol) <- "Pdvol"

R4.4.11.16mvol <- as.data.frame(matrix(ncol  = 1, nrow = 1, mean(hvsted_R4_4[(hvsted_R4_4$year>2010),"Pdvol"])))
colnames(R4.4.11.16mvol) <- "Pdvol"

R5.1.11.16mvol <- as.data.frame(matrix(ncol  = 1, nrow = 1, mean(hvsted_R5_1[(hvsted_R5_1$year>2010),"Pdvol"])))
colnames(R5.1.11.16mvol) <- "Pdvol"

R5.2.11.16mvol <- as.data.frame(matrix(ncol  = 1, nrow = 1, mean(hvsted_R5_2[(hvsted_R5_2$year>2010),"Pdvol"])))
colnames(R5.2.11.16mvol) <- "Pdvol"

R5.3.11.16mvol <- as.data.frame(matrix(ncol  = 1, nrow = 1, mean(hvsted_R5_3[(hvsted_R5_3$year>2010),"Pdvol"])))
colnames(R5.3.11.16mvol) <- "Pdvol"

R5.4.11.16mvol <- as.data.frame(matrix(ncol  = 1, nrow = 1, mean(hvsted_R5_4[(hvsted_R5_4$year>2010),"Pdvol"])))
colnames(R5.4.11.16mvol) <- "Pdvol"

#get.dr is get discount rate, this takes most of the time. Set to FALSE to turn off.
out.R4.1.01.06mvol <- analyze.forests(R4.1.01.06mvol, site.c = 1, get.dr= TRUE, fguess = 50)  #individual site class 1
out.R4.2.01.06mvol <- analyze.forests(R4.2.01.06mvol, site.c = 2, get.dr= TRUE, fguess = 50)  #individual site class 2
out.R4.3.01.06mvol <- analyze.forests(R4.3.01.06mvol, site.c = 3, get.dr= TRUE, fguess = 50)  #indidivual site class 3
out.R4.4.01.06mvol <- analyze.forests(R4.4.01.06mvol, site.c = 4, get.dr= TRUE, fguess = 50)  #indidivual site class 4

out.R5.1.01.06mvol <- analyze.forests(R5.1.01.06mvol, site.c = 1, get.dr= TRUE, fguess = 50)  #corporate site class 1
out.R5.2.01.06mvol <- analyze.forests(R5.2.01.06mvol, site.c = 2, get.dr= TRUE, fguess = 50)  #corporate site class 2
out.R5.3.01.06mvol <- analyze.forests(R5.3.01.06mvol, site.c = 3, get.dr= TRUE, fguess = 50)  #corporate site class 3
out.R5.4.01.06mvol <- analyze.forests(R5.4.01.06mvol, site.c = 4, get.dr= TRUE, fguess = 50)  #corporate site class 4

out.R4.1.11.16mvol <- analyze.forests(R4.1.11.16mvol, site.c = 1, get.dr= TRUE, fguess = 50)  #individual site class 1
out.R4.2.11.16mvol <- analyze.forests(R4.2.11.16mvol, site.c = 2, get.dr= TRUE, fguess = 50)  #individual site class 2
out.R4.3.11.16mvol <- analyze.forests(R4.3.11.16mvol, site.c = 3, get.dr= TRUE, fguess = 50)  #indidivual site class 3
out.R4.4.11.16mvol <- analyze.forests(R4.4.11.16mvol, site.c = 4, get.dr= TRUE, fguess = 50)  #indidivual site class 4

out.R5.1.11.16mvol <- analyze.forests(R5.1.11.16mvol, site.c = 1, get.dr= TRUE, fguess = 50)  #corporate site class 1
out.R5.2.11.16mvol <- analyze.forests(R5.2.11.16mvol, site.c = 2, get.dr= TRUE, fguess = 50)  #corporate site class 2
out.R5.3.11.16mvol <- analyze.forests(R5.3.11.16mvol, site.c = 3, get.dr= TRUE, fguess = 50)  #corporate site class 3
out.R5.4.11.16mvol <- analyze.forests(R5.4.11.16mvol, site.c = 4, get.dr= TRUE, fguess = 50)  #corporate site class 4

amen.out.01.06 <- as.data.frame(cbind(out.R4.1.01.06mvol$amenity,
                                      out.R4.2.01.06mvol$amenity,
                                      out.R4.3.01.06mvol$amenity,
                                      out.R4.4.01.06mvol$amenity,
                                      out.R5.1.01.06mvol$amenity,
                                      out.R5.2.01.06mvol$amenity,
                                      out.R5.3.01.06mvol$amenity,
                                      out.R5.4.01.06mvol$amenity))
colnames(amen.out.01.06) <- c("individual.site.1",
                              "individual.site.2",
                              "individual.site.3",
                              "individual.site.4",
                              "corporate.site.1",
                              "corporate.site.2",
                              "corporate.site.3",
                              "corporate.site.4")

amen.out.11.16 <- as.data.frame(cbind(out.R4.1.11.16mvol$amenity,
                                      out.R4.2.11.16mvol$amenity,
                                      out.R4.3.11.16mvol$amenity,
                                      out.R4.4.11.16mvol$amenity,
                                      out.R5.1.11.16mvol$amenity,
                                      out.R5.2.11.16mvol$amenity,
                                      out.R5.3.11.16mvol$amenity,
                                      out.R5.4.11.16mvol$amenity))
colnames(amen.out.11.16) <- c("individual.site.1",
                              "individual.site.2",
                              "individual.site.3",
                              "individual.site.4",
                              "corporate.site.1",
                              "corporate.site.2",
                              "corporate.site.3",
                              "corporate.site.4")

amen.vec.01.06 <- c(t(amen.out.01.06[1:4]), t(amen.out.01.06[5:8]))
amen.vec.11.16 <- c(t(amen.out.11.16[1:4]), t(amen.out.11.16[5:8]))
site.sums <- cbind(site.sums, site.sums$vol.01.06 * amen.vec.01.06 , site.sums$vol.11.16 * amen.vec.11.16 )
colnames(site.sums) <-c("Owner.type", "Site.group", "vol.01.06", "vol.11.16", "Amenity.01.06", "Amenity.11.16")

temp <- cbind("Totals",NA,t(colSums(site.sums[,3:6])))
colnames(temp) <- colnames(site.sums)
site.sums <- rbind(site.sums,temp)

stargazer(site.sums, type = "text", summary = FALSE, 
          flip = FALSE, rownames = TRUE,
          title = "Sums of standing volume") 

write_excel_csv(site.sums, "savedoutput/amenity_change.xls")


#########################################################################################
#now do the wealth changes. 

age.class <- as.numeric(levels(as.factor(stands.at.age$age.group)))
age.class[9] <- 81


res.comb <- list(out.R4.1.01.06mvol$results,
                 out.R4.2.01.06mvol$results,
                 out.R4.3.01.06mvol$results,
                 out.R4.4.01.06mvol$results,
                 out.R5.1.01.06mvol$results,
                 out.R5.2.01.06mvol$results,
                 out.R5.3.01.06mvol$results,
                 out.R5.4.01.06mvol$results,
                 out.R4.1.11.16mvol$results,
                 out.R4.2.11.16mvol$results,
                 out.R4.3.11.16mvol$results,
                 out.R4.4.11.16mvol$results,
                 out.R5.1.11.16mvol$results,
                 out.R5.2.11.16mvol$results,
                 out.R5.3.11.16mvol$results,
                 out.R5.4.11.16mvol$results)
                 

vf.comp <- function(res.set, site.class){
  temp <- as.data.frame(t.of.s(res.comb[[res.set]][,1], oregon.reg.list[[site.class]])$t.of.s)
  colnames(temp) <- "age"
  temp.el <- rep(0, 9)
  for (j in 1:9){
    temp1 <- which.min(abs(temp$age - age.class[j]))
    temp.el[j] <- res.comb[[res.set]][temp1,4]
  
  }

  my.result <- cbind(rep(res.set,9),
                     rep(site.class,9),
                     age.class, 
                     temp.el)
  return(my.result)
}

val.table <- rbind(vf.comp(1,1), 
                  vf.comp(2,2),
                  vf.comp(3,3),
                  vf.comp(4,4),
                  vf.comp(5,1), 
                  vf.comp(6,2),
                  vf.comp(7,3),
                  vf.comp(8,4))

val.table <- cbind(val.table, rbind(vf.comp(9,1),vf.comp(10,2),vf.comp(11,3),vf.comp(12,4),vf.comp(13,1), 
                                    vf.comp(14,2),vf.comp(15,3),vf.comp(16,4))) 

size(val.table)
val.table[-31,]
saa <- cbind(stands.at.age, val.table[-31,c(3:4,8)])
colnames(saa) <- c(colnames(stands.at.age),"age.class", "Vf.01.06","Vf.11.16")

val.table <- cbind(saa$area.11.16 - saa$area.01.06,
                   saa$area.11.16*saa$Vf.11.16 - saa$area.01.06*saa$Vf.01.06)

val.table <- as.data.frame(cbind(rbind(saa[,1:5], c("Total", NA, NA, sum(saa$area.01.06), sum(saa$area.11.16))),
                          rbind(val.table, cbind(NA, sum(val.table[,2])))))

colnames(val.table) <- c("owner.type", "Site.group", "age.group", "area.01.06",
                         "area.11.16", "Change.acres", "Change.in.value")

stargazer(val.table, type = "text", summary = FALSE, 
          flip = FALSE, rownames = TRUE,
          title = "Change in area and value") 


write_excel_csv(val.table, "savedoutput/value_change.xls")
