

faust.r<-list()
#get Faustmann results. 
for(sc.now in 1:4){
  faust.r[[sc.now]] <- faust.results(sc.now)
  }

r.table2 <- as.data.frame(rbind(
  t(faust.r[[1]]["age"]),
  t(faust.r[[2]]["age"]),
  t(faust.r[[3]]["age"]),
  t(faust.r[[4]]["age"])
))
rownames(r.table2) <- c("site.class.1",
                        "site.class.2",
                        "site.class.3",
                        "site.class.4")
colnames(r.table2) <- c("harvest.age.F7", "havest.age.F3")

temp <- as.data.frame(rbind(
  t(faust.r[[1]][2]),
  t(faust.r[[2]][2]),
  t(faust.r[[3]][2]),
  t(faust.r[[4]][2])
 ))
colnames(temp) <- c("harvest.volume.F7", "havest.volume.F3")
r.table2 <- cbind(r.table2, temp)
rm(temp)



r.table2 <- cbind(r.table2, 
      t(sum.stats[16,1:4]), t(sum.stats[16,6:9]),
      t(sum.stats[15,1:4]), t(sum.stats[15,6:9]),
      t(amen.out[1:4]),
      t(amen.out[5:8]),
      t(r.rates[1:4]),
      t(r.rates[5:8])
      )
colnames(r.table2) <- c("Faustmann.harvest.age.07", 
                        "Faustmann.harvest.age.03",
                        "Faustmann.harvest.volume.07",
                        "Faustmann.harvest.volume.03",
                        "Observed.age.non.industrial",
                        "Observed.age.industrial",
                        "Observed.harvest.non.industrial",
                        "Observed.harvest.industrial",
                        "Amenity.non.industrial",
                        "Amenity.industrial",
                        "Discount.rate.non.industrial",
                        "Discount.rate.industrial")
is.data.frame(r.table2)

stargazer(r.table2, type = "text", summary = FALSE, 
          flip = TRUE, rownames = TRUE,
          title = "Table 2 results") 
#write_excel_csv(r.table2, "savedoutput/table2.xls")
