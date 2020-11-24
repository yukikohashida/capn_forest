
harvest.units <- c(size(hvsted_R4_1)[1], size(hvsted_R4_2)[1], size(hvsted_R4_3)[1], size(hvsted_R4_4)[1],
  size(hvsted_07_R4)[1],
  size(hvsted_R5_1)[1], size(hvsted_R5_2)[1], size(hvsted_R5_3)[1], size(hvsted_R5_4)[1],
  size(hvsted_07_R5)[1])
percent.harv.units <- as.numeric(
  c(harvest.units[1:4]/size(hvsted_07_R4)[1],"", harvest.units[6:9]/size(hvsted_07_R5)[1], ""))


acres.data <- rbind( #10-99
  c(table(hvsted_R4_1$SDesc)[[4]], 
  table(hvsted_R4_2$SDesc)[[4]], 
  table(hvsted_R4_3$SDesc)[[4]],
  table(hvsted_R4_4$SDesc)[[4]],
  table(hvsted_R4_1$SDesc)[[4]]+
    table(hvsted_R4_2$SDesc)[[4]]+
    table(hvsted_R4_3$SDesc)[[4]]+
    table(hvsted_R4_4$SDesc)[[4]],
  table(hvsted_R5_1$SDesc)[[4]], 
  table(hvsted_R5_2$SDesc)[[4]], 
  table(hvsted_R5_3$SDesc)[[4]],
  table(hvsted_R5_4$SDesc)[[4]],
  table(hvsted_R5_1$SDesc)[[4]]+
    table(hvsted_R5_2$SDesc)[[4]]+
    table(hvsted_R5_3$SDesc)[[4]]+
    table(hvsted_R5_4$SDesc)[[4]]
  ), #100-499
  c(table(hvsted_R4_1$SDesc)[[5]], 
    table(hvsted_R4_2$SDesc)[[5]], 
    table(hvsted_R4_3$SDesc)[[5]],
    table(hvsted_R4_4$SDesc)[[5]],
    table(hvsted_R4_1$SDesc)[[5]]+
      table(hvsted_R4_2$SDesc)[[5]]+
      table(hvsted_R4_3$SDesc)[[5]]+
      table(hvsted_R4_4$SDesc)[[5]],
    table(hvsted_R5_1$SDesc)[[5]], 
    table(hvsted_R5_2$SDesc)[[5]], 
    table(hvsted_R5_3$SDesc)[[5]],
    table(hvsted_R5_4$SDesc)[[5]],
    table(hvsted_R5_1$SDesc)[[5]]+
      table(hvsted_R5_2$SDesc)[[5]]+
      table(hvsted_R5_3$SDesc)[[5]]+
      table(hvsted_R5_4$SDesc)[[5]]
  ), #500-999
  c(table(hvsted_R4_1$SDesc)[[8]], 
    table(hvsted_R4_2$SDesc)[[8]], 
    table(hvsted_R4_3$SDesc)[[8]],
    table(hvsted_R4_4$SDesc)[[8]],
    table(hvsted_R4_1$SDesc)[[8]]+
      table(hvsted_R4_2$SDesc)[[8]]+
      table(hvsted_R4_3$SDesc)[[8]]+
      table(hvsted_R4_4$SDesc)[[8]],
    table(hvsted_R5_1$SDesc)[[8]], 
    table(hvsted_R5_2$SDesc)[[8]], 
    table(hvsted_R5_3$SDesc)[[8]],
    table(hvsted_R5_4$SDesc)[[8]],
    table(hvsted_R5_1$SDesc)[[8]]+
      table(hvsted_R5_2$SDesc)[[8]]+
      table(hvsted_R5_3$SDesc)[[8]]+
      table(hvsted_R5_4$SDesc)[[8]]
  ), #1000-4,999
  c(table(hvsted_R4_1$SDesc)[[3]], 
     table(hvsted_R4_2$SDesc)[[3]], 
     table(hvsted_R4_3$SDesc)[[3]],
     table(hvsted_R4_4$SDesc)[[3]],
     table(hvsted_R4_1$SDesc)[[3]]+
       table(hvsted_R4_2$SDesc)[[3]]+
       table(hvsted_R4_3$SDesc)[[3]]+
       table(hvsted_R4_4$SDesc)[[3]],
     table(hvsted_R5_1$SDesc)[[3]], 
     table(hvsted_R5_2$SDesc)[[3]], 
     table(hvsted_R5_3$SDesc)[[3]],
     table(hvsted_R5_4$SDesc)[[3]],
     table(hvsted_R5_1$SDesc)[[3]]+
       table(hvsted_R5_2$SDesc)[[3]]+
       table(hvsted_R5_3$SDesc)[[3]]+
       table(hvsted_R5_4$SDesc)[[3]]
  ), #5000+
  c(table(hvsted_R4_1$SDesc)[[6]]+table(hvsted_R4_1$SDesc)[[7]], 
    table(hvsted_R4_2$SDesc)[[6]]+table(hvsted_R4_2$SDesc)[[7]], 
    table(hvsted_R4_3$SDesc)[[6]]+table(hvsted_R4_3$SDesc)[[7]],
    table(hvsted_R4_4$SDesc)[[6]]+table(hvsted_R4_4$SDesc)[[7]],
    table(hvsted_R4_1$SDesc)[[6]]+table(hvsted_R4_1$SDesc)[[7]]+
      table(hvsted_R4_2$SDesc)[[6]]+table(hvsted_R4_2$SDesc)[[7]]+
      table(hvsted_R4_3$SDesc)[[6]]+table(hvsted_R4_3$SDesc)[[7]]+
      table(hvsted_R4_4$SDesc)[[6]]+table(hvsted_R4_4$SDesc)[[7]],
    table(hvsted_R5_1$SDesc)[[6]]+table(hvsted_R5_1$SDesc)[[7]], 
    table(hvsted_R5_2$SDesc)[[6]]+table(hvsted_R5_2$SDesc)[[7]], 
    table(hvsted_R5_3$SDesc)[[6]]+table(hvsted_R5_3$SDesc)[[7]],
    table(hvsted_R5_4$SDesc)[[6]]+table(hvsted_R5_4$SDesc)[[7]],
    table(hvsted_R5_1$SDesc)[[6]]+table(hvsted_R5_1$SDesc)[[7]]+
      table(hvsted_R5_2$SDesc)[[6]]+table(hvsted_R5_2$SDesc)[[7]]+
      table(hvsted_R5_3$SDesc)[[6]]+table(hvsted_R5_3$SDesc)[[7]]+
      table(hvsted_R5_4$SDesc)[[6]]+table(hvsted_R5_4$SDesc)[[7]]
  )
)

mean.elevation <- c(mean(hvsted_R4_1$elev), mean(hvsted_R4_2$elev), mean(hvsted_R4_3$elev), mean(hvsted_R4_4$elev),
      mean(hvsted_07_R4$elev),
      mean(hvsted_R5_1$elev),mean(hvsted_R5_2$elev),mean(hvsted_R5_3$elev), mean(hvsted_R5_4$elev),
     mean(hvsted_07_R5$elev))
    
mean.acres<- c(mean(hvsted_R4_1$ActAcreage), mean(hvsted_R4_2$ActAcreage), mean(hvsted_R4_3$ActAcreage), mean(hvsted_R4_4$ActAcreage),
                mean(hvsted_07_R4$ActAcreage),
                mean(hvsted_R5_1$ActAcreage),mean(hvsted_R5_2$ActAcreage),mean(hvsted_R5_3$ActAcreage), mean(hvsted_R5_4$ActAcreage),
                mean(hvsted_07_R5$ActAcreage))

mean.harvest<- c(R4.1mvol$Pdvol, R4.2mvol$Pdvol, R4.3mvol$Pdvol,R4.4mvol$Pdvol, mean(hvsted_07_R4$Pdvol),
                 R5.1mvol$Pdvol, R5.2mvol$Pdvol, R5.3mvol$Pdvol,R5.4mvol$Pdvol, mean(hvsted_07_R5$Pdvol))
mean.harvest.age <- 
  c(mean(hvsted_R4_1$ages.out), mean(hvsted_R4_2$ages.out), mean(hvsted_R4_3$ages.out), mean(hvsted_R4_4$ages.out),
    mean(hvsted_07_R4$ages.out),
    mean(hvsted_R5_1$ages.out),mean(hvsted_R5_2$ages.out),mean(hvsted_R5_3$ages.out), mean(hvsted_R5_4$ages.out),
    mean(hvsted_07_R5$ages.out))


mean.harvest.price<-
  c(mean(hvsted_R4_1$prices.out), mean(hvsted_R4_2$prices.out), mean(hvsted_R4_3$prices.out), mean(hvsted_R4_4$prices.out),
  mean(hvsted_07_R4$prices.out),
  mean(hvsted_R5_1$prices.out),mean(hvsted_R5_2$prices.out),mean(hvsted_R5_3$prices.out), mean(hvsted_R5_4$prices.out),
  mean(hvsted_07_R5$prices.out))


sum.stats<-rbind(harvest.units, 
      percent.harv.units, 
      acres.data, 
      prop.table(acres.data, margin = 2),
      mean.acres,
      mean.elevation,
      mean.harvest,
      mean.harvest.age,
      mean.harvest.price
      )
 

sum.stats<- as.data.frame(sum.stats, row.names = TRUE)
colnames(sum.stats) <- c("NIF.site.1","NIF.site.2","NIF.site.3","NIF.site.4","NIF.site.Total",
                         "Ind.site.1","Ind.site.2","Ind.site.3","Ind.site.4","Ind.site.Total")
rownames(sum.stats) <- c("harvest.units", "percent.harvest.units","acres.10-99", "acreas.100-499",
                          "acres.500-999","acres.1000-4999","acres5000",
                         "percent.acres.10-99", "percent.acreas.100-499",
                         "percent.acres.500-999","percent.acres.1000-4999","percent.acres5000",
                         "mean.acres","mean.elevation",
                          "mean.harvest.volume","mean.harvest.age","mean.havest.price")
#write_excel_csv(sum.stats, "savedoutput/sum_stats.xls")
stargazer(sum.stats, type = "text", summary = FALSE, 
          flip = FALSE, rownames = TRUE,
          title = "Summary Statistics")  

