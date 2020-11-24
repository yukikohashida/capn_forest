if (!require("pacman")) install.packages("pacman")
p_load(cowplot, gridExtra, grid, ggplot2, lattice)


h.vols <- list(hvsted_R4_1$Pdvol,
                    hvsted_R4_2$Pdvol,
                    hvsted_R4_3$Pdvol,
                    hvsted_R4_4$Pdvol,
                    hvsted_R5_1$Pdvol,
                    hvsted_R5_2$Pdvol,
                    hvsted_R5_3$Pdvol,
                    hvsted_R5_4$Pdvol)


for(j in 1:8){
  df.temp <- as.data.frame(unlist(h.vols[[j]]))
  colnames(df.temp) <- "harv.vol"
  tplot <- 
    ggplot(data = df.temp, aes(x = harv.vol))+
    geom_histogram(binwidth = 3) +
    #scale_x_continuous(breaks = seq(0, 60, 10)) +
    #xlim(c(0,75)) +
    labs(x = "Harvest Volume",
         y = "Frequency") +
    theme(  #http://ggplot2.tidyverse.org/reference/theme.html
      axis.line = element_line(color = "black"), 
      panel.background = element_rect(fill = "transparent",color = NA),
      plot.background = element_rect(fill = "transparent",color = NA)
    )
  assign(paste("hist",j, sep=""), tplot)
  
}
plot_grid(hist1, hist2, hist3, hist4, hist5, hist6, hist7, hist8, ncol = 4)


#################################################################################

#The reason this is not working is that there are sites that have volumes greater
#than the asymptote.  
site.now <- c(c(1:4),c(1:4))
for(j in 1:8){
  jx <- site.now[j]
  sc.now <- oregon.reg.list[[jx]]
  temp.out <- lapply(h.vols[[j]],
                     t.of.s,
                     sc = sc.now)
  age.out <- rep(0, length(temp.out))
  for(jj in 1:length(temp.out)){
    age.out[jj] <- temp.out[[jj]]$t.of.s
  }
  
  df.age.out <- as.data.frame(age.out)
  colnames(df.age.out) <- "harv.age"
  
  colMeans(df.age.out)
  
  tplot <- 
    ggplot(data = df.age.out, aes(x = harv.age))+
    geom_histogram(binwidth=3) +
    #scale_x_continuous(breaks = seq(0, 60, 10)) +
    #xlim(c(0,75)) +
    #labs(x = "Harvest age",
    #     y = "Frequency") +
    theme(  #http://ggplot2.tidyverse.org/reference/theme.html
      axis.line = element_line(color = "black"), 
      panel.background = element_rect(fill = "transparent",color = NA),
      plot.background = element_rect(fill = "transparent",color = NA),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text=element_text(size=12)
    )
  assign(paste("hist",j, sep=""), tplot)
}
#The commented out text will generate a pdf of the figure. 
size.me <- 7.5/2.2
#pdf(file = "figsave/hists.pdf", width = 2.2*size.me, height = size.me)
plot.age <- plot_grid(hist5, hist6, hist7, hist8, ncol = 4)

#create common x and y labels

y.grob <- textGrob("Frequency", 
                   gp=gpar(fontface="bold", col="black", fontsize=15), rot=90)

x.grob <- textGrob("Harvest age", 
                   gp=gpar(fontface="bold", col="black", fontsize=15))

#add to plot

grid.arrange(arrangeGrob(plot.age, left = y.grob, bottom = x.grob))
dev.off()


