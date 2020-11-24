# Draw graphs of yield curves and growth curves to present in Appendix


#yield curve for each site class
size.me <- 7.5/2.2
#pdf(file = "figsave/vol.age_curves.pdf", width = 1.61*size.me, height = size.me)
ggplot() +
  geom_line(data=trees.temp1, aes(x=time, y=vol), color='black') +
  geom_line(data=trees.temp2, aes(x=time, y=vol), color='blue') +
  geom_line(data=trees.temp3, aes(x=time, y=vol), color='red') +
  geom_line(data=trees.temp4, aes(x=time, y=vol), color='brown') +
  labs( 
    x= "Years",
    y = "Volume (thousand board feet)")  +
  theme(  #http://ggplot2.tidyverse.org/reference/theme.html
    axis.line = element_line(color = "black"), 
    axis.text=element_text(size=10),
    axis.title=element_text(size=10,face="bold"),
    legend.position = c(0.6,0.4),
    legend.text = element_text(size=10),
    panel.background = element_rect(fill = "transparent",color = NA),
    plot.background = element_rect(fill = "transparent",color = NA)
  )
dev.off()

#incremental growth on volume
size.me <- 7.5/2.2
#pdf(file = "figsave/growth.vol_curves.pdf", width = 1.61*size.me, height = size.me)
growth1 <- ggplot() +
  geom_line(data = trees.grow1, aes(x = vol, y=dvol),color = 'black')+
  labs(title="Site group 1",x="Volume", y="Volume Growth")+
  ylim(0,1.5)+
  theme(  #http://ggplot2.tidyverse.org/reference/theme.html
    plot.title = element_text(size=10),
    axis.line = element_line(color = "black"), 
    axis.text=element_text(size=10),
    axis.title=element_text(size=10,face="bold"),
    legend.position = c(0.6,0.4),
    legend.text = element_text(size=10),
    panel.background = element_rect(fill = "transparent",color = NA),
    plot.background = element_rect(fill = "transparent",color = NA)
  )
growth2 <- ggplot() +
  geom_line(data = trees.grow2, aes(x = vol, y=dvol),color = 'blue')+
  labs(title="Site group 2", x="Volume", y="Volume Growth")+
  ylim(0,1.5)+
  theme(  #http://ggplot2.tidyverse.org/reference/theme.html
    plot.title = element_text(size=10),
    axis.line = element_line(color = "black"), 
    axis.text=element_text(size=10),
    axis.title=element_text(size=10,face="bold"),
    legend.position = c(0.6,0.4),
    legend.text = element_text(size=10),
    panel.background = element_rect(fill = "transparent",color = NA),
    plot.background = element_rect(fill = "transparent",color = NA)
  )
growth3 <- ggplot() +
  geom_line(data = trees.grow3, aes(x = vol, y=dvol),color = 'red')+
  labs(title="Site group 3", x="Volume", y="Volume Growth")+
  ylim(0,1.5)+
  theme(  #http://ggplot2.tidyverse.org/reference/theme.html
    plot.title = element_text(size=10),
    axis.line = element_line(color = "black"),
    axis.text=element_text(size=10),
    axis.title=element_text(size=10,face="bold"),
    legend.position = c(0.6,0.4),
    legend.text = element_text(size=10),
    panel.background = element_rect(fill = "transparent",color = NA),
    plot.background = element_rect(fill = "transparent",color = NA)
  )
growth4 <- ggplot() +
  geom_line(data = trees.grow4, aes(x = vol, y=dvol),color = 'brown')+
  labs(title="Site group 4", x="Volume", y="Volume Growth")+
  ylim(0,1.5)+
  theme(  #http://ggplot2.tidyverse.org/reference/theme.html
    plot.title = element_text(size=10),
    axis.line = element_line(color = "black"), 
    axis.text=element_text(size=10),
    axis.title=element_text(size=10,face="bold"),
    legend.position = c(0.6,0.4),
    legend.text = element_text(size=10),
    panel.background = element_rect(fill = "transparent",color = NA),
    plot.background = element_rect(fill = "transparent",color = NA)
  )

plot_grid(growth1, growth2, growth3, growth4, ncol = 2)
#dev.off()

