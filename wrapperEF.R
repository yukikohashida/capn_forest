######################################################################################
# Wrapper for natural capital of forest model
# a private DF forest owner who participates in the small for Western Oregon
# tract forestland program (STF) that gives him a tax deferral until harvest.
# Updated: 12/18/2018 Eli Fenichel based on Yukiko's version as of 12/18/2018
######################################################################################
########################################################################################
if (!require("pacman")) install.packages("pacman")
p_load(capn, pracma, nleqslv, ggplot2, memisc, optimbase,
       tidyverse, stargazer, foreach,doParallel, doSNOW, cowplot)

rm(list = ls())

source("capn_v_split.R")    #auxillary functions for capn to handle the repeat stopping nature of the economic program
# call the basic model functions
# loop for each site class for R4 owner type, but first run the base model that defines parameters and growth equations for each site class

## Modeled parameters
source("parameters.R")  #holds tree growth function, price function, observed harvested volume

# Save tree yield and growth curves
source("tree_plots.R")

# Universal assumptions that are identical across site classes
paramAll$delta <- 0.07 #discount rate 
paramAll$order <- 400
paramAll$lowerK <- 1 
paramAll$upperK <- 80
paramAll$nodes <- 400
paramAll$cost <- 150 #simulated harvest cost per MBF with Fuel Reduction Cost Simulator
paramAll$tax1 <- 3.53 #timber product harvest tax per thousand board feet for all forest owners in 2014; uniform across site classes
paramAll$tax2 <- 0 #severance tax per thousand board feet for small tract forestland program owners in 2014; ; set 0
paramAll$ptax1 <- 0 #per-acre property tax that forestland program foresters pay; set zero as it's invariant to volume  
#paramAll$ptax2 <- c(2.00,1.58,1.32,1.13,0.73) #per-acre property tax that small tract forestland program owners must pay 

#############################################################################################

## Capn parameters
#since this is the same for all versions of the model I have pulled it out of the loop.
Aspace <- aproxdef(paramAll$order[1],paramAll$lowerK[1],paramAll$upperK[1],paramAll$delta[1])
Snodes <- chebnodegen(paramAll$nodes[1],paramAll$lowerK[1],paramAll$upperK[1]) 
############################################################################################

## Read in and organize plot data
source("sitedata.R")
#   hvsted_R4_1 is individual site group 1
#   hvsted_R4_2 is individual site group 2
#   hvsted_R4_3 is individual site group 3
#   hvsted_R4_4 is individual site group 4

#   hvsted_R5_1 is corporate site group 1
#   hvsted_R5_2 is corporate site group 2
#   hvsted_R5_3 is corporate site group 3
#   hvsted_R5_4 is corporate site group 4


#   means follow the naming convention RX.Ymvol 
#   can check regression output using 
source("check_reg.R")
plot_grid(rp1, rp2, rp3, rp4, ncol = 2)

## Forest analysis
source("run_forests.R")


# These are runs for the average behavior
# get.dr is get discount rate, this takes most of the time. Set to FALSE to turn off. 
# fguess is the inital guess for the Faustmann rotation. The default is 20.
out.R4.1mvol <- analyze.forests(R4.1mvol, site.c = 1, get.dr= TRUE, fguess = 50)  #individual site class 1
out.R4.2mvol <- analyze.forests(R4.2mvol, site.c = 2, get.dr= TRUE, fguess = 50)  #individual site class 2
out.R4.3mvol <- analyze.forests(R4.3mvol, site.c = 3, get.dr= TRUE, fguess = 50)  #indidivual site class 3
out.R4.4mvol <- analyze.forests(R4.4mvol, site.c = 4, get.dr= TRUE, fguess = 50)  #indidivual site class 4

out.R5.1mvol <- analyze.forests(R5.1mvol, site.c = 1, get.dr= TRUE, fguess = 50)  #corporate site class 1
out.R5.2mvol <- analyze.forests(R5.2mvol, site.c = 2, get.dr= TRUE, fguess = 50)  #corporate site class 2
out.R5.3mvol <- analyze.forests(R5.3mvol, site.c = 3, get.dr= TRUE, fguess = 50)  #corporate site class 3
out.R5.4mvol <- analyze.forests(R5.4mvol, site.c = 4, get.dr= TRUE, fguess = 50)  #corporate site class 4

r.rates<-as.data.frame(cbind(out.R4.1mvol$r.delta,
                             out.R4.2mvol$r.delta,
                             out.R4.3mvol$r.delta,
                             out.R4.4mvol$r.delta,
                             out.R5.1mvol$r.delta,
                             out.R5.2mvol$r.delta,
                             out.R5.3mvol$r.delta,
                             out.R5.4mvol$r.delta))

colnames(r.rates) <- c("individual.site.1",
                       "individual.site.2",
                       "individual.site.3",
                       "individual.site.4",
                       "corporate.site.1",
                       "corporate.site.2",
                       "corporate.site.3",
                       "corporate.site.4")

stargazer(r.rates, type = "text", summary = FALSE, 
          flip = TRUE, rownames = TRUE,
          title = "Discount rates that rationalize the Faustmann rotation")   

# hold off here
amen.out<-as.data.frame(cbind(out.R4.1mvol$amenity,
                             out.R4.2mvol$amenity,
                             out.R4.3mvol$amenity,
                             out.R4.4mvol$amenity,
                             out.R5.1mvol$amenity,
                             out.R5.2mvol$amenity,
                             out.R5.3mvol$amenity,
                             out.R5.4mvol$amenity))

colnames(amen.out) <- c("individual.site.1",
                       "individual.site.2",
                       "individual.site.3",
                       "individual.site.4",
                       "corporate.site.1",
                       "corporate.site.2",
                       "corporate.site.3",
                       "corporate.site.4")

# stargazer(amen.out, type = "text", summary = FALSE,
#           flip = TRUE, rownames = TRUE,
#           title = "Ammenity value")
# 
# write_excel_csv(rbind(r.rates,amen.out), "savedoutput/rate_amen.xls")

source("results2.R")
source("wealth_changes.R")


##################################################################################
## This section loops over all data and has been commented out completely. 

# loop through all the data to generate results for every record.  
# This currently takes about 38 hours and is commented out when not needed.
# need to make sure that upperK is set above maximum Pdvol
# paramAll$upperK[1] <- max(c(80, 1.15*hvsted_R4_1$Pdvol, 1.15*hvsted_R5_1$Pdvol ) )
# paramAll$upperK[2] <- max(c(80, 1.15*hvsted_R4_2$Pdvol, 1.15*hvsted_R5_2$Pdvol ) )
# paramAll$upperK[3] <- max(c(80, 1.15*hvsted_R4_3$Pdvol, 1.15*hvsted_R5_3$Pdvol ) )
# paramAll$upperK[4] <- max(c(80, 1.15*hvsted_R4_4$Pdvol, 1.15*hvsted_R5_4$Pdvol ) )
# 
# 
# source("run_all.R")
# 
# #put things back - should have been done better.
# paramAll$upperK[1] <- 80
# Aspace <- aproxdef(paramAll$order[1],paramAll$lowerK[1],paramAll$upperK[1],paramAll$delta[1])
# Snodes <- chebnodegen(paramAll$nodes[1],paramAll$lowerK[1],paramAll$upperK[1])

# Summary of shadow price results.  process_all.R is a bit slow so the result is saved as mepaa. 

# source("process_all.R")
# mepaa <- readRDS("savedoutput/mepaa.rds")
# 
# stargazer(mepaa, type = "text", summary = FALSE, 
#           flip = TRUE, rownames = TRUE,
#           title = "Shadow price at age",
#           column.labels = c("private 1","private 2","private 3","private 4", 
#                             "corporate 1","corporate 2","corporate 3","corporate 4"))   
# 
# #The data on stand type by age is saved on Oregon_capn/econprog_files/fia_vol_acre_summary_02.xlsx.
# #"summary" tab has graphs; "OUTEST2" is the original data; "pivot2" is a pivot table that looks more pleasing to your eyes. 
# levels(hvsted_R4_1$SDesc)



#########################################################################################################
## Plotting 

# Make histogram of harvest ages, need to added the file to save a new version. 
# The save function is currently commented out.

# source("all_plot_out_v02.R")

# Need new histogram code.  In the file more_hist.R
source("more_hist.R")


## Assign out data set corporate plots site class 5.3
# Might need to adjust the axes for different plots
# Code to save the figures is commented out.
graph1 <- prep.graphs(out.R5.3mvol)
graph2 <- prep.graphs(out.R5.3.01.06mvol)
graph3 <- prep.graphs(out.R5.3.11.16mvol)

f.node <- graph1$faust.node[1,1]

attach(graph1)  #be sure to detach when done
#shadow price
size.me <- 7.5/2.2
#pdf(file = "figsave/sp53.pdf", width = 1.61*size.me, height = size.me)
ggplot() +
  geom_line(data = results, aes(x= stock, y = marg.prof),
            color = 'grey') +
  geom_line(data = results, aes(x = stock, y = mean.econ.price),
            color = 'blue') +
  geom_line(data = results[1:f.node ,], aes(x = stock, y = mean.faust.price),
            color = 'black') +
  #  geom_line(data = price.at.mean[1:101,], aes(x = stock, y = price), # for the plot selected at "price.at.mean", look up a node in econ.node file
  #            color = 'blue') + 
  geom_line(data = at.t, aes(x= stock, y = price),
            color = 'blue', linetype = "dashed") +
  geom_line(data = at.t.F[1:f.node ,], aes(x= stock, y = price),
            color = 'black', linetype = "dashed") +
  #  geom_line(data = at.mean.t[1:101,], aes(x= stock, y = price),
  #            color = 'blue', linetype = "dashed") +
  labs( 
    x= "forest volume (MBF)",
    y = "shadow price ($/MBF)")  +
  xlim(0,25) +
  ylim(0,1000) +
  theme(  #http://ggplot2.tidyverse.org/reference/theme.html
    axis.line = element_line(color = "black"), 
    panel.background = element_rect(fill = "transparent",color = NA),
    plot.background = element_rect(fill = "transparent",color = NA)
  )
dev.off()


# Value function plot (intertemporal welfare)
size.me <- 7.5/2.2
#pdf(file = "figsave/v53.pdf", width = 1.61*size.me, height = size.me)
stop.here <- max(which(graph1$results[["mean.econ.price"]] > 0))
ggplot() +
  geom_line(data = results[1:stop.here,], aes(x = stock, y = mean.econ.vfun),
            color = 'blue') +
  geom_line(data = results[1:f.node,], aes(x = stock, y = mean.faust.vfun),
            color = 'black') +
  # geom_line(data = v.at.t[1:129,], aes(x= stock, y = value),
  #           color = 'blue', linetype = "dashed") +
  # geom_line(data = v.at.t.F[1:colMeans(faust.node),], aes(x= stock, y = value),
  #           color = 'black', linetype = "dashed") +
  labs(
    x= "forest volume (MBF)",
    y = "Value ($/acre)")  +
  xlim(0,25) +
  ylim(0, graph1$results[["mean.econ.vfun"]][stop.here]*1.25)+
  theme(  #http://ggplot2.tidyverse.org/reference/theme.html
    axis.line = element_line(color = "black"), 
    panel.background = element_rect(fill = "transparent",color = NA),
    plot.background = element_rect(fill = "transparent",color = NA)
  )
dev.off()
detach(graph1)

# Three shadow price curves, 1) average, 2) 2001-2006, and 3) 2011-2016
graphall <- as.data.frame(cbind(graph1["results"], graph2[["results"]][,2:6], graph3[["results"]][,2:6]))
colnames(graphall) <- c("stock", "mean.econ.price1", "mean.faust.price1", "mean.econ.vfun1", "mean.faust.vfun1", 
                        "marg.prof1", "mean.econ.price2", "mean.faust.price2", "mean.econ.vfun2", "mean.faust.vfun2",
                        "marg.prof2", "mean.econ.price3", "mean.faust.price3", "mean.econ.vfun3", "mean.faust.vfun3",
                        "marg.prof3")
at.t.all <- as.data.frame(cbind(graph1[["at.t"]][,1:2],graph2[["at.t"]][,2],graph3[["at.t"]][,2]))
colnames(at.t.all) <- c("stock", "price1","price2","price3")

at.t.F.all <- as.data.frame(cbind(graph1[["at.t.F"]][,1:2],graph2[["at.t.F"]][,2],graph3[["at.t.F"]][,2]))
colnames(at.t.F.all) <- c("stock", "price1","price2","price3")

size.me <- 7.5/2.2


# With the legend
ggplot() +
  geom_line(data = graphall, aes(x= stock, y = marg.prof1,color = "Marginal profit")) +
  geom_line(data = graphall, aes(x = stock, y = mean.econ.price1, color = "Average shadow price")) +
  geom_line(data = graphall, aes(x = stock, y = mean.econ.price2, color = "Shadow price 2001-2006")) +
  geom_line(data = graphall, aes(x = stock, y = mean.econ.price3, color = "Shadow price 2011-2014")) +
  geom_line(data = graphall[1:f.node ,], aes(x = stock, y = mean.faust.price1, color = "Faustmann shadow price")) +
  scale_color_manual(name='',values = c('Marginal profit'='gray', 'Average shadow price'='blue',
                                        'Shadow price 2001-2006'='brown', 'Shadow price 2011-2014'='cyan4',
                                        'Faustmann shadow price'='black'))+
  geom_line(data = at.t.all, aes(x= stock, y = price1),
            color = 'blue', linetype = "dashed") +
  geom_line(data = at.t.all, aes(x= stock, y = price2),
            color = 'brown', linetype = "dashed") +
  geom_line(data = at.t.all, aes(x= stock, y = price3),
            color = 'cyan4', linetype = "dashed") +
  geom_line(data = at.t.F.all[1:f.node ,], aes(x= stock, y = price1),
            color = 'black', linetype = "dashed") +
  
  labs( 
    x= "forest volume (MBF)",
    y = "shadow price ($/MBF)")  +
  xlim(0,25) +
  ylim(0,1000) +
  theme(  #http://ggplot2.tidyverse.org/reference/theme.html
    axis.line = element_line(color = "black"), 
    legend.position = c(0.6,0.4),
    legend.text = element_text(size=10),
    panel.background = element_rect(fill = "transparent",color = NA),
    plot.background = element_rect(fill = "transparent",color = NA)
  )

dev.off()
