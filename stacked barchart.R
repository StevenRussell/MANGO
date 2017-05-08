


tb <- data.frame(cbind( c("Dose 1", "Dose 2"), referral.prop.table))
colnames(tb)[1] <- "Arm"
colnames(tb)[4] <- "Lost to Follow Up"
tb <- tb[,c(1, 3, 5, 6, 2, 4)]

sequential <- brewer.pal(4, "BuGn")

png(paste0(dir, 'stacked_barchart.png'), 12, 8, units='in', res=300)
  
par(mfrow=c(1, 1), oma=c(2, 2, 2, 2)) #, mar=c(0, 0, 0, 0))
    #, mgp=c(1.5, 0.5, 0)) mar=c(0.5, 3, 1, 1.5), 

sequential <- brewer.pal(6, "BuGn")

barplot(t(tb[,2:6]),
        names.arg = tb$arm, # x-axis labels
        cex.names = 1.5, # makes x-axis labels small enough to show all
        col = sequential[6:2], # colors
        #xlab = "Trial arm",
        #ylab = "Percentage of participants",
        xlim = c(0,3.5), # these two lines allow space for the legend
        width = 1,
        axes=F) # these two lines allow space for the legend
legend(x=2.7, y=.5, 
       legend = names(tb)[2:6], #in order from top to bottom
       fill = sequential[6:2], # 6:1 reorders so legend order matches graph
       title = "Classification",
       cex = 1.4)
mtext("Classification of Participants", 3, 3, cex = 2)
mtext("Percentage of Participants", 2, 3, cex = 1.5)
#mtext("Trial Arm", 1, 3, cex = 1.5)

#axis(1, at=c(1, 2),  line=-0.5, labels=c("Dose 1", "Dose 2"),
#     cex.axis=1.5, cex.lab=1.5)

axis(2, at=c(0, .2, .4, .6, .8, 1),  line=-0.5, labels=c("0%", "20%", "40%", "60%", "80%", "100%"),
     cex.axis=1.5, cex.lab=1.5)


#text(c(1,2), y=-10, 
#     adj=1, xpd=T, cex=1.3,
#     labels=c("Control", "Test"))

dev.off()
