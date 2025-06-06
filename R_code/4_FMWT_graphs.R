



library(readxl)
library(RColorBrewer)

options(scipen = 10)


fmwt <- read_xlsx("./Data_Raw/FMWT_lfs_index.xlsx",
                  sheet = 1,
                  col_types = "numeric")
names(fmwt)

wyIndex <- read_xlsx("./Data_Raw/WY_Index_through2018.xlsx",
                     sheet = 2)

fmwt.plot <- fmwt[fmwt$Year > 1986, ]
fmwt.plot <- merge(fmwt.plot, wyIndex[, c("WY", "Yr_type")],
                   by.x = "Year", by.y = "WY",
                   all.x = TRUE)
fmwt.plot$Yr_type <- factor(fmwt.plot$Yr_type,
                            levels = c("C", "D", "BN", "AN", "W"))
color.table <- data.frame(Yr_type = levels(fmwt.plot$Yr_type),
                          color = brewer.pal(n = 5, "Blues"))

# add colors to the dataframe for plotting
fmwt.plot<- merge(x = fmwt.plot, y = color.table,
                  by = "Yr_type",
                  all.x = TRUE)

# COMBINED FIGURE
png("./Figures/fmwt_ALL.png", height = 1600, width = 1200, 
    units = "px", res = 90)
par(mfrow = c(5, 1),
    mar = c(2.1, 4.1, 1.1, 2.1),
    cex = 1.25)
# TOTAL INDEX
plot(fmwt.plot$Year, fmwt.plot$Total+ 0.02*fmwt.plot$Total, #+20 makes an outline on the top of the bar
     type = "h", lwd = 10, lend = 1,
     yaxs = "i",
     xaxt = "n", yaxt = "n",
     log = "y",
     xlab = "Year", ylab = "Abundance Index",
     xlim = c(1987, 2016),
     ylim = c(1, 100000),
     cex = 1.25)
for(i in levels(fmwt.plot$Yr_type)){
  lines(fmwt.plot[which(fmwt.plot$Yr_type == i),]$Year, 
        fmwt.plot[which(fmwt.plot$Yr_type == i),]$Total,
        type = "h", lwd = 8, lend = 1,
        col = brewer.pal(n = 5, "Blues")[which(levels(fmwt.plot$Yr_type) == i)])
}
axis(side = 1, at = seq(1970, 2010, by = 5), cex = 1.25)
axis(side = 2, at = c(10, 100, 1000, 10000, 100000), 
     labels = c(10, 100, 1000, 10000, 100000),
     las = 2, cex = 1.25)
legend("topright", title = "Water Year Type",
       legend = levels(fmwt.plot$Yr_type),
       fill = brewer.pal(n = 5, "Blues"),
       cex = 0.85, h = TRUE)
text(1990, 20000, "FMWT Total Index", cex = 1.25)

# SEPTEMBER
plot(fmwt.plot$Year, 
     fmwt.plot$Sept+ 0.03*fmwt.plot$Sept, #+20 makes an outline on the top of the bar
     type = "h", lwd = 10, lend = 1,
     yaxs = "i",
     xaxt = "n", yaxt = "n",
     log = "y",
     xlab = "Year", ylab = "Abundance Index",
     xlim = c(1987, 2016),
     ylim = c(1, 100000),
     cex = 1.25)
for(i in levels(fmwt.plot$Yr_type)){
  lines(fmwt.plot[which(fmwt.plot$Yr_type == i),]$Year, 
        fmwt.plot[which(fmwt.plot$Yr_type == i),]$Sept,
        type = "h", lwd = 8, lend = 1,
        col = brewer.pal(n = 5, "Blues")[which(levels(fmwt.plot$Yr_type) == i)])
}
axis(side = 1, at = seq(1970, 2010, by = 5), cex = 1.25)
axis(side = 2, at = c(10, 100, 1000, 10000, 100000), 
     labels = c(10, 100, 1000, 10000, 100000),
     las = 2, cex = 1.25)
text(1990, 20000, "FMWT September Index", cex = 1.25)

#OCTOBER
plot(fmwt.plot$Year, 
     fmwt.plot$Oct+ 0.03*fmwt.plot$Oct, #+20 makes an outline on the top of the bar
     type = "h", lwd = 10, lend = 1,
     yaxs = "i",
     xaxt = "n", yaxt = "n",
     log = "y",
     xlab = "Year", ylab = "Abundance Index",
     xlim = c(1987, 2016),
     ylim = c(1, 100000),
     cex = 1.25)
for(i in levels(fmwt.plot$Yr_type)){
  lines(fmwt.plot[which(fmwt.plot$Yr_type == i),]$Year, 
        fmwt.plot[which(fmwt.plot$Yr_type == i),]$Oct,
        type = "h", lwd = 8, lend = 1,
        col = brewer.pal(n = 5, "Blues")[which(levels(fmwt.plot$Yr_type) == i)])
}
axis(side = 1, at = seq(1970, 2010, by = 5), cex = 1.25)
axis(side = 2, at = c(10, 100, 1000, 10000, 100000), 
     labels = c(10, 100, 1000, 10000, 100000),
     las = 2, cex = 1.25)
text(1990, 20000, "FMWT October Index", cex = 1.25)

#NOVEMBER
plot(fmwt.plot$Year, 
     fmwt.plot$Nov+ 0.03*fmwt.plot$Nov, #+20 makes an outline on the top of the bar
     type = "h", lwd = 10, lend = 1,
     yaxs = "i",
     xaxt = "n", yaxt = "n",
     log = "y",
     xlab = "Year", ylab = "Abundance Index",
     xlim = c(1987, 2016),
     ylim = c(1, 100000),
     cex = 1.25)
for(i in levels(fmwt.plot$Yr_type)){
  lines(fmwt.plot[which(fmwt.plot$Yr_type == i),]$Year, 
        fmwt.plot[which(fmwt.plot$Yr_type == i),]$Nov,
        type = "h", lwd = 8, lend = 1,
        col = brewer.pal(n = 5, "Blues")[which(levels(fmwt.plot$Yr_type) == i)])
}
axis(side = 1, at = seq(1970, 2010, by = 5), cex = 1.25)
axis(side = 2, at = c(10, 100, 1000, 10000, 100000), 
     labels = c(10, 100, 1000, 10000, 100000),
     las = 2, cex = 1.25)
text(1990, 20000, "FMWT November Index", cex = 1.25)

#DECEMBER
plot(fmwt.plot$Year, 
     fmwt.plot$Dec+ 0.03*fmwt.plot$Dec, #+20 makes an outline on the top of the bar
     type = "h", lwd = 10, lend = 1,
     yaxs = "i",
     xaxt = "n", yaxt = "n",
     log = "y",
     xlab = "Year", ylab = "Abundance Index",
     xlim = c(1987, 2016),
     ylim = c(1, 100000),
     cex = 1.25)
for(i in levels(fmwt.plot$Yr_type)){
  lines(fmwt.plot[which(fmwt.plot$Yr_type == i),]$Year, 
        fmwt.plot[which(fmwt.plot$Yr_type == i),]$Dec,
        type = "h", lwd = 8, lend = 1,
        col = brewer.pal(n = 5, "Blues")[which(levels(fmwt.plot$Yr_type) == i)])
}
axis(side = 1, at = seq(1970, 2010, by = 5), cex = 1.25)
axis(side = 2, at = c(10, 100, 1000, 10000, 100000), 
     labels = c(10, 100, 1000, 10000, 100000),
     las = 2, cex = 1.25)
text(1990, 20000, "FMWT December Index", cex = 1.25)

dev.off()