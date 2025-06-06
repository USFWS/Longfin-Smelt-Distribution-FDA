
# Setup ####
library(tidyverse)
library(fda)
library(fda.usc)
library(fdasrvf) #installed from CRAN 2024-08-01
library(RColorBrewer)


# Function to create functional data object for predictions
fdata.pp <- function(bay.num, trend, gear){
  # rows = years = curves for FDA
  # columns = months
  # predicted probs (not derivatives) 
  # trend = c("lt", "seasonal")
  
  # subset & format data
  if (gear == "mwt"){
    temp.data <- mwt.ot.all.pred %>% filter(bay == bay.num) %>%
      dplyr::select(cohort, month36, fit.mwt)
    
    temp.data <- if(trend == "seasonal") spread(temp.data, month36, fit.mwt) else
      spread(temp.data, cohort, fit.mwt) 
  } else {
    temp.data <- mwt.ot.all.pred %>% 
      filter(bay == bay.num) %>%
      dplyr::select(cohort, month36, fit.ot)
    
    temp.data <- if(trend == "seasonal") spread(temp.data, month36, fit.ot) else
      spread(temp.data, cohort, fit.ot) 
  }
  
  # create functional data object
  temp.fda <- fdata(temp.data[,-1])
  
  #return fdata object
  return(temp.fda)
}

# Create Functional Data ####

# Midwater Trawl

# f = functional data
# s = seasonal trend
# number = bay number
mwt.fs1 <- fdata.pp(bay.num = 1, trend = "seasonal", gear = "mwt")
mwt.fs2 <- fdata.pp(bay.num = 2, trend = "seasonal", gear = "mwt")
mwt.fs3 <- fdata.pp(bay.num = 3, trend = "seasonal", gear = "mwt")
mwt.fs4 <- fdata.pp(bay.num = 4, trend = "seasonal", gear = "mwt")
mwt.fs5 <- fdata.pp(bay.num = 5, trend = "seasonal", gear = "mwt")


# Otter Trawl

# f = functional data
# s = seasonal trend
# number = bay number
ot.fs1 <- fdata.pp(bay.num = 1, trend = "seasonal", gear = "ot")
ot.fs2 <- fdata.pp(bay.num = 2, trend = "seasonal", gear = "ot")
ot.fs3 <- fdata.pp(bay.num = 3, trend = "seasonal", gear = "ot")
ot.fs4 <- fdata.pp(bay.num = 4, trend = "seasonal", gear = "ot")
ot.fs5 <- fdata.pp(bay.num = 5, trend = "seasonal", gear = "ot")


# Apply Time Warping ####

# Midwater Trawl
mwt.fs1.tw <- time_warping(f = t(mwt.fs1$data), time = 1:36)
mwt.fs2.tw <- time_warping(f = t(mwt.fs2$data), time = 1:36)
mwt.fs3.tw <- time_warping(f = t(mwt.fs3$data), time = 1:36)
mwt.fs4.tw <- time_warping(f = t(mwt.fs4$data), time = 1:36)
mwt.fs5.tw <- time_warping(f = t(mwt.fs5$data), time = 1:36)

# Otter Trawl
ot.fs1.tw <- time_warping(f = t(ot.fs1$data), time = 1:36)
ot.fs2.tw <- time_warping(f = t(ot.fs2$data), time = 1:36)
ot.fs3.tw <- time_warping(f = t(ot.fs3$data), time = 1:36)
ot.fs4.tw <- time_warping(f = t(ot.fs4$data), time = 1:36)
ot.fs5.tw <- time_warping(f = t(ot.fs5$data), time = 1:36)

# Plot phase variance ####

# Midwater Trawl
png("./Figures/phas_var_mwt.png",
    width = 11,
    height = 7,
    units = "in",
    res = 300)
par(cex = 1.5,
    mar = c(5.1, 4.1, 1.1, 1.1))
plot(1:36, 1:36,
     ylim = c(0, 0.0014),
     type = "b",
     main = "Midwater Trawl",
     xlab = "",
     ylab = "Variance",
     xaxt = "n")
for(i in 1:5){
  lines(1:36, apply(eval(parse(text = paste0("mwt.fs", i, ".tw$warping_functions"))), 1, FUN = var),
        #".tw$gam"))), 1, FUN = var), #warping_functions = gam
        lwd = 4,
        col = brewer.pal(5, "Set2")[i])
  points(1:36, apply(eval(parse(text = paste0("mwt.fs", i, ".tw$warping_functions"))), 1, FUN = var),
         #".tw$gam"))), 1, FUN = var),
         pch = 16,
         col = brewer.pal(5, "Set2")[i])
}
legend("topright",
       legend = bays$name[1:5],
       col = brewer.pal(5, "Set2"),
       lwd = 4,
       cex = 0.5)
#text(12, 0.0013, "Midwater Trawl", pos = 4, cex = 1)
# make axes more intuitive:
axis(side = 1,
     at = 1:36,
     labels = rep(month.abb, 3),
     las = 2)
# vertical lines to separate years:
## use segments instead of abline to control how far the lines go into the axis labels
segments(x0 = 12.52, y0 = 0.5, x1 = 12.52, y1 = -0.13,
         col = "grey", lty = 2, lwd = 3)
segments(x0 = 24.52, y0 = 0.5, x1 = 24.52, y1 = -0.13,
         col = "grey", lty = 2, lwd = 3)
text(0.1, 0.0013, "Age-0", pos = 4, cex = 1, col = "grey")
text(12.5, 0.0013, "Age-1", pos = 4, cex = 1, col = "grey")
text(24.5, 0.0013, "Age-2+", pos = 4, cex = 1, col = "grey")
dev.off()

# Otter Trawl
png("./Figures/phas_var_ot.png",
    width = 11,
    height = 7,
    units = "in",
    res = 300)
par(cex = 1.5,
    mar = c(5.1, 4.1, 1.1, 1.1))
plot(1:36, 1:36,
     ylim = c(0, 0.0014),
     type = "b",
     main = "Otter Trawl",
     xlab = "",
     ylab = "Variance",
     xaxt = "n")
for(i in 1:5){
  lines(1:36, apply(eval(parse(text = paste0("ot.fs", i, ".tw$warping_functions"))), 1, FUN = var),
        lwd = 4,
        col = brewer.pal(5, "Set2")[i])
  points(1:36, apply(eval(parse(text = paste0("ot.fs", i, ".tw$warping_functions"))), 1, FUN = var),
         pch = 16,
         col = brewer.pal(5, "Set2")[i])
}
legend("topright",
       legend = bays$name[1:5],
       col = brewer.pal(5, "Set2"),
       lwd = 4,
       cex = 0.5)
#text(13, 0.0013, "Otter Trawl", pos = 4, cex = 1)
# make axes more intuitive:
axis(side = 1,
     at = 1:36,
     labels = rep(month.abb, 3),
     las = 2)

# vertical lines to separate years:
## use segments instead of abline to control how far the lines go into the axis labels
segments(x0 = 12.52, y0 = 0.5, x1 = 12.52, y1 = -0.13,
         col = "grey", lty = 2, lwd = 3)
segments(x0 = 24.52, y0 = 0.5, x1 = 24.52, y1 = -0.13,
         col = "grey", lty = 2, lwd = 3)
text(0.1, 0.0013, "Age-0", pos = 4, cex = 1, col = "grey")
text(12.5, 0.0013, "Age-1", pos = 4, cex = 1, col = "grey")
text(24.5, 0.0013, "Age-2+", pos = 4, cex = 1, col = "grey")
dev.off()

# Plot mean functions for each bay on one plot ####

tiff(filename = "./Figures/mean_lines.tif",
     width = 7,
     height = 7,
     units = "in",
     res = 300)
par(cex = 1.5,
    mar = c(5.1, 4.1, 1.1, 1.1),
    mfrow = c(2, 1))
#### MWT ####
plot(1:36, 1:36,
     ylim = c(0, 0.5),
     type = "n",
     #main = "Midwater Trawl",
     xlab = "",
     ylab = "Predicted Probability",
     xaxt = "n")
abline(v = c(12.5, 24.5), col = "grey", lwd = 2, lty = 2)
for(i in 1:5)
{
  lines(1:36, eval(parse(text = paste0("mwt.fs", i, ".tw$fmean"))),
        lwd = 4,
        col = brewer.pal(5, "Set2")[i])
}
legend("right",
       legend = c("South SF Bay", "Central SF Bay", "San Pablo Bay", 
                  "Suisun Bay", "West Delta"), # bays$name[1:5],
       col = brewer.pal(5, "Set2"),
       lwd = 4,
       cex = 0.85)
text(0, 0.45, "Midwater Trawl", pos = 4, cex = 1)
text(0.1, 0.485, "Age-0", pos = 4, cex = 1, col = "grey")
text(12.5, 0.485, "Age-1", pos = 4, cex = 1, col = "grey")
text(24.5, 0.485, "Age-2+", pos = 4, cex = 1, col = "grey")
# Add month numbers to x-axis
axis(side = 1,
     at = 1:36,
     labels = paste0("(", 1:36, ")"),
     las = 2,
     line = NA,
     cex.axis = 0.75)
# Add month abbreviations to x-axis
axis(side = 1,
     at = 1:36,
     labels = rep(month.abb, 3),
     las = 2,
     line = 1.25,
     cex.axis = 0.9,
     tick = FALSE)

#### OT ####
plot(1:36, 1:36,
     ylim = c(0, 0.5),
     type = "n",
     #main = "Otter Trawl",
     xlab = "",
     ylab = "Predicted Probability",
     xaxt = "n")
abline(v = c(12.5, 24.5), col = "grey", lwd = 2, lty = 2)
for(i in 1:5)
{
  lines(1:36, eval(parse(text = paste0("ot.fs", i, ".tw$fmean"))),
        lwd = 4,
        col = brewer.pal(5, "Set2")[i])
}
legend("right",
       legend = c("South SF Bay", "Central SF Bay", "San Pablo Bay", 
                  "Suisun Bay", "West Delta"), # bays$name[1:5],
       col = brewer.pal(5, "Set2"),
       lwd = 4,
       cex = 0.85)
text(0, 0.45, "Otter Trawl", pos = 4, cex = 1)
text(0.1, 0.485, "Age-0", pos = 4, cex = 1, col = "grey")
text(12.5, 0.485, "Age-1", pos = 4, cex = 1, col = "grey")
text(24.5, 0.485, "Age-2+", pos = 4, cex = 1, col = "grey")
# Add month numbers to x-axis
axis(side = 1,
     at = 1:36,
     labels = paste0("(", 1:36, ")"),
     las = 2,
     line = NA,
     cex.axis = 0.75)
# Add month abbreviations to x-axis
axis(side = 1,
     at = 1:36,
     labels = rep(month.abb, 3),
     las = 2,
     line = 1.25,
     cex.axis = 0.9,
     tick = FALSE)
dev.off()

# Table of variances for amplitude and phase shifts for MWT & OT ####

data.frame(
  region = bays$name[1:5],
  mwt.ampvar = signif(c(mwt.fs1.tw$amplitude_variance,
                        mwt.fs2.tw$amplitude_variance,
                        mwt.fs3.tw$amplitude_variance,
                        mwt.fs4.tw$amplitude_variance,
                        mwt.fs5.tw$amplitude_variance), digits = 3),
  mwt.phasevar = signif(c(mwt.fs1.tw$phase_variance,
                          mwt.fs2.tw$phase_variance,
                          mwt.fs3.tw$phase_variance,
                          mwt.fs4.tw$phase_variance,
                          mwt.fs5.tw$phase_variance), digits = 3),
  ot.ampvar = signif(c(ot.fs1.tw$amplitude_variance,
                       ot.fs2.tw$amplitude_variance,
                       ot.fs3.tw$amplitude_variance,
                       ot.fs4.tw$amplitude_variance,
                       ot.fs5.tw$amplitude_variance), digits = 3),
  ot.phasevar = signif(c(ot.fs1.tw$phase_variance,
                         ot.fs2.tw$phase_variance,
                         ot.fs3.tw$phase_variance,
                         ot.fs4.tw$phase_variance,
                         ot.fs5.tw$phase_variance), digits = 3)
)


# Plots of FDA Components ####
# 3 panels: (see Marron 2015)
#  - Axes: x = time; y = prob
#  - 3 example years for one bay in each
# 1. Original GAM predictions
# 2. Amplitude functions - fn
# 3. Phase functions - warping_functions
#    - see warping function graphs above

# Color palette
warpLineCols <- colorRampPalette(c("#ADE8F4", "#03045E"))(29)

# Midwater Trawl ####
png("./Figures/PredAmpPhase_GraphsMWT.png",
    width = 8.5, height = 11, res = 300, units = "in")
par(mfcol = c(6, 3),
    mar = c(2, 3.5, 1, 1),
    oma = c(3, 3, 4, 0),
    xpd = TRUE)
# GAM predictions
for(i in 1:5){
  plot(1:36, seq(0, 1, length.out = 36),
       ylim = c(0, 1),
       type = "n",
       xlab = "",
       ylab = "")
  if(i == 1) mtext(side = 3, text = "Model Predictions", line = 0, cex = 1.5)
  mtext(side = 2, text = "Predicted Probability", line = 2, cex = 0.7)
  mtext(side = 3, text = bays$name[i], line = -2, cex = 0.9)
  if(i == 5) mtext(side = 1, text = "Month of Lifecycle", line = 2, cex = 0.7)
  y <- mwt.ot.all.pred[which(mwt.ot.all.pred$bay == i),]
  
  for(j in unique(y$cohort)){ # 29 columns
    lines(1:36, 
          y$fit.mwt[which(y$cohort == j)],
          col = warpLineCols[which(y$cohort == j)],
          lwd = 2)
    
  }
}
# blank plot as a spacer
plot(0, 0, type = "n", xlab = "", ylab = "", axes = FALSE)

# Aligned functions (amplitude)
# Aligned functions
# - illustrate amplitude
for(i in 1:5){
  plot(1:36, seq(0, 1, length.out = 36),
       ylim = c(0, 1),
       type = "n",
       xlab = "",
       ylab = "")
  if(i == 1) mtext(side = 3, text = "Aligned Functions", line = 0, cex = 1.5)
  mtext(side = 2, text = "Predicted Probability", line = 2, cex = 0.7)
  mtext(side = 3, text = bays$name[i], line = -2, cex = 0.9)
  if(i == 5) mtext(side = 1, text = "Month of Lifecycle", line = 2, cex = 0.7)
  y <- eval(parse(text = paste0("mwt.fs", i, ".tw$fn")))
  
  for(j in 1:29){ # 29 columns
    lines(1:36, 
          y[,j],
          col = warpLineCols[j],
          lwd = 2)
  }
}

# legend
xl <- 1
yb <- -4
xr <- 30
yt <- 2
plot(0, 0, type = "n", bty = "n",
     xlim = c(1, 36),
     ylim = c(-5, 7),
     xaxt = "n", yaxt = "n", xlab = "", ylab = "")

# horizontal ramp
rect(
  head(seq(xl, xr, (xr-xl)/29), -1), #xl,
  yb, #head(seq(yb,yt,(yt-yb)/29),-1),
  tail(seq(xl, xr, (xr-xl)/29), -1), #xr,
  yt, #tail(seq(yb,yt,(yt-yb)/29),-1),
  col=warpLineCols,
  border = NA)
rect(
  xl,
  yb,
  xr,
  yt,
  col=NA,
  border = "black"
)

text(labels = c(1987, 2015),
     x = c(xl, xr), # xr + 4,
     y = yb - 1, #c(yb, yt),
     las= 2,
     cex= 1)
text(x = ((xr - xl)/2)+xl,
     y = yt + 1.5,
     labels = "Year (Midwater Trawl)",
     cex = 1.25)

# Warping functions (phase var)
# par(mfrow = c(3, 2),
#     mar = c(2.5, 2.5, 2.5, 1),
#     oma = c(3, 3, 0, 0),
#     xpd = TRUE)

for(i in 1:5){ # Bays
  plot(1:36, seq(0, 1, length.out = 36),
       type = "n",
       ylim = c(-5, 7),
       xlab = "Month",
       ylab = "")
  if(i == 1) mtext(side = 3, text = "Warping Functions", line = 0, cex = 1.5)
  if(i == 5) mtext(side = 1, text = "Month of Lifecycle", line = 2, cex = 0.7)
  mtext(side = 3, text = bays$name[i], line = -2, cex = 0.9)
  y <- eval(parse(text = paste0("mwt.fs", i, ".tw$warping_functions")))
  for(j in 1:29){ # 29 columns
    lines(1:36, # cohorts
          y[,j]*35 - 0:35,
          col = warpLineCols[j],
          lwd = 2)
    mtext(side = 2, text = "Warping Distance (months)", line = 2, cex = 0.7)
  }
}
# blank plot as a spacer
plot(0, 0, type = "n", xlab = "", ylab = "", axes = FALSE)
dev.off()

# Otter Trawl ####
png("./Figures/PredAmpPhase_GraphsOT.png",
    width = 8.5, height = 11, res = 300, units = "in")
par(mfcol = c(6, 3),
    mar = c(2, 3.5, 1, 1),
    oma = c(3, 3, 4, 0),
    xpd = TRUE)
# GAM predictions
for(i in 1:5){
  plot(1:36, seq(0, 1, length.out = 36),
       ylim = c(0, 1),
       type = "n",
       xlab = "",
       ylab = "")
  if(i == 1) mtext(side = 3, text = "Model Predictions", line = 0, cex = 1.5)
  mtext(side = 2, text = "Predicted Probability", line = 2, cex = 0.7)
  mtext(side = 3, text = bays$name[i], line = -2, cex = 0.9)
  if(i == 5) mtext(side = 1, text = "Month of Lifecycle", line = 2, cex = 0.7)
  y <- mwt.ot.all.pred[which(mwt.ot.all.pred$bay == i),]
  
  for(j in unique(y$cohort)){ # 29 columns
    lines(1:36, 
          y$fit.ot[which(y$cohort == j)],
          col = warpLineCols[which(y$cohort == j)],
          lwd = 2)
    
  }
}
# blank plot as a spacer
plot(0, 0, type = "n", xlab = "", ylab = "", axes = FALSE)

# Aligned functions (amplitude)
# Aligned functions
# - illustrate amplitude
for(i in 1:5){
  plot(1:36, seq(0, 1, length.out = 36),
       ylim = c(0, 1),
       type = "n",
       xlab = "",
       ylab = "")
  if(i == 1) mtext(side = 3, text = "Aligned Functions", line = 0, cex = 1.5)
  mtext(side = 2, text = "Predicted Probability", line = 2, cex = 0.7)
  mtext(side = 3, text = bays$name[i], line = -2, cex = 0.9)
  if(i == 5) mtext(side = 1, text = "Month of Lifecycle", line = 2, cex = 0.7)
  y <- eval(parse(text = paste0("ot.fs", i, ".tw$fn")))
  
  for(j in 1:29){ # 29 columns
    lines(1:36, 
          y[,j],
          col = warpLineCols[j],
          lwd = 2)
  }
}

# legend
xl <- 1
yb <- -4
xr <- 30
yt <- 2
plot(0, 0, type = "n", bty = "n",
     xlim = c(1, 36),
     ylim = c(-5, 7),
     xaxt = "n", yaxt = "n", xlab = "", ylab = "")

# horizontal ramp
rect(
  head(seq(xl, xr, (xr-xl)/29), -1), #xl,
  yb, #head(seq(yb,yt,(yt-yb)/29),-1),
  tail(seq(xl, xr, (xr-xl)/29), -1), #xr,
  yt, #tail(seq(yb,yt,(yt-yb)/29),-1),
  col=warpLineCols,
  border = NA)
rect(
  xl,
  yb,
  xr,
  yt,
  col=NA,
  border = "black"
)

text(labels = c(1987, 2015),
     x = c(xl, xr), # xr + 4,
     y = yb - 1, #c(yb, yt),
     las= 2,
     cex= 1)
text(x = ((xr - xl)/2)+xl,
     y = yt + 1.5,
     labels = "Year (Otter Trawl)",
     cex = 1.25)

for(i in 1:5){ # Bays
  plot(1:36, seq(0, 1, length.out = 36),
       type = "n",
       ylim = c(-5, 7),
       xlab = "Month",
       ylab = "")
  if(i == 1) mtext(side = 3, text = "Warping Functions", line = 0, cex = 1.5)
  if(i == 5) mtext(side = 1, text = "Month of Lifecycle", line = 2, cex = 0.7)
  mtext(side = 3, text = bays$name[i], line = -2, cex = 0.9)
  y <- eval(parse(text = paste0("ot.fs", i, ".tw$warping_functions")))
  for(j in 1:29){ # 29 columns
    lines(1:36, # cohorts
          y[,j]*35 - 0:35,
          col = warpLineCols[j],
          lwd = 2)
    mtext(side = 2, text = "Warping Distance (months)", line = 2, cex = 0.7)
  }
}
# blank plot as a spacer
plot(0, 0, type = "n", xlab = "", ylab = "", axes = FALSE)
dev.off()
























