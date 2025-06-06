

# Setup ####
library(doBy)
library(mgcv)

# Table of bay names and numbers for reference
bays <- data.frame(num = 1:7,
                   name = c("South SF Bay", "Central SF Bay", "San Pablo Bay", "Suisun Bay", 
                            "West Delta (confluence)", "Sacramento River", "San Joaquin River"))



# NOTES: 
# 1. These models may take a long time to run. If you aren't interested in 
#    comparing possible models, and only want to run the model used for the FDA
#    component of the manuscript, you only need to run mwt.ot.all.3. Then you can
#    skip down to the predictions section of this script.
# 2. The GAMs are set up for parallel processing to speed them up a little.
#    Before running them, you may need to adjust the number of threads in the
#    control lists to fit the number of available cores in your computer's
#    processor.


# Find the best k for month36 ####
Sys.time()
mwt.ot.all.17 <- gam(present ~ 
                       s(month36, by = bay, k = 4) ####
                     + s(cohort, by = bay, k = 10)
                     + ti(month36, cohort, by = bay)
                     + gear*bay,
                     data = mwt.ot.co[which(mwt.ot.co$series %in% c(1,2)),],
                     family = binomial,
                     link = logit,
                     control=list(nthreads = 12),
                     method = "REML")
Sys.time()

mwt.ot.all.18 <- gam(present ~ 
                       s(month36, by = bay, k = 7) ####
                     + s(cohort, by = bay, k = 9)
                     + ti(month36, cohort, by = bay)
                     + gear*bay,
                     data = mwt.ot.co[which(mwt.ot.co$series %in% c(1,2)),],
                     family = binomial,
                     link = logit,
                     control=list(nthreads = 12),
                     method = "REML")
Sys.time()

mwt.ot.all.3 <- gam(present ~ 
                      s(month36, by = bay, k = 11) ####  #seasonal timestep, given months where detections don't happen
                    + s(cohort, by = bay, k = 9)
                    + ti(month36, cohort, by = bay)
                    + gear*bay,
                    data = mwt.ot.co[which(mwt.ot.co$series %in% c(1,2)),],
                    family = binomial,
                    link = logit,
                    control=list(nthreads = 12),
                    method = "REML")
Sys.time()

mwt.ot.all <- gam(present ~ 
                    s(month36, by = bay, k = 15)
                  + s(cohort, by = bay, k = 9)
                  + ti(month36, cohort, by = bay)
                  + gear*bay,
                  data = mwt.ot.co[which(mwt.ot.co$series %in% c(1,2)),],
                  family = binomial,
                  link = logit,
                  control=list(nthreads = 12),
                  method = "REML")
Sys.time()

mwt.ot.all.19 <- gam(present ~ 
                       s(month36, by = bay, k = 22) ####
                     + s(cohort, by = bay, k = 9)
                     + ti(month36, cohort, by = bay)
                     + gear*bay,
                     data = mwt.ot.co[which(mwt.ot.co$series %in% c(1,2)),],
                     family = binomial,
                     link = logit,
                     control=list(nthreads = 12),
                     method = "REML")
Sys.time()

mwt.ot.all.2 <- gam(present ~ 
                      s(month36, by = bay, k = 24) ####  #monthly timestep, given months where detections don't happen
                    + s(cohort, by = bay, k = 9)
                    + ti(month36, cohort, by = bay)
                    + gear*bay,
                    data = mwt.ot.co[which(mwt.ot.co$series %in% c(1,2)),],
                    family = binomial,
                    link = logit,
                    control=list(nthreads = 12),
                    method = "REML")
Sys.time()

pdf("chooseK_month36.pdf", 
    height = 9, width = 7.5,
    paper = "letter")
for(i in 1:5){
  par(mfrow = c(3, 2),
      mar = c(2.1, 1.1, 0.1, 1.1), #origional = 5.1 4.1 4.1 2.1
      oma = c(1.5, 3.1, 1.5, 0),
      cex = 1.1)
  plot.gam(mwt.ot.all.17, select = i,
           xlab = "",
           ylab ="",
           ylim = c(-16, 5),
           lwd = 2)
  text("k = 4", x = 33, y =-12)
  #abline(v = c(12.5, 24.5), lwd = 1, col = "grey")
  plot.gam(mwt.ot.all.18, select = i,
           xlab = "",
           ylab ="",
           ylim = c(-16, 5),
           lwd = 2)
  text("k = 7", x = 33, y =-12)
  plot.gam(mwt.ot.all.3, select = i,
           xlab = "",
           ylab ="",
           ylim = c(-16, 5),
           lwd = 2)
  text("k = 11", x = 33, y =-12)
  plot.gam(mwt.ot.all, select = i,
           xlab = "",
           ylab ="",
           ylim = c(-16, 5),
           lwd = 2)
  text("k = 15", x = 33, y =-12)
  plot.gam(mwt.ot.all.19, select = i,
           xlab = "",
           ylab ="",
           ylim = c(-16, 5),
           lwd = 2)
  text("k = 22", x = 33, y =-12)
  plot.gam(mwt.ot.all.2, select = i,
           xlab = "",
           ylab ="",
           ylim = c(-16, 5),
           lwd = 2)
  text("k = 24", x = 33, y =-12)
  mtext("s(month)", side = 2, line = 1.5, outer = TRUE, cex = 1.1)
  mtext(side = 1, line = 0, "Month", outer = TRUE, cex = 1.1)
  mtext(side = 3, line = 0.5, bays$name[i], outer = TRUE, cex = 1.1)
}
dev.off()




# Find the best k for cohort ####
Sys.time()
mwt.ot.all.6 <- gam(present ~ 
                      s(month36, by = bay, k = 11)
                    + s(cohort, by = bay, k = 3)  #### minimum number of knots
                    + ti(month36, cohort, by = bay)
                    + gear*bay,
                    data = mwt.ot.co[which(mwt.ot.co$series %in% c(1,2)),],
                    family = binomial,
                    link = logit,
                    control=list(nthreads = 12),
                    method = "REML")
Sys.time()

Sys.time()
mwt.ot.all.7 <- gam(present ~ 
                      s(month36, by = bay, k = 11)
                    + s(cohort, by = bay, k = 6)  #### max number of knots
                    + ti(month36, cohort, by = bay)
                    + gear*bay,
                    data = mwt.ot.co[which(mwt.ot.co$series %in% c(1,2)),],
                    family = binomial,
                    link = logit,
                    control=list(nthreads = 12),
                    method = "REML")
Sys.time()

Sys.time()
mwt.ot.all.8 <- gam(present ~ 
                      s(month36, by = bay, k = 11)
                    + s(cohort, by = bay, k = 9)  #### 
                    + ti(month36, cohort, by = bay)
                    + gear*bay,
                    data = mwt.ot.co[which(mwt.ot.co$series %in% c(1,2)),],
                    family = binomial,
                    link = logit,
                    control=list(nthreads = 12),
                    method = "REML")
Sys.time()

mwt.ot.all.9 <- gam(present ~ 
                      s(month36, by = bay, k = 11)
                    + s(cohort, by = bay, k = 15)  #### 
                    + ti(month36, cohort, by = bay)
                    + gear*bay,
                    data = mwt.ot.co[which(mwt.ot.co$series %in% c(1,2)),],
                    family = binomial,
                    link = logit,
                    control=list(nthreads = 12),
                    method = "REML")
Sys.time()

mwt.ot.all.10 <- gam(present ~ 
                       s(month36, by = bay, k = 11)
                     + s(cohort, by = bay, k = 21)  #### 
                     + ti(month36, cohort, by = bay)
                     + gear*bay,
                     data = mwt.ot.co[which(mwt.ot.co$series %in% c(1,2)),],
                     family = binomial,
                     link = logit,
                     control=list(nthreads = 12),
                     method = "REML")
Sys.time()

mwt.ot.all.11 <- gam(present ~ 
                       s(month36, by = bay, k = 11)
                     + s(cohort, by = bay, k = 27)  ####
                     + ti(month36, cohort, by = bay)
                     + gear*bay,
                     data = mwt.ot.co[which(mwt.ot.co$series %in% c(1,2)),],
                     family = binomial,
                     link = logit,
                     control=list(nthreads = 12),
                     method = "REML")
Sys.time()


pdf("chooseK_cohort.pdf", 
    height = 9, width = 7.5,
    paper = "letter")
for(i in 6:10){
  par(mfrow = c(3, 2),
      mar = c(2.1, 1.1, 0.1, 1.1), #origional = 5.1 4.1 4.1 2.1
      oma = c(1.5, 3.1, 1.5, 0),
      cex = 1.1)
  plot.gam(mwt.ot.all.6, select = i,
           xlab = "",
           ylab ="",
           ylim = c(-5, 5),
           lwd = 2)
  text("k = 3", x = 2010, y =-3.75)
  #abline(v = c(12.5, 24.5), lwd = 1, col = "grey")
  plot.gam(mwt.ot.all.7, select = i,
           xlab = "",
           ylab ="",
           ylim = c(-5, 5),
           lwd = 2)
  text("k = 6", x = 2010, y =-3.75)
  plot.gam(mwt.ot.all.8, select = i,
           xlab = "",
           ylab ="",
           ylim = c(-5, 5),
           lwd = 2)
  text("k = 9", x = 2010, y =-3.75)
  plot.gam(mwt.ot.all.9, select = i,
           xlab = "",
           ylab ="",
           ylim = c(-5, 5),
           lwd = 2)
  text("k = 15", x = 2010, y =-3.75)
  plot.gam(mwt.ot.all.10, select = i,
           xlab = "",
           ylab ="",
           ylim = c(-5, 5),
           lwd = 2)
  text("k = 21", x = 2010, y =-3.75)
  plot.gam(mwt.ot.all.11, select = i,
           xlab = "",
           ylab ="",
           ylim = c(-5, 5),
           lwd = 2)
  text("k = 27", x = 2010, y =-3.75)
  mtext("s(cohort)", side = 2, line = 1.5, outer = TRUE, cex = 1.1)
  mtext(side = 1, line = 0, "Cohort", outer = TRUE, cex = 1.1)
  mtext(side = 3, line = 0.5, bays$name[i-5], outer = TRUE, cex = 1.1)
}
dev.off()


# MAKE NEW PREDICTIONS ####
#make data.frames for "newdata"
newDF.mot.mwt <- data.frame(cohort = rep(1987:2015, times = 36*5), 
                            month36 = rep(1:36, each = length(1987:2015), times = 5), 
                            bay = rep(1:5, each = length(1987:2015)*36),
                            bay.name = rep(bays$name[1:5], each = length(1987:2015)*36),
                            gear = "mwt")
newDF.mot.ot <- data.frame(cohort = rep(1987:2015, times = 36*5), 
                           month36 = rep(1:36, each = length(1987:2015), times = 5), 
                           bay = rep(1:5, each = length(1987:2015)*36),
                           bay.name = rep(bays$name[1:5], each = length(1987:2015)*36),
                           gear = "ot")
#make data.frame to hold predictions and predictors
mwt.ot.all.pred <- newDF.mot.mwt[,1:4]
#make predictions for Midwater Trawl:
mwt.ot.all.pred$fit.mwt <- NA
mwt.ot.all.pred$se.mwt <- NA
mwt.ot.all.pred$fit.mwt <- as.vector(predict(mwt.ot.all.3, 
                                             newdata = newDF.mot.mwt,
                                             type = "response", se.fit=TRUE)$fit)
mwt.ot.all.pred$se.mwt <- as.vector(predict(mwt.ot.all.3, 
                                            newdata = newDF.mot.mwt,
                                            type = "response", se.fit=TRUE)$se.fit)

#just to convince myself that the linkinv function gives the same results as type = "response"
# mwt.ot.all$family$linkinv(predict(mwt.ot.all, 
#                                   newdata = newDF.mot.mwt,
#                                   type = "link", se.fit=TRUE)$fit)
mwt.ot.all.pred$mwt.lcl <- NA
mwt.ot.all.pred$mwt.ucl <- NA
mwt.ot.all.pred$mwt.lcl <- mwt.ot.all$family$linkinv(predict(mwt.ot.all.3, 
                                                             newdata = newDF.mot.mwt, 
                                                             type = "link", se.fit=TRUE)$fit - 1.96*predict(mwt.ot.all, 
                                                                                                            newdata = newDF.mot.mwt, 
                                                                                                            type = "link", se.fit=TRUE)$se.fit)
mwt.ot.all.pred$mwt.ucl <- mwt.ot.all$family$linkinv(predict(mwt.ot.all.3, 
                                                             newdata = newDF.mot.mwt, 
                                                             type = "link", se.fit=TRUE)$fit + 1.96*predict(mwt.ot.all, 
                                                                                                            newdata = newDF.mot.mwt, 
                                                                                                            type = "link", se.fit=TRUE)$se.fit)


##
# make predictions for Otter Trawl:
mwt.ot.all.pred$fit.ot <- NA
mwt.ot.all.pred$se.ot <- NA
mwt.ot.all.pred$ot.lcl <- NA
mwt.ot.all.pred$ot.ucl <- NA
mwt.ot.all.pred$fit.ot <- predict(mwt.ot.all.3, 
                                  newdata = newDF.mot.ot,
                                  type = "response", se.fit=TRUE)$fit
mwt.ot.all.pred$se.ot <- predict(mwt.ot.all.3, 
                                 newdata = newDF.mot.ot,
                                 type = "response", se.fit=TRUE)$se.fit
mwt.ot.all.pred$ot.lcl <- mwt.ot.all.3$family$linkinv(predict(mwt.ot.all.3, 
                                                              newdata = newDF.mot.ot, 
                                                              type = "link", se.fit=TRUE)$fit - 1.96*predict(mwt.ot.all, 
                                                                                                             newdata = newDF.mot.ot, 
                                                                                                             type = "link", se.fit=TRUE)$se.fit)
mwt.ot.all.pred$ot.ucl <- mwt.ot.all.3$family$linkinv(predict(mwt.ot.all.3, 
                                                              newdata = newDF.mot.ot, 
                                                              type = "link", se.fit=TRUE)$fit + 1.96*predict(mwt.ot.all, 
                                                                                                             newdata = newDF.mot.ot, 
                                                                                                             type = "link", se.fit=TRUE)$se.fit)

mwt.ot.all.pred$bayname <- bays$name[which(mwt.ot.all.pred$bay %in% bays$num)]

# write.csv(mwt.ot.all.pred, "mwt.ot.all.pred.csv")

