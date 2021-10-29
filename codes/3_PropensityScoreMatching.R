#PROPENSITY SCORE MATCHING (PSM)
pscore.data <- dWild
colnames(pscore.data)[which(names(pscore.data) == "pa_dum")] <- "treat"
pscore.data$treat<-plyr(revalue("nonPA" = "0", "PA" = "1"))

pscore.data <- na.omit(pscore.data[, c("x", "y", "treat","lu_Base_max","cc_Base_max","all_spp","elevation","GID_0","dist_ocean","realms")])
names(pscore.data) <- c("lon", "lat","treat","vel.luse","vel.clim","spp.rich","elevation", "nations","dist.ocean", "bioRealms")

dim(pscore.data)
pscore.data <- pscore.data[is.finite(pscore.data$vel.luse),]




library("MatchIt")
set.seed(123456)
matched.mn <- matchit(treat ~ 
                      spp.rich + 
                      elevation + 
                      dist.ocean + 
                      bioRealms, 
                    data = pscore.data, 
                    method = "nearest", 
                    caliper = .25,
                    replace = FALSE,
                    Ratio = 1, 
                    discard = "both", 
                    reestimate = F
)
summary(matched.mn)

#write.csv(summary(matched.mn)[4], "matchStats.csv") #Save matching performance statistics
tiff("./figs/SI_FigS5.tiff", units="in", width=5, height=6, res=300)
plot(matched.mn, type = "hist", interactive = FALSE)
dev.off()




matched.mn.data<-match.data(matched.mn)
#Check characteristics of PAs before matching and compare to after matching
RItools::xBalance(treat ~ 
                    spp.rich+
                    elevation+
                    bioRealms+
                    dist.ocean,
                  data = matched.mn.data, 
                  report = c("chisquare.test"))

RItools::xBalance(treat ~ 
                    spp.rich+
                    elevation+
                    bioRealms+
                    dist.ocean,
                  data = pscore.data, 
                  report = c("chisquare.test")) #

#t.test(matched.mn.data$vel.clim[matched.mn.data$treat == 1],matched.mn.data$vel.clim[matched.mn.data$treat == 0], paired = TRUE)
#t.test(matched.mn.data$vel.luse[matched.mn.data$treat == 1],matched.mn.data$vel.luse[matched.mn.data$treat == 0], paired = TRUE)

library(lme4)
matched.mn.data$trt <- relevel(factor(matched.mn.data$treat), ref = "0")
matched.mn.data$vel.lusePlus <- log(matched.mn.data$vel.luse+0.001)
nest.luse<-lmerTest::lmer(vel.lusePlus ~ 
                            trt + spp.rich + elevation + dist.ocean + 
                            (1|bioRealms/nations),data = matched.mn.data)
summary(nest.luse)
performance::performance(nest.luse)
sjPlot::get_model_data(nest.luse, type = "std2")
sjPlot::tab_model(nest.luse, 
                  show.aicc = TRUE, 
                  show.loglik = TRUE,
                  show.std = TRUE, 
                  show.est = F, show.stat = T)

matched.mn.data$vel.climPlus <- log(matched.mn.data$vel.clim)
nest.clim<-lmerTest::lmer(vel.climPlus ~ 
                            trt + spp.rich + elevation + dist.ocean + 
                            (1|bioRealms/nations),data = matched.mn.data)
summary(nest.clim)
performance::performance(nest.clim)
sjPlot::get_model_data(nest.clim, type = "std2")
sjPlot::tab_model(nest.clim, 
                  show.aicc = TRUE, 
                  show.loglik = TRUE,
                  show.std = TRUE, 
                  show.est = F, show.stat = T)
