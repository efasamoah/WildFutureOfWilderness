#PROPENSITY SCORE MATCHING (PSM)
pscore.data <- read_rds("output_tables/wildDataRevised.Rds")%>%subset(wildBin=="wild")
colnames(pscore.data)[which(names(pscore.data) == "pa_dum")] <- "treat"
pscore.data <- na.omit(pscore.data[, c("long", "lat", "treat","base_lui","base_vcc","tetrapods","elevation","GID_0","dist_ocean","realms","wildBin")])
names(pscore.data) <- c("long", "lat","treat","vel.luse","vel.clim","spp.rich","elevation", "nations","dist.ocean","bioRealms","wilderness")

dim(pscore.data)
pscore.data <- pscore.data[is.finite(pscore.data$vel.luse),]
pscore.data <- pscore.data %>% mutate(treat = ifelse(treat == "nonPA", 0, 1))
pscore.data$bioRealms <- factor(pscore.data$bioRealms); pscore.data$nations <- factor(pscore.data$nations)
# 
# library("MatchIt")
# set.seed(123456)
# matched.mn <- matchit(treat ~ 
#                         spp.rich + 
#                         elevation+ 
#                         dist.ocean + 
#                         nations +
#                         bioRealms,
#                       data = pscore.data,
#                       method = "nearest",
#                       distance = "glm",
#                       caliper = .25,
#                       replace = FALSE,
#                       Ratio = 1,
#                       discard = "both",
#                       reestimate = F,
#                       exact = ~ nations + bioRealms
# )
# summary(matched.mn)
# 
# #write.csv(summary(matched.mn)[4], "matchStats.csv") #Save matching performance statistics
# tiff("./out_plots/SI_FigS5.tiff", units = "in", width = 5, height = 6, res = 300)
# plot(matched.mn, type = "hist", interactive = FALSE)
# dev.off()
# 
# matched.mn.data <- match.data(matched.mn)
#write_rds(matched.mn.data, "matched_data.Rds")

matched.mn.data <- readRDS("./output_tables/matched_data.Rds")
#Check characteristics of PAs before matching and compare to after matching
matched.mn.data %>% group_by(treat)%>%
  summarise(
    spp.rich.mn = mean(spp.rich, na.rm=TRUE),
    spp.rich.sd = sd(spp.rich, na.rm=TRUE),
    elevation.mn = mean(elevation, na.rm=TRUE),
    elevation.sd = sd(elevation, na.rm=TRUE),
    dist.ocean.mn = mean(dist.ocean/1000, na.rm=TRUE),
    dist.ocean.sd = sd(dist.ocean/1000, na.rm=TRUE)
  )

RItools::xBalance(treat ~ 
                    spp.rich + 
                    elevation + 
                    dist.ocean +
                    nations +  
                    bioRealms,
                  data = matched.mn.data, 
                  report = c("chisquare.test"))

RItools::xBalance(treat ~ 
                    spp.rich + 
                    elevation + 
                    dist.ocean +
                    nations + 
                    bioRealms,
                  data = pscore.data, 
                  report = c("chisquare.test")) #

#t.test(matched.mn.data$vel.clim[matched.mn.data$treat == 1],matched.mn.data$vel.clim[matched.mn.data$treat == 0], paired = TRUE)
#t.test(matched.mn.data$vel.luse[matched.mn.data$treat == 1],matched.mn.data$vel.luse[matched.mn.data$treat == 0], paired = TRUE)

library(lme4)
#set reference category for factors
matched.mn.data$trt <- relevel(factor(matched.mn.data$treat), ref = "0")
matched.mn.data$vel.luse.log <- log(matched.mn.data$vel.luse+0.1) #add small tp facilitate log-transformation

nest.luse <- lmerTest::lmer(vel.luse.log ~ trt + log(spp.rich) + abs(elevation) + log(dist.ocean+1)+ (1|bioRealms) + (1|nations), data = subset(matched.mn.data, wilderness=="wild"))
summary(nest.luse)
sjPlot::tab_model(nest.luse, 
                  show.aicc = TRUE, 
                  show.loglik = TRUE,
                  show.std = TRUE, 
                  show.est = F, show.stat = T,
                  digits = 3)

matched.mn.data$vel.clim.log <- log(matched.mn.data$vel.clim)
nest.clim <- lmerTest::lmer(vel.clim.log ~ trt + log(spp.rich) + abs(elevation) + log(dist.ocean+1) + (1|bioRealms)+(1|nations),data = subset(matched.mn.data, wilderness=="wild"))
summary(nest.clim)
sjPlot::tab_model(nest.clim, 
                  show.aicc = TRUE, 
                  show.loglik = TRUE,
                  show.std = TRUE, 
                  show.est = F, show.stat = T,
                  digits = 3)




