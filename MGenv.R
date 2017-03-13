# 

# trying a CCA to bring in ENV data

# import the data
mg.env16<- read.csv("./data/mgveg_env_vars.csv", sep=",",
                 header = TRUE, stringsAsFactors = FALSE, na.strings = "NA")

# clean it up
# 
names(mg.env16)[8:13] <- c("LCnum", "LCtype", "GPPkg", "GPPg", "EVI", "NDVI")  #fix column names
tbl_df(mg.env16) 

sub.env<- mg.env16 %>%
          select( LCtype, EVI, as.numeric(daily.albedo.WSA)) %>%
          slice(1:35)

# substituting a value for the one NA in albedo to see if it makes it work
mean(sub.env$daily.albedo.WSA[3:22])
sub.env[2,3]<- 0.2016



# aggregating the species cover values by Site:
site.sp <- site.sp.quad %>%
            select(-Plot) 
#reduce to mean cover by Site for now bc I don't know how to do this otherwise
t<- aggregate(site.sp, by=list(Site= site.sp$site_numeral), FUN=mean) 
site.sp<- select(t, -Site, -site_numeral)  #this is aggregated data!!!


#Convert to relative frequencies ?
rel.frq<-site.sp/rowSums(site.sp)


# NMDS with the aggregated data, for comparison:
#
# remember, this is with the aggregated site data so have to change treatment list
sp.dist<- vegdist(site.sp, method="bray")
sp.mds <- metaMDS(site.sp, trace = FALSE, autotransform = FALSE, shrink= FALSE)  # mds w sitexspecies
sp.mds <- metaMDS(sp.dist,  trace = FALSE, autotransform = FALSE, shrink= FALSE) # mds with distance matrix
#sp.mds4 <- metaMDS(sp.dist,  trace = FALSE, autotransform = FALSE, shrink= FALSE, k= 4) # mds with distance matrix
plot(sp.mds)
sp.scores <- data.frame(scores(sp.mds, display = "sites")) # this extracts the scores you want to plot in ggplot
ag.treat<- c(rep("mongolia", 22), rep("imar", 13))   # new treatment list for just 35 rows

ordihull(sp.mds, groups = ag.treat, draw = "polygon", label= T)


# aggregated sites 
ag.sites <- seq(1:35)

scores.sp <- data.frame(scores(sp.mds, display = "sites")) # this extracts the scores you want to plot in ggplot
scores3<- cbind(ag.sites, scores.sp) # bind the scores to the site info
ggplot(scores3, aes(x=NMDS1, y=NMDS2, color=factor(ag.sites)))+#here you can color the points how you need to look at substructure.
  geom_point(aes(shape= ag.treat), size=5)+#not sure if you also need shape info. This has helped me look at a lot of substructure among points.
  #scale_color_brewer("Spectral")+
  scale_fill_identity()+
  #scale_fill_grey(start = 0.2, end = 0.8)+ 
  xlab("NMDS Axis 1")+
  ylab("NMDS Axis 2")

#
#  DCA
#
# NOTE:  the DCA ends up more weird than the regular NMDS. 
#
sp.dca <- decorana(site.sp)
sp.dca
summary(sp.dca)

dca.scores <- data.frame(scores(sp.dca, display = "sites")) # this extracts the scores you want to plot in ggplot
dca.scores2 <- cbind(ag.sites, dca.scores)
ggplot(dca.scores2, aes(x=DCA2, y=DCA3, color=factor(ag.sites)))+#here you can color the points how you need to look at substructure.
  geom_point(aes(shape= ag.treat), size=5)+#not sure if you also need shape info. This has helped me look at a lot of substructure among points.
  #scale_color_brewer("Spectral")+
  scale_fill_identity()+
  #scale_fill_grey(start = 0.2, end = 0.8)+ 
  xlab("DCA Axis 2")+
  ylab("DCA Axis 3")
#
# trying to createa  df w DCA1 and the env data to plot to check as in the ordin webpage
# but there is just a cloud of points. Does this mean DCA not approp or not useful? or did it wrong?
dca.site.env <- as.data.frame(cbind(ag.sites, dca.scores$DCA1, sub.env$EVI, sub.env$daily.albedo.WSA))
rename(dca.site.env, c("V2"="DCA1", "V3"="EVI", "V4"="albedo"))
plot(dca.site.env$V2 ~ dca.site.env$V3)


# 
# trying this approach, but don't totally understand what it is giving me
sp.adonis<- adonis(rel.frq ~ EVI * daily.albedo.WSA, data= sub.env, dist="bray")
sp.adonis2<- adonis2(rel.frq ~ EVI * daily.albedo.WSA,data= sub.env, dist=bray, by=NULL)


#
# CCA
#
# attempting to combine sitexsp w env data via CCA :
# this still needs fine tuning.

im.cca <- cca(site.sp[1:22,] ~  EVI+ daily.albedo.WSA, data= sub.env[1:22,])   # removed LCtype
im.cca
plot(im.cca, display=c("wa", "cn"))
summary(im.cca)

mg.cca <- cca(site.sp[23:35,] ~  + EVI + daily.albedo.WSA, data= sub.env[23:35,]) # removed LCtype
mg.cca
plot(mg.cca, display=c("wa", "cn"))
ccasummary(mg.cca)


test.cca <- cca(site.sp ~  EVI + daily.albedo.WSA, data= sub.env)   # removed LCtype +
plot(test.cca,display=c("wa", "cn"))
summary(test.cca)


