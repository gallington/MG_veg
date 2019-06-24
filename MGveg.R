# 21 Apr 2019 updated to reshape2
library(vegan)
library(MASS)
library(dplyr)
library(tidyr)
library(reshape2)
library(stringr)


#load data
mgp16<- read.csv("./data/2016_MGIM_ga_veg_subset.csv", 
                  header = TRUE)

sp.df<- dplyr::select(mgp16, site_numeral, Plot, Species, Cover)
sp.df$Species<- str_replace_all(sp.df$Species, " ", ".")

# convert to a sitexspecies matrix
site.sp.quad <- dcast(sp.df, site_numeral + Plot ~ Species, 
                     value.var ='Cover', FUN=mean)
# don't need this anymore 
#site.sp.quad <- as.data.frame(site.sp.quad) #convert from class "cast" to a df
site.sp.quad[is.na(site.sp.quad)] <- 0   #replace NA with 0
# and don't need this anymore either I think
#site.matrix <- as.matrix.cast_df(site.sp.quad) #convert to a matrix
site.matrix <- site.sp.quad    # just renaming it so I don't have to change everything below:

# calc distance matrix
dis.bray <- vegdist(site.matrix,method="bray")
# id 'treatments' = region
  # for use in later steps when need to have labels for plotting
treat=c(rep("Mongolia",66),rep("Inner Mongolia",38))  
sites<- site.sp.quad[1:104, 1]  
plots<- site.sp.quad[1:104, 2]


# Method 1: metaMDS
dis.mds <- metaMDS(dis.bray, trace = FALSE, autotransform = FALSE, shrink= FALSE)
dis.mds



scores <- data.frame(scores(dis.mds, display = "sites")) # this extracts the scores you want to plot in ggplot
scores2<- cbind(sites, scores) # bind the scores to the site info

ggplot(scores2, aes(x=NMDS1, y=NMDS2, color=factor(sites), alpha=factor(plots)))+#here you can color the points how you need to look at substructure.
   geom_point(aes(shape= treat), size=5)+#not sure if you also need shape info. This has helped me look at a lot of substructure among points.
   #scale_color_brewer("Spectral")+
   scale_fill_identity()+
   #scale_fill_grey(start = 0.2, end = 0.8)+ 
   xlab("NMDS Axis 1")+
   ylab("NMDS Axis 2")

plot(dis.mds, type = "t")
ordihull(dis.mds,groups=treat,draw="polygon",col="grey90",label=F)
ordihull(dis.mds,groups=sites,draw="polygon",col="grey90",label=T)

#
# subsetting to look just at MG:
#
mg.site.sp <- cast(sp.df[1:330,], site_numeral + Plot ~ Species, 
                     value='Cover', FUN=mean)
mg.site.sp <- as.data.frame(mg.site.sp) #convert from class "cast" to a df
mg.site.sp[is.na(mg.site.sp)] <- 0   #replace NA with 0
mg.matrix <- as.matrix.cast_df(mg.site.sp)
# calc distance matrix
mg.dis <- vegdist(mg.matrix,method="bray")

# id 'sites' and 'plots'
mg.sites<- mg.site.sp[1:66, 1]
mg.plots<- mg.site.sp[1:66, 2]
# apply mds function to distance matrix:
mg.mds <- metaMDS(mg.dis, trace = FALSE, autotransform = FALSE, shrink= FALSE)

mg.scores <- data.frame(scores(mg.mds, display = "sites")) # this extracts the scores you want to plot in ggplot
mg.scores2<- cbind(mg.sites, mg.scores) # bind the scores to the site info

ggplot(mg.scores2, aes(x=NMDS1, y=NMDS2, color=factor(mg.sites)))+#here you can color the points how you need to look at substructure.
  geom_point(size=5)+#not sure if you also need shape info. This has helped me look at a lot of substructure among points.
  #scale_color_brewer("Spectral")+
  scale_fill_identity()+
  #scale_fill_grey(start = 0.2, end = 0.8)+ 
  xlab("NMDS Axis 1")+
  ylab("NMDS Axis 2")+
  ggtitle("Mongolia Plots")

plot(mg.mds, type = "p")
title(main = "Mongolia Sites")
ordihull(mg.mds,groups=mg.sites,draw="polygon",col="grey90",label=T)

# subsetting to look just at IMAR:
#
im.site.sp <- cast(sp.df[331:540,], site_numeral + Plot ~ Species, 
                   value='Cover', FUN=mean)
im.site.sp <- as.data.frame(im.site.sp) #convert from class "cast" to a df
im.site.sp[is.na(im.site.sp)] <- 0   #replace NA with 0
im.matrix <- as.matrix.cast_df(im.site.sp)
# calc distance matrix
im.dis <- vegdist(im.matrix,method="bray")

# id 'sites' and 'plots'
im.sites<- im.site.sp[1:38, 1]
im.plots<- im.site.sp[1:38, 2]
# apply mds function to distance matrix:
im.mds <- metaMDS(im.dis, trace = FALSE, autotransform = FALSE, shrink= FALSE)

im.scores <- data.frame(scores(im.mds, display = "sites")) # this extracts the scores you want to plot in ggplot
im.scores2<- cbind(im.sites, im.scores) # bind the scores to the site info

ggplot(im.scores2, aes(x=NMDS1, y=NMDS2, color=factor(im.sites)))+#here you can color the points how you need to look at substructure.
  geom_point(size=5)+#not sure if you also need shape info. This has helped me look at a lot of substructure among points.
  #scale_color_brewer("Spectral")+
  scale_fill_identity()+
  #scale_fill_grey(start = 0.2, end = 0.8)+ 
  xlab("NMDS Axis 1")+
  ylab("NMDS Axis 2")+
  ggtitle("IMAR Plots")

plot(im.mds, type = "p")
title(main = "IMAR Sites")
ordihull(im.mds,groups=im.sites,draw="polygon",col="grey90",label=T)


# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Other things I played around with: 

# Method 2: isoMDS
# vare.mds0 <- isoMDS(dis.bray) #this does converge though??? 
# stressplot(vare.mds0, dis.bray)
# ordiplot(vare.mds0, type = "t")
# ordihull(vare.mds0,groups=treat,draw="polygon",col="grey90",label=TRUE)

# w raw matrix not dissim matrix: 
mat.nmds<- metaMDS(site.matrix, k=2, trymax=100)
mat.nmds
ordiplot(mat.nmds,type="n")
ordiplot(mat.nmds, type="points", display = "sites")
ordiplot(mat.nmds, type="points", display = "species")
ordihull(mat.nmds,groups=treat,draw="polygon",col="grey90",label=TRUE)
#ordiplot(mat.nmds, groups = site_numeral, type= "points", display = "sites")

#orditorp(mat.nmds,display="species",col="red",air=0.01)
#orditorp(mat.nmds,display="sites",cex=1.25,air=0.01)

# commands for some PCOs, but these aren't very interesting.
mg.pco <- pco(dis.bray,k=10)
barplot(mg.pco$eig)
plot(mg.pco)
dis.mht <- dist(site.matrix,"manhattan")
mht.pco <- pco(dis.mht,k=10)
plot(mht.pco)
dis.bin <- dist(site.matrix,"binary")
bin.pco <- pco(dis.bin,k=10)
plot(bin.pco)
plot(mg.pco$eig/sum(mg.pco$eig),type="b",xlab="Axis Number",
     ylab="Fraction of Sum")
lines(mht.pco$eig/sum(mht.pco$eig),type="b",col=2)
lines(bin.pco$eig/sum(bin.pco$eig),type="b",col=3)
text(8.0,0.45,'Euclidean')
text(8.0,0.4,"Manhattan",col=2)
text(8.0,0.35,"binary",col=3)