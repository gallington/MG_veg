
  # This is using the data that were summarized across all plots. 
  # need to bring in all plot data so can capture var?

# Naiman data: ---------

nmv<- read.csv("./data/ChinaVeg.csv")
tbl_df(nmv)
nmv<- gather(nmv, "Species", "Num", 2:26)
# remove NAs
nmv <- na.omit(nmv)

# make a df that also has Time Ung and ?

## Diff Data Matrices ------------------
  # Site x Species matrix
site.sp.nmv <- cast(nmv, Site  ~ Species, 
                     value='Num', FUN=mean)
site.sp.nmv[is.na(site.sp.nmv)] <- 0   #replace NA with 0
nmv.site.matrix <- as.matrix.cast_df(site.sp.nmv) #convert to a matrix
# THIS STILL JUST HAS TOTAL NUMBER OF COUNTS ACROSS PLOTS WE NEED REL % COVER?


  # Pres/Abs :
nmv_pa <- nmv %>% 
  mutate(
    PA = case_when(
      Num == 0 ~ 0,
      Num > 0  ~ 1
    )
  )

  # Distance matrix
nm.dis.bray <- vegdist(nmv.site.matrix,method="bray")


## Ordinations NMDS--------
  # Method 1: metaMDS
dis.mds <- metaMDS(dis.bray, trace = FALSE, autotransform = FALSE, shrink= FALSE)
dis.mds

  #Method 2: 
nmds<- metaMDS(nmv.site.matrix, k=2, trymax = 100)
ordiplot(nmds, choices = c(1,2), type="text")
ordiplot(nmds, type="points", display = "sites")
ordiplot(nmds, type="points", display = "species")
ordihull(nmds,groups = XXXX, draw="polygon",col="grey90",label=TRUE)


# SPECIES RICHNESS BY SITE -----
nmvSR<- specnumber(nmv.site.matrix)

# COMPOSITION------ 
  #Sorensons index by site
nm.beta <- vegdist(nmv.site.matrix, binary=TRUE)
mean(nm.beta)

# then using the df w Yr since Gzd by Site, model distance by Yr Since grzd

  #nmds of distances? 
  # REALLY NEED MORE DATA FOR THIS ONE ... 
beta.ord<- metaMDS(nm.beta, k = 2, trymax = 100)


# transect 2008 all:
t09 <- read.csv("./data/transect09.csv", sep= ",", header = TRUE)
t09 <- t09 %>% dplyr::select(-YEAR, -Month, -Day)
# then need to create a summary of sp by transect:

t09sum <- t09 %>% count(SiteName, Transect, SpeciesCode)
t09sum <- t09sum[2:254,]




