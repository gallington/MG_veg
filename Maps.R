

#USE readOGR bc it brings in the projection info. readShapePoly does not.
im_mn<-readOGR(dsn="/nfs/gallington-data/Timeseries/GIS_data", layer='IM_MN_state')
china_provs<- readOGR(dsn="/nfs/gallington-data/Timeseries/GIS_data/china/CHN_adm_shp", layer='CHN_adm1')
china <- readOGR(dsn="/nfs/gallington-data/Timeseries/GIS_data/china/CHN_adm_shp", layer='CHN_adm0')
china_leagues<- readOGR(dsn="/nfs/gallington-data/Timeseries/GIS_data/china/CHN_adm_shp", layer='CHN_adm2')
# Rename levels
levels(im_mn$NAME) <- c("Mongolia","Inner Mongolia")

#subset out IMAR, remember that indexing stars w 1 at column name, so add one to get the right row
imar<- im_mn[2,]
#mg<- im_mn[1,]
imar2 <- china_provs[19 ,5]


#subset out Chifeng & Tongliao
chf<- subset(china_leagues, NAME_2=="Chifeng")
tong<- subset(china_leagues, NAME_2=="Tongliao")

# subset out IMAR leagues
imar_cty<- subset(china_leagues, NAME_1=="Nei Mongol")

