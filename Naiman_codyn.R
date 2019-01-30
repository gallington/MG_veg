# REDO INTEGRATING NAIMAN 2018
# need to annotate for Jhovae

# DON:T USE THESE< SCROLL DOWN
# Need to sort out the variable names bc something is weird with SiteCode
nm09 <- read_excel("./data/2009.Data.Entry.xlsx", sheet = 2)
nm09t<- read_excel("./data/2009.Data.Entry.xlsx", sheet = 1)

nm09$Count <- as.numeric(nm09$Count)
# replace NAs w 0 :
nm09$Count[is.na(nm09$Count)] <- 0

nm09$X__1 <- str_sub(nm09$PlotNumber, start= -1)
nm09 <- nm09 %>% 
  dplyr::rename(Quad = X__1) %>% 
  dplyr::select( -Day)

# average abund on transects in 2008:
nmt <- read.csv("./data/transectmeans.csv", sep = ",", header= TRUE)
tbl_df(nmt)
nmt$Avg.Abund <- as.numeric(nmt$Avg.Abund)

# START HERE: #####################
# Data import --------

############ 2008 transects all:
t09 <- read.csv("./data/transect09.csv", sep= ",", header = TRUE, stringsAsFactors = FALSE)
t09 <- t09 %>% dplyr::select( -Month, -Day)
# then need to create a summary of sp by transect:

t09sum <- t09 %>% count(SiteName, Transect, SpeciesCode)%>% mutate(Year = 2008)
t09sum <- t09sum[2:254,]
# remove NAs
t09sum <- na.omit(t09sum)
#restrict number of transects:
#t09sum <- t09sum %>% dplyr::filter(Transect <= 1)

  # change variable names to match 2018:
t09sum <- t09sum %>% mutate(Site = case_when(SiteName == "HeavyGrazing" ~ "Heavy",
                                             SiteName == "RedoControl" ~ "Control",
                                             SiteName == "DXG" ~ "DXG",
                                             SiteName == "Naiman" ~ "Naiman",
                                             SiteName == "Grazing" ~ "Grazing",
                                             TRUE ~ as.character(SiteName))) #else NA

########## 2018 transects :
nm18 <- read_excel("./data/2018.Data.Entry.xlsx", sheet = 1)
nm18 <- nm18 %>% dplyr::select(-Month, -Day, -X__1)
tbl_df(nm18)

t18sum <- nm18 %>% count(SiteName, Transect, SpeciesCode) %>% mutate(Year = 2018)
t18sum <- mutate(t18sum, Site = SiteName)

t18sum$SiteName<- as.factor(t18sum$SiteName)
#t18sum$Transect<- as.factor(t18sum$Transect)
t18sum$SpeciesCode<- as.factor(t18sum$SpeciesCode)
#t18sum$Site<- as.factor(t18sum$Site)
#
#Manually add the Naiman transect data for 2018:
nm08<- t09sum %>% filter(SiteName == "Naiman")

SpeciesCode = rep(c("AH" ,"CM", "LD", "CS", "SM", "SV", "LO", "SalixQ", "EE"), 2)
SiteName <- rep("Naiman", 18)
n <- as.numeric(rep(c(15, 40, 13, 6, 20, 30, 1, 5, 6),2))
Transect <- c(rep(1, 9), rep(2, 9))
Year <- as.numeric(rep(2018, 18))
Site<- as.character(rep("Naiman", 18))

cols<-cbind(SiteName, Transect, SpeciesCode, n, Year, Site)
dat<-as.data.frame(cols, stringsAsFactors = FALSE)
dat$n<-as.numeric(as.character(dat$n))
dat$Year<-as.numeric(as.character(dat$Year))
dat$Transect<- as.numeric(as.character(dat$Transect))
str(dat)



# combine the two years:
x <- bind_rows(dat, t18sum, t09sum)
x18 <- x %>% filter(Year == 2018)

# add unique ID as Replicate whichis Site&Transect combined:

x <- x %>% mutate(Replicate = paste(Site, Transect, sep = "."))


# remove Light grazing (and DXG?)

trt <- c("Grazing", "Heavy", "Control", "Naiman")
trt2 <- c("Grazing", "Heavy", "Control", "Naiman", "DXG") # to use DXG as reference

nmtrans <- x %>% dplyr::filter(Site  %in% trt)
nmdxg <- x %>% dplyr::filter(Site  %in% trt2)

# w just 3 transects each:
nmveg3t<- nmdxg %>% filter(Transect <= 3)


###### mg_imar transect data:
        
    # not using this right now
mgp16<- read.csv("./data/2016_MGIM_ga_veg_subset.csv", 
                 header = TRUE)

mgsum <- mgp16 %>% group_by(plat, site_original, site_numeral, Plot, Species) %>%
                   summarise(n = sum(Cover)) %>%
                   mutate(Replicate = paste(site_original, Plot, sep = "."))

t09.3t<- nmveg3t %>% filter(Year == 2008)

#-----------Analysis -----------

# community structure: --------

  #2008         ******* this isn't right now. the richness values are too high???
community_diversity(df= t09, abundance.var = "n", replicate.var = "Site")
community_structure(df= t09.3t, abundance.var = "n", replicate.var = "Site")

  #2018
community_diversity(x18, abundance.var = "n", replicate.var = "SiteName")
community_structure(df= x18, abundance.var = "n", replicate.var = "SiteName")

  #mg_imar
community_diversity(df = mgsum, abundance.var = "n", replicate.var = "site_original")
community_structure(df= mgsum, abundance.var = "n", replicate.var = "site_original")

community_diversity(df = mgsum, abundance.var = "n", replicate.var = "plat")
community_structure(df= mgsum, abundance.var = "n", replicate.var = "plat")




# rank abundance curves: --------

RAC_change(df = nmveg3t , 
           species.var = "SpeciesCode",
           time.var = "Year",
           abundance.var = "n",
           replicate.var = "Replicate")





RAC_difference(df= mgsum, 
               species.var = "Species", 
               abundance.var = "n", 
               replicate.var = "Replicate", treatment.var = "plat")

RAC_difference(df = nmt,
               species.var = "Sp.Code",
               abundance.var = "Avg.Abund",
               replicate.var = "Trt")



RAC_difference(df = t18sum,
               species.var = "SpeciesCode",
               abundance.var = "n",
               replicate.var = "Transect", treatment.var = "SiteName")


# species abundance ----------------

abundance_change(df = nmveg3t,
                 time.var = "Year",
                 species.var = "SpeciesCode",
                 abundance.var = "n",
                 replicate.var = "Replicate"
                 )
    
abundance_difference(df = mgsum,
                     species.var = "Species",
                     abundance.var = "n",
                     replicate.var = "Replicate", 
                     treatment.var = "plat",
                     pool = TRUE)

# curve change ----------
curve_change(df = nmveg3t,
             time.var = "Year",
             species.var = "SpeciesCode",
             abundance.var = "n",
             replicate.var = "Replicate")

   # ERROR__ READ THE HELP FILE
curve_difference(df = nmveg3t,
                 time.var = "Year",
                 species.var = "SpeciesCode",
                 abundance.var = "n",
                 replicate.var = "Replicate",
                 treatment.var = "Site",
                 pool = TRUE)
  # mg_imar
curve_difference(df = mgsum,
                 #time.var = "Year",
                 species.var = "Species",
                 abundance.var = "n",
                 replicate.var = "Replicate",
                 treatment.var = "plat",
                 pool = TRUE)
  # ERROR: 
curve_difference(df = mgsum,
                 #time.var = "Year",
                 species.var = "Species",
                 abundance.var = "n",
                 replicate.var = "Replicate",
                 treatment.var = "site_original",
                 block.var = "plat")

## multivariate difference and change: -----
# assesses distance btwn community centroids and degree of dispersion.
multivariate_change(df = nmveg, 
                    species.var = "SpeciesCode", 
                    time.var  = "Year",
                    abundance.var = "n", 
                    replicate.var = "Replicate", 
                    treatment.var = "Site"
)

multivariate_change(df = nmveg3t, 
                    species.var = "SpeciesCode", 
                    time.var  = "Year",
                    abundance.var = "n", 
                    replicate.var = "Replicate", 
                    treatment.var = "Site")

multivariate_change(df = tnaiman, 
                    species.var = "SpeciesCode", 
                    time.var  = "Year",
                    abundance.var = "n", 
                    replicate.var = "Replicate", 
                    treatment.var = "Site")

multivariate_difference(df = nmveg3t, 
                        species.var = "SpeciesCode", 
                        abundance.var = "n", 
                        replicate.var = "Replicate", 
                        treatment.var = "SiteName",
                        time.var = "Year"
)

multivariate_difference(df = mgsum,
                        species.var = "Species",
                        abundance.var = "n",
                        replicate.var = "Replicate",
                        treatment.var = "plat")


