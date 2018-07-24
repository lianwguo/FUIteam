

#============================================================
# Function to rank annual top fishing sites in FLorida and Louisiana 
#============================================================
rankSites <- function(yearVec,harvestType,stateFipCode, howMany,catchDat,tripDat) {

	# # test
	# catchDat <- datCatch # test
	# tripDat <- datTrip # test
	# harvestType="harv_d3" # test
	# howMany=20 # test
	# yearVec <- 2010 # test
	# stateFipCode <- 22 # test
	
	stateCatchDat <- catchDat[which(catchDat$ST==stateFipCode),]
	
	# subset of only data and year needed (STRAT_ID years are added as a vector using lapply below)
	catchDat <- subset(stateCatchDat,select=c("ID_CODE","ST","YEAR",paste0(harvestType)))
	catchDat <- catchDat[which(catchDat$YEAR==yearVec),]

	tripDat <- subset(tripDat,select=c("ID_CODE","INTSITE","CNTY"))

	uniqueTripDat <- unique(tripDat)
	uniqueTripDat[,2:3]<- apply(uniqueTripDat[,2:3],2,function(x) as.numeric(x))

	# unique(uniqueTripDat$CNTY)

	# merge together catch data wth trip data to subset ONLY for trips for given year
	MergedCatchTripDat <- merge(catchDat,uniqueTripDat,by="ID_CODE",all.x=TRUE)

	# MergedCatchTripDat <- apply(MergedCatchTripDat,2,function(x) as.numeric(x))

	# take subset only for shoreline fishing sites in counties adjacent to FOR EACH STATE (FL=12, LA=22)
	# conditional for Florida, county codes are: 103, 57
	if(MergedCatchTripDat$ST[1]==12) {
		MergedDat <- MergedCatchTripDat[which( MergedCatchTripDat$CNTY==103 | 
			MergedCatchTripDat$CNTY==57 ),]
	}

	# conditional for Louisiana FIP 22, county codes are: 103, 71, 87, 75, 51, 89, 95
	if(MergedCatchTripDat$ST[1]==22) {
		MergedDat <- MergedCatchTripDat[which( MergedCatchTripDat$CNTY==71 | MergedCatchTripDat$CNTY==87 | 
				MergedCatchTripDat$CNTY==75 | MergedCatchTripDat$CNTY==51| 
				MergedCatchTripDat$CNTY==89 | MergedCatchTripDat$CNTY==95),]
	}

	# split data into each year for aggregation
	# eachyearDat <- subset(MergedDat,YEAR==yearVec)
	# MergedDat[] <- lapply(MergedDat, function(x) type.convert(as.character(x)))
	aggregated <- aggregate(MergedDat[,paste0(harvestType)] ~ INTSITE, MergedDat, sum)
	names(aggregated)[2] <- "harvestValue"

	top <- aggregated[order(aggregated[,"harvestValue"],decreasing = TRUE),][1:howMany,]
	rownames(top) <- NULL
	top$harvestType <- paste0(harvestType)
	top$YEAR <- yearVec


	
	removeZeros <- top[top[,"harvestValue"]!=0,] #removes zeros
	clean <- removeZeros[!is.na(removeZeros[,"harvestValue"]==TRUE),] #removes NAs
	
	removeZeros
	clean

	return(clean)

}

#============================================================
# Function extract zip codes associated with top sites
#============================================================

# NOTE best way to do this is most likley with the existing lists... 
# and then putting it in a long format dataframe under columns (LA/TB, Year, ZipCode,Site,Quantity)

rankZipCodes <- function(yearVec, harvestType,howMany,state, datCatch, datTrip,METRO,metrozips) {
	

	# # # test
	# datCatch <- read.csv("data/mripCatch_2004_2017.csv") #test
	# datTrip <- read.csv("data/mripTrip_2004_2017.csv") #test
	# harvestType <- "landing"
	# yearVec <- 2011 #test
	# state <- "Louisiana"	#test
	# howMany <- 30
	# METRO=TRUE
	# metrozips <- read.csv("data/metropolitan_ZCTA.csv",header = TRUE) # ZCTAs metro areas from Sarita
	# # # test


	if(state=="Florida") stateFipCode <- 12
	if(state=="Louisiana") stateFipCode <- 22

	stateCatchDat <- datCatch[which(datCatch$ST==stateFipCode),] #subset data cuz faster
	
	# subset of only data and year needed (STRAT_ID years are added as a vector using lapply below)
	catchDat <- subset(stateCatchDat,select=c("ID_CODE","ST","YEAR",paste0(harvestType)))
	catchDat <- catchDat[which(catchDat$YEAR==yearVec),]

	tripDat <- subset(datTrip,select=c("ID_CODE","CNTY","ZIP"))

	uniqueTripDat <- unique(tripDat)
	uniqueTripDat[,2:3]<- apply(uniqueTripDat[,2:3],2,function(x) as.numeric(x))


	# merge together catch data wth trip data to subset ONLY for trips for given year
	MergedCatchTripDat <- merge(catchDat,uniqueTripDat,by="ID_CODE",all.x=TRUE)

	# MergedCatchTripDat <- apply(MergedCatchTripDat,2,function(x) as.numeric(x))

	# take subset only for shoreline fishing sites in counties adjacent to FOR EACH STATE (FL=12, LA=22)
	# conditional for Florida, county codes are: 103, 57
	if(MergedCatchTripDat$ST[1]==12) {
		MergedDat <- MergedCatchTripDat[which( MergedCatchTripDat$CNTY==103 | 
			MergedCatchTripDat$CNTY==57 ),]
	}

	# conditional for Louisiana FIP 22, county codes are: 103, 71, 87, 75, 51, 89, 95
	if(MergedCatchTripDat$ST[1]==22) {
		MergedDat <- MergedCatchTripDat[which( MergedCatchTripDat$CNTY==71 | MergedCatchTripDat$CNTY==87 | 
				MergedCatchTripDat$CNTY==75 | MergedCatchTripDat$CNTY==51| 
				MergedCatchTripDat$CNTY==89 | MergedCatchTripDat$CNTY==95),]
	}

	# subset only metropolitan areas based on logic. 
	if(METRO==TRUE) {
		if(state=="Florida") statemetrozips <- metrozips[metrozips$STATE==12,] #specify only Tampa/St Pete metro zips
		if(state=="Louisiana") statemetrozips <- metrozips[metrozips$STATE==22,] #specify only NOLA metro zips
		MergedDat <- merge(statemetrozips,MergedDat, by.x="ZCTA5",by.y="ZIP",all.x=TRUE) # Merges full dataset ONLY to Metropolitan zips
		names(MergedDat)[1] <- "ZIP"
	}

	#only include rows with >0 landings and NA values
	MergedDat <- MergedDat[MergedDat$landing>0,]

	# sums all harvest observations for each site
	aggregated <- aggregate(MergedDat[,paste0(harvestType)] ~ ST, MergedDat, sum)
	names(aggregated)[2] <- "harvestValue"

	# sums all harvest observations for each zip (consumes sites)
	aggregated <- aggregate(MergedDat[,paste0(harvestType)] ~ ZIP, MergedDat, sum)
	names(aggregated)[2] <- "harvestValue"

	top <- aggregated[order(aggregated[,"harvestValue"],decreasing = TRUE),][1:howMany,]
	rownames(top) <- NULL
	top$harvestType <- paste0(harvestType)
	top$YEAR <- yearVec

	
	removeZeros <- top[top[,"harvestValue"]!=0,] #removes zeros
	clean <- removeZeros[!is.na(removeZeros[,"harvestValue"]==TRUE),] #removes NAs


	return(clean)
}

#============================================================
# Function extract zip codes AND sites for each zip code 
#============================================================


zipSitesLanding <- function(yearVec, harvestType,howMany,state, datCatch, datTrip,METRO,metrozips) {
	
	# # # test
	# datCatch <- read.csv("data/mripCatch_2004_2017.csv") #test
	# datTrip <- read.csv("data/mripTrip_2004_2017.csv") #test
	# harvestType <- "landing"
	# yearVec <- 2011 #test
	# state <- "Louisiana"	#test
	# howMany <- 50
	# METRO=TRUE
	# metrozips <- read.csv("data/metropolitan_ZCTA.csv",header = TRUE) # ZCTAs metro areas from Sarita
	# # # test


	if(state=="Florida") stateFipCode <- 12
	if(state=="Louisiana") stateFipCode <- 22

	stateCatchDat <- datCatch[which(datCatch$ST==stateFipCode),] #subset data cuz faster
	
	# subset of only data and year needed (STRAT_ID years are added as a vector using lapply below)
	catchDat <- subset(stateCatchDat,select=c("ID_CODE","ST","YEAR",paste0(harvestType)))
	catchDat <- catchDat[which(catchDat$YEAR==yearVec),]

	tripDat <- subset(datTrip,select=c("ID_CODE","CNTY","ZIP","INTSITE"))

	uniqueTripDat <- unique(tripDat)
	uniqueTripDat[,2:3]<- apply(uniqueTripDat[,2:3],2,function(x) as.numeric(x))


	# merge together catch data wth trip data to subset ONLY for trips for given year
	MergedCatchTripDat <- merge(catchDat,uniqueTripDat,by="ID_CODE",all.x=TRUE)

	# MergedCatchTripDat <- apply(MergedCatchTripDat,2,function(x) as.numeric(x))

	# take subset only for shoreline fishing sites in counties adjacent to FOR EACH STATE (FL=12, LA=22)
	# conditional for Florida, county codes are: 103, 57
	if(MergedCatchTripDat$ST[1]==12) {
		MergedDat <- MergedCatchTripDat[which( MergedCatchTripDat$CNTY==103 | 
			MergedCatchTripDat$CNTY==57 ),]
	}

	# conditional for Louisiana FIP 22, county codes are: 103, 71, 87, 75, 51, 89, 95
	if(MergedCatchTripDat$ST[1]==22) {
		MergedDat <- MergedCatchTripDat[which( MergedCatchTripDat$CNTY==71 | MergedCatchTripDat$CNTY==87 | 
				MergedCatchTripDat$CNTY==75 | MergedCatchTripDat$CNTY==51| 
				MergedCatchTripDat$CNTY==89 | MergedCatchTripDat$CNTY==95),]
	}

	# subset only metropolitan areas based on logic. 
	if(METRO==TRUE) {
		if(state=="Florida") statemetrozips <- metrozips[metrozips$STATE==12,] #specify only Tampa/St Pete metro zips
		if(state=="Louisiana") statemetrozips <- metrozips[metrozips$STATE==22,] #specify only NOLA metro zips
		MergedDat <- merge(statemetrozips,MergedDat, by.x="ZCTA5",by.y="ZIP",all.x=TRUE) # Merges full dataset ONLY to Metropolitan zips
		names(MergedDat)[1] <- "ZIP"
	}

	#only include rows with >0 landings

	MergedDat <- MergedDat[MergedDat$landing>0,]
	MergedDat <- MergedDat[is.na(MergedDat$INTSITE)==FALSE,]
	MergedDat <- MergedDat[,-MergedDat$ST]
	# add year
	MergedDat$YEAR <- yearVec

	MergedDatOrg <- subset(MergedDat,select=c("YEAR","ID_CODE","CNTY","ZIP","INTSITE","landing"))


	return(MergedDatOrg)
}


#============================================================
# Function to plot subsistence engagement by zips
#============================================================

zipPlot <- function(yearsSeq,Mat,state, harvestType) {
	yearsSeq <- 1
	Mat <- MetroZipLandingYearState
	state <- "Florida"
	harvestType <- "landing"
	if(state=="Louisiana") yearsVec <- LAyearsVec
	if(state=="Florida") yearsVec <- FLyearsVec
	df <- Mat[which( (Mat$state==state) & (Mat$year==yearsVec[yearsSeq])) ,][1:2]
	names(df) <- c("region","value")
	df$value <- as.numeric(df$value)
	df$region <- as.character(df$region)

	zip_choropleth(df, state_zoom = tolower(state), title=paste0(state," Zipcode for Summed",sep=" ",harvestType,sep=" ",yearsVec[yearsSeq])) + coord_map()
}
