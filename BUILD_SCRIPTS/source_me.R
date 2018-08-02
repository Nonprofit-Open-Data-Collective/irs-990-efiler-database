###---------------------------------------------------
###   FUNCTIONS TO FACILITATE PRODUCTION RULES
###---------------------------------------------------
  

zeroPC <- function( var )
{ 
    if( FORMTYPE=="990" )
    {
       if( length(var) == 0 ){ return("0") }
       if( is.na(var) ){ return("0") }
    }
    return( var )
}
    
 
 
zeroEZ <- function( var )
{ 
    if( FORMTYPE=="990EZ" )
    {
      if( length(var) == 0 ){ return("0") }
      if( is.na(var) ){ return("0") }
    }
    return( var )
}


    
zeroALL <- function( var )
{
    if( length(var) == 0 ){ return("0") }
    if( is.na(var)  ){ return("0") }
    return( var )
}


  
get_object_id <- function( url )
{
    object.id <- gsub( "https://s3.amazonaws.com/irs-form-990/", "", url )
    object.id <- gsub( "_public.xml", "", object.id )
    return( object.id )
}




###---------------------------------------------------
###   BUILD INDEX
###---------------------------------------------------


buildIndex <- function( file.years=2011:2018 )
{

	library( jsonlite )
	library( R.utils )
	library( dplyr )
	
	index.list <- list()
	counter <- 1
	
	for( i in file.years )
	{
	  
	  index.url <- paste0( "https://s3.amazonaws.com/irs-form-990/index_", i, ".json" )
	  index.list[[ counter ]] <- fromJSON( index.url )[[1]]
	  counter <- counter + 1
	  
	}

        index <- bind_rows( index.list )

	# dat1 <- fromJSON("https://s3.amazonaws.com/irs-form-990/index_2011.json")[[1]]
	# dat2 <- fromJSON("https://s3.amazonaws.com/irs-form-990/index_2012.json")[[1]]
	# dat3 <- fromJSON("https://s3.amazonaws.com/irs-form-990/index_2013.json")[[1]]
	# dat4 <- fromJSON("https://s3.amazonaws.com/irs-form-990/index_2014.json")[[1]]
	# dat5 <- fromJSON("https://s3.amazonaws.com/irs-form-990/index_2015.json")[[1]]
	# dat6 <- fromJSON("https://s3.amazonaws.com/irs-form-990/index_2016.json")[[1]]
	# dat7 <- fromJSON("https://s3.amazonaws.com/irs-form-990/index_2017.json")[[1]]
	# dat8 <- fromJSON("https://s3.amazonaws.com/irs-form-990/index_2018.json")[[1]]

	# index <- rbind( dat1, dat2, dat3, dat4, dat5, dat6, dat7, dat8  )

        index <- unique( index )  # remove a couple of strange duplicates

	# REFORMAT DATE FROM YYYY-MM TO YYYY
	# Tax Period represents the end of the nonprofit's accounting year
	# The tax filing year is always the previous year, unless the accounting year ends in December
	
        tax.year <- as.numeric( substr( index$TaxPeriod, 1, 4 ) )
	month <- substr( index$TaxPeriod, 5, 6 )
	index$TaxYear <- tax.year - 1
	index$TaxYear[ month == "12" ] <- tax.year[ month == "12" ]
	

	return( index )

}
















###---------------------------------------------------
###   RELATIONAL DATABASE FUNCTIONS
###---------------------------------------------------

# "group" here means the root xpath that defines
# a collection of variables on a one-to-many table
#
# find_group_names() identifies all possible roots
# get_var_map() creates a crosswalk between IRS 
#   variable names and concordance variable names
# get_table() build the data table
# re_name() converts IRS names to concordance names



find_group_names <- function( dat, table.name )
{
  names( dat ) <- toupper( names(dat) )
  TABLE <- filter( dat, RDB_TABLE == table.name )
  xpaths <- TABLE$XPATH %>% as.character()
  xpaths <- gsub( "IRS990EZ", "IRS990", xpaths )
  nodes <- strsplit( xpaths, "/" )
  d1 <- suppressWarnings( data.frame( do.call( cbind, nodes ), stringsAsFactors=F ) )
  not.equal <- apply( d1, MARGIN=1, FUN=function(x){ length( unique( x )) > 1 } ) 
  this.one <- which( not.equal == T )[ 1 ]
  group.names <- d1[ this.one,  ] %>% as.character() %>% unique()
  group.names <- paste0( "//", group.names )
  return( group.names )
}

# find_group_names( dat=partvii, table.name="F9-P07-TABLE-01-DTK-COMPENSATION" )




get_var_map <- function( dat, table.name )
{
   names( dat ) <- toupper( names(dat) )
   TABLE <- filter( dat, RDB_TABLE == table.name )
   xpaths <- TABLE$XPATH %>% as.character()
   res <- strsplit( xpaths, "/" )
   v.map <- data.frame( VARIABLE=as.character(TABLE$VARIABLE_NAME_NEW), 
            XSD_VARNAME=unlist( lapply( res, last ) ), stringsAsFactors=F )
   v.map <- unique( v.map )
   return( v.map )
}

# v.map <- get_var_map( dat=partvii, table.name="F9-P07-TABLE-01-DTK-COMPENSATION" )

  

# find_group_names( schedj, "SJ-P02-T01-COMPENSATION" )
# get_var_map( schedj, table.name="SJ-P02-T01-COMPENSATION" )

# find_group_names( dat, "F9-P07-TABLE-01-DTK-COMPENSATION" )
# get_var_map( dat, table.name="F9-P07-TABLE-01-DTK-COMPENSATION" )




get_table <- function( doc, group.names )
{
  all.groups <- paste0( group.names, collapse="|" )
  nd <- xml_find_all( doc, all.groups )
  if( length( nd ) == 0 ){ return(NULL) }
  
  # ensure we are using root node for table
  table.xpaths <- (xml_get_paths( nd, only_terminal_parent = TRUE ))[[1]]
  if( length( table.xpaths ) > 1 )
  {
     nodes <- strsplit( table.xpaths, "/" )
     d1 <- suppressWarnings( data.frame( do.call( cbind, nodes ), stringsAsFactors=F ) )
     not.equal <- apply( d1, MARGIN=1, FUN=function(x){ length( unique( x )) > 1 } ) 
     this.one <- which( not.equal == T )[ 1 ]
     if( this.one < 2 ){ return( NULL ) }
     table.root <- d1[ this.one - 1,  ] %>% as.character() %>% unique()
     table.root <- paste0( "//", table.root )
     nd <- xml_find_all( doc, table.root )
  }

  rdb.table <- xml_dig_df( nd ) %>% bind_rows() 
  rdb.table %>% mutate_if(is.factor, as.character) -> rdb.table
  return( rdb.table )
}

# get_table( doc, group.names )



re_name <- function( df, v.map )
{

  for( i in unique( v.map$VARIABLE ) )
  {
         this.one <- names( df ) %in% v.map[ v.map$VARIABLE == i , "XSD_VARNAME" ] 
         names( df )[this.one] <- i
  }

      return( df )

}

  

build_rdb_table <- function( doc, url, group.names, v.map )
{

    
	####----------------------------------------------------
	####     KEYS
        ####----------------------------------------------------


        ## OBJECT ID
        
        OBJECTID <- get_object_id( url )



	## URL
	
	URL <- url
	
	

	## EIN

	EIN  <- xml_text( xml_find_all( doc, "//Return/ReturnHeader/Filer/EIN" ) )



	## NAME

	V_990NAMEpost2014 <- "//Return/ReturnHeader/Filer/BusinessName/BusinessNameLine1Txt"
	V_990NAME_2013 <- "//Return/ReturnHeader/Filer/BusinessName/BusinessNameLine1"
	V_990NAMEpre2013  <- "//Return/ReturnHeader/Filer/Name/BusinessNameLine1"
	name.xpath <- paste( V_990NAME_2013, V_990NAMEpre2013, V_990NAMEpost2014, sep="|" )
	NAME <- xml_text( xml_find_all( doc, name.xpath ) )


	## TYPE OF TAX FORM

	V_990TFpost2013 <- "//Return/ReturnHeader/ReturnTypeCd"
	V_990TFpre2013  <- "//Return/ReturnHeader/ReturnType"
	tax.form.xpath <- paste( V_990TFpost2013, V_990TFpre2013, sep="|" )
	FORMTYPE <- xml_text( xml_find_all( doc, tax.form.xpath ) )
	
	
	## TAX YEAR

	V_990FYRpost2013 <- "//Return/ReturnHeader/TaxYr"
	V_990FYRpre2013  <- "//Return/ReturnHeader/TaxYear"
	fiscal.year.xpath <- paste( V_990FYRpost2013, V_990FYRpre2013, sep="|" )
	TAXYR <- xml_text( xml_find_all( doc, fiscal.year.xpath ) )
	
	
	
	####  BUILD TABLE 
	
	df <- get_table( doc, group.names )
	
	if( is.null(df) ){ return( NULL ) }
	
	rdb.table <- data.frame( OBJECT_ID=OBJECTID, EIN=EIN, NAME=NAME, TAXYR=TAXYR, 
	                         FORMTYPE=FORMTYPE, URL=URL, df, stringsAsFactors=F )
	
        rdb.table <- re_name( rdb.table, v.map )
        
        return ( rdb.table )
        
}



# USAGE:
#
#  partvii.01 <- list()   # dtk compensation table
#  partvii.01.group.names <- find_group_names( partvii, "F9-P07-TABLE-01-DTK-COMPENSATION" )
#  partvii.01.v.map       <- get_var_map( partvii, table.name="F9-P07-TABLE-01-DTK-COMPENSATION" )
#  partvii.01[[1]]        <- build_rdb_table( doc, url, group.names=partvii.01.group.names, v.map=partvii.01.v.map )






#-------------------------------------------------------------------------------------


# FUNCTION TO COLLECT DATA FROM XML DOCS ON AWS
#
# Arguments:
#   url - link to a nonprofits xml page
#   form.type - check to ensure data is from the correct form in case of poor data in index file
#
# Return Value:
#   one-row data frame containing elements from one nonprofit




scrapeXML <- function( doc, url )
{



    
	#------------------------------------------------------------------------------------------------------------------------
	#### FROM NCCS CORE - HEADER DATA
	#### Fields here are same for forms of same year (990 & 990EZ post-2013; 990 & 990EZ pre-2013)


        ## OBJECT ID
        
        OBJECTID <- get_object_id( url )


	## EIN
	#### EIN field is the same for all forms

	EIN  <- xml_text( xml_find_all( doc, "//Return/ReturnHeader/Filer/EIN" ) )



	## NAME

	V_990NAMEpost2014 <- "//Return/ReturnHeader/Filer/BusinessName/BusinessNameLine1Txt"
	V_990NAME_2013 <- "//Return/ReturnHeader/Filer/BusinessName/BusinessNameLine1"
	V_990NAMEpre2013  <- "//Return/ReturnHeader/Filer/Name/BusinessNameLine1"
	name.xpath <- paste( V_990NAME_2013, V_990NAMEpre2013, V_990NAMEpost2014, sep="|" )
	NAME <- xml_text( xml_find_all( doc, name.xpath ) )



	## DOING BUSINESS AS

	V_990DBApost2013 <- "//Return/ReturnHeader/Filer/BusinessName/BusinessNameLine2Txt"
	V_990DBApre2013  <- "//Return/ReturnHeader/Filer/Name/BusinessNameLine2"
	dba.xpath <- paste( V_990DBApost2013, V_990DBApre2013, sep="|" )
	DBA  <- xml_text( xml_find_all( doc, dba.xpath ) )



	## FISCAL YEAR

	V_990FYRpost2013 <- "//Return/ReturnHeader/TaxYr"
	V_990FYRpre2013  <- "//Return/ReturnHeader/TaxYear"
	fiscal.year.xpath <- paste( V_990FYRpost2013, V_990FYRpre2013, sep="|" )
	FISYR <- xml_text( xml_find_all( doc, fiscal.year.xpath ) )



	## STATE

	V_990STATEpost2013 <- "//Return/ReturnHeader/Filer/USAddress/StateAbbreviationCd"
	V_990STATEpre2013  <- "//Return/ReturnHeader/Filer/USAddress/State"
	state.xpath <- paste( V_990STATEpost2013, V_990STATEpre2013, sep="|" )
	STATE <- xml_text( xml_find_all( doc, state.xpath ) )



	## ADDRESS

	V_990ADDRpost2013 <- "//Return/ReturnHeader/Filer/USAddress/AddressLine1Txt"
	V_990ADDRpre2013  <- "//Return/ReturnHeader/Filer/USAddress/AddressLine1"
	address.xpath <- paste( V_990ADDRpost2013, V_990ADDRpre2013, sep="|" )
	ADDRESS <- xml_text( xml_find_all( doc, address.xpath ) )



	## CITY

	V_990CITYpost2013 <- "//Return/ReturnHeader/Filer/USAddress/CityNm"
	V_990CITYpre2013  <- "//Return/ReturnHeader/Filer/USAddress/City"
	city.xpath <- paste( V_990CITYpost2013, V_990CITYpre2013, sep="|" )
	CITY <- xml_text( xml_find_all( doc, city.xpath ) )



	## ZIP CODE

	V_990ZIPpost2013 <- "//Return/ReturnHeader/Filer/USAddress/ZIPCd"
	V_990ZIPpre2013  <- "//Return/ReturnHeader/Filer/USAddress/ZIPCode"
	zip.xpath <- paste( V_990ZIPpost2013, V_990ZIPpre2013, sep="|" )
	ZIP <- xml_text( xml_find_all( doc, zip.xpath ) )



	## START OF YEAR

	V_990SYpost2013 <- "//Return/ReturnHeader/TaxPeriodBeginDt"
	V_990SYpre2013  <- "//Return/ReturnHeader/TaxPeriodBeginDate"
	start.year.xpath <- paste( V_990SYpost2013, V_990SYpre2013, sep="|" )
	STYEAR <- xml_text( xml_find_all( doc, start.year.xpath ) )



	## END OF YEAR

	V_990EYpost2013 <- "//Return/ReturnHeader/TaxPeriodEndDt"
	V_990EYpre2013  <- "//Return/ReturnHeader/TaxPeriodEndDate"
	end.year.xpath <- paste( V_990EYpost2013, V_990EYpre2013, sep="|" )
	ENDYEAR <- xml_text( xml_find_all( doc, end.year.xpath ) )
	
	
	## AMMENDED RETURN

	V1  <- "//Return/ReturnData/IRS990/AmendedReturn"
	V2  <- "//Return/ReturnData/IRS990/AmendedReturnInd"
	V3  <- "//Return/ReturnData/IRS990EZ/AmendedReturn"
	V4  <- "//Return/ReturnData/IRS990EZ/AmendedReturnInd"
	ammended.xpath <- paste( V1, V2, V3, V4, sep="|" )
	AMMENDED <- xml_text( xml_find_all( doc, ammended.xpath ) )


	## TAX PREPARER

	V_990TPpost2013 <- "//Return/ReturnHeader/PreparerPersonGrp/PreparerPersonNm"
	V_990TPpre2013  <- "//Return/ReturnHeader/Preparer/Name"
	tax.prep.xpath <- paste( V_990TPpost2013, V_990TPpre2013, sep="|" )
	TAXPREP <- xml_text( xml_find_all( doc, tax.prep.xpath ) )



	## TYPE OF TAX FORM

	V_990TFpost2013 <- "//Return/ReturnHeader/ReturnTypeCd"
	V_990TFpre2013  <- "//Return/ReturnHeader/ReturnType"
	tax.form.xpath <- paste( V_990TFpost2013, V_990TFpre2013, sep="|" )
	FORMTYPE <- xml_text( xml_find_all( doc, tax.form.xpath ) )







	zeroPC <- function( var )
	{ 
	    if( FORMTYPE=="990" )
	    {
	       if( length(var) == 0 ){ return("0") }
	       if( is.na(var) ){ return("0") }
	    }
	    return( var )
	}



	zeroEZ <- function( var )
	{ 
	    if( FORMTYPE=="990EZ" )
	    {
	      if( length(var) == 0 ){ return("0") }
	      if( is.na(var) ){ return("0") }
	    }
	    return( var )
	}



	zeroALL <- function( var )
	{
	    if( length(var) == 0 ){ return("0") }
	    if( is.na(var)  ){ return("0") }
	    return( var )
	}






	#------------------------------------------------------------------------------------------------------------------------
	##### BASIC INFO

	## GROSS RECEIPTS

	V_990GRCpost2013 <- "//Return/ReturnData/IRS990/GrossReceiptsAmt"
	V_990GRCpre2013  <- "//Return/ReturnData/IRS990/GrossReceipts"
	V_990GRC.EZpost2013 <- "//Return/ReturnData/IRS990EZ/GrossReceiptsAmt"
	V_990GRC.EZpre2013  <- "//Return/ReturnData/IRS990EZ/GrossReceipts"
	greceipts.xpath <- paste( V_990GRCpost2013, V_990GRCpre2013, V_990GRC.EZpost2013, V_990GRC.EZpre2013, sep="|" )
	GROSSRECEIPTS <- xml_text( xml_find_all( doc, greceipts.xpath ) ) 
	GROSSRECEIPTS <- zeroALL( GROSSRECEIPTS )


	## GROUP RETURNS  

	V_990GRTpost2013 <- "//Return/ReturnData/IRS990/GroupReturnForAffiliatesInd"
	V_990GRTpre2013  <- "//Return/ReturnData/IRS990/GroupReturnForAffiliates"
	greturn.xpath <- paste( V_990GRTpost2013, V_990GRTpre2013, sep="|" )
	GROUPRETURN <- xml_text( xml_find_all( doc, greturn.xpath ) ) 



	## GROUP EXEMPTION NUMBER

	V_990GENpost2013 <- "//Return/ReturnData/IRS990/GroupExemptionNum"
	V_990GENpre2013  <- "//Return/ReturnData/IRS990/GroupExemptionNumber"
	V_990GEN.EZpost2013 <- "//Return/ReturnData/IRS990EZ/GroupExemptionNum"
	V_990GEN.EZpre2013  <- "//Return/ReturnData/IRS990EZ/GroupExemptionNumber"
	gexempt.number.xpath <- paste( V_990GENpost2013, V_990GENpre2013, V_990GEN.EZpost2013, V_990GEN.EZpre2013, sep="|" )
	GROUPEXEMPTNUM <- xml_text( xml_find_all( doc, gexempt.number.xpath ) ) 



	## FORM YEAR

	V_990FORMYRpost2013 <- "//Return/ReturnData/IRS990/FormationYr"
	V_990FORMYRpre2013  <- "//Return/ReturnData/IRS990/YearFormation"
	form.year.xpath <- paste( V_990FORMYRpost2013, V_990FORMYRpre2013, sep="|" )
	FORMYEAR <- xml_text( xml_find_all( doc, form.year.xpath ) ) 



	## STATE OF LEGAL DOMICILE

	V_990DOMpost2013 <- "//Return/ReturnData/IRS990/LegalDomicileStateCd"
	V_990DOMpre2013  <- "//Return/ReturnData/IRS990/StateLegalDomicile"
	domicile.xpath <- paste( V_990DOMpost2013, V_990DOMpre2013, sep="|" )
	DOMICILE <- xml_text( xml_find_all( doc, domicile.xpath ) ) 
        DOMICILE <- paste( DOMICILE, collapse=" " )


	## WEBSITE

	V_990WEBpost2013 <- "//Return/ReturnData/IRS990/WebsiteAddressTxt"
	V_990WEBpre2013  <- "//Return/ReturnData/IRS990/WebSite"
	V_990WEB.EZpost2013 <- "//Return/ReturnData/IRS990EZ/WebsiteAddressTxt"
	V_990WEB.EZpre2013  <- "//Return/ReturnData/IRS990EZ/WebSite"
	website.xpath <- paste( V_990WEBpost2013, V_990WEBpre2013, V_990WEB.EZpost2013, V_990WEB.EZpre2013, sep="|" )
	WEBSITE <- xml_text( xml_find_all( doc, website.xpath ) ) 



	## URL

	URL <- url



	## FORM OF ORGANIZATION: represent the 4 possible values, broken out then collapsed
	## EZ Values are extrapolated from 990s

	## ORGANIZATION IS ASSOCATION

	V_990FOApost2013 <- "//Return/ReturnData/IRS990/TypeOfOrganizationAssocInd"
	V_990FOApre2013  <- "//Return/ReturnData/IRS990/TypeOfOrganizationAssociation"
	V_990FOA.EZpost2013 <- "//Return/ReturnData/IRS990EZ/TypeOfOrganizationAssocInd"
	V_990FOA.EZpre2013  <- "//Return/ReturnData/IRS990EZ/TypeOfOrganizationAssociation"
	type.org.assoc.xpath <- paste( V_990FOApost2013, V_990FOApre2013, V_990FOA.EZpost2013, V_990FOA.EZpre2013, sep="|" )
	FORMORGASSOC <- xml_text( xml_find_all( doc, type.org.assoc.xpath ) ) 

	FORMORGASSOC[ length( FORMORGASSOC ) == 0]  <- NA
	if( is.na( FORMORGASSOC ) == FALSE ) { FORMORGASSOC <- "Association" }



	## ORGANIZATION IS CORPORATION

	V_990FOCpost2013 <- "//Return/ReturnData/IRS990/TypeOfOrganizationCorpInd"
	V_990FOCpre2013  <- "//Return/ReturnData/IRS990/TypeOfOrganizationCorporation"
	V_990FOC.EZpost2013 <- "//Return/ReturnData/IRS990EZ/TypeOfOrganizationCorpInd"
	V_990FOC.EZpre2013  <- "//Return/ReturnData/IRS990EZ/TypeOfOrganizationCorporation"
	type.org.corp.xpath <- paste( V_990FOCpost2013, V_990FOCpre2013, V_990FOC.EZpost2013, V_990FOC.EZpre2013, sep="|" )
	FORMORGCORP <- xml_text( xml_find_all( doc, type.org.corp.xpath ) ) 

	FORMORGCORP[ length( FORMORGCORP ) == 0]  <- NA
	if( is.na( FORMORGCORP ) == FALSE ) { FORMORGCORP <- "Corporation" }



	## ORGANIZATION IS TRUST

	V_990FOTpost2013 <- "//Return/ReturnData/IRS990/TypeOfOrganizationTrustInd"
	V_990FOTpre2013  <- "//Return/ReturnData/IRS990/TypeOfOrganizationTrust"
	V_990FOT.EZpost2013 <- "//Return/ReturnData/IRS990EZ/TypeOfOrganizationTrustInd"
	V_990FOT.EZpre2013  <- "//Return/ReturnData/IRS990EZ/TypeOfOrganizationTrust"
	type.org.trust.xpath <- paste( V_990FOTpost2013, V_990FOTpre2013, V_990FOT.EZpost2013, V_990FOT.EZpre2013, sep="|" )
	FORMORGTRUST <- xml_text( xml_find_all( doc, type.org.trust.xpath ) ) 

	FORMORGTRUST[ length( FORMORGTRUST ) == 0]  <- NA
	if( is.na( FORMORGTRUST ) == FALSE ) { FORMORGTRUST <- "Trust" }



	## ORGANIZATION IS OTHER (CHECK BOX)

	V_990FOOpost2013 <- "//Return/ReturnData/IRS990/TypeOfOrganizationOtherInd"
	V_990FOOpre2013  <- "//Return/ReturnData/IRS990/TypeOfOrganizationOther"
	V_990FOO.EZpost2013 <- "//Return/ReturnData/IRS990EZ/TypeOfOrganizationOtherInd"
	V_990FOO.EZpre2013  <- "//Return/ReturnData/IRS990EZ/TypeOfOrganizationOther"
	type.org.other.xpath <- paste( V_990FOOpost2013, V_990FOOpre2013, V_990FOO.EZpost2013, V_990FOO.EZpre2013, sep="|" )
	FORMORGOTHER <- xml_text( xml_find_all( doc, type.org.other.xpath ) ) 

	FORMORGOTHER[ length( FORMORGOTHER ) == 0]  <- NA
	if( is.na( FORMORGOTHER ) == FALSE ) { FORMORGOTHER <- "" }



	## WRITTEN-IN DESCRIPTION OF ORGANIZATION:OTHER

	V_990FOWpost2013 <- "//Return/ReturnData/IRS990/OtherOrganizationDsc"
	V_990FOWpre2013  <- "//Return/ReturnData/IRS990/TypeOfOrgOtherDescription"
	V_990FOW.EZpost2013 <- "//Return/ReturnData/IRS990EZ/OtherOrganizationDsc"
	V_990FOW.EZpre2013  <- "//Return/ReturnData/IRS990EZ/TypeOfOrgOtherDescription"
	type.org.written.xpath <- paste( V_990FOWpost2013, V_990FOWpre2013, V_990FOW.EZpost2013, V_990FOW.EZpre2013, sep="|" )
	FORMORGOTHERDESC <- xml_text( xml_find_all( doc, type.org.written.xpath ) ) 



	## FORM OF ORGANIZATION (COLLAPSED)

	FORMORG <- gsub( "NA", "", paste( FORMORGASSOC, FORMORGCORP, FORMORGTRUST, FORMORGOTHER, FORMORGOTHERDESC, sep="" ) )
	FORMORG[ FORMORG  == "" ] <- NA



	##  ACCOUNTING METHODS: represent the 3 possible values, broken out then collapsed
	## ACCRUAL ACCOUNTING METHOD

	V_990AMApost2013 <- "//Return/ReturnData/IRS990/MethodOfAccountingAccrualInd"
	V_990AMApre2013  <- "//Return/ReturnData/IRS990/MethodOfAccountingAccrual"
	V_990AMA.EZpost2013 <- "//Return/ReturnData/IRS990EZ/MethodOfAccountingAccrualInd"
	V_990AMA.EZpre2013  <- "//Return/ReturnData/IRS990EZ/MethodOfAccountingAccrual"
	accounting.accrual.xpath <- paste( V_990AMApost2013, V_990AMApre2013, V_990AMA.EZpost2013, V_990AMA.EZpre2013, sep="|" )
	ACCTACCRUAL <- xml_text( xml_find_all( doc, accounting.accrual.xpath ) ) 

	ACCTACCRUAL[ length( ACCTACCRUAL ) == 0]  <- NA
	if( is.na( ACCTACCRUAL ) == FALSE ) { ACCTACCRUAL <- "Accrual" }



	## CASH ACCOUNTING METHOD

	V_990AMCpost2013 <- "//Return/ReturnData/IRS990/MethodOfAccountingCashInd"
	V_990AMCpre2013  <- "//Return/ReturnData/IRS990/MethodOfAccountingCash"
	V_990AMC.EZpost2013 <- "//Return/ReturnData/IRS990EZ/MethodOfAccountingCashInd"
	V_990AMC.EZpre2013  <- "//Return/ReturnData/IRS990EZ/MethodOfAccountingCash"
	accounting.cash.xpath <- paste( V_990AMCpost2013, V_990AMCpre2013, V_990AMC.EZpost2013, V_990AMC.EZpre2013, sep="|" )
	ACCTCASH <- xml_text( xml_find_all( doc, accounting.cash.xpath ) ) 

	ACCTCASH[ length( ACCTCASH ) == 0]  <- NA
	if( is.na(  ACCTCASH ) == FALSE ) {  ACCTCASH <- "Cash" }



	## OTHER ACCOUNTING METHOD
	## Should return a string, not an "X" indicating checkbox

	V_990AMOpost2013 <- "//Return/ReturnData/IRS990/MethodOfAccountingOtherInd/@methodOfAccountingOtherDesc"
	V_990AMOpre2013  <- "//Return/ReturnData/IRS990/MethodOfAccountingOther/@note"
	V_990AMO.EZpost2013 <- "//Return/ReturnData/IRS990EZ/MethodOfAccountingOtherDesc"
	V_990AMO.EZpre2013  <- "//Return/ReturnData/IRS990EZ/MethodOfAccountingOther"
	accounting.other.xpath <- paste( V_990AMOpost2013, V_990AMOpre2013, V_990AMO.EZpost2013, V_990AMO.EZpre2013, sep="|" )
	ACCTOTHER <- xml_text( xml_find_all( doc, accounting.other.xpath ) ) 

	ACCTOTHER[ length( ACCTOTHER ) == 0]  <- NA



	## ACCOUNTING METHOD (COLLAPSED)

	ACCTMETHOD <- gsub( "NA", "", paste( ACCTACCRUAL, ACCTCASH, ACCTOTHER, sep="" ) )
	ACCTMETHOD[ ACCTMETHOD  == "" ] <- NA



	##   TAX EXEMPT STATUS: represent the 5 possible values, broken out then collapsed
	## EXEMPT STATUS 4947(a)(1)

	V_990.4947post2013 <- "//Return/ReturnData/IRS990/Organization4947a1NotPFInd"
	V_990.4947pre2013  <- "//Return/ReturnData/IRS990/Organization4947a1"
	V_990.4947.EZpost2013 <- "//Return/ReturnData/IRS990EZ/Organization4947a1NotPFInd"
	V_990.4947.EZpre2013  <- "//Return/ReturnData/IRS990EZ/Organization4947a1"
	exempt.4947.xpath <- paste( V_990.4947post2013, V_990.4947pre2013, V_990.4947.EZpost2013, V_990.4947.EZpre2013, sep="|" )
	EXEMPT4947A1 <- xml_text( xml_find_all( doc, exempt.4947.xpath ) ) 

	EXEMPT4947A1[ length( EXEMPT4947A1 ) == 0]  <- NA
	if( is.na( EXEMPT4947A1 ) == FALSE) { EXEMPT4947A1 <- "4947a1" }



	## EXEMPT STATUS 501(c)(other than 3)

	V_990.501Cpost2013 <- "//Return/ReturnData/IRS990/Organization501cInd"
	V_990.501Cpre2013  <- "//Return/ReturnData/IRS990/Organization501c"
	V_990.501C.EZpost2013 <- "//Return/ReturnData/IRS990EZ/Organization501cInd"
	V_990.501C.EZpre2013  <- "//Return/ReturnData/IRS990EZ/Organization501c"
	exempt.501c.xpath <- paste( V_990.501Cpost2013, V_990.501Cpre2013, V_990.501C.EZpost2013, V_990.501C.EZpre2013, sep="|" )
	EXEMPT501C <- xml_text( xml_find_all( doc, exempt.501c.xpath ) ) 

	EXEMPT501C[ length( EXEMPT501C ) == 0]  <- NA
	if( is.na( EXEMPT501C ) == FALSE ) { EXEMPT501C <- "501c" }



	## NUMBER OF EXEMPT STATUS 501(c)(other than 3)

	V_990.501C.NUMpost2013 <- "//Return/ReturnData/IRS990/Organization501cInd/@organization501cTypeTxt"
	V_990.501C.NUMpre2013  <- "//Return/ReturnData/IRS990/Organization501c/@typeOf501cOrganization"
	V_990.501C.NUM.EZpost2013 <- "//Return/ReturnData/IRS990EZ/Organization501cInd/@organization501cTypeTxt"
	V_990.501C.NUM.EZpre2013  <- "//Return/ReturnData/IRS990EZ/Organization501c/@typeOf501cOrganization"
	exempt.num.xpath <- paste( V_990.501C.NUMpost2013, V_990.501C.NUMpre2013, V_990.501C.NUM.EZpost2013, V_990.501C.NUM.EZpre2013, sep="|" )
	EXEMPT501CNUM <- xml_text( xml_find_all( doc, exempt.num.xpath ) ) 



	## EXEMPT STATUS 501(c)(3)

	V_990.501C.3post2013 <- "//Return/ReturnData/IRS990/Organization501c3Ind"
	V_990.501C.3pre2013  <- "//Return/ReturnData/IRS990/Organization501c3"
	V_990.501C.3.EZpost2013 <- "//Return/ReturnData/IRS990EZ/Organization501c3Ind"
	V_990.501C.3.EZpre2013  <- "//Return/ReturnData/IRS990EZ/Organization501c3"
	exempt.501c.3.xpath <- paste( V_990.501C.3post2013, V_990.501C.3pre2013, V_990.501C.3.EZpost2013, V_990.501C.3.EZpre2013, sep="|" )
	EXEMPT501C3 <- xml_text( xml_find_all( doc, exempt.501c.3.xpath ) ) 

	EXEMPT501C3[ length( EXEMPT501C3 ) == 0]  <- NA
	if( is.na( EXEMPT501C3 ) == FALSE ) { EXEMPT501C3 <- "501c3" }



	## EXEMPT STATUS 527

	V_990.527post2013 <- "//Return/ReturnData/IRS990/Organization527Ind"
	V_990.527pre2013  <- "//Return/ReturnData/IRS990/Organization527"
	V_990.527.EZpost2013 <- "//Return/ReturnData/IRS990EZ/Organization527Ind"
	V_990.527.EZpre2013  <- "//Return/ReturnData/IRS990EZ/Organization527"
	exempt.527.xpath <- paste( V_990.527post2013, V_990.527pre2013, V_990.527.EZpost2013, V_990.527.EZpre2013, sep="|" )
	EXEMPT527 <- xml_text( xml_find_all( doc, exempt.527.xpath ) ) 

	EXEMPT527[ length( EXEMPT527 ) == 0]  <- NA
	if( is.na( EXEMPT527 ) == FALSE ) { EXEMPT527 <- "527" }



	## EXEMPT STATUS COLLAPSED

	EXEMPTSTATUS <- gsub( "NA", "", paste( EXEMPT4947A1, EXEMPT501C, EXEMPT501CNUM, EXEMPT501C3, EXEMPT527, sep="" ) )
	EXEMPTSTATUS[ EXEMPTSTATUS  == "" ] <- NA



	#------------------------------------------------------------------------------------------------------------------------
	#####  PART I - ACTIVITIES AND GOVERNANCE 

	## MISSION

	V_990Mpost2013 <- "//Return/ReturnData/IRS990/ActivityOrMissionDesc"
	V_990Mpre2013 <- "//Return/ReturnData/IRS990/ActivityOrMissionDescription"
	V_990M.EZpost2013 <- "//Return/ReturnData/IRS990EZ/PrimaryExemptPurposeTxt"
	V_990M.EZpre2013 <- "//Return/ReturnData/IRS990EZ/PrimaryExemptPurpose"
	mission.xpath <- paste( V_990Mpost2013, V_990Mpre2013, V_990M.EZpost2013, V_990M.EZpre2013, sep="|" )
	MISSION <- xml_text( xml_find_all( doc, mission.xpath ) ) 



	## DISCONTINUED OPERATIONS OR DISPOSAL OF >25% ASSETS
	## Double-check this xpath

	V_990DOpost2013 <- "//Return/ReturnData/IRS990/ContractTerminationInd"
	V_990DOpre2013 <- "//Return/ReturnData/IRS990/TerminationOrContraction"
	discontinued.ops.xpath <- paste( V_990DOpost2013, V_990DOpre2013, sep="|" )
	DISCOPS <- xml_text( xml_find_all( doc, discontinued.ops.xpath ) ) 



	## VOTING MEMBERS

	V_990VMpost2013 <- "//Return/ReturnData/IRS990/VotingMembersGoverningBodyCnt"
	V_990VMpre2013  <- "//Return/ReturnData/IRS990/NbrVotingMembersGoverningBody"
	voting.mbrs.xpath <- paste( V_990VMpost2013, V_990VMpre2013, sep="|" )
	VOTINGMEMBERS <- xml_text( xml_find_all( doc, voting.mbrs.xpath ) ) 



	## INDEPENDENT VOTING MEMBERS

	V_990IVMpost2013 <- "//Return/ReturnData/IRS990/VotingMembersIndependentCnt"
	V_990IVMpre2013  <- "//Return/ReturnData/IRS990/NbrIndependentVotingMembers"
	indvoting.mbrs.xpath <- paste( V_990IVMpost2013, V_990IVMpre2013, sep="|" )
	INDVOTINGMEMBERS <- xml_text( xml_find_all( doc, indvoting.mbrs.xpath ) ) 



	## TOTAL EMPLOYEE COUNT

	V_990TEpost2013 <- "//Return/ReturnData/IRS990/TotalEmployeeCnt"
	V_990TEpre2013  <- "//Return/ReturnData/IRS990/TotalNbrEmployees"
	tot.employee.xpath <- paste( V_990TEpost2013, V_990TEpre2013, sep="|" )
	TOTEMPLOYEE <- xml_text( xml_find_all( doc, tot.employee.xpath ) ) 



	## TOTAL VOLUNTEER COUNT

	V_990TVpost2013 <- "//Return/ReturnData/IRS990/TotalVolunteersCnt"
	V_990TVpre2013  <- "//Return/ReturnData/IRS990/TotalNbrVolunteers"
	tot.volunteers.xpath <- paste( V_990TVpost2013, V_990TVpre2013, sep="|" )
	TOTVOLUNTEERS <- xml_text( xml_find_all( doc, tot.volunteers.xpath ) )



	## TOTAL GROSS UBI

	V_990TGUpost2013 <- "//Return/ReturnData/IRS990/TotalGrossUBIAmt"
	V_990TGUpre2013  <- "//Return/ReturnData/IRS990/TotalGrossUBI"
	tot.ubi.xpath <- paste( V_990TGUpost2013, V_990TGUpre2013, sep="|" )
	TOTUBI <- xml_text( xml_find_all( doc, tot.ubi.xpath ) )
	TOTUBI <- zeroPC( TOTUBI )



	## NET UBI

	V_990NUpost2013 <- "//Return/ReturnData/IRS990/NetUnrelatedBusTxblIncmAmt"
	V_990NUpre2013  <- "//Return/ReturnData/IRS990/NetUnrelatedBusinessTxblIncome"
	net.ubi.xpath <- paste( V_990NUpost2013, V_990NUpre2013, sep="|" )
	NETUBI <- xml_text( xml_find_all( doc, net.ubi.xpath ) )
  	NETUBI <- zeroPC( NETUBI )


  
	#------------------------------------------------------------------------------------------------------------------------
	#####  PART I - REVENUES 
	## The 990-PC forms split columns in this area into Current Year and Prior Year, the 990-EZs do not. 990-EZ data
	## in this section only maps to current year values unless indicated otherwise.

	## PRIOR YEAR CONTRIBUTIONS

	V_990PCpost2013 <- "//Return/ReturnData/IRS990/ContributionsGrantsPriorYear"
	V_990PCpre2013  <- "//Return/ReturnData/IRS990/PYContributionsGrantsAmt"
	contrib.prior.xpath <- paste( V_990PCpost2013, V_990PCpre2013, sep="|" )
	CONTRIBPRIOR <- xml_text( xml_find_all( doc, contrib.prior.xpath ) ) 
	CONTRIBPRIOR <- zeroPC( CONTRIBPRIOR )


	## CURRENT YEAR CONTRIBUTIONS

	V_990CCpost2013 <- "//Return/ReturnData/IRS990/CYContributionsGrantsAmt"
	V_990CCpre2013  <- "//Return/ReturnData/IRS990/ContributionsGrantsCurrentYear"
	V_990CC.EZpost2013 <- "//Return/ReturnData/IRS990EZ/ContributionsGiftsGrantsEtc"
	V_990CC.EZpre2013  <- "//Return/ReturnData/IRS990EZ/ContributionsGiftsGrantsEtcAmt"
	contrib.current.xpath <- paste( V_990CCpost2013, V_990CCpre2013, V_990CC.EZpost2013, V_990CC.EZpre2013, sep="|" )
	CONTRIBCURRENT <- xml_text( xml_find_all( doc, contrib.current.xpath ) ) 
	CONTRIBCURRENT <- zeroALL( CONTRIBCURRENT )


	## PRIOR YEAR PROGRAM SERVICE REVENUE

	V_990PPSRpost2013 <- "//Return/ReturnData/IRS990/PYProgramServiceRevenueAmt"
	V_990PPSRpre2013  <- "//Return/ReturnData/IRS990/ProgramServiceRevenuePriorYear"
	psr.prior.xpath <- paste( V_990PPSRpost2013, V_990PPSRpre2013, sep="|" )
	PSRPRIOR <- xml_text( xml_find_all( doc, psr.prior.xpath ) ) 
	PSRPRIOR <- zeroPC( PSRPRIOR )

	## CURRENT YEAR PROGRAM SERVICE REVENUE

	V_990CPSRpost2013 <- "//Return/ReturnData/IRS990/CYProgramServiceRevenueAmt"
	V_990CPSRpre2013  <- "//Return/ReturnData/IRS990/ProgramServiceRevenueCY"
	V_990CPSR.EZpost2013 <- "//Return/ReturnData/IRS990EZ/ProgramServiceRevenueAmt"
	V_990CPSR.EZpre2013  <- "//Return/ReturnData/IRS990EZ/ProgramServiceRevenue"
	psr.current.xpath <- paste( V_990CPSRpost2013, V_990CPSRpre2013, V_990CPSR.EZpost2013, V_990CPSR.EZpre2013, sep="|" )
	PSRCURRENT <- xml_text( xml_find_all( doc, psr.current.xpath ) )  
	PSRCURRENT <- zeroALL( PSRCURRENT )


	## PRIOR YEAR INVESTMENT INCOME

	V_990PIVpost2013 <- "//Return/ReturnData/IRS990/PYInvestmentIncomeAmt"
	V_990PIVpre2013  <- "//Return/ReturnData/IRS990/InvestmentIncomePriorYear"
	invest.income.prior.xpath <- paste( V_990PIVpost2013, V_990PIVpre2013, sep="|" )
	INVINCPRIOR <- xml_text( xml_find_all( doc, invest.income.prior.xpath ) )  
	INVINCPRIOR <- zeroPC( INVINCPRIOR )


	## CURRENT YEAR INVESTMENT INCOME

	V_990CIVpost2013 <- "//Return/ReturnData/IRS990/CYInvestmentIncomeAmt"
	V_990CIVpre2013  <- "//Return/ReturnData/IRS990/InvestmentIncomeCurrentYear"
	V_990CIV.EZpost2013 <- "//Return/ReturnData/IRS990EZ/InvestmentIncomeAmt"
	V_990CIV.EZpre2013  <- "//Return/ReturnData/IRS990EZ/InvestmentIncome"
	invest.income.current.xpath <- paste( V_990CIVpost2013, V_990CIVpre2013, V_990CIV.EZpost2013, V_990CIV.EZpre2013, sep="|" )
	INVINCCURRENT <- xml_text( xml_find_all( doc, invest.income.current.xpath ) )  
	INVINCCURRENT <- zeroALL( INVINCCURRENT )


	## PRIOR YEAR OTHER REVENUE

	V_990PORpost2013 <- "//Return/ReturnData/IRS990/PYOtherRevenueAmt"
	V_990PORpre2013  <- "//Return/ReturnData/IRS990/OtherRevenuePriorYear"
	other.rev.prior.xpath <- paste( V_990PORpost2013, V_990PORpre2013, sep="|" )
	OTHERREVPRIOR <- xml_text( xml_find_all( doc, other.rev.prior.xpath ) )  
	OTHERREVPRIOR <- zeroPC( OTHERREVPRIOR )


	## CURRENT YEAR OTHER REVENUE

	V_990CORpost2013 <- "//Return/ReturnData/IRS990/CYOtherRevenueAmt"
	V_990CORpre2013  <- "//Return/ReturnData/IRS990/OtherRevenueCurrentYear"
	V_990CR.EZpost2013 <- "//Return/ReturnData/IRS990EZ/OtherRevenueTotalAmt"
	V_990CR.EZpre2013  <- "//Return/ReturnData/IRS990EZ/OtherRevenueTotal"
	other.rev.current.xpath <- paste( V_990CORpost2013, V_990CORpre2013, V_990CR.EZpost2013, V_990CR.EZpre2013, sep="|" )
	OTHERREVCURRENT <- xml_text( xml_find_all( doc, other.rev.current.xpath ) )  
	OTHERREVCURRENT <- zeroALL( OTHERREVCURRENT )



	## PRIOR YEAR TOTAL REVENUE

	V_990PTRpost2013 <- "//Return/ReturnData/IRS990/PYTotalRevenueAmt"
	V_990PTRpre2013  <- "//Return/ReturnData/IRS990/TotalRevenuePriorYear"
	total.rev.prior.xpath <- paste( V_990PTRpost2013, V_990PTRpre2013, sep="|" )
	TOTALREVPRIOR <- xml_text( xml_find_all( doc, total.rev.prior.xpath ) )  
	TOTALREVPRIOR <- zeroPC( TOTALREVPRIOR )


	## CURRENT YEAR TOTAL REVENUE

	V_990CTRpost2013 <- "//Return/ReturnData/IRS990/CYTotalRevenueAmt"
	V_990CTRpre2013  <- "//Return/ReturnData/IRS990/TotalRevenueCurrentYear"
	V_990CTR.EZpost2013 <- "//Return/ReturnData/IRS990EZ/TotalRevenueAmt"
	V_990CTR.EZpre2013  <- "//Return/ReturnData/IRS990EZ/TotalRevenue"
	total.rev.current.xpath <- paste( V_990CTRpost2013, V_990CTRpre2013, V_990CTR.EZpost2013, V_990CTR.EZpre2013, sep="|" )
	TOTALREVCURRENT <- xml_text( xml_find_all( doc, total.rev.current.xpath ) )  
	TOTALREVCURRENT <- zeroALL( TOTALREVCURRENT )

	

	#------------------------------------------------------------------------------------------------------------------------
	#####  PART I - REVENUES (990EZ-specific fields)
	## Some of the paths here are on the 990 PC but in different areas. They are included here to help
	## with mapping across forms. Some of the PC fields here roll up to map to 1 EZ field.

	## MEMBERSHIP DUES

	V_990MBRDpost2013 <- "//Return/ReturnData/IRS990/MembershipDuesAmt"
	V_990MBRDpre2013  <- "//Return/ReturnData/IRS990/MembershipDues"
	V_990MBRD.EZpost2013 <- "//Return/ReturnData/IRS990EZ/MembershipDuesAmt"
	V_990MBRD.EZpre2013  <- "//Return/ReturnData/IRS990EZ/MembershipDues"
	member.dues.xpath <- paste( V_990MBRDpost2013, V_990MBRDpre2013, V_990MBRD.EZpost2013, V_990MBRD.EZpre2013, sep="|" )
	MEMBERDUES <- xml_text( xml_find_all( doc, member.dues.xpath ) )  
	MEMBERDUES <- zeroALL( MEMBERDUES )


	## GROSS SALES OF NON-INVENTORY ASSETS

	V_990GSNApost2013 <- "//Return/ReturnData/IRS990/GrossAmountSalesAssetsGrp/OtherAmt"
	V_990GSNApre2013  <- "//Return/ReturnData/IRS990/GrossAmountSalesAssets/Other"
	V_990GSNA.EZpost2013 <- "//Return/ReturnData/IRS990EZ/SaleOfAssetsGrossAmt"
	V_990GSNA.EZpre2013  <- "//Return/ReturnData/IRS990EZ/GrossAmountFromSaleOfAssets"
	grosssales.nonasset.xpath <- paste( V_990GSNApost2013, V_990GSNApre2013, V_990GSNA.EZpost2013, V_990GSNA.EZpre2013, sep="|" )
	GROSSSALESOTHER <- xml_text( xml_find_all( doc, grosssales.nonasset.xpath ) )  
	GROSSSALESOTHER <- zeroALL( GROSSSALESOTHER )


	## COST AND SALES EXPENSES FROM NON-INVENTORY ASSET SALES

	V_990TSNApost2013 <- "//Return/ReturnData/IRS990/LessCostOthBasisSalesExpnssGrp/OtherAmt"
	V_990TSNApre2013  <- "//Return/ReturnData/IRS990/LessCostOthBasisSalesExpenses/Other"
	V_990TSNA.EZpost2013 <- "//Return/ReturnData/IRS990EZ/CostOrOtherBasisExpenseSaleAmt"
	V_990TSNA.EZpre2013  <- "//Return/ReturnData/IRS990EZ/CostOtherBasisAndSalesExpenses"
	totalsales.nonasset.xpath <- paste( V_990TSNApost2013, V_990TSNApre2013, V_990TSNA.EZpost2013, V_990TSNA.EZpre2013, sep="|" )
	SALESCOSTOTHER <- xml_text( xml_find_all( doc, totalsales.nonasset.xpath ) )  
	SALESCOSTOTHER <- zeroALL( SALESCOSTOTHER )


	## NET SALES OF NON-INVENTORY ASSETS
	## includes securities for the PC forms

	V_990NSNApost2013 <- "//Return/ReturnData/IRS990/NetGainOrLossInvestmentsGrp/TotalRevenueColumnAmt"
	V_990NSNApre2013  <- "//Return/ReturnData/IRS990/NetGainOrLossInvestments/TotalRevenueColumn"
	V_990NSNA.EZpost2013 <- "//Return/ReturnData/IRS990EZ/GainOrLossFromSaleOfAssetsAmt"
	V_990NSNA.EZpre2013  <- "//Return/ReturnData/IRS990EZ/GainOrLossFromSaleOfAssets"
	netsales.nonassets.xpath <- paste( V_990NSNApost2013, V_990NSNApre2013, V_990NSNA.EZpost2013, V_990NSNA.EZpre2013, sep="|" )
	NETSALESOTHER <- xml_text( xml_find_all( doc, netsales.nonassets.xpath ) )  
	NETSALESOTHER <- zeroALL( NETSALESOTHER )


	## GROSS INCOME FROM GAMING

	V_990GIGpost2013 <- "//Return/ReturnData/IRS990/GamingGrossIncomeAmt"
	V_990GIGpre2013  <- "//Return/ReturnData/IRS990/GrossIncomeGaming"
	V_990GIG.EZpost2013 <- "//Return/ReturnData/IRS990EZ/GamingGrossIncomeAmt"
	V_990GIG.EZpre2013  <- "//Return/ReturnData/IRS990EZ/GamingGrossIncome"
	grossinc.gaming.xpath <- paste( V_990GIGpost2013, V_990GIGpre2013, V_990GIG.EZpost2013, V_990GIG.EZpre2013, sep="|" )
	GROSSINCGAMING <- xml_text( xml_find_all( doc, grossinc.gaming.xpath ) )  
	GROSSINCGAMING <- zeroALL( GROSSINCGAMING )


	## GROSS INCOME FROM FUNDRAISING EVENTS

	V_990GIFpost2013 <- "//Return/ReturnData/IRS990/FundraisingGrossIncomeAmt"
	V_990GIFpre2013  <- "//Return/ReturnData/IRS990/GrossIncomeFundraisingEvents"
	V_990GIF.EZpost2013 <- "//Return/ReturnData/IRS990EZ/FundraisingGrossIncomeAmt"
	V_990GIF.EZpre2013  <- "//Return/ReturnData/IRS990EZ/FundraisingGrossIncome"
	grossinc.fundrs.xpath <- paste( V_990GIFpost2013, V_990GIFpre2013, V_990GIF.EZpost2013, V_990GIF.EZpre2013, sep="|" )
	GROSSINCFNDEVENTS <- xml_text( xml_find_all( doc, grossinc.fundrs.xpath ) )  
	GROSSINCFNDEVENTS <- zeroALL( GROSSINCFNDEVENTS )


	## EXPENSES FROM GAMING EVENTS
	## PC only

	V_990gamexppost2013 <- "//Return/ReturnData/IRS990/GamingDirectExpensesAmt"
	V_990gamexppre2013 <- "//Return/ReturnData/IRS990/GamingDirectExpenses"
	gaming.exp.xpath <- paste( V_990gamexppost2013, V_990gamexppre2013, sep="|" )
	GAMINGEXP <- xml_text( xml_find_all( doc, gaming.exp.xpath ) )  

	GAMINGEXP[ length( GAMINGEXP ) == 0 ]  <- NA
	GAMINGEXP <- zeroPC( GAMINGEXP )


	## EXPENSES FROM FUNDRAISING EVENTS
	## PC only

	V_990fndexppost2013 <- "//Return/ReturnData/IRS990/FundraisingDirectExpensesAmt"
	V_990fndexppre2013 <- "//Return/ReturnData/IRS990/FundraisingDirectExpenses"
	fnd.events.exp.xpath <- paste( V_990fndexppost2013, V_990fndexppre2013, sep="|" )
	FNDEVENTSEXP <- xml_text( xml_find_all( doc, fnd.events.exp.xpath ) )  

	FNDEVENTSEXP[ length( FNDEVENTSEXP ) == 0]  <- NA
	FNDEVENTSEXP <- zeroPC( FNDEVENTSEXP )


	## EXPENSES FROM GAMING AND FUNDRAISING EVENTS

	if( FORMTYPE == "990EZ" ){
	  V_990EGF.EZpost2013 <- "//Return/ReturnData/IRS990EZ/SpecialEventsDirectExpensesAmt"
	  V_990EGF.EZpre2013  <- "//Return/ReturnData/IRS990EZ/SpecialEventsDirectExpenses"
	  exp.gaming.fundrs.xpath <- paste( V_990EGF.EZpost2013, V_990EGF.EZpre2013, sep="|" )
	  EXPGAMINGFNDEVENTS <- xml_text( xml_find_all( doc, exp.gaming.fundrs.xpath ) )
	} else if( FORMTYPE == "990" ){
	  EXPGAMINGFNDEVENTS <- sum( as.numeric( GAMINGEXP ), as.numeric( FNDEVENTSEXP ), na.rm=T )
	}
	EXPGAMINGFNDEVENTS <- as.character( EXPGAMINGFNDEVENTS )
	EXPGAMINGFNDEVENTS <- zeroALL( EXPGAMINGFNDEVENTS )


	## NET GAIN OR LOSS FROM GAMING EVENTS
	## PC only

	V_990totrevgampost2013 <- "//Return/ReturnData/IRS990/NetIncomeFromGamingGrp/TotalRevenueColumnAmt"
	V_990totrevgampre2013 <- "//Return/ReturnData/IRS990/NetIncomeFromGaming/TotalRevenueColumn"
	gaming.net.xpath <- paste( V_990totrevgampost2013, V_990totrevgampre2013, sep="|" )
	GAMINGNET <- xml_text( xml_find_all( doc, gaming.net.xpath ) )  

	GAMINGNET[ length( GAMINGNET ) == 0]  <- NA
	GAMINGNET <- zeroPC( GAMINGNET )


	## NET GAIN OR LOSS FROM FUNDRAISING EVENTS
	## PC only

	V_990totrevfndpost2013 <- "//Return/ReturnData/IRS990/NetIncmFromFundraisingEvtGrp/TotalRevenueColumnAmt"
	V_990totrevfndpre2013 <- "//Return/ReturnData/IRS990/NetIncomeFromFundraisingEvents/TotalRevenueColumn"
	fnd.events.net.xpath <- paste( V_990totrevfndpost2013, V_990totrevfndpre2013, sep="|" )
	FNDEVENTSNET <- xml_text( xml_find_all( doc, fnd.events.net.xpath ) )  

	FNDEVENTSNET[ length( FNDEVENTSNET ) == 0]  <- NA
	FNDEVENTSNET <- zeroPC( FNDEVENTSNET )


	## NET DIFFERENCE FOR GAMING AND FUNDRAISING EVENTS

	if( FORMTYPE == "990EZ" ){
	  V_990NGF.EZpost2013 <- "//Return/ReturnData/IRS990EZ/SpecialEventsNetIncomeLossAmt"
	  V_990NGF.EZpre2013  <- "//Return/ReturnData/IRS990EZ/SpecialEventsNetIncomeLoss"
	  net.gaming.fundrs.xpath <- paste( V_990NGF.EZpost2013, V_990NGF.EZpre2013, sep="|" )
	  NETGAMINGFNDEVENTS <- xml_text( xml_find_all( doc, net.gaming.fundrs.xpath ) )  
	} else if( FORMTYPE == "990" ){
	  NETGAMINGFNDEVENTS <- sum( as.numeric( GAMINGNET ), as.numeric( FNDEVENTSNET ), na.rm=T ) 
	}
	NETGAMINGFNDEVENTS <- as.character( NETGAMINGFNDEVENTS )
	NETGAMINGFNDEVENTS <- zeroALL( NETGAMINGFNDEVENTS )


	## GROSS SALES OF INVENTORY ASSETS

	V_990GSIpost2013 <- "//Return/ReturnData/IRS990/GrossSalesOfInventoryAmt"
	V_990GSIpre2013  <- "//Return/ReturnData/IRS990/GrossSalesOfInventory"
	V_990GSI.EZpost2013 <- "//Return/ReturnData/IRS990EZ/GrossSalesOfInventoryAmt"
	V_990GSI.EZpre2013  <- "//Return/ReturnData/IRS990EZ/GrossSalesOfInventory"
	gross.salesinv.xpath <- paste( V_990GSIpost2013, V_990GSIpre2013, V_990GSI.EZpost2013, V_990GSI.EZpre2013, sep="|" )
	GROSSSALESINV <- xml_text( xml_find_all( doc, gross.salesinv.xpath ) )  
	GROSSSALESINV <- zeroALL( GROSSSALESINV )


	## COST OF GOODS SOLD

	V_990CSIpost2013 <- "//Return/ReturnData/IRS990/CostOfGoodsSoldAmt"
	V_990CSIpre2013  <- "//Return/ReturnData/IRS990/CostOfGoodsSold"
	V_990CSI.EZpost2013 <- "//Return/ReturnData/IRS990EZ/CostOfGoodsSoldAmt"
	V_990CSI.EZpre2013  <- "//Return/ReturnData/IRS990EZ/CostOfGoodsSold"
	cost.salesinv.xpath <- paste( V_990CSIpost2013, V_990CSIpre2013, V_990CSI.EZpost2013, V_990CSI.EZpre2013, sep="|" )
	SALESCOSTINV <- xml_text( xml_find_all( doc, cost.salesinv.xpath ) )  
	SALESCOSTINV <- zeroALL( SALESCOSTINV )


	## NET DIFFERENCE OF SALES MINUS COST OF GOODS

	V_990NSIpost2013 <- "//Return/ReturnData/IRS990/NetIncomeOrLossGrp/TotalRevenueColumnAmt"
	V_990NSIpre2013  <- "//Return/ReturnData/IRS990/NetIncomeOrLoss/TotalRevenueColumn"
	V_990NSI.EZpost2013 <- "//Return/ReturnData/IRS990EZ/GrossProfitLossSlsOfInvntryAmt"
	V_990NSI.EZpre2013  <- "//Return/ReturnData/IRS990EZ/GroProfitLossSalesOfInventory"
	net.salesinv.xpath <- paste( V_990NSIpost2013, V_990NSIpre2013, V_990NSI.EZpost2013, V_990NSI.EZpre2013, sep="|" )
	NETSALESINV <- xml_text( xml_find_all( doc, net.salesinv.xpath ) )  
	NETSALESINV <- zeroALL( NETSALESINV )


	
	#------------------------------------------------------------------------------------------------------------------------
	#####  PART I - EXPENSES
	## The 990-PC forms split columns in this area into Current Year and Prior Year, the 990-EZs do not. 990-EZ data
	## in this section only maps to current year values unless indicated otherwise.

	## PRIOR YEAR GRANTS PAID

	V_990PGPpost2013 <- "//Return/ReturnData/IRS990/PYGrantsAndSimilarPaidAmt"
	V_990PGPpre2013  <- "//Return/ReturnData/IRS990/GrantsAndSimilarAmntsPriorYear"
	grants.paid.prior.xpath <- paste( V_990PGPpost2013, V_990PGPpre2013, sep="|" )
	GRANTSPAIDPRIOR <- xml_text( xml_find_all( doc, grants.paid.prior.xpath ) ) 
	GRANTSPAIDPRIOR <- zeroPC( GRANTSPAIDPRIOR )


	## CURRENT YEAR GRANTS PAID

	V_990CGPpost2013 <- "//Return/ReturnData/IRS990/CYGrantsAndSimilarPaidAmt"
	V_990CGPpre2013  <- "//Return/ReturnData/IRS990/GrantsAndSimilarAmntsCY"
	V_990CGP.EZpost2013 <- "//Return/ReturnData/IRS990EZ/GrantsAndSimilarAmountsPaidAmt"
	V_990CGP.EZpre2013  <- "//Return/ReturnData/IRS990EZ/GrantsAndSimilarAmountsPaid"
	grants.paid.current.xpath <- paste( V_990CGPpost2013, V_990CGPpre2013, V_990CGP.EZpost2013, V_990CGP.EZpre2013, sep="|" )
	GRANTSPAIDCURRENT <- xml_text( xml_find_all( doc, grants.paid.current.xpath ) ) 
	GRANTSPAIDCURRENT <- zeroALL( GRANTSPAIDCURRENT )


	## PRIOR YEAR BENEFITS PAID TO OR FOR MEMBERS 

	V_990PBPpost2013 <- "//Return/ReturnData/IRS990/PYBenefitsPaidToMembersAmt"
	V_990PBPpre2013  <- "//Return/ReturnData/IRS990/BenefitsPaidToMembersPriorYear"
	benefits.paid.prior.xpath <- paste( V_990PGPpost2013, V_990PGPpre2013, sep="|" )
	MEMBERBENPRIOR <- xml_text( xml_find_all( doc, benefits.paid.prior.xpath ) ) 
	MEMBERBENPRIOR <- zeroPC( MEMBERBENPRIOR )


	## CURRENT YEAR BENEFITS PAID TO OR FOR MEMBERS 

	V_990CBPpost2013 <- "//Return/ReturnData/IRS990/CYBenefitsPaidToMembersAmt"
	V_990CBPpre2013  <- "//Return/ReturnData/IRS990/BenefitsPaidToMembersCY"
	V_990CBP.EZpost2013 <- "//Return/ReturnData/IRS990EZ/BenefitsPaidToOrForMembersAmt"
	V_990CBP.EZpre2013  <- "//Return/ReturnData/IRS990EZ/BenefitsPaidToOrForMembers"
	benefits.paid.current.xpath <- paste( V_990CBPpost2013, V_990CBPpre2013, V_990CBP.EZpost2013, V_990CBP.EZpre2013, sep="|" )
	MEMBERBENCURRENT <- xml_text( xml_find_all( doc, benefits.paid.current.xpath ) ) 
	MEMBERBENCURRENT <- zeroALL( MEMBERBENCURRENT )


	## PRIOR YEAR SALARIES PAID

	V_990PSPpost2013 <- "//Return/ReturnData/IRS990/PYSalariesCompEmpBnftPaidAmt"
	V_990PSPpre2013  <- "//Return/ReturnData/IRS990/SalariesEtcPriorYear"
	salaries.prior.xpath <- paste( V_990PSPpre2013, V_990PSPpost2013, sep="|" )
	SALARIESPRIOR <- xml_text( xml_find_all( doc, salaries.prior.xpath ) ) 
	SALARIESPRIOR <- zeroPC( SALARIESPRIOR )


	## CURRENT YEAR SALARIES PAID

	V_990CSPpost2013 <- "//Return/ReturnData/IRS990/CYSalariesCompEmpBnftPaidAmt"
	V_990CSPpre2013  <- "//Return/ReturnData/IRS990/SalariesEtcCurrentYear"
	V_990CSP.EZpost2013 <- "//Return/ReturnData/IRS990EZ/SalariesOtherCompEmplBnftAmt"
	V_990CSP.EZpre2013  <- "//Return/ReturnData/IRS990EZ/SalariesOtherCompEmplBenefits"
	salaries.current.xpath <- paste( V_990CSPpost2013, V_990CSPpre2013, V_990CSP.EZpost2013, V_990CSP.EZpre2013, sep="|" )
	SALARIESCURRENT <- xml_text( xml_find_all( doc, salaries.current.xpath ) ) 
	SALARIESCURRENT <- zeroALL( SALARIESCURRENT )


	## PRIOR YEAR PROFESSIONAL FUNDRAISING FEES

	V_990PFFpost2013 <- "//Return/ReturnData/IRS990/PYTotalProfFndrsngExpnsAmt"
	V_990PFFpre2013  <- "//Return/ReturnData/IRS990/TotalProfFundrsngExpPriorYear"
	profund.fees.prior.xpath <- paste( V_990PFFpre2013, V_990PFFpost2013, sep="|" )
	PROFUNDFEESPRIOR <- xml_text( xml_find_all( doc, profund.fees.prior.xpath ) ) 
	PROFUNDFEESPRIOR <- zeroPC( PROFUNDFEESPRIOR )


	## CURRENT YEAR PROFESSIONAL FUNDRAISING FEES

	V_990CFFpost2013 <- "//Return/ReturnData/IRS990/CYTotalProfFndrsngExpnsAmt"
	V_990CFFpre2013  <- "//Return/ReturnData/IRS990/TotalProfFundrsngExpCY"
	profund.fees.current.xpath <- paste( V_990CFFpost2013, V_990CFFpre2013, sep="|" )
	PROFUNDFEESCURRENT <- xml_text( xml_find_all( doc, profund.fees.current.xpath ) ) 
	PROFUNDFEESCURRENT <- zeroPC( PROFUNDFEESCURRENT )


	## TOTAL FUNDRAISING EXPENSES

	V_990TFFpost2013 <- "//Return/ReturnData/IRS990/CYTotalFundraisingExpenseAmt"
	V_990TFFpre2013  <- "//Return/ReturnData/IRS990/TotalFundrsngExpCurrentYear"
	totexp.fundrs.xpath <- paste( V_990TFFpost2013, V_990TFFpre2013, sep="|" )
	TOTFUNDEXP <- xml_text( xml_find_all( doc, totexp.fundrs.xpath ) ) 
	TOTFUNDEXP <- zeroPC( TOTFUNDEXP )
	

	##   FEES FOR SERVICES are broken out on PC and consolidated in EZ. 
	## This section consolidates the PC values

	## FEES FOR SERVICES: MANAGEMENT

	V_990F4S.mgmt.post2013 <- "//Return/ReturnData/IRS990/FeesForServicesManagementGrp/TotalAmt"
	V_990F4S.mgmt.pre2013 <- "//Return/ReturnData/IRS990/FeesForServicesManagement/Total"
	fees.mgmt.xpath <- paste( V_990F4S.mgmt.post2013, V_990F4S.mgmt.pre2013, sep="|" )
	FEESMGMT <- xml_text( xml_find_all( doc, fees.mgmt.xpath ) ) 

	FEESMGMT[ length( FEESMGMT ) == 0]  <- NA
	FEESMGMT <- zeroPC( FEESMGMT )


	## FEES FOR SERVICES: LEGAL

	V_990F4S.legal.post2013 <- "//Return/ReturnData/IRS990/FeesForServicesLegalGrp/TotalAmt"  
	V_990F4S.legal.pre2013 <- "//Return/ReturnData/IRS990/FeesForServicesLegal/Total"
	fees.legal.xpath <- paste( V_990F4S.legal.post2013, V_990F4S.legal.pre2013, sep="|" )
	FEESLEGAL <- xml_text( xml_find_all( doc, fees.legal.xpath ) ) 

	FEESLEGAL[ length( FEESLEGAL ) == 0]  <- NA
	FEESLEGAL <- zeroPC( FEESLEGAL )


	## FEES FOR SERVICES: ACCOUNTING

	V_990F4S.accting.post2013 <- "//Return/ReturnData/IRS990/FeesForServicesAccountingGrp/TotalAmt"
	V_990F4S.accting.pre2013 <- "//Return/ReturnData/IRS990/FeesForServicesAccounting/Total"
	fees.acct.xpath <- paste( V_990F4S.accting.post2013, V_990F4S.accting.pre2013, sep="|" )
	FEESACCT <- xml_text( xml_find_all( doc, fees.acct.xpath ) ) 

	FEESACCT[ length( FEESACCT ) == 0]  <- NA
	FEESACCT <- zeroPC( FEESACCT )


	## FEES FOR SERVICES: LOBBYING

	V_990F4S.lobbying.post2013 <- "//Return/ReturnData/IRS990/FeesForServicesLobbyingGrp/TotalAmt"
	V_990F4S.lobbying.pre2013 <- "//Return/ReturnData/IRS990/FeesForServicesLobbying/Total"
	fees.lobby.xpath <- paste( V_990F4S.lobbying.post2013, V_990F4S.lobbying.pre2013, sep="|" )
	FEESLOBBY <- xml_text( xml_find_all( doc, fees.lobby.xpath ) ) 

	FEESLOBBY[ length( FEESLOBBY ) == 0]  <- NA
	FEESLOBBY <- zeroPC( FEESLOBBY )


	## FEES FOR SERVICES: PROFESSIONAL FUNDRAISING
	## Should equal PROFUNDFEESCURRENT

	V_990F4S.profundserv.post2013 <- "//Return/ReturnData/IRS990/FeesForServicesProfFundraising/TotalAmt"
	V_990F4S.profundserv.pre2013 <- "//Return/ReturnData/IRS990/FeesForServicesProfFundraising/Total"
	fees.profund.xpath <- paste( V_990F4S.profundserv.post2013, V_990F4S.profundserv.pre2013, sep="|" )
	FEESPROFND <- xml_text( xml_find_all( doc, fees.profund.xpath ) ) 

	FEESPROFND[ length( FEESPROFND ) == 0]  <- NA
	FEESPROFND <- zeroPC( FEESPROFND )


	## FEES FOR SERVICES: INVESTMENT MANAGEMENT

	V_990F4S.invmgmt.post2013 <- "//Return/ReturnData/IRS990/FeesForSrvcInvstMgmntFeesGrp/TotalAmt"
	V_990F4S.invmgmt.pre2013 <- "//Return/ReturnData/IRS990/FeesForServicesInvstMgmntFees/Total"
	fees.invmgmt.xpath <- paste( V_990F4S.invmgmt.post2013, V_990F4S.invmgmt.pre2013, sep="|" )
	FEESINVMGMT <- xml_text( xml_find_all( doc, fees.invmgmt.xpath ) ) 

	FEESINVMGMT[ length( FEESINVMGMT ) == 0]  <- NA
	FEESINVMGMT <- zeroPC( FEESINVMGMT )


	## FEES FOR SERVICES: OTHER

	V_990F4S.other.post2013 <- "//Return/ReturnData/IRS990/FeesForServicesOtherGrp/TotalAmt"
	V_990F4S.other.pre2013 <- "//Return/ReturnData/IRS990/FeesForServicesOther/Total"
	fees.other.xpath <- paste( V_990F4S.other.post2013, V_990F4S.other.pre2013, sep="|" )
	FEESOTHER <- xml_text( xml_find_all( doc, fees.other.xpath ) ) 

	FEESOTHER[ length( FEESOTHER ) == 0]  <- NA
	FEESOTHER <- zeroPC( FEESOTHER )


	## PRO. FEES AND OTHERS TO INDEPENDENT CONTRACTORS   

	V_990PFID.EZpost2013 <- "//Return/ReturnData/IRS990EZ/FeesAndOtherPymtToIndCntrctAmt"
	V_990PFID.EZpre2013  <- "//Return/ReturnData/IRS990EZ/FeesAndOthPymtToIndContractors"
	profees.indep.contractors.xpath <- paste( V_990PFID.EZpost2013, V_990PFID.EZpre2013, sep="|" )

	PROFEESINDEP <- sum( as.numeric( FEESMGMT ), as.numeric( FEESLEGAL ), as.numeric( FEESACCT ), 
			     as.numeric( FEESLOBBY ), as.numeric( FEESPROFND ), as.numeric( FEESINVMGMT ), 
			     as.numeric( FEESOTHER ), na.rm=T ) 
	if( is.na(FEESMGMT) == TRUE & is.na(FEESLEGAL) == TRUE & is.na(FEESACCT) == TRUE 
	    & is.na(FEESLOBBY) == TRUE & is.na(FEESPROFND) == TRUE & is.na(FEESINVMGMT) == TRUE 
	    & is.na(FEESOTHER) == TRUE )
	{
	  PROFEESINDEP <- as.numeric( xml_text( xml_find_all( doc, profees.indep.contractors.xpath ) ) ) 
	}
	PROFEESINDEP <- as.character( PROFEESINDEP )
	PROFEESINDEP <- zeroALL( PROFEESINDEP )
  

	## OCCUPANCY

	V_990RENTpost2013 <- "//Return/ReturnData/IRS990/OccupancyGrp/TotalAmt"
	V_990RENTpre2013  <- "//Return/ReturnData/IRS990/Occupancy/Total"
	V_990RENT.EZpost2013 <- "//Return/ReturnData/IRS990EZ/OccupancyRentUtltsAndMaintAmt"
	V_990RENT.EZpre2013  <- "//Return/ReturnData/IRS990EZ/OccupancyRentUtilitiesAndMaint"
	occupancy.xpath <- paste( V_990RENTpost2013, V_990RENTpre2013, V_990RENT.EZpost2013, V_990RENT.EZpre2013, sep="|" )
	OCCUPANCY <- xml_text( xml_find_all( doc, occupancy.xpath ) ) 
  	OCCUPANCY <- zeroALL( OCCUPANCY )


	## OFFICE EXPENSES

	V_990EXPOFpost2013 <- "//Return/ReturnData/IRS990/OfficeExpensesGrp/TotalAmt"
	V_990EXPOFpre2013  <- "//Return/ReturnData/IRS990/OfficeExpenses/Total"
	V_990EXPOF.EZpost2013 <- "//Return/ReturnData/IRS990EZ/PrintingPublicationsPostageAmt"
	V_990EXPOF.EZpre2013  <- "//Return/ReturnData/IRS990EZ/PrintingPublicationsPostage"
	exp.office.xpath <- paste( V_990EXPOFpost2013, V_990EXPOFpre2013, V_990EXPOF.EZpost2013, V_990EXPOF.EZpre2013, sep="|" )
	OFFICEEXP <- xml_text( xml_find_all( doc, exp.office.xpath ) ) 
  	OFFICEEXP <- zeroALL( OFFICEEXP )


	## PRIOR YEAR OTHER EXPENSES

	V_990POEpost2013 <- "//Return/ReturnData/IRS990/PYOtherExpensesAmt"
	V_990POEpre2013  <- "//Return/ReturnData/IRS990/OtherExpensePriorYear"
	other.exp.prior.xpath <- paste( V_990POEpost2013, V_990POEpre2013, sep="|" )
	OTHEREXPPRIOR <- xml_text( xml_find_all( doc, other.exp.prior.xpath ) ) 
	OTHEREXPPRIOR <- zeroPC( OTHEREXPPRIOR )


	## CURRENT YEAR CURRENT EXPENSES

	V_990COEpost2013 <- "//Return/ReturnData/IRS990/CYOtherExpensesAmt"
	V_990COEpre2013  <- "//Return/ReturnData/IRS990/OtherExpensesCurrentYear"
	V_990COE.EZpost2013 <- "//Return/ReturnData/IRS990EZ/OtherExpensesTotalAmt"
	V_990COE.EZpre2013  <- "//Return/ReturnData/IRS990EZ/OtherExpensesTotal"
	other.exp.current.xpath <- paste( V_990COEpost2013, V_990COEpre2013, V_990COE.EZpost2013, V_990COE.EZpre2013, sep="|" )
	OTHEREXPCURRENT <- xml_text( xml_find_all( doc, other.exp.current.xpath ) ) 
	OTHEREXPCURRENT <- zeroALL( OTHEREXPCURRENT )


	## PRIOR YEAR TOTAL EXPENSES

	V_990PTEpost2013 <- "//Return/ReturnData/IRS990/PYTotalExpensesAmt"
	V_990PTEpre2013  <- "//Return/ReturnData/IRS990/TotalExpensesPriorYear"
	total.exp.prior.xpath <- paste( V_990PTEpost2013, V_990PTEpre2013, sep="|" )
	TOTALEXPPRIOR <- xml_text( xml_find_all( doc, total.exp.prior.xpath ) ) 
	TOTALEXPPRIOR <- zeroPC( TOTALEXPPRIOR )


	## CURRENT YEAR TOTAL EXPENSES

	V_990CTEpost2013 <- "//Return/ReturnData/IRS990/CYTotalExpensesAmt"
	V_990CTEpre2013  <- "//Return/ReturnData/IRS990/TotalExpensesCurrentYear"
	V_990CTE.EZpost2013 <- "//Return/ReturnData/IRS990EZ/TotalExpensesAmt"
	V_990CTE.EZpre2013  <- "//Return/ReturnData/IRS990EZ/TotalExpenses"
	total.exp.current.xpath <- paste( V_990CTEpost2013, V_990CTEpre2013, V_990CTE.EZpost2013, V_990CTE.EZpre2013, sep="|" )
	TOTALEXPCURRENT <- xml_text( xml_find_all( doc, total.exp.current.xpath ) ) 
	TOTALEXPCURRENT <- zeroALL( TOTALEXPCURRENT )


	## PRIOR YEAR REVENUES LESS EXPENSES

	V_990PRLEpost2013 <- "//Return/ReturnData/IRS990/PYRevenuesLessExpensesAmt"
	V_990PRLEpre2013  <- "//Return/ReturnData/IRS990/RevenuesLessExpensesPriorYear"
	rev.less.exp.prior.xpath <- paste( V_990PRLEpost2013, V_990PRLEpre2013, sep="|" )
	REVLESSEXPPRIOR <- xml_text( xml_find_all( doc, rev.less.exp.prior.xpath ) ) 
	REVLESSEXPPRIOR <- zeroPC( REVLESSEXPPRIOR )


	## CURRENT YEAR REVENUES LESS EXPENSES

	V_990CRLEpost2013 <- "//Return/ReturnData/IRS990/CYRevenuesLessExpensesAmt"
	V_990CRLEpre2013  <- "//Return/ReturnData/IRS990/RevenuesLessExpensesCY"
	V_990CRLE.EZpost2013 <- "//Return/ReturnData/IRS990EZ/ExcessOrDeficitForYearAmt"
	V_990CRLE.EZpre2013  <- "//Return/ReturnData/IRS990EZ/ExcessOrDeficitForYear"
	rev.less.exp.current.xpath <- paste( V_990CRLEpost2013, V_990CRLEpre2013, V_990CRLE.EZpost2013, V_990CRLE.EZpre2013, sep="|" )
	REVLESSEXPCURRENT <- xml_text( xml_find_all( doc, rev.less.exp.current.xpath ) ) 
	REVLESSEXPCURRENT <- zeroALL( REVLESSEXPCURRENT )

	

	#------------------------------------------------------------------------------------------------------------------------
	#####  PART I - NET ASSETS
	## The 990-PC forms split columns in this area into Beginning of Year and End of Year.
	## Some 990-EZ data is located in Part II of that form.

	## BEGINNING OF YEAR TOTAL ASSETS

	V_990BOYTApost2013 <- "//Return/ReturnData/IRS990/TotalAssetsBOYAmt"
	V_990BOYTApre2013  <- "//Return/ReturnData/IRS990/TotalAssetsBOY"
	V_990BOYTA.EZpost2013 <- "//Return/ReturnData/IRS990EZ/Form990TotalAssetsGrp/BOYAmt"
	V_990BOYTA.EZpre2013  <- "//Return/ReturnData/IRS990EZ/TotalAssets/BOY"
	total.assets.beg.xpath <- paste( V_990BOYTApost2013, V_990BOYTApre2013, V_990BOYTA.EZpost2013, V_990BOYTA.EZpre2013, sep="|" )
	TOTALASSETSBEGYEAR <- xml_text( xml_find_all( doc, total.assets.beg.xpath ) ) 
	TOTALASSETSBEGYEAR <- zeroALL( TOTALASSETSBEGYEAR )


	## END OF YEAR TOTAL ASSETS

	V_990EOYTApost2013 <- "//Return/ReturnData/IRS990/TotalAssetsEOYAmt"
	V_990EOYTApre2013  <- "//Return/ReturnData/IRS990/TotalAssetsEOY"
	V_990EOYTA.EZpost2013 <- "//Return/ReturnData/IRS990EZ/Form990TotalAssetsGrp/EOYAmt"
	V_990EOYTA.EZpre2013  <- "//Return/ReturnData/IRS990EZ/TotalAssets/EOY"
	total.assets.end.xpath <- paste( V_990EOYTApost2013, V_990EOYTApre2013, V_990EOYTA.EZpost2013, V_990EOYTA.EZpre2013, sep="|" )
	TOTALASSETSENDYEAR <- xml_text( xml_find_all( doc, total.assets.end.xpath ) ) 
	TOTALASSETSENDYEAR <- zeroALL( TOTALASSETSENDYEAR )


	## BEGINNING OF YEAR TOTAL LIABILITIES

	V_990BOYTLpost2013 <- "//Return/ReturnData/IRS990/TotalLiabilitiesBOYAmt"
	V_990BOYTLpre2013  <- "//Return/ReturnData/IRS990/TotalLiabilitiesBOY"
	V_990BOYTL.EZpost2013 <- "//Return/ReturnData/IRS990EZ/SumOfTotalLiabilitiesGrp/BOYAmt"
	V_990BOYTL.EZpre2013  <- "//Return/ReturnData/IRS990EZ/SumOfTotalLiabilities/BOY"
	total.liab.beg.xpath <- paste( V_990BOYTLpost2013, V_990BOYTLpre2013, V_990BOYTL.EZpost2013, V_990BOYTL.EZpre2013, sep="|" )
	TOTALLIABBEGYEAR <- xml_text( xml_find_all( doc, total.liab.beg.xpath ) ) 
	TOTALLIABBEGYEAR <- zeroALL( TOTALLIABBEGYEAR )


	## END OF YEAR TOTAL LIABILITIES

	V_990EOYTLpost2013 <- "//Return/ReturnData/IRS990/TotalLiabilitiesEOYAmt"
	V_990EOYTLpre2013  <- "//Return/ReturnData/IRS990/TotalLiabilitiesEOY"
	V_990EOYTL.EZpost2013 <- "//Return/ReturnData/IRS990EZ/SumOfTotalLiabilitiesGrp/EOYAmt"
	V_990EOYTL.EZpre2013  <- "//Return/ReturnData/IRS990EZ/SumOfTotalLiabilities/EOY"
	total.liab.end.xpath <- paste( V_990EOYTLpost2013, V_990EOYTLpre2013, V_990EOYTL.EZpost2013, V_990EOYTL.EZpre2013, sep="|" )
	TOTALLIABENDYEAR <- xml_text( xml_find_all( doc, total.liab.end.xpath ) ) 
	TOTALLIABENDYEAR <- zeroALL( TOTALLIABENDYEAR )


	## BEGINNING OF YEAR NET ASSETS

	V_990BOYNApost2013 <- "//Return/ReturnData/IRS990/NetAssetsOrFundBalancesBOYAmt"
	V_990BOYNApre2013  <- "//Return/ReturnData/IRS990/NetAssetsOrFundBalancesBOY"
	V_990BOYNA.EZpost2013 <- "//Return/ReturnData/IRS990EZ/NetAssetsOrFundBalancesGrp/BOYAmt"
	V_990BOYNA.EZpre2013  <- "//Return/ReturnData/IRS990EZ/NetAssetsOrFundBalances/BOY"
	net.assets.beg.xpath <- paste( V_990BOYNApost2013, V_990BOYNApre2013, V_990BOYNA.EZpost2013, V_990BOYNA.EZpre2013, sep="|" )
	NETASSETSBEGYEAR <- xml_text( xml_find_all( doc, net.assets.beg.xpath ) ) 
	NETASSETSBEGYEAR <- zeroALL( NETASSETSBEGYEAR )


	## OTHER CHANGES IN NET ASSETS

	V_990NAOC.EZpost2013 <- "//Return/ReturnData/IRS990EZ/OtherChangesInNetAssetsAmt"
	V_990NAOC.EZpre2013  <- "//Return/ReturnData/IRS990EZ/OtherChangesInNetAssets"
	net.assets.other.changes.xpath <- paste( V_990NAOC.EZpre2013, sep="|" )
	OTHERASSETSCHANGES <- xml_text( xml_find_all( doc, net.assets.other.changes.xpath ) )  
	OTHERASSETSCHANGES <- zeroEZ( OTHERASSETSCHANGES )


	## END OF YEAR NET ASSETS

	V_990EOYNApost2013 <- "//Return/ReturnData/IRS990/NetAssetsOrFundBalancesEOYAmt"
	V_990EOYNApre2013  <- "//Return/ReturnData/IRS990/NetAssetsOrFundBalancesEOY"
	V_990EOYNA.EZpost2013 <- "//Return/ReturnData/IRS990EZ/NetAssetsOrFundBalancesGrp/EOYAmt"
	V_990EOYNA.EZpre2013  <- "//Return/ReturnData/IRS990EZ/NetAssetsOrFundBalances/EOY"
	net.assets.end.xpath <- paste( V_990EOYNApost2013, V_990EOYNApre2013, V_990EOYNA.EZpost2013, V_990EOYNA.EZpre2013, sep="|" )
	NETASSETSENDYEAR <- xml_text( xml_find_all( doc, net.assets.end.xpath ) ) 
	NETASSETSENDYEAR <- zeroALL( NETASSETSENDYEAR )


	
	#------------------------------------------------------------------------------------------------------------------------
	#####  PART II(EZ) / X (PC) - BALANCE SHEET
	##   Organized to capture the values present on both PC and EZ first, then PC-specific values after

	## BEGINNING OF YEAR CASH

	V_990BOYCpost2013 <- "//Return/ReturnData/IRS990/CashNonInterestBearingGrp/BOYAmt"
	V_990BOYCpre2013  <- "//Return/ReturnData/IRS990/CashNonInterestBearing/BOY"
	cash.beg.xpath <- paste( V_990BOYCpost2013, V_990BOYCpre2013, sep="|" )
	CASHBEGYEAR <- xml_text( xml_find_all( doc, cash.beg.xpath ) ) 

	CASHBEGYEAR[ length( CASHBEGYEAR ) == 0]  <- NA
	CASHBEGYEAR <- zeroPC( CASHBEGYEAR )


	## END OF YEAR CASH

	V_990EOYCpost2013 <- "//Return/ReturnData/IRS990/CashNonInterestBearingGrp/EOYAmt"
	V_990EOYCpre2013  <- "//Return/ReturnData/IRS990/CashNonInterestBearing/EOY"
	cash.end.xpath <- paste( V_990EOYCpost2013, V_990EOYCpre2013, sep="|" )
	CASHENDYEAR <- xml_text( xml_find_all( doc, cash.end.xpath ) ) 

	CASHENDYEAR[ length( CASHENDYEAR ) == 0]  <- NA
	CASHENDYEAR <- zeroPC( CASHENDYEAR )


	## BEGINNING OF YEAR SAVINGS AND TEMPORARY INVESTMENTS

	V_990BOYSIpost2013 <- "//Return/ReturnData/IRS990/SavingsAndTempCashInvstGrp/BOYAmt"
	V_990BOYSIpre2013  <- "//Return/ReturnData/IRS990/SavingsAndTempCashInvestments/BOY"
	sav.tempinv.beg.xpath <- paste( V_990BOYSIpost2013, V_990BOYSIpre2013, sep="|" )
	SAVINVBEGYEAR <- xml_text( xml_find_all( doc, sav.tempinv.beg.xpath ) ) 

	SAVINVBEGYEAR[ length( SAVINVBEGYEAR ) == 0]  <- NA
	SAVINVBEGYEAR <- zeroPC( SAVINVBEGYEAR )


	## END OF YEAR SAVINGS AND TEMPORARY INVESTMENTS

	V_990EOYSIpost2013 <- "//Return/ReturnData/IRS990/SavingsAndTempCashInvstGrp/EOYAmt"
	V_990EOYSIpre2013  <- "//Return/ReturnData/IRS990/SavingsAndTempCashInvestments/EOY"
	sav.tempinv.end.xpath <- paste( V_990EOYSIpost2013, V_990EOYSIpre2013, sep="|" )
	SAVINVENDYEAR <- xml_text( xml_find_all( doc, sav.tempinv.end.xpath ) )

	SAVINVENDYEAR[ length( SAVINVENDYEAR ) == 0]  <- NA
	SAVINVENDYEAR <- zeroPC( SAVINVENDYEAR )


	## BEGINNING OF YEAR CASH, SAVINGS, AND INVESTMENTS
	if( FORMTYPE == "990EZ" ){
	  V_990BOYCSI.EZpost2013 <- "//Return/ReturnData/IRS990EZ/CashSavingsAndInvestmentsGrp/BOYAmt"
	  V_990BOYCSI.EZpre2013  <- "//Return/ReturnData/IRS990EZ/CashSavingsAndInvestments/BOY"
	  cash.inv.beg.xpath <- paste( V_990BOYCSI.EZpost2013, V_990BOYCSI.EZpre2013, sep="|" )
	  CASHINVBEGYEAR <- xml_text( xml_find_all( doc, cash.inv.beg.xpath ) ) 
	} else if( FORMTYPE == "990" ){
	  CASHINVBEGYEAR <- sum( as.numeric( CASHBEGYEAR ), as.numeric( SAVINVBEGYEAR ), na.rm=T ) 
	}
	CASHINVBEGYEAR <- as.character( CASHINVBEGYEAR )
  	CASHINVBEGYEAR <- zeroALL( CASHINVBEGYEAR )


	## END OF YEAR CASH, SAVINGS, AND INVESTMENTS
	if( FORMTYPE == "990EZ" ){
	  V_990EOYNA.EZpost2013 <- "//Return/ReturnData/IRS990EZ/CashSavingsAndInvestmentsGrp/EOYAmt"
	  V_990EOYNA.EZpre2013  <- "//Return/ReturnData/IRS990EZ/CashSavingsAndInvestments/EOY"
	  cash.inv.end.xpath <- paste( V_990EOYNA.EZpost2013, V_990EOYNA.EZpre2013, sep="|" )
	  CASHINVENDYEAR <- xml_text( xml_find_all( doc, cash.inv.end.xpath ) ) 
	} else if( FORMTYPE == "990" ){
	  CASHINVENDYEAR <- sum( as.numeric( CASHENDYEAR ), as.numeric( SAVINVENDYEAR ), na.rm=T )
	}
	CASHINVENDYEAR <- as.character( CASHINVENDYEAR )
  	CASHINVENDYEAR <- zeroALL( CASHINVENDYEAR )


	## COST OF LAND, BUILDINGS, AND EQUIPMENT

	V_990CLBEpost2013 <- "//Return/ReturnData/IRS990/LandBldgEquipCostOrOtherBssAmt"
	V_990CLBEpre2013  <- "//Return/ReturnData/IRS990/LandBuildingsEquipmentBasis"
	lbe.cost.xpath <- paste( V_990CLBEpost2013, V_990CLBEpre2013, sep="|" )
	LANDBLDEQUIPCOST <- xml_text( xml_find_all( doc, lbe.cost.xpath ) ) 
	LANDBLDEQUIPCOST <- zeroPC( LANDBLDEQUIPCOST )


	## DEPRECIATION OF LAND, BUILDINGS, AND EQUIPMENT

	V_990DLBEpost2013 <- "//Return/ReturnData/IRS990/LandBldgEquipAccumDeprecAmt"
	V_990DLBEpre2013  <- "//Return/ReturnData/IRS990/LandBldgEquipmentAccumDeprec"
	lbe.depreciation.xpath <- paste( V_990DLBEpost2013, V_990DLBEpre2013, sep="|" )
	LANDBLDEQUIPDEP <- xml_text( xml_find_all( doc, lbe.depreciation.xpath ) )
	LANDBLDEQUIPDEP <- zeroPC( LANDBLDEQUIPDEP )


	## BEGINNING OF YEAR LAND AND BUILDINGS (AND EQUIPMENT FOR 990-PC ONLY)

	V_990BOYLBpost2013 <- "//Return/ReturnData/IRS990/LandBldgEquipBasisNetGrp/BOYAmt"
	V_990BOYLBpre2013  <- "//Return/ReturnData/IRS990/LandBuildingsEquipmentBasisNet/BOY"
	V_990BOYLB.EZpost2013 <- "//Return/ReturnData/IRS990EZ/LandAndBuildingsGrp/BOYAmt"
	V_990BOYLB.EZpre2013  <- "//Return/ReturnData/IRS990EZ/LandAndBuildings/BOY"
	land.buildings.beg.xpath <- paste( V_990BOYLBpost2013, V_990BOYLBpre2013, V_990BOYLB.EZpost2013, V_990BOYLB.EZpre2013, sep="|" )
	LANDBEGYEAR <- xml_text( xml_find_all( doc, land.buildings.beg.xpath ) ) 
	LANDBEGYEAR <- zeroALL( LANDBEGYEAR )


	## END OF YEAR LAND AND BUILDINGS (AND EQUIPMENT FOR 990-PC ONLY)

	V_990EOYLBpost2013 <- "//Return/ReturnData/IRS990/LandBldgEquipBasisNetGrp/EOYAmt"
	V_990EOYLBpre2013  <- "//Return/ReturnData/IRS990/LandBuildingsEquipmentBasisNet/EOY"
	V_990EOYLB.EZpost2013 <- "//Return/ReturnData/IRS990EZ/LandAndBuildingsGrp/EOYAmt"
	V_990EOYLB.EZpre2013  <- "//Return/ReturnData/IRS990EZ/LandAndBuildings/EOY"
	land.buildings.end.xpath <- paste( V_990EOYLBpost2013, V_990EOYLBpre2013, V_990EOYLB.EZpost2013, V_990EOYLB.EZpre2013, sep="|" )
	LANDENDYEAR <- xml_text( xml_find_all( doc, land.buildings.end.xpath ) ) 
	LANDENDYEAR <- zeroALL( LANDENDYEAR )


	## BEGINNING OF YEAR OTHER ASSETS

	V_990BOYOApost2013 <- "//Return/ReturnData/IRS990/OtherAssetsTotalGrp/BOYAmt"
	V_990BOYOApre2013  <- "//Return/ReturnData/IRS990/OtherAssetsTotal/BOY"
	V_990BOYOA.EZpost2013 <- "//Return/ReturnData/IRS990EZ/OtherAssetsTotalDetail/BOYAmt"
	V_990BOYOA.EZpre2013  <- "//Return/ReturnData/IRS990EZ/OtherAssetsTotal/BOY"
	other.assets.beg.xpath <- paste( V_990BOYOApost2013, V_990BOYOApre2013, V_990BOYOA.EZpost2013, sep="|" )
	OTHERASSETSBEGYEAR <- xml_text( xml_find_all( doc, other.assets.beg.xpath ) ) 
	OTHERASSETSBEGYEAR <- zeroALL( OTHERASSETSBEGYEAR )


	## END OF YEAR OTHER ASSETS 

	V_990EOYOApost2013 <- "//Return/ReturnData/IRS990/OtherAssetsTotalGrp/EOYAmt"
	V_990EOYOApre2013  <- "//Return/ReturnData/IRS990/OtherAssetsTotal/EOY"
	V_990EOYOA.EZpost2013 <- "//Return/ReturnData/IRS990EZ/OtherAssetsTotalDetail/EOYAmt"
	V_990EOYOA.EZpre2013  <- "//Return/ReturnData/IRS990EZ/OtherAssetsTotal/EOY"
	other.assets.end.xpath <- paste( V_990EOYOApost2013, V_990EOYOApre2013, V_990EOYOA.EZpost2013, V_990EOYOA.EZpre2013, sep="|" )
	OTHERASSETSENDYEAR <- xml_text( xml_find_all( doc, other.assets.end.xpath ) ) 
	OTHERASSETSENDYEAR <- zeroALL( OTHERASSETSENDYEAR )


	
	#------------------------------------------------------------------------------------------------------------------------
	####  PART III - STATEMENT OF PROGRAM SERVICE ACCOMPLISHMENTS

	## TOTAL PROGRAM SERVICE EXPENSES

	V_990TPSEpost2013 <- "//Return/ReturnData/IRS990/TotalProgramServiceExpensesAmt"
	V_990TPSEpre2013  <- "//Return/ReturnData/IRS990/TotalProgramServiceExpense"
	V_990TPSE.EZpost2013 <- "//Return/ReturnData/IRS990EZ/TotalProgramServiceExpensesAmt"
	V_990TPSE.EZpre2013  <- "//Return/ReturnData/IRS990EZ/TotalProgramServiceExpenses"
	total.psr.exp.xpath <- paste( V_990TPSEpost2013, V_990TPSEpre2013, V_990TPSE.EZpost2013, V_990TPSE.EZpre2013, sep="|" )
	TOTALPROGSERVEXP <- xml_text( xml_find_all( doc, total.psr.exp.xpath ) ) 
	TOTALPROGSERVEXP <- zeroALL( TOTALPROGSERVEXP )


	
	#------------------------------------------------------------------------------------------------------------------------
	####  PART IV - CHECKLIST OF REQUIRED SCHEDULES

	## LOBBYING ACTIVITIES

	V_990Lpost2013 <- "//Return/ReturnData/IRS990/LobbyingActivitiesInd"
	V_990Lpre2013  <- "//Return/ReturnData/IRS990/LobbyingActivities"
	V_990L.EZpost2013 <- "//Return/ReturnData/IRS990EZ/LobbyingActivitiesInd"
	V_990L.EZpre2013  <- "//Return/ReturnData/IRS990EZ/EngageInLobbyingActivities"
	lobbying.xpath <- paste( V_990Lpost2013, V_990Lpre2013, V_990L.EZpost2013, V_990L.EZpre2013, sep="|" )
	LOBBYING <- xml_text( xml_find_all( doc, lobbying.xpath ) )
	
	
	
	## FOREIGN REVENUE OR EXPENSES OR $10K, OR FOREIGN INVESTMENTS OVER $100K
	
	V_990FRpost2013 <- "//Return/ReturnData/IRS990/ForeignActivitiesInd"
	V_990FRpre2013  <- "//Return/ReturnData/IRS990/ForeignActivities"
	foreign.rev.xpath <- paste( V_990FRpost2013, V_990FRpre2013, sep="|" )
	FOREIGNREV <- xml_text( xml_find_all( doc, foreign.rev.xpath ) )
	


	#------------------------------------------------------------------------------------------------------------------------
	####  PART VI - GOVERNMENT, MANAGEMENT, AND DISCLOSURE
	####  PC ONLY

	## PART VI RESPONSE IN SCHEDULE O
	## The 2013 update added an "Ind" on the tail of these xpaths, which looks 
	## like an additional roman numeral, and can be visually confusing.
	## For example, 2012: Info...PartIX   Same path in 2013: Info...PartIXInd

	V_990VMGpost2013 <- "//Return/ReturnData/IRS990/InfoInScheduleOPartVIInd"
	V_990VMGpre2013  <- "//Return/ReturnData/IRS990/InfoInScheduleOPartVI"
	schedO.partvi.xpath <- paste( V_990VMGpost2013, V_990VMGpre2013, sep="|" )
	SCHEDOPARTVI <- xml_text( xml_find_all( doc, schedO.partvi.xpath ) )



	#### SECTION A. GOVERNING BODY AND MANAGEMENT

	## NUMBER OF VOTING MEMBERS IN GOVERNING BODY
	## Should be equal to VOTINGMEMBERS

	V_990VMGpost2013 <- "//Return/ReturnData/IRS990/GoverningBodyVotingMembersCnt"
	V_990VMGpre2013  <- "//Return/ReturnData/IRS990/NbrVotingGoverningBodyMembers"
	voting.mem.governing.xpath <- paste( V_990VMGpost2013, V_990VMGpre2013, sep="|" )
	VMGOVERNING <- xml_text( xml_find_all( doc, voting.mem.governing.xpath ) )



	## NUMBER OF INDEPENDENT VOTING MEMBERS IN GOVERNING BODY
	## Should be equal to INDVOTINGMEMBERS

	V_990IVMGpost2013 <- "//Return/ReturnData/IRS990/IndependentVotingMemberCnt"
	V_990IVMGpre2013  <- "//Return/ReturnData/IRS990/NumberIndependentVotingMembers"
	voting.indep.governing.xpath <- paste( V_990IVMGpost2013, V_990IVMGpre2013, sep="|" )
	IVMGOVERNING <- xml_text( xml_find_all( doc, voting.indep.governing.xpath ) )



	## DID ANY OFFICERS ETC. HAVE RELATIONSHIPS WITH OTHER OFFICERS ETC.

	V_990RWOpost2013 <- "//Return/ReturnData/IRS990/FamilyOrBusinessRlnInd"
	V_990RWOpre2013  <- "//Return/ReturnData/IRS990/FamilyOrBusinessRelationship"
	rel.with.officers.xpath <- paste( V_990RWOpost2013, V_990RWOpre2013, sep="|" )
	OFFICERREL <- xml_text( xml_find_all( doc, rel.with.officers.xpath ) )



	## WERE MANAGEMENT DUTIES DELEGATED TO A MANAGEMENT COMPANY OR OTHER PERSON

	V_990MDpost2013 <- "//Return/ReturnData/IRS990/DelegationOfMgmtDutiesInd"
	V_990MDpre2013  <- "//Return/ReturnData/IRS990/DelegationOfManagementDuties"
	mgmt.delegated.xpath <- paste( V_990MDpost2013, V_990MDpost2013, sep="|" )
	MGMTDEL <- xml_text( xml_find_all( doc, mgmt.delegated.xpath ) )



	## ANY CHANGES IN GOVERNANCE DOCUMENTS SINCE PRIOR 990 WAS FILED

	V_990CGDpost2013 <- "//Return/ReturnData/IRS990/ChangeToOrgDocumentsInd"
	V_990CGDpre2013  <- "//Return/ReturnData/IRS990/ChangesToOrganizingDocs"
	changes.gov.docs.xpath <- paste( V_990CGDpost2013, V_990CGDpre2013, sep="|" )
	CHANGESGOVDOCS <- xml_text( xml_find_all( doc, changes.gov.docs.xpath ) )  



	## ANY SIGNIFICANT DIVERSION OF ORGANIZATION ASSETS

	V_990DOApost2013 <- "//Return/ReturnData/IRS990/MaterialDiversionOrMisuseInd"
	V_990DOApre2013  <- "//Return/ReturnData/IRS990/MaterialDiversionOrMisuse"
	diversion.org.assets.xpath <- paste( V_990DOApost2013, V_990DOApre2013, sep="|" )
	DIVASSETS <- xml_text( xml_find_all( doc, diversion.org.assets.xpath ) )  



	## DID ORGANIZATION HAVE MEMBERS OR STOCKHOLDERS

	V_990MSpost2013 <- "//Return/ReturnData/IRS990/MembersOrStockholdersInd"
	V_990MSpre2013  <- "//Return/ReturnData/IRS990/MembersOrStockholders"
	org.members.stakeholders.xpath <- paste( V_990MSpost2013, V_990MSpre2013, sep="|" )
	STOCKMEMBER <- xml_text( xml_find_all( doc, org.members.stakeholders.xpath ) )   



	## DID ORGANIZATION HAVE MEMBERS ETC. WHO COULD CHOOSE GOVERNING BODY MEMBERS

	V_990MCGBpost2013 <- "//Return/ReturnData/IRS990/ElectionOfBoardMembersInd"
	V_990MCGBpre2013  <- "//Return/ReturnData/IRS990/ElectionOfBoardMembers"
	members.choose.govbody.xpath <- paste( V_990MCGBpost2013, V_990MCGBpre2013, sep="|" )
	MEMBERCHOOSE <- xml_text( xml_find_all( doc, members.choose.govbody.xpath ) )    



	## ARE ANY GOVERNANCE DECISIONS RESTRICTED TO PEOPLE OUTSIDE GOVERNING BODY

	V_990GBRDpost2013 <- "//Return/ReturnData/IRS990/DecisionsSubjectToApprovaInd"
	V_990GBRDpre2013  <- "//Return/ReturnData/IRS990/DecisionsSubjectToApproval"
	restricted.decisions.xpath <- paste( V_990GBRDpost2013, V_990GBRDpre2013, sep="|" )
	GOVBODYDECISION <- xml_text( xml_find_all( doc, restricted.decisions.xpath ) )    



	## WERE GOVERNANCE MEETINGS DOCUMENTED

	V_990GMDpost2013 <- "//Return/ReturnData/IRS990/MinutesOfGoverningBodyInd"
	V_990GMDpre2013  <- "//Return/ReturnData/IRS990/MinutesOfGoverningBody"
	gov.mtng.documented.xpath <- paste( V_990GMDpost2013, V_990GMDpre2013, sep="|" )
	GOVBODYDOCU <- xml_text( xml_find_all( doc, gov.mtng.documented.xpath ) )  



	## WERE COMMITEE MEETINGS DOCUMENTED

	V_990CMDpost2013 <- "//Return/ReturnData/IRS990/MinutesOfCommitteesInd"
	V_990CMDpre2013  <- "//Return/ReturnData/IRS990/MinutesOfCommittees"
	committee.mtng.documented.xpath <- paste( V_990CMDpost2013, V_990CMDpre2013, sep="|" )
	COMMITTEEDOCU <- xml_text( xml_find_all( doc, committee.mtng.documented.xpath ) )  



	## ANY OFFICERS NOT REACHABLE AT ORGANIZATION'S MAILING ADDRESS

	V_990NRMApost2013 <- "//Return/ReturnData/IRS990/OfficerMailingAddressInd"
	V_990NRMApre2013  <- "//Return/ReturnData/IRS990/OfficerMailingAddress"
	not.reachable.mailing.xpath <- paste( V_990NRMApost2013, V_990NRMApre2013, sep="|" )
	NOTREACH <- xml_text( xml_find_all( doc, not.reachable.mailing.xpath ) )   



	## CONFLICT OF INTEREST POLICY

	V_990COIPpost2013 <- "//Return/ReturnData/IRS990/ConflictOfInterestPolicyInd"
	V_990COIPpre2013  <- "//Return/ReturnData/IRS990/ConflictOfInterestPolicy"
	coi.policy.xpath <- paste( V_990COIPpost2013, V_990COIPpre2013, sep="|" )
	COIPOLICY <- xml_text( xml_find_all( doc, coi.policy.xpath ) )     



	## CONFLICT OF INTEREST ANNUAL DISCLOSURE REQUIREMENT FOR OFFICERS ETC.

	V_990COIDpost2013 <- "//Return/ReturnData/IRS990/AnnualDisclosureCoveredPrsnInd"
	V_990COIDpre2013  <- "//Return/ReturnData/IRS990/AnnualDisclosureCoveredPersons"
	coi.disclosure.xpath <- paste( V_990COIDpost2013, V_990COIDpre2013, sep="|" )
	COIDISCLOSE <- xml_text( xml_find_all( doc, coi.disclosure.xpath ) )      



	## CONFLICT OF INTEREST MONITORING BY THE ORGANIZATION

	V_990COIMpost2013 <- "//Return/ReturnData/IRS990/RegularMonitoringEnfrcInd"
	V_990COIMpre2013  <- "//Return/ReturnData/IRS990/RegularMonitoringEnforcement"
	coi.monitor.xpath <- paste( V_990COIMpost2013, V_990COIMpre2013, sep="|" )
	COIMONITOR <- xml_text( xml_find_all( doc, coi.monitor.xpath ) )    



	## WHISTLEBLOWER POLICY

	V_990WBPpost2013 <- "//Return/ReturnData/IRS990/WhistleblowerPolicyInd"
	V_990WBPpre2013  <- "//Return/ReturnData/IRS990/WhistleblowerPolicy"
	wb.policy.xpath <- paste( V_990WBPpost2013, V_990WBPpre2013, sep="|" )
	WBPOLICY <- xml_text( xml_find_all( doc, wb.policy.xpath ) )   



	## STATES THIS 990 MUST BE FILED WITH

	V_990SMFWpost2013 <- "//Return/ReturnData/IRS990/StatesWhereCopyOfReturnIsFldCd"
	V_990SMFWpre2013  <- "//Return/ReturnData/IRS990/StatesWhereCopyOfReturnIsFiled"
	V_990SMFW.EZpost2013 <- "//Return/ReturnData/IRS990EZ/StatesWhereCopyOfReturnIsFldCd"
	V_990SMFW.EZpre2013  <- "//Return/ReturnData/IRS990EZ/StatesWhereCopyOfReturnIsFiled"
	states.must.file.xpath <- paste( V_990SMFWpost2013, V_990SMFWpre2013, V_990SMFW.EZpost2013, V_990SMFW.EZpre2013, sep="|" )
	FILINGSTATES <- xml_text( xml_find_all( doc, states.must.file.xpath ) )     
        FILINGSTATES <- paste( FILINGSTATES, collapse=" " )


	## PUBLIC AVAILABILITY: represent the 4 possible values, broken out then collapsed

	## PUBLICLY AVAILABLE THROUGH OWN WEBSITE

	V_990PAOWpost2013 <- "//Return/ReturnData/IRS990/OwnWebsiteInd"
	V_990PAOWpre2013  <- "//Return/ReturnData/IRS990/OwnWebsite"
	public.web.self.xpath <- paste( V_990PAOWpost2013, V_990PAOWpre2013, sep="|" )
	PUBLICWEBSELF <- xml_text( xml_find_all( doc, public.web.self.xpath ) )

	PUBLICWEBSELF[ length( PUBLICWEBSELF ) == 0] <- NA
	if( is.na( PUBLICWEBSELF ) == FALSE ) { PUBLICWEBSELF <- "Own Website" }



	## PUBLICLY AVAILABLE THROUGH ANOTHER'S WEBSITE

	V_990PAAWpost2013 <- "//Return/ReturnData/IRS990/OtherWebsiteInd"
	V_990PAAWpre2013  <- "//Return/ReturnData/IRS990/OtherWebsite"
	public.web.other.xpath <- paste( V_990PAAWpost2013, V_990PAAWpre2013, sep="|" )
	PUBLICWEBOTHER <- xml_text( xml_find_all( doc, public.web.other.xpath ) ) 

	PUBLICWEBOTHER[ length( PUBLICWEBOTHER ) == 0] <- NA
	if( is.na( PUBLICWEBOTHER ) == FALSE ) { PUBLICWEBOTHER <- "Another's Website" }



	## PUBLICLY AVAILABLE UPON REQUEST

	V_990PAURpost2013 <- "//Return/ReturnData/IRS990/UponRequestInd"
	V_990PAURpre2013  <- "//Return/ReturnData/IRS990/UponRequest"
	public.request.xpath <- paste( V_990PAURpost2013, V_990PAURpre2013, sep="|" )
	PUBLICREQUEST <- xml_text( xml_find_all( doc, public.request.xpath ) ) 

	PUBLICREQUEST[ length( PUBLICREQUEST ) == 0] <- NA
	if( is.na( PUBLICREQUEST ) == FALSE ) { PUBLICREQUEST <- "Upon Request" }



	## PUBLICLY AVAILABLE THROUGH ANOTHER METHOD

	V_990PAOMpost2013 <- "//Return/ReturnData/IRS990/OtherInd"
	V_990PAOMpre2013  <- "//Return/ReturnData/IRS990/OtherExplainInSchO"
	public.other.xpath <- paste( V_990PAOMpost2013, V_990PAOMpre2013, sep="|" )
	PUBLICOTHER <- xml_text( xml_find_all( doc, public.other.xpath ) ) 

	PUBLICOTHER[ length( PUBLICOTHER ) == 0] <- NA
	if( is.na( PUBLICOTHER ) == FALSE ) { PUBLICOTHER <- "Other-In Schedule O" }



	## PUBLICLY AVAILABLE METHOD (Collapsed)
	## The output should be comma-delimited to represent the non-exclusive nature of the checkboxes

	PUBLICSHARE <- gsub( "NA", "", paste( PUBLICWEBSELF, PUBLICWEBOTHER, PUBLICREQUEST, PUBLICOTHER, sep="," ) )
	PUBLICSHARE[ PUBLICSHARE  == ",,," ] <- NA


	## PERSON WITH ORGANIZATION'S BOOKS: NAME

	V_990OBMpost2013 <- "//Return/ReturnData/IRS990/BooksInCareOfDetail/PersonNm"
	V_990OBMWpre2013  <- "//Return/ReturnData/IRS990/TheBooksAreInCareOf/NamePerson"
	org.books.name.xpath <- paste( V_990OBMpost2013, V_990OBMWpre2013, sep="|" )
	ORGBOOKNAME <- xml_text( xml_find_all( doc, org.books.name.xpath ) )    



	## PERSON WITH ORGANIZATION'S BOOKS: PHONE NUMBER

	V_990OBPpost2013 <- "//Return/ReturnData/IRS990/BooksInCareOfDetail/PhoneNum"
	V_990OBPpre2013  <- "//Return/ReturnData/IRS990/TheBooksAreInCareOf/TelephoneNumber"
	org.books.phone.xpath <- paste( V_990OBPpost2013, V_990OBPpre2013, sep="|" )
	ORGBOOKPHONE <- xml_text( xml_find_all( doc, org.books.phone.xpath ) )    



	## PERSON WITH ORGANIZATION'S BOOKS: ADDRESS

	V_990OBApost2013 <- "//Return/ReturnData/IRS990/BooksInCareOfDetail/USAddress/AddressLine1Txt"
	V_990OBApre2013  <- "//Return/ReturnData/IRS990/TheBooksAreInCareOf/AddressUS/AddressLine1"
	org.books.address.xpath <- paste( V_990OBApost2013, V_990OBApre2013, sep="|" )
	ORGBOOKADDRESS <- xml_text( xml_find_all( doc, org.books.address.xpath ) )    



	## PERSON WITH ORGANIZATION'S BOOKS: CITY

	V_990OBCpost2013 <- "//Return/ReturnData/IRS990/BooksInCareOfDetail/USAddress/CityNm"
	V_990OBCpre2013  <- "//Return/ReturnData/IRS990/TheBooksAreInCareOf/AddressUS/City"
	org.books.city.xpath <- paste( V_990OBCpost2013, V_990OBCpre2013, sep="|" )
	ORGBOOKCITY <- xml_text( xml_find_all( doc, org.books.city.xpath ) )    



	## PERSON WITH ORGANIZATION'S BOOKS: STATE

	V_990OBSpost2013 <- "//Return/ReturnData/IRS990/BooksInCareOfDetail/USAddress/StateAbbreviationCd"
	V_990OBSpre2013  <- "//Return/ReturnData/IRS990/TheBooksAreInCareOf/AddressUS/State"
	org.books.state.xpath <- paste( V_990OBSpost2013, V_990OBSpre2013, sep="|" )
	ORGBOOKSTATE <- xml_text( xml_find_all( doc, org.books.state.xpath ) )    



	## PERSON WITH ORGANIZATION'S BOOKS: ZIP

	V_990OBZpost2013 <- "//Return/ReturnData/IRS990/BooksInCareOfDetail/USAddress/ZIPCd"
	V_990OBZpre2013  <- "//Return/ReturnData/IRS990/TheBooksAreInCareOf/AddressUS/ZIPCode"
	org.books.zip.xpath <- paste( V_990OBZpost2013, V_990OBZpre2013, sep="|" )
	ORGBOOKZIP <- xml_text( xml_find_all( doc, org.books.zip.xpath ) )



	#------------------------------------------------------------------------------------------------------------------------
	#####  PART X - ASSETS
	##   PC-specific Values

	## BEGINNING OF YEAR PLEDGES AND GRANTS

	V_990BOYPGpost2013 <- "//Return/ReturnData/IRS990/PledgesAndGrantsReceivableGrp/BOYAmt"
	V_990BOYPGpre2013  <- "//Return/ReturnData/IRS990/PledgesAndGrantsReceivable/BOY"
	pledges.grants.beg.xpath <- paste( V_990BOYPGpost2013, V_990BOYPGpre2013, sep="|" )
	PLEDGEGRANTBEGYEAR <- xml_text( xml_find_all( doc, pledges.grants.beg.xpath ) ) 
	PLEDGEGRANTBEGYEAR <- zeroPC( PLEDGEGRANTBEGYEAR )


	## END OF YEAR PLEDGES AND GRANTS

	V_990EOYPGpost2013 <- "//Return/ReturnData/IRS990/PledgesAndGrantsReceivableGrp/EOYAmt"
	V_990EOYPGpre2013  <- "//Return/ReturnData/IRS990/PledgesAndGrantsReceivable/EOY"
	pledges.grants.end.xpath <- paste( V_990EOYPGpost2013, V_990EOYPGpre2013, sep="|" )
	PLEDGEGRANTENDYEAR <- xml_text( xml_find_all( doc, pledges.grants.end.xpath ) ) 
	PLEDGEGRANTENDYEAR <- zeroPC( PLEDGEGRANTENDYEAR )


	## BEGINNING OF YEAR ACCOUNTS RECEIVABLE

	V_990BOYAROpost2013 <- "//Return/ReturnData/IRS990/AccountsReceivableGrp/BOYAmt"
	V_990BOYAROpre2013  <- "//Return/ReturnData/IRS990/AccountsReceivable/BOY"
	accts.receivable.beg.xpath <- paste( V_990BOYAROpost2013, V_990BOYAROpre2013, sep="|" )
	ACCTRECBEGYEAR <- xml_text( xml_find_all( doc, accts.receivable.beg.xpath ) ) 
	ACCTRECBEGYEAR <- zeroPC( ACCTRECBEGYEAR )


	## END OF YEAR ACCOUNTS RECEIVABLE

	V_990EOYAROpost2013 <- "//Return/ReturnData/IRS990/AccountsReceivableGrp/EOYAmt"
	V_990EOYAROpre2013  <- "//Return/ReturnData/IRS990/AccountsReceivable/EOY"
	accts.receivable.end.xpath <- paste( V_990EOYAROpost2013, V_990EOYAROpre2013, sep="|" )
	ACCTRECENDYEAR <- xml_text( xml_find_all( doc, accts.receivable.end.xpath ) ) 
	ACCTRECENDYEAR <- zeroPC( ACCTRECENDYEAR )


	## BEGINNING OF YEAR LOANS FROM OFFICERS
	## Confirm that this is the appropriate xpath - switch with Xpath in Loans to Officers?

	V_990BOYLFOpost2013 <- "//Return/ReturnData/IRS990/ReceivablesFromOfficersEtcGrp/BOYAmt"
	V_990BOYLFOpre2013  <- "//Return/ReturnData/IRS990/ReceivablesFromOfficersEtc/BOY"
	loans.from.officers.beg.xpath <- paste( V_990BOYLFOpost2013, V_990BOYLFOpre2013, sep="|" )
	LOANSFROMOFFBEGYEAR <- xml_text( xml_find_all( doc, loans.from.officers.beg.xpath ) ) 
	LOANSFROMOFFBEGYEAR <- zeroPC( LOANSFROMOFFBEGYEAR )


	## END OF YEAR LOANS FROM OFFICERS
	## Confirm that this is the appropriate xpath - switch with Xpath in Loans to Officers?

	V_990EOYLFOpost2013 <- "//Return/ReturnData/IRS990/ReceivablesFromOfficersEtcGrp/EOYAmt"
	V_990EOYLFOpre2013  <- "//Return/ReturnData/IRS990/ReceivablesFromOfficersEtc/EOY"
	loans.from.officers.end.xpath <- paste( V_990EOYLFOpost2013, V_990EOYLFOpre2013, sep="|" )
	LOANSFROMOFFENDYEAR <- xml_text( xml_find_all( doc, loans.from.officers.end.xpath ) ) 
	LOANSFROMOFFENDYEAR <- zeroPC( LOANSFROMOFFENDYEAR )


	## BEGINNING OF YEAR LOANS FROM DISQUALIFIED PERSONS

	V_990BOYLDQpost2013 <- "//Return/ReturnData/IRS990/RcvblFromDisqualifiedPrsnGrp/BOYAmt"
	V_990BOYLDQpre2013  <- "//Return/ReturnData/IRS990/ReceivablesFromDisqualPersons/BOY"
	loans.disqual.persons.beg.xpath <- paste( V_990BOYLDQpost2013, V_990BOYLDQpre2013, sep="|" )
	LOANSDQPBEGYEAR <- xml_text( xml_find_all( doc, loans.disqual.persons.beg.xpath ) ) 
	LOANSDQPBEGYEAR <- zeroPC( LOANSDQPBEGYEAR )


	## END OF YEAR LOANS FROM DISQUALIFIED PERSONS

	V_990EOYLDQpost2013 <- "//Return/ReturnData/IRS990/RcvblFromDisqualifiedPrsnGrp/EOYAmt"
	V_990EOYLDQpre2013  <- "//Return/ReturnData/IRS990/ReceivablesFromDisqualPersons/EOY"
	loans.disqual.persons.end.xpath <- paste( V_990EOYLDQpost2013, V_990EOYLDQpre2013, sep="|" )
	LOANSDQPENDYEAR <- xml_text( xml_find_all( doc, loans.disqual.persons.end.xpath ) ) 
	LOANSDQPENDYEAR <- zeroPC( LOANSDQPENDYEAR )


	## BEGINNING OF YEAR NET NOTES AND LOANS RECEIVABLE

	V_990BOYNNLpost2013 <- "//Return/ReturnData/IRS990/OthNotesLoansReceivableNetGrp/BOYAmt"
	V_990BOYNNLpre2013  <- "//Return/ReturnData/IRS990/OtherNotesLoansReceivableNet/BOY"
	net.notes.beg.xpath <- paste( V_990BOYNNLpost2013, V_990BOYNNLpre2013, sep="|" )
	LOANSNOTESBEGYEAR <- xml_text( xml_find_all( doc, net.notes.beg.xpath ) ) 
	LOANSNOTESBEGYEAR <- zeroPC( LOANSNOTESBEGYEAR )


	## END OF YEAR NET NOTES AND LOANS RECEIVABLE

	V_990EOYNNLpost2013 <- "//Return/ReturnData/IRS990/OthNotesLoansReceivableNetGrp/EOYAmt"
	V_990EOYNNLpre2013  <- "//Return/ReturnData/IRS990/OtherNotesLoansReceivableNet/EOY"
	net.notes.end.xpath <- paste( V_990EOYNNLpost2013, V_990EOYNNLpre2013, sep="|" )
	LOANSNOTESENDYEAR <- xml_text( xml_find_all( doc, net.notes.end.xpath ) ) 
	LOANSNOTESENDYEAR <- zeroPC( LOANSNOTESENDYEAR )


	## BEGINNING OF YEAR INVENTORIES FOR SALE OR USE

	V_990BOYISUpost2013 <- "//Return/ReturnData/IRS990/InventoriesForSaleOrUseGrp/BOYAmt"
	V_990BOYISUpre2013  <- "//Return/ReturnData/IRS990/InventoriesForSaleOrUse/BOY"
	inventory.beg.xpath <- paste( V_990BOYISUpost2013, V_990BOYISUpre2013, sep="|" )
	INVENTORYBEGYEAR <- xml_text( xml_find_all( doc, inventory.beg.xpath ) ) 
	INVENTORYBEGYEAR <- zeroPC( INVENTORYBEGYEAR )


	## END OF YEAR INVENTORIES FOR SALE OR USE

	V_990EOYISUpost2013 <- "//Return/ReturnData/IRS990/InventoriesForSaleOrUseGrp/EOYAmt"
	V_990EOYISUpre2013  <- "//Return/ReturnData/IRS990/InventoriesForSaleOrUse/EOY"
	inventory.end.xpath <- paste( V_990EOYISUpost2013, V_990EOYISUpre2013, sep="|" )
	INVENTORYENDYEAR <- xml_text( xml_find_all( doc, inventory.end.xpath ) ) 
	INVENTORYENDYEAR <- zeroPC( INVENTORYENDYEAR )


	## BEGINNING OF YEAR PREPAID EXPENSES

	V_990BOYPPEpost2013 <- "//Return/ReturnData/IRS990/PrepaidExpensesDefrdChargesGrp/BOYAmt"
	V_990BOYPPEpre2013  <- "//Return/ReturnData/IRS990/PrepaidExpensesDeferredCharges/BOY"
	prepaid.expenses.beg.xpath <- paste( V_990BOYPPEpost2013, V_990BOYPPEpre2013, sep="|" )
	PREEXPBEGYEAR <- xml_text( xml_find_all( doc, prepaid.expenses.beg.xpath ) ) 
	PREEXPBEGYEAR <- zeroPC( PREEXPBEGYEAR )


	## END OF YEAR PREPAID EXPENSES

	V_990EOYPPEpost2013 <- "//Return/ReturnData/IRS990/PrepaidExpensesDefrdChargesGrp/EOYAmt"
	V_990EOYPPEpre2013  <- "//Return/ReturnData/IRS990/PrepaidExpensesDeferredCharges/EOY"
	prepaid.expenses.end.xpath <- paste( V_990EOYPPEpost2013, V_990EOYPPEpre2013, sep="|" )
	PREEXPENDYEAR <- xml_text( xml_find_all( doc, prepaid.expenses.end.xpath ) ) 
	PREEXPENDYEAR <- zeroPC( PREEXPENDYEAR )


	## BEGINNING OF YEAR PUBLICLY-TRADED SECURITIES

	V_990BOYPTSpost2013 <- "//Return/ReturnData/IRS990/InvestmentsPubTradedSecGrp/BOYAmt"
	V_990BOYPTSpre2013  <- "//Return/ReturnData/IRS990/InvestmentsPubTradedSecurities/BOY"
	investments.public.beg.xpath <- paste( V_990BOYPTSpost2013, V_990BOYPTSpre2013, sep="|" )
	INVESTPUBBEGYEAR <- xml_text( xml_find_all( doc, investments.public.beg.xpath ) ) 
	INVESTPUBBEGYEAR <- zeroPC( INVESTPUBBEGYEAR )


	## END OF YEAR PUBLICLY-TRADED SECURITIES INVESTMENTS

	V_990EOYPTSpost2013 <- "//Return/ReturnData/IRS990/InvestmentsPubTradedSecGrp/EOYAmt"
	V_990EOYPTSpre2013  <- "//Return/ReturnData/IRS990/InvestmentsPubTradedSecurities/EOY"
	investments.public.end.xpath <- paste( V_990EOYPTSpost2013, V_990EOYPTSpre2013, sep="|" )
	INVESTPUBENDYEAR <- xml_text( xml_find_all( doc, investments.public.end.xpath ) ) 
	INVESTPUBENDYEAR <- zeroPC( INVESTPUBENDYEAR )


	## BEGINNING OF YEAR OTHER SECURITIES INVESTMENTS

	V_990BOYOSIpost2013 <- "//Return/ReturnData/IRS990/InvestmentsOtherSecuritiesGrp/BOYAmt"
	V_990BOYOSIpre2013  <- "//Return/ReturnData/IRS990/InvestmentsOtherSecurities/BOY"
	investments.other.beg.xpath <- paste( V_990BOYOSIpost2013, V_990BOYOSIpre2013, sep="|" )
	INVESTOTHBEGYEAR <- xml_text( xml_find_all( doc, investments.other.beg.xpath ) ) 
	INVESTOTHBEGYEAR <- zeroPC( INVESTOTHBEGYEAR )


	## END OF YEAR OTHER SECURITIES INVESTMENTS

	V_990EOYOSIpost2013 <- "//Return/ReturnData/IRS990/InvestmentsOtherSecuritiesGrp/EOYAmt"
	V_990EOYOSIpre2013  <- "//Return/ReturnData/IRS990/InvestmentsOtherSecurities/EOY"
	investments.other.end.xpath <- paste( V_990EOYOSIpost2013, V_990EOYOSIpre2013, sep="|" )
	INVESTOTHENDYEAR <- xml_text( xml_find_all( doc, investments.other.end.xpath ) ) 
	INVESTOTHENDYEAR <- zeroPC( INVESTOTHENDYEAR )


	## BEGINNING OF YEAR PROGRAM-RELATED INVESTMENTS

	V_990BOYPRIpost2013 <- "//Return/ReturnData/IRS990/InvestmentsProgramRelatedGrp/BOYAmt"
	V_990BOYPRIpre2013  <- "//Return/ReturnData/IRS990/InvestmentsProgramRelated/BOY"
	investments.prog.beg.xpath <- paste( V_990BOYPRIpost2013, V_990BOYPRIpre2013, sep="|" )
	INVESTPRGBEGYEAR <- xml_text( xml_find_all( doc, investments.prog.beg.xpath ) ) 
	INVESTPRGBEGYEAR <- zeroPC( INVESTPRGBEGYEAR )


	## END OF YEAR PROGRAM-RELATED INVESTMENTS

	V_990EOYPRIpost2013 <- "//Return/ReturnData/IRS990/InvestmentsOtherSecuritiesGrp/EOYAmt"
	V_990EOYPRIpre2013  <- "//Return/ReturnData/IRS990/InvestmentsOtherSecurities/EOY"
	investments.prog.end.xpath <- paste( V_990EOYPRIpost2013, V_990EOYPRIpre2013, sep="|" )
	INVESTPRGENDYEAR <- xml_text( xml_find_all( doc, investments.prog.end.xpath ) ) 
	INVESTPRGENDYEAR <- zeroPC( INVESTPRGENDYEAR )


	## BEGINNING OF YEAR INTANGIBLE ASSETS

	V_990BOYIApost2013 <- "//Return/ReturnData/IRS990/IntangibleAssetsGrp/BOYAmt"
	V_990BOYIApre2013  <- "//Return/ReturnData/IRS990/IntangibleAssets/BOY"
	intangible.assets.beg.xpath <- paste( V_990BOYIApost2013, V_990BOYIApre2013, sep="|" )
	INTANASSETSBEGYEAR <- xml_text( xml_find_all( doc, intangible.assets.beg.xpath ) ) 
	INTANASSETSBEGYEAR <- zeroPC( INTANASSETSBEGYEAR )


	## END OF YEAR INTANGIBLE ASSETS

	V_990EOYIApost2013 <- "//Return/ReturnData/IRS990/IntangibleAssetsGrp/EOYAmt"
	V_990EOYIApre2013  <- "//Return/ReturnData/IRS990/IntangibleAssets/EOY"
	intangible.assets.end.xpath <- paste( V_990EOYIApost2013, V_990EOYIApre2013, sep="|" )
	INTANASSETSENDYEAR <- xml_text( xml_find_all( doc, intangible.assets.end.xpath ) ) 
	INTANASSETSENDYEAR <- zeroPC( INTANASSETSENDYEAR )


	## BEGINNING OF YEAR TOTAL ASSETS FROM BALANCE SHEET
	## Should equal TOTALASSETSBEGYEAR

	V_990BOYBSTApost2013 <- "//Return/ReturnData/IRS990/TotalAssetsGrp/BOYAmt"
	V_990BOYBSTApre2013  <- "//Return/ReturnData/IRS990/TotalAssets/BOY"
	totalassets.balsheet.beg.xpath <- paste( V_990BOYBSTApost2013, V_990BOYBSTApre2013, sep="|" )
	TABALSHEETBEGYEAR <- xml_text( xml_find_all( doc, totalassets.balsheet.beg.xpath ) ) 
	TABALSHEETBEGYEAR <- zeroPC( TABALSHEETBEGYEAR )


	## END OF YEAR TOTAL ASSETS FROM BALANCE SHEET
	## Should equal TOTALASSETSENDYEAR

	V_990EOYBSTApost2013 <- "//Return/ReturnData/IRS990/TotalAssetsGrp/EOYAmt"
	V_990EOYBSTApre2013  <- "//Return/ReturnData/IRS990/TotalAssets/EOY"
	totalassets.balsheet.end.xpath <- paste( V_990EOYBSTApost2013, V_990EOYBSTApre2013, sep="|" )
	TABALSHEETENDYEAR <- xml_text( xml_find_all( doc, totalassets.balsheet.end.xpath ) ) 
	TABALSHEETENDYEAR <- zeroPC( TABALSHEETENDYEAR )

	

	#------------------------------------------------------------------------------------------------------------------------
	#####  PART X - LIABILITIES
	#####   PC-specific Values

	## BEGINNING OF YEAR ACCOUNTS PAYABLE AND ACCRUED EXPENSES

	V_990BOYAPpost2013 <- "//Return/ReturnData/IRS990/AccountsPayableAccrExpnssGrp/BOYAmt"
	V_990BOYAPpre2013  <- "//Return/ReturnData/IRS990/AccountsPayableAccruedExpenses/BOY"
	accts.payable.beg.xpath <- paste( V_990BOYAPpost2013, V_990BOYAPpre2013, sep="|" )
	ACCTPAYBEGYEAR <- xml_text( xml_find_all( doc, accts.payable.beg.xpath ) ) 
	ACCTPAYBEGYEAR <- zeroPC( ACCTPAYBEGYEAR )


	## END OF YEAR ACCOUNTS PAYABLE AND ACCRUED EXPENSES

	V_990EOYAPpost2013 <- "//Return/ReturnData/IRS990/AccountsPayableAccrExpnssGrp/EOYAmt"
	V_990EOYAPpre2013  <- "//Return/ReturnData/IRS990/AccountsPayableAccruedExpenses/EOY"
	accts.payable.end.xpath <- paste( V_990EOYAPpost2013, V_990EOYAPpre2013, sep="|" )
	ACCTPAYENDYEAR <- xml_text( xml_find_all( doc, accts.payable.end.xpath ) )
	ACCTPAYENDYEAR <- zeroPC( ACCTPAYENDYEAR )


	## BEGINNING OF YEAR GRANTS PAYABLE

	V_990BOYGPpost2013 <- "//Return/ReturnData/IRS990/GrantsPayableGrp/BOYAmt"
	V_990BOYGPpre2013  <- "//Return/ReturnData/IRS990/GrantsPayable/BOY"
	grants.payable.beg.xpath <- paste( V_990BOYGPpost2013, V_990BOYGPpre2013, sep="|" )
	GRANTSPAYBEGYEAR <- xml_text( xml_find_all( doc, grants.payable.beg.xpath ) ) 
	GRANTSPAYBEGYEAR <- zeroPC( GRANTSPAYBEGYEAR )


	## END OF YEAR GRANTS PAYABLE

	V_990EOYGPpost2013 <- "//Return/ReturnData/IRS990/GrantsPayableGrp/EOYAmt"
	V_990EOYGPpre2013  <- "//Return/ReturnData/IRS990/GrantsPayable/EOY"
	grants.payable.end.xpath <- paste( V_990EOYGPpost2013, V_990EOYGPpre2013, sep="|" )
	GRANTSPAYENDYEAR <- xml_text( xml_find_all( doc, grants.payable.end.xpath ) )
	GRANTSPAYENDYEAR <- zeroPC( GRANTSPAYENDYEAR )


	## BEGINNING OF YEAR DEFERRED REVENUE

	V_990BOYDRpost2013 <- "//Return/ReturnData/IRS990/DeferredRevenueGrp/BOYAmt"
	V_990BOYDRpre2013  <- "//Return/ReturnData/IRS990/DeferredRevenue/BOY"
	deferred.rev.beg.xpath <- paste( V_990BOYDRpost2013, V_990BOYDRpre2013, sep="|" )
	DEFREVBEGYEAR <- xml_text( xml_find_all( doc, deferred.rev.beg.xpath ) ) 
	DEFREVBEGYEAR <- zeroPC( DEFREVBEGYEAR )


	## END OF YEAR DEFERRED REVENUE

	V_990EOYDRpost2013 <- "//Return/ReturnData/IRS990/DeferredRevenueGrp/EOYAmt"
	V_990EOYDRpre2013  <- "//Return/ReturnData/IRS990/DeferredRevenue/EOY"
	deferred.rev.end.xpath <- paste( V_990EOYDRpost2013, V_990EOYDRpre2013, sep="|" )
	DEFREVENDYEAR <- xml_text( xml_find_all( doc, deferred.rev.end.xpath ) )
	DEFREVENDYEAR <- zeroPC( DEFREVENDYEAR )


	## BEGINNING OF YEAR TAX-EXEMPT BOND LIABILITIES

	V_990BOYBLpost2013 <- "//Return/ReturnData/IRS990/TaxExemptBondLiabilitiesGrp/BOYAmt"
	V_990BOYBLpre2013  <- "//Return/ReturnData/IRS990/TaxExemptBondLiabilities/BOY"
	bond.liab.beg.xpath <- paste( V_990BOYBLpost2013, V_990BOYBLpre2013, sep="|" )
	BONDBEGYEAR <- xml_text( xml_find_all( doc, bond.liab.beg.xpath ) ) 
	BONDBEGYEAR <- zeroPC( BONDBEGYEAR )


	## END OF YEAR TAX-EXEMPT BOND LIABILITIES

	V_990EOYBLpost2013 <- "//Return/ReturnData/IRS990/TaxExemptBondLiabilitiesGrp/EOYAmt"
	V_990EOYBLpre2013  <- "//Return/ReturnData/IRS990/TaxExemptBondLiabilities/EOY"
	bond.liab.end.xpath <- paste( V_990EOYBLpost2013, V_990EOYBLpre2013, sep="|" )
	BONDENDYEAR <- xml_text( xml_find_all( doc, bond.liab.end.xpath ) )
	BONDENDYEAR <- zeroPC( BONDENDYEAR )


	## BEGINNING OF YEAR ESCROW ACCOUNT LIABILITIES

	V_990BOYELpost2013 <- "//Return/ReturnData/IRS990/EscrowAccountLiabilityGrp/BOYAmt"
	V_990BOYELpre2013  <- "//Return/ReturnData/IRS990/EscrowAccountLiability/BOY"
	escrow.liab.beg.xpath <- paste( V_990BOYELpost2013, V_990BOYELpre2013, sep="|" )
	ESCROWBEGYEAR <- xml_text( xml_find_all( doc, escrow.liab.beg.xpath ) ) 
	ESCROWBEGYEAR <- zeroPC( ESCROWBEGYEAR )


	## END OF YEAR ESCROW ACCOUNT LIABILITIES

	V_990EOYELpost2013 <- "//Return/ReturnData/IRS990/EscrowAccountLiabilityGrp/EOYAmt"
	V_990EOYELpre2013  <- "//Return/ReturnData/IRS990/EscrowAccountLiability/EOY"
	escrow.liab.end.xpath <- paste( V_990EOYELpost2013, V_990EOYELpre2013, sep="|" )
	ESCROWENDYEAR <- xml_text( xml_find_all( doc, escrow.liab.end.xpath ) )
	ESCROWENDYEAR <- zeroPC( ESCROWENDYEAR )


	## BEGINNING OF YEAR LOANS TO OFFICERS
	## Confirm that this is the appropriate xpath - switch with xpath in Loans from Officers?

	V_990BOYLTOpost2013 <- "//Return/ReturnData/IRS990/LoansFromOfficersDirectorsGrp/BOYAmt"
	V_990BOYLTOpre2013  <- "//Return/ReturnData/IRS990/LoansFromOfficersDirectors/BOY"
	loans.to.officers.beg.xpath <- paste( V_990BOYLTOpost2013, V_990BOYLTOpre2013, sep="|" )
	LOANSTOOFFBEGYEAR <- xml_text( xml_find_all( doc, loans.to.officers.beg.xpath ) ) 
	LOANSTOOFFBEGYEAR <- zeroPC( LOANSTOOFFBEGYEAR )


	## END OF YEAR LOANS TO OFFICERS
	## Confirm that this is the appropriate xpath - switch with Xpath in Loans from Officers?

	V_990EOYLTOpost2013 <- "//Return/ReturnData/IRS990/LoansFromOfficersDirectorsGrp/EOYAmt"
	V_990EOYLTOpre2013  <- "//Return/ReturnData/IRS990/LoansFromOfficersDirectors/EOY"
	loans.to.officers.end.xpath <- paste( V_990EOYLTOpost2013, V_990EOYLTOpre2013, sep="|" )
	LOANSTOOFFENDYEAR <- xml_text( xml_find_all( doc, loans.to.officers.end.xpath ) )
	LOANSTOOFFENDYEAR <- zeroPC( LOANSTOOFFENDYEAR )


	## BEGINNING OF YEAR SECURED MORTGAGES

	V_990BOYSMpost2013 <- "//Return/ReturnData/IRS990/MortgNotesPyblScrdInvstPropGrp/BOYAmt"
	V_990BOYSMpre2013  <- "//Return/ReturnData/IRS990/MortNotesPyblSecuredInvestProp/BOY"
	mortgage.beg.xpath <- paste( V_990BOYSMpost2013, V_990BOYSMpre2013, sep="|" )
	MORTGAGEBEGYEAR <- xml_text( xml_find_all( doc, mortgage.beg.xpath ) ) 
	MORTGAGEBEGYEAR <- zeroPC( MORTGAGEBEGYEAR )


	## END OF YEAR SECURED MORTGAGES

	V_990EOYSMpost2013 <- "//Return/ReturnData/IRS990/MortgNotesPyblScrdInvstPropGrp/EOYAmt"
	V_990EOYSMpre2013  <- "//Return/ReturnData/IRS990/MortNotesPyblSecuredInvestProp/EOY"
	mortgage.end.xpath <- paste( V_990EOYSMpost2013, V_990EOYSMpre2013, sep="|" )
	MORTGAGEENDYEAR <- xml_text( xml_find_all( doc, mortgage.end.xpath ) )
	MORTGAGEENDYEAR <- zeroPC( MORTGAGEENDYEAR )


	## BEGINNING OF YEAR UNSECURED NOTES

	V_990BOYUNpost2013 <- "//Return/ReturnData/IRS990/UnsecuredNotesLoansPayableGrp/BOYAmt"
	V_990BOYUNpre2013  <- "//Return/ReturnData/IRS990/UnsecuredNotesLoansPayable/BOY"
	unsec.notes.beg.xpath <- paste( V_990BOYUNpost2013, V_990BOYUNpre2013, sep="|" )
	UNSECNOTESBEGYEAR <- xml_text( xml_find_all( doc, unsec.notes.beg.xpath ) ) 
	UNSECNOTESBEGYEAR <- zeroPC( UNSECNOTESBEGYEAR )


	## END OF YEAR UNSECURED NOTES

	V_990EOYUNpost2013 <- "//Return/ReturnData/IRS990/UnsecuredNotesLoansPayableGrp/EOYAmt"
	V_990EOYUNpre2013  <- "//Return/ReturnData/IRS990/UnsecuredNotesLoansPayable/EOY"
	unsec.notes.end.xpath <- paste( V_990EOYUNpost2013, V_990EOYUNpre2013, sep="|" )
	UNSECNOTESENDYEAR <- xml_text( xml_find_all( doc, unsec.notes.end.xpath ) )
	UNSECNOTESENDYEAR <- zeroPC( UNSECNOTESENDYEAR )


	## BEGINNING OF YEAR OTHER LIABILITIES

	V_990BOYOLpost2013 <- "//Return/ReturnData/IRS990/OtherLiabilitiesGrp/BOYAmt"
	V_990BOYOLpre2013  <- "//Return/ReturnData/IRS990/OtherLiabilities/BOY"
	other.liab.beg.xpath <- paste( V_990BOYOLpost2013, V_990BOYOLpre2013, sep="|" )
	OTHERLIABBEGYEAR <- xml_text( xml_find_all( doc, other.liab.beg.xpath ) ) 
	OTHERLIABBEGYEAR <- zeroPC( OTHERLIABBEGYEAR )


	## END OF YEAR OTHER LIABILITIES

	V_990EOYOLpost2013 <- "//Return/ReturnData/IRS990/OtherLiabilitiesGrp/EOYAmt"
	V_990EOYOLpre2013  <- "//Return/ReturnData/IRS990/OtherLiabilities/EOY"
	other.liab.end.xpath <- paste( V_990EOYOLpost2013, V_990EOYOLpre2013, sep="|" )
	OTHERLIABENDYEAR <- xml_text( xml_find_all( doc, other.liab.end.xpath ) )
	OTHERLIABENDYEAR <- zeroPC( OTHERLIABENDYEAR )


	## BEGINNING OF YEAR TOTAL LIABILITIES FROM BALANCE SHEET
	## Should equal TOTALLIABBEGYEAR

	V_990BOYBSTLpost2013 <- "//Return/ReturnData/IRS990/TotalLiabilitiesGrp/BOYAmt"
	V_990BOYBSTLpre2013  <- "//Return/ReturnData/IRS990/TotalLiabilities/BOY"
	totalliab.balsheet.beg.xpath <- paste( V_990BOYBSTLpost2013, V_990BOYBSTLpre2013, sep="|" )
	TLBALSHEETBEGYEAR <- xml_text( xml_find_all( doc, totalliab.balsheet.beg.xpath ) ) 
	TLBALSHEETBEGYEAR <- zeroPC( TLBALSHEETBEGYEAR )


	## END OF YEAR TOTAL LIABILITIES FROM BALANCE SHEET
	## Should equal TOTALLIABENDYEAR

	V_990EOYBSTLpost2013 <- "//Return/ReturnData/IRS990/TotalLiabilitiesGrp/EOYAmt"
	V_990EOYBSTLpre2013  <- "//Return/ReturnData/IRS990/TotalLiabilities/EOY"
	totalliab.balsheet.end.xpath <- paste( V_990EOYBSTLpost2013, V_990EOYBSTLpre2013, sep="|" )
	TLBALSHEETENDYEAR <- xml_text( xml_find_all( doc, totalliab.balsheet.end.xpath ) ) 
	TLBALSHEETENDYEAR <- zeroPC( TLBALSHEETENDYEAR )


	
	#------------------------------------------------------------------------------------------------------------------------
	#####  PART X - NET ASSETS OR FUND BALANCES
	#####   PC-specific Values

	## ORGANIZATION FOLLOWS SFAS 117 (checkbox)

	V_990SFASYpost2013 <- "//Return/ReturnData/IRS990/OrganizationFollowsSFAS117Ind"
	V_990SFASYpre2013  <- "//Return/ReturnData/IRS990/FollowSFAS117"
	sfas.yes.xpath <- paste( V_990SFASYpost2013, V_990SFASYpre2013, sep="|" )
	ORGSFAS117 <- xml_text( xml_find_all( doc, sfas.yes.xpath ) ) 



	## ORGANIZATION DOES NOT FOLLOWS SFAS 117 (checkbox)

	V_990SFASYpost2013 <- "//Return/ReturnData/IRS990/OrgDoesNotFollowSFAS117Ind"
	V_990SFASYpre2013  <- "//Return/ReturnData/IRS990/DoNotFollowSFAS117"
	sfas.no.xpath <- paste( V_990SFASYpost2013, V_990SFASYpre2013, sep="|" )
	ORGNOTSFAS117 <- xml_text( xml_find_all( doc, sfas.no.xpath ) )



	## BEGINNING OF YEAR UNRESTRICTED NET ASSETS

	V_990BOYUApost2013 <- "//Return/ReturnData/IRS990/UnrestrictedNetAssetsGrp/BOYAmt"
	V_990BOYUApre2013  <- "//Return/ReturnData/IRS990/UnrestrictedNetAssets/BOY"
	unrest.na.beg.xpath <- paste( V_990BOYUApost2013, V_990BOYUApre2013, sep="|" )
	URESTNABEGYEAR <- xml_text( xml_find_all( doc, unrest.na.beg.xpath ) ) 
	URESTNABEGYEAR <- zeroPC( URESTNABEGYEAR )


	## END OF YEAR UNRESTRICTED NET ASSETS

	V_990EOYUApost2013 <- "//Return/ReturnData/IRS990/UnrestrictedNetAssetsGrp/EOYAmt"
	V_990EOYUApre2013  <- "//Return/ReturnData/IRS990/UnrestrictedNetAssets/EOY"
	unrest.na.end.xpath <- paste( V_990EOYUApost2013, V_990EOYUApre2013, sep="|" )
	URESTNAENDYEAR <- xml_text( xml_find_all( doc, unrest.na.end.xpath ) )
	URESTNAENDYEAR <- zeroPC( URESTNAENDYEAR )


	## BEGINNING OF YEAR TEMPORARILY RESTRICTED NET ASSETS

	V_990BOYTRApost2013 <- "//Return/ReturnData/IRS990/TemporarilyRstrNetAssetsGrp/BOYAmt"
	V_990BOYTRApre2013  <- "//Return/ReturnData/IRS990/TemporarilyRestrictedNetAssets/BOY"
	temp.rest.na.beg.xpath <- paste( V_990BOYTRApost2013, V_990BOYTRApre2013, sep="|" )
	TRESTNABEGYEAR <- xml_text( xml_find_all( doc, temp.rest.na.beg.xpath ) ) 
	TRESTNABEGYEAR <- zeroPC( TRESTNABEGYEAR )


	## END OF YEAR TEMPORARILY RESTRICTED NET ASSETS

	V_990EOYTRApost2013 <- "//Return/ReturnData/IRS990/TemporarilyRstrNetAssetsGrp/EOYAmt"
	V_990EOYTRApre2013  <- "//Return/ReturnData/IRS990/TemporarilyRestrictedNetAssets/EOY"
	temp.rest.na.end.xpath <- paste( V_990EOYTRApost2013, V_990EOYTRApre2013, sep="|" )
	TRESTNAENDYEAR  <- xml_text( xml_find_all( doc, temp.rest.na.end.xpath ) )
	TRESTNAENDYEAR <- zeroPC( TRESTNAENDYEAR )


	## BEGINNING OF YEAR PERMANENTLY RESTRICTED NET ASSETS

	V_990BOYPRApost2013 <- "//Return/ReturnData/IRS990/PermanentlyRstrNetAssetsGrp/BOYAmt"
	V_990BOYPRApre2013  <- "//Return/ReturnData/IRS990/PermanentlyRestrictedNetAssets/BOY"
	perm.rest.na.beg.xpath <- paste( V_990BOYPRApost2013, V_990BOYPRApre2013, sep="|" )
	PRESTNABEGYEAR <- xml_text( xml_find_all( doc, perm.rest.na.beg.xpath ) ) 
	PRESTNABEGYEAR <- zeroPC( PRESTNABEGYEAR )


	## END OF YEAR PERMANENTLY RESTRICTED NET ASSETS

	V_990EOYPRApost2013 <- "//Return/ReturnData/IRS990/PermanentlyRstrNetAssetsGrp/EOYAmt"
	V_990EOYPRApre2013  <- "//Return/ReturnData/IRS990/PermanentlyRestrictedNetAssets/EOY"
	perm.rest.na.end.xpath <- paste( V_990EOYPRApost2013, V_990EOYPRApre2013, sep="|" )
	PRESTNAENDYEAR  <- xml_text( xml_find_all( doc, perm.rest.na.end.xpath ) )
	PRESTNAENDYEAR <- zeroPC( PRESTNAENDYEAR )


	## BEGINNING OF YEAR CAPITAL STOCK

	V_990BOYSTOCKpost2013 <- "//Return/ReturnData/IRS990/CapStkTrPrinCurrentFundsGrp/BOYAmt"
	V_990BOYSTOCKpre2013  <- "//Return/ReturnData/IRS990/CapStckTrstPrinCurrentFunds/BOY"
	stock.beg.xpath <- paste( V_990BOYSTOCKpost2013, V_990BOYSTOCKpre2013, sep="|" )
	STOCKBEGYEAR <- xml_text( xml_find_all( doc, stock.beg.xpath ) ) 
	STOCKBEGYEAR <- zeroPC( STOCKBEGYEAR )


	## END OF YEAR CAPITAL STOCK

	V_990EOYSTOCKpost2013 <- "//Return/ReturnData/IRS990/CapStkTrPrinCurrentFundsGrp/EOYAmt"
	V_990EOYSTOCKpre2013  <- "//Return/ReturnData/IRS990/CapStckTrstPrinCurrentFunds/EOY"
	stock.end.xpath <- paste( V_990EOYSTOCKpost2013, V_990EOYSTOCKpre2013, sep="|" )
	STOCKENDYEAR  <- xml_text( xml_find_all( doc, stock.end.xpath ) )
	STOCKENDYEAR <- zeroPC( STOCKENDYEAR )


	## BEGINNING OF YEAR PAID-IN SURPLUS

	V_990BOYPISpost2013 <- "//Return/ReturnData/IRS990/PdInCapSrplsLandBldgEqpFundGrp/BOYAmt"
	V_990BOYPISpre2013  <- "//Return/ReturnData/IRS990/PaidInCapSrplsLandBldgEqpFund/BOY"
	surplus.beg.xpath <- paste( V_990BOYPISpost2013, V_990BOYPISpre2013, sep="|" )
	SURPLUSBEGYEAR <- xml_text( xml_find_all( doc, surplus.beg.xpath ) ) 
	SURPLUSBEGYEAR <- zeroPC( SURPLUSBEGYEAR )


	## END OF YEAR PAID-IN SURPLUS

	V_990EOYPISpost2013 <- "//Return/ReturnData/IRS990/PdInCapSrplsLandBldgEqpFundGrp/EOYAmt"
	V_990EOYPISpre2013  <- "//Return/ReturnData/IRS990/PaidInCapSrplsLandBldgEqpFund/EOY"
	surplus.end.xpath <- paste( V_990EOYPISpost2013, V_990EOYPISpre2013, sep="|" )
	SURPLUSENDYEAR  <- xml_text( xml_find_all( doc, surplus.end.xpath ) )
	SURPLUSENDYEAR <- zeroPC( SURPLUSENDYEAR )


	## BEGINNING OF YEAR RETAINED EARNINGS

	V_990BOYREpost2013 <- "//Return/ReturnData/IRS990/RtnEarnEndowmentIncmOthFndsGrp/BOYAmt"
	V_990BOYREpre2013  <- "//Return/ReturnData/IRS990/RetainedEarningsEndowmentEtc/BOY"
	earnings.beg.xpath <- paste( V_990BOYREpost2013, V_990BOYREpre2013, sep="|" )
	EARNINGSBEGYEAR <- xml_text( xml_find_all( doc, earnings.beg.xpath ) ) 
	EARNINGSBEGYEAR <- zeroPC( EARNINGSBEGYEAR )


	## END OF YEAR RETAINED EARNINGS

	V_990EOYREpost2013 <- "//Return/ReturnData/IRS990/RtnEarnEndowmentIncmOthFndsGrp/EOYAmt"
	V_990EOYREpre2013 <- "//Return/ReturnData/IRS990/RetainedEarningsEndowmentEtc/EOY"
	earnings.end.xpath <- paste( V_990EOYREpost2013, V_990EOYREpre2013, sep="|" )
	EARNINGSENDYEAR <- xml_text( xml_find_all( doc, earnings.end.xpath ) )
	EARNINGSENDYEAR <- zeroPC( EARNINGSENDYEAR )


	## BEGINNING OF YEAR TOTAL NET ASSETS AND FUND BALANCES

	V_990BOYTNApost2013 <- "//Return/ReturnData/IRS990/TotalNetAssetsFundBalanceGrp/BOYAmt"
	V_990BOYTNApre2013 <- "//Return/ReturnData/IRS990/TotalNetAssetsFundBalances/BOY"
	total.na.beg.xpath <- paste( V_990BOYTNApost2013, V_990BOYTNApre2013, sep="|" )
	TOTNETASSETSBEGYEAR <- xml_text( xml_find_all( doc, total.na.beg.xpath ) ) 
	TOTNETASSETSBEGYEAR <- zeroPC( TOTNETASSETSBEGYEAR )


	## END OF YEAR TOTAL NET ASSETS AND FUND BALANCES

	V_990EOYTNApost2013 <- "//Return/ReturnData/IRS990/TotalNetAssetsFundBalanceGrp/EOYAmt"
	V_990EOYTNApre2013 <- "//Return/ReturnData/IRS990/TotalNetAssetsFundBalances/EOY"
	total.na.end.xpath <- paste( V_990EOYTNApost2013, V_990EOYTNApre2013, sep="|" )
	TOTNETASSETSENDYEAR <- xml_text( xml_find_all( doc, total.na.end.xpath ) )
	TOTNETASSETSENDYEAR <- zeroPC( TOTNETASSETSENDYEAR )


	## BEGINNING OF YEAR TOTAL LIABILITIES AND NET ASSETS/FUND BALANCES

	V_990BOYTLANApost2013 <- "//Return/ReturnData/IRS990/TotLiabNetAssetsFundBalanceGrp/BOYAmt"
	V_990BOYTLANApre2013  <- "//Return/ReturnData/IRS990/TotalLiabNetAssetsFundBalances/BOY"
	total.liab.na.beg.xpath <- paste( V_990BOYTLANApost2013, V_990BOYTLANApre2013, sep="|" )
	TOTLIABNABEGYEAR <- xml_text( xml_find_all( doc, total.liab.na.beg.xpath ) ) 
	TOTLIABNABEGYEAR <- zeroPC( TOTLIABNABEGYEAR )


	## END OF YEAR TOTAL LIABILITIES AND NET ASSETS/FUND BALANCES

	V_990EOYTLANApost2013 <- "//Return/ReturnData/IRS990/TotLiabNetAssetsFundBalanceGrp/EOYAmt"
	V_990EOYTLANApre2013  <- "//Return/ReturnData/IRS990/TotalLiabNetAssetsFundBalances/EOY"
	total.liab.na.end.xpath <- paste( V_990EOYTLANApost2013, V_990EOYTLANApre2013, sep="|" )
	TOTLIABNAENDYEAR <- xml_text( xml_find_all( doc, total.liab.na.end.xpath ) )
	TOTLIABNAENDYEAR <- zeroPC( TOTLIABNAENDYEAR )
	
	
	
	#------------------------------------------------------------------------------------------------------------------------
	#####  LIST OF SCHEDULES
	

	## SCHEDULE A FILED
	
	SCHEDA <- grepl( "IRS990ScheduleA", doc )
	
	
	
	## SCHEDULE B FILED
	
	SCHEDB <- grepl( "IRS990ScheduleB", doc )
	
	
	
	## SCHEDULE C FILED
	
	SCHEDC <- grepl( "IRS990ScheduleC", doc )
	
	
	
	## SCHEDULE D FILED
	
	SCHEDD <- grepl( "IRS990ScheduleD", doc )
	
	
	
	## SCHEDULE E FILED
	
	SCHEDE <- grepl( "IRS990ScheduleE", doc )
	
	
	
	## SCHEDULE F FILED
	
	SCHEDF <- grepl( "IRS990ScheduleF", doc )
	
	
	
	## SCHEDULE G FILED
	
	SCHEDG <- grepl( "IRS990ScheduleG", doc )
	
	
	
	## SCHEDULE H FILED
	
	SCHEDH <- grepl( "IRS990ScheduleH", doc )
	
	
	
	## SCHEDULE I FILED
	
	SCHEDI <- grepl( "IRS990ScheduleI", doc )
	
	
	
	## SCHEDULE J FILED
	
	SCHEDJ <- grepl( "IRS990ScheduleJ", doc )
	
	
	
	## SCHEDULE K FILED
	
	SCHEDK <- grepl( "IRS990ScheduleK", doc )
	
	
	
	## SCHEDULE L FILED
	
	SCHEDL <- grepl( "IRS990ScheduleL", doc )
	
	
	
	## SCHEDULE M FILED
	
	SCHEDM <- grepl( "IRS990ScheduleM", doc )
	
	
	
	## SCHEDULE N FILED
	
	# need to add criteria that 990EZ, Part V, Line 36 != FALSE
	v1 <- "//Return/ReturnData/IRS990EZ/OrganizationDissolvedEtcInd"
	v2 <- "/Return/ReturnData/IRS990EZ/OrganizationDissolvedEtc"
        org.dissolved.xpath <- paste( v1, v2, sep="|" )
	ORGDISSOLVED <- xml_text( xml_find_all( doc, org.dissolved.xpath ) )

        
	# SCHEDN <- grepl( "IRS990ScheduleN", doc )
	SCHEDN <- sum( grepl( "IRS990ScheduleN", (doc %>% xml_find_all( '//*') %>% xml_path() ) ) ) > 0
	
	
	
	## SCHEDULE O FILED
	
	SCHEDO <- grepl( "IRS990ScheduleO", doc )
	

		
	## SCHEDULE R FILED
	
	SCHEDR <- grepl( "IRS990ScheduleR", doc )




	## FINAL RETURN / TERMINATING OPERATIONS
	
	v1 <- "//Return/ReturnData/IRS990/FinalReturnInd"
	v2 <- "//Return/ReturnData/IRS990EZ/FinalReturnInd"
	v3 <- "//Return/ReturnData/IRS990/TerminatedReturn"
	v4 <- "//Return/ReturnData/IRS990EZ/TerminatedReturn"
	v5 <- "//Return/ReturnData/IRS990EZ/TerminationReturn"
	terminated.xpaths <- paste( v1, v2, v3, v4, v5, sep="|" )
	TERMINATED <- xml_text( xml_find_all( doc, terminated.xpaths ) )

	
	

	

	#------------------------------------------------------------------------------------------------------------------------
	#####  SCHEDULE C
	## The xpaths are the same for PC and EZ


	## PUBLIC OPINION/GRASS ROOTS LOBBYING EXPENSES OF FILING ORGANIZATION

	V_990POLFpost2013 <- "//Return/ReturnData/IRS990ScheduleC/TotalGrassrootsLobbyingGrp/FilingOrganizationsTotalAmt"
	V_990POLFpre2013  <- "//Return/ReturnData/IRS990ScheduleC/TotalGrassrootsLobbying/FilingOrganizationsTotals"
	lobbying.pubopinion.filing.xpath <- paste( V_990POLFpost2013, V_990POLFpre2013, sep="|" )
	LOBPOFILING  <- xml_text( xml_find_all( doc, lobbying.pubopinion.filing.xpath ) )
	LOBPOFILING  <- zeroALL( LOBPOFILING )


	## PUBLIC OPINION/GRASS ROOTS LOBBYING EXPENSES OF AFFILIATED GROUP

	V_990POLApost2013 <- "//Return/ReturnData/IRS990ScheduleC/TotalGrassrootsLobbyingGrp/AffiliatedGroupTotalAmt"
	V_990POLApre2013  <- "//Return/ReturnData/IRS990ScheduleC/TotalGrassrootsLobbying/AffiliatedGroupTotals"
	lobbying.pubopinion.affiliated.xpath <- paste( V_990POLApost2013, V_990POLApre2013, sep="|" )
	LOBPOAFFIL  <- xml_text( xml_find_all( doc, lobbying.pubopinion.affiliated.xpath ) )
	LOBPOAFFIL  <- zeroALL( LOBPOAFFIL )


	## LEGISLATIVE BODY/DIRECT LOBBYING EXPENSES OF FILING ORGANIZATION

	V_990LBLFpost2013 <- "//Return/ReturnData/IRS990ScheduleC/TotalDirectLobbyingGrp/FilingOrganizationsTotalAmt"
	V_990LBLFpre2013  <- "//Return/ReturnData/IRS990ScheduleC/TotalDirectLobbying/FilingOrganizationsTotals"
	lobbying.legislative.filing.xpath <- paste( V_990LBLFpost2013, V_990LBLFpre2013, sep="|" )
	LOBLBFILING  <- xml_text( xml_find_all( doc, lobbying.legislative.filing.xpath ) )
	LOBLBFILING  <- zeroALL( LOBLBFILING )


	## LEGISTLATIVE BODY/DIRECT LOBBYING EXPENSES OF AFFILIATED GROUP

	V_990LBLApost2013 <- "//Return/ReturnData/IRS990ScheduleC/TotalDirectLobbyingGrp/AffiliatedGroupTotalAmt"
	V_990LBLApre2013  <- "//Return/ReturnData/IRS990ScheduleC/TotalDirectLobbying/AffiliatedGroupTotals"
	lobbying.legislative.affiliated.xpath <- paste( V_990LBLApost2013, V_990LBLApre2013, sep="|" )
	LOBLBAFFIL  <- xml_text( xml_find_all( doc, lobbying.legislative.affiliated.xpath ) )
	LOBLBAFFIL  <- zeroALL( LOBLBAFFIL )


	## TOTAL LOBBYING EXPENSES OF FILING ORGANIZATION

	V_990TLXFpost2013 <- "//Return/ReturnData/IRS990ScheduleC/TotalLobbyingExpendGrp/FilingOrganizationsTotalAmt"
	V_990TLXFpre2013  <- "//Return/ReturnData/IRS990ScheduleC/TotalLobbyingExpenditures/FilingOrganizationsTotals"
	lobbying.totalexp.filing.xpath <- paste( V_990TLXFpost2013, V_990TLXFpre2013, sep="|" )
	TOTLOBEXPFILING  <- xml_text( xml_find_all( doc, lobbying.totalexp.filing.xpath ) )
	TOTLOBEXPFILING  <- zeroALL( TOTLOBEXPFILING )


	## TOTAL LOBBYING EXPENSES OF AFFILIATED GROUP

	V_990TLXApost2013 <- "//Return/ReturnData/IRS990ScheduleC/TotalLobbyingExpendGrp/AffiliatedGroupTotalAmt"
	V_990TLXApre2013  <- "//Return/ReturnData/IRS990ScheduleC/TotalLobbyingExpenditures/AffiliatedGroupTotals"
	lobbying.totalexp.affiliated.xpath <- paste( V_990TLXApost2013, V_990TLXApre2013, sep="|" )
	TOTLOBEXPAFFIL  <- xml_text( xml_find_all( doc, lobbying.totalexp.affiliated.xpath ) )
	TOTLOBEXPAFFIL  <- zeroALL( TOTLOBEXPAFFIL )


	## OTHER EXEMPT EXPENSES OF FILING ORGANIZATION

	V_990.OXFpost2013 <- "//Return/ReturnData/IRS990ScheduleC/OtherExemptPurposeExpendGrp/FilingOrganizationsTotalAmt"
	V_990.OXFpre2013  <- "//Return/ReturnData/IRS990ScheduleC/OtherExemptPurposeExpenditures/FilingOrganizationsTotals"
	lobbying.otherexemptexp.filing.xpath <- paste( V_990.OXFpost2013, V_990.OXFpre2013, sep="|" )
	OTHEREXEMPTFILING  <- xml_text( xml_find_all( doc, lobbying.otherexemptexp.filing.xpath ) )
	OTHEREXEMPTFILING  <- zeroALL( OTHEREXEMPTFILING )


	## OTHER EXEMPT EXPENSES OF AFFILIATED GROUP

	V_990.OXApost2013 <- "//Return/ReturnData/IRS990ScheduleC/OtherExemptPurposeExpendGrp/AffiliatedGroupTotalAmt"
	V_990.OXApre2013  <- "//Return/ReturnData/IRS990ScheduleC/OtherExemptPurposeExpenditures/AffiliatedGroupTotals"
	lobbying.otherexemptexp.affiliated.xpath <- paste( V_990.OXApost2013, V_990.OXApre2013, sep="|" )
	OTHEREXEMPTAFFIL  <- xml_text( xml_find_all( doc, lobbying.otherexemptexp.affiliated.xpath ) )
	OTHEREXEMPTAFFIL  <- zeroALL( OTHEREXEMPTAFFIL )


	## TOTAL EXEMPT EXPENSES OF FILING ORGANIZATION

	V_990TEEFpost2013 <- "//Return/ReturnData/IRS990ScheduleC/TotalExemptPurposeExpendGrp/FilingOrganizationsTotalAmt"
	V_990TEEFpre2013  <- "//Return/ReturnData/IRS990ScheduleC/TotalExemptPurposeExpenditures/FilingOrganizationsTotals"
	lobbying.totexempt.filing.xpath <- paste( V_990TEEFpost2013, V_990TEEFpre2013, sep="|" )
	TOTEXEMPTFILING  <- xml_text( xml_find_all( doc, lobbying.totexempt.filing.xpath ) )
	TOTEXEMPTFILING  <- zeroALL( TOTEXEMPTFILING )


	## TOTAL EXEMPT EXPENSES OF AFFILIATED GROUP

	V_990TEEApost2013 <- "//Return/ReturnData/IRS990ScheduleC/TotalExemptPurposeExpendGrp/AffiliatedGroupTotalAmt"
	V_990TEEApre2013  <- "//Return/ReturnData/IRS990ScheduleC/TotalExemptPurposeExpenditures/AffiliatedGroupTotals"
	lobbying.totexempt.affiliated.xpath <- paste( V_990TEEApost2013, V_990TEEApre2013, sep="|" )
	TOTEXEMPTAFFIL  <- xml_text( xml_find_all( doc, lobbying.totexempt.affiliated.xpath ) )
	TOTEXEMPTAFFIL  <- zeroALL( TOTEXEMPTAFFIL )


	## LOBBYING NONTAXABLE AMOUNT OF FILING ORGANIZATION

	V_990LNTFpost2013 <- "//Return/ReturnData/IRS990ScheduleC/LobbyingNontaxableAmountGrp/FilingOrganizationsTotalAmt"
	V_990LNTFpre2013  <- "//Return/ReturnData/IRS990ScheduleC/LobbyingNontaxableAmount/FilingOrganizationsTotals"
	lobbying.nontax.filing.xpath <- paste( V_990LNTFpost2013, V_990LNTFpre2013, sep="|" )
	LOBNTFILING  <- xml_text( xml_find_all( doc, lobbying.nontax.filing.xpath ) )
	LOBNTFILING  <- zeroALL( LOBNTFILING )


	## LOBBYING NONTAXABLE AMOUNT OF AFFILIATED GROUP

	V_990LNTApost2013 <- "//Return/ReturnData/IRS990ScheduleC/LobbyingNontaxableAmountGrp/AffiliatedGroupTotalAmt"
	V_990LNTApre2013  <- "//Return/ReturnData/IRS990ScheduleC/LobbyingNontaxableAmount/AffiliatedGroupTotals"
	lobbying.nontax.affil.xpath <- paste( V_990LNTApost2013, V_990LNTApre2013, sep="|" )
	LOBNTAFFIL  <- xml_text( xml_find_all( doc, lobbying.nontax.affil.xpath ) )
	LOBNTAFFIL  <- zeroALL( LOBNTAFFIL )

	
	

	#------------------------------------------------------------------------------------------------------------------------
	###  BIND VARIABLES TOGETHER

	namedList <- function(...){
		      names <- as.list(substitute(list(...)))[-1L]
		      result <- list(...)
		      names(result) <- names
		      result[sapply(result, function(x){length(x)==0})] <- NA
		      result[sapply(result, is.null)] <- NA
		      result
		  }


	core <- namedList( # HEADER
			       EIN, NAME, DBA, FISYR, STATE, ADDRESS, CITY, ZIP, STYEAR, ENDYEAR, 
			       TAXPREP, FORMTYPE, AMMENDED,
			       # BASIC INFO
			       GROSSRECEIPTS, GROUPRETURN, GROUPEXEMPTNUM, FORMYEAR, DOMICILE, 
			       WEBSITE, URL,TERMINATED,ORGDISSOLVED,
			       ## Collapse values for FORMORG
			       FORMORGASSOC, FORMORGCORP, FORMORGTRUST, FORMORGOTHER, 
			       FORMORGOTHERDESC, FORMORG, 
			       ## Collapse values for ACCTMETHOD
			       ACCTACCRUAL, ACCTCASH, ACCTOTHER, ACCTMETHOD, 
			       ## Collapse values for EXEMPTSTATUS
			       EXEMPT4947A1, EXEMPT501C, EXEMPT501CNUM, EXEMPT501C3, 
			       EXEMPT527, EXEMPTSTATUS,
			       # PART I 
			       MISSION, DISCOPS, VOTINGMEMBERS, INDVOTINGMEMBERS, TOTEMPLOYEE, 
			       TOTVOLUNTEERS, TOTUBI, NETUBI, CONTRIBPRIOR, CONTRIBCURRENT, 
			       PSRPRIOR, PSRCURRENT, INVINCPRIOR, INVINCCURRENT, OTHERREVPRIOR, 
			       OTHERREVCURRENT, TOTALREVPRIOR, TOTALREVCURRENT, MEMBERDUES, 
			       GROSSSALESOTHER, SALESCOSTOTHER, NETSALESOTHER, GROSSINCGAMING, 
			       GROSSINCFNDEVENTS, 
			       ## Sum PC values for EXPGAMINGFNDEVENTS
			       GAMINGEXP, FNDEVENTSEXP, EXPGAMINGFNDEVENTS, 
			       ## Sum PC values for NETGAMINGFNDEVENTS
			       GAMINGNET, FNDEVENTSNET, NETGAMINGFNDEVENTS, GROSSSALESINV, 
			       SALESCOSTINV, NETSALESINV, GRANTSPAIDPRIOR, GRANTSPAIDCURRENT, 
			       MEMBERBENPRIOR, MEMBERBENCURRENT, SALARIESPRIOR, SALARIESCURRENT, 
			       PROFUNDFEESPRIOR, PROFUNDFEESCURRENT, TOTFUNDEXP, 
			       ## Sum PC values for PROFEESINDEP
			       FEESMGMT, FEESLEGAL, FEESACCT, FEESLOBBY,FEESPROFND, FEESINVMGMT, 
			       FEESOTHER, PROFEESINDEP, 
			       OCCUPANCY, OFFICEEXP, OTHEREXPPRIOR, OTHEREXPCURRENT, 
			       TOTALEXPPRIOR, TOTALEXPCURRENT, REVLESSEXPPRIOR, REVLESSEXPCURRENT, 
			       TOTALASSETSBEGYEAR, TOTALASSETSENDYEAR, TOTALLIABBEGYEAR, 
			       TOTALLIABENDYEAR, NETASSETSBEGYEAR, OTHERASSETSCHANGES, 
			       NETASSETSENDYEAR, 
			       # PART II (EZ) / X (PC)
			       ## Sum PC values for CASHINVBEGYEAR and CASHINVENDYEAR
			       CASHBEGYEAR, CASHENDYEAR, SAVINVBEGYEAR, SAVINVENDYEAR, 
			       CASHINVBEGYEAR, CASHINVENDYEAR, 
			       LANDBLDEQUIPCOST, LANDBLDEQUIPDEP, LANDBEGYEAR, LANDENDYEAR, 
			       OTHERASSETSBEGYEAR, OTHERASSETSENDYEAR,
			       # PART III
			       TOTALPROGSERVEXP,
			       # PART IV
			       LOBBYING, FOREIGNREV,
			       # PART VI
			       SCHEDOPARTVI, VMGOVERNING, IVMGOVERNING, OFFICERREL, MGMTDEL,
			       CHANGESGOVDOCS, DIVASSETS, STOCKMEMBER, MEMBERCHOOSE, 
			       GOVBODYDECISION, GOVBODYDOCU, COMMITTEEDOCU, NOTREACH,
			       COIPOLICY, COIDISCLOSE, COIMONITOR, WBPOLICY, FILINGSTATES,
			       ## Collapse values for PUBLICSHARE
			       PUBLICWEBSELF, PUBLICWEBOTHER, PUBLICREQUEST, PUBLICOTHER, 
			       PUBLICSHARE,
			       ORGBOOKNAME, ORGBOOKPHONE, ORGBOOKADDRESS, ORGBOOKCITY, 
			       ORGBOOKSTATE, ORGBOOKZIP,
			       # PART X
			       PLEDGEGRANTBEGYEAR, PLEDGEGRANTENDYEAR, ACCTRECBEGYEAR, ACCTRECENDYEAR,
			       LOANSFROMOFFBEGYEAR, LOANSFROMOFFENDYEAR, LOANSDQPBEGYEAR, LOANSDQPENDYEAR,
			       LOANSNOTESBEGYEAR, LOANSNOTESENDYEAR, INVENTORYBEGYEAR, INVENTORYENDYEAR,
			       PREEXPBEGYEAR, PREEXPENDYEAR, INVESTPUBBEGYEAR, INVESTPUBENDYEAR, 
			       INVESTOTHBEGYEAR, INVESTOTHENDYEAR, INVESTPRGBEGYEAR, INVESTPRGENDYEAR, 
			       INTANASSETSBEGYEAR, INTANASSETSENDYEAR, TABALSHEETBEGYEAR, TABALSHEETENDYEAR,
			       ACCTPAYBEGYEAR, ACCTPAYENDYEAR, GRANTSPAYBEGYEAR, GRANTSPAYENDYEAR, 
			       DEFREVBEGYEAR, DEFREVENDYEAR, BONDBEGYEAR, BONDENDYEAR, ESCROWBEGYEAR, 
			       ESCROWENDYEAR, LOANSTOOFFBEGYEAR, LOANSTOOFFENDYEAR, MORTGAGEBEGYEAR, 
			       MORTGAGEENDYEAR, UNSECNOTESBEGYEAR, UNSECNOTESENDYEAR, OTHERLIABBEGYEAR, 
			       OTHERLIABENDYEAR, TLBALSHEETBEGYEAR, TLBALSHEETENDYEAR, ORGSFAS117, 
			       ORGNOTSFAS117, URESTNABEGYEAR, URESTNAENDYEAR, TRESTNABEGYEAR, 
			       TRESTNAENDYEAR, PRESTNABEGYEAR, PRESTNAENDYEAR, STOCKBEGYEAR, STOCKENDYEAR, 
			       SURPLUSBEGYEAR, SURPLUSENDYEAR, EARNINGSBEGYEAR, EARNINGSENDYEAR,
			       TOTNETASSETSBEGYEAR, TOTNETASSETSENDYEAR, TOTLIABNABEGYEAR, TOTLIABNAENDYEAR,
			       # LIST OF SCHEDULES
			       SCHEDA, SCHEDB, SCHEDC, SCHEDD, SCHEDE, SCHEDF, SCHEDG, SCHEDH, 
			       SCHEDI, SCHEDJ, SCHEDK, SCHEDL, SCHEDM, SCHEDN, SCHEDO, SCHEDR,
			       ORGDISSOLVED,
			       # SCHEDULE C
			       LOBPOFILING, LOBPOAFFIL, LOBLBFILING, LOBLBAFFIL, TOTLOBEXPFILING, 
			       TOTLOBEXPAFFIL, OTHEREXEMPTFILING, OTHEREXEMPTAFFIL, TOTEXEMPTFILING,
			       TOTEXEMPTAFFIL, LOBNTFILING, LOBNTAFFIL
			      )

	core <- as.data.frame( core, stringsAsFactors=F )
	
	# variables that are functions:
	## FORMORG: Collapses values of FORMORGASSOC, FORMORGCORP, FORMORGTRUST, FORMORGOTHER, FORMORGOTHERDESC
	## ACCTMETHOD: Collapses values of ACCTACCRUAL, ACCTCASH, ACCTOTHER
	## EXEMPTSTATUS: Collapses values of EXEMPT4947A1, EXEMPT501C, EXEMPT501CNUM, EXEMPT501C3, EXEMPT527
	## EXPGAMINGFNDEVENTS: PC values are sum of GAMINGEXP and FNDEVENTSEXP
	## NETGAMINGFNDEVENTS: PC values are sum of GAMINGNET and FNDEVENTSNET
	## PROFEESINDEP: PC values are sum of FEESMGMT, FEESLEGAL, FEESACCT, FEESLOBBY,
	##                                    FEESPROFND, FEESINVMGMT, FEESOTHER
	## CASHINVBEGYEAR: PC values are sum of CASHBEGYEAR and SAVINVBEGYEAR
	## CASHINVENDYEAR: PC values are sum of CASHENDYEAR and SAVINVENDYEAR
	## PUBLICSHARE: Collapses values of PUBLICWEBSELF, PUBLICWEBOTHER, PUBLICREQUEST, PUBLICOTHER  




    
    
	



	#------------------------------------------------------------------------------------------------------------------------
	###  SCHEDULE N
	
	schedNtable.00 <- NULL
	schedNtable.01 <- NULL
	schedNtable.02 <- NULL
	schedNtable.03 <- NULL
	
	if( SCHEDN )
	{


	###############     PRIMARY TABLE - ALL 1:1 WITH EIN    #########################
	
	## VARIABLE NAME:  SN_PZ_01_SUCCESSORDIRECTOR
	## DESCRIPTION:  Become a director or trustee of a successor or transferee organization? (Liquidation of assets)
	## LOCATION:  SCHED-N-PART-01-LINE-2A
	## TABLE:  TABLE_00
	
	## REQUIRED (Y/N):  NA
	
	V1 <- '//Return/ReturnData/IRS990ScheduleN/DirectorOfSuccessor'
	V2 <- '//Return/ReturnData/IRS990ScheduleN/DirectorOfSuccessorInd'
	SUCCESSORDIRECTOR <- paste( V1, V2 , sep='|' )
	SN_PZ_01_SUCCESSORDIRECTOR <- xml_text( xml_find_all( doc, SUCCESSORDIRECTOR ) )
	
	
	
	
	
	## VARIABLE NAME:  SN_PZ_01_SUCCESSORCONTRACTOR
	## DESCRIPTION:  Become an employee of, or independent contractor for, a successor or transferee organization? (Liquidation of assets)
	## LOCATION:  SCHED-N-PART-01-LINE-2B
	## TABLE:  TABLE_00
	
	## REQUIRED (Y/N):  NA
	
	V1 <- '//Return/ReturnData/IRS990ScheduleN/EmployeeOfSuccessor'
	V2 <- '//Return/ReturnData/IRS990ScheduleN/EmployeeOfSuccessorInd'
	SUCCESSORCONTRACTOR <- paste( V1, V2 , sep='|' )
	SN_PZ_01_SUCCESSORCONTRACTOR <- xml_text( xml_find_all( doc, SUCCESSORCONTRACTOR ) )
	
	
	
	
	
	## VARIABLE NAME:  SN_PZ_01_SUCCESSOROWNER
	## DESCRIPTION:  Become a direct or indirect owner of a successor or transferee organization? (Liquidation of assets) 
	## LOCATION:  SCHED-N-PART-01-LINE-2C
	## TABLE:  TABLE_00
	
	## REQUIRED (Y/N):  NA
	
	V1 <- '//Return/ReturnData/IRS990ScheduleN/OwnerOfSuccessor'
	V2 <- '//Return/ReturnData/IRS990ScheduleN/OwnerOfSuccessorInd'
	SUCCESSOROWNER <- paste( V1, V2 , sep='|' )
	SN_PZ_01_SUCCESSOROWNER <- xml_text( xml_find_all( doc, SUCCESSOROWNER ) )
	
	
	
	
	
	## VARIABLE NAME:  SN_PZ_01_SUCCESSORCOMPENSATION
	## DESCRIPTION:  Receive, or become entitled to, compensation or other similar payments as a result of the organization's liquidation, termination, or dissolution? (Liquidation of assets)
	## LOCATION:  SCHED-N-PART-01-LINE-2D
	## TABLE:  TABLE_00
	
	## REQUIRED (Y/N):  NA
	
	V1 <- '//Return/ReturnData/IRS990ScheduleN/ReceiveCompensation'
	V2 <- '//Return/ReturnData/IRS990ScheduleN/ReceiveCompensationInd'
	SUCCESSORCOMPENSATION <- paste( V1, V2 , sep='|' )
	SN_PZ_01_SUCCESSORCOMPENSATION <- xml_text( xml_find_all( doc, SUCCESSORCOMPENSATION ) )
	
	
	
	
	
	## VARIABLE NAME:  SN_PZ_01_ASSETSDISTRIBUTED
	## DESCRIPTION:  Did the organization distribute its assets in accordance with its governing instruments?
	## LOCATION:  SCHED-N-PART-01-LINE-3
	## TABLE:  TABLE_00
	
	## REQUIRED (Y/N):  NA
	
	V1 <- '//Return/ReturnData/IRS990ScheduleN/AssetsDistributed'
	V2 <- '//Return/ReturnData/IRS990ScheduleN/AssetsDistributedInd'
	ASSETSDISTRIBUTED <- paste( V1, V2 , sep='|' )
	SN_PZ_01_ASSETSDISTRIBUTED <- xml_text( xml_find_all( doc, ASSETSDISTRIBUTED ) )
	
	
	
	
	
	## VARIABLE NAME:  SN_PZ_01_EXEMPTSTATUSTERMINATED
	## DESCRIPTION:  Did the organization request or receive a determination letter from EO Determinations that the organization's exempt status was terminated?
	## LOCATION:  SCHED-N-PART-01-LINE-4A
	## TABLE:  TABLE_00
	
	## REQUIRED (Y/N):  NA
	
	SN_PZ_01_EXEMPTSTATUSTERMINATED <- xml_text( xml_find_all( doc, '/Return/ReturnData/IRS990ScheduleN/DeterminationLetter' ) )
	
	
	
	
	
	## VARIABLE NAME:  SN_PZ_01_DISSOLVENOTIFICATIONREQ
	## DESCRIPTION:  Is the organization required to notify the attorney general or other appropriate state official of its intent to dissolve?
	## LOCATION:  SCHED-N-PART-01-LINE-4A
	## TABLE:  TABLE_00
	
	## REQUIRED (Y/N):  NA
	
	V1 <- '//Return/ReturnData/IRS990ScheduleN/RequiredToNotifyAG'
	V2 <- '//Return/ReturnData/IRS990ScheduleN/RequiredToNotifyAGInd'
	V3 <- '//Return/ReturnData/IRS990ScheduleN/RequiredToNotifyAG'
	DISSOLVENOTIFICATIONREQ <- paste( V1, V2, V3 , sep='|' )
	SN_PZ_01_DISSOLVENOTIFICATIONREQ <- xml_text( xml_find_all( doc, DISSOLVENOTIFICATIONREQ ) )
	
	
	
	
	
	## VARIABLE NAME:  SN_PZ_01_DISSOLVENOTIFICATIONPROVIDED
	## DESCRIPTION:  If the organization was required to notify AG or state official of its dissolution, did the organization provide such notice?
	## LOCATION:  SCHED-N-PART-01-LINE-4B
	## TABLE:  TABLE_00
	
	## REQUIRED (Y/N):  NA
	
	V1 <- '//Return/ReturnData/IRS990ScheduleN/AGNotified'
	V2 <- '//Return/ReturnData/IRS990ScheduleN/AttorneyGeneralNotifiedInd'
	V3 <- '//Return/ReturnData/IRS990ScheduleN/AGNotified'
	DISSOLVENOTIFICATIONPROVIDED <- paste( V1, V2, V3 , sep='|' )
	SN_PZ_01_DISSOLVENOTIFICATIONPROVIDED <- xml_text( xml_find_all( doc, DISSOLVENOTIFICATIONPROVIDED ) )
	
	
	
	
	
	## VARIABLE NAME:  SN_PZ_01_EXEMPTSTATUSTERMINATEDDATE
	## DESCRIPTION:  Date of letter from the IRS that the organization's exempt status was terminated
	## LOCATION:  SCHED-N-PART-01-LINE-4B
	## TABLE:  TABLE_00
	
	## REQUIRED (Y/N):  NA
	
	SN_PZ_01_EXEMPTSTATUSTERMINATEDDATE <- xml_text( xml_find_all( doc, '/Return/ReturnData/IRS990ScheduleN/DateOfLetter' ) )
	
	
	
	
	
	## VARIABLE NAME:  SN_PZ_01_LIABILITIESPAID
	## DESCRIPTION:  Did the organization discharge or pay all liabilities in accordance with state laws?
	## LOCATION:  SCHED-N-PART-01-LINE-5
	## TABLE:  TABLE_00
	
	## REQUIRED (Y/N):  NA
	
	V1 <- '//Return/ReturnData/IRS990ScheduleN/LiabilitiesPaid'
	V2 <- '//Return/ReturnData/IRS990ScheduleN/LiabilitiesPaidInd'
	V3 <- '//Return/ReturnData/IRS990ScheduleN/LiabilitiesPaid'
	LIABILITIESPAID <- paste( V1, V2, V3 , sep='|' )
	SN_PZ_01_LIABILITIESPAID <- xml_text( xml_find_all( doc, LIABILITIESPAID ) )
	
	
	
	
	
	## VARIABLE NAME:  SN_PZ_01_BONDSOUTSTANDING
	## DESCRIPTION:  Did the organization have any tax-exempt bonds outstanding during the year?
	## LOCATION:  SCHED-N-PART-01-LINE-6A
	## TABLE:  TABLE_00
	
	## REQUIRED (Y/N):  NA
	
	V1 <- '//Return/ReturnData/IRS990ScheduleN/BondsOutstanding'
	V2 <- '//Return/ReturnData/IRS990ScheduleN/BondsOutstandingInd'
	V3 <- '//Return/ReturnData/IRS990ScheduleN/BondsOutstanding'
	BONDSOUTSTANDING <- paste( V1, V2, V3 , sep='|' )
	SN_PZ_01_BONDSOUTSTANDING <- xml_text( xml_find_all( doc, BONDSOUTSTANDING ) )
	
	
	
	
	
	## VARIABLE NAME:  SN_PZ_01_BONDSDISCHARGED
	## DESCRIPTION:  Did the organization discharge or defease tax-exempt bond liabilities in accordance with the Internal Revenue Code and state laws?
	## LOCATION:  SCHED-N-PART-01-LINE-6B
	## TABLE:  TABLE_00
	
	## REQUIRED (Y/N):  NA
	
	V1 <- '//Return/ReturnData/IRS990ScheduleN/BondLiabilitiesDischarged'
	V2 <- '//Return/ReturnData/IRS990ScheduleN/BondLiabilitiesDischargedInd'
	V3 <- '//Return/ReturnData/IRS990ScheduleN/BondLiabilitiesDischarged'
	BONDSDISCHARGED <- paste( V1, V2, V3 , sep='|' )
	SN_PZ_01_BONDSDISCHARGED <- xml_text( xml_find_all( doc, BONDSDISCHARGED ) )
	
	
	
	
	
	## VARIABLE NAME:  SN_PZ_02_DISPDIRECTOR
	## DESCRIPTION:  Become a director or trustee of a successor or transferee organization? (Disposal of assets)
	## LOCATION:  SCHED-N-PART-02-LINE-2A
	## TABLE:  TABLE_00
	
	## REQUIRED (Y/N):  NA
	
	V1 <- '//Return/ReturnData/IRS990ScheduleN/DirectorOfSuccessor2'
	V2 <- '//Return/ReturnData/IRS990ScheduleN/DirectorOfSuccessor2Ind'
	DISPDIRECTOR <- paste( V1, V2 , sep='|' )
	SN_PZ_02_DISPDIRECTOR <- xml_text( xml_find_all( doc, DISPDIRECTOR ) )
	
	
	
	
	
	## VARIABLE NAME:  SN_PZ_02_DISPCONTRACTOR
	## DESCRIPTION:  Become an employee of, or independent contractor for, a successor or transferee organization? (Disposal of assets)
	## LOCATION:  SCHED-N-PART-02-LINE-2B
	## TABLE:  TABLE_00
	
	## REQUIRED (Y/N):  NA
	
	V1 <- '//Return/ReturnData/IRS990ScheduleN/EmployeeOfSuccessor2'
	V2 <- '//Return/ReturnData/IRS990ScheduleN/EmployeeOfSuccessor2Ind'
	DISPCONTRACTOR <- paste( V1, V2 , sep='|' )
	SN_PZ_02_DISPCONTRACTOR <- xml_text( xml_find_all( doc, DISPCONTRACTOR ) )
	
	
	
	
	
	## VARIABLE NAME:  SN_PZ_02_DISPOWNER
	## DESCRIPTION:  Become a direct or indirect owner of a successor or transferee organization? (Disposal of assets)
	## LOCATION:  SCHED-N-PART-02-LINE-2C
	## TABLE:  TABLE_00
	
	## REQUIRED (Y/N):  NA
	
	V1 <- '//Return/ReturnData/IRS990ScheduleN/OwnerOfSuccessor2'
	V2 <- '//Return/ReturnData/IRS990ScheduleN/OwnerOfSuccessor2Ind'
	DISPOWNER <- paste( V1, V2 , sep='|' )
	SN_PZ_02_DISPOWNER <- xml_text( xml_find_all( doc, DISPOWNER ) )
	
	
	
	
	
	## VARIABLE NAME:  SN_PZ_02_DISPCOMPENSATION
	## DESCRIPTION:  Receive, or become entitled to, compensation or other similar payments as a result of the organization's significant disposition of assets? (Disposal of assets)
	## LOCATION:  SCHED-N-PART-02-LINE-2D
	## TABLE:  TABLE_00
	
	## REQUIRED (Y/N):  NA
	
	V1 <- '//Return/ReturnData/IRS990ScheduleN/ReceiveCompensation2'
	V2 <- '//Return/ReturnData/IRS990ScheduleN/ReceiveCompensation2Ind'
	DISPCOMPENSATION <- paste( V1, V2 , sep='|' )
	SN_PZ_02_DISPCOMPENSATION <- xml_text( xml_find_all( doc, DISPCOMPENSATION ) )

	


	#------------------------------------------------------------------------------------------------------------------------
	###  BIND VARIABLES TOGETHER





	schedNtable.00 <- namedList( # HEADER
			       EIN, NAME, FORMTYPE, FISYR, ORGDISSOLVED, DISCOPS, TERMINATED, SCHEDN,
			       # BASIC INFO
			       GROSSRECEIPTS, GROUPRETURN, GROUPEXEMPTNUM, URL,
			       # SCHEDULE N
			       SN_PZ_01_SUCCESSORDIRECTOR,
			       SN_PZ_01_SUCCESSORCONTRACTOR,
			       SN_PZ_01_SUCCESSOROWNER,
			       SN_PZ_01_SUCCESSORCOMPENSATION,
			       SN_PZ_01_ASSETSDISTRIBUTED,
			       SN_PZ_01_EXEMPTSTATUSTERMINATED,
			       SN_PZ_01_DISSOLVENOTIFICATIONREQ,
			       SN_PZ_01_DISSOLVENOTIFICATIONPROVIDED,
			       SN_PZ_01_EXEMPTSTATUSTERMINATEDDATE,
			       SN_PZ_01_LIABILITIESPAID,
			       SN_PZ_01_BONDSOUTSTANDING,
			       SN_PZ_01_BONDSDISCHARGED,
			       SN_PZ_02_DISPDIRECTOR,
			       SN_PZ_02_DISPCONTRACTOR,
			       SN_PZ_02_DISPOWNER,
			       SN_PZ_02_DISPCOMPENSATION
			       
			      )

        schedNtable.00  <- as.data.frame( schedNtable.00, stringsAsFactors=F  )









	#------------------------------------------------------------------------------------------------------------------------

	###############     TABLE 01 - LIQUIDATION OF ASSETS - 1:M WITH EIN    #########################

	#------------------------------------------------------------------------------------------------------------------------
	
	
	nd <- xml_find_all( doc, "//LiquidationOfAssetsDetail|//LiquidationDetail" )
	
	one.npo <- NULL
	
	if( length(nd) > 0 )
        {
	
	for( i in 1:length(nd) )
	{
	
	
	nodei <- nd[[i]]
	
	
	## VARIABLE NAME:  SN_PZ_01_LIQASSETDESCRIP
	## DESCRIPTION:  Description of asset(s) distributed or transactional expenses paid (Liquidated assets)
	## LOCATION:  SCHED-N-PART-01-LINE-1A
	## TABLE:  TABLE_01
	
	## REQUIRED (Y/N):  NA
	
	V1 <- 'AssetsDistriOrExpnssPaidDesc'
	V2 <- 'DescriptionOfAsset'
	LIQASSETDESCRIP <- paste( V1, V2 , sep='|' )
	SN_PZ_01_LIQASSETDESCRIP <- xml_text( xml_find_all( nodei, LIQASSETDESCRIP ) )
	
	
	
	
	
	## VARIABLE NAME:  SN_PZ_01_LIQASSETDATE
	## DESCRIPTION:  Date of distribution (Liquidated assets)
	## LOCATION:  SCHED-N-PART-01-LINE-1B
	## TABLE:  TABLE_01
	
	## REQUIRED (Y/N):  NA
	
	V1 <- 'DistributionDt'
	V2 <- 'DateOfDistribution'
	LIQASSETDATE <- paste( V1, V2 , sep='|' )
	SN_PZ_01_LIQASSETDATE <- xml_text( xml_find_all( nodei, LIQASSETDATE ) )
	
	
	
	
	
	## VARIABLE NAME:  SN_PZ_01_LIQASSETFMV
	## DESCRIPTION:  Fair market value of asset(s) distributed or amount of transactional expenses (Liquidated assets)
	## LOCATION:  SCHED-N-PART-01-LINE-1C
	## TABLE:  TABLE_01
	
	## REQUIRED (Y/N):  NA
	
	V1 <- 'FairMarketValueOfAssetAmt'
	V2 <- 'FMVOfAsset'
	LIQASSETFMV <- paste( V1, V2 , sep='|' )
	SN_PZ_01_LIQASSETFMV <- xml_text( xml_find_all( nodei, LIQASSETFMV ) )
	
	
	
	
	
	## VARIABLE NAME:  SN_PZ_01_LIQASSETACCTMETHOD
	## DESCRIPTION:  Method of determining FMV for asset(s) distributed or transactional expenses (Liquidated assets)
	## LOCATION:  SCHED-N-PART-01-LINE-1D
	## TABLE:  TABLE_01
	
	## REQUIRED (Y/N):  NA
	
	V1 <- 'MethodOfFMVDeterminationTxt'
	V2 <- 'MethodOfFMVDetermination'
	LIQASSETACCTMETHOD <- paste( V1, V2 , sep='|' )
	SN_PZ_01_LIQASSETACCTMETHOD <- xml_text( xml_find_all( nodei, LIQASSETACCTMETHOD ) )
	
	
	
	
	
	## VARIABLE NAME:  SN_PZ_01_LIQASSETRECIPIENTEIN
	## DESCRIPTION:  EIN of recipient (Liquidated assets; if tax-exempt)
	## LOCATION:  SCHED-N-PART-01-LINE-1E
	## TABLE:  TABLE_01
	
	## REQUIRED (Y/N):  NA
	
	V1 <- 'EIN'
	V2 <- 'EIN'
	LIQASSETRECIPIENTEIN <- paste( V1, V2 , sep='|' )
	SN_PZ_01_LIQASSETRECIPIENTEIN <- xml_text( xml_find_all( nodei, LIQASSETRECIPIENTEIN ) )
	
	
	
	
	
	## VARIABLE NAME:  SN_PZ_01_LIQASSETRECIPIENTNAME
	## DESCRIPTION:  Name of recipient (Liquidated assets)
	## LOCATION:  SCHED-N-PART-01-LINE-1F
	## TABLE:  TABLE_01
	
	## REQUIRED (Y/N):  NA
	
	V1 <- 'PersonNm'
	V2 <- 'NamePerson'
	LIQASSETRECIPIENTNAME <- paste( V1, V2 , sep='|' )
	SN_PZ_01_LIQASSETRECIPIENTNAME <- xml_text( xml_find_all( nodei, LIQASSETRECIPIENTNAME ) )
	
	
	
	
	
	## VARIABLE NAME:  SN_PZ_01_LIQASSETRECIPIENTBUSNAME1
	## DESCRIPTION:  Business name of recipient of liquidated assets (Line 1)
	## LOCATION:  SCHED-N-PART-01-LINE-1F
	## TABLE:  TABLE_01
	
	## REQUIRED (Y/N):  NA
	
	V1 <- 'BusinessName/BusinessNameLine1'
	V2 <- 'BusinessName/BusinessNameLine1Txt'
	V3 <- 'NameBusiness/BusinessNameLine1'
	LIQASSETRECIPIENTBUSNAME1 <- paste( V1, V2, V3 , sep='|' )
	SN_PZ_01_LIQASSETRECIPIENTBUSNAME1 <- xml_text( xml_find_all( nodei, LIQASSETRECIPIENTBUSNAME1 ) )
	
	
	
	
	
	## VARIABLE NAME:  SN_PZ_01_LIQASSETRECIPIENTBUSNAME2
	## DESCRIPTION:  Business name of recipient of liquidated assets (Line 2)
	## LOCATION:  SCHED-N-PART-01-LINE-1F
	## TABLE:  TABLE_01
	
	## REQUIRED (Y/N):  NA
	
	V1 <- 'BusinessName/BusinessNameLine2'
	V2 <- 'BusinessName/BusinessNameLine2Txt'
	V3 <- 'NameBusiness/BusinessNameLine2'
	LIQASSETRECIPIENTBUSNAME2 <- paste( V1, V2, V3 , sep='|' )
	SN_PZ_01_LIQASSETRECIPIENTBUSNAME2 <- xml_text( xml_find_all( nodei, LIQASSETRECIPIENTBUSNAME2 ) )
	
	
	
	
	
	## VARIABLE NAME:  SN_PZ_01_LIQASSETRECIPIENTFORADDRESS1
	## DESCRIPTION:  Address of recipient of liquidated assets (Foreign address)
	## LOCATION:  SCHED-N-PART-01-LINE-1F
	## TABLE:  TABLE_01
	
	## REQUIRED (Y/N):  NA
	
	V1 <- 'ForeignAddress/AddressLine1'
	V2 <- 'ForeignAddress/AddressLine1Txt'
	V3 <- 'AddressForeign/AddressLine1'
	LIQASSETRECIPIENTFORADDRESS1 <- paste( V1, V2, V3 , sep='|' )
	SN_PZ_01_LIQASSETRECIPIENTFORADDRESS1 <- xml_text( xml_find_all( nodei, LIQASSETRECIPIENTFORADDRESS1 ) )
	
	
	
	
	
	## VARIABLE NAME:  SN_PZ_01_LIQASSETRECIPIENTFORCITY
	## DESCRIPTION:  City of recipient of liquidated assets (Foreign address)
	## LOCATION:  SCHED-N-PART-01-LINE-1F
	## TABLE:  TABLE_01
	
	## REQUIRED (Y/N):  NA
	
	V1 <- 'ForeignAddress/City'
	V2 <- 'ForeignAddress/CityNm'
	V3 <- 'AddressForeign/City'
	LIQASSETRECIPIENTFORCITY <- paste( V1, V2, V3 , sep='|' )
	SN_PZ_01_LIQASSETRECIPIENTFORCITY <- xml_text( xml_find_all( nodei, LIQASSETRECIPIENTFORCITY ) )
	
	
	
	
	
	## VARIABLE NAME:  SN_PZ_01_LIQASSETRECIPIENTFORCTRY
	## DESCRIPTION:  Country of recipient of liquidated assets (Foreign address)
	## LOCATION:  SCHED-N-PART-01-LINE-1F
	## TABLE:  TABLE_01
	
	## REQUIRED (Y/N):  NA
	
	V1 <- 'ForeignAddress/Country'
	V2 <- 'ForeignAddress/CountryCd'
	V3 <- 'AddressForeign/Country'
	LIQASSETRECIPIENTFORCTRY <- paste( V1, V2, V3 , sep='|' )
	SN_PZ_01_LIQASSETRECIPIENTFORCTRY <- xml_text( xml_find_all( nodei, LIQASSETRECIPIENTFORCTRY ) )
	
	
	
	
	
	## VARIABLE NAME:  SN_PZ_01_LIQASSETRECIPIENTFORPOSTCODE
	## DESCRIPTION:  Postal code of recipient of liquidated assets (Foreign address)
	## LOCATION:  SCHED-N-PART-01-LINE-1F
	## TABLE:  TABLE_01
	
	## REQUIRED (Y/N):  NA
	
	V1 <- 'ForeignAddress/ForeignPostalCd'
	V2 <- 'ForeignAddress/PostalCode'
	V3 <- 'AddressForeign/PostalCode'
	LIQASSETRECIPIENTFORPOSTCODE <- paste( V1, V2, V3 , sep='|' )
	SN_PZ_01_LIQASSETRECIPIENTFORPOSTCODE <- xml_text( xml_find_all( nodei, LIQASSETRECIPIENTFORPOSTCODE ) )
	
	
	
	
	
	## VARIABLE NAME:  SN_PZ_01_LIQASSETRECIPIENTFORSTATE
	## DESCRIPTION:  Province or state of recipient of liquidated assets (Foreign address)
	## LOCATION:  SCHED-N-PART-01-LINE-1F
	## TABLE:  TABLE_01
	
	## REQUIRED (Y/N):  NA
	
	V1 <- 'ForeignAddress/ProvinceOrState'
	V2 <- 'ForeignAddress/ProvinceOrStateNm'
	V3 <- 'AddressForeign/ProvinceOrState'
	LIQASSETRECIPIENTFORSTATE <- paste( V1, V2, V3 , sep='|' )
	SN_PZ_01_LIQASSETRECIPIENTFORSTATE <- xml_text( xml_find_all( nodei, LIQASSETRECIPIENTFORSTATE ) )
	
	
	
	
	
	## VARIABLE NAME:  SN_PZ_01_LIQASSETRECIPIENTUSADDRESS1
	## DESCRIPTION:  Address of recipient of liquidated assets (US Address; Line 1)
	## LOCATION:  SCHED-N-PART-01-LINE-1F
	## TABLE:  TABLE_01
	
	## REQUIRED (Y/N):  NA
	
	V1 <- 'USAddress/AddressLine1'
	V2 <- 'USAddress/AddressLine1Txt'
	V3 <- 'AddressUS/AddressLine1'
	LIQASSETRECIPIENTUSADDRESS1 <- paste( V1, V2, V3 , sep='|' )
	SN_PZ_01_LIQASSETRECIPIENTUSADDRESS1 <- xml_text( xml_find_all( nodei, LIQASSETRECIPIENTUSADDRESS1 ) )
	
	
	
	
	
	## VARIABLE NAME:  SN_PZ_01_LIQASSETRECIPIENTUSADDRESS2
	## DESCRIPTION:  Address of recipient of liquidated assets (US Address; Line 2)
	## LOCATION:  SCHED-N-PART-01-LINE-1F
	## TABLE:  TABLE_01
	
	## REQUIRED (Y/N):  NA
	
	V1 <- 'USAddress/AddressLine2'
	V2 <- 'USAddress/AddressLine2Txt'
	V3 <- 'AddressUS/AddressLine2'
	LIQASSETRECIPIENTUSADDRESS2 <- paste( V1, V2, V3 , sep='|' )
	SN_PZ_01_LIQASSETRECIPIENTUSADDRESS2 <- xml_text( xml_find_all( nodei, LIQASSETRECIPIENTUSADDRESS2 ) )
	
	
	
	
	
	## VARIABLE NAME:  SN_PZ_01_LIQASSETRECIPIENTUSCITY
	## DESCRIPTION:  City of recipient of liquidated assets (US Address)
	## LOCATION:  SCHED-N-PART-01-LINE-1F
	## TABLE:  TABLE_01
	
	## REQUIRED (Y/N):  NA
	
	V1 <- 'USAddress/City'
	V2 <- 'USAddress/CityNm'
	V3 <- 'AddressUS/City'
	LIQASSETRECIPIENTUSCITY <- paste( V1, V2, V3 , sep='|' )
	SN_PZ_01_LIQASSETRECIPIENTUSCITY <- xml_text( xml_find_all( nodei, LIQASSETRECIPIENTUSCITY ) )
	
	
	
	
	
	## VARIABLE NAME:  SN_PZ_01_LIQASSETRECIPIENTUSSTATE
	## DESCRIPTION:  State of recipient of liquidated assets (US Address)
	## LOCATION:  SCHED-N-PART-01-LINE-1F
	## TABLE:  TABLE_01
	
	## REQUIRED (Y/N):  NA
	
	V1 <- 'USAddress/State'
	V2 <- 'USAddress/StateAbbreviationCd'
	V3 <- 'AddressUS/State'
	LIQASSETRECIPIENTUSSTATE <- paste( V1, V2, V3 , sep='|' )
	SN_PZ_01_LIQASSETRECIPIENTUSSTATE <- xml_text( xml_find_all( nodei, LIQASSETRECIPIENTUSSTATE ) )
	
	
	
	
	
	## VARIABLE NAME:  SN_PZ_01_LIQASSETRECIPIENTUSZIP
	## DESCRIPTION:  Zip code of recipient of liquidated assets (US Address)
	## LOCATION:  SCHED-N-PART-01-LINE-1F
	## TABLE:  TABLE_01
	
	## REQUIRED (Y/N):  NA
	
	V1 <- 'USAddress/ZIPCd'
	V2 <- 'USAddress/ZIPCode'
	V3 <- 'AddressUS/ZIPCode'
	LIQASSETRECIPIENTUSZIP <- paste( V1, V2, V3 , sep='|' )
	SN_PZ_01_LIQASSETRECIPIENTUSZIP <- xml_text( xml_find_all( nodei, LIQASSETRECIPIENTUSZIP ) )
	
	
	
	
	
	## VARIABLE NAME:  SN_PZ_01_LIQASSETRECIPIENTFORADDRESS2
	## DESCRIPTION:  Zip code of recipient of liquidated assets (Foreign Address; Line 2)
	## LOCATION:  SCHED-N-PART-01-LINE-1F
	## TABLE:  TABLE_01
	
	## REQUIRED (Y/N):  NA
	
	SN_PZ_01_LIQASSETRECIPIENTFORADDRESS2 <- xml_text( xml_find_all( nodei, '/Return/ReturnData/IRS990ScheduleN/LiquidationTable/LiquidationDetail/AddressForeign/AddressLine2' ) )
	
	
	
	
	
	## VARIABLE NAME:  SN_PZ_01_LIQASSETRECIPIENTIRC
	## DESCRIPTION:  IRC code section of recipient(s) (if tax-exempt) or type of entity (Liquidated assets)
	## LOCATION:  SCHED-N-PART-01-LINE-1G
	## TABLE:  TABLE_01
	
	## REQUIRED (Y/N):  NA
	
	V1 <- 'IRCSectionTxt'
	V2 <- 'IRCSection'
	LIQASSETRECIPIENTIRC <- paste( V1, V2 , sep='|' )
	SN_PZ_01_LIQASSETRECIPIENTIRC <- xml_text( xml_find_all( nodei, LIQASSETRECIPIENTIRC ) )







	###  BIND VARIABLES TOGETHER


	var.list <- namedList( # HEADER
	# var.list <- data.frame( # HEADER 
			       EIN, NAME, FORMTYPE, FISYR, ORGDISSOLVED, DISCOPS, TERMINATED, SCHEDN,
			       # BASIC INFO
			       GROSSRECEIPTS, GROUPRETURN, GROUPEXEMPTNUM, URL,			       
			       # TABLE 01
			       	SN_PZ_01_LIQASSETDESCRIP,
			       	SN_PZ_01_LIQASSETDATE,
			       	SN_PZ_01_LIQASSETFMV,
			       	SN_PZ_01_LIQASSETACCTMETHOD,
			       	SN_PZ_01_LIQASSETRECIPIENTEIN,
			       	SN_PZ_01_LIQASSETRECIPIENTNAME,
			       	SN_PZ_01_LIQASSETRECIPIENTBUSNAME1,
			       	SN_PZ_01_LIQASSETRECIPIENTBUSNAME2,
			       	SN_PZ_01_LIQASSETRECIPIENTFORADDRESS1,
			       	SN_PZ_01_LIQASSETRECIPIENTFORCITY,
			       	SN_PZ_01_LIQASSETRECIPIENTFORCTRY,
			       	SN_PZ_01_LIQASSETRECIPIENTFORPOSTCODE,
			       	SN_PZ_01_LIQASSETRECIPIENTFORSTATE,
			       	SN_PZ_01_LIQASSETRECIPIENTUSADDRESS1,
			       	SN_PZ_01_LIQASSETRECIPIENTUSADDRESS2,
			       	SN_PZ_01_LIQASSETRECIPIENTUSCITY,
			       	SN_PZ_01_LIQASSETRECIPIENTUSSTATE,
			       	SN_PZ_01_LIQASSETRECIPIENTUSZIP,
			       	SN_PZ_01_LIQASSETRECIPIENTFORADDRESS2,
			       	SN_PZ_01_LIQASSETRECIPIENTIRC
			      )

        
	     one.npo <- as.data.frame( var.list, stringsAsFactors=F )
	     
	     if( ! is.null(one.npo) )
	     {
	       schedNtable.01 <- bind_rows( schedNtable.01, one.npo )
             }
     
     
        }  # end of Table 01 for loop
        
        }  # end of if nd > 0 statement
        # if no data, then table1 <- NULL






        
     
	#------------------------------------------------------------------------------------------------------------------------

	###############     TABLE 02 - DISPOSITION OF ASSETS - 1:M WITH EIN    #########################
	
	#------------------------------------------------------------------------------------------------------------------------




	nd <- NULL
	nd <- xml_find_all( doc, "//DispositionOfAssetsDetail|//DispositionTable" )
	
	one.npo <- NULL
	
	if( length(nd) > 0 )
        {
	
	for( i in 1:length(nd) )
	{
	
	
	nodei <- nd[[i]]
	
	
	
	
	## VARIABLE NAME:  SN_PZ_02_DISPASSETDESCRIP
	## DESCRIPTION:  Description of asset(s) distributed or transactional expenses paid (Disposed assets)
	## LOCATION:  SCHED-N-PART-02-LINE-1A
	## TABLE:  TABLE_02
	
	## REQUIRED (Y/N):  NA
	
	V1 <- 'AssetsDistriOrExpnssPaidDesc'
	V2 <- 'DescriptionOfAsset'
	DISPASSETDESCRIP <- paste( V1, V2 , sep='|' )
	SN_PZ_02_DISPASSETDESCRIP <- xml_text( xml_find_all( nodei, DISPASSETDESCRIP ) )
	
	
	
	
	
	## VARIABLE NAME:  SN_PZ_02_DISPASSETDATE
	## DESCRIPTION:  Date of distribution (Disposed assets)
	## LOCATION:  SCHED-N-PART-02-LINE-1B
	## TABLE:  TABLE_02
	
	## REQUIRED (Y/N):  NA
	
	V1 <- 'DistributionDt'
	V2 <- 'DateOfDistribution'
	DISPASSETDATE <- paste( V1, V2 , sep='|' )
	SN_PZ_02_DISPASSETDATE <- xml_text( xml_find_all( nodei, DISPASSETDATE ) )
	
	
	
	
	
	## VARIABLE NAME:  SN_PZ_02_DISPASSETFMV
	## DESCRIPTION:  Fair market value of asset(s) distributed or amount of transactional expenses (Disposed assets)
	## LOCATION:  SCHED-N-PART-02-LINE-1C
	## TABLE:  TABLE_02
	
	## REQUIRED (Y/N):  NA
	
	V1 <- 'FairMarketValueOfAssetAmt'
	V2 <- 'FMVOfAsset'
	DISPASSETFMV <- paste( V1, V2 , sep='|' )
	SN_PZ_02_DISPASSETFMV <- xml_text( xml_find_all( nodei, DISPASSETFMV ) )
	
	
	
	
	
	## VARIABLE NAME:  SN_PZ_02_DISPASSETACCTMETHOD
	## DESCRIPTION:  Method of determining FMV for asset(s) distributed or transactional expenses (Disposed assets)
	## LOCATION:  SCHED-N-PART-02-LINE-1D
	## TABLE:  TABLE_02
	
	## REQUIRED (Y/N):  NA
	
	V1 <- 'MethodOfFMVDeterminationTxt'
	V2 <- 'MethodOfFMVDetermination'
	DISPASSETACCTMETHOD <- paste( V1, V2 , sep='|' )
	SN_PZ_02_DISPASSETACCTMETHOD <- xml_text( xml_find_all( nodei, DISPASSETACCTMETHOD ) )
	
	
	
	
	
	## VARIABLE NAME:  SN_PZ_02_DISPASSETRECIPIENTEIN
	## DESCRIPTION:  EIN of recipient (Disposed assets; if tax-exempt)
	## LOCATION:  SCHED-N-PART-02-LINE-1E
	## TABLE:  TABLE_02
	
	## REQUIRED (Y/N):  NA
	
	V1 <- 'EIN'
	V2 <- 'EIN'
	DISPASSETRECIPIENTEIN <- paste( V1, V2 , sep='|' )
	SN_PZ_02_DISPASSETRECIPIENTEIN <- xml_text( xml_find_all( nodei, DISPASSETRECIPIENTEIN ) )
	
	
	
	
	
	## VARIABLE NAME:  SN_PZ_02_DISPASSETRECIPIENTNAME
	## DESCRIPTION:  Name of recipient (Disposed assets)
	## LOCATION:  SCHED-N-PART-02-LINE-1F
	## TABLE:  TABLE_02
	
	## REQUIRED (Y/N):  NA
	
	V1 <- 'PersonNm'
	V2 <- 'NamePerson'
	DISPASSETRECIPIENTNAME <- paste( V1, V2 , sep='|' )
	SN_PZ_02_DISPASSETRECIPIENTNAME <- xml_text( xml_find_all( nodei, DISPASSETRECIPIENTNAME ) )
	
	
	
	
	
	## VARIABLE NAME:  SN_PZ_02_DISPASSETRECIPIENTBUSNAME1
	## DESCRIPTION:  Business name of recipient of disposed assets (Line 1)
	## LOCATION:  SCHED-N-PART-02-LINE-1F
	## TABLE:  TABLE_02
	
	## REQUIRED (Y/N):  NA
	
	V1 <- 'BusinessName/BusinessNameLine1'
	V2 <- 'BusinessName/BusinessNameLine1Txt'
	V3 <- 'NameBusiness/BusinessNameLine1'
	DISPASSETRECIPIENTBUSNAME1 <- paste( V1, V2, V3 , sep='|' )
	SN_PZ_02_DISPASSETRECIPIENTBUSNAME1 <- xml_text( xml_find_all( nodei, DISPASSETRECIPIENTBUSNAME1 ) )
	
	
	
	
	
	## VARIABLE NAME:  SN_PZ_02_DISPASSETRECIPIENTBUSNAME2
	## DESCRIPTION:  Business name of recipient of disposed assets (Line 2)
	## LOCATION:  SCHED-N-PART-02-LINE-1F
	## TABLE:  TABLE_02
	
	## REQUIRED (Y/N):  NA
	
	V1 <- 'BusinessName/BusinessNameLine2'
	V2 <- 'BusinessName/BusinessNameLine2Txt'
	V3 <- 'NameBusiness/BusinessNameLine2'
	DISPASSETRECIPIENTBUSNAME2 <- paste( V1, V2, V3 , sep='|' )
	SN_PZ_02_DISPASSETRECIPIENTBUSNAME2 <- xml_text( xml_find_all( nodei, DISPASSETRECIPIENTBUSNAME2 ) )
	
	
	
	
	
	## VARIABLE NAME:  SN_PZ_02_DISPASSETRECIPIENTFORADDRESS1
	## DESCRIPTION:  Address of recipient of disposed assets (Foreign Address; Line 1)
	## LOCATION:  SCHED-N-PART-02-LINE-1F
	## TABLE:  TABLE_02
	
	## REQUIRED (Y/N):  NA
	
	V1 <- 'ForeignAddress/AddressLine1'
	V2 <- 'ForeignAddress/AddressLine1Txt'
	V3 <- 'AddressForeign/AddressLine1'
	DISPASSETRECIPIENTFORADDRESS1 <- paste( V1, V2, V3 , sep='|' )
	SN_PZ_02_DISPASSETRECIPIENTFORADDRESS1 <- xml_text( xml_find_all( nodei, DISPASSETRECIPIENTFORADDRESS1 ) )
	
	
	
	
	
	## VARIABLE NAME:  SN_PZ_02_DISPASSETRECIPIENTFORADDRESS2
	## DESCRIPTION:  Address of recipient of disposed assets (Foreign address; Line 2)
	## LOCATION:  SCHED-N-PART-02-LINE-1F
	## TABLE:  TABLE_02
	
	## REQUIRED (Y/N):  NA
	
	V1 <- 'ForeignAddress/AddressLine2'
	V2 <- 'AddressForeign/AddressLine2'
	DISPASSETRECIPIENTFORADDRESS2 <- paste( V1, V2 , sep='|' )
	SN_PZ_02_DISPASSETRECIPIENTFORADDRESS2 <- xml_text( xml_find_all( nodei, DISPASSETRECIPIENTFORADDRESS2 ) )
	
	
	
	
	
	## VARIABLE NAME:  SN_PZ_02_DISPASSETRECIPIENTFORCITY
	## DESCRIPTION:  City of recipient of disposed assets (Foreign Address)
	## LOCATION:  SCHED-N-PART-02-LINE-1F
	## TABLE:  TABLE_02
	
	## REQUIRED (Y/N):  NA
	
	V1 <- 'ForeignAddress/City'
	V2 <- 'ForeignAddress/CityNm'
	V3 <- 'AddressForeign/City'
	DISPASSETRECIPIENTFORCITY <- paste( V1, V2, V3 , sep='|' )
	SN_PZ_02_DISPASSETRECIPIENTFORCITY <- xml_text( xml_find_all( nodei, DISPASSETRECIPIENTFORCITY ) )
	
	
	
	
	
	## VARIABLE NAME:  SN_PZ_02_DISPASSETRECIPIENTFORCTRY
	## DESCRIPTION:  Country of recipient of disposed assets (Foreign Address)
	## LOCATION:  SCHED-N-PART-02-LINE-1F
	## TABLE:  TABLE_02
	
	## REQUIRED (Y/N):  NA
	
	V1 <- 'ForeignAddress/Country'
	V2 <- 'ForeignAddress/CountryCd'
	V3 <- 'AddressForeign/Country'
	DISPASSETRECIPIENTFORCTRY <- paste( V1, V2, V3 , sep='|' )
	SN_PZ_02_DISPASSETRECIPIENTFORCTRY <- xml_text( xml_find_all( nodei, DISPASSETRECIPIENTFORCTRY ) )
	
	
	
	
	
	## VARIABLE NAME:  SN_PZ_02_DISPASSETRECIPIENTFORPOSTCODE
	## DESCRIPTION:  Postal code of recipient of disposed assets (Foreign Address)
	## LOCATION:  SCHED-N-PART-02-LINE-1F
	## TABLE:  TABLE_02
	
	## REQUIRED (Y/N):  NA
	
	V1 <- 'ForeignAddress/ForeignPostalCd'
	V2 <- 'ForeignAddress/PostalCode'
	V3 <- 'AddressForeign/PostalCode'
	DISPASSETRECIPIENTFORPOSTCODE <- paste( V1, V2, V3 , sep='|' )
	SN_PZ_02_DISPASSETRECIPIENTFORPOSTCODE <- xml_text( xml_find_all( nodei, DISPASSETRECIPIENTFORPOSTCODE ) )
	
	
	
	
	
	## VARIABLE NAME:  SN_PZ_02_DISPASSETRECIPIENTFORSTATE
	## DESCRIPTION:  Province or state of recipient of disposed assets (Foreign address)
	## LOCATION:  SCHED-N-PART-02-LINE-1F
	## TABLE:  TABLE_02
	
	## REQUIRED (Y/N):  NA
	
	V1 <- 'ForeignAddress/ProvinceOrState'
	V2 <- 'ForeignAddress/ProvinceOrStateNm'
	V3 <- 'AddressForeign/ProvinceOrState'
	DISPASSETRECIPIENTFORSTATE <- paste( V1, V2, V3 , sep='|' )
	SN_PZ_02_DISPASSETRECIPIENTFORSTATE <- xml_text( xml_find_all( nodei, DISPASSETRECIPIENTFORSTATE ) )
	
	
	
	
	
	## VARIABLE NAME:  SN_PZ_02_DISPASSETRECIPIENTUSADDRESS1
	## DESCRIPTION:  Address of recipient of disposed assets (US Address; Line 1)
	## LOCATION:  SCHED-N-PART-02-LINE-1F
	## TABLE:  TABLE_02
	
	## REQUIRED (Y/N):  NA
	
	V1 <- 'USAddress/AddressLine1'
	V2 <- 'USAddress/AddressLine1Txt'
	V3 <- 'AddressUS/AddressLine1'
	DISPASSETRECIPIENTUSADDRESS1 <- paste( V1, V2, V3 , sep='|' )
	SN_PZ_02_DISPASSETRECIPIENTUSADDRESS1 <- xml_text( xml_find_all( nodei, DISPASSETRECIPIENTUSADDRESS1 ) )
	
	
	
	
	
	## VARIABLE NAME:  SN_PZ_02_DISPASSETRECIPIENTUSADDRESS2
	## DESCRIPTION:  Address of recipient of disposed assets (US Address; Line 2)
	## LOCATION:  SCHED-N-PART-02-LINE-1F
	## TABLE:  TABLE_02
	
	## REQUIRED (Y/N):  NA
	
	V1 <- 'USAddress/AddressLine2'
	V2 <- 'USAddress/AddressLine2Txt'
	V3 <- 'AddressUS/AddressLine2'
	DISPASSETRECIPIENTUSADDRESS2 <- paste( V1, V2, V3 , sep='|' )
	SN_PZ_02_DISPASSETRECIPIENTUSADDRESS2 <- xml_text( xml_find_all( nodei, DISPASSETRECIPIENTUSADDRESS2 ) )
	
	
	
	
	
	## VARIABLE NAME:  SN_PZ_02_DISPASSETRECIPIENTUSCITY
	## DESCRIPTION:  City of recipient of disposed assets (US Address)
	## LOCATION:  SCHED-N-PART-02-LINE-1F
	## TABLE:  TABLE_02
	
	## REQUIRED (Y/N):  NA
	
	V1 <- 'USAddress/City'
	V2 <- 'USAddress/CityNm'
	V3 <- 'AddressUS/City'
	DISPASSETRECIPIENTUSCITY <- paste( V1, V2, V3 , sep='|' )
	SN_PZ_02_DISPASSETRECIPIENTUSCITY <- xml_text( xml_find_all( nodei, DISPASSETRECIPIENTUSCITY ) )
	
	
	
	
	
	## VARIABLE NAME:  SN_PZ_02_DISPASSETRECIPIENTUSSTATE
	## DESCRIPTION:  State of recipient of disposed assets (US Address)
	## LOCATION:  SCHED-N-PART-02-LINE-1F
	## TABLE:  TABLE_02
	
	## REQUIRED (Y/N):  NA
	
	V1 <- 'USAddress/State'
	V2 <- 'USAddress/StateAbbreviationCd'
	V3 <- 'AddressUS/State'
	DISPASSETRECIPIENTUSSTATE <- paste( V1, V2, V3 , sep='|' )
	SN_PZ_02_DISPASSETRECIPIENTUSSTATE <- xml_text( xml_find_all( nodei, DISPASSETRECIPIENTUSSTATE ) )
	
	
	
	
	
	## VARIABLE NAME:  SN_PZ_02_DISPASSETRECIPIENTUSZIP
	## DESCRIPTION:  Zip code of recipient of disposed assets (US Address)
	## LOCATION:  SCHED-N-PART-02-LINE-1F
	## TABLE:  TABLE_02
	
	## REQUIRED (Y/N):  NA
	
	V1 <- 'USAddress/ZIPCd'
	V2 <- 'USAddress/ZIPCode'
	V3 <- 'AddressUS/ZIPCode'
	DISPASSETRECIPIENTUSZIP <- paste( V1, V2, V3 , sep='|' )
	SN_PZ_02_DISPASSETRECIPIENTUSZIP <- xml_text( xml_find_all( nodei, DISPASSETRECIPIENTUSZIP ) )
	
	
	
	
	
	## VARIABLE NAME:  SN_PZ_02_DISPASSETRECIPIENTIRC
	## DESCRIPTION:  IRC code section of recipient(s) (if tax-exempt) or type of entity (Disposed assets)
	## LOCATION:  SCHED-N-PART-02-LINE-1G
	## TABLE:  TABLE_02
	
	## REQUIRED (Y/N):  NA
	
	V1 <- 'IRCSectionTxt'
	V2 <- 'IRCSection'
	V3 <- 'IRCSection'
	DISPASSETRECIPIENTIRC <- paste( V1, V2, V3 , sep='|' )
	SN_PZ_02_DISPASSETRECIPIENTIRC <- xml_text( xml_find_all( nodei, DISPASSETRECIPIENTIRC ) )




	#------------------------------------------------------------------------------------------------------------------------
	###  BIND VARIABLES TOGETHER - TABLE 02

	var.list <- namedList( # HEADER
			       EIN, NAME, FORMTYPE, FISYR, ORGDISSOLVED, DISCOPS, TERMINATED, SCHEDN,
			       # BASIC INFO
			       GROSSRECEIPTS, GROUPRETURN, GROUPEXEMPTNUM, URL,
			       # SCHEDULE N - TABLE 02
			       SN_PZ_02_DISPASSETDESCRIP,
			       SN_PZ_02_DISPASSETDATE,
			       SN_PZ_02_DISPASSETFMV,
			       SN_PZ_02_DISPASSETACCTMETHOD,
			       SN_PZ_02_DISPASSETRECIPIENTEIN,
			       SN_PZ_02_DISPASSETRECIPIENTNAME,
			       SN_PZ_02_DISPASSETRECIPIENTBUSNAME1,
			       SN_PZ_02_DISPASSETRECIPIENTBUSNAME2,
			       SN_PZ_02_DISPASSETRECIPIENTFORADDRESS1,
			       SN_PZ_02_DISPASSETRECIPIENTFORADDRESS2,
			       SN_PZ_02_DISPASSETRECIPIENTFORCITY,
			       SN_PZ_02_DISPASSETRECIPIENTFORCTRY,
			       SN_PZ_02_DISPASSETRECIPIENTFORPOSTCODE,
			       SN_PZ_02_DISPASSETRECIPIENTFORSTATE,
			       SN_PZ_02_DISPASSETRECIPIENTUSADDRESS1,
			       SN_PZ_02_DISPASSETRECIPIENTUSADDRESS2,
			       SN_PZ_02_DISPASSETRECIPIENTUSCITY,
			       SN_PZ_02_DISPASSETRECIPIENTUSSTATE,
			       SN_PZ_02_DISPASSETRECIPIENTUSZIP,
			       SN_PZ_02_DISPASSETRECIPIENTIRC       
			      )




	     one.npo <- as.data.frame( var.list, stringsAsFactors=F )
	     
	     if( ! is.null(one.npo) )
	     {
	       schedNtable.02 <- bind_rows( schedNtable.02, one.npo )
             }
     
     
        }  # end of Table 02 for loop
        
        }  # end of if nd > 0 statement
        # if no data, then table1 <- NULL



















	#------------------------------------------------------------------------------------------------------------------------

	###############     TABLE 03 - SUPPLEMENTAL INFORMATION - 1:M WITH EIN    #########################
	
	#------------------------------------------------------------------------------------------------------------------------




	

	
	nd <- NULL
	nd <- xml_find_all( doc, "//Form990ScheduleNPartIII|//SupplementalInformationDetail" )
	
	one.npo <- NULL
	
	if( length(nd) > 0 )
        {
	
	for( i in 1:length(nd) )
	{
	
	
	nodei <- nd[[i]]	
	
	## VARIABLE NAME:  SN_PZ_03_SECNSUPPLEMENTALINFO
	## DESCRIPTION:  Additional information regarding current officers/employees taking on roles in successor/transferee organization
	## LOCATION:  SCHED-N-PART-03
	## TABLE:  TABLE_03
	
	## REQUIRED (Y/N):  NA
	
	SN_PZ_03_SECNSUPPLEMENTALINFO <- xml_text( xml_find_all( nodei, 'Explanation' ) )
	
	
	
	
	
	## VARIABLE NAME:  SN_PZ_03_SECNIDENTIFIER
	## DESCRIPTION:  Identifier
	## LOCATION:  SCHED-N-PART-03
	## TABLE:  TABLE_03
	
	## REQUIRED (Y/N):  NA
	
	SN_PZ_03_SECNIDENTIFIER <- xml_text( xml_find_all( nodei, 'Identifier' ) )
	
	
	
	
	
	## VARIABLE NAME:  SN_PZ_03_SECNRETURNREF
	## DESCRIPTION:  Return reference
	## LOCATION:  SCHED-N-PART-03
	## TABLE:  TABLE_03
	
	## REQUIRED (Y/N):  NA
	
	SN_PZ_03_SECNRETURNREF <- xml_text( xml_find_all( nodei, 'ReturnReference' ) )
	
	
	
	
	
	## VARIABLE NAME:  SN_PZ_03_SECNLINEREFEXPLAIN
	## DESCRIPTION:  Form, part and line number reference explanation
	## LOCATION:  SCHED-N-PART-03
	## TABLE:  TABLE_03
	
	## REQUIRED (Y/N):  NA
	
	SN_PZ_03_SECNLINEREFEXPLAIN <- xml_text( xml_find_all( nodei, 'ExplanationTxt' ) )
	
	
	
	
	
	## VARIABLE NAME:  SN_PZ_03_SECNLINEREF
	## DESCRIPTION:  Form, part and line number reference
	## LOCATION:  SCHED-N-PART-03
	## TABLE:  TABLE_03
	
	## REQUIRED (Y/N):  NA
	
	SN_PZ_03_SECNLINEREF <- xml_text( xml_find_all( nodei, 'FormAndLineReferenceDesc' ) )

	





	
	
	

	#------------------------------------------------------------------------------------------------------------------------
	###  BIND VARIABLES TOGETHER


	var.list <- namedList( # HEADER
			       EIN, NAME, FORMTYPE, FISYR, ORGDISSOLVED, DISCOPS, TERMINATED, SCHEDN,
			       # BASIC INFO
			       GROSSRECEIPTS, GROUPRETURN, GROUPEXEMPTNUM, URL,
			       # SCHEDULE N
			       SN_PZ_03_SECNSUPPLEMENTALINFO,
			       SN_PZ_03_SECNIDENTIFIER,
			       SN_PZ_03_SECNRETURNREF,
			       SN_PZ_03_SECNLINEREFEXPLAIN,
			       SN_PZ_03_SECNLINEREF
			      )


	
	     one.npo <- as.data.frame( var.list, stringsAsFactors=F )
	     
	     if( ! is.null(one.npo) )
	     {
	       schedNtable.03 <- bind_rows( schedNtable.03, one.npo )
             }
     
     
        }  # end of Table 03 for loop
        
        }  # end of if nd > 0 statement
        # if no data, then table1 <- NULL
        



        
        } # end of if( SCHEDN )
        
        
	
	dat.990 <- list( core, schedNtable.00, schedNtable.01, schedNtable.02, schedNtable.03 )

        return( dat.990 )


}








# build table for one-to-one fields from 990 part vii


buildPartVII <- function( doc, url )
{

    
	#------------------------------------------------------------------------------------------------------------------------



	## NAME

	V_990NAMEpost2014 <- "//Return/ReturnHeader/Filer/BusinessName/BusinessNameLine1Txt"
	V_990NAME_2013 <- "//Return/ReturnHeader/Filer/BusinessName/BusinessNameLine1"
	V_990NAMEpre2013  <- "//Return/ReturnHeader/Filer/Name/BusinessNameLine1"
	name.xpath <- paste( V_990NAME_2013, V_990NAMEpre2013, V_990NAMEpost2014, sep="|" )
	NAME <- xml_text( xml_find_all( doc, name.xpath ) )


	## EIN

	EIN  <- xml_text( xml_find_all( doc, "//Return/ReturnHeader/Filer/EIN" ) )


	## TAX YEAR

	V_990FYRpost2013 <- "//Return/ReturnHeader/TaxYr"
	V_990FYRpre2013  <- "//Return/ReturnHeader/TaxYear"
	tax.year.xpath <- paste( V_990FYRpost2013, V_990FYRpre2013, sep="|" )
	TAXYR <- xml_text( xml_find_all( doc, tax.year.xpath ) )

	## TYPE OF TAX FORM

	V_990TFpost2013 <- "//Return/ReturnHeader/ReturnTypeCd"
	V_990TFpre2013  <- "//Return/ReturnHeader/ReturnType"
	tax.form.xpath <- paste( V_990TFpost2013, V_990TFpre2013, sep="|" )
	FORMTYPE <- xml_text( xml_find_all( doc, tax.form.xpath ) )
	
	
	## OBJECT ID

	OBJECTID <- get_object_id( url )


	## URL

	URL <- url


	#------------------------------------------------------------------------------------------------------------------------





	## VARIABLE NAME:  F9_07_PC_COMP_GT_150K_CHECKBOX
	## DESCRIPTION:  Line1a; total greater than $150K?
	## LOCATION:  F990-PC-PART-07-SECTION-A-LINE-4
	## TABLE:  F9-P07-TABLE-00-COMP-OVERVIEW
	## VARIABLE TYPE:  boolean

	V1 <- '//Return/ReturnData/IRS990/Form990PartVIISectionA/Line1ATotalGT150K'
	V2 <- '//Return/ReturnData/IRS990/TotalCompGreaterThan150KInd'
	V3 <- '//Return/ReturnData/IRS990/TotalCompGT150K'
	COMP_GT_150K_CHECKBOX <- paste( V1, V2, V3 , sep='|' )
	F9_07_PC_COMP_GT_150K_CHECKBOX <- xml_text( xml_find_all( doc, COMP_GT_150K_CHECKBOX ) )





	## VARIABLE NAME:  F9_07_PC_COMP_SUBTOT_DIRECT
	## DESCRIPTION:  Total; column D
	## LOCATION:  F990-PC-PART-07-SECTION-A-LINE-1b-COL-D
	## TABLE:  F9-P07-TABLE-00-COMP-OVERVIEW
	## VARIABLE TYPE:  numeric

	F9_07_PC_COMP_SUBTOT_DIRECT <- xml_text( xml_find_all( doc, '/Return/ReturnData/IRS990/Form990PartVIISectionA/TotalColumnD' ) )





	## VARIABLE NAME:  F9_07_PC_COMP_SUBTOT_OTHER
	## DESCRIPTION:  Total; column F
	## LOCATION:  F990-PC-PART-07-SECTION-A-LINE-1b-COL-F
	## TABLE:  F9-P07-TABLE-00-COMP-OVERVIEW
	## VARIABLE TYPE:  numeric

	F9_07_PC_COMP_SUBTOT_OTHER <- xml_text( xml_find_all( doc, '/Return/ReturnData/IRS990/Form990PartVIISectionA/TotalColumnF' ) )





	## VARIABLE NAME:  F9_07_PC_COMP_SUBTOT_RELATED
	## DESCRIPTION:  Total; column E
	## LOCATION:  F990-PC-PART-07-SECTION-A-LINE-1b-COL-E
	## TABLE:  F9-P07-TABLE-00-COMP-OVERVIEW
	## VARIABLE TYPE:  numeric

	F9_07_PC_COMP_SUBTOT_RELATED <- xml_text( xml_find_all( doc, '/Return/ReturnData/IRS990/Form990PartVIISectionA/TotalColumnE' ) )





	## VARIABLE NAME:  F9_07_PC_FORMER_CHECKBOX
	## DESCRIPTION:  Is a former officer, director, or trustee listed in the compensation table in Sectin A Line 1a? 
	## LOCATION:  F990-PC-PART-07-SECTION-A-LINE-3
	## TABLE:  F9-P07-TABLE-00-COMP-OVERVIEW
	## VARIABLE TYPE:  boolean

	V1 <- '//Return/ReturnData/IRS990/Form990PartVIISectionA/FormersListedIn1A'
	V2 <- '//Return/ReturnData/IRS990/FormerOfcrEmployeesListedInd'
	V3 <- '//Return/ReturnData/IRS990/FormersListed'
	FORMER_CHECKBOX <- paste( V1, V2, V3 , sep='|' )
	F9_07_PC_FORMER_CHECKBOX <- xml_text( xml_find_all( doc, FORMER_CHECKBOX ) )





	## VARIABLE NAME:  F9_07_PC_NO_PERSON_COMPENSATED
	## DESCRIPTION:  No listed persons compensated
	## LOCATION:  F990-PC-PART-07_SECTION-A-LINE-1a
	## TABLE:  F9-P07-TABLE-00-COMP-OVERVIEW
	## VARIABLE TYPE:  boolean

	V1 <- '//Return/ReturnData/IRS990/Form990PartVIISectionA/NoListedPersonsCompensated'
	V2 <- '//Return/ReturnData/IRS990/NoListedPersonsCompensated'
	V3 <- '//Return/ReturnData/IRS990/NoListedPersonsCompensatedInd'
	NO_PERSON_COMPENSATED <- paste( V1, V2, V3 , sep='|' )
	F9_07_PC_NO_PERSON_COMPENSATED <- xml_text( xml_find_all( doc, NO_PERSON_COMPENSATED ) )





	## VARIABLE NAME:  F9_07_PC_NUM_EMP_COMP_GT_100K
	## DESCRIPTION:  Number individuals greater than $100K
	## LOCATION:  F990-PC-PART-07-SECTION-A-LINE-2
	## TABLE:  F9-P07-TABLE-00-COMP-OVERVIEW
	## VARIABLE TYPE:  numeric

	V1 <- '//Return/ReturnData/IRS990/IndivRcvdGreaterThan100KCnt'
	V2 <- '//Return/ReturnData/IRS990/NumberIndividualsGT100K'
	NUM_EMP_COMP_GT_100K <- paste( V1, V2 , sep='|' )
	F9_07_PC_NUM_EMP_COMP_GT_100K <- xml_text( xml_find_all( doc, NUM_EMP_COMP_GT_100K ) )





	## VARIABLE NAME:  F9_07_PC_OTHER_COMP_CHECKBOX
	## DESCRIPTION:  Compensation from other sources?
	## LOCATION:  F990-PC-PART-07-SECTION-A-LINE-5
	## TABLE:  F9-P07-TABLE-00-COMP-OVERVIEW
	## VARIABLE TYPE:  boolean

	V1 <- '//Return/ReturnData/IRS990/CompensationFromOtherSources'
	V2 <- '//Return/ReturnData/IRS990/CompensationFromOtherSrcsInd'
	V3 <- '//Return/ReturnData/IRS990/Form990PartVIISectionA/CompensationFromOtherSources'
	OTHER_COMP_CHECKBOX <- paste( V1, V2, V3 , sep='|' )
	F9_07_PC_OTHER_COMP_CHECKBOX <- xml_text( xml_find_all( doc, OTHER_COMP_CHECKBOX ) )





	## VARIABLE NAME:  F9_07_PZ_COMP_TOTAL_DIRECT
	## DESCRIPTION:  Total, column D
	## LOCATION:  F990-PC-PART-07-SECTION-A-LINE-1d-COL-D
	## TABLE:  F9-P07-TABLE-00-COMP-OVERVIEW
	## VARIABLE TYPE:  numeric

	V1 <- '//Return/ReturnData/IRS990/TotalReportableCompFromOrg'
	V2 <- '//Return/ReturnData/IRS990/TotalReportableCompFromOrgAmt'
	COMP_TOTAL_DIRECT <- paste( V1, V2 , sep='|' )
	F9_07_PZ_COMP_TOTAL_DIRECT <- xml_text( xml_find_all( doc, COMP_TOTAL_DIRECT ) )





	## VARIABLE NAME:  F9_07_PZ_COMP_TOTAL_OTHER
	## DESCRIPTION:  Total, column F
	## LOCATION:  F990-PC-PART-07-SECTION-A-LINE-1d-COL-F
	## TABLE:  F9-P07-TABLE-00-COMP-OVERVIEW
	## VARIABLE TYPE:  numeric

	V1 <- '//Return/ReturnData/IRS990/TotalOtherCompensation'
	V2 <- '//Return/ReturnData/IRS990/TotalOtherCompensationAmt'
	COMP_TOTAL_OTHER <- paste( V1, V2 , sep='|' )
	F9_07_PZ_COMP_TOTAL_OTHER <- xml_text( xml_find_all( doc, COMP_TOTAL_OTHER ) )





	## VARIABLE NAME:  F9_07_PZ_COMP_TOTAL_RELATED
	## DESCRIPTION:  Total, column E
	## LOCATION:  F990-PC-PART-07-SECTION-A-LINE-1d-COL-E
	## TABLE:  F9-P07-TABLE-00-COMP-OVERVIEW
	## VARIABLE TYPE:  numeric

	V1 <- '//Return/ReturnData/IRS990/TotalReportableCompFrmRltdOrgs'
	V2 <- '//Return/ReturnData/IRS990/TotReportableCompRltdOrgAmt'
	COMP_TOTAL_RELATED <- paste( V1, V2 , sep='|' )
	F9_07_PZ_COMP_TOTAL_RELATED <- xml_text( xml_find_all( doc, COMP_TOTAL_RELATED ) )





	## VARIABLE NAME:  F9_07_PZ_NUM_HIGH_PAY_CONTXRS
	## DESCRIPTION:  Total number of independent contractors who received 100K+ from org
	## LOCATION:  F990-PC-PART-07-SECTION-B-LINE-02
	## TABLE:  F9-P07-TABLE-00-COMP-OVERVIEW
	## VARIABLE TYPE:  numeric

	V1 <- '//Return/ReturnData/IRS990/CntrctRcvdGreaterThan100KCnt'
	V2 <- '//Return/ReturnData/IRS990/Form990PartVIISectionA/NumberIndividualsGT100K'
	V3 <- '//Return/ReturnData/IRS990/Form990PartVIISectionB/NumberOfContractorsGT100K'
	V4 <- '//Return/ReturnData/IRS990/NumberOfContractorsGT100K'
	V5 <- '//Return/ReturnData/IRS990EZ/CntrctRcvdGreaterThan100KCnt'
	V6 <- '//Return/ReturnData/IRS990EZ/TotNumCntrctPdOver100KProfSer'
	NUM_HIGH_PAY_CONTXRS <- paste( V1, V2, V3, V4, V5, V6 , sep='|' )
	F9_07_PZ_NUM_HIGH_PAY_CONTXRS <- xml_text( xml_find_all( doc, NUM_HIGH_PAY_CONTXRS ) )


	namedList <- function(...){
		      names <- as.list(substitute(list(...)))[-1L]
		      result <- list(...)
		      names(result) <- names
		      result[sapply(result, function(x){length(x)==0})] <- NA
		      result[sapply(result, is.null)] <- NA
		      result
		  }


	d <- namedList(  NAME, EIN, TAXYR, FORMTYPE, OBJECTID, URL,
			 F9_07_PC_COMP_GT_150K_CHECKBOX, F9_07_PC_COMP_SUBTOT_DIRECT, 
			 F9_07_PC_COMP_SUBTOT_OTHER, F9_07_PC_COMP_SUBTOT_RELATED, 
			 F9_07_PC_FORMER_CHECKBOX, F9_07_PC_NO_PERSON_COMPENSATED, 
			 F9_07_PC_NUM_EMP_COMP_GT_100K, F9_07_PC_OTHER_COMP_CHECKBOX, 
			 F9_07_PZ_COMP_TOTAL_DIRECT, F9_07_PZ_COMP_TOTAL_OTHER, 
			 F9_07_PZ_COMP_TOTAL_RELATED, F9_07_PZ_NUM_HIGH_PAY_CONTXRS )


	d <- as.data.frame( d, stringsAsFactors=F )
	return( d )



}







# build table for one-to-one fields from schedule j

buildSchedJ <- function( doc, url )
{

        
        if( ! grepl( "IRS990ScheduleJ", doc ) ){ return(NULL) }
        
	#------------------------------------------------------------------------------------------------------------------------



	## NAME

	V_990NAMEpost2014 <- "//Return/ReturnHeader/Filer/BusinessName/BusinessNameLine1Txt"
	V_990NAME_2013 <- "//Return/ReturnHeader/Filer/BusinessName/BusinessNameLine1"
	V_990NAMEpre2013  <- "//Return/ReturnHeader/Filer/Name/BusinessNameLine1"
	name.xpath <- paste( V_990NAME_2013, V_990NAMEpre2013, V_990NAMEpost2014, sep="|" )
	NAME <- xml_text( xml_find_all( doc, name.xpath ) )


	## EIN

	EIN  <- xml_text( xml_find_all( doc, "//Return/ReturnHeader/Filer/EIN" ) )


	## TAX YEAR

	V_990FYRpost2013 <- "//Return/ReturnHeader/TaxYr"
	V_990FYRpre2013  <- "//Return/ReturnHeader/TaxYear"
	tax.year.xpath <- paste( V_990FYRpost2013, V_990FYRpre2013, sep="|" )
	TAXYR <- xml_text( xml_find_all( doc, tax.year.xpath ) )

	## TYPE OF TAX FORM

	V_990TFpost2013 <- "//Return/ReturnHeader/ReturnTypeCd"
	V_990TFpre2013  <- "//Return/ReturnHeader/ReturnType"
	tax.form.xpath <- paste( V_990TFpost2013, V_990TFpre2013, sep="|" )
	FORMTYPE <- xml_text( xml_find_all( doc, tax.form.xpath ) )
	
	
	## OBJECT ID

	OBJECTID <- get_object_id( url )


	## URL

	URL <- url


	#------------------------------------------------------------------------------------------------------------------------



	## VARIABLE NAME:  SJ_01_PC_CLUB_FEES
	## DESCRIPTION:  Club dues or fees
	## LOCATION:  SCHED-J-PART-01-LINE-1a
	## TABLE:  SJ-P01-T00-CHECKLIST
	## VARIABLE TYPE:  text

	V1 <- '//Return/ReturnData/IRS990ScheduleJ/ClubDuesOrFees'
	V2 <- '//Return/ReturnData/IRS990ScheduleJ/ClubDuesOrFeesInd'
	V3 <- '//Return/ReturnData/IRS990ScheduleJ/Form990ScheduleJPartI/ClubDuesOrFees'
	CLUB_FEES <- paste( V1, V2, V3 , sep='|' )
	SJ_01_PC_CLUB_FEES <- xml_text( xml_find_all( doc, CLUB_FEES ) )





	## VARIABLE NAME:  SJ_01_PC_COMPANION_TRAVEL
	## DESCRIPTION:  Travel for companions
	## LOCATION:  SCHED-J-PART-01-LINE-1a
	## TABLE:  SJ-P01-T00-CHECKLIST
	## VARIABLE TYPE:  text

	V1 <- '//Return/ReturnData/IRS990ScheduleJ/TravelForCompanions'
	V2 <- '//Return/ReturnData/IRS990ScheduleJ/TravelForCompanionsInd'
	V3 <- '//Return/ReturnData/IRS990ScheduleJ/Form990ScheduleJPartI/TravelForCompanions'
	COMPANION_TRAVEL <- paste( V1, V2, V3 , sep='|' )
	SJ_01_PC_COMPANION_TRAVEL <- xml_text( xml_find_all( doc, COMPANION_TRAVEL ) )





	## VARIABLE NAME:  SJ_01_PC_DISCRETIONARY_ACCOUNT
	## DESCRIPTION:  Discretionary spending account
	## LOCATION:  SCHED-J-PART-01-LINE-1a
	## TABLE:  SJ-P01-T00-CHECKLIST
	## VARIABLE TYPE:  text

	V1 <- '//Return/ReturnData/IRS990ScheduleJ/DiscretionarySpendingAccount'
	V2 <- '//Return/ReturnData/IRS990ScheduleJ/DiscretionarySpendingAcctInd'
	V3 <- '//Return/ReturnData/IRS990ScheduleJ/Form990ScheduleJPartI/DiscretionarySpendingAccount'
	DISCRETIONARY_ACCOUNT <- paste( V1, V2, V3 , sep='|' )
	SJ_01_PC_DISCRETIONARY_ACCOUNT <- xml_text( xml_find_all( doc, DISCRETIONARY_ACCOUNT ) )





	## VARIABLE NAME:  SJ_01_PC_FIRST_CLASS_TRAVEL
	## DESCRIPTION:  First class or charter travel
	## LOCATION:  SCHED-J-PART-01-LINE-1a
	## TABLE:  SJ-P01-T00-CHECKLIST
	## VARIABLE TYPE:  text

	V1 <- '//Return/ReturnData/IRS990ScheduleJ/FirstClassOrCharterTravel'
	V2 <- '//Return/ReturnData/IRS990ScheduleJ/FirstClassOrCharterTravelInd'
	V3 <- '//Return/ReturnData/IRS990ScheduleJ/Form990ScheduleJPartI/FirstClassOrCharterTravel'
	FIRST_CLASS_TRAVEL <- paste( V1, V2, V3 , sep='|' )
	SJ_01_PC_FIRST_CLASS_TRAVEL <- xml_text( xml_find_all( doc, FIRST_CLASS_TRAVEL ) )





	## VARIABLE NAME:  SJ_01_PC_HOME_OFFICE_SUBSIDY
	## DESCRIPTION:  Payments for use of residence
	## LOCATION:  SCHED-J-PART-01-LINE-1a
	## TABLE:  SJ-P01-T00-CHECKLIST
	## VARIABLE TYPE:  text

	V1 <- '//Return/ReturnData/IRS990ScheduleJ/PaymentsForUseOfResidence'
	V2 <- '//Return/ReturnData/IRS990ScheduleJ/PaymentsForUseOfResidenceInd'
	V3 <- '//Return/ReturnData/IRS990ScheduleJ/Form990ScheduleJPartI/PaymentsForUseOfResidence'
	HOME_OFFICE_SUBSIDY <- paste( V1, V2, V3 , sep='|' )
	SJ_01_PC_HOME_OFFICE_SUBSIDY <- xml_text( xml_find_all( doc, HOME_OFFICE_SUBSIDY ) )





	## VARIABLE NAME:  SJ_01_PC_HOUSING_ALLOWANCE
	## DESCRIPTION:  Housing allowance or residence
	## LOCATION:  SCHED-J-PART-01-LINE-1a
	## TABLE:  SJ-P01-T00-CHECKLIST
	## VARIABLE TYPE:  text

	V1 <- '//Return/ReturnData/IRS990ScheduleJ/HousingAllowanceOrResidence'
	V2 <- '//Return/ReturnData/IRS990ScheduleJ/HousingAllowanceOrResidenceInd'
	V3 <- '//Return/ReturnData/IRS990ScheduleJ/Form990ScheduleJPartI/HousingAllowanceOrResidence'
	HOUSING_ALLOWANCE <- paste( V1, V2, V3 , sep='|' )
	SJ_01_PC_HOUSING_ALLOWANCE <- xml_text( xml_find_all( doc, HOUSING_ALLOWANCE ) )





	## VARIABLE NAME:  SJ_01_PC_INDEMNIFICATION
	## DESCRIPTION:  Idemnification and gross-up payments
	## LOCATION:  SCHED-J-PART-01-LINE-1a
	## TABLE:  SJ-P01-T00-CHECKLIST
	## VARIABLE TYPE:  text

	V1 <- '//Return/ReturnData/IRS990ScheduleJ/IdemnificationGrossUpPayments'
	V2 <- '//Return/ReturnData/IRS990ScheduleJ/IdemnificationGrossUpPmtsInd'
	V3 <- '//Return/ReturnData/IRS990ScheduleJ/Form990ScheduleJPartI/IdemnificationGrossUpPayments'
	INDEMNIFICATION <- paste( V1, V2, V3 , sep='|' )
	SJ_01_PC_INDEMNIFICATION <- xml_text( xml_find_all( doc, INDEMNIFICATION ) )





	## VARIABLE NAME:  SJ_01_PC_PERSONAL_SERVICES
	## DESCRIPTION:  Personal services
	## LOCATION:  SCHED-J-PART-01-LINE-1a
	## TABLE:  SJ-P01-T00-CHECKLIST
	## VARIABLE TYPE:  text

	V1 <- '//Return/ReturnData/IRS990ScheduleJ/PersonalServices'
	V2 <- '//Return/ReturnData/IRS990ScheduleJ/PersonalServicesInd'
	V3 <- '//Return/ReturnData/IRS990ScheduleJ/Form990ScheduleJPartI/PersonalServices'
	PERSONAL_SERVICES <- paste( V1, V2, V3 , sep='|' )
	SJ_01_PC_PERSONAL_SERVICES <- xml_text( xml_find_all( doc, PERSONAL_SERVICES ) )





	## VARIABLE NAME:  SJ_01_PC_WRITTEN_POLICY
	## DESCRIPTION:  Written policy reference T and E expenses?
	## LOCATION:  SCHED-J-PART-01-LINE-1b
	## TABLE:  SJ-P01-T00-CHECKLIST
	## VARIABLE TYPE:  text

	V1 <- '//Return/ReturnData/IRS990ScheduleJ/WrittenPolicyRefTAndEExpnssInd'
	V2 <- '//Return/ReturnData/IRS990ScheduleJ/WrittenPolicyReTAndEExpenses'
	V3 <- '//Return/ReturnData/IRS990ScheduleJ/Form990ScheduleJPartI/WrittenPolicyReTAndEExpenses'
	WRITTEN_POLICY <- paste( V1, V2, V3 , sep='|' )
	SJ_01_PC_WRITTEN_POLICY <- xml_text( xml_find_all( doc, WRITTEN_POLICY ) )





	## VARIABLE NAME:  SJ_01_PC_SUBSTANTIATION_REQUIRED
	## DESCRIPTION:  Substantiation required?
	## LOCATION:  SCHED-J-PART-01-LINE-2
	## TABLE:  SJ-P01-T00-CHECKLIST
	## VARIABLE TYPE:  text

	V1 <- '//Return/ReturnData/IRS990ScheduleJ/SubstantiationRequired'
	V2 <- '//Return/ReturnData/IRS990ScheduleJ/SubstantiationRequiredInd'
	V3 <- '//Return/ReturnData/IRS990ScheduleJ/Form990ScheduleJPartI/SubstantiationRequired'
	SUBSTANTIATION_REQUIRED <- paste( V1, V2, V3 , sep='|' )
	SJ_01_PC_SUBSTANTIATION_REQUIRED <- xml_text( xml_find_all( doc, SUBSTANTIATION_REQUIRED ) )





	## VARIABLE NAME:  SJ_01_PC_BOARD_APPROVAL
	## DESCRIPTION:  Board or committee approval
	## LOCATION:  SCHED-J-PART-01-LINE-3
	## TABLE:  SJ-P01-T00-CHECKLIST
	## VARIABLE TYPE:  text

	V1 <- '//Return/ReturnData/IRS990ScheduleJ/BoardOrCommitteeApproval'
	V2 <- '//Return/ReturnData/IRS990ScheduleJ/BoardOrCommitteeApprovalInd'
	V3 <- '//Return/ReturnData/IRS990ScheduleJ/Form990ScheduleJPartI/BoardOrCommitteeApproval'
	BOARD_APPROVAL <- paste( V1, V2, V3 , sep='|' )
	SJ_01_PC_BOARD_APPROVAL <- xml_text( xml_find_all( doc, BOARD_APPROVAL ) )





	## VARIABLE NAME:  SJ_01_PC_COMPENSATION_COMMITTEE
	## DESCRIPTION:  Compensation committee
	## LOCATION:  SCHED-J-PART-01-LINE-3
	## TABLE:  SJ-P01-T00-CHECKLIST
	## VARIABLE TYPE:  text

	V1 <- '//Return/ReturnData/IRS990ScheduleJ/CompensationCommittee'
	V2 <- '//Return/ReturnData/IRS990ScheduleJ/CompensationCommitteeInd'
	V3 <- '//Return/ReturnData/IRS990ScheduleJ/Form990ScheduleJPartI/CompensationCommittee'
	COMPENSATION_COMMITTEE <- paste( V1, V2, V3 , sep='|' )
	SJ_01_PC_COMPENSATION_COMMITTEE <- xml_text( xml_find_all( doc, COMPENSATION_COMMITTEE ) )





	## VARIABLE NAME:  SJ_01_PC_COMPENSATION_SURVEY
	## DESCRIPTION:  Compensation survey
	## LOCATION:  SCHED-J-PART-01-LINE-3
	## TABLE:  SJ-P01-T00-CHECKLIST
	## VARIABLE TYPE:  text

	V1 <- '//Return/ReturnData/IRS990ScheduleJ/CompensationSurvey'
	V2 <- '//Return/ReturnData/IRS990ScheduleJ/CompensationSurveyInd'
	V3 <- '//Return/ReturnData/IRS990ScheduleJ/Form990ScheduleJPartI/CompensationSurvey'
	COMPENSATION_SURVEY <- paste( V1, V2, V3 , sep='|' )
	SJ_01_PC_COMPENSATION_SURVEY <- xml_text( xml_find_all( doc, COMPENSATION_SURVEY ) )





	## VARIABLE NAME:  SJ_01_PC_CONSULTANT
	## DESCRIPTION:  Independent consultant
	## LOCATION:  SCHED-J-PART-01-LINE-3
	## TABLE:  SJ-P01-T00-CHECKLIST
	## VARIABLE TYPE:  text

	V1 <- '//Return/ReturnData/IRS990ScheduleJ/IndependentConsultant'
	V2 <- '//Return/ReturnData/IRS990ScheduleJ/IndependentConsultantInd'
	V3 <- '//Return/ReturnData/IRS990ScheduleJ/Form990ScheduleJPartI/IndependentConsultant'
	CONSULTANT <- paste( V1, V2, V3 , sep='|' )
	SJ_01_PC_CONSULTANT <- xml_text( xml_find_all( doc, CONSULTANT ) )





	## VARIABLE NAME:  SJ_01_PC_CONTRACT
	## DESCRIPTION:  Written employment contract
	## LOCATION:  SCHED-J-PART-01-LINE-3
	## TABLE:  SJ-P01-T00-CHECKLIST
	## VARIABLE TYPE:  text

	V1 <- '//Return/ReturnData/IRS990ScheduleJ/WrittenEmploymentContract'
	V2 <- '//Return/ReturnData/IRS990ScheduleJ/WrittenEmploymentContractInd'
	V3 <- '//Return/ReturnData/IRS990ScheduleJ/Form990ScheduleJPartI/WrittenEmploymentContract'
	CONTRACT <- paste( V1, V2, V3 , sep='|' )
	SJ_01_PC_CONTRACT <- xml_text( xml_find_all( doc, CONTRACT ) )





	## VARIABLE NAME:  SJ_01_PC_OTHER_ORGS_990
	## DESCRIPTION:  Form 990 of other organizations
	## LOCATION:  SCHED-J-PART-01-LINE-3
	## TABLE:  SJ-P01-T00-CHECKLIST
	## VARIABLE TYPE:  text

	V1 <- '//Return/ReturnData/IRS990ScheduleJ/Form990OfOtherOrganizations'
	V2 <- '//Return/ReturnData/IRS990ScheduleJ/Form990OfOtherOrganizationsInd'
	V3 <- '//Return/ReturnData/IRS990ScheduleJ/Form990ScheduleJPartI/Form990OfOtherOrganizations'
	OTHER_ORGS_990 <- paste( V1, V2, V3 , sep='|' )
	SJ_01_PC_OTHER_ORGS_990 <- xml_text( xml_find_all( doc, OTHER_ORGS_990 ) )





	## VARIABLE NAME:  SJ_01_PC_SEVERANCE
	## DESCRIPTION:  Severance payment?
	## LOCATION:  SCHED-J-PART-01-LINE-4a
	## TABLE:  SJ-P01-T00-CHECKLIST
	## VARIABLE TYPE:  text

	V1 <- '//Return/ReturnData/IRS990ScheduleJ/SeverancePayment'
	V2 <- '//Return/ReturnData/IRS990ScheduleJ/SeverancePaymentInd'
	V3 <- '//Return/ReturnData/IRS990ScheduleJ/Form990ScheduleJPartI/SeverancePayment'
	SEVERANCE <- paste( V1, V2, V3 , sep='|' )
	SJ_01_PC_SEVERANCE <- xml_text( xml_find_all( doc, SEVERANCE ) )





	## VARIABLE NAME:  SJ_01_PC_SUPPLEMENTAL_RETIREMENT
	## DESCRIPTION:  Supplemental nonqualified retirement plan?
	## LOCATION:  SCHED-J-PART-01-LINE-4b
	## TABLE:  SJ-P01-T00-CHECKLIST
	## VARIABLE TYPE:  text

	V1 <- '//Return/ReturnData/IRS990ScheduleJ/SupplementalNonqualRetirePlan'
	V2 <- '//Return/ReturnData/IRS990ScheduleJ/SupplementalNonqualRtrPlanInd'
	V3 <- '//Return/ReturnData/IRS990ScheduleJ/Form990ScheduleJPartI/SupplementalNonqualRetirePlan'
	SUPPLEMENTAL_RETIREMENT <- paste( V1, V2, V3 , sep='|' )
	SJ_01_PC_SUPPLEMENTAL_RETIREMENT <- xml_text( xml_find_all( doc, SUPPLEMENTAL_RETIREMENT ) )





	## VARIABLE NAME:  SJ_01_PC_EQUITY_BASED_COMP
	## DESCRIPTION:  Equity based compensation arrangement?
	## LOCATION:  SCHED-J-PART-01-LINE-4c
	## TABLE:  SJ-P01-T00-CHECKLIST
	## VARIABLE TYPE:  text

	V1 <- '//Return/ReturnData/IRS990ScheduleJ/EquityBasedCompArrangement'
	V2 <- '//Return/ReturnData/IRS990ScheduleJ/EquityBasedCompArrngmInd'
	V3 <- '//Return/ReturnData/IRS990ScheduleJ/Form990ScheduleJPartI/EquityBasedCompArrangement'
	EQUITY_BASED_COMP <- paste( V1, V2, V3 , sep='|' )
	SJ_01_PC_EQUITY_BASED_COMP <- xml_text( xml_find_all( doc, EQUITY_BASED_COMP ) )





	## VARIABLE NAME:  SJ_01_PC_CONTINGENT_REV_OWN
	## DESCRIPTION:  Compensation based on revenue of filing org?
	## LOCATION:  SCHED-J-PART-01-LINE-5a
	## TABLE:  SJ-P01-T00-CHECKLIST
	## VARIABLE TYPE:  text

	V1 <- '//Return/ReturnData/IRS990ScheduleJ/CompBasedOnRevenueOfFilingOrg'
	V2 <- '//Return/ReturnData/IRS990ScheduleJ/CompBasedOnRevenueOfFlngOrgInd'
	V3 <- '//Return/ReturnData/IRS990ScheduleJ/Form990ScheduleJPartI/CompBasedOnRevenueOfFilingOrg'
	CONTINGENT_REV_OWN <- paste( V1, V2, V3 , sep='|' )
	SJ_01_PC_CONTINGENT_REV_OWN <- xml_text( xml_find_all( doc, CONTINGENT_REV_OWN ) )





	## VARIABLE NAME:  SJ_01_PC_CONTINGENT_REV_RELATED
	## DESCRIPTION:  Compensation based on revenue of related orgs?
	## LOCATION:  SCHED-J-PART-01-LINE-5b
	## TABLE:  SJ-P01-T00-CHECKLIST
	## VARIABLE TYPE:  text

	V1 <- '//Return/ReturnData/IRS990ScheduleJ/CompBasedOnRevenueRelatedOrgs'
	V2 <- '//Return/ReturnData/IRS990ScheduleJ/CompBsdOnRevRelatedOrgsInd'
	V3 <- '//Return/ReturnData/IRS990ScheduleJ/Form990ScheduleJPartI/CompBasedOnRevenueRelatedOrgs'
	CONTINGENT_REV_RELATED <- paste( V1, V2, V3 , sep='|' )
	SJ_01_PC_CONTINGENT_REV_RELATED <- xml_text( xml_find_all( doc, CONTINGENT_REV_RELATED ) )





	## VARIABLE NAME:  SJ_01_PC_CONTINGENT_NET_OWN
	## DESCRIPTION:  Compensation based on net earnings of filing org?
	## LOCATION:  SCHED-J-PART-01-LINE-6a
	## TABLE:  SJ-P01-T00-CHECKLIST
	## VARIABLE TYPE:  text

	V1 <- '//Return/ReturnData/IRS990ScheduleJ/CompBasedNetEarningsFilingOrg'
	V2 <- '//Return/ReturnData/IRS990ScheduleJ/CompBsdNetEarnsFlngOrgInd'
	V3 <- '//Return/ReturnData/IRS990ScheduleJ/Form990ScheduleJPartI/CompBasedNetEarningsFilingOrg'
	CONTINGENT_NET_OWN <- paste( V1, V2, V3 , sep='|' )
	SJ_01_PC_CONTINGENT_NET_OWN <- xml_text( xml_find_all( doc, CONTINGENT_NET_OWN ) )





	## VARIABLE NAME:  SJ_01_PC_CONTINGENT_NET_RELATED
	## DESCRIPTION:  Compensation based on net earnings of related orgs?
	## LOCATION:  SCHED-J-PART-01-LINE-6b
	## TABLE:  SJ-P01-T00-CHECKLIST
	## VARIABLE TYPE:  text

	V1 <- '//Return/ReturnData/IRS990ScheduleJ/CompBasedNetEarningsRelateOrgs'
	V2 <- '//Return/ReturnData/IRS990ScheduleJ/CompBsdNetEarnsRltdOrgsInd'
	V3 <- '//Return/ReturnData/IRS990ScheduleJ/Form990ScheduleJPartI/CompBasedNetEarningsRelateOrgs'
	CONTINGENT_NET_RELATED <- paste( V1, V2, V3 , sep='|' )
	SJ_01_PC_CONTINGENT_NET_RELATED <- xml_text( xml_find_all( doc, CONTINGENT_NET_RELATED ) )





	## VARIABLE NAME:  SJ_01_PC_NON_FIXED_PAYMENTS
	## DESCRIPTION:  Any non-fixed payments?
	## LOCATION:  SCHED-J-PART-01-LINE-7
	## TABLE:  SJ-P01-T00-CHECKLIST
	## VARIABLE TYPE:  text

	V1 <- '//Return/ReturnData/IRS990ScheduleJ/AnyNonFixedPayments'
	V2 <- '//Return/ReturnData/IRS990ScheduleJ/AnyNonFixedPaymentsInd'
	V3 <- '//Return/ReturnData/IRS990ScheduleJ/Form990ScheduleJPartI/AnyNonFixedPayments'
	NON_FIXED_PAYMENTS <- paste( V1, V2, V3 , sep='|' )
	SJ_01_PC_NON_FIXED_PAYMENTS <- xml_text( xml_find_all( doc, NON_FIXED_PAYMENTS ) )





	## VARIABLE NAME:  SJ_01_PC_CONTRACT_EXCEPTION
	## DESCRIPTION:  Initial contract exception?
	## LOCATION:  SCHED-J-PART-01-LINE-8
	## TABLE:  SJ-P01-T00-CHECKLIST
	## VARIABLE TYPE:  text

	V1 <- '//Return/ReturnData/IRS990ScheduleJ/InitialContractException'
	V2 <- '//Return/ReturnData/IRS990ScheduleJ/InitialContractExceptionInd'
	V3 <- '//Return/ReturnData/IRS990ScheduleJ/Form990ScheduleJPartI/InitialContractException'
	CONTRACT_EXCEPTION <- paste( V1, V2, V3 , sep='|' )
	SJ_01_PC_CONTRACT_EXCEPTION <- xml_text( xml_find_all( doc, CONTRACT_EXCEPTION ) )





	## VARIABLE NAME:  SJ_01_PC_REBUTTABLE_PRESUMPTION
	## DESCRIPTION:  Rebuttable presumption procedure?
	## LOCATION:  SCHED-J-PART-01-LINE-9
	## TABLE:  SJ-P01-T00-CHECKLIST
	## VARIABLE TYPE:  text

	V1 <- '//Return/ReturnData/IRS990ScheduleJ/RebuttablePresumptionProcedure'
	V2 <- '//Return/ReturnData/IRS990ScheduleJ/RebuttablePresumptionProcInd'
	REBUTTABLE_PRESUMPTION <- paste( V1, V2 , sep='|' )
	SJ_01_PC_REBUTTABLE_PRESUMPTION <- xml_text( xml_find_all( doc, REBUTTABLE_PRESUMPTION ) )


	namedList <- function(...){
		      names <- as.list(substitute(list(...)))[-1L]
		      result <- list(...)
		      names(result) <- names
		      result[sapply(result, function(x){length(x)==0})] <- NA
		      result[sapply(result, is.null)] <- NA
		      result
		  }


	d <- namedList( NAME, EIN, TAXYR, FORMTYPE, OBJECTID, URL,
	                 SJ_01_PC_CLUB_FEES, SJ_01_PC_COMPANION_TRAVEL, SJ_01_PC_DISCRETIONARY_ACCOUNT, 
			 SJ_01_PC_FIRST_CLASS_TRAVEL, SJ_01_PC_HOME_OFFICE_SUBSIDY, SJ_01_PC_HOUSING_ALLOWANCE, 
			 SJ_01_PC_INDEMNIFICATION, SJ_01_PC_PERSONAL_SERVICES, SJ_01_PC_WRITTEN_POLICY, 
			 SJ_01_PC_SUBSTANTIATION_REQUIRED, SJ_01_PC_BOARD_APPROVAL, SJ_01_PC_COMPENSATION_COMMITTEE, 
			 SJ_01_PC_COMPENSATION_SURVEY, SJ_01_PC_CONSULTANT, SJ_01_PC_CONTRACT, SJ_01_PC_OTHER_ORGS_990, 
			 SJ_01_PC_SEVERANCE, SJ_01_PC_SUPPLEMENTAL_RETIREMENT, SJ_01_PC_EQUITY_BASED_COMP, 
			 SJ_01_PC_CONTINGENT_REV_OWN, SJ_01_PC_CONTINGENT_REV_RELATED, SJ_01_PC_CONTINGENT_NET_OWN, 
			 SJ_01_PC_CONTINGENT_NET_RELATED, SJ_01_PC_NON_FIXED_PAYMENTS, SJ_01_PC_CONTRACT_EXCEPTION, 
			 SJ_01_PC_REBUTTABLE_PRESUMPTION )
			 
        d <- as.data.frame( d, stringsAsFactors=F )
        
	return( d )


}








#-------------------------------------------------------------------------------------




# FUNCTION TO BUILD THE CORE DATASET
# 
# Arguments: 
#    ein - character vector of nonprofits to sample
#    years - vector of which years to collect
#    form.type - which type of data to collect
#    index - database of all electronic filers provided by the IRS


buildDatabase <- function( eins=NULL, index=NULL, year=NULL, form.type=c("990","990EZ") )
{
  
  library( dplyr )
  library( xml2 ) 
  library( xmltools )
  library( purrr )

  
  # BUILD NECESSARY RESOURCES
  
  if( is.null(index) ) { index <- buildIndex() }
  
  if( is.null(eins) ) { eins <- unique( index$EIN ) }
  
  
  
  # SUBSET INDEX FILE BY SPECIFIED YEARS AND FORMS
  # Return list of URLS to scrape
  
  these <- index[ index$EIN %in% eins & index$TaxYear %in% year, "URL" ]
  
  if( length(these) == 0 ){ return( NULL ) }

  #---------------------------------------------------
  # CREATE A FAIL LIST FOR POOR URL CONNECTIONS
  #---------------------------------------------------
  
  failed.urls <- NULL
  # write.table( index[NULL,], file="FAIL_LIST.csv", sep=",", append=F, col.names=T, row.names=F )  




  #---------------------------------------------------
  # INSTANTIATE EMPTY DATASETS
  #---------------------------------------------------
  
  core <- list()    

  partvii.00 <- list()   # boards and compensation base
  
  partvii.01 <- list()   # dtk compensation table
  partvii.01.group.names <- find_group_names( concordance, "F9-P07-TABLE-01-DTK-COMPENSATION" )
  partvii.01.v.map <- get_var_map( concordance, table.name="F9-P07-TABLE-01-DTK-COMPENSATION" )
  
  partvii.02 <- list()   # contractor compensation table
  partvii.02.group.names <- find_group_names( concordance, "F9-P07-TABLE-02-CONTXR-COMPENSATION" )
  partvii.02.v.map <- get_var_map( concordance, table.name="F9-P07-TABLE-02-CONTXR-COMPENSATION" )
  
  schedNtable00 <- list()
  schedNtable01 <- list()
  schedNtable02 <- list()
  schedNtable03 <- list()

  schedj.00 <- list()  # "SJ-P01-T00-CHECKLIST"             

  schedj.01 <- list()   # contractor compensation table
  schedj.01.group.names <- find_group_names( concordance, "SJ-P02-T01-COMPENSATION" )
  schedj.01.v.map <- get_var_map( concordance, table.name="SJ-P02-T01-COMPENSATION" )   

  schedj.02 <- list()   # contractor compensation table
  schedj.02.group.names <- find_group_names( concordance, "SJ-P03-T02-EXPLANATION-TEXT" )
  schedj.02.v.map <- get_var_map( concordance, table.name="SJ-P03-T02-EXPLANATION-TEXT" )   
  
  
  for( i in 1:length(these) )
  {
     
     # print( i )
     
     url <- these[i]
     
     doc <- NULL
     try( doc <- read_xml( url ), silent=T )
     if( is.null(doc) )
     { 
        failed.urls <- c( failed.urls, url )
        next
     }
     xml_ns_strip( doc )



     # ADD DATA FROM ONE NPO

     one.npo <- scrapeXML( doc, url )
     
     core[[i]]           <- one.npo[[1]]
     schedNtable00[[i]]  <- one.npo[[2]]
     schedNtable01[[i]]  <- one.npo[[3]]
     schedNtable02[[i]]  <- one.npo[[4]]
     schedNtable03[[i]]  <- one.npo[[5]] 
     
     partvii.00[[i]]     <- buildPartVII( doc, url )
     partvii.01[[i]]     <- build_rdb_table( doc, url, group.names=partvii.01.group.names, v.map=partvii.01.v.map )
     # partvii.02[[i]]     <- build_rdb_table( doc, url, group.names=partvii.02.group.names, v.map=partvii.02.v.map )     

     
     schedj.00[[i]]      <- buildSchedJ( doc, url )
     schedj.01[[i]]      <- build_rdb_table( doc, url, group.names=schedj.01.group.names, v.map=schedj.01.v.map )
     schedj.02[[i]]      <- build_rdb_table( doc, url, group.names=schedj.02.group.names, v.map=schedj.02.v.map )    
     
     
  }




  if( ! is.null(failed.urls) )
  {
  
  print( paste( "There are", length(failed.urls), "failed XML URLs to re-try." ) )
  
  for( j in 1:length(failed.urls) )
  {
     
     last.pos <- length(these)
     
     url <- failed.urls[j]
     
     doc <- NULL
     try( doc <- read_xml( url ), silent=T )
     
     if( is.null(doc) ){ next }
     
     if( !is.null(doc) )
     {
       xml_ns_strip( doc )
       failed.urls[j] <- NA
     }



     # ADD DATA FROM ONE NPO

     one.npo <- scrapeXML( doc, url )
     
     core[[last.pos+j]]           <- one.npo[[1]]
     schedNtable00[[last.pos+j]]  <- one.npo[[2]]
     schedNtable01[[last.pos+j]]  <- one.npo[[3]]
     schedNtable02[[last.pos+j]]  <- one.npo[[4]]
     schedNtable03[[last.pos+j]]  <- one.npo[[5]] 
     
     partvii.00[[last.pos+j]]     <- buildPartVII( doc, url )
     partvii.01[[last.pos+j]]     <- build_rdb_table( doc, url, group.names=partvii.01.group.names, v.map=partvii.01.v.map )
     # partvii.02[[last.pos+j]]     <- build_rdb_table( doc, url, group.names=partvii.02.group.names, v.map=partvii.02.v.map )     

     
     schedj.00[[last.pos+j]]      <- buildSchedJ( doc, url )
     schedj.01[[last.pos+j]]      <- build_rdb_table( doc, url, group.names=schedj.01.group.names, v.map=schedj.01.v.map )
     schedj.02[[last.pos+j]]      <- build_rdb_table( doc, url, group.names=schedj.02.group.names, v.map=schedj.02.v.map )    
     
     
  }  # end of failed cases loop
  
  }  # end of failed cases if
  
  failed.urls <- na.omit( failed.urls )
  if( length( failed.urls ) > 0 )
  {
     failed.cases <- index[ index$URL %in% failed.urls , ]
     write.table( failed.cases, file="FAIL_LIST.csv", sep=",", append=T, col.names=F, row.names=F )  
  }




  # bind rows together

  
  core.df <- bind_rows( core )
  
  schedNtable00.df <- bind_rows( schedNtable00  )
  schedNtable01.df <- bind_rows( schedNtable01  )
  schedNtable02.df <- bind_rows( schedNtable02  )
  schedNtable03.df <- bind_rows( schedNtable03  )
  
  partvii.00.df <- bind_rows( partvii.00 )
  partvii.01.df <- bind_rows( partvii.01 )  
  # partvii.02.df <- bind_rows( partvii.02 )  

  schedj.00.df <- bind_rows( schedj.00 )
  schedj.01.df <- bind_rows( schedj.01 )  
  schedj.02.df <- bind_rows( schedj.02 )   



  # apply production rules here...



  # write file to disk

  time <- format(Sys.time(), "%b-%d-%Y-%Hh-%Mm")
  
  if( length(year) > 1 ){ year <- paste( year, collapse="-" ) }
  
  saveRDS( core.df, paste0( year, "-CORE ", time, ".rds" ) )
  
  saveRDS( schedNtable00.df, paste0( year, "-SCHED-N-TABLE-00 ", time, ".rds" ) )
  saveRDS( schedNtable01.df, paste0( year, "-SCHED-N-TABLE-01 ", time, ".rds" ) ) 
  saveRDS( schedNtable02.df, paste0( year, "-SCHED-N-TABLE-02 ", time, ".rds" ) )

  saveRDS( partvii.00.df, paste0( year, "-PART-VII-TABLE-00 ", time, ".rds" ) )
  saveRDS( partvii.01.df, paste0( year, "-PART-VII-TABLE-01 ", time, ".rds" ) ) 
  # saveRDS( partvii.02.df, paste0( year, "-PART-VII-TABLE-02 ", time, ".rds" ) )
   
  saveRDS( schedj.00.df, paste0( year, "-SCHED-J-TABLE-00 ", time, ".rds" ) )
  saveRDS( schedj.01.df, paste0( year, "-SCHED-J-TABLE-01 ", time, ".rds" ) ) 
  saveRDS( schedj.02.df, paste0( year, "-SCHED-J-TABLE-02 ", time, ".rds" ) )


  write.csv( core.df, paste0( year, "-CORE ", time, ".csv" ), row.names=F )
  
  write.csv( schedNtable00.df, paste0( year, "-SCHED-N-TABLE-00 ", time, ".csv" ) )
  write.csv( schedNtable01.df, paste0( year, "-SCHED-N-TABLE-01 ", time, ".csv" ) ) 
  write.csv( schedNtable02.df, paste0( year, "-SCHED-N-TABLE-02 ", time, ".csv" ) )

  write.csv( partvii.00.df, paste0( year, "-PART-VII-TABLE-00 ", time, ".csv" ) )
  write.csv( partvii.01.df, paste0( year, "-PART-VII-TABLE-01 ", time, ".csv" ) ) 
  # write.csv( partvii.02.df, paste0( year, "-PART-VII-TABLE-02 ", time, ".csv" ) )
   
  write.csv( schedj.00.df, paste0( year, "-SCHED-J-TABLE-00 ", time, ".csv" ) )
  write.csv( schedj.01.df, paste0( year, "-SCHED-J-TABLE-01 ", time, ".csv" ) ) 
  write.csv( schedj.02.df, paste0( year, "-SCHED-J-TABLE-02 ", time, ".csv" ) )      
  
  num.failed.cases <- length( failed.urls )
  print( paste( num.failed.cases, "cases failed." ) )
  # print( "Failed cases recorded in FAIL_LIST.csv" )
  
  return( NULL )
  
  
  
  # dat.990 <- list( core, schedNtable00, schedNtable01, schedNtable02, schedNtable03 )
  # return( dat.990 )
   
   
}








create_code_chunks <- function( dat, function.name="buildFUNCTION" )
{

  library( dplyr )
  
  dat$VARIABLE_NAME_NEW <- gsub( "-", "_", dat$VARIABLE_NAME_NEW ) 

  file.create( "CodeChunks.txt" )
  fileConn <- file( "CodeChunks.txt", open="a" )


  #-------------------------------------------------------------------

  writeLines( paste0( function.name, " <- function( doc, url )" ), con = fileConn, sep = "\n" )
  writeLines( "{", con = fileConn, sep = "\n\n\n" )
  writeLines( "## NAME", con = fileConn, sep = "\n\n" )
  writeLines( "V_990NAMEpost2014 <- \"//Return/ReturnHeader/Filer/BusinessName/BusinessNameLine1Txt\" ", con = fileConn, sep = "\n" )
  writeLines( "V_990NAME_2013 <- \"//Return/ReturnHeader/Filer/BusinessName/BusinessNameLine1\" ", con = fileConn, sep = "\n" )
  writeLines( "V_990NAMEpre2013  <- \"//Return/ReturnHeader/Filer/Name/BusinessNameLine1\"", con = fileConn, sep = "\n" )
  writeLines( "name.xpath <- paste( V_990NAME_2013, V_990NAMEpre2013, V_990NAMEpost2014, sep=\"|\" )", con = fileConn, sep = "\n" )
  writeLines( "NAME <- xml_text( xml_find_all( doc, name.xpath ) )", con = fileConn, sep = "\n\n" )
  writeLines( "## EIN", con = fileConn, sep = "\n" )
  writeLines( "EIN  <- xml_text( xml_find_all( doc, \"//Return/ReturnHeader/Filer/EIN\" ) )", con = fileConn, sep = "\n\n" )
  writeLines( "## TAX YEAR", con = fileConn, sep = "\n" )
  writeLines( "V_990FYRpost2013 <- \"//Return/ReturnHeader/TaxYr\"", con = fileConn, sep = "\n" )
  writeLines( "V_990FYRpre2013  <- \"//Return/ReturnHeader/TaxYear\"", con = fileConn, sep = "\n" )
  writeLines( "tax.year.xpath <- paste( V_990FYRpost2013, V_990FYRpre2013, sep=\"|\" )", con = fileConn, sep = "\n" )
  writeLines( "TAXYR <- xml_text( xml_find_all( doc, tax.year.xpath ) )", con = fileConn, sep = "\n\n" )
  writeLines( "## TYPE OF TAX FORM", con = fileConn, sep = "\n" )
  writeLines( "V_990TFpost2013 <- \"//Return/ReturnHeader/ReturnTypeCd\"", con = fileConn, sep = "\n" )
  writeLines( "V_990TFpre2013  <- \"//Return/ReturnHeader/ReturnType\"", con = fileConn, sep = "\n" )
  writeLines( "tax.form.xpath <- paste( V_990TFpost2013, V_990TFpre2013, sep=\"|\" )", con = fileConn, sep = "\n" )
  writeLines( "FORMTYPE <- xml_text( xml_find_all( doc, tax.form.xpath ) )", con = fileConn, sep = "\n\n" )
  writeLines( "## OBJECT ID", con = fileConn, sep = "\n" )
  writeLines( "OBJECTID <- get_object_id( url )", con = fileConn, sep = "\n\n" )
  writeLines( "## URL", con = fileConn, sep = "\n" )
  writeLines( "URL <- url", con = fileConn, sep = "\n\n" )
  writeLines( "#-------------------------------------------------------------------", con = fileConn, sep = "\n\n\n" )

  


  for( i in unique( dat$VARIABLE_NAME_NEW ) )
  {

     sub.dat <- dat[ dat$VARIABLE_NAME_NEW == i , ]

     writeLines( paste("## VARIABLE NAME: ", i), con = fileConn, sep = "\n" )
     writeLines( paste("## DESCRIPTION: ", sub.dat$DESCRIPTION[1]), con = fileConn, sep = "\n" )
     writeLines( paste("## LOCATION: ", sub.dat$LOCATION_CODE[1]), con = fileConn, sep = "\n" )
     writeLines( paste("## TABLE: ", sub.dat$RDB_TABLE[1]), con = fileConn, sep = "\n" )
     writeLines( paste("## VARIABLE TYPE: ", sub.dat$DATA_TYPE_SIMPLE[1]), con = fileConn, sep = "\n\n" )


     xpath.vec <- sub.dat$XPATH

     num.paths <- length( xpath.vec )


     if( num.paths > 1 ){

     for( j in 1:num.paths )
     {
       xpath.j <- paste( "V", j, sep="" )
       writeLines( paste( xpath.j, " <- '/", xpath.vec[j], "'", sep=""), con = fileConn, sep = "\n" )

     }
      or.var.name <- substr( i, 10, nchar(i) )
      or.statement <- paste( or.var.name, "<- paste(", paste( "V", 1:num.paths, sep="", collapse=", " ), ", sep='|' )" ) 
      writeLines( or.statement, con = fileConn, sep = "\n" )
      writeLines( paste( i, "<- xml_text( xml_find_all( doc,", or.var.name, ") )"), con = fileConn, sep = "\n\n\n\n\n\n" )

      } else
      {

        single.xpath <- paste( "'", xpath.vec, "'", sep="" )
        writeLines( paste( i, "<- xml_text( xml_find_all( doc,", single.xpath, ") )"), con = fileConn, sep = "\n\n\n\n\n\n" )

      }
  }

  
  var.names <- as.character( unique( dat$VARIABLE_NAME_NEW ) )


  writeLines(  "namedList <- function(...){
		      names <- as.list(substitute(list(...)))[-1L]
		      result <- list(...)
		      names(result) <- names
		      result[sapply(result, function(x){length(x)==0})] <- NA
		      result[sapply(result, is.null)] <- NA
		      result
		  }", , con = fileConn, sep = "\n\n" )

  
  writeLines( paste0( "d <- namedList( NAME, EIN, TAXYR, FORMTYPE, OBJECTID, URL, ", paste( var.names, collapse=", " ), " )" ), con = fileConn, sep = "\n\n" )  
  writeLines( "return( d )", con = fileConn, sep = "\n\n\n" ) 
  writeLines( "}", con = fileConn, sep = "\n" )   
  
  close( fileConn ) 
  
  shell( "CodeChunks.txt" )
  
  return( NULL )

}



























### CONCORDANCE PART VII

partvii <- 
structure(list(xpath = structure(c(58L, 144L, 145L, 72L, 74L, 
73L, 131L, 132L, 137L, 138L, 133L, 139L, 134L, 81L, 52L, 54L, 
82L, 57L, 85L, 86L, 67L, 55L, 83L, 56L, 84L, 136L, 140L, 135L, 
141L, 53L, 109L, 110L, 111L, 112L, 117L, 118L, 113L, 119L, 114L, 
116L, 120L, 115L, 121L, 65L, 127L, 128L, 124L, 129L, 2L, 3L, 
51L, 69L, 89L, 185L, 191L, 199L, 205L, 68L, 87L, 184L, 189L, 
70L, 90L, 186L, 192L, 148L, 150L, 151L, 146L, 147L, 149L, 152L, 
4L, 21L, 22L, 92L, 167L, 5L, 23L, 24L, 93L, 10L, 33L, 34L, 98L, 
159L, 172L, 11L, 35L, 36L, 99L, 160L, 173L, 17L, 43L, 44L, 105L, 
154L, 155L, 178L, 18L, 45L, 46L, 106L, 179L, 6L, 25L, 26L, 94L, 
168L, 12L, 37L, 38L, 100L, 161L, 162L, 174L, 15L, 20L, 103L, 
156L, 177L, 7L, 27L, 28L, 95L, 169L, 19L, 47L, 107L, 157L, 180L, 
16L, 48L, 104L, 158L, 181L, 9L, 31L, 32L, 97L, 171L, 13L, 39L, 
40L, 101L, 163L, 164L, 175L, 8L, 29L, 30L, 96L, 170L, 14L, 41L, 
42L, 102L, 165L, 166L, 176L, 49L, 76L, 182L, 204L, 50L, 75L, 
188L, 198L, 59L, 62L, 77L, 78L, 142L, 60L, 63L, 79L, 80L, 143L, 
61L, 64L, 88L, 183L, 190L, 202L, 206L, 71L, 91L, 122L, 123L, 
1L, 66L, 108L, 130L, 153L, 207L, 125L, 126L, 194L, 195L, 196L, 
197L, 187L, 193L, 203L, 200L, 201L), .Label = c("/Return/ReturnData/IRS990/CntrctRcvdGreaterThan100KCnt", 
"/Return/ReturnData/IRS990/CompensationFromOtherSources", "/Return/ReturnData/IRS990/CompensationFromOtherSrcsInd", 
"/Return/ReturnData/IRS990/ContractorCompensation/AddressOfContractor/AddressForeign/AddressLine1", 
"/Return/ReturnData/IRS990/ContractorCompensation/AddressOfContractor/AddressForeign/AddressLine2", 
"/Return/ReturnData/IRS990/ContractorCompensation/AddressOfContractor/AddressForeign/City", 
"/Return/ReturnData/IRS990/ContractorCompensation/AddressOfContractor/AddressForeign/Country", 
"/Return/ReturnData/IRS990/ContractorCompensation/AddressOfContractor/AddressForeign/PostalCode", 
"/Return/ReturnData/IRS990/ContractorCompensation/AddressOfContractor/AddressForeign/ProvinceOrState", 
"/Return/ReturnData/IRS990/ContractorCompensation/AddressOfContractor/AddressUS/AddressLine1", 
"/Return/ReturnData/IRS990/ContractorCompensation/AddressOfContractor/AddressUS/AddressLine2", 
"/Return/ReturnData/IRS990/ContractorCompensation/AddressOfContractor/AddressUS/City", 
"/Return/ReturnData/IRS990/ContractorCompensation/AddressOfContractor/AddressUS/State", 
"/Return/ReturnData/IRS990/ContractorCompensation/AddressOfContractor/AddressUS/ZIPCode", 
"/Return/ReturnData/IRS990/ContractorCompensation/Compensation", 
"/Return/ReturnData/IRS990/ContractorCompensation/DescriptionOfServices", 
"/Return/ReturnData/IRS990/ContractorCompensation/NameOfContractor/NameBusiness/BusinessNameLine1", 
"/Return/ReturnData/IRS990/ContractorCompensation/NameOfContractor/NameBusiness/BusinessNameLine2", 
"/Return/ReturnData/IRS990/ContractorCompensation/NameOfContractor/NamePerson", 
"/Return/ReturnData/IRS990/ContractorCompensationGrp/CompensationAmt", 
"/Return/ReturnData/IRS990/ContractorCompensationGrp/ContractorAddress/ForeignAddress/AddressLine1", 
"/Return/ReturnData/IRS990/ContractorCompensationGrp/ContractorAddress/ForeignAddress/AddressLine1Txt", 
"/Return/ReturnData/IRS990/ContractorCompensationGrp/ContractorAddress/ForeignAddress/AddressLine2", 
"/Return/ReturnData/IRS990/ContractorCompensationGrp/ContractorAddress/ForeignAddress/AddressLine2Txt", 
"/Return/ReturnData/IRS990/ContractorCompensationGrp/ContractorAddress/ForeignAddress/City", 
"/Return/ReturnData/IRS990/ContractorCompensationGrp/ContractorAddress/ForeignAddress/CityNm", 
"/Return/ReturnData/IRS990/ContractorCompensationGrp/ContractorAddress/ForeignAddress/Country", 
"/Return/ReturnData/IRS990/ContractorCompensationGrp/ContractorAddress/ForeignAddress/CountryCd", 
"/Return/ReturnData/IRS990/ContractorCompensationGrp/ContractorAddress/ForeignAddress/ForeignPostalCd", 
"/Return/ReturnData/IRS990/ContractorCompensationGrp/ContractorAddress/ForeignAddress/PostalCode", 
"/Return/ReturnData/IRS990/ContractorCompensationGrp/ContractorAddress/ForeignAddress/ProvinceOrState", 
"/Return/ReturnData/IRS990/ContractorCompensationGrp/ContractorAddress/ForeignAddress/ProvinceOrStateNm", 
"/Return/ReturnData/IRS990/ContractorCompensationGrp/ContractorAddress/USAddress/AddressLine1", 
"/Return/ReturnData/IRS990/ContractorCompensationGrp/ContractorAddress/USAddress/AddressLine1Txt", 
"/Return/ReturnData/IRS990/ContractorCompensationGrp/ContractorAddress/USAddress/AddressLine2", 
"/Return/ReturnData/IRS990/ContractorCompensationGrp/ContractorAddress/USAddress/AddressLine2Txt", 
"/Return/ReturnData/IRS990/ContractorCompensationGrp/ContractorAddress/USAddress/City", 
"/Return/ReturnData/IRS990/ContractorCompensationGrp/ContractorAddress/USAddress/CityNm", 
"/Return/ReturnData/IRS990/ContractorCompensationGrp/ContractorAddress/USAddress/State", 
"/Return/ReturnData/IRS990/ContractorCompensationGrp/ContractorAddress/USAddress/StateAbbreviationCd", 
"/Return/ReturnData/IRS990/ContractorCompensationGrp/ContractorAddress/USAddress/ZIPCd", 
"/Return/ReturnData/IRS990/ContractorCompensationGrp/ContractorAddress/USAddress/ZIPCode", 
"/Return/ReturnData/IRS990/ContractorCompensationGrp/ContractorName/BusinessName/BusinessNameLine1", 
"/Return/ReturnData/IRS990/ContractorCompensationGrp/ContractorName/BusinessName/BusinessNameLine1Txt", 
"/Return/ReturnData/IRS990/ContractorCompensationGrp/ContractorName/BusinessName/BusinessNameLine2", 
"/Return/ReturnData/IRS990/ContractorCompensationGrp/ContractorName/BusinessName/BusinessNameLine2Txt", 
"/Return/ReturnData/IRS990/ContractorCompensationGrp/ContractorName/PersonNm", 
"/Return/ReturnData/IRS990/ContractorCompensationGrp/ServicesDesc", 
"/Return/ReturnData/IRS990/Form990PartVIISectionA/AverageHoursPerWeek", 
"/Return/ReturnData/IRS990/Form990PartVIISectionA/AverageHoursPerWeekRelated", 
"/Return/ReturnData/IRS990/Form990PartVIISectionA/CompensationFromOtherSources", 
"/Return/ReturnData/IRS990/Form990PartVIISectionA/Former", "/Return/ReturnData/IRS990/Form990PartVIISectionA/FormersListedIn1A", 
"/Return/ReturnData/IRS990/Form990PartVIISectionA/HighestCompensatedEmployee", 
"/Return/ReturnData/IRS990/Form990PartVIISectionA/IndividualTrusteeOrDirector", 
"/Return/ReturnData/IRS990/Form990PartVIISectionA/InstitutionalTrustee", 
"/Return/ReturnData/IRS990/Form990PartVIISectionA/KeyEmployee", 
"/Return/ReturnData/IRS990/Form990PartVIISectionA/Line1ATotalGT150K", 
"/Return/ReturnData/IRS990/Form990PartVIISectionA/Name/NameBusiness/BusinessNameLine1", 
"/Return/ReturnData/IRS990/Form990PartVIISectionA/Name/NameBusiness/BusinessNameLine2", 
"/Return/ReturnData/IRS990/Form990PartVIISectionA/Name/NamePerson", 
"/Return/ReturnData/IRS990/Form990PartVIISectionA/NameBusiness/BusinessNameLine1", 
"/Return/ReturnData/IRS990/Form990PartVIISectionA/NameBusiness/BusinessNameLine2", 
"/Return/ReturnData/IRS990/Form990PartVIISectionA/NamePerson", 
"/Return/ReturnData/IRS990/Form990PartVIISectionA/NoListedPersonsCompensated", 
"/Return/ReturnData/IRS990/Form990PartVIISectionA/NumberIndividualsGT100K", 
"/Return/ReturnData/IRS990/Form990PartVIISectionA/Officer", "/Return/ReturnData/IRS990/Form990PartVIISectionA/OtherCompensation", 
"/Return/ReturnData/IRS990/Form990PartVIISectionA/ReportableCompFromOrganization", 
"/Return/ReturnData/IRS990/Form990PartVIISectionA/ReportableCompFromRelatedOrgs", 
"/Return/ReturnData/IRS990/Form990PartVIISectionA/Title", "/Return/ReturnData/IRS990/Form990PartVIISectionA/TotalColumnD", 
"/Return/ReturnData/IRS990/Form990PartVIISectionA/TotalColumnE", 
"/Return/ReturnData/IRS990/Form990PartVIISectionA/TotalColumnF", 
"/Return/ReturnData/IRS990/Form990PartVIISectionAGrp/AverageHoursPerWeekRltdOrgRt", 
"/Return/ReturnData/IRS990/Form990PartVIISectionAGrp/AverageHoursPerWeekRt", 
"/Return/ReturnData/IRS990/Form990PartVIISectionAGrp/BusinessName/BusinessNameLine1", 
"/Return/ReturnData/IRS990/Form990PartVIISectionAGrp/BusinessName/BusinessNameLine1Txt", 
"/Return/ReturnData/IRS990/Form990PartVIISectionAGrp/BusinessName/BusinessNameLine2", 
"/Return/ReturnData/IRS990/Form990PartVIISectionAGrp/BusinessName/BusinessNameLine2Txt", 
"/Return/ReturnData/IRS990/Form990PartVIISectionAGrp/FormerOfcrDirectorTrusteeInd", 
"/Return/ReturnData/IRS990/Form990PartVIISectionAGrp/HighestCompensatedEmployeeInd", 
"/Return/ReturnData/IRS990/Form990PartVIISectionAGrp/IndividualTrusteeOrDirectorInd", 
"/Return/ReturnData/IRS990/Form990PartVIISectionAGrp/InstitutionalTrusteeInd", 
"/Return/ReturnData/IRS990/Form990PartVIISectionAGrp/KeyEmployeeInd", 
"/Return/ReturnData/IRS990/Form990PartVIISectionAGrp/OfficerInd", 
"/Return/ReturnData/IRS990/Form990PartVIISectionAGrp/OtherCompensationAmt", 
"/Return/ReturnData/IRS990/Form990PartVIISectionAGrp/PersonNm", 
"/Return/ReturnData/IRS990/Form990PartVIISectionAGrp/ReportableCompFromOrgAmt", 
"/Return/ReturnData/IRS990/Form990PartVIISectionAGrp/ReportableCompFromRltdOrgAmt", 
"/Return/ReturnData/IRS990/Form990PartVIISectionAGrp/TitleTxt", 
"/Return/ReturnData/IRS990/Form990PartVIISectionB/ContractorCompensation/AddressOfContractor/AddressForeign/AddressLine1", 
"/Return/ReturnData/IRS990/Form990PartVIISectionB/ContractorCompensation/AddressOfContractor/AddressForeign/AddressLine2", 
"/Return/ReturnData/IRS990/Form990PartVIISectionB/ContractorCompensation/AddressOfContractor/AddressForeign/City", 
"/Return/ReturnData/IRS990/Form990PartVIISectionB/ContractorCompensation/AddressOfContractor/AddressForeign/Country", 
"/Return/ReturnData/IRS990/Form990PartVIISectionB/ContractorCompensation/AddressOfContractor/AddressForeign/PostalCode", 
"/Return/ReturnData/IRS990/Form990PartVIISectionB/ContractorCompensation/AddressOfContractor/AddressForeign/ProvinceOrState", 
"/Return/ReturnData/IRS990/Form990PartVIISectionB/ContractorCompensation/AddressOfContractor/AddressUS/AddressLine1", 
"/Return/ReturnData/IRS990/Form990PartVIISectionB/ContractorCompensation/AddressOfContractor/AddressUS/AddressLine2", 
"/Return/ReturnData/IRS990/Form990PartVIISectionB/ContractorCompensation/AddressOfContractor/AddressUS/City", 
"/Return/ReturnData/IRS990/Form990PartVIISectionB/ContractorCompensation/AddressOfContractor/AddressUS/State", 
"/Return/ReturnData/IRS990/Form990PartVIISectionB/ContractorCompensation/AddressOfContractor/AddressUS/ZIPCode", 
"/Return/ReturnData/IRS990/Form990PartVIISectionB/ContractorCompensation/Compensation", 
"/Return/ReturnData/IRS990/Form990PartVIISectionB/ContractorCompensation/DescriptionOfServices", 
"/Return/ReturnData/IRS990/Form990PartVIISectionB/ContractorCompensation/NameOfContractor/NameBusiness/BusinessNameLine1", 
"/Return/ReturnData/IRS990/Form990PartVIISectionB/ContractorCompensation/NameOfContractor/NameBusiness/BusinessNameLine2", 
"/Return/ReturnData/IRS990/Form990PartVIISectionB/ContractorCompensation/NameOfContractor/NamePerson", 
"/Return/ReturnData/IRS990/Form990PartVIISectionB/NumberOfContractorsGT100K", 
"/Return/ReturnData/IRS990/FormerOfcrEmployeesListedInd", "/Return/ReturnData/IRS990/FormersListed", 
"/Return/ReturnData/IRS990/FrmrOfcrDirTrstOrKeyEmployee/AddressForeign/AddressLine1", 
"/Return/ReturnData/IRS990/FrmrOfcrDirTrstOrKeyEmployee/AddressForeign/AddressLine2", 
"/Return/ReturnData/IRS990/FrmrOfcrDirTrstOrKeyEmployee/AddressForeign/City", 
"/Return/ReturnData/IRS990/FrmrOfcrDirTrstOrKeyEmployee/AddressForeign/Country", 
"/Return/ReturnData/IRS990/FrmrOfcrDirTrstOrKeyEmployee/AddressForeign/PostalCode", 
"/Return/ReturnData/IRS990/FrmrOfcrDirTrstOrKeyEmployee/AddressForeign/ProvinceOrState", 
"/Return/ReturnData/IRS990/FrmrOfcrDirTrstOrKeyEmployee/AddressUS/AddressLine1", 
"/Return/ReturnData/IRS990/FrmrOfcrDirTrstOrKeyEmployee/AddressUS/AddressLine2", 
"/Return/ReturnData/IRS990/FrmrOfcrDirTrstOrKeyEmployee/AddressUS/City", 
"/Return/ReturnData/IRS990/FrmrOfcrDirTrstOrKeyEmployee/AddressUS/State", 
"/Return/ReturnData/IRS990/FrmrOfcrDirTrstOrKeyEmployee/AddressUS/ZIPCode", 
"/Return/ReturnData/IRS990/FrmrOfcrDirTrstOrKeyEmployee/BusinessName/BusinessNameLine1", 
"/Return/ReturnData/IRS990/FrmrOfcrDirTrstOrKeyEmployee/BusinessName/BusinessNameLine2", 
"/Return/ReturnData/IRS990/IndivRcvdGreaterThan100KCnt", "/Return/ReturnData/IRS990/InfoInScheduleOPartVII", 
"/Return/ReturnData/IRS990/InfoInScheduleOPartVIIInd", "/Return/ReturnData/IRS990/NoListedPersonsCompensated", 
"/Return/ReturnData/IRS990/NoListedPersonsCompensatedInd", "/Return/ReturnData/IRS990/NumberIndividualsGT100K", 
"/Return/ReturnData/IRS990/NumberOfContractorsGT100K", "/Return/ReturnData/IRS990/OfcrDirTrusteesOrKeyEmployee/AddressForeign/AddressLine1", 
"/Return/ReturnData/IRS990/OfcrDirTrusteesOrKeyEmployee/AddressForeign/AddressLine2", 
"/Return/ReturnData/IRS990/OfcrDirTrusteesOrKeyEmployee/AddressForeign/City", 
"/Return/ReturnData/IRS990/OfcrDirTrusteesOrKeyEmployee/AddressForeign/Country", 
"/Return/ReturnData/IRS990/OfcrDirTrusteesOrKeyEmployee/AddressForeign/PostalCode", 
"/Return/ReturnData/IRS990/OfcrDirTrusteesOrKeyEmployee/AddressForeign/ProvinceOrState", 
"/Return/ReturnData/IRS990/OfcrDirTrusteesOrKeyEmployee/AddressUS/AddressLine1", 
"/Return/ReturnData/IRS990/OfcrDirTrusteesOrKeyEmployee/AddressUS/AddressLine2", 
"/Return/ReturnData/IRS990/OfcrDirTrusteesOrKeyEmployee/AddressUS/City", 
"/Return/ReturnData/IRS990/OfcrDirTrusteesOrKeyEmployee/AddressUS/State", 
"/Return/ReturnData/IRS990/OfcrDirTrusteesOrKeyEmployee/AddressUS/ZIPCode", 
"/Return/ReturnData/IRS990/OfcrDirTrusteesOrKeyEmployee/BusinessName/BusinessNameLine1", 
"/Return/ReturnData/IRS990/OfcrDirTrusteesOrKeyEmployee/BusinessName/BusinessNameLine2", 
"/Return/ReturnData/IRS990/TotalCompGreaterThan150KInd", "/Return/ReturnData/IRS990/TotalCompGT150K", 
"/Return/ReturnData/IRS990/TotalOtherCompensation", "/Return/ReturnData/IRS990/TotalOtherCompensationAmt", 
"/Return/ReturnData/IRS990/TotalPartVII", "/Return/ReturnData/IRS990/TotalReportableCompFrmRltdOrgs", 
"/Return/ReturnData/IRS990/TotalReportableCompFromOrg", "/Return/ReturnData/IRS990/TotalReportableCompFromOrgAmt", 
"/Return/ReturnData/IRS990/TotReportableCompRltdOrgAmt", "/Return/ReturnData/IRS990EZ/CntrctRcvdGreaterThan100KCnt", 
"/Return/ReturnData/IRS990EZ/CompensationOfHghstPdCntrctGrp/BusinessName/BusinessNameLine1", 
"/Return/ReturnData/IRS990EZ/CompensationOfHghstPdCntrctGrp/BusinessName/BusinessNameLine1Txt", 
"/Return/ReturnData/IRS990EZ/CompensationOfHghstPdCntrctGrp/CompensationAmt", 
"/Return/ReturnData/IRS990EZ/CompensationOfHghstPdCntrctGrp/PersonNm", 
"/Return/ReturnData/IRS990EZ/CompensationOfHghstPdCntrctGrp/ServiceTypeTxt", 
"/Return/ReturnData/IRS990EZ/CompensationOfHghstPdCntrctGrp/USAddress/AddressLine1", 
"/Return/ReturnData/IRS990EZ/CompensationOfHghstPdCntrctGrp/USAddress/AddressLine1Txt", 
"/Return/ReturnData/IRS990EZ/CompensationOfHghstPdCntrctGrp/USAddress/City", 
"/Return/ReturnData/IRS990EZ/CompensationOfHghstPdCntrctGrp/USAddress/CityNm", 
"/Return/ReturnData/IRS990EZ/CompensationOfHghstPdCntrctGrp/USAddress/State", 
"/Return/ReturnData/IRS990EZ/CompensationOfHghstPdCntrctGrp/USAddress/StateAbbreviationCd", 
"/Return/ReturnData/IRS990EZ/CompensationOfHghstPdCntrctGrp/USAddress/ZIPCd", 
"/Return/ReturnData/IRS990EZ/CompensationOfHghstPdCntrctGrp/USAddress/ZIPCode", 
"/Return/ReturnData/IRS990EZ/CompOfHghstPaidCntrctProfSer/AddressForeign/AddressLine1", 
"/Return/ReturnData/IRS990EZ/CompOfHghstPaidCntrctProfSer/AddressForeign/City", 
"/Return/ReturnData/IRS990EZ/CompOfHghstPaidCntrctProfSer/AddressForeign/Country", 
"/Return/ReturnData/IRS990EZ/CompOfHghstPaidCntrctProfSer/AddressForeign/PostalCode", 
"/Return/ReturnData/IRS990EZ/CompOfHghstPaidCntrctProfSer/AddressForeign/ProvinceOrState", 
"/Return/ReturnData/IRS990EZ/CompOfHghstPaidCntrctProfSer/AddressUS/AddressLine1", 
"/Return/ReturnData/IRS990EZ/CompOfHghstPaidCntrctProfSer/AddressUS/AddressLine2", 
"/Return/ReturnData/IRS990EZ/CompOfHghstPaidCntrctProfSer/AddressUS/City", 
"/Return/ReturnData/IRS990EZ/CompOfHghstPaidCntrctProfSer/AddressUS/State", 
"/Return/ReturnData/IRS990EZ/CompOfHghstPaidCntrctProfSer/AddressUS/ZIPCode", 
"/Return/ReturnData/IRS990EZ/CompOfHghstPaidCntrctProfSer/Compensation", 
"/Return/ReturnData/IRS990EZ/CompOfHghstPaidCntrctProfSer/NameBusiness/BusinessNameLine1", 
"/Return/ReturnData/IRS990EZ/CompOfHghstPaidCntrctProfSer/NameBusiness/BusinessNameLine2", 
"/Return/ReturnData/IRS990EZ/CompOfHghstPaidCntrctProfSer/NamePerson", 
"/Return/ReturnData/IRS990EZ/CompOfHghstPaidCntrctProfSer/TypeOfService", 
"/Return/ReturnData/IRS990EZ/Form990PartVIISectionA/AverageHoursPerWeek", 
"/Return/ReturnData/IRS990EZ/Form990PartVIISectionA/NamePerson", 
"/Return/ReturnData/IRS990EZ/Form990PartVIISectionA/OtherCompensation", 
"/Return/ReturnData/IRS990EZ/Form990PartVIISectionA/ReportableCompFromOrganization", 
"/Return/ReturnData/IRS990EZ/Form990PartVIISectionA/ReportableCompFromRelatedOrgs", 
"/Return/ReturnData/IRS990EZ/Form990PartVIISectionA/Title", "/Return/ReturnData/IRS990EZ/Form990PartVIISectionAGrp/AverageHoursPerWeekRt", 
"/Return/ReturnData/IRS990EZ/Form990PartVIISectionAGrp/OtherCompensationAmt", 
"/Return/ReturnData/IRS990EZ/Form990PartVIISectionAGrp/PersonNm", 
"/Return/ReturnData/IRS990EZ/Form990PartVIISectionAGrp/ReportableCompFromOrgAmt", 
"/Return/ReturnData/IRS990EZ/Form990PartVIISectionAGrp/ReportableCompFromRltdOrgAmt", 
"/Return/ReturnData/IRS990EZ/Form990PartVIISectionAGrp/TitleTxt", 
"/Return/ReturnData/IRS990EZ/InfoInScheduleOPartIV", "/Return/ReturnData/IRS990EZ/InfoInScheduleOPartIVInd", 
"/Return/ReturnData/IRS990EZ/InfoInScheduleOPartVII", "/Return/ReturnData/IRS990EZ/InfoInScheduleOPartVIIInd", 
"/Return/ReturnData/IRS990EZ/OfficerDirectorTrusteeEmplGrp/AverageHrsPerWkDevotedToPosRt", 
"/Return/ReturnData/IRS990EZ/OfficerDirectorTrusteeEmplGrp/CompensationAmt", 
"/Return/ReturnData/IRS990EZ/OfficerDirectorTrusteeEmplGrp/EmployeeBenefitProgramAmt", 
"/Return/ReturnData/IRS990EZ/OfficerDirectorTrusteeEmplGrp/ExpenseAccountOtherAllwncAmt", 
"/Return/ReturnData/IRS990EZ/OfficerDirectorTrusteeEmplGrp/PersonNm", 
"/Return/ReturnData/IRS990EZ/OfficerDirectorTrusteeEmplGrp/TitleTxt", 
"/Return/ReturnData/IRS990EZ/OfficerDirectorTrusteeKeyEmpl/AvgHoursPerWkDevotedToPosition", 
"/Return/ReturnData/IRS990EZ/OfficerDirectorTrusteeKeyEmpl/Compensation", 
"/Return/ReturnData/IRS990EZ/OfficerDirectorTrusteeKeyEmpl/PersonName", 
"/Return/ReturnData/IRS990EZ/TotNumCntrctPdOver100KProfSer"), class = "factor"), 
    variable_name_new = structure(c(2L, 2L, 2L, 3L, 4L, 5L, 6L, 
    7L, 8L, 9L, 10L, 11L, 12L, 13L, 13L, 14L, 14L, 15L, 15L, 
    16L, 16L, 17L, 17L, 18L, 18L, 19L, 20L, 21L, 22L, 23L, 23L, 
    23L, 24L, 25L, 26L, 27L, 28L, 29L, 30L, 31L, 32L, 33L, 34L, 
    35L, 35L, 35L, 36L, 36L, 37L, 37L, 37L, 38L, 38L, 38L, 38L, 
    38L, 38L, 39L, 39L, 39L, 39L, 40L, 40L, 40L, 40L, 41L, 42L, 
    42L, 43L, 43L, 44L, 44L, 45L, 45L, 45L, 45L, 45L, 46L, 46L, 
    46L, 46L, 47L, 47L, 47L, 47L, 47L, 47L, 48L, 48L, 48L, 48L, 
    48L, 48L, 49L, 49L, 49L, 49L, 49L, 49L, 49L, 50L, 50L, 50L, 
    50L, 50L, 51L, 51L, 51L, 51L, 51L, 52L, 52L, 52L, 52L, 52L, 
    52L, 52L, 53L, 53L, 53L, 53L, 53L, 54L, 54L, 54L, 54L, 54L, 
    55L, 55L, 55L, 55L, 55L, 56L, 56L, 56L, 56L, 56L, 57L, 57L, 
    57L, 57L, 57L, 58L, 58L, 58L, 58L, 58L, 58L, 58L, 59L, 59L, 
    59L, 59L, 59L, 60L, 60L, 60L, 60L, 60L, 60L, 60L, 61L, 61L, 
    61L, 61L, 62L, 62L, 62L, 62L, 63L, 63L, 63L, 63L, 63L, 64L, 
    64L, 64L, 64L, 64L, 65L, 65L, 65L, 65L, 65L, 65L, 65L, 66L, 
    66L, 67L, 68L, 69L, 69L, 69L, 69L, 69L, 69L, 70L, 70L, 70L, 
    70L, 70L, 70L, 66L, 66L, 66L, 1L, 39L), .Label = c("F9_07_EZ_COMP_BENF", 
    "F9_07_PC_COMP_GT_150K_CHECKBOX", "F9_07_PC_COMP_SUBTOT_DIRECT", 
    "F9_07_PC_COMP_SUBTOT_OTHER", "F9_07_PC_COMP_SUBTOT_RELATED", 
    "F9_07_PC_DTK_ADDR_FRGN_L1", "F9_07_PC_DTK_ADDR_FRGN_L2", 
    "F9_07_PC_DTK_ADDR_US_L1", "F9_07_PC_DTK_ADDR_US_L2", "F9_07_PC_DTK_CITY_FRGN", 
    "F9_07_PC_DTK_CITY_US", "F9_07_PC_DTK_COUNTRY_FRGN", "F9_07_PC_DTK_POS_FORMER", 
    "F9_07_PC_DTK_POS_HIGH_COMP_EMP", "F9_07_PC_DTK_POS_KEY_EMPLOYEE", 
    "F9_07_PC_DTK_POS_OFFICER", "F9_07_PC_DTK_POS_TRUSTEE_INDIV", 
    "F9_07_PC_DTK_POS_TRUSTEE_INST", "F9_07_PC_DTK_STATE_FRGN", 
    "F9_07_PC_DTK_STATE_US", "F9_07_PC_DTK_ZIP_FRGN", "F9_07_PC_DTK_ZIP_US", 
    "F9_07_PC_FORMER_CHECKBOX", "F9_07_PC_FORMER_DTK_ADDR_FRGN_L1", 
    "F9_07_PC_FORMER_DTK_ADDR_FRGN_L2", "F9_07_PC_FORMER_DTK_ADDR_US_L1", 
    "F9_07_PC_FORMER_DTK_ADDR_US_L2", "F9_07_PC_FORMER_DTK_CITY_FRGN", 
    "F9_07_PC_FORMER_DTK_CITY_US", "F9_07_PC_FORMER_DTK_COUNTRY_FRGN", 
    "F9_07_PC_FORMER_DTK_STATE_FRGN", "F9_07_PC_FORMER_DTK_STATE_US", 
    "F9_07_PC_FORMER_DTK_ZIP_FRGN", "F9_07_PC_FORMER_DTK_ZIP_US", 
    "F9_07_PC_NO_PERSON_COMPENSATED", "F9_07_PC_NUM_EMP_COMP_GT_100K", 
    "F9_07_PC_OTHER_COMP_CHECKBOX", "F9_07_PZ_COMP_DIRECT", "F9_07_PZ_COMP_OTHER", 
    "F9_07_PZ_COMP_RELATED", "F9_07_PZ_COMP_TOTAL_COLS_B_D_E", 
    "F9_07_PZ_COMP_TOTAL_DIRECT", "F9_07_PZ_COMP_TOTAL_OTHER", 
    "F9_07_PZ_COMP_TOTAL_RELATED", "F9_07_PZ_CONTXRS_ADDR_FRGN_L1", 
    "F9_07_PZ_CONTXRS_ADDR_FRGN_L2", "F9_07_PZ_CONTXRS_ADDR_US_L1", 
    "F9_07_PZ_CONTXRS_ADDR_US_L2", "F9_07_PZ_CONTXRS_BIZ_NAME_L1", 
    "F9_07_PZ_CONTXRS_BIZ_NAME_L2", "F9_07_PZ_CONTXRS_CITY_FRGN", 
    "F9_07_PZ_CONTXRS_CITY_US", "F9_07_PZ_CONTXRS_COMP", "F9_07_PZ_CONTXRS_COUNTRY_FRGN", 
    "F9_07_PZ_CONTXRS_NAME", "F9_07_PZ_CONTXRS_SERVICES", "F9_07_PZ_CONTXRS_STATE_FRGN", 
    "F9_07_PZ_CONTXRS_STATE_US", "F9_07_PZ_CONTXRS_ZIP_FRGN", 
    "F9_07_PZ_CONTXRS_ZIP_US", "F9_07_PZ_DTK_AVE_HOURS_WEEK", 
    "F9_07_PZ_DTK_AVE_HOURS_WEEK_RLTD", "F9_07_PZ_DTK_BIZ_NAME_L1", 
    "F9_07_PZ_DTK_BIZ_NAME_L2", "F9_07_PZ_DTK_NAME", "F9_07_PZ_DTK_TITLE", 
    "F9_07_PZ_FORMER_DTK_BIZ_NAME_L1", "F9_07_PZ_FORMER_DTK_BIZ_NAME_L2", 
    "F9_07_PZ_NUM_HIGH_PAY_CONTXRS", "F9_07_PZ_SCHED_O"), class = "factor"), 
    nch = c(30L, 30L, 30L, 27L, 26L, 28L, 25L, 25L, 23L, 23L, 
    22L, 20L, 25L, 23L, 23L, 30L, 30L, 29L, 29L, 24L, 24L, 30L, 
    30L, 29L, 29L, 23L, 21L, 21L, 19L, 24L, 24L, 24L, 32L, 32L, 
    30L, 30L, 29L, 27L, 32L, 30L, 28L, 28L, 26L, 30L, 30L, 30L, 
    29L, 29L, 28L, 28L, 28L, 20L, 20L, 20L, 20L, 20L, 20L, 19L, 
    19L, 19L, 19L, 21L, 21L, 21L, 21L, 30L, 26L, 26L, 25L, 25L, 
    27L, 27L, 29L, 29L, 29L, 29L, NA, 29L, 29L, 29L, 29L, 27L, 
    27L, 27L, 27L, NA, NA, 27L, 27L, 27L, 27L, NA, NA, 28L, 28L, 
    28L, 28L, NA, NA, NA, 28L, 28L, 28L, 28L, NA, 26L, 26L, 26L, 
    26L, NA, 24L, 24L, 24L, 24L, NA, NA, NA, 21L, 21L, 21L, NA, 
    NA, 29L, 29L, 29L, 29L, NA, 21L, 21L, 21L, NA, NA, 25L, 25L, 
    25L, NA, NA, 27L, 27L, 27L, 27L, NA, 25L, 25L, 25L, 25L, 
    NA, NA, NA, 25L, 25L, 25L, 25L, NA, 23L, 23L, 23L, 23L, NA, 
    NA, NA, 27L, 27L, 27L, 27L, 32L, 32L, 32L, 32L, 24L, 24L, 
    24L, 24L, 24L, 24L, 24L, 24L, 24L, 24L, 17L, 17L, 17L, 17L, 
    17L, 17L, 17L, 18L, 18L, 31L, 31L, 29L, 29L, 29L, NA, NA, 
    NA, 16L, 16L, 16L, 16L, 16L, 16L, 18L, 18L, 18L, NA, NA), 
    description = structure(c(51L, 50L, 50L, 72L, 74L, 73L, 1L, 
    2L, 6L, 7L, 3L, 8L, 4L, 28L, 48L, 29L, 29L, 49L, 24L, 26L, 
    60L, 46L, 46L, 47L, 47L, 62L, 9L, 5L, 10L, 48L, 48L, 48L, 
    1L, 2L, 6L, 7L, 3L, 8L, 4L, 62L, 9L, 5L, 10L, 57L, 57L, 57L, 
    58L, 58L, 17L, 17L, 17L, 63L, 63L, 63L, 63L, 63L, 63L, 61L, 
    61L, 61L, 61L, 64L, 64L, 64L, 64L, 67L, 69L, 69L, 71L, 71L, 
    70L, 70L, 36L, 36L, 36L, 1L, 36L, 37L, 37L, 37L, 2L, 42L, 
    42L, 42L, 6L, 42L, 42L, 43L, 43L, 43L, 7L, 42L, 43L, 30L, 
    30L, 30L, 54L, 30L, 30L, 30L, 31L, 31L, 31L, 55L, 31L, 34L, 
    34L, 34L, 3L, 34L, 41L, 41L, 41L, 8L, 41L, 41L, 41L, 32L, 
    32L, 18L, 32L, 32L, 35L, 35L, 35L, 4L, 35L, 40L, 40L, 56L, 
    40L, 40L, 33L, 33L, 20L, 33L, 33L, 38L, 38L, 38L, 62L, 38L, 
    44L, 44L, 75L, 9L, 44L, 44L, 44L, 39L, 39L, 39L, 5L, 39L, 
    45L, 45L, 45L, 10L, 45L, 45L, 45L, 11L, 11L, 11L, 11L, 12L, 
    12L, 11L, 11L, 54L, 54L, 13L, 14L, 13L, 55L, 55L, 15L, 16L, 
    15L, 52L, 53L, 53L, 22L, 25L, 53L, 53L, 66L, 66L, 13L, 15L, 
    68L, 58L, 59L, 68L, 68L, 68L, 65L, 65L, 65L, 65L, 65L, 65L, 
    23L, 27L, 66L, 19L, 21L), .Label = c("Address Foreign - AddressLine1", 
    "Address Foreign - AddressLine2", "Address Foreign - City", 
    "Address Foreign - Country", "Address Foreign - Postal code", 
    "Address US - AddressLine1", "Address US - AddressLine2", 
    "Address US - City", "Address US - State", "Address US - ZIPCode", 
    "Average hours per week", "Average hours per week for related organizations", 
    "Business Name - BusinessNameLine1", "Business Name - BusinessNameLine1Txt", 
    "Business Name - BusinessNameLine2", "Business Name - BusinessNameLine2Txt", 
    "Compensation from other sources?", "Contractor Compensation - Compensation", 
    "Contributions to employee benefit plans and deferred compensation", 
    "Description of services", "Expense account and other allowances", 
    "Form990 Part VIISection A - Name - Person", "Form990 Part VIISection A - Title", 
    "Form990 Part VIISection AGrp - Key Employee", "Form990 Part VIISection AGrp - Name - Person", 
    "Form990 Part VIISection AGrp - Officer", "Form990 Part VIISection AGrp - Title", 
    "Former", "Highest compensated employee", "Highest compensated independent contractor (top 5 above 100K) business name line 1", 
    "Highest compensated independent contractor (top 5 above 100K) business name line 2", 
    "Highest compensated independent contractor (top 5 above 100K) compensation", 
    "Highest compensated independent contractor (top 5 above 100K) description of services", 
    "Highest compensated independent contractor (top 5 above 100K) foreign address city", 
    "Highest compensated independent contractor (top 5 above 100K) foreign address country", 
    "Highest compensated independent contractor (top 5 above 100K) foreign address line 1", 
    "Highest compensated independent contractor (top 5 above 100K) foreign address line 2", 
    "Highest compensated independent contractor (top 5 above 100K) foreign address province or state", 
    "Highest compensated independent contractor (top 5 above 100K) foreign address zip code", 
    "Highest compensated independent contractor (top 5 above 100K) person name", 
    "Highest compensated independent contractor (top 5 above 100K) US address city", 
    "Highest compensated independent contractor (top 5 above 100K) US address line 1", 
    "Highest compensated independent contractor (top 5 above 100K) US address line 2", 
    "Highest compensated independent contractor (top 5 above 100K) US address state", 
    "Highest compensated independent contractor (top 5 above 100K) US address zip code", 
    "Individual trustee or director", "Institutional Trustee", 
    "Is a former officer, director, or trustee listed in the compensation table in Sectin A Line 1a? ", 
    "Key Employee", "Line1a, total greater than $150K?", "Line1a; total greater than $150K?", 
    "Name - Name - Person", "Name - Person", "Name Business - BusinessNameLine1", 
    "Name Business - BusinessNameLine2", "Name Of Contractor - Name - Person", 
    "No listed persons compensated", "Number individuals greater than $100K", 
    "Number of contractors greater than $100K", "Officer", "Other compensation", 
    "Province or state", "Reportable compensation from organization", 
    "Reportable compensation from related organizations", "Schedule O contains a response to a question in Part VII", 
    "Title", "Total (add line 104; columns (B); (D); and (E))", 
    "Total number of independent contractors who received 100K+ from org", 
    "Total, column D", "Total, column E", "Total, column F", 
    "Total; column D", "Total; column E", "Total; column F", 
    "USAddress - StateAbbreviationCd"), class = "factor"), location_code_xsd = structure(c(1L, 
    75L, 76L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 33L, 32L, 
    36L, 37L, 47L, 48L, 65L, 55L, 38L, 39L, 45L, 46L, 1L, 1L, 
    1L, 1L, 1L, 34L, 35L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
    1L, 1L, 1L, 51L, 52L, 40L, 53L, 9L, 10L, 1L, 70L, 69L, 1L, 
    1L, 57L, 63L, 66L, 67L, 1L, 1L, 71L, 72L, 1L, 1L, 1L, 80L, 
    81L, 77L, 78L, 79L, 83L, 20L, 28L, 28L, 1L, 15L, 20L, 28L, 
    28L, 1L, 21L, 31L, 31L, 1L, 14L, 16L, 21L, 31L, 31L, 1L, 
    14L, 16L, 24L, 26L, 26L, 1L, 1L, 1L, 1L, 24L, 26L, 26L, 1L, 
    1L, 20L, 28L, 28L, 1L, 15L, 21L, 31L, 31L, 1L, 14L, 14L, 
    16L, 22L, 27L, 1L, 11L, 17L, 20L, 28L, 28L, 1L, 15L, 25L, 
    29L, 1L, 12L, 18L, 23L, 30L, 1L, 13L, 19L, 20L, 28L, 28L, 
    1L, 15L, 21L, 31L, 31L, 1L, 14L, 14L, 16L, 20L, 28L, 28L, 
    1L, 15L, 21L, 31L, 31L, 1L, 14L, 14L, 16L, 2L, 5L, 1L, 62L, 
    3L, 4L, 1L, 56L, 1L, 49L, 6L, 6L, 1L, 1L, 49L, 6L, 6L, 1L, 
    1L, 50L, 68L, 1L, 1L, 60L, 64L, 73L, 74L, 1L, 1L, 8L, 1L, 
    1L, 54L, 7L, 82L, 43L, 44L, 41L, 42L, 1L, 1L, 1L, 1L, 61L, 
    58L, 59L), .Label = c("", "[AverageHoursPerWeek] Part VII Section A Line 1a B", 
    "[AverageHoursPerWeekRelated] Part VII Section A Line 1a B", 
    "[AverageHoursPerWeekRltdOrgRt] Part VII Section A Line 1a B", 
    "[AverageHoursPerWeekRt] Part VII Section A Line 1a B", "[BusinessName] Part VII Section A Line 1a A", 
    "[CntrctRcvdGreaterThan100KCnt] Part VI Line 51d", "[CntrctRcvdGreaterThan100KCnt] Part VII Section B Line 2", 
    "[CompensationFromOtherSources] Part VII Section A Line 5", 
    "[CompensationFromOtherSrcsInd] Part VII Section A Line 5", 
    "[CompensationOfHghstPdCntrctGrp] Part VI Line 51; [CompensationAmt] Part VI Line 51 Column (c)", 
    "[CompensationOfHghstPdCntrctGrp] Part VI Line 51; [PersonNm] Part VI Line 51 Column (a)", 
    "[CompensationOfHghstPdCntrctGrp] Part VI Line 51; [ServiceTypeTxt] Part VI Line 51 Column (b)", 
    "[CompensationOfHghstPdCntrctGrp] Part VI Line 51; [USAddress] Part VI Line 51 Column (a)", 
    "[CompOfHghstPaidCntrctProfSer] Part VI Line 51; [AddressForeign] Part VI Line 51 Column (a)", 
    "[CompOfHghstPaidCntrctProfSer] Part VI Line 51; [AddressUS] Part VI Line 51 Column (a)", 
    "[CompOfHghstPaidCntrctProfSer] Part VI Line 51; [Compensation] Part VI Line 51 Column (c)", 
    "[CompOfHghstPaidCntrctProfSer] Part VI Line 51; [NamePerson] Part VI Line 51 Column (a)", 
    "[CompOfHghstPaidCntrctProfSer] Part VI Line 51; [TypeOfService] Part VI Line 51 Column (b)", 
    "[ContractorCompensation] Part VII Section B; [AddressForeign] Part VII Section B Line 1(A)", 
    "[ContractorCompensation] Part VII Section B; [AddressUS] Part VII Section B Line 1(A)", 
    "[ContractorCompensation] Part VII Section B; [Compensation] Part VII Section B Line 1(C)", 
    "[ContractorCompensation] Part VII Section B; [DescriptionOfServices] Part VII Section B Line 1(B)", 
    "[ContractorCompensation] Part VII Section B; [NameBusiness] Part VII Section B Line 1(A)", 
    "[ContractorCompensation] Part VII Section B; [NamePerson] Part VII Section B Line 1(A)", 
    "[ContractorCompensationGrp] Part VII Section B; [BusinessName] Part VII Section B Line 1(A)", 
    "[ContractorCompensationGrp] Part VII Section B; [CompensationAmt] Part VII Section B Line 1(C)", 
    "[ContractorCompensationGrp] Part VII Section B; [ForeignAddress] Part VII Section B Line 1(A)", 
    "[ContractorCompensationGrp] Part VII Section B; [PersonNm] Part VII Section B Line 1(A)", 
    "[ContractorCompensationGrp] Part VII Section B; [ServicesDesc] Part VII Section B Line 1(B)", 
    "[ContractorCompensationGrp] Part VII Section B; [USAddress] Part VII Section B Line 1(A)", 
    "[Former] Part VII Section A Line 1a C", "[FormerOfcrDirectorTrusteeInd] Part VII Section A Line 1a C", 
    "[FormerOfcrEmployeesListedInd] Part VII Section A Line 3", 
    "[FormersListed] Part VII Section A Line 3", "[HighestCompensatedEmployee] Part VII Section A Line 1a C", 
    "[HighestCompensatedEmployeeInd] Part VII Section A Line 1a C", 
    "[IndividualTrusteeOrDirector] Part VII Section A Line 1a C", 
    "[IndividualTrusteeOrDirectorInd] Part VII Section A Line 1a C", 
    "[IndivRcvdGreaterThan100KCnt] Part VII Section A Line 2", 
    "[InfoInScheduleOPartIV] Part IV", "[InfoInScheduleOPartIVInd] Part IV", 
    "[InfoInScheduleOPartVII] Part VII", "[InfoInScheduleOPartVIIInd] Part VII", 
    "[InstitutionalTrustee] Part VII Section A Line 1a C", "[InstitutionalTrusteeInd] Part VII Section A Line 1a C", 
    "[KeyEmployee] Part VII Section A Line 1a C", "[KeyEmployeeInd] Part VII Section A Line 1a C", 
    "[NameBusiness] Part VII Section A Line 1a A", "[NamePerson] Part VII Section A Line 1a A", 
    "[NoListedPersonsCompensated] Part VII Section A", "[NoListedPersonsCompensatedInd] Part VII Section A", 
    "[NumberIndividualsGT100K] Part VII Section A Line 2", "[NumberOfContractorsGT100K] Part VII Section B Line 2", 
    "[Officer] Part VII Section A Line 1a C", "[OfficerDirectorTrusteeEmplGrp] Part IV; [AverageHrsPerWkDevotedToPosRt] Part IV - Column (b)", 
    "[OfficerDirectorTrusteeEmplGrp] Part IV; [CompensationAmt] Part IV - Column (c)", 
    "[OfficerDirectorTrusteeEmplGrp] Part IV; [EmployeeBenefitProgramAmt] Part IV - Column (d)", 
    "[OfficerDirectorTrusteeEmplGrp] Part IV; [ExpenseAccountOtherAllwncAmt] Part IV - Column (e)", 
    "[OfficerDirectorTrusteeEmplGrp] Part IV; [PersonNm] Part IV - Column (a)", 
    "[OfficerDirectorTrusteeEmplGrp] Part IV; [TitleTxt] Part IV - Column (a)", 
    "[OfficerDirectorTrusteeKeyEmpl] Part IV; [AvgHoursPerWkDevotedToPosition] Part IV - Column (b)", 
    "[OfficerDirectorTrusteeKeyEmpl] Part IV; [Compensation] Part IV - Column (c)", 
    "[OfficerDirectorTrusteeKeyEmpl] Part IV; [PersonName] Part IV - Column (a)", 
    "[OfficerInd] Part VII Section A Line 1a C", "[OtherCompensation] Part VII Section A Line 1a F", 
    "[OtherCompensationAmt] Part VII Section A Line 1a F", "[PersonNm] Part VII Section A Line 1a A", 
    "[ReportableCompFromOrgAmt] Part VII Section A Line 1a D", 
    "[ReportableCompFromOrganization] Part VII Section A Line 1a D", 
    "[ReportableCompFromRelatedOrgs] Part VII Section A Line 1a E", 
    "[ReportableCompFromRltdOrgAmt] Part VII Section A Line 1a E", 
    "[Title] Part VII Section A Line 1a A", "[TitleTxt] Part VII Section A Line 1a A", 
    "[TotalCompGreaterThan150KInd] Part VII Section A Line 4", 
    "[TotalCompGT150K] Part VII Section A Line 4", "[TotalOtherCompensation] Part VII Section A Line 1d F", 
    "[TotalOtherCompensationAmt] Part VII Section A Line 1d F", 
    "[TotalReportableCompFrmRltdOrgs] Part VII Section A Line 1d E", 
    "[TotalReportableCompFromOrg] Part VII Section A Line 1d D", 
    "[TotalReportableCompFromOrgAmt] Part VII Section A Line 1d D", 
    "[TotNumCntrctPdOver100KProfSer] Part VI Line 51d", "[TotReportableCompRltdOrgAmt] Part VII Section A Line 1d E"
    ), class = "factor"), location_code = structure(c(35L, 35L, 
    35L, 27L, 29L, 28L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 18L, 
    18L, 23L, 23L, 22L, 22L, 21L, 21L, 19L, 19L, 20L, 20L, 14L, 
    14L, 14L, 14L, 34L, 34L, 34L, 14L, 14L, 14L, 14L, 14L, 14L, 
    14L, 14L, 14L, 14L, 14L, 41L, 41L, 41L, 33L, 33L, 36L, 36L, 
    36L, 24L, 24L, 3L, 3L, 3L, 3L, 26L, 26L, 6L, 6L, 25L, 25L, 
    4L, 4L, 13L, 30L, 30L, 32L, 32L, 31L, 31L, 37L, 37L, 37L, 
    37L, 8L, 37L, 37L, 37L, 37L, 37L, 37L, 37L, 37L, 8L, 8L, 
    37L, 37L, 37L, 37L, 8L, 8L, 37L, 37L, 37L, 37L, 8L, 8L, 8L, 
    37L, 37L, 37L, 37L, 8L, 37L, 37L, 37L, 37L, 8L, 37L, 37L, 
    37L, 37L, 8L, 8L, 8L, 39L, 39L, 39L, 10L, 10L, 37L, 37L, 
    37L, 37L, 8L, 37L, 37L, 37L, 8L, 8L, 38L, 38L, 38L, 9L, 9L, 
    37L, 37L, 37L, 37L, 8L, 37L, 37L, 37L, 37L, 8L, 8L, 8L, 37L, 
    37L, 37L, 37L, 8L, 37L, 37L, 37L, 37L, 8L, 8L, 8L, 16L, 16L, 
    2L, 2L, 17L, 17L, 2L, 2L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 
    14L, 14L, 14L, 14L, 14L, 14L, 1L, 1L, 1L, 1L, 15L, 15L, 14L, 
    14L, 40L, 40L, 40L, 40L, 11L, 11L, 12L, 12L, 7L, 7L, 7L, 
    7L, 1L, 1L, 1L, 5L, 6L), .Label = c("F990-EZ-PART-04-COL-A", 
    "F990-EZ-PART-04-COL-B", "F990-EZ-PART-04-COL-C", "F990-EZ-PART-04-COL-C-LINE-2", 
    "F990-EZ-PART-04-COL-D", "F990-EZ-PART-04-COL-E", "F990-EZ-PART-04-LINE-00", 
    "F990-EZ-PART-06-LINE-51-COL-A", "F990-EZ-PART-06-LINE-51-COL-B", 
    "F990-EZ-PART-06-LINE-51-COL-C", "F990-EZ-PART-06-LINE-51-COL-D", 
    "F990-PC-PART-07-LINE-00", "F990-PC-PART-07-LINE-105", "F990-PC-PART-07-SECTION-A-COL-A", 
    "F990-PC-PART-07-SECTION-A-COL-A-LINE-2", "F990-PC-PART-07-SECTION-A-COL-B-LINE-1", 
    "F990-PC-PART-07-SECTION-A-COL-B-LINE-2", "F990-PC-PART-07-SECTION-A-COL-C", 
    "F990-PC-PART-07-SECTION-A-COL-C-POS-1", "F990-PC-PART-07-SECTION-A-COL-C-POS-2", 
    "F990-PC-PART-07-SECTION-A-COL-C-POS-3", "F990-PC-PART-07-SECTION-A-COL-C-POS-4", 
    "F990-PC-PART-07-SECTION-A-COL-C-POS-5", "F990-PC-PART-07-SECTION-A-COL-D", 
    "F990-PC-PART-07-SECTION-A-COL-E", "F990-PC-PART-07-SECTION-A-COL-F", 
    "F990-PC-PART-07-SECTION-A-LINE-1b-COL-D", "F990-PC-PART-07-SECTION-A-LINE-1b-COL-E", 
    "F990-PC-PART-07-SECTION-A-LINE-1b-COL-F", "F990-PC-PART-07-SECTION-A-LINE-1d-COL-D", 
    "F990-PC-PART-07-SECTION-A-LINE-1d-COL-E", "F990-PC-PART-07-SECTION-A-LINE-1d-COL-F", 
    "F990-PC-PART-07-SECTION-A-LINE-2", "F990-PC-PART-07-SECTION-A-LINE-3", 
    "F990-PC-PART-07-SECTION-A-LINE-4", "F990-PC-PART-07-SECTION-A-LINE-5", 
    "F990-PC-PART-07-SECTION-B-LINE-01-COL-A", "F990-PC-PART-07-SECTION-B-LINE-01-COL-B", 
    "F990-PC-PART-07-SECTION-B-LINE-01-COL-C", "F990-PC-PART-07-SECTION-B-LINE-02", 
    "F990-PC-PART-07_SECTION-A-LINE-1a"), class = "factor"), 
    form = structure(c(2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 
    2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 
    2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 
    2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 1L, 1L, 
    1L, 1L, 2L, 2L, 1L, 1L, 2L, 2L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 
    2L, 2L, 2L, 2L, 2L, 2L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 
    1L, 1L, 2L, 2L, 2L, 2L, 1L, 1L, 2L, 2L, 2L, 2L, 1L, 1L, 1L, 
    2L, 2L, 2L, 2L, 1L, 2L, 2L, 2L, 2L, 1L, 2L, 2L, 2L, 2L, 1L, 
    1L, 1L, 2L, 2L, 2L, 1L, 1L, 2L, 2L, 2L, 2L, 1L, 2L, 2L, 2L, 
    1L, 1L, 2L, 2L, 2L, 1L, 1L, 2L, 2L, 2L, 2L, 1L, 2L, 2L, 2L, 
    2L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 1L, 2L, 2L, 2L, 2L, 1L, 1L, 
    1L, 2L, 2L, 1L, 1L, 2L, 2L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 
    2L, 2L, 2L, 2L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 
    2L, 2L, 2L, 2L, 1L, 1L, 2L, 2L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
    1L, 1L), .Label = c("F990-EZ", "F990-PC"), class = "factor"), 
    part = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
    1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
    1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
    1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
    1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
    1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
    1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
    1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
    1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
    1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
    1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
    1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
    1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
    1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
    1L, 1L), .Label = "PART-07", class = "factor"), line_number = structure(c(27L, 
    28L, 29L, 22L, 24L, 23L, 15L, 15L, 15L, 15L, 15L, 15L, 15L, 
    17L, 17L, 17L, 17L, 17L, 17L, 17L, 17L, 17L, 17L, 17L, 17L, 
    15L, 15L, 15L, 15L, 26L, 26L, 26L, 15L, 15L, 15L, 15L, 15L, 
    15L, 15L, 15L, 15L, 15L, 15L, 21L, 21L, 21L, 25L, 25L, 28L, 
    28L, 28L, 18L, 18L, 4L, 4L, 4L, 4L, 20L, 20L, 6L, 6L, 19L, 
    19L, 4L, 4L, 1L, 8L, 8L, 10L, 10L, 9L, 9L, 30L, 30L, 30L, 
    30L, 11L, 30L, 30L, 30L, 30L, 30L, 30L, 30L, 30L, 11L, 11L, 
    30L, 30L, 30L, 30L, 11L, 11L, 30L, 30L, 30L, 30L, 11L, 11L, 
    11L, 30L, 30L, 30L, 30L, 11L, 30L, 30L, 30L, 30L, 11L, 30L, 
    30L, 30L, 30L, 11L, 11L, 11L, 32L, 32L, 32L, 13L, 13L, 30L, 
    30L, 30L, 30L, 11L, 30L, 30L, 30L, 11L, 11L, 31L, 31L, 31L, 
    12L, 12L, 30L, 30L, 30L, 30L, 11L, 30L, 30L, 30L, 30L, 11L, 
    11L, 11L, 30L, 30L, 30L, 30L, 11L, 30L, 30L, 30L, 30L, 11L, 
    11L, 11L, 16L, 16L, 3L, 3L, 16L, 16L, 3L, 3L, 15L, 15L, 15L, 
    15L, 15L, 15L, 15L, 15L, 15L, 15L, 15L, 15L, 15L, 2L, 2L, 
    2L, 2L, 15L, 15L, 15L, 15L, 33L, 33L, 33L, 33L, 14L, 14L, 
    7L, 7L, 7L, 7L, 7L, 7L, 2L, 2L, 2L, 5L, 6L), .Label = c("", 
    "Column (a)", "Column (b)", "Column (c)", "Column (d)", "Column (e)", 
    "Line 00", "Line 1d D", "Line 1d E", "Line 1d F", "Line 51; Line 51 Column (a)", 
    "Line 51; Line 51 Column (b)", "Line 51; Line 51 Column (c)", 
    "Line 51d", "Section A Column (a)", "Section A Column (b)", 
    "Section A Column (c)", "Section A Column (d)", "Section A Column (e)", 
    "Section A Column (f)", "Section A Line 1a", "Section A Line 1b Col D", 
    "Section A Line 1b Col E", "Section A Line 1b Col F", "Section A Line 2", 
    "Section A Line 3", "Section A Line 4", "Section A Line 5", 
    "Section A Line 6", "Section B Column (a)", "Section B Column (b)", 
    "Section B Column (c)", "Section B Line 2"), class = "factor"), 
    scope = structure(c(2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 
    2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 
    2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 
    2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 
    3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 2L, 3L, 3L, 3L, 3L, 
    3L, 3L, 3L, 3L, 3L, 2L, 3L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 2L, 
    3L, 3L, 3L, 3L, 3L, 2L, 3L, 3L, 3L, 3L, 3L, 2L, 3L, 3L, 3L, 
    3L, 3L, 3L, 2L, 3L, 3L, 3L, 3L, 2L, 3L, 3L, 3L, 3L, 2L, 3L, 
    3L, 3L, 3L, 3L, 2L, 3L, 3L, 3L, 3L, 3L, 2L, 3L, 3L, 3L, 2L, 
    3L, 3L, 3L, 3L, 2L, 3L, 3L, 3L, 3L, 3L, 2L, 3L, 3L, 3L, 2L, 
    2L, 3L, 3L, 3L, 3L, 3L, 3L, 2L, 3L, 3L, 3L, 3L, 2L, 3L, 3L, 
    3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 2L, 3L, 
    3L, 3L, 3L, 2L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 
    2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 
    1L, 3L), .Label = c("EZ", "PC", "PZ"), class = "factor"), 
    data_type_xsd = structure(c(1L, 2L, 2L, 1L, 1L, 1L, 1L, 1L, 
    14L, 14L, 1L, 6L, 1L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 
    5L, 5L, 5L, 1L, 13L, 1L, 19L, 1L, 2L, 2L, 1L, 1L, 1L, 1L, 
    1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 5L, 5L, 8L, 8L, 2L, 2L, 1L, 
    17L, 17L, 1L, 1L, 16L, 16L, 17L, 17L, 1L, 1L, 17L, 17L, 1L, 
    1L, 17L, 17L, 17L, 17L, 17L, 17L, 17L, 14L, 14L, 14L, 1L, 
    14L, 14L, 14L, 14L, 1L, 14L, 14L, 14L, 1L, 14L, 14L, 14L, 
    14L, 14L, 1L, 14L, 14L, 3L, 3L, 3L, 1L, 3L, 3L, 3L, 4L, 4L, 
    4L, 1L, 4L, 15L, 15L, 15L, 1L, 15L, 6L, 6L, 6L, 1L, 6L, 6L, 
    6L, 17L, 17L, 1L, 16L, 16L, 7L, 7L, 7L, 1L, 7L, 12L, 12L, 
    1L, 12L, 12L, 11L, 11L, 1L, 11L, 11L, 15L, 15L, 15L, 1L, 
    15L, 13L, 13L, 13L, 1L, 13L, 13L, 13L, 15L, 15L, 15L, 1L, 
    15L, 19L, 19L, 19L, 1L, 19L, 19L, 19L, 9L, 18L, 1L, 10L, 
    1L, 18L, 1L, 18L, 1L, 3L, 3L, 3L, 3L, 1L, 4L, 4L, 4L, 1L, 
    1L, 12L, 12L, 1L, 1L, 12L, 12L, 11L, 11L, 1L, 1L, 8L, 1L, 
    1L, 8L, 9L, 9L, 5L, 5L, 5L, 5L, 1L, 1L, 1L, 1L, 11L, 16L, 
    16L), .Label = c("", "BooleanType", "BusinessNameLine1Type", 
    "BusinessNameLine2Type", "CheckboxType", "CityType", "CountryType", 
    "CountType", "IntegerNNType", "LargeRatioType", "LineExplanationType", 
    "PersonNameType", "StateType", "StreetAddressType", "TextType", 
    "USAmountNNType", "USAmountType", "xsd:decimal", "ZIPCodeType"
    ), class = "factor"), data_type_simple = structure(c(2L, 
    2L, 2L, 3L, 3L, 3L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 2L, 2L, 2L, 
    2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 4L, 4L, 4L, 4L, 2L, 2L, 
    2L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 2L, 2L, 2L, 
    3L, 3L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 
    3L, 3L, 3L, 3L, 1L, 3L, 3L, 3L, 3L, 3L, 3L, 4L, 4L, 4L, 4L, 
    4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 
    4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 
    4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 3L, 3L, 3L, 3L, 
    3L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 
    4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 
    4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 3L, 3L, 3L, 3L, 3L, 
    3L, 3L, 3L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 
    4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 3L, 3L, 3L, 3L, 3L, 3L, 
    2L, 2L, 2L, 2L, 2L, 2L, 4L, 4L, 4L, 3L, 3L), .Label = c("", 
    "boolean", "numeric", "text"), class = "factor"), cardinality = structure(c(3L, 
    3L, 3L, 3L, 3L, 3L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 
    2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 
    3L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 
    3L, 3L, 3L, 3L, 3L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 
    2L, 2L, 2L, 2L, 1L, 3L, 3L, 3L, 3L, 3L, 3L, 2L, 2L, 2L, 2L, 
    2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 
    2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 
    2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 
    2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 
    2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 
    2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 
    2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 
    2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 3L, 
    2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L), .Label = c("", 
    "MANY", "ONE"), class = "factor"), rdb_table = structure(c(2L, 
    2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 
    3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 2L, 2L, 
    2L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 2L, 2L, 2L, 
    2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 
    3L, 3L, 3L, 3L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 4L, 4L, 4L, 4L, 
    4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 
    4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 
    4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 
    4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 
    4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 
    4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 3L, 3L, 3L, 3L, 3L, 
    3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 
    3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 2L, 2L, 2L, 2L, 2L, 2L, 
    5L, 5L, 5L, 5L, 5L, 5L, 3L, 3L, 3L, 3L, 3L), .Label = c("", 
    "F9-P07-TABLE-00-COMP-OVERVIEW", "F9-P07-TABLE-01-DTK-COMPENSATION", 
    "F9-P07-TABLE-02-CONTXR-COMPENSATION", "F9-P07-TABLE-03-SCHED-O-REPORT"
    ), class = "factor"), required = c(NA, FALSE, FALSE, NA, 
    NA, NA, NA, NA, NA, NA, NA, NA, NA, FALSE, FALSE, FALSE, 
    FALSE, FALSE, NA, NA, FALSE, FALSE, NA, FALSE, NA, NA, NA, 
    NA, NA, NA, FALSE, FALSE, NA, NA, NA, NA, NA, NA, NA, NA, 
    NA, NA, NA, NA, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, 
    NA, FALSE, FALSE, NA, NA, FALSE, FALSE, FALSE, FALSE, NA, 
    NA, FALSE, FALSE, NA, NA, NA, FALSE, FALSE, FALSE, FALSE, 
    FALSE, FALSE, FALSE, FALSE, FALSE, NA, FALSE, FALSE, FALSE, 
    FALSE, NA, FALSE, FALSE, FALSE, NA, FALSE, FALSE, FALSE, 
    FALSE, FALSE, NA, FALSE, FALSE, FALSE, FALSE, FALSE, NA, 
    FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, NA, FALSE, FALSE, 
    FALSE, FALSE, NA, FALSE, FALSE, FALSE, FALSE, NA, FALSE, 
    FALSE, FALSE, FALSE, FALSE, NA, FALSE, FALSE, FALSE, FALSE, 
    FALSE, NA, FALSE, FALSE, FALSE, NA, FALSE, FALSE, FALSE, 
    FALSE, NA, FALSE, FALSE, FALSE, FALSE, FALSE, NA, FALSE, 
    FALSE, FALSE, NA, NA, FALSE, FALSE, FALSE, FALSE, FALSE, 
    FALSE, NA, FALSE, FALSE, FALSE, FALSE, NA, FALSE, FALSE, 
    FALSE, FALSE, FALSE, NA, FALSE, NA, FALSE, NA, FALSE, NA, 
    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, FALSE, FALSE, NA, 
    NA, FALSE, FALSE, FALSE, FALSE, NA, NA, FALSE, NA, NA, FALSE, 
    FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, NA, NA, NA, NA, 
    FALSE, FALSE, FALSE), versions = structure(c(1L, 6L, 2L, 
    1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 6L, 2L, 2L, 6L, 2L, 
    6L, 6L, 2L, 2L, 6L, 2L, 6L, 1L, 1L, 1L, 1L, 1L, 6L, 2L, 1L, 
    1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 6L, 6L, 2L, 
    2L, 6L, 1L, 2L, 6L, 1L, 1L, 6L, 4L, 2L, 6L, 1L, 1L, 2L, 6L, 
    1L, 1L, 1L, 3L, 6L, 3L, 6L, 3L, 6L, 2L, 5L, 7L, 1L, 2L, 2L, 
    5L, 7L, 1L, 2L, 5L, 7L, 1L, 5L, 2L, 2L, 5L, 7L, 1L, 7L, 2L, 
    2L, 5L, 7L, 1L, 1L, 1L, 1L, 2L, 5L, 7L, 1L, 1L, 2L, 5L, 7L, 
    1L, 2L, 2L, 5L, 7L, 1L, 5L, 7L, 2L, 2L, 6L, 1L, 6L, 2L, 2L, 
    5L, 7L, 1L, 2L, 2L, 6L, 1L, 6L, 2L, 2L, 6L, 1L, 6L, 2L, 2L, 
    5L, 7L, 1L, 2L, 2L, 5L, 7L, 1L, 5L, 7L, 2L, 2L, 7L, 5L, 1L, 
    2L, 2L, 7L, 5L, 1L, 7L, 5L, 2L, 2L, 6L, 1L, 4L, 4L, 6L, 1L, 
    6L, 1L, 2L, 5L, 7L, 1L, 1L, 2L, 5L, 7L, 1L, 1L, 2L, 6L, 1L, 
    1L, 6L, 4L, 2L, 6L, 1L, 1L, 6L, 1L, 1L, 2L, 6L, 2L, 3L, 6L, 
    3L, 6L, 1L, 1L, 1L, 1L, 6L, 6L, 6L), .Label = c("", "2009v1.0;2009v1.1;2009v1.2;2009v1.3;2009v1.4;2009v1.7;2010v3.2;2010v3.4;2010v3.6;2010v3.7;2011v1.2;2011v1.3;2011v1.4;2011v1.5;2012v2.0;2012v2.1;2012v2.2;2012v2.3;2012v3.0", 
    "2010v3.2;2010v3.4;2010v3.6;2010v3.7;2011v1.2;2011v1.3;2011v1.4;2011v1.5;2012v2.0;2012v2.1;2012v2.2;2012v2.3;2012v3.0", 
    "2012v2.0;2012v2.1;2012v2.2;2012v2.3;2012v3.0", "2013v3.0;2013v3.1;2013v4.0", 
    "2013v3.0;2013v3.1;2013v4.0;2014v5.0;2014v6.0;2015v2.0;2015v2.1;2015v3.0;2016v3.0", 
    "2014v5.0;2014v6.0;2015v2.0;2015v2.1;2015v3.0;2016v3.0"), class = "factor"), 
    latest_version = c(NA, 2016L, 2012L, NA, NA, NA, NA, NA, 
    NA, NA, NA, NA, NA, 2016L, 2012L, 2012L, 2016L, 2012L, 2016L, 
    2016L, 2012L, 2012L, 2016L, 2012L, 2016L, NA, NA, NA, NA, 
    NA, 2016L, 2012L, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
    NA, NA, 2012L, 2016L, 2016L, 2012L, 2012L, 2016L, NA, 2012L, 
    2016L, NA, NA, 2016L, 2012L, 2012L, 2016L, NA, NA, 2012L, 
    2016L, NA, NA, NA, 2012L, 2016L, 2012L, 2016L, 2012L, 2016L, 
    2012L, 2013L, 2016L, NA, 2012L, 2012L, 2013L, 2016L, NA, 
    2012L, 2013L, 2016L, NA, 2013L, 2012L, 2012L, 2013L, 2016L, 
    NA, 2016L, 2012L, 2012L, 2013L, 2016L, NA, NA, NA, NA, 2012L, 
    2013L, 2016L, NA, NA, 2012L, 2013L, 2016L, NA, 2012L, 2012L, 
    2013L, 2016L, NA, 2013L, 2016L, 2012L, 2012L, 2016L, NA, 
    2016L, 2012L, 2012L, 2013L, 2016L, NA, 2012L, 2012L, 2016L, 
    NA, 2016L, 2012L, 2012L, 2016L, NA, 2016L, 2012L, 2012L, 
    2013L, 2016L, NA, 2012L, 2012L, 2013L, 2016L, NA, 2013L, 
    2016L, 2012L, 2012L, 2016L, 2013L, NA, 2012L, 2012L, 2016L, 
    2013L, NA, 2016L, 2013L, 2012L, 2012L, 2016L, NA, 2012L, 
    2012L, 2016L, NA, 2016L, NA, 2012L, 2013L, 2016L, NA, NA, 
    2012L, 2013L, 2016L, NA, NA, 2012L, 2016L, NA, NA, 2016L, 
    2012L, 2012L, 2016L, NA, NA, 2016L, NA, NA, 2012L, 2016L, 
    2012L, 2012L, 2016L, 2012L, 2016L, NA, NA, NA, NA, 2016L, 
    2016L, 2016L), duplicated = c(NA, FALSE, FALSE, NA, NA, NA, 
    NA, NA, NA, NA, NA, NA, NA, FALSE, FALSE, FALSE, FALSE, FALSE, 
    FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, NA, NA, 
    NA, NA, NA, FALSE, FALSE, NA, NA, NA, NA, NA, NA, NA, NA, 
    NA, NA, NA, NA, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, 
    NA, FALSE, FALSE, NA, NA, FALSE, TRUE, FALSE, FALSE, NA, 
    NA, FALSE, FALSE, NA, NA, NA, TRUE, FALSE, TRUE, FALSE, TRUE, 
    FALSE, FALSE, FALSE, FALSE, NA, FALSE, FALSE, FALSE, FALSE, 
    NA, FALSE, FALSE, FALSE, NA, FALSE, FALSE, FALSE, FALSE, 
    FALSE, NA, FALSE, FALSE, FALSE, FALSE, FALSE, NA, NA, NA, 
    NA, FALSE, FALSE, FALSE, NA, NA, FALSE, FALSE, FALSE, NA, 
    FALSE, FALSE, FALSE, FALSE, NA, FALSE, FALSE, FALSE, FALSE, 
    FALSE, NA, FALSE, FALSE, FALSE, FALSE, FALSE, NA, FALSE, 
    FALSE, FALSE, NA, FALSE, FALSE, FALSE, FALSE, NA, FALSE, 
    FALSE, FALSE, FALSE, FALSE, NA, FALSE, FALSE, FALSE, FALSE, 
    NA, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, NA, FALSE, 
    FALSE, FALSE, FALSE, NA, FALSE, FALSE, FALSE, FALSE, FALSE, 
    NA, TRUE, FALSE, FALSE, NA, FALSE, NA, FALSE, FALSE, FALSE, 
    NA, NA, FALSE, FALSE, FALSE, NA, NA, FALSE, FALSE, NA, NA, 
    FALSE, TRUE, FALSE, FALSE, NA, NA, FALSE, NA, NA, FALSE, 
    FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, NA, NA, NA, NA, 
    FALSE, FALSE, FALSE), current_version = c(NA, TRUE, TRUE, 
    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, TRUE, TRUE, TRUE, 
    TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, NA, 
    NA, NA, NA, NA, TRUE, TRUE, NA, NA, NA, NA, NA, NA, NA, NA, 
    NA, NA, NA, NA, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, NA, TRUE, 
    TRUE, NA, NA, TRUE, TRUE, TRUE, TRUE, NA, NA, TRUE, TRUE, 
    NA, NA, NA, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, 
    TRUE, NA, TRUE, TRUE, TRUE, TRUE, NA, TRUE, TRUE, TRUE, NA, 
    TRUE, TRUE, TRUE, TRUE, TRUE, NA, TRUE, TRUE, TRUE, TRUE, 
    TRUE, NA, NA, NA, NA, TRUE, TRUE, TRUE, NA, NA, TRUE, TRUE, 
    TRUE, NA, TRUE, TRUE, TRUE, TRUE, NA, TRUE, TRUE, TRUE, TRUE, 
    TRUE, NA, TRUE, TRUE, TRUE, TRUE, TRUE, NA, TRUE, TRUE, TRUE, 
    NA, TRUE, TRUE, TRUE, TRUE, NA, TRUE, TRUE, TRUE, TRUE, TRUE, 
    NA, TRUE, TRUE, TRUE, TRUE, NA, TRUE, TRUE, TRUE, TRUE, TRUE, 
    TRUE, NA, TRUE, TRUE, TRUE, TRUE, NA, TRUE, TRUE, TRUE, TRUE, 
    TRUE, NA, TRUE, TRUE, TRUE, NA, TRUE, NA, TRUE, TRUE, TRUE, 
    NA, NA, TRUE, TRUE, TRUE, NA, NA, TRUE, TRUE, NA, NA, TRUE, 
    TRUE, TRUE, TRUE, NA, NA, TRUE, NA, NA, TRUE, TRUE, TRUE, 
    TRUE, TRUE, TRUE, TRUE, NA, NA, NA, NA, TRUE, TRUE, TRUE), 
    production_rule = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
    NA, NA), validated = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, 
    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
    NA, NA, NA)), .Names = c("xpath", "variable_name_new", "nch", 
"description", "location_code_xsd", "location_code", "form", 
"part", "line_number", "scope", "data_type_xsd", "data_type_simple", 
"cardinality", "rdb_table", "required", "versions", "latest_version", 
"duplicated", "current_version", "production_rule", "validated"
), class = "data.frame", row.names = c(NA, -207L))






### CONCORDANCE SCHEDULE J

schedj <-
structure(list(xpath = c("/Return/ReturnData/IRS990ScheduleJ/ClubDuesOrFees", 
"/Return/ReturnData/IRS990ScheduleJ/ClubDuesOrFeesInd", "/Return/ReturnData/IRS990ScheduleJ/Form990ScheduleJPartI/ClubDuesOrFees", 
"/Return/ReturnData/IRS990ScheduleJ/TravelForCompanions", "/Return/ReturnData/IRS990ScheduleJ/TravelForCompanionsInd", 
"/Return/ReturnData/IRS990ScheduleJ/Form990ScheduleJPartI/TravelForCompanions", 
"/Return/ReturnData/IRS990ScheduleJ/DiscretionarySpendingAccount", 
"/Return/ReturnData/IRS990ScheduleJ/DiscretionarySpendingAcctInd", 
"/Return/ReturnData/IRS990ScheduleJ/Form990ScheduleJPartI/DiscretionarySpendingAccount", 
"/Return/ReturnData/IRS990ScheduleJ/FirstClassOrCharterTravel", 
"/Return/ReturnData/IRS990ScheduleJ/FirstClassOrCharterTravelInd", 
"/Return/ReturnData/IRS990ScheduleJ/Form990ScheduleJPartI/FirstClassOrCharterTravel", 
"/Return/ReturnData/IRS990ScheduleJ/PaymentsForUseOfResidence", 
"/Return/ReturnData/IRS990ScheduleJ/PaymentsForUseOfResidenceInd", 
"/Return/ReturnData/IRS990ScheduleJ/Form990ScheduleJPartI/PaymentsForUseOfResidence", 
"/Return/ReturnData/IRS990ScheduleJ/HousingAllowanceOrResidence", 
"/Return/ReturnData/IRS990ScheduleJ/HousingAllowanceOrResidenceInd", 
"/Return/ReturnData/IRS990ScheduleJ/Form990ScheduleJPartI/HousingAllowanceOrResidence", 
"/Return/ReturnData/IRS990ScheduleJ/IdemnificationGrossUpPayments", 
"/Return/ReturnData/IRS990ScheduleJ/IdemnificationGrossUpPmtsInd", 
"/Return/ReturnData/IRS990ScheduleJ/Form990ScheduleJPartI/IdemnificationGrossUpPayments", 
"/Return/ReturnData/IRS990ScheduleJ/PersonalServices", "/Return/ReturnData/IRS990ScheduleJ/PersonalServicesInd", 
"/Return/ReturnData/IRS990ScheduleJ/Form990ScheduleJPartI/PersonalServices", 
"/Return/ReturnData/IRS990ScheduleJ/WrittenPolicyRefTAndEExpnssInd", 
"/Return/ReturnData/IRS990ScheduleJ/WrittenPolicyReTAndEExpenses", 
"/Return/ReturnData/IRS990ScheduleJ/Form990ScheduleJPartI/WrittenPolicyReTAndEExpenses", 
"/Return/ReturnData/IRS990ScheduleJ/SubstantiationRequired", 
"/Return/ReturnData/IRS990ScheduleJ/SubstantiationRequiredInd", 
"/Return/ReturnData/IRS990ScheduleJ/Form990ScheduleJPartI/SubstantiationRequired", 
"/Return/ReturnData/IRS990ScheduleJ/BoardOrCommitteeApproval", 
"/Return/ReturnData/IRS990ScheduleJ/BoardOrCommitteeApprovalInd", 
"/Return/ReturnData/IRS990ScheduleJ/Form990ScheduleJPartI/BoardOrCommitteeApproval", 
"/Return/ReturnData/IRS990ScheduleJ/CompensationCommittee", "/Return/ReturnData/IRS990ScheduleJ/CompensationCommitteeInd", 
"/Return/ReturnData/IRS990ScheduleJ/Form990ScheduleJPartI/CompensationCommittee", 
"/Return/ReturnData/IRS990ScheduleJ/CompensationSurvey", "/Return/ReturnData/IRS990ScheduleJ/CompensationSurveyInd", 
"/Return/ReturnData/IRS990ScheduleJ/Form990ScheduleJPartI/CompensationSurvey", 
"/Return/ReturnData/IRS990ScheduleJ/IndependentConsultant", "/Return/ReturnData/IRS990ScheduleJ/IndependentConsultantInd", 
"/Return/ReturnData/IRS990ScheduleJ/Form990ScheduleJPartI/IndependentConsultant", 
"/Return/ReturnData/IRS990ScheduleJ/WrittenEmploymentContract", 
"/Return/ReturnData/IRS990ScheduleJ/WrittenEmploymentContractInd", 
"/Return/ReturnData/IRS990ScheduleJ/Form990ScheduleJPartI/WrittenEmploymentContract", 
"/Return/ReturnData/IRS990ScheduleJ/Form990OfOtherOrganizations", 
"/Return/ReturnData/IRS990ScheduleJ/Form990OfOtherOrganizationsInd", 
"/Return/ReturnData/IRS990ScheduleJ/Form990ScheduleJPartI/Form990OfOtherOrganizations", 
"/Return/ReturnData/IRS990ScheduleJ/SeverancePayment", "/Return/ReturnData/IRS990ScheduleJ/SeverancePaymentInd", 
"/Return/ReturnData/IRS990ScheduleJ/Form990ScheduleJPartI/SeverancePayment", 
"/Return/ReturnData/IRS990ScheduleJ/SupplementalNonqualRetirePlan", 
"/Return/ReturnData/IRS990ScheduleJ/SupplementalNonqualRtrPlanInd", 
"/Return/ReturnData/IRS990ScheduleJ/Form990ScheduleJPartI/SupplementalNonqualRetirePlan", 
"/Return/ReturnData/IRS990ScheduleJ/EquityBasedCompArrangement", 
"/Return/ReturnData/IRS990ScheduleJ/EquityBasedCompArrngmInd", 
"/Return/ReturnData/IRS990ScheduleJ/Form990ScheduleJPartI/EquityBasedCompArrangement", 
"/Return/ReturnData/IRS990ScheduleJ/CompBasedOnRevenueOfFilingOrg", 
"/Return/ReturnData/IRS990ScheduleJ/CompBasedOnRevenueOfFlngOrgInd", 
"/Return/ReturnData/IRS990ScheduleJ/Form990ScheduleJPartI/CompBasedOnRevenueOfFilingOrg", 
"/Return/ReturnData/IRS990ScheduleJ/CompBasedOnRevenueRelatedOrgs", 
"/Return/ReturnData/IRS990ScheduleJ/CompBsdOnRevRelatedOrgsInd", 
"/Return/ReturnData/IRS990ScheduleJ/Form990ScheduleJPartI/CompBasedOnRevenueRelatedOrgs", 
"/Return/ReturnData/IRS990ScheduleJ/CompBasedNetEarningsFilingOrg", 
"/Return/ReturnData/IRS990ScheduleJ/CompBsdNetEarnsFlngOrgInd", 
"/Return/ReturnData/IRS990ScheduleJ/Form990ScheduleJPartI/CompBasedNetEarningsFilingOrg", 
"/Return/ReturnData/IRS990ScheduleJ/CompBasedNetEarningsRelateOrgs", 
"/Return/ReturnData/IRS990ScheduleJ/CompBsdNetEarnsRltdOrgsInd", 
"/Return/ReturnData/IRS990ScheduleJ/Form990ScheduleJPartI/CompBasedNetEarningsRelateOrgs", 
"/Return/ReturnData/IRS990ScheduleJ/AnyNonFixedPayments", "/Return/ReturnData/IRS990ScheduleJ/AnyNonFixedPaymentsInd", 
"/Return/ReturnData/IRS990ScheduleJ/Form990ScheduleJPartI/AnyNonFixedPayments", 
"/Return/ReturnData/IRS990ScheduleJ/InitialContractException", 
"/Return/ReturnData/IRS990ScheduleJ/InitialContractExceptionInd", 
"/Return/ReturnData/IRS990ScheduleJ/Form990ScheduleJPartI/InitialContractException", 
"/Return/ReturnData/IRS990ScheduleJ/RebuttablePresumptionProcedure", 
"/Return/ReturnData/IRS990ScheduleJ/RebuttablePresumptionProcInd", 
"/Return/ReturnData/IRS990ScheduleJ/Form990ScheduleJPartII/NamePerson", 
"/Return/ReturnData/IRS990ScheduleJ/RltdOrgOfficerTrstKeyEmplGrp/PersonNm", 
"/Return/ReturnData/IRS990ScheduleJ/Form990ScheduleJPartII/NameBusiness/BusinessNameLine1", 
"/Return/ReturnData/IRS990ScheduleJ/RltdOrgOfficerTrstKeyEmplGrp/BusinessName/BusinessNameLine1", 
"/Return/ReturnData/IRS990ScheduleJ/RltdOrgOfficerTrstKeyEmplGrp/BusinessName/BusinessNameLine1Txt", 
"/Return/ReturnData/IRS990ScheduleJ/Form990ScheduleJPartII/NameBusiness/BusinessNameLine2", 
"/Return/ReturnData/IRS990ScheduleJ/RltdOrgOfficerTrstKeyEmplGrp/BusinessName/BusinessNameLine2", 
"/Return/ReturnData/IRS990ScheduleJ/RltdOrgOfficerTrstKeyEmplGrp/BusinessName/BusinessNameLine2Txt", 
"/Return/ReturnData/IRS990ScheduleJ/Form990ScheduleJPartII/Title", 
"/Return/ReturnData/IRS990ScheduleJ/RltdOrgOfficerTrstKeyEmplGrp/TitleTxt", 
"/Return/ReturnData/IRS990ScheduleJ/Form990ScheduleJPartII/BaseCompensationFilingOrg", 
"/Return/ReturnData/IRS990ScheduleJ/RltdOrgOfficerTrstKeyEmplGrp/BaseCompensationFilingOrgAmt", 
"/Return/ReturnData/IRS990ScheduleJ/Form990ScheduleJPartII/CompBasedOnRelatedOrgs", 
"/Return/ReturnData/IRS990ScheduleJ/RltdOrgOfficerTrstKeyEmplGrp/CompensationBasedOnRltdOrgsAmt", 
"/Return/ReturnData/IRS990ScheduleJ/Form990ScheduleJPartII/BonusFilingOrg", 
"/Return/ReturnData/IRS990ScheduleJ/RltdOrgOfficerTrstKeyEmplGrp/BonusFilingOrganizationAmount", 
"/Return/ReturnData/IRS990ScheduleJ/Form990ScheduleJPartII/BonusRelatedOrgs", 
"/Return/ReturnData/IRS990ScheduleJ/RltdOrgOfficerTrstKeyEmplGrp/BonusRelatedOrganizationsAmt", 
"/Return/ReturnData/IRS990ScheduleJ/Form990ScheduleJPartII/OtherCompensationFilingOrg", 
"/Return/ReturnData/IRS990ScheduleJ/RltdOrgOfficerTrstKeyEmplGrp/OtherCompensationFilingOrgAmt", 
"/Return/ReturnData/IRS990ScheduleJ/Form990ScheduleJPartII/OtherCompensationRelatedOrgs", 
"/Return/ReturnData/IRS990ScheduleJ/RltdOrgOfficerTrstKeyEmplGrp/OtherCompensationRltdOrgsAmt", 
"/Return/ReturnData/IRS990ScheduleJ/Form990ScheduleJPartII/DeferredCompFilingOrg", 
"/Return/ReturnData/IRS990ScheduleJ/RltdOrgOfficerTrstKeyEmplGrp/DeferredCompensationFlngOrgAmt", 
"/Return/ReturnData/IRS990ScheduleJ/Form990ScheduleJPartII/DeferredCompRelatedOrgs", 
"/Return/ReturnData/IRS990ScheduleJ/RltdOrgOfficerTrstKeyEmplGrp/DeferredCompRltdOrgsAmt", 
"/Return/ReturnData/IRS990ScheduleJ/Form990ScheduleJPartII/NontaxableBenefitsFilingOrg", 
"/Return/ReturnData/IRS990ScheduleJ/RltdOrgOfficerTrstKeyEmplGrp/NontaxableBenefitsFilingOrgAmt", 
"/Return/ReturnData/IRS990ScheduleJ/Form990ScheduleJPartII/NontaxableBenefitsRelatedOrgs", 
"/Return/ReturnData/IRS990ScheduleJ/RltdOrgOfficerTrstKeyEmplGrp/NontaxableBenefitsRltdOrgsAmt", 
"/Return/ReturnData/IRS990ScheduleJ/Form990ScheduleJPartII/TotalCompensationFilingOrg", 
"/Return/ReturnData/IRS990ScheduleJ/RltdOrgOfficerTrstKeyEmplGrp/TotalCompensationFilingOrgAmt", 
"/Return/ReturnData/IRS990ScheduleJ/Form990ScheduleJPartII/TotalCompensationRelatedOrgs", 
"/Return/ReturnData/IRS990ScheduleJ/RltdOrgOfficerTrstKeyEmplGrp/TotalCompensationRltdOrgsAmt", 
"/Return/ReturnData/IRS990ScheduleJ/Form990ScheduleJPartII/CompReportPrior990FilingOrg", 
"/Return/ReturnData/IRS990ScheduleJ/RltdOrgOfficerTrstKeyEmplGrp/CompReportPrior990FilingOrgAmt", 
"/Return/ReturnData/IRS990ScheduleJ/Form990ScheduleJPartII/CompReportPrior990RelatedOrgs", 
"/Return/ReturnData/IRS990ScheduleJ/RltdOrgOfficerTrstKeyEmplGrp/CompReportPrior990RltdOrgsAmt", 
"/Return/ReturnData/IRS990ScheduleJ/Form990ScheduleJPartIII/Explanation", 
"/Return/ReturnData/IRS990ScheduleJ/SupplementalInformationDetail/ExplanationTxt", 
"/Return/ReturnData/IRS990ScheduleJ/Form990ScheduleJPartIII/ReturnReference", 
"/Return/ReturnData/IRS990ScheduleJ/SupplementalInformationDetail/FormAndLineReferenceDesc", 
"/Return/ReturnData/IRS990ScheduleJ/Form990ScheduleJPartIII/Identifier", 
"/Return/ReturnData/IRS990ScheduleJ/SupplementalInformationDetail/IdentifierTxt"
), variable_name_new = c("SJ_01_PC_CLUB_FEES", "SJ_01_PC_CLUB_FEES", 
"SJ_01_PC_CLUB_FEES", "SJ_01_PC_COMPANION_TRAVEL", "SJ_01_PC_COMPANION_TRAVEL", 
"SJ_01_PC_COMPANION_TRAVEL", "SJ_01_PC_DISCRETIONARY_ACCOUNT", 
"SJ_01_PC_DISCRETIONARY_ACCOUNT", "SJ_01_PC_DISCRETIONARY_ACCOUNT", 
"SJ_01_PC_FIRST_CLASS_TRAVEL", "SJ_01_PC_FIRST_CLASS_TRAVEL", 
"SJ_01_PC_FIRST_CLASS_TRAVEL", "SJ_01_PC_HOME_OFFICE_SUBSIDY", 
"SJ_01_PC_HOME_OFFICE_SUBSIDY", "SJ_01_PC_HOME_OFFICE_SUBSIDY", 
"SJ_01_PC_HOUSING_ALLOWANCE", "SJ_01_PC_HOUSING_ALLOWANCE", "SJ_01_PC_HOUSING_ALLOWANCE", 
"SJ_01_PC_INDEMNIFICATION", "SJ_01_PC_INDEMNIFICATION", "SJ_01_PC_INDEMNIFICATION", 
"SJ_01_PC_PERSONAL_SERVICES", "SJ_01_PC_PERSONAL_SERVICES", "SJ_01_PC_PERSONAL_SERVICES", 
"SJ_01_PC_WRITTEN_POLICY", "SJ_01_PC_WRITTEN_POLICY", "SJ_01_PC_WRITTEN_POLICY", 
"SJ_01_PC_SUBSTANTIATION_REQUIRED", "SJ_01_PC_SUBSTANTIATION_REQUIRED", 
"SJ_01_PC_SUBSTANTIATION_REQUIRED", "SJ_01_PC_BOARD_APPROVAL", 
"SJ_01_PC_BOARD_APPROVAL", "SJ_01_PC_BOARD_APPROVAL", "SJ_01_PC_COMPENSATION_COMMITTEE", 
"SJ_01_PC_COMPENSATION_COMMITTEE", "SJ_01_PC_COMPENSATION_COMMITTEE", 
"SJ_01_PC_COMPENSATION_SURVEY", "SJ_01_PC_COMPENSATION_SURVEY", 
"SJ_01_PC_COMPENSATION_SURVEY", "SJ_01_PC_CONSULTANT", "SJ_01_PC_CONSULTANT", 
"SJ_01_PC_CONSULTANT", "SJ_01_PC_CONTRACT", "SJ_01_PC_CONTRACT", 
"SJ_01_PC_CONTRACT", "SJ_01_PC_OTHER_ORGS_990", "SJ_01_PC_OTHER_ORGS_990", 
"SJ_01_PC_OTHER_ORGS_990", "SJ_01_PC_SEVERANCE", "SJ_01_PC_SEVERANCE", 
"SJ_01_PC_SEVERANCE", "SJ_01_PC_SUPPLEMENTAL_RETIREMENT", "SJ_01_PC_SUPPLEMENTAL_RETIREMENT", 
"SJ_01_PC_SUPPLEMENTAL_RETIREMENT", "SJ_01_PC_EQUITY_BASED_COMP", 
"SJ_01_PC_EQUITY_BASED_COMP", "SJ_01_PC_EQUITY_BASED_COMP", "SJ_01_PC_CONTINGENT_REV_OWN", 
"SJ_01_PC_CONTINGENT_REV_OWN", "SJ_01_PC_CONTINGENT_REV_OWN", 
"SJ_01_PC_CONTINGENT_REV_RELATED", "SJ_01_PC_CONTINGENT_REV_RELATED", 
"SJ_01_PC_CONTINGENT_REV_RELATED", "SJ_01_PC_CONTINGENT_NET_OWN", 
"SJ_01_PC_CONTINGENT_NET_OWN", "SJ_01_PC_CONTINGENT_NET_OWN", 
"SJ_01_PC_CONTINGENT_NET_RELATED", "SJ_01_PC_CONTINGENT_NET_RELATED", 
"SJ_01_PC_CONTINGENT_NET_RELATED", "SJ_01_PC_NON_FIXED_PAYMENTS", 
"SJ_01_PC_NON_FIXED_PAYMENTS", "SJ_01_PC_NON_FIXED_PAYMENTS", 
"SJ_01_PC_CONTRACT_EXCEPTION", "SJ_01_PC_CONTRACT_EXCEPTION", 
"SJ_01_PC_CONTRACT_EXCEPTION", "SJ_01_PC_REBUTTABLE_PRESUMPTION", 
"SJ_01_PC_REBUTTABLE_PRESUMPTION", "SJ_02_PC_DIRTRSTKEY_NAME", 
"SJ_02_PC_DIRTRSTKEY_NAME", "SJ_02_PC_DIRTRSTKEY_BIZ_NAME_L1", 
"SJ_02_PC_DIRTRSTKEY_BIZ_NAME_L1", "SJ_02_PC_DIRTRSTKEY_BIZ_NAME_L1", 
"SJ_02_PC_DIRTRSTKEY_BIZ_NAME_L2", "SJ_02_PC_DIRTRSTKEY_BIZ_NAME_L2", 
"SJ_02_PC_DIRTRSTKEY_BIZ_NAME_L2", "SJ_02_PC_DIRTRSTKEY_TITLE", 
"SJ_02_PC_DIRTRSTKEY_TITLE", "SJ_02_PC_COMP_BASE", "SJ_02_PC_COMP_BASE", 
"SJ_02_PC_COMP_BASE_RELATED", "SJ_02_PC_COMP_BASE_RELATED", "SJ_02_PC_COMP_BONUS", 
"SJ_02_PC_COMP_BONUS", "SJ_02_PC_COMP_BONUS_RELATED", "SJ_02_PC_COMP_BONUS_RELATED", 
"SJ_02_PC_COMP_OTHER", "SJ_02_PC_COMP_OTHER", "SJ_02_PC_COMP_OTHER_RELATED", 
"SJ_02_PC_COMP_OTHER_RELATED", "SJ_02_PC_COMP_DEFERRED", "SJ_02_PC_COMP_DEFERRED", 
"SJ_02_PC_COMP_DEFERRED_RELATED", "SJ_02_PC_COMP_DEFERRED_RELATED", 
"SJ_02_PC_COMP_BENF", "SJ_02_PC_COMP_BENF", "SJ_02_PC_COMP_BENF_RELATED", 
"SJ_02_PC_COMP_BENF_RELATED", "SJ_02_PC_COMP_TOTAL", "SJ_02_PC_COMP_TOTAL", 
"SJ_02_PC_COMP_TOTAL_RELATED", "SJ_02_PC_COMP_TOTAL_RELATED", 
"SJ_02_PC_COMP_DEF_PRIOR", "SJ_02_PC_COMP_DEF_PRIOR", "SJ_02_PC_COMP_DEF_PRIOR_RELATED", 
"SJ_02_PC_COMP_DEF_PRIOR_RELATED", "SJ_03_PC_EXPLANATION_TEXT", 
"SJ_03_PC_EXPLANATION_TEXT", "SJ_03_PC_FORM_AND_LINE_REFERENCE", 
"SJ_03_PC_FORM_AND_LINE_REFERENCE", "SJ_03_PC_IDENTIFIER", "SJ_03_PC_IDENTIFIER"
), X = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
24L, 24L, 31L, 31L, 31L, 31L, 31L, 31L, 25L, NA, NA, NA, NA, 
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA), 
    description = c("Club dues or fees", "Club dues or fees", 
    "Club dues or fees", "Travel for companions", "Travel for companions", 
    "Travel for companions", "Discretionary spending account", 
    "Discretionary spending account", "Discretionary spending account", 
    "First class or charter travel", "First class or charter travel", 
    "First class or charter travel", "Payments for use of residence", 
    "Payments for use of residence", "Payments for use of residence", 
    "Housing allowance or residence", "Housing allowance or residence", 
    "Housing allowance or residence", "Idemnification and gross-up payments", 
    "Idemnification and gross-up payments", "Idemnification and gross-up payments", 
    "Personal services", "Personal services", "Personal services", 
    "Written policy reference T and E expenses?", "Written policy reference T and E expenses?", 
    "Written policy reference T and E expenses?", "Substantiation required?", 
    "Substantiation required?", "Substantiation required?", "Board or committee approval", 
    "Board or committee approval", "Board or committee approval", 
    "Compensation committee", "Compensation committee", "Compensation committee", 
    "Compensation survey", "Compensation survey", "Compensation survey", 
    "Independent consultant", "Independent consultant", "Independent consultant", 
    "Written employment contract", "Written employment contract", 
    "Written employment contract", "Form 990 of other organizations", 
    "Form 990 of other organizations", "Form 990 of other organizations", 
    "Severance payment?", "Severance payment?", "Severance payment?", 
    "Supplemental nonqualified retirement plan?", "Supplemental nonqualified retirement plan?", 
    "Supplemental nonqualified retirement plan?", "Equity based compensation arrangement?", 
    "Equity based compensation arrangement?", "Equity based compensation arrangement?", 
    "Compensation based on revenue of filing org?", "Compensation based on revenue of filing org?", 
    "Compensation based on revenue of filing org?", "Compensation based on revenue of related orgs?", 
    "Compensation based on revenue of related orgs?", "Compensation based on revenue of related orgs?", 
    "Compensation based on net earnings of filing org?", "Compensation based on net earnings of filing org?", 
    "Compensation based on net earnings of filing org?", "Compensation based on net earnings of related orgs?", 
    "Compensation based on net earnings of related orgs?", "Compensation based on net earnings of related orgs?", 
    "Any non-fixed payments?", "Any non-fixed payments?", "Any non-fixed payments?", 
    "Initial contract exception?", "Initial contract exception?", 
    "Initial contract exception?", "Rebuttable presumption procedure?", 
    "Rebuttable presumption procedure?", "Name of officer - person", 
    "Name of officer - person", "Name Business - BusinessNameLine1", 
    "Business Name - BusinessNameLine1", "Business Name - BusinessNameLine1Txt", 
    "Name Business - BusinessNameLine2", "Business Name - BusinessNameLine2", 
    "Business Name - BusinessNameLine2Txt", "Form990 Schedule JPart II - Title", 
    "Title of Officer", "Base compensation ($) from filing organization", 
    "Base compensation ($) from filing organization", "Compensation based on related organizations?", 
    "Compensation based on related organizations?", "Bonus and incentive compensation ($) from filing organization", 
    "Bonus and incentive compensation ($) from filing organization", 
    "Bonus and incentive compensation ($) from related organizations", 
    "Bonus and incentive compensation ($) from related organizations", 
    "Other compensation ($) from filing organization", "Other compensation ($) from filing organization", 
    "Other compensation ($) from realted organizations", "Other compensation ($) from related organizations", 
    "Deferred compensation ($) from filing organization", "Deferred compensation ($) from filing organization", 
    "Deferred compensation ($) from related organizations", "Deferred compensation ($) from related organizations", 
    "Nontaxable benefits ($) from filing organization", "Nontaxable benefits ($) from filing organization", 
    "Nontaxable benefits ($) from related organizations", "Nontaxable benefits ($) from related organizations", 
    "Total of (B)(i) - (D); from filing org", "Total of (B)(i) - (D); from filing org", 
    "Total of (B)(i) - (D); from related orgs", "Total of (B)(i) - (D); from related orgs", 
    "Comp reported prior 990 - from filing org", "Comp reported prior 990 - from filing org", 
    "Comp reported prior 990 - from related orgs", "Comp reported prior 990 - from related orgs", 
    "Form990 Schedule JPart III - Explanation", "Form; Part and line number reference explanation", 
    "Return reference", "Form; Part and line number reference", 
    "Form990 Schedule JPart III - Identifier", "Supplemental Information Detail - Identifier"
    ), location_code_xsd = c("[ClubDuesOrFees] Part I Line 1a", 
    "[ClubDuesOrFeesInd] Part I Line 1a", "", "[TravelForCompanions] Part I Line 1a", 
    "[TravelForCompanionsInd] Part I Line 1a", "", "[DiscretionarySpendingAccount] Part I Line 1a", 
    "[DiscretionarySpendingAcctInd] Part I Line 1a", "", "[FirstClassOrCharterTravel] Part I Line 1a", 
    "[FirstClassOrCharterTravelInd] Part I Line 1a", "", "[PaymentsForUseOfResidence] Part I Line 1a", 
    "[PaymentsForUseOfResidenceInd] Part I Line 1a", "", "[HousingAllowanceOrResidence] Part I Line 1a", 
    "[HousingAllowanceOrResidenceInd] Part I Line 1a", "", "[IdemnificationGrossUpPayments] Part I Line 1a", 
    "[IdemnificationGrossUpPmtsInd] Part I Line 1a", "", "[PersonalServices] Part I Line 1a", 
    "[PersonalServicesInd] Part I Line 1a", "", "[WrittenPolicyRefTAndEExpnssInd] Part I Line 1b", 
    "[WrittenPolicyReTAndEExpenses] Part I Line 1b", "", "[SubstantiationRequired] Part I Line 2", 
    "[SubstantiationRequiredInd] Part I Line 2", "", "[BoardOrCommitteeApproval] Part I Line 3", 
    "[BoardOrCommitteeApprovalInd] Part I Line 3", "", "[CompensationCommittee] Part I Line 3", 
    "[CompensationCommitteeInd] Part I Line 3", "", "[CompensationSurvey] Part I Line 3", 
    "[CompensationSurveyInd] Part I Line 3", "", "[IndependentConsultant] Part I Line 3", 
    "[IndependentConsultantInd] Part I Line 3", "", "[WrittenEmploymentContract] Part I Line 3", 
    "[WrittenEmploymentContractInd] Part I Line 3", "", "[Form990OfOtherOrganizations] Part I Line 3", 
    "[Form990OfOtherOrganizationsInd] Part I Line 3", "", "[SeverancePayment] Part I Line 4a", 
    "[SeverancePaymentInd] Part I Line 4a", "", "[SupplementalNonqualRetirePlan] Part I Line 4b", 
    "[SupplementalNonqualRtrPlanInd] Part I Line 4b", "", "[EquityBasedCompArrangement] Part I Line 4c", 
    "[EquityBasedCompArrngmInd] Part I Line 4c", "", "[CompBasedOnRevenueOfFilingOrg] Part I Line 5a", 
    "[CompBasedOnRevenueOfFlngOrgInd] Part I Line 5a", "", "[CompBasedOnRevenueRelatedOrgs] Part I Line 5b", 
    "[CompBsdOnRevRelatedOrgsInd] Part I Line 5b", "", "[CompBasedNetEarningsFilingOrg] Part I Line 6a", 
    "[CompBsdNetEarnsFlngOrgInd] Part I Line 6a", "", "[CompBasedNetEarningsRelateOrgs] Part I Line 6b", 
    "[CompBsdNetEarnsRltdOrgsInd] Part I Line 6b", "", "[AnyNonFixedPayments] Part I Line 7", 
    "[AnyNonFixedPaymentsInd] Part I Line 7", "", "[InitialContractException] Part I Line 8", 
    "[InitialContractExceptionInd] Part I Line 8", "", "[RebuttablePresumptionProcedure] Part I Line 9", 
    "[RebuttablePresumptionProcInd] Part I Line 9", "[NamePerson] Part II Column (A)", 
    "[PersonNm] Part II Column (A)", "[NameBusiness] Part II Column (A)", 
    "[BusinessName] Part II Column (A)", "[BusinessName] Part II Column (A)", 
    "[NameBusiness] Part II Column (A)", "[BusinessName] Part II Column (A)", 
    "[BusinessName] Part II Column (A)", "[Title] Part II Column (A)", 
    "[TitleTxt] Part II Column (A)", "[BaseCompensationFilingOrg] Part II Column (B)(i)", 
    "[BaseCompensationFilingOrgAmt] Part II Column (B)(i)", "[CompBasedOnRelatedOrgs] Part II Column (B)(i)", 
    "[CompensationBasedOnRltdOrgsAmt] Part II Column (B)(i)", 
    "[BonusFilingOrg] Part II Column (B)(ii)", "[BonusFilingOrganizationAmount] Part II Column (B)(ii)", 
    "[BonusRelatedOrgs] Part II Column (B)(ii)", "[BonusRelatedOrganizationsAmt] Part II Column (B)(ii)", 
    "[OtherCompensationFilingOrg] Part II Column (B)(iii)", "[OtherCompensationFilingOrgAmt] Part II Column (B)(iii)", 
    "[OtherCompensationRelatedOrgs] Part II Column (B)(iii)", 
    "[OtherCompensationRltdOrgsAmt] Part II Column (B)(iii)", 
    "[DeferredCompFilingOrg] Part II Column (C)", "[DeferredCompensationFlngOrgAmt] Part II Column (C)", 
    "[DeferredCompRelatedOrgs] Part II Column (C)", "[DeferredCompRltdOrgsAmt] Part II Column (C)", 
    "[NontaxableBenefitsFilingOrg] Part II Column (D)", "[NontaxableBenefitsFilingOrgAmt] Part II Column (D)", 
    "[NontaxableBenefitsRelatedOrgs] Part II Column (D)", "[NontaxableBenefitsRltdOrgsAmt] Part II Column (D)", 
    "[TotalCompensationFilingOrg] Part II Column (E)", "[TotalCompensationFilingOrgAmt] Part II Column (E)", 
    "[TotalCompensationRelatedOrgs] Part II Column (E)", "[TotalCompensationRltdOrgsAmt] Part II Column (E)", 
    "[CompReportPrior990FilingOrg] Part II Column (F)", "[CompReportPrior990FilingOrgAmt] Part II Column (F)", 
    "[CompReportPrior990RelatedOrgs] Part II Column (F)", "[CompReportPrior990RltdOrgsAmt] Part II Column (F)", 
    "[Explanation] Part III", "[ExplanationTxt] Part III", "[ReturnReference] Part III", 
    "[FormAndLineReferenceDesc] Part III", "[Identifier] Part III", 
    ""), location_code = c("SCHED-J-PART-01-LINE-1a", "SCHED-J-PART-01-LINE-1a", 
    "SCHED-J-PART-01-LINE-1a", "SCHED-J-PART-01-LINE-1a", "SCHED-J-PART-01-LINE-1a", 
    "SCHED-J-PART-01-LINE-1a", "SCHED-J-PART-01-LINE-1a", "SCHED-J-PART-01-LINE-1a", 
    "SCHED-J-PART-01-LINE-1a", "SCHED-J-PART-01-LINE-1a", "SCHED-J-PART-01-LINE-1a", 
    "SCHED-J-PART-01-LINE-1a", "SCHED-J-PART-01-LINE-1a", "SCHED-J-PART-01-LINE-1a", 
    "SCHED-J-PART-01-LINE-1a", "SCHED-J-PART-01-LINE-1a", "SCHED-J-PART-01-LINE-1a", 
    "SCHED-J-PART-01-LINE-1a", "SCHED-J-PART-01-LINE-1a", "SCHED-J-PART-01-LINE-1a", 
    "SCHED-J-PART-01-LINE-1a", "SCHED-J-PART-01-LINE-1a", "SCHED-J-PART-01-LINE-1a", 
    "SCHED-J-PART-01-LINE-1a", "SCHED-J-PART-01-LINE-1b", "SCHED-J-PART-01-LINE-1b", 
    "SCHED-J-PART-01-LINE-1b", "SCHED-J-PART-01-LINE-2", "SCHED-J-PART-01-LINE-2", 
    "SCHED-J-PART-01-LINE-2", "SCHED-J-PART-01-LINE-3", "SCHED-J-PART-01-LINE-3", 
    "SCHED-J-PART-01-LINE-3", "SCHED-J-PART-01-LINE-3", "SCHED-J-PART-01-LINE-3", 
    "SCHED-J-PART-01-LINE-3", "SCHED-J-PART-01-LINE-3", "SCHED-J-PART-01-LINE-3", 
    "SCHED-J-PART-01-LINE-3", "SCHED-J-PART-01-LINE-3", "SCHED-J-PART-01-LINE-3", 
    "SCHED-J-PART-01-LINE-3", "SCHED-J-PART-01-LINE-3", "SCHED-J-PART-01-LINE-3", 
    "SCHED-J-PART-01-LINE-3", "SCHED-J-PART-01-LINE-3", "SCHED-J-PART-01-LINE-3", 
    "SCHED-J-PART-01-LINE-3", "SCHED-J-PART-01-LINE-4a", "SCHED-J-PART-01-LINE-4a", 
    "SCHED-J-PART-01-LINE-4a", "SCHED-J-PART-01-LINE-4b", "SCHED-J-PART-01-LINE-4b", 
    "SCHED-J-PART-01-LINE-4b", "SCHED-J-PART-01-LINE-4c", "SCHED-J-PART-01-LINE-4c", 
    "SCHED-J-PART-01-LINE-4c", "SCHED-J-PART-01-LINE-5a", "SCHED-J-PART-01-LINE-5a", 
    "SCHED-J-PART-01-LINE-5a", "SCHED-J-PART-01-LINE-5b", "SCHED-J-PART-01-LINE-5b", 
    "SCHED-J-PART-01-LINE-5b", "SCHED-J-PART-01-LINE-6a", "SCHED-J-PART-01-LINE-6a", 
    "SCHED-J-PART-01-LINE-6a", "SCHED-J-PART-01-LINE-6b", "SCHED-J-PART-01-LINE-6b", 
    "SCHED-J-PART-01-LINE-6b", "SCHED-J-PART-01-LINE-7", "SCHED-J-PART-01-LINE-7", 
    "SCHED-J-PART-01-LINE-7", "SCHED-J-PART-01-LINE-8", "SCHED-J-PART-01-LINE-8", 
    "SCHED-J-PART-01-LINE-8", "SCHED-J-PART-01-LINE-9", "SCHED-J-PART-01-LINE-9", 
    "SCHED-J-PART-02-COL-A-LINE-(i)", "SCHED-J-PART-02-COL-A-LINE-(i)", 
    "SCHED-J-PART-02-COL-A-LINE-(i)", "SCHED-J-PART-02-COL-A-LINE-(i)", 
    "SCHED-J-PART-02-COL-A-LINE-(i)", "SCHED-J-PART-02-COL-A-LINE-(ii)", 
    "SCHED-J-PART-02-COL-A-LINE-(ii)", "SCHED-J-PART-02-COL-A-LINE-(ii)", 
    "SCHED-J-PART-02-COL-A-LINE-(ii)", "SCHED-J-PART-02-COL-A-LINE-(ii)", 
    "SCHED-J-PART-02-COL-B(i)-LINE-(i)", "SCHED-J-PART-02-COL-B(i)-LINE-(i)", 
    "SCHED-J-PART-02-COL-B(i)-LINE-(ii)", "SCHED-J-PART-02-COL-B(i)-LINE-(ii)", 
    "SCHED-J-PART-02-COL-B(ii)-LINE-(i)", "SCHED-J-PART-02-COL-B(ii)-LINE-(i)", 
    "SCHED-J-PART-02-COL-B(ii)-LINE-(ii)", "SCHED-J-PART-02-COL-B(ii)-LINE-(ii)", 
    "SCHED-J-PART-02-COL-B(iii)-LINE-(i)", "SCHED-J-PART-02-COL-B(iii)-LINE-(i)", 
    "SCHED-J-PART-02-COL-B(iii)-LINE-(ii)", "SCHED-J-PART-02-COL-B(iii)-LINE-(ii)", 
    "SCHED-J-PART-02-COL-C-LINE-(i)", "SCHED-J-PART-02-COL-C-LINE-(i)", 
    "SCHED-J-PART-02-COL-C-LINE-(ii)", "SCHED-J-PART-02-COL-C-LINE-(ii)", 
    "SCHED-J-PART-02-COL-D-LINE-(i)", "SCHED-J-PART-02-COL-D-LINE-(i)", 
    "SCHED-J-PART-02-COL-D-LINE-(ii)", "SCHED-J-PART-02-COL-D-LINE-(ii)", 
    "SCHED-J-PART-02-COL-E-LINE-(i)", "SCHED-J-PART-02-COL-E-LINE-(i)", 
    "SCHED-J-PART-02-COL-E-LINE-(ii)", "SCHED-J-PART-02-COL-E-LINE-(ii)", 
    "SCHED-J-PART-02-COL-F-LINE-(i)", "SCHED-J-PART-02-COL-F-LINE-(i)", 
    "SCHED-J-PART-02-COL-F-LINE-(ii)", "SCHED-J-PART-02-COL-F-LINE-(ii)", 
    "SCHED-J-PART-03", "SCHED-J-PART-03", "SCHED-J-PART-03", 
    "SCHED-J-PART-03", "SCHED-J-PART-03", "SCHED-J-PART-03"), 
    form = c("SCHED-J", "SCHED-J", "SCHED-J", "SCHED-J", "SCHED-J", 
    "SCHED-J", "SCHED-J", "SCHED-J", "SCHED-J", "SCHED-J", "SCHED-J", 
    "SCHED-J", "SCHED-J", "SCHED-J", "SCHED-J", "SCHED-J", "SCHED-J", 
    "SCHED-J", "SCHED-J", "SCHED-J", "SCHED-J", "SCHED-J", "SCHED-J", 
    "SCHED-J", "SCHED-J", "SCHED-J", "SCHED-J", "SCHED-J", "SCHED-J", 
    "SCHED-J", "SCHED-J", "SCHED-J", "SCHED-J", "SCHED-J", "SCHED-J", 
    "SCHED-J", "SCHED-J", "SCHED-J", "SCHED-J", "SCHED-J", "SCHED-J", 
    "SCHED-J", "SCHED-J", "SCHED-J", "SCHED-J", "SCHED-J", "SCHED-J", 
    "SCHED-J", "SCHED-J", "SCHED-J", "SCHED-J", "SCHED-J", "SCHED-J", 
    "SCHED-J", "SCHED-J", "SCHED-J", "SCHED-J", "SCHED-J", "SCHED-J", 
    "SCHED-J", "SCHED-J", "SCHED-J", "SCHED-J", "SCHED-J", "SCHED-J", 
    "SCHED-J", "SCHED-J", "SCHED-J", "SCHED-J", "SCHED-J", "SCHED-J", 
    "SCHED-J", "SCHED-J", "SCHED-J", "SCHED-J", "SCHED-J", "SCHED-J", 
    "SCHED-J", "SCHED-J", "SCHED-J", "SCHED-J", "SCHED-J", "SCHED-J", 
    "SCHED-J", "SCHED-J", "SCHED-J", "SCHED-J", "SCHED-J", "SCHED-J", 
    "SCHED-J", "SCHED-J", "SCHED-J", "SCHED-J", "SCHED-J", "SCHED-J", 
    "SCHED-J", "SCHED-J", "SCHED-J", "SCHED-J", "SCHED-J", "SCHED-J", 
    "SCHED-J", "SCHED-J", "SCHED-J", "SCHED-J", "SCHED-J", "SCHED-J", 
    "SCHED-J", "SCHED-J", "SCHED-J", "SCHED-J", "SCHED-J", "SCHED-J", 
    "SCHED-J", "SCHED-J", "SCHED-J", "SCHED-J", "SCHED-J", "SCHED-J", 
    "SCHED-J", "SCHED-J"), part = c("PART-01", "PART-01", "PART-01", 
    "PART-01", "PART-01", "PART-01", "PART-01", "PART-01", "PART-01", 
    "PART-01", "PART-01", "PART-01", "PART-01", "PART-01", "PART-01", 
    "PART-01", "PART-01", "PART-01", "PART-01", "PART-01", "PART-01", 
    "PART-01", "PART-01", "PART-01", "PART-01", "PART-01", "PART-01", 
    "PART-01", "PART-01", "PART-01", "PART-01", "PART-01", "PART-01", 
    "PART-01", "PART-01", "PART-01", "PART-01", "PART-01", "PART-01", 
    "PART-01", "PART-01", "PART-01", "PART-01", "PART-01", "PART-01", 
    "PART-01", "PART-01", "PART-01", "PART-01", "PART-01", "PART-01", 
    "PART-01", "PART-01", "PART-01", "PART-01", "PART-01", "PART-01", 
    "PART-01", "PART-01", "PART-01", "PART-01", "PART-01", "PART-01", 
    "PART-01", "PART-01", "PART-01", "PART-01", "PART-01", "PART-01", 
    "PART-01", "PART-01", "PART-01", "PART-01", "PART-01", "PART-01", 
    "PART-01", "PART-01", "PART-02", "PART-02", "PART-02", "PART-02", 
    "PART-02", "PART-02", "PART-02", "PART-02", "PART-02", "PART-02", 
    "PART-02", "PART-02", "PART-02", "PART-02", "PART-02", "PART-02", 
    "PART-02", "PART-02", "PART-02", "PART-02", "PART-02", "PART-02", 
    "PART-02", "PART-02", "PART-02", "PART-02", "PART-02", "PART-02", 
    "PART-02", "PART-02", "PART-02", "PART-02", "PART-02", "PART-02", 
    "PART-02", "PART-02", "PART-02", "PART-02", "PART-03", "PART-03", 
    "PART-03", "PART-03", "PART-03", "PART-03"), line_number = c("Line 1a", 
    "Line 1a", "Line 1a", "Line 1a", "Line 1a", "Line 1a", "Line 1a", 
    "Line 1a", "Line 1a", "Line 1a", "Line 1a", "Line 1a", "Line 1a", 
    "Line 1a", "Line 1a", "Line 1a", "Line 1a", "Line 1a", "Line 1a", 
    "Line 1a", "Line 1a", "Line 1a", "Line 1a", "Line 1a", "Line 1b", 
    "Line 1b", "Line 1b", "Line 2", "Line 2", "Line 2", "Line 3", 
    "Line 3", "Line 3", "Line 3", "Line 3", "Line 3", "Line 3", 
    "Line 3", "Line 3", "Line 3", "Line 3", "Line 3", "Line 3", 
    "Line 3", "Line 3", "Line 3", "Line 3", "Line 3", "Line 4a", 
    "Line 4a", "Line 4a", "Line 4b", "Line 4b", "Line 4b", "Line 4c", 
    "Line 4c", "Line 4c", "Line 5a", "Line 5a", "Line 5a", "Line 5b", 
    "Line 5b", "Line 5b", "Line 6a", "Line 6a", "Line 6a", "Line 6b", 
    "Line 6b", "Line 6b", "Line 7", "Line 7", "Line 7", "Line 8", 
    "Line 8", "Line 8", "Line 9", "Line 9", "Column (A) Line (i)", 
    "Column (A) Line (i)", "Column (A) Line (i)", "Column (A) Line (i)", 
    "Column (A) Line (i)", "Column (A) Line (i)", "Column (A Line )(i)", 
    "Column (A) Line (i)", "Column (A) Line (ii)", "Column (A) Line (ii)", 
    "Column (B)(i) Line (i)", "Column (B)(i) Line (i)", "Column (B)(i) Line (ii)", 
    "Column (B)(i) Line (ii)", "Column (B)(ii) Line (i)", "Column (B)(ii) Line (i)", 
    "Column (B)(ii) Line (ii)", "Column (B)(ii) Line (ii)", "Column (B)(iii) Line (i)", 
    "Column (B)(iii) Line (i)", "Column (B)(iii) Line (ii)", 
    "Column (B)(iii) Line (ii)", "Column (C) Line (i)", "Column (C) Line (i)", 
    "Column (C) Line (ii)", "Column (C) Line (ii)", "Column (D) Line (i)", 
    "Column (D) Line (i)", "Column (D) Line (ii)", "Column (D) Line (ii)", 
    "Column (E) Line (i)", "Column (E) Line (i)", "Column (E) Line (ii)", 
    "Column (E) Line (ii)", "Column (F) Line (i)", "Column (F) Line (i)", 
    "Column (F) Line (ii)", "Column (F) Line (ii)", "Part 03", 
    "Part 03", "Part 03", "Part 03", "Part 03", "Part 03"), scope = c("PC", 
    "PC", "PC", "PC", "PC", "PC", "PC", "PC", "PC", "PC", "PC", 
    "PC", "PC", "PC", "PC", "PC", "PC", "PC", "PC", "PC", "PC", 
    "PC", "PC", "PC", "PC", "PC", "PC", "PC", "PC", "PC", "PC", 
    "PC", "PC", "PC", "PC", "PC", "PC", "PC", "PC", "PC", "PC", 
    "PC", "PC", "PC", "PC", "PC", "PC", "PC", "PC", "PC", "PC", 
    "PC", "PC", "PC", "PC", "PC", "PC", "PC", "PC", "PC", "PC", 
    "PC", "PC", "PC", "PC", "PC", "PC", "PC", "PC", "PC", "PC", 
    "PC", "PC", "PC", "PC", "PC", "PC", "PC", "PC", "PC", "PC", 
    "PC", "PC", "PC", "PC", "PC", "PC", "PC", "PC", "PC", "PC", 
    "PC", "PC", "PC", "PC", "PC", "PC", "PC", "PC", "PC", "PC", 
    "PC", "PC", "PC", "PC", "PC", "PC", "PC", "PC", "PC", "PC", 
    "PC", "PC", "PC", "PC", "PC", "PC", "PC", "PC", "PC", "PC"
    ), data_type_xsd = c("CheckboxType", "CheckboxType", "", 
    "CheckboxType", "CheckboxType", "", "CheckboxType", "CheckboxType", 
    "", "CheckboxType", "CheckboxType", "", "CheckboxType", "CheckboxType", 
    "", "CheckboxType", "CheckboxType", "", "CheckboxType", "CheckboxType", 
    "", "CheckboxType", "CheckboxType", "", "BooleanType", "BooleanType", 
    "", "BooleanType", "BooleanType", "", "CheckboxType", "CheckboxType", 
    "", "CheckboxType", "CheckboxType", "", "CheckboxType", "CheckboxType", 
    "", "CheckboxType", "CheckboxType", "", "CheckboxType", "CheckboxType", 
    "", "CheckboxType", "CheckboxType", "", "BooleanType", "BooleanType", 
    "", "BooleanType", "BooleanType", "", "BooleanType", "BooleanType", 
    "", "BooleanType", "BooleanType", "", "BooleanType", "BooleanType", 
    "", "BooleanType", "BooleanType", "", "BooleanType", "BooleanType", 
    "", "BooleanType", "BooleanType", "", "BooleanType", "BooleanType", 
    "", "BooleanType", "BooleanType", "PersonNameType", "PersonNameType", 
    "BusinessNameLine1Type", "BusinessNameLine1Type", "BusinessNameLine1Type", 
    "BusinessNameLine2Type", "BusinessNameLine2Type", "BusinessNameLine2Type", 
    "", "LineExplanationType", "USAmountType", "USAmountType", 
    "USAmountType", "USAmountType", "USAmountType", "USAmountType", 
    "USAmountType", "USAmountType", "USAmountType", "USAmountType", 
    "USAmountType", "USAmountType", "USAmountType", "USAmountType", 
    "USAmountType", "USAmountType", "USAmountType", "USAmountType", 
    "USAmountType", "USAmountType", "USAmountType", "USAmountType", 
    "USAmountType", "USAmountType", "USAmountType", "USAmountType", 
    "USAmountType", "USAmountType", "ExplanationType", "ExplanationType", 
    "LineExplanationType", "LineExplanationType", "LineExplanationType", 
    ""), data_type_simple = c("text", "text", "text", "text", 
    "text", "text", "text", "text", "text", "text", "text", "text", 
    "text", "text", "text", "text", "text", "text", "text", "text", 
    "text", "text", "text", "text", "text", "text", "text", "text", 
    "text", "text", "text", "text", "text", "text", "text", "text", 
    "text", "text", "text", "text", "text", "text", "text", "text", 
    "text", "text", "text", "text", "text", "text", "text", "text", 
    "text", "text", "text", "text", "text", "text", "text", "text", 
    "text", "text", "text", "text", "text", "text", "text", "text", 
    "text", "text", "text", "text", "text", "text", "text", "text", 
    "text", "text", "text", "text", "text", "text", "text", "text", 
    "text", "text", "text", "numeric", "numeric", "numeric", 
    "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
    "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
    "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
    "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
    "numeric", "text", "text", "text", "text", "text", "text"
    ), cardinality = c("ONE", "ONE", "ONE", "ONE", "ONE", "ONE", 
    "ONE", "ONE", "ONE", "ONE", "ONE", "ONE", "ONE", "ONE", "ONE", 
    "ONE", "ONE", "ONE", "ONE", "ONE", "ONE", "ONE", "ONE", "ONE", 
    "ONE", "ONE", "ONE", "ONE", "ONE", "ONE", "ONE", "ONE", "ONE", 
    "ONE", "ONE", "ONE", "ONE", "ONE", "ONE", "ONE", "ONE", "ONE", 
    "ONE", "ONE", "ONE", "ONE", "ONE", "ONE", "ONE", "ONE", "ONE", 
    "ONE", "ONE", "ONE", "ONE", "ONE", "ONE", "ONE", "ONE", "ONE", 
    "ONE", "ONE", "ONE", "ONE", "ONE", "ONE", "ONE", "ONE", "ONE", 
    "ONE", "ONE", "ONE", "ONE", "ONE", "ONE", "ONE", "ONE", "MANY", 
    "MANY", "MANY", "MANY", "MANY", "MANY", "MANY", "MANY", "MANY", 
    "MANY", "MANY", "MANY", "MANY", "MANY", "MANY", "MANY", "MANY", 
    "MANY", "MANY", "MANY", "MANY", "MANY", "MANY", "MANY", "MANY", 
    "MANY", "MANY", "MANY", "MANY", "MANY", "MANY", "MANY", "MANY", 
    "MANY", "MANY", "MANY", "MANY", "MANY", "ONE", "ONE", "ONE", 
    "ONE", "ONE", "ONE"), rdb_table = c("SJ-P01-T00-CHECKLIST", 
    "SJ-P01-T00-CHECKLIST", "SJ-P01-T00-CHECKLIST", "SJ-P01-T00-CHECKLIST", 
    "SJ-P01-T00-CHECKLIST", "SJ-P01-T00-CHECKLIST", "SJ-P01-T00-CHECKLIST", 
    "SJ-P01-T00-CHECKLIST", "SJ-P01-T00-CHECKLIST", "SJ-P01-T00-CHECKLIST", 
    "SJ-P01-T00-CHECKLIST", "SJ-P01-T00-CHECKLIST", "SJ-P01-T00-CHECKLIST", 
    "SJ-P01-T00-CHECKLIST", "SJ-P01-T00-CHECKLIST", "SJ-P01-T00-CHECKLIST", 
    "SJ-P01-T00-CHECKLIST", "SJ-P01-T00-CHECKLIST", "SJ-P01-T00-CHECKLIST", 
    "SJ-P01-T00-CHECKLIST", "SJ-P01-T00-CHECKLIST", "SJ-P01-T00-CHECKLIST", 
    "SJ-P01-T00-CHECKLIST", "SJ-P01-T00-CHECKLIST", "SJ-P01-T00-CHECKLIST", 
    "SJ-P01-T00-CHECKLIST", "SJ-P01-T00-CHECKLIST", "SJ-P01-T00-CHECKLIST", 
    "SJ-P01-T00-CHECKLIST", "SJ-P01-T00-CHECKLIST", "SJ-P01-T00-CHECKLIST", 
    "SJ-P01-T00-CHECKLIST", "SJ-P01-T00-CHECKLIST", "SJ-P01-T00-CHECKLIST", 
    "SJ-P01-T00-CHECKLIST", "SJ-P01-T00-CHECKLIST", "SJ-P01-T00-CHECKLIST", 
    "SJ-P01-T00-CHECKLIST", "SJ-P01-T00-CHECKLIST", "SJ-P01-T00-CHECKLIST", 
    "SJ-P01-T00-CHECKLIST", "SJ-P01-T00-CHECKLIST", "SJ-P01-T00-CHECKLIST", 
    "SJ-P01-T00-CHECKLIST", "SJ-P01-T00-CHECKLIST", "SJ-P01-T00-CHECKLIST", 
    "SJ-P01-T00-CHECKLIST", "SJ-P01-T00-CHECKLIST", "SJ-P01-T00-CHECKLIST", 
    "SJ-P01-T00-CHECKLIST", "SJ-P01-T00-CHECKLIST", "SJ-P01-T00-CHECKLIST", 
    "SJ-P01-T00-CHECKLIST", "SJ-P01-T00-CHECKLIST", "SJ-P01-T00-CHECKLIST", 
    "SJ-P01-T00-CHECKLIST", "SJ-P01-T00-CHECKLIST", "SJ-P01-T00-CHECKLIST", 
    "SJ-P01-T00-CHECKLIST", "SJ-P01-T00-CHECKLIST", "SJ-P01-T00-CHECKLIST", 
    "SJ-P01-T00-CHECKLIST", "SJ-P01-T00-CHECKLIST", "SJ-P01-T00-CHECKLIST", 
    "SJ-P01-T00-CHECKLIST", "SJ-P01-T00-CHECKLIST", "SJ-P01-T00-CHECKLIST", 
    "SJ-P01-T00-CHECKLIST", "SJ-P01-T00-CHECKLIST", "SJ-P01-T00-CHECKLIST", 
    "SJ-P01-T00-CHECKLIST", "SJ-P01-T00-CHECKLIST", "SJ-P01-T00-CHECKLIST", 
    "SJ-P01-T00-CHECKLIST", "SJ-P01-T00-CHECKLIST", "SJ-P01-T00-CHECKLIST", 
    "SJ-P01-T00-CHECKLIST", "SJ-P02-T01-COMPENSATION", "SJ-P02-T01-COMPENSATION", 
    "SJ-P02-T01-COMPENSATION", "SJ-P02-T01-COMPENSATION", "SJ-P02-T01-COMPENSATION", 
    "SJ-P02-T01-COMPENSATION", "SJ-P02-T01-COMPENSATION", "SJ-P02-T01-COMPENSATION", 
    "SJ-P02-T01-COMPENSATION", "SJ-P02-T01-COMPENSATION", "SJ-P02-T01-COMPENSATION", 
    "SJ-P02-T01-COMPENSATION", "SJ-P02-T01-COMPENSATION", "SJ-P02-T01-COMPENSATION", 
    "SJ-P02-T01-COMPENSATION", "SJ-P02-T01-COMPENSATION", "SJ-P02-T01-COMPENSATION", 
    "SJ-P02-T01-COMPENSATION", "SJ-P02-T01-COMPENSATION", "SJ-P02-T01-COMPENSATION", 
    "SJ-P02-T01-COMPENSATION", "SJ-P02-T01-COMPENSATION", "SJ-P02-T01-COMPENSATION", 
    "SJ-P02-T01-COMPENSATION", "SJ-P02-T01-COMPENSATION", "SJ-P02-T01-COMPENSATION", 
    "SJ-P02-T01-COMPENSATION", "SJ-P02-T01-COMPENSATION", "SJ-P02-T01-COMPENSATION", 
    "SJ-P02-T01-COMPENSATION", "SJ-P02-T01-COMPENSATION", "SJ-P02-T01-COMPENSATION", 
    "SJ-P02-T01-COMPENSATION", "SJ-P02-T01-COMPENSATION", "SJ-P02-T01-COMPENSATION", 
    "SJ-P02-T01-COMPENSATION", "SJ-P02-T01-COMPENSATION", "SJ-P02-T01-COMPENSATION", 
    "SJ-P03-T02-EXPLANATION-TEXT", "SJ-P03-T02-EXPLANATION-TEXT", 
    "SJ-P03-T02-EXPLANATION-TEXT", "SJ-P03-T02-EXPLANATION-TEXT", 
    "SJ-P03-T02-EXPLANATION-TEXT", "SJ-P03-T02-EXPLANATION-TEXT"
    ), production_rule = c("", "", "", "", "", "", "", "", "", 
    "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", 
    "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", 
    "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", 
    "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", 
    "", "", "", "", "", "", "", "", "SJ_02_PC_NAME_OFF_TRST_KEYEMP, SJ_02_PC_NAME_OFF_TRST_KEYEMP_L1, and SJ_02_PC_NAME_OFF_TRST_KEYEMP_L2 are the same variables", 
    "SJ_02_PC_NAME_OFF_TRST_KEYEMP, SJ_02_PC_NAME_OFF_TRST_KEYEMP_L1, and SJ_02_PC_NAME_OFF_TRST_KEYEMP_L2 are the same variables", 
    "SJ_02_PC_NAME_OFF_TRST_KEYEMP, SJ_02_PC_NAME_OFF_TRST_KEYEMP_L1, and SJ_02_PC_NAME_OFF_TRST_KEYEMP_L2 are the same variables", 
    "SJ_02_PC_NAME_OFF_TRST_KEYEMP, SJ_02_PC_NAME_OFF_TRST_KEYEMP_L1, and SJ_02_PC_NAME_OFF_TRST_KEYEMP_L2 are the same variables", 
    "SJ_02_PC_NAME_OFF_TRST_KEYEMP, SJ_02_PC_NAME_OFF_TRST_KEYEMP_L1, and SJ_02_PC_NAME_OFF_TRST_KEYEMP_L2 are the same variables", 
    "SJ_02_PC_NAME_OFF_TRST_KEYEMP, SJ_02_PC_NAME_OFF_TRST_KEYEMP_L1, and SJ_02_PC_NAME_OFF_TRST_KEYEMP_L2 are the same variables", 
    "SJ_02_PC_NAME_OFF_TRST_KEYEMP, SJ_02_PC_NAME_OFF_TRST_KEYEMP_L1, and SJ_02_PC_NAME_OFF_TRST_KEYEMP_L2 are the same variables", 
    "SJ_02_PC_NAME_OFF_TRST_KEYEMP, SJ_02_PC_NAME_OFF_TRST_KEYEMP_L1, and SJ_02_PC_NAME_OFF_TRST_KEYEMP_L2 are the same variables", 
    "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", 
    "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", 
    "", "", "", "", "", ""), validated = c("X", "X", "X", "X", 
    "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", 
    "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", 
    "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", 
    "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", 
    "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", 
    "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", 
    "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", 
    "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", 
    "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", 
    "X", "X", "X", "X", "X", "X", "X", "X", "X")), .Names = c("xpath", 
"variable_name_new", "X", "description", "location_code_xsd", 
"location_code", "form", "part", "line_number", "scope", "data_type_xsd", 
"data_type_simple", "cardinality", "rdb_table", "production_rule", 
"validated"), class = "data.frame", row.names = c(NA, -121L))





### INDEX OF 1,000 ORGS

index <-
structure(list(EIN = c("680005569", "237214512", "562651670", 
"271095844", "311745612", "311771488", "756061082", "362169181", 
"341863723", "382108168", "271619966", "952112634", "466016361", 
"521746234", "731601056", "010609738", "382352462", "570830548", 
"941111162", "951704098", "820310587", "261764960", "820373954", 
"820201238", "341623436", "452972289", "042707815", "364759072", 
"113296103", "201198064", "140569810", "161120156", "462452765", 
"042899784", "260331924", "570807801", "203844292", "540636119", 
"510157794", "382325400", "382510839", "431744159", "133398825", 
"222744650", "751653696", "330630243", "953685875", "043478758", 
"271381854", "860984718", "920094773", "541514192", "942389750", 
"510162283", "271661997", "756043078", "742959486", "742131451", 
"270633091", "464743792", "150504447", "956195769", "432017147", 
"680516055", "205071765", "432094375", "521871429", "251741343", 
"742664650", "760284248", "821038131", "470638422", "205121181", 
"010659623", "800529798", "860274412", "591172985", "550273308", 
"042911227", "420557760", "010540180", "450409209", "392064992", 
"586067236", "311073702", "311698291", "043520478", "047024796", 
"203087869", "363166891", "540838193", "621030368", "046170153", 
"224214028", "112396324", "381326857", "931213063", "930591582", 
"351362542", "251585685", "010932880", "237403820", "710709590", 
"264112380", "272101347", "640904134", "410619561", "251115026", 
"136140415", "134339476", "363257065", "752131256", "066068891", 
"370797812", "272371076", "593742789", "204662789", "580626152", 
"311158260", "311707802", "621666732", "272198317", "731462474", 
"521339835", "742676587", "366118373", "416026869", "208568180", 
"731725356", "352299211", "271091117", "344429605", "770154887", 
"141803390", "630849262", "542124192", "362212706", "382737597", 
"866006371", "050445136", "370661219", "391017036", "204920903", 
"770513867", "116038130", "223397308", "953945598", "710780981", 
"541924932", "453566338", "721004770", "132644641", "453855536", 
"480365150", "263790847", "943031441", "201967120", "540519824", 
"431796215", "043783995", "470400336", "010528831", "452737648", 
"300080369", "316050105", "941687824", "621373982", "166089072", 
"201893848", "381704855", "465416831", "841471157", "311632961", 
"431584898", "841313257", "464378376", "061661135", "150574854", 
"880293000", "112719958", "223751640", "202114904", "746063504", 
"370710280", "240737000", "752931813", "521812633", "263465689", 
"571218027", "582561433", "760310905", "810521215", "382860464", 
"640696987", "251924636", "593321080", "201993440", "200685073", 
"141498767", "264232684", "452888930", "546073925", "954774844", 
"204901024", "455611172", "274655069", "202166479", "041734980", 
"610605336", "451138073", "942518498", "954733266", "016180895", 
"311732059", "710981064", "350987917", "481003533", "272406162", 
"382992690", "621602886", "041859910", "364444200", "521665967", 
"751050316", "740898333", "453555807", "223321236", "270975374", 
"640392760", "351019658", "042138688", "752118399", "474822192", 
"161499299", "454737631", "470761300", "200776717", "341776015", 
"453513116", "205289738", "431527982", "411832349", "481248881", 
"546073930", "562169175", "460209185", "260549085", "990078306", 
"226298139", "208604076", "205908359", "742377026", "043372500", 
"134092516", "261820462", "380592500", "264431956", "237324136", 
"621498314", "260574981", "391737699", "943297850", "452037275", 
"134196230", "272320951", "561443222", "846036443", "061021061", 
"042541474", "526012327", "237351999", "043507398", "043798875", 
"510107972", "710719840", "391506910", "161272748", "752307639", 
"362002758", "204768612", "263294241", "431744938", "591390908", 
"841337868", "650109252", "060733693", "560548249", "262826610", 
"061037606", "526071802", "237418075", "270128214", "480973178", 
"521735637", "900213476", "205774438", "721472220", "250993380", 
"454810098", "200911301", "383763630", "314229730", "300554741", 
"146045548", "465473317", "760402131", "521268658", "742471234", 
"237417715", "770383198", "237410005", "420897662", "954822407", 
"760742003", "251758392", "810641467", "042752504", "141648493", 
"571080743", "930241680", "741198379", "300078509", "630400591", 
"756028696", "810467553", "421206961", "840811908", "273714084", 
"720225967", "231679207", "232010639", "570534193", "956042683", 
"237424429", "582519959", "237024582", "580593403", "341925767", 
"621247459", "363488086", "200857688", "954576761", "208017565", 
"386150526", "581553259", "320390494", "931148843", "455266712", 
"462857218", "760676143", "113835561", "510201684", "911747156", 
"721436720", "760807719", "836003400", "810504125", "432066949", 
"452518707", "272066321", "481115370", "411592982", "510466947", 
"205782342", "223083506", "920185626", "593075070", "330490263", 
"112920409", "946083305", "204791422", "263426758", "136844298", 
"411366265", "746023028", "431298527", "770226589", "273626192", 
"201107218", "330007728", "581423958", "271991142", "042437845", 
"237181863", "200193936", "223135464", "920095913", "953695020", 
"910760837", "352233307", "371505299", "391551378", "203329031", 
"626047237", "231413304", "311308729", "203202277", "161676744", 
"820374496", "470123465", "953119548", "580659988", "943144297", 
"420707114", "810593483", "113616961", "310939742", "205095702", 
"841544750", "201569453", "370212080", "370764215", "263850702", 
"251840914", "472646525", "237026777", "237403306", "331168166", 
"363305973", "204532991", "161662507", "316402416", "562345377", 
"141724406", "061431360", "208170174", "470860675", "010510768", 
"381115312", "930685385", "113081712", "133964284", "237362556", 
"541670786", "131426099", "140706188", "201332310", "953649525", 
"203619604", "261886495", "200036627", "200736119", "161142463", 
"020222122", "202845141", "311051350", "470765107", "133455677", 
"240796034", "650350161", "510141638", "261164672", "146020279", 
"631134289", "900905832", "460447957", "592564232", "453170905", 
"136379135", "251762145", "521830243", "251213006", "113470463", 
"640781985", "331205084", "381869458", "453039286", "460757472", 
"311680598", "061290619", "742069099", "203934581", "510201811", 
"231690790", "251158354", "742410127", "311416313", "841650210", 
"310918565", "930576774", "430388927", "455242188", "360724325", 
"264094629", "731696754", "133472524", "462995587", "640314538", 
"930603023", "650387766", "521961154", "651243140", "274552853", 
"341798216", "043422173", "391411316", "596153372", "060932935", 
"742657050", "431823801", "710933645", "112965575", "453249404", 
"954488745", "391607707", "208168836", "260239674", "582393540", 
"421227728", "481241079", "943303385", "421604185", "230901643", 
"131426099", "751281410", "203545411", "910551353", "341975964", 
"200863495", "592315435", "274118692", "237278106", "930948164", 
"942921729", "510237953", "721442365", "043488006", "237388274", 
"752367391", "680046109", "980354404", "026007640", "680164491", 
"386070507", "472495422", "592527684", "237375919", "421323904", 
"274766992", "314379478", "592334063", "520591466", "020334587", 
"043503656", "751844908", "161359060", "311343000", "752400742", 
"371375842", "263131386", "370212950", "370818878", "311130225", 
"010206603", "043531015", "311775793", "450610199", "141715482", 
"752912190", "311486139", "911192064", "391844829", "411992053", 
"132642091", "800334135", "020582674", "260005176", "752559624", 
"911725914", "752111165", "840645455", "351987955", "161113373", 
"042897220", "208720689", "561799522", "940722843", "760088702", 
"263928285", "840952432", "240814118", "270296118", "363796181", 
"800227070", "481248443", "391807356", "341533689", "362810758", 
"640427465", "740706484", "841305687", "270187676", "271651095", 
"030464855", "341872838", "251834852", "752176054", "954809316", 
"593169557", "630340320", "760106968", "271537539", "275559473", 
"221803021", "460402371", "316060696", "820392371", "510157606", 
"300044814", "431308084", "233034371", "412076312", "911790564", 
"942938348", "363884422", "200831271", "451477861", "742358809", 
"270972853", "470647188", "431286484", "475022580", "203652671", 
"930122405", "221613656", "050462175", "237301516", "261769093", 
"430764111", "465032425", "521528004", "592154072", "042454675", 
"841397409", "720594425", "237094889", "202824933", "526063291", 
"381986068", "943346991", "760175369", "464057749", "200481788", 
"135539807", "474810028", "200900981", "133174602", "236414296", 
"562087110", "160741816", "383602221", "931293019", "381387122", 
"232591316", "341160220", "680266030", "943161863", "721585651", 
"426018990", "431665992", "382391068", "042606857", "954056741", 
"341747342", "262061726", "270538837", "237156560", "591195405", 
"264663584", "630510904", "341110901", "030599253", "742688078", 
"273033901", "010910861", "910934552", "841635749", "363051900", 
"043275098", "030264149", "680049924", "570670742", "541115312", 
"200971486", "930146820", "362889883", "751909562", "133376303", 
"951916051", "540937966", "363371659", "621826358", "262511045", 
"205499127", "770348681", "112500538", "990351979", "270917237", 
"941700718", "943275981", "204238821", "205668144", "351971431", 
"363739630", "820382001", "270733930", "591756933", "520591413", 
"030179299", "721415610", "202207634", "990082181", "161126262", 
"222768668", "311000354", "810414547", "510193969", "396075608", 
"330676791", "800035158", "841550842", "751709180", "510251108", 
"232902139", "812087221", "203868366", "237036305", "480858281", 
"043580947", "742323822", "521273585", "953013310", "381309993", 
"580801717", "364217577", "204875861", "263774888", "270353666", 
"362553797", "382921300", "223802830", "621150503", "580312254", 
"481141444", "340868798", "911669119", "570115088", "042103854", 
"030102265", "546054836", "200154989", "731661458", "582132516", 
"223150402", "010881587", "510644783", "208140961", "770019347", 
"561913994", "237282033", "590686769", "943152742", "521266582", 
"431242842", "273143754", "460355127", "300093284", "264803719", 
"866057676", "351047413", "742586154", "382530216", "820261086", 
"264291973", "208560529", "391563286", "953657496", "452847170", 
"680465984", "731127500", "132795571", "521932138", "274496147", 
"390944097", "311152977", "204225177", "990290389", "112436848", 
"462695778", "752859064", "455329399", "223884319", "431371143", 
"721387536", "450454056", "010533972", "640219645", "273205476", 
"251585685", "640308935", "480758105", "752963717", "943124729", 
"920093463", "134185397", "741261718", "232383155", "161693220", 
"596169745", "521309705", "421741600", "382164580", "263109696", 
"711003737", "204724799", "262789307", "472565110", "356041567", 
"264048763", "116017985", "066260375", "760157412", "205297040", 
"043209075", "460471316", "204849519", "311448122", "205546131", 
"460599821", "382468643", "371126810", "651189617", "900958731", 
"112462594", "042684459", "510192902", "562227722", "382082993", 
"237433570", "383575399", "043323169", "391385391", "510203229", 
"741181708", "900942338", "203973862", "943297241", "311774892", 
"953964928", "462483195", "464593465", "841352664", "461068529", 
"240770786", "043555545", "421685278", "341400918", "311667995", 
"050447391", "222202843", "800230452", "042623573", "363580125", 
"752820283", "470783326", "454753585", "431238407", "743010326", 
"610444843", "391552886", "430729359", "391856935", "460506224", 
"431681945", "270037508", "300147221", "237208134", "350679800", 
"261467144", "460670388", "942894849", "371121504", "237044560", 
"911731833", "510216642", "364433994", "830447441", "223050662", 
"274729995", "133663180", "205935626", "463415835", "341403393", 
"450363454", "201764724", "341351588", "270645485", "396078335", 
"590258435", "421389223", "420398820", "370223453", "550749993", 
"930748169", "830240406", "760035517", "237331238", "204628418", 
"310891445", "391759169", "581123741", "391362996", "274656798", 
"363890062", "464228639", "421009507", "460796907", "411599007", 
"460599813", "942708024", "571117542", "134091963", "470550845", 
"840804957", "222191353", "236423342", "271432690", "205989540", 
"362934532", "410634559", "133348620", "580520688", "411747145", 
"461159883", "391200332", "421667888", "330419461", "860096795", 
"454365069", "736104113", "203468782", "736111847", "371272207", 
"830298259", "431793352", "113157910", "311833734", "411703456", 
"237107484", "720927228", "391102430", "830318392", "382946309", 
"363892403", "261987732", "381558078", "232248803", "161602086", 
"310905748", "860841345", "461559207", "591148342", "222301675", 
"742403338", "381288014", "521055738", "261704829", "270898346", 
"880142068", "223682199", "582628872", "205910152", "941634554", 
"521119350", "463275283", "042805551", "810469886", "204388158", 
"860169935", "742708627", "943098473", "956118534", "954554824", 
"390506432", "204623687", "866052939", "263432434", "522142963", 
"201649101", "351447453", "150270826", "431754347", "311672352", 
"411467352", "480874448", "912161100", "311492241", "454576856", 
"410498482", "943165001"), TaxPeriod = c("201506", "201606", 
"201012", "201712", "201703", "201306", "201412", "201006", "201512", 
"201309", "201612", "201606", "201706", "201212", "201412", "201012", 
"201306", "201506", "201706", "201705", "201206", "201112", "201308", 
"201612", "201412", "201406", "201306", "201412", "201606", "201412", 
"201612", "201212", "201512", "201112", "201312", "201612", "201412", 
"201212", "201105", "201106", "201412", "201106", "201703", "201212", 
"201112", "201409", "201706", "201403", "201112", "201612", "201506", 
"201112", "201112", "201412", "201512", "201512", "201012", "201206", 
"201412", "201712", "201312", "201212", "201409", "201104", "201705", 
"201412", "201506", "201512", "201710", "201512", "201706", "201210", 
"201506", "201612", "201112", "201206", "201506", "201612", "201612", 
"201710", "201412", "201612", "201606", "201512", "201212", "201012", 
"201206", "201512", "201512", "201506", "201410", "201312", "201405", 
"201307", "201212", "201706", "201212", "201406", "201512", "201603", 
"201012", "201212", "201709", "201412", "201412", "201512", "201512", 
"201512", "201106", "201612", "201012", "201412", "201012", "201706", 
"201312", "201106", "201607", "201012", "201112", "201108", "201112", 
"201612", "201012", "201212", "201405", "201406", "201412", "201312", 
"201506", "201406", "201312", "201412", "201306", "201512", "201512", 
"201212", "201212", "201709", "201606", "201009", "201506", "201412", 
"201112", "201106", "201312", "201408", "201306", "201012", "201112", 
"201612", "201612", "201206", "201512", "201312", "201112", "201409", 
"201406", "201209", "201412", "201406", "201610", "201512", "201612", 
"201106", "201706", "201309", "201706", "201505", "201512", "201412", 
"201512", "201409", "201512", "201412", "201506", "201706", "201309", 
"201112", "201412", "201406", "201206", "201112", "201612", "201212", 
"201312", "201212", "201606", "201309", "201612", "201206", "201612", 
"201112", "201606", "201506", "201412", "201206", "201312", "201112", 
"201303", "201612", "201406", "201306", "201512", "201006", "201612", 
"201612", "201212", "201412", "201506", "201512", "201306", "201512", 
"201012", "201406", "201507", "201112", "201612", "201312", "201612", 
"201012", "201012", "201606", "201612", "201412", "201307", "201512", 
"201312", "201112", "201105", "201406", "201405", "201612", "201612", 
"201609", "201312", "201112", "201112", "201212", "201612", "201012", 
"201212", "201706", "201312", "201306", "201206", "201112", "201612", 
"201205", "201112", "201212", "201608", "201406", "201612", "201612", 
"201512", "201212", "201212", "201112", "201508", "201612", "201512", 
"201312", "201606", "201312", "201312", "201309", "201512", "201508", 
"201406", "201606", "201112", "201512", "201306", "201412", "201112", 
"201406", "201112", "201506", "201506", "201505", "201406", "201312", 
"201706", "201201", "201612", "201006", "201406", "201004", "201406", 
"201206", "201412", "201612", "201612", "201312", "201612", "201509", 
"201212", "201406", "201612", "201006", "201312", "201203", "201606", 
"201706", "201612", "201706", "201112", "201309", "201703", "201206", 
"201706", "201709", "201212", "201008", "201109", "201206", "201208", 
"201112", "201112", "201112", "201412", "201212", "201509", "201105", 
"201406", "201506", "201312", "201403", "201612", "201512", "201606", 
"201406", "201012", "201306", "201706", "201606", "201406", "201505", 
"201406", "201210", "201106", "201112", "201312", "201406", "201112", 
"201406", "201006", "201512", "201612", "201312", "201212", "201212", 
"201108", "201306", "201312", "201206", "201412", "201512", "201412", 
"201612", "201507", "201112", "201312", "201612", "201412", "201312", 
"201412", "201412", "201312", "201506", "201509", "201312", "201012", 
"201312", "201512", "201403", "201104", "201112", "201306", "201606", 
"201412", "201412", "201406", "201612", "201112", "201012", "201612", 
"201509", "201212", "201506", "201706", "201208", "201708", "201606", 
"201206", "201012", "201512", "201505", "201412", "201412", "201306", 
"201002", "201112", "201705", "201212", "201206", "201106", "201106", 
"201012", "201106", "201112", "201509", "201312", "201412", "201708", 
"201212", "201312", "201512", "201112", "201612", "201512", "201112", 
"201312", "201212", "201406", "201506", "201203", "201504", "201412", 
"201609", "201312", "201112", "201206", "201706", "201304", "201112", 
"201108", "201006", "201212", "201112", "201412", "201412", "201506", 
"201409", "201503", "201212", "201212", "201701", "201706", "201112", 
"201406", "201312", "201512", "201709", "201312", "201312", "201406", 
"201512", "201612", "201006", "201406", "201506", "201312", "201012", 
"201212", "201012", "201512", "201606", "201612", "201606", "201412", 
"201112", "201412", "201709", "201312", "201112", "201512", "201612", 
"201206", "201412", "201412", "201506", "201112", "201312", "201412", 
"201612", "201608", "201006", "201006", "201012", "201612", "201706", 
"201512", "201012", "201312", "201602", "201705", "201309", "201312", 
"201612", "201512", "201012", "201612", "201612", "201408", "201608", 
"201112", "201306", "201608", "201109", "201512", "201512", "201405", 
"201704", "201108", "201112", "201106", "201312", "201612", "201309", 
"201312", "201406", "201212", "201312", "201606", "201106", "201312", 
"201406", "201112", "201312", "201106", "201206", "201509", "201712", 
"201512", "201506", "201512", "201412", "201206", "201212", "201312", 
"201306", "201112", "201109", "201408", "201306", "201112", "201208", 
"201606", "201312", "201411", "201412", "201212", "201412", "201212", 
"201709", "201212", "201004", "201412", "201109", "201312", "201309", 
"201109", "201406", "201412", "201206", "201112", "201512", "201112", 
"201206", "201212", "201302", "201310", "201112", "201312", "201512", 
"201409", "201209", "201112", "201612", "201612", "201412", "201612", 
"201710", "201412", "201709", "201203", "201406", "201012", "201604", 
"201212", "201112", "201412", "201112", "201306", "201312", "201612", 
"201406", "201506", "201412", "201512", "201112", "201512", "201412", 
"201512", "201202", "201109", "201206", "201112", "201006", "201306", 
"201312", "201412", "201012", "201103", "201406", "201412", "201312", 
"201412", "201006", "201312", "201712", "201706", "201103", "201506", 
"201212", "201112", "201212", "201105", "201512", "201612", "201512", 
"201206", "201512", "201312", "201709", "201306", "201507", "201412", 
"201512", "201512", "201412", "201012", "201012", "201712", "201212", 
"201706", "201206", "201612", "201612", "201212", "201312", "201509", 
"201106", "201512", "201512", "201606", "201012", "201409", "201703", 
"201312", "201312", "201609", "201612", "201412", "201312", "201604", 
"201612", "201707", "201609", "201006", "201503", "201207", "201512", 
"201512", "201212", "201612", "201012", "201612", "201506", "201306", 
"201412", "201209", "201512", "201706", "201103", "201012", "201606", 
"201505", "201706", "201206", "201406", "201706", "201606", "201405", 
"201108", "201412", "201509", "201512", "201412", "201212", "201712", 
"201301", "201512", "201312", "201312", "201606", "201406", "201106", 
"201106", "201212", "201012", "201512", "201402", "201512", "201312", 
"201312", "201112", "201512", "201312", "201212", "201012", "201006", 
"201507", "201612", "201412", "201306", "201312", "201306", "201506", 
"201406", "201006", "201603", "201611", "201306", "201012", "201312", 
"201412", "201512", "201509", "201612", "201206", "201303", "201406", 
"201606", "201606", "201012", "201206", "201606", "201512", "201407", 
"201112", "201606", "201512", "201512", "201406", "201212", "201112", 
"201306", "201106", "201312", "201212", "201112", "201207", "201612", 
"201612", "201312", "201112", "201606", "201012", "201112", "201406", 
"201506", "201312", "201512", "201212", "201609", "201512", "201412", 
"201202", "201512", "201512", "201412", "201212", "201112", "201412", 
"201506", "201106", "201609", "201112", "201305", "201312", "201512", 
"201209", "201612", "201406", "201112", "201612", "201703", "201507", 
"201612", "201112", "201401", "201306", "201306", "201512", "201512", 
"201309", "201003", "201609", "201304", "201506", "201012", "201112", 
"201312", "201412", "201512", "201412", "201412", "201412", "201306", 
"201702", "201512", "201210", "201206", "201312", "201012", "201012", 
"201512", "201506", "201012", "201312", "201412", "201406", "201705", 
"201412", "201512", "201506", "201506", "201412", "201012", "201012", 
"201012", "201305", "201606", "201312", "201312", "201512", "201006", 
"201412", "201512", "201112", "201609", "201512", "201612", "201606", 
"201506", "201312", "201312", "201612", "201312", "201107", "201312", 
"201612", "201012", "201512", "201309", "201306", "201612", "201512", 
"201209", "201105", "201012", "201405", "201606", "201504", "201412", 
"201512", "201609", "201412", "201406", "201503", "201012", "201309", 
"201412", "201012", "201512", "201703", "201212", "201112", "201312", 
"201612", "201706", "201112", "201612", "201505", "201612", "201606", 
"201512", "201212", "201509", "201012", "201509", "201306", "201706", 
"201609", "201606", "201612", "201609", "201612", "201212", "201612", 
"201106", "201506", "201512", "201012", "201612", "201201", "201412", 
"201410", "201306", "201512", "201612", "201706", "201012", "201012", 
"201509", "201409", "201612", "201506", "201012", "201509", "201306", 
"201512", "201012", "201212", "201606", "201112", "201612", "201609", 
"201412", "201706", "201406", "201410", "201512", "201512", "201512", 
"201506", "201412", "201502", "201612", "201012", "201406", "201106", 
"201112", "201212", "201506", "201612", "201506", "201606", "201409", 
"201112", "201212", "201412", "201512", "201506", "201212", "201612", 
"201612", "201506", "201612", "201612", "201111", "201106", "201412", 
"201612", "201506", "201306", "201512", "201112", "201012", "201206", 
"201112", "201612", "201212", "201205", "201412", "201512", "201612", 
"201508", "201612", "201112", "201012", "201307", "201509", "201012", 
"201412", "201606", "201412", "201112"), DLN = c("93493316058865", 
"93492319014426", "93492319016391", "93492100004358", "93492043001108", 
"93492214005013", "93493271004085", "93493133003301", "93493320124376", 
"93492035004234", "93493178002157", "93493291008926", "93492024006078", 
"93493268003173", "93493320061385", "93493227041811", "93493134061834", 
"93493322005215", "93493015007158", "93493278012117", "93493293006192", 
"93492129009152", "93492053007064", "93492167003207", "93492285001065", 
"93493318044864", "93493318054763", "93493056007195", "93493342002266", 
"93493176008115", "93493313025217", "93492073007213", "93493137081156", 
"93492286002042", "93493224009254", "93493255005487", "93493134003485", 
"93493319064913", "93492290000001", "93493314013271", "93492112005165", 
"93493315037511", "93493045004208", "93492067001233", "93493223015642", 
"93493021007255", "93493052009958", "93493192000034", "93492136037652", 
"93492066005287", "93493139010106", "93493228045032", "93492229007142", 
"93493320025285", "93493228029376", "93493075007216", "93493319056261", 
"93492258005132", "93493316005495", "93493135115748", "93492258003444", 
"93492164000143", "93493043004125", "93493062016712", "93492229000157", 
"93493166008255", "93493043031716", "93493057000226", "93492102005278", 
"93493306005096", "93493102007018", "93493072001193", "93492054003026", 
"93493133008707", "93493123006312", "93493120009633", "93493357001005", 
"93493313013577", "93493317065437", "93493318011487", "93493131031725", 
"93492314000427", "93493129018357", "93493068007157", "93493207012033", 
"93492122003501", "93493320074582", "93492215000066", "93492102004386", 
"93493131030836", "93493233005145", "93493133012224", "93493272001454", 
"93492167003474", "93493212005123", "93493347001057", "93492317024793", 
"93493015011945", "93493229007466", "93493038012047", "93492131016531", 
"93493317021213", "93492071006428", "93492308002065", "93492307004015", 
"93492228000416", "93493320071226", "93493078008596", "93492088002302", 
"93492227006977", "93493181008071", "93493131015385", "93492129009141", 
"93493313034067", "93493321096884", "93493259007001", "93492235006166", 
"93493088007221", "93492090007472", "93492071005042", "93493135053112", 
"93492174006037", "93492251006121", "93492126005273", "93493098003135", 
"93492048000045", "93493155007255", "93493358005014", "93492019005076", 
"93492237003684", "93492175000284", "93493126009705", "93493319037173", 
"93493085003126", "93493132037306", "93492221007233", "93493111002013", 
"93493075011098", "93493135096667", "93493223012231", "93493134011246", 
"93493133006325", "93493136061522", "93493265007141", "93493073008364", 
"93492105000085", "93493189000383", "93493153001061", "93493074007072", 
"93493319178007", "93493228012017", "93492107003123", "93492144002176", 
"93492316024824", "93492320009422", "93492322006404", "93493318032194", 
"93492024003073", "93493315018665", "93493132014405", "93493072010547", 
"93493320099816", "93493135020777", "93492062009532", "93492349004217", 
"93493227038414", "93492223005147", "93493014001266", "93492057006126", 
"93493229015335", "93492228013686", "93493156008165", "93493162000156", 
"93493226005375", "93493356000355", "93492230005007", "93492112003124", 
"93493128009252", "93493232002065", "93492321005484", "93493122007233", 
"93493080000412", "93492131029267", "93493217005123", "93493318087834", 
"93492221002453", "93493100005097", "93492203006124", "93492201003277", 
"93493348005092", "93493160000327", "93493227041662", "93492316005386", 
"93492308002435", "93493317049265", "93493044010093", "93493321117494", 
"93492241004202", "93493253002253", "93492222004197", "93493321057054", 
"93492318007513", "93493320122576", "93492046006161", "93493031001038", 
"93493317074547", "93492063006083", "93493126002125", "93493134032406", 
"93492133006346", "93493312018383", "93493195004457", "93492131016741", 
"93493318094184", "93492020008036", "93493312014052", "93493061001067", 
"93492154000084", "93492262002107", "93493133038281", "93493124004411", 
"93493046035067", "93493319124407", "93493320061605", "93493345006423", 
"93493133029236", "93493321030034", "93493199000212", "93493339000611", 
"93493267005394", "93493015003295", "93493135070447", "93492146001137", 
"93493045027047", "93492321040004", "93492299000232", "93492215003162", 
"93493133034043", "93493318088357", "93492068007171", "93492130018673", 
"93493270010227", "93492133023734", "93492319041723", "93493311011642", 
"93493320042962", "93492131015917", "93493353010072", "93493136063372", 
"93492134042203", "93493193004267", "93492124012035", "93493240001197", 
"93493156002147", "93492204000066", "93493219009303", "93492293004023", 
"93493285011632", "93493009008037", "93493318130947", "93493239004036", 
"93493317069534", "93493135119867", "93493281013094", "93492318004064", 
"93493045023414", "93493137070676", "93493079002086", "93493321056154", 
"93492340002516", "93493215007082", "93493319111766", "93493098002234", 
"93493218010815", "93493223016522", "93493318065764", "93492130006312", 
"93493133056766", "93493090006126", "93493109000096", "93492232001094", 
"93492118009334", "93493319080887", "93493240003162", "93493201005367", 
"93493131005361", "93493321025524", "93492012006281", "93493134092155", 
"93492289004492", "93493320068975", "93492068002227", "93493130017077", 
"93493318047984", "93492178005167", "93492343001145", "93493191000343", 
"93493020006175", "93493311023347", "93493021010021", "93492321031894", 
"93492163002122", "93492025004367", "93492123004478", "93492074006457", 
"93493296013587", "93493076000382", "93493345005123", "93493321001487", 
"93492317011502", "93493101008498", "93493045015078", "93493283011603", 
"93493012007701", "93492075004202", "93493348006242", "93492014001003", 
"93493319009402", "93493319054602", "93493320083602", "93493133024645", 
"93493316018833", "93493040003186", "93492290002311", "93492259006104", 
"93492039002386", "93493314026614", "93492044003115", "93493103007397", 
"93493217009136", "93493017014087", "93493308009374", "93493319057901", 
"93493119011004", "93493131041058", "93493051007017", "93493134041855", 
"93493324002255", "93493135085235", "93493061008113", "93492319030131", 
"93493143003002", "93493234002054", "93492253007124", "93493221014062", 
"93492273000164", "93493026009041", "93492237004066", "93492080007127", 
"93492226008884", "93492318041043", "93492135014303", "93493107002312", 
"93493316014523", "93492135034694", "93492133026553", "93493125005465", 
"93493136007426", "93493168003215", "93493240008007", "93493014015016", 
"93493310005092", "93493216007074", "93493292015007", "93493281004255", 
"93493316032154", "93493219003305", "93492092006005", "93493153004204", 
"93493136012516", "93493021002106", "93493321075924", "93493262005091", 
"93493321019644", "93493314030526", "93493287007354", "93493066011242", 
"93492223006262", "93493127018074", "93493131032967", "93493120004095", 
"93493134058755", "93493081002505", "93493319025067", "93493214007782", 
"93492133026241", "93493314037987", "93492067004346", "93493126010893", 
"93492133006256", "93492258009617", "93493007014733", "93492014004028", 
"93492286001316", "93493134027963", "93493312008061", "93492139000236", 
"93492102004306", "93493134025045", "93493229036785", "93493134028724", 
"93493012012181", "93493310013812", "93493060007118", "93492221009573", 
"93492045003393", "93493314020201", "93493243005091", "93493228011351", 
"93493132008402", "93493132012502", "93493042023176", "93492206003214", 
"93493111000175", "93492354008227", "93493097003533", "93493266001344", 
"93493132046056", "93493135008062", "93493131018247", "93493253010376", 
"93493227019282", "93493289007064", "93493175001323", "93493015001215", 
"93492043006436", "93493275001152", "93493025007556", "93493090007985", 
"93493045012367", "93493254005004", "93493345005122", "93493337007114", 
"93492229008207", "93492291004013", "93493150001052", "93493166003072", 
"93493042001131", "93493235000063", "93493206004042", "93493058005175", 
"93493229022595", "93493307000095", "93493012004055", "93492294006305", 
"93493134054443", "93493318001343", "93493164008287", "93493048006078", 
"93492227001402", "93493134032525", "93493101009334", "93493107002056", 
"93493320018047", "93492112007064", "93493135091324", "93492342006034", 
"93493134044846", "93493269008587", "93493133023951", "93493132014135", 
"93493084006016", "93493133025254", "93492319040581", "93492109002123", 
"93493319024011", "93492318002206", "93492045014727", "93493130020307", 
"93493121000367", "93493229036045", "93493219000042", "93493159003485", 
"93493040005258", "93492199000134", "93493271001272", "93492319005406", 
"93492181000057", "93493135022123", "93492126012925", "93492133010505", 
"93493320168126", "93493222013742", "93493321043364", "93492223010795", 
"93493284006287", "93492098003077", "93493133042121", "93492216003210", 
"93493320003201", "93493319042267", "93493312004447", "93493194008416", 
"93492308004011", "93493308014054", "93493013012687", "93492230001247", 
"93493227007084", "93493308013084", "93493318125217", "93493259010946", 
"93493315005161", "93492317031757", "93493319002217", "93493020008405", 
"93493198019077", "93493256003282", "93492048004034", "93493012003047", 
"93493223007142", "93492214010586", "93493190000136", "93493012008695", 
"93492310010997", "93493103005442", "93492122006212", "93493271002301", 
"93493101003125", "93492228005037", "93493219004354", "93493321050714", 
"93492226021674", "93493070004333", "93493317064154", "93493096013057", 
"93493125004322", "93493321052454", "93493135046715", "93493233001182", 
"93492317027164", "93493308017061", "93493122005193", "93493041009296", 
"93492046006398", "93492225015256", "93493246008005", "93493216013036", 
"93493320108895", "93492251001242", "93493319053763", "93493294004284", 
"93493048017514", "93492074004322", "93493089009212", "93493033000785", 
"93493354008083", "93493129014182", "93492307006192", "93493052010117", 
"93492135028274", "93493218018035", "93493138000055", "93492221000103", 
"93493317023405", "93492134013093", "93493016004258", "93493168001743", 
"93492254000010", "93493202012035", "93492070003172", "93492316031004", 
"93493028008184", "93492097004202", "93493120006265", "93493320040255", 
"93492283000722", "93492272003062", "93493131014746", "93493319034482", 
"93492319023002", "93493175002203", "93492193002033", "93493059002264", 
"93493081003032", "93492133013694", "93492319019226", "93493127020255", 
"93493176005473", "93492227016362", "93493170002357", "93493188004207", 
"93493133036955", "93492128015787", "93493024000038", "93492131015825", 
"93493052002018", "93493024002133", "93493307001174", "93493136066101", 
"93493328006036", "93493226016663", "93493320005362", "93492098000195", 
"93492188003072", "93493049015914", "93492129012304", "93492319068817", 
"93492126012995", "93493299000065", "93493230003295", "93493061002116", 
"93492180007222", "93493223001366", "93493300009395", "93493137026936", 
"93493331003222", "93493107006232", "93493278001122", "93493198004012", 
"93493131028331", "93493259006483", "93493317060114", "93493135092955", 
"93493319032041", "93493227006031", "93493318021514", "93492166003025", 
"93492318035634", "93492133005435", "93493028002041", "93493321114564", 
"93492043005118", "93493019008128", "93493180009011", "93493273005485", 
"93493226015793", "93492152002292", "93493176007143", "93493338003001", 
"93493109009686", "93493135079157", "93493319126026", "93493052002093", 
"93493146006386", "93493128011354", "93493046016248", "93493029002084", 
"93493057001176", "93493119002165", "93493227005096", "93493308020056", 
"93493320031125", "93493319046671", "93493276007681", "93492057003028", 
"93493317037563", "93492122005088", "93492307004132", "93493137005037", 
"93493319053417", "93492119004423", "93493318049604", "93493356003015", 
"93493339000041", "93493126000186", "93492179006286", "93493298011216", 
"93492029006021", "93493135081705", "93492187002277", "93493311016554", 
"93493134076114", "93492045013267", "93492318024227", "93493139009055", 
"93492124008004", "93492319016636", "93492206004077", "93493072009308", 
"93493227000387", "93492074005251", "93493043025196", "93493067000683", 
"93493286004456", "93492320013716", "93493221007253", "93493318071247", 
"93493158006721", "93493177012077", "93493020000126", "93493346008403", 
"93493125002385", "93493225008393", "93492227001276", "93493116009468", 
"93492167006021", "93493318003151", "93493135048557", "93493287001475", 
"93493066007108", "93493042002333", "93493065007185", "93493135134598", 
"93492180000167", "93493352007264", "93493098006012", "93493275007335", 
"93492357001135", "93493320131326", "93492187004035", "93493176001083", 
"93492022006628", "93492102004193", "93493133028856", "93492129003364", 
"93492084006094", "93493295007006", "93493012014875", "93493082008202", 
"93493294010091", "93493060000123", "93493318074311", "93492135005096", 
"93493280006514", "93493307004436", "93493321115464", "93492252000234", 
"93493110001032", "93493132044216", "93492223013134", "93492188000043", 
"93493269003271", "93492042009031", "93492348003135", "93492125005087", 
"93492219005155", "93492288010023", "93492199000264", "93492235001083", 
"93493134078356", "93493042000275", "93493133023321", "93492216005336", 
"93493107007137", "93493268001243", "93492228013071", "93493184006114", 
"93493313023765", "93493043025476", "93493046002276", "93492314014937", 
"93493134044063", "93493319018053", "93493317001414", "93493025009137", 
"93492318006126", "93493227033302", "93493133035773", "93493313006176", 
"93492137014486", "93492284002014", "93492226008802", "93493131035767", 
"93492320005106", "93493050010096", "93493037006195", "93493177002443", 
"93492130008342", "93493028002114", "93493067007282", "93492231001024", 
"93492153007204", "93493228025222", "93492340003033", "93493132058557", 
"93493142001127", "93493125001164", "93492124000052", "93493302010946", 
"93493220005011", "93493320091712", "93493295002354", "93493047022006", 
"93492127004424", "93493235006226", "93493122011223", "93492013008397", 
"93493162000086", "93492055005075", "93493172002252", "93492119007106", 
"93492116003046", "93492236001285", "93492225009993", "93493221002122", 
"93492103004165", "93493050011026", "93492315004121", "93493123004307", 
"93493228028572", "93492241006213", "93492210005234", "93493319064096", 
"93492355006102", "93493180007487", "93493126017505", "93493074010312", 
"93493257002037", "93493019007178", "93493343002395", "93493129003057", 
"93492240005092", "93493212007324", "93493132030164", "93493135039354", 
"93493095001326", "93492091005006", "93492045000224", "93493028004061", 
"93492173000087", "93493073001184", "93493134045916", "93492318024861", 
"93492080003182", "93492321010194", "93493314030685", "93492074010046", 
"93493155002245", "93492173000025", "93493114001185", "93493318017503", 
"93492015001158", "93493314027876", "93492033000023", "93493046021163", 
"93492171001024", "93492234001041", "93492131012051", "93492063007236", 
"93493137008916", "93493060006001", "93493196011154", "93493310003295", 
"93493028007095", "93493277015727", "93493278012535", "93493224016366", 
"93493302006315", "93493029010416", "93493224013715", "93493265006111", 
"93493322004191", "93493136038371", "93493329002113", "93493044002007", 
"93493202003104", "93493121012324", "93492292006166", "93493136067331", 
"93492205005205", "93492138007736", "93493194008122", "93492223003287", 
"93493218008026", "93493319104427", "93493272008016", "93493043022396", 
"93493220002064", "93492121001344", "93492317005367", "93493134028194", 
"93493348006191", "93492134035084", "93492129021557", "93492315017011", 
"93492133000456", "93493227012194", "93492045000104", "93493159012027", 
"93493124010826", "93493224011333", "93493072015562", "93492315017571", 
"93492015004185", "93493135071467", "93492074007126", "93493211011635", 
"93493132019426", "93492023006797", "93492107001025", "93493135097505", 
"93493253007035", "93493131016151", "93493230011004", "93492229023225", 
"93493143003191", "93493098003126", "93493223008407", "93493135076113", 
"93493240000352", "93492134016864", "93493319176437", "93493347001107", 
"93492320023762", "93492188003057", "93492334002025", "93492312006267", 
"93492321008066", "93493320122406", "93493063000113", "93493221004226", 
"93493318076011", "93492044002056", "93493044022894", "93492313009177", 
"93493139000257", "93493129026327", "93493124017047", "93493226028857", 
"93493317067337", "93492274004123", "93492132015257", "93493132029542", 
"93493309002295", "93492134033856", "93492131005101", "93492181001077", 
"93493220001372", "93492100000115", "93492020001075", "93493046004044", 
"93493319118526", "93492314008207", "93493318086627", "93493306009061", 
"93493319057261", "93493046023806", "93493044011835", "93492289011517", 
"93492320053155", "93492116006221", "93493221018086", "93493015010364", 
"93493099003066", "93493227023651", "93492204005023", "93493131016627", 
"93493278004222", "93493129026017", "93492017004147", "93493184001035", 
"93493022010008", "93493135017795", "93493048002455", "93492260004016", 
"93493181004206", "93493279002456", "93493029013136", "93492128008275", 
"93493257002205", "93493152005047", "93492318027581", "93493316008924", 
"93493136058112", "93493083007042", "93493127010143", "93492138010586", 
"93492319028897", "93493132030136", "93492320015496", "93492023000105", 
"93493201001112", "93493106001093", "93492106002065", "93493060009586", 
"93493134065006", "93492210002043", "93493160010077", "93493237005437", 
"93493131014996", "93493313023587", "93492318041487", "93493289008272", 
"93493081002162", "93492135050565", "93493128005037", "93493040015056", 
"93492318007333", "93493133062356", "93492319017582", "93493305014091", 
"93493007010143", "93493150007382", "93493088011137", "93492084006103", 
"93492269004102", "93493125013015", "93492221008146", "93492319026567", 
"93493189002076", "93493135013197", "93493320085812", "93493207008151", 
"93492077003094", "93493349007575", "93493049001091", "93493226005355", 
"93492037005407", "93492134026895", "93492320003012"), FormType = c("990", 
"990EZ", "990EZ", "990EZ", "990EZ", "990EZ", "990", "990", "990", 
"990EZ", "990", "990", "990EZ", "990", "990", "990", "990", "990", 
"990", "990", "990", "990EZ", "990EZ", "990EZ", "990EZ", "990", 
"990", "990", "990", "990", "990", "990EZ", "990", "990EZ", "990", 
"990", "990", "990", "990EZ", "990", "990EZ", "990", "990", "990EZ", 
"990", "990", "990", "990", "990EZ", "990EZ", "990", "990", "990EZ", 
"990", "990", "990", "990", "990EZ", "990", "990", "990EZ", "990EZ", 
"990", "990", "990EZ", "990", "990", "990", "990EZ", "990", "990", 
"990", "990EZ", "990", "990", "990", "990", "990", "990", "990", 
"990", "990EZ", "990", "990", "990", "990EZ", "990", "990EZ", 
"990EZ", "990", "990", "990", "990", "990EZ", "990", "990", "990EZ", 
"990", "990", "990", "990EZ", "990", "990EZ", "990EZ", "990EZ", 
"990EZ", "990", "990", "990EZ", "990EZ", "990", "990", "990EZ", 
"990", "990", "990", "990EZ", "990", "990EZ", "990EZ", "990", 
"990EZ", "990EZ", "990EZ", "990", "990EZ", "990", "990", "990EZ", 
"990EZ", "990EZ", "990", "990", "990", "990", "990EZ", "990", 
"990", "990", "990", "990", "990", "990", "990", "990", "990EZ", 
"990", "990", "990", "990", "990", "990EZ", "990EZ", "990EZ", 
"990EZ", "990EZ", "990", "990EZ", "990", "990", "990", "990", 
"990", "990EZ", "990EZ", "990", "990EZ", "990", "990EZ", "990", 
"990EZ", "990", "990", "990", "990", "990EZ", "990EZ", "990", 
"990", "990EZ", "990", "990", "990EZ", "990", "990", "990EZ", 
"990", "990EZ", "990EZ", "990", "990", "990", "990EZ", "990EZ", 
"990", "990", "990", "990EZ", "990", "990EZ", "990", "990EZ", 
"990", "990EZ", "990", "990", "990EZ", "990", "990", "990EZ", 
"990", "990", "990EZ", "990", "990EZ", "990", "990", "990EZ", 
"990EZ", "990", "990", "990", "990", "990", "990", "990", "990", 
"990", "990", "990", "990", "990", "990EZ", "990", "990EZ", "990EZ", 
"990EZ", "990", "990", "990EZ", "990EZ", "990", "990EZ", "990EZ", 
"990", "990", "990EZ", "990", "990", "990EZ", "990", "990EZ", 
"990", "990", "990EZ", "990", "990EZ", "990", "990", "990", "990", 
"990", "990", "990", "990EZ", "990", "990", "990", "990", "990EZ", 
"990", "990", "990", "990", "990", "990", "990EZ", "990", "990", 
"990", "990EZ", "990EZ", "990", "990", "990", "990", "990", "990EZ", 
"990", "990EZ", "990", "990EZ", "990", "990", "990EZ", "990EZ", 
"990", "990", "990", "990", "990EZ", "990EZ", "990EZ", "990EZ", 
"990EZ", "990", "990", "990", "990", "990EZ", "990", "990", "990", 
"990", "990EZ", "990", "990EZ", "990", "990", "990", "990", "990", 
"990", "990EZ", "990EZ", "990EZ", "990", "990EZ", "990", "990", 
"990", "990", "990", "990", "990", "990", "990", "990", "990", 
"990", "990EZ", "990", "990", "990EZ", "990", "990EZ", "990", 
"990EZ", "990EZ", "990EZ", "990EZ", "990EZ", "990", "990", "990EZ", 
"990EZ", "990", "990", "990", "990", "990", "990", "990", "990", 
"990", "990", "990", "990EZ", "990", "990", "990", "990", "990", 
"990", "990", "990", "990", "990EZ", "990", "990", "990", "990", 
"990", "990", "990", "990EZ", "990", "990EZ", "990", "990EZ", 
"990EZ", "990", "990EZ", "990EZ", "990", "990", "990EZ", "990EZ", 
"990", "990", "990", "990", "990", "990", "990EZ", "990EZ", "990", 
"990", "990", "990", "990", "990", "990EZ", "990", "990EZ", "990", 
"990", "990", "990", "990", "990", "990", "990", "990", "990", 
"990EZ", "990", "990", "990", "990", "990", "990", "990", "990EZ", 
"990EZ", "990", "990", "990", "990", "990", "990", "990", "990", 
"990", "990EZ", "990", "990", "990", "990", "990EZ", "990", "990", 
"990", "990", "990EZ", "990", "990EZ", "990", "990", "990", "990", 
"990", "990", "990EZ", "990EZ", "990", "990EZ", "990EZ", "990", 
"990", "990", "990", "990", "990", "990EZ", "990", "990EZ", "990EZ", 
"990", "990EZ", "990EZ", "990", "990", "990", "990EZ", "990", 
"990EZ", "990", "990EZ", "990", "990", "990", "990", "990EZ", 
"990", "990", "990EZ", "990", "990", "990", "990", "990", "990EZ", 
"990", "990", "990", "990", "990EZ", "990", "990", "990EZ", "990", 
"990", "990EZ", "990", "990EZ", "990", "990", "990EZ", "990", 
"990", "990EZ", "990", "990", "990", "990", "990", "990", "990", 
"990EZ", "990", "990", "990", "990EZ", "990EZ", "990", "990", 
"990", "990EZ", "990", "990", "990", "990EZ", "990", "990", "990", 
"990", "990EZ", "990", "990EZ", "990", "990", "990EZ", "990", 
"990EZ", "990", "990", "990EZ", "990", "990EZ", "990EZ", "990", 
"990EZ", "990", "990", "990EZ", "990EZ", "990", "990", "990EZ", 
"990", "990EZ", "990", "990", "990EZ", "990EZ", "990", "990", 
"990EZ", "990", "990", "990", "990EZ", "990", "990EZ", "990", 
"990", "990", "990", "990", "990", "990", "990EZ", "990EZ", "990", 
"990EZ", "990EZ", "990EZ", "990", "990", "990", "990EZ", "990", 
"990", "990", "990", "990", "990", "990", "990", "990", "990", 
"990", "990", "990", "990", "990EZ", "990EZ", "990EZ", "990", 
"990", "990EZ", "990", "990", "990", "990", "990EZ", "990", "990", 
"990", "990", "990", "990", "990", "990", "990", "990", "990", 
"990", "990", "990", "990", "990", "990", "990EZ", "990", "990EZ", 
"990EZ", "990", "990", "990EZ", "990", "990", "990", "990", "990EZ", 
"990", "990EZ", "990", "990EZ", "990", "990", "990EZ", "990EZ", 
"990", "990EZ", "990EZ", "990EZ", "990", "990", "990EZ", "990", 
"990", "990", "990EZ", "990", "990", "990", "990", "990", "990", 
"990", "990", "990EZ", "990", "990EZ", "990", "990", "990", "990", 
"990", "990", "990", "990EZ", "990", "990", "990", "990EZ", "990", 
"990EZ", "990", "990EZ", "990EZ", "990", "990EZ", "990EZ", "990", 
"990", "990", "990", "990", "990", "990EZ", "990", "990", "990", 
"990EZ", "990", "990", "990EZ", "990EZ", "990", "990EZ", "990EZ", 
"990EZ", "990EZ", "990EZ", "990EZ", "990EZ", "990", "990", "990", 
"990EZ", "990", "990", "990EZ", "990", "990", "990", "990", "990EZ", 
"990", "990", "990", "990", "990EZ", "990", "990", "990", "990EZ", 
"990EZ", "990EZ", "990", "990EZ", "990", "990", "990", "990EZ", 
"990", "990", "990EZ", "990EZ", "990", "990EZ", "990", "990", 
"990", "990EZ", "990", "990", "990", "990", "990", "990EZ", "990", 
"990", "990EZ", "990", "990EZ", "990", "990EZ", "990EZ", "990EZ", 
"990EZ", "990", "990EZ", "990", "990EZ", "990", "990", "990EZ", 
"990EZ", "990", "990EZ", "990", "990", "990", "990", "990", "990", 
"990", "990EZ", "990", "990", "990", "990", "990EZ", "990EZ", 
"990", "990EZ", "990", "990", "990EZ", "990EZ", "990EZ", "990", 
"990EZ", "990", "990EZ", "990", "990", "990EZ", "990", "990EZ", 
"990", "990EZ", "990EZ", "990EZ", "990EZ", "990", "990", "990", 
"990", "990", "990", "990", "990", "990", "990", "990", "990", 
"990", "990", "990", "990", "990", "990", "990EZ", "990", "990EZ", 
"990EZ", "990", "990EZ", "990", "990", "990", "990", "990", "990EZ", 
"990EZ", "990", "990", "990EZ", "990EZ", "990EZ", "990EZ", "990", 
"990EZ", "990", "990", "990", "990", "990EZ", "990EZ", "990", 
"990EZ", "990", "990", "990EZ", "990EZ", "990", "990", "990", 
"990", "990EZ", "990", "990", "990", "990", "990", "990EZ", "990", 
"990", "990EZ", "990EZ", "990EZ", "990EZ", "990EZ", "990", "990", 
"990", "990", "990EZ", "990", "990EZ", "990", "990", "990", "990", 
"990", "990EZ", "990EZ", "990", "990", "990EZ", "990EZ", "990EZ", 
"990", "990EZ", "990EZ", "990", "990", "990EZ", "990", "990", 
"990", "990", "990", "990EZ", "990EZ", "990EZ", "990", "990", 
"990", "990", "990EZ", "990", "990", "990", "990EZ", "990", "990", 
"990", "990", "990EZ", "990", "990", "990", "990EZ", "990", "990", 
"990EZ", "990", "990", "990", "990", "990EZ", "990EZ", "990", 
"990EZ", "990EZ", "990", "990", "990EZ", "990", "990", "990EZ", 
"990", "990", "990", "990", "990EZ", "990", "990", "990EZ", "990", 
"990", "990EZ", "990", "990EZ", "990", "990", "990", "990", "990EZ", 
"990EZ", "990", "990EZ", "990EZ", "990", "990", "990", "990", 
"990EZ", "990", "990", "990", "990EZ", "990EZ", "990EZ"), URL = c("https://s3.amazonaws.com/irs-form-990/201513169349305886_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201623199349201442_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201143199349201639_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201801009349200435_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201800439349200110_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201312149349200501_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201532719349300408_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201101339349300330_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201623209349312437_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201430359349200423_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201701789349300215_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201622919349300892_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201820249349200607_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201322689349300317_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201533209349306138_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201112279349304181_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201431349349306183_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201513229349300521_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201800159349300715_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201712789349301211_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201242939349300619_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201201299349200915_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201410539349200706_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201701679349200320_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201512859349200106_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201413189349304486_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201313189349305476_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201540569349300719_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201613429349300226_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201511769349300811_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201713139349302521_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201310739349200721_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201601379349308115_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201242869349200204_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201402249349300925_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201732559349300548_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201531349349300348_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201313199349306491_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201102909349200000_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201123149349301327_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201511129349200516_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201113159349303751_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201800459349300420_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201330679349200123_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201242239349301564_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201500219349300725_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201800529349300995_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201431929349300003_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201201369349203765_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201730669349200528_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201601399349301010_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201232289349304503_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201242299349200714_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201533209349302528_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201622289349302937_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201610759349300721_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201113199349305626_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201232589349200513_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201543169349300549_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201841359349311574_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201442589349200344_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201341649349200014_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201520439349300412_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201210629349301671_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201702299349200015_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201501669349300825_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201610439349303171_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201620579349300022_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201821029349200527_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201643069349300509_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201811029349300701_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201340729349300119_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201620549349200302_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201701339349300870_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201211239349300631_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201331209349300963_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201503579349300100_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201723139349301357_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201733179349306543_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201733189349301148_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201521319349303172_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201723149349200042_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201701299349301835_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201700689349300715_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201332079349301203_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201101229349200350_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201233209349307458_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201612159349200006_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201631029349200438_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201631319349303083_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201542339349300514_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201421339349301222_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201402729349300145_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201421679349200347_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201322129349300512_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201703479349300105_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201343179349202479_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201540159349301194_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201612299349300746_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201740389349301204_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201131319349201653_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201313179349302121_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201820719349200642_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201513089349200206_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201513079349200401_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201612289349200041_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201623209349307122_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201640789349300859_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201200889349200230_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201722279349200697_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201121819349300807_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201531319349301538_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201141299349200914_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201713139349303406_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201433219349309688_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201102599349300700_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201612359349200616_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201120889349300722_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201220909349200747_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201240719349200504_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201211359349305311_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201731749349200603_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201122519349200612_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201321269349200527_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201530989349300313_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201540489349200004_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201501559349300725_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201413589349300501_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201620199349200507_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201432379349200368_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201431759349200028_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201501269349300970_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201323199349303717_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201620859349300312_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201601329349303730_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201332219349200723_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201311119349300201_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201840759349301109_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201711359349309666_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201132239349301223_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201641349349301124_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201521339349300632_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201221369349306152_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201142659349300714_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201410739349300836_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201531059349200008_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201331899349300038_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201111539349300106_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201220749349300707_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201703199349317800_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201712289349301201_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201321079349200312_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201621449349200217_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201423169349202482_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201223209349200942_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201403229349200640_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201443189349303219_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201320249349200307_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201513159349301866_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201501329349301440_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201740729349301054_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201613209349309981_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201721359349302077_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201230629349200953_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201713499349200421_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201412279349303841_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201742239349200514_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201610149349300126_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201620579349200612_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201532299349301533_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201632289349201368_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201511569349300816_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201601629349300015_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201522269349300537_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201503569349300035_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201702309349200500_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201421129349200312_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201201289349300925_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201512329349300206_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201433219349200548_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201331229349300723_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201210809349300041_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201711319349202926_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201322179349300512_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201433189349308783_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201302219349200245_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201741009349300509_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201422039349200612_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201722019349200327_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201243489349300509_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201721609349300032_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201212279349304166_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201633169349200538_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201533089349200243_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201513179349304926_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201340449349301009_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201443219349311749_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201202419349200420_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201302539349300225_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201742229349200419_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201403219349305705_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201313189349200751_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201623209349312257_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201110469349200616_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201830319349300103_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201743179349307454_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201330639349200608_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201521269349300212_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201601349349303240_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201641339349200634_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201333129349301838_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201701959349300445_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201141319349201674_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201433189349309418_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201630209349200803_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201203129349301405_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201710619349300106_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201431549349200008_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201702629349200210_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201131339349303828_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201111249349300441_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201710469349303506_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201703199349312440_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201503209349306160_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201323459349300642_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201631339349302923_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201433219349303003_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201211999349300021_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201113399349300061_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201442679349300539_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201540159349300329_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201741359349307044_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201731469349200113_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201740459349302704_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201403219349204000_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201232999349200023_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201212159349200316_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201341339349303404_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201703189349308835_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201120689349200717_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201321309349201867_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201722709349301022_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201431339349202373_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201323199349204172_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201243119349301164_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201213209349304296_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201711319349201591_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201223539349301007_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201221369349306337_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201301349349204220_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201711939349300426_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201531249349201203_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201742409349300119_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201741569349300214_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201612049349200006_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201302199349300930_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201322939349200402_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201232859349301163_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201730099349300803_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201743189349313094_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201632399349300403_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201433179349306953_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201711359349311986_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201442819349301309_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201413189349200406_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201410459349302341_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201621379349307067_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201630799349300208_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201403219349305615_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201613409349200251_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201232159349300708_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201613199349311176_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201430989349300223_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201512189349301081_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201222239349301652_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201413189349306576_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201211309349200631_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201611339349305676_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201620909349300612_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201641099349300009_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201442329349200109_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201431189349200933_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201733199349308088_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201212409349300316_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201712019349300536_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201111319349300536_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201423219349302552_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201130129349200628_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201501349349309215_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201242899349200449_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201523209349306897_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201720689349200222_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201721309349301707_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201433189349304798_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201711789349200516_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201543439349200114_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201341919349300034_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201520209349300617_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201743119349302334_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201120219349301002_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201443219349203189_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201221639349200212_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201710259349200436_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201821239349200447_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201700749349200645_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201732969349301358_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201230769349300038_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201323459349300512_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201733219349300148_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201203179349201150_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201841019349300849_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201820459349301507_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201302839349301160_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201100129349300770_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201200759349200420_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201243489349300624_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201300149349200100_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201203199349300940_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201203199349305460_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201203209349308360_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201541339349302464_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201333169349301883_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201630409349300318_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201112909349200231_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201402599349200610_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201630399349200238_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201413149349302661_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201510449349200311_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201741039349300739_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201632179349300913_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201730179349301408_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201423089349300937_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201103199349305790_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201401199349301100_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201801319349304105_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201710519349300701_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201501349349304185_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201503249349300225_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201531359349308523_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201310619349300811_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201133199349203013_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201201439349300300_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201402349349300205_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201422539349200712_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201212219349301406_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201412739349200016_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201140269349300904_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201612379349200406_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201720809349200712_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201432269349200888_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201343189349204104_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201301359349201430_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201211079349300231_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201323169349301452_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201441359349203469_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201301339349202655_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201511259349300546_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201621369349300742_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201511689349300321_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201702409349300800_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201610149349301501_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201243109349300509_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201422169349300707_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201702929349301500_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201502819349300425_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201403169349303215_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201502199349300330_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201500929349200600_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201401539349300420_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201611369349301251_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201600219349300210_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201423219349307592_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201142629349300509_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201443219349301964_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201623149349303052_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201402879349300735_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201240669349301124_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201212239349200626_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201421279349301807_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201711319349303296_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201541209349300409_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201501349349305875_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201500819349300250_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201713199349302506_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201232149349300778_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201141339349202624_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201733149349303798_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201640679349200434_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201341269349301089_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201601339349200625_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201712589349200961_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201330079349301473_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201820149349200402_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201612869349200131_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201311349349302796_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201113129349300806_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201631399349200023_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201601029349200430_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201541349349302504_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201532299349303678_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201421349349302872_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201130129349301218_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201213109349301381_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201810609349300711_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201322219349200957_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201340459349200339_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201103149349302020_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201142439349300509_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201102289349301135_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201201329349300840_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201201329349301250_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201620429349302317_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201412069349200321_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201521119349300017_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201723549349200822_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201330979349300353_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201442669349300134_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201601329349304605_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201211359349300806_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201741319349301824_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201622539349301037_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201232279349301928_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201412899349300706_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201321759349300132_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201510159349300121_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201630439349200643_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201202759349300115_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201600259349300755_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201530909349300798_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201710459349301236_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201402549349300500_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201223459349300512_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201413379349300711_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201702299349200820_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201312919349200401_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201201509349300105_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201221669349300307_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201130429349300113_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201312359349300006_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201242069349300404_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201520589349300517_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201542299349302259_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201543079349300009_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201500129349300405_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201502949349200630_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201341349349305444_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201343189349300134_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201731649349300828_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201820489349300607_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201202279349200140_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201521349349303252_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201431019349300933_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201601079349300205_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201743209349301804_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201411129349200706_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201421359349309132_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201433429349200603_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201641349349304484_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201732699349300858_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201101339349302395_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201531329349301413_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201610849349300601_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201401339349302525_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201133199349204058_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201321099349200212_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201113199349302401_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201603189349200220_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201720459349201472_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201701309349302030_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201711219349300036_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201542299349303604_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201242199349300004_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201531599349300348_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201800409349300525_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201431999349200013_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201222719349300127_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201603199349200540_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201701819349200005_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201321359349302212_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201521269349201292_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201501339349201050_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201623209349316812_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201242229349301374_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201413219349304336_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201542239349201079_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201732849349300628_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201720989349200307_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201121339349304212_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201021693492003210_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201103209349300320_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201713199349304226_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201743129349300444_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201611949349300841_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201113089349200401_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201403089349301405_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201730139349301268_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201742309349200124_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201432279349300708_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201433089349301308_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201713189349312521_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201642599349301094_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201113159349300516_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201703179349203175_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201713199349300221_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201500209349300840_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201721989349301907_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201232569349300328_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201430489349200403_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201740129349300304_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201242239349300714_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201632149349201058_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201631909349300013_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201540129349300869_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201743109349201099_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201241039349300544_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201211229349200621_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201102719349300230_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201521019349300312_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201732289349200503_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201402199349300435_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201413219349305071_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201422269349202167_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201330709349300433_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201403179349306415_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201700969349301305_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201221259349300432_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201403219349305245_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201511359349304671_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201232339349300118_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201413179349202716_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201113089349301706_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201341229349300519_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201640419349300929_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201840469349200639_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201602259349201525_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201502469349300800_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201632169349301303_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201543209349310889_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201242519349200124_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201313199349305376_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201432949349300428_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201410489349301751_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201220749349200432_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201210899349300921_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201530339349300078_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201333549349300808_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201231299349301418_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201243079349200619_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201710529349301011_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201421359349202827_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201532189349301803_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201501389349300005_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201302219349200010_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201503179349302340_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201341349349201309_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201800169349300425_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201341689349300174_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201025493492000010_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201532029349301203_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201220709349200317_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201403169349203100_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201430289349300818_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201200979349200420_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201511209349300626_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201503209349304025_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201222839349200072_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201212729349200306_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201641319349301474_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201233199349303448_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201203199349202300_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201301759349300220_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201331939349200203_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201410599349300226_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201230819349300303_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201441339349201369_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201623199349201922_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201501279349302025_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201321769349300547_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201212279349201636_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201701709349300235_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201701889349300420_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201501339349303695_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201731289349201578_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201830249349300003_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201521319349201582_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201810529349300201_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201330249349300213_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201423079349300117_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201101369349306610_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201633289349300603_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201312269349301666_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201213209349300536_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201540989349200019_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201221889349200307_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201410499349301591_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201401299349201230_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201713199349206881_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201541269349201299_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201512999349300006_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201542309349300329_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201610619349300211_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201221809349200722_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201612239349300136_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201543009349300939_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201631379349302693_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201223319349300322_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201231079349300623_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201222789349300112_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201211989349300401_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201131319349302833_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201332599349300648_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201413179349306011_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201501359349309295_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201143199349303204_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201132279349300603_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201413189349302151_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201521669349200302_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201433189349203563_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201531339349200543_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201140289349300204_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201413219349311456_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201810439349200511_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201820199349300812_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201111809349300901_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201532739349300548_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201342269349301579_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201241529349200229_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201341769349300714_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201103389349300300_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201631099349300968_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201701359349307915_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201623199349312602_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201340529349300209_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201631469349300638_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201401289349301135_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201840469349301624_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201430299349300208_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201620579349300117_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201511199349300216_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201642279349300509_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201603089349302005_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201523209349303112_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201123199349304667_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201132769349300768_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201820579349200302_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201313179349303756_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201831229349200508_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201233079349200413_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201731379349300503_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201713199349305341_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201321199349200442_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201403189349304960_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201513569349300301_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201143399349300004_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201631269349300018_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201631799349200628_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201612989349301121_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201120299349200602_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201501359349308170_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201721879349200227_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201403119349301655_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201411349349307611_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201710459349201326_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201723189349202422_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201501399349300905_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201401249349200800_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201633199349201663_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201722069349200407_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201800729349300930_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201732279349300038_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201100749349200525_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201640439349302519_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201330679349300068_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201602869349300445_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201613209349201371_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201302219349300725_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201743189349307124_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201121589349300672_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201721779349301207_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201620209349300012_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201303469349300840_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201531259349300238_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201342259349300839_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201622279349200127_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201811169349300946_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201121679349200602_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201103189349300315_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201701359349304855_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201522879349300147_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201800669349300710_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201330429349300233_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201530659349300718_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201841359349313459_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201711809349200016_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201413529349300726_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201210989349300601_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201532759349300733_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201533579349200113_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201623209349313132_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201531879349200403_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201331769349300108_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201820229349200662_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201341029349200419_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201601339349302885_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201411299349200336_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201440849349200609_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201602959349300700_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201520129349301487_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201200829349300820_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201142949349301009_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201320609349300012_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201113189349307431_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201641359349200509_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201412809349300651_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201633079349300443_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201413219349311546_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201432529349200023_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201231109349300103_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201611329349304421_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201432239349201313_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201341889349200004_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201122699349300327_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201130429349200903_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201533489349200313_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201731259349200508_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201502199349200515_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201322889349201002_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201411999349200026_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201332359349200108_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201601349349307835_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201520429349300027_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201121339349302332_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201632169349200533_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201731079349300713_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201342689349300124_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201122289349201307_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201411849349300611_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201513139349302376_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201620439349302547_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201620469349300227_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201733149349201493_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201311349349304406_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201303199349301805_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201413179349300141_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201730259349300913_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201623189349200612_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201202279349303330_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201321339349303577_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201623139349300617_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201631379349201448_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201412849349200201_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201202269349200880_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201711319349303576_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201603209349200510_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201640509349301009_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201540379349300619_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201341779349300244_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201241309349200834_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201410289349300211_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201230679349300728_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201422319349200102_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201401539349200720_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201222289349302522_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201333409349200303_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201701329349305855_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201721429349300112_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201411259349300116_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201201249349200005_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201643029349301094_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201112209349300501_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201213209349309171_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201402959349300235_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201600479349302200_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201421279349200442_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201622359349300622_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201321229349301122_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201740139349200839_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201631629349300008_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201520559349200507_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201201729349300225_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201601199349200710_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201641169349200304_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201532369349200128_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201342259349200999_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201222219349300212_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201511039349200416_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201620509349301102_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201123159349200412_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201701239349300430_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201222289349302857_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201312419349200621_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201432109349200523_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201643199349306409_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201203559349200610_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201731809349300748_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201501269349301750_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201210749349301031_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201732579349300203_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201820199349300717_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201543439349300239_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201701299349300305_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201242409349200509_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201422129349300732_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201411329349303016_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201401359349303935_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201620959349300132_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201600919349200500_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201420459349200022_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201110289349300406_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201731739349200008_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201430739349300118_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201611349349304591_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201113189349202486_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201230809349200318_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201443219349201019_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201533149349303068_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201640749349201004_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201541559349300224_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201521739349200002_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201531149349300118_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201303189349301750_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201800159349200115_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201623149349302787_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201320339349200002_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201310469349302116_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201421719349200102_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201142349349200104_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201101319349201205_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201630639349200723_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201611379349300891_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201100609349300600_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201401969349301115_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201543109349300329_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201540289349300709_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201722779349301572_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201532789349301253_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201612249349301636_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201513029349300631_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201610299349301041_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201512249349301371_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201112659349300611_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201143229349300419_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201121369349303837_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201313299349300211_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201700449349300200_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201402029349300310_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201421219349301232_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201612929349200616_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201131369349306733_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201502059349200520_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201631389349200773_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201221949349300812_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201732239349200328_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201622189349300802_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201723199349310442_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201612729349300801_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201640439349302239_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201412209349300206_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201441219349200134_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201713179349200536_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201441349349302819_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201143489349300619_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201431349349203508_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201701299349202155_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201113159349201701_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201601339349200045_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201442279349301219_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201400459349200010_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201721599349301202_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201621249349301082_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201332249349301133_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201210729349301556_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201123159349201757_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201530159349200418_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201711359349307146_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201620749349200712_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201532119349301163_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201621329349301942_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201740239349200679_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201521079349200102_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201501359349309750_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201532539349300703_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201101319349301615_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201402309349301100_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201522299349202322_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201141439349300319_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201620989349300312_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201702239349300840_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201311359349307611_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201202409349300035_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201411349349201686_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201733199349317643_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201703479349300110_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201213209349202376_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201701889349200305_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201523349349200202_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201713129349200626_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201613219349200806_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201603209349312240_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201310639349300011_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201622219349300422_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201113189349307601_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201600449349200205_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201440449349302289_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201723139349200917_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201701399349300025_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201721299349302632_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201741249349301704_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201702269349302885_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201733179349306733_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201322749349200412_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201701329349201525_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201241329349302954_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201543099349300229_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201601349349203385_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201101319349200510_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201721819349200107_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201222209349300137_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201511009349200011_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201520209349200107_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201440469349300404_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201623199349311852_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201703149349200820_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201723189349308662_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201113069349300906_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201113199349305726_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201600469349302380_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201530449349301183_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201712899349201151_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201503209349205315_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201121169349200622_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201632219349301808_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201410159349301036_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201610999349300306_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201102279349302365_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201322049349200502_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201721319349301662_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201222789349300422_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201711299349302601_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201740179349200414_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201531849349300103_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201800229349301000_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201541359349301779_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201500489349300245_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201612609349200401_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201601819349300420_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201602799349300245_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201630299349301313_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201521289349200827_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201502579349300220_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201741529349300504_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201133189349202758_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201423169349300892_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201211369349305811_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201240839349300704_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201341279349301014_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201631389349201058_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201743199349202889_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201631329349303013_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201643209349201549_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201500239349200010_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201212019349300111_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201341069349300109_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201511069349200206_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201630609349300958_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201601349349306500_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201342109349200204_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201721609349301007_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201732379349300543_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201641319349301499_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201733139349302358_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201733189349204148_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201222899349300827_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201210819349300216_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201511359349205056_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201731289349300503_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201600409349301505_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201333189349200733_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201601339349306235_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201233199349201758_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201143059349301409_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201340079349301014_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201231509349300738_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201730889349301113_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201300849349200610_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201202699349200410_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201511259349301301_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201642219349200814_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201713199349202656_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201621899349300207_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201741359349301319_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201213209349308581_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201102079349300815_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201440779349200309_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201523499349300757_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201140499349300109_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201502269349300535_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201700379349200540_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201541349349202689_public.xml", 
"https://s3.amazonaws.com/irs-form-990/201213209349200301_public.xml"
), OrganizationName = c("SONOMA VALLEY VISITORS BUREAU", "PTA DRUM POINT ROAD SCHOOL 021219", 
"BLACKSTONE VALLEY YOUTH FOOTBALL AND CHEERLEADING", "TOYS FOR FREEPORT TOTS INC", 
"NCASI FOUNDATION", "UFCW LOCAL UNION 1099 SOUTHERN DIV", "JUNIOR PLAYERS GUILD", 
"ST MARYS HOSPITAL STREATOR HOSPITAL SISTERS OF THE 3RD ORDER ST FRANCIS", 
"HATTIE LARLHAM COMMUNITY SERVICES", "WOMENS GUILD OF THE CHARLEVOIX AREA HOSPITAL", 
"HOPE LOANPORT INC", "LEGAL AID FOUNDATON OF SANTA BARBARA COUNTY", 
"SOUTH DAKOTA MUSIC EDUCATORS ASSN", "PRINCETON AREA COMMUNITY FOUNDATION INC", 
"GLOBAL EVANGELICAL TEAM INC", "ALI DUNN PACKER MEMORIAL FUND INC", 
"DETROIT REGIONAL CHAMBER FOUNDATION INC", "WESTSIDE VOLUNTEER FIRE DEPARTMENT", 
"VACAVILLE CHAMBER OF COMMERCE", "CALIFORNIA FEDERATION OF WOMENS CLUBS SHERMAN OAKS WOMANS CLUB", 
"OPPORTUNITIES UNLIMITED INC", "BROOKVILLE SOCCER ASSOCIATION INC", 
"CANCER COMMUNITY CHARITIES", "MIDDLETON MILL DITCH COMPANY", 
"GLOBE CRAFTS INC", "PTA SEATTLE NORTH APP 6 15 234", "WESTPORT ASSOCIATES INC", 
"IOWA ASSISTED LIVING ASSOCIATION", "MOSDOS BETH YOSEF ZVI", 
"SHEPHERDS CALL INC", "GERMANIA OF POUGHKEEPSIE INC", "PENFIELD FIRE COMPANY INC", 
"STEM COMPASS INC", "INSULATION CONTRACTORS ASSOCIATION OF NEW ENGLAND", 
"332 E22 HOUSING DEVELOPMENT FUND CORPORATION CO MET COUNCIL", 
"PALMETTO PROJECT INC", "IAR PLAZA INC", "FREMONT STREET NURSERY", 
"SAFARI CLUB INTERNATIONAL SOUTH FLORIDA CHAPTER", "FREEDOM BAPTIST INC", 
"STAR VOLUNTEERS", "LAMAR INDEPENDENT FOUNDATION FOR EDUCATION", 
"ACTORS EQUITY HOLDING CORPORATION CO JOE DE MICHELE", "SUSSEX RETIREMENT ASSOCIATES INC", 
"SOUTHWEST OFFICIALS ASSOCIATION INC", "OPTIMIST CLUB FOUNDATION OF POINT LOMA", 
"WESTSIDE FOOD BANK", "SHAWME HEIGHTS II INC", "INTERNATIONAL CHRISTIAN RADIO ASSOC INC", 
"CHANDLER YOUTH BASEBALL INC", "ANCHOR POINT SENIOR CITIZENS INC", 
"NEW LIGHT BAPTIST SCHOOL OF EXCELLENCE", "NATIONAL MOTORCYCLE PATROL INC", 
"TENNIS INDUSTRY ASSOCIATION", "GIVEDIRECTLY INC", "LONE PINE WATER SUPPLY CORPORATION", 
"EUPSYCHIA INSTITUTE INC", "HOUSTON YOUNG ARTIST CONCERT ATTN MIWA SAKASHITA", 
"GREATER WYOMING VALLEY CHAMBER", "SERVING ASIA", "MEMORIAL BUILDING CORP", 
"AFGE LOCAL 1399", "INDEPENDENT INDUSTRIES INC", "EQUITY HEADQUARTERS INC", 
"WILLIAMS COUNTY COMMUNITY CONCERTS", "BEACON OF HOPE CHRISTIAN MINI", 
"HUMAN SERVICE RENOVATIONS INC", "HOLLIDAYSBURG COMMUNITY PARTNERSHIP", 
"TEXANS FOR THE ARTS", "AMERICAN LEADERSHIP FORUM HOUSTON GULF COAST CHAPTER", 
"KAPNICK FUND INC", "NEBRASKA FOUNDATION FOR THE HUMANITIES", 
"CAST EDUCATION INC DBA CAROLINA ACTOR STUDIO THEATRE", "MORRIS CREEK WATERSHED ASSOCIATION", 
"SPIRIT OF CAMARILLO INC", "TUCSON HEBREW ACADEMY", "MARINE INDUSTRIES ASSOCIATION OF SOUTH FLORIDA INC", 
"ANCIENT AND ACCEPTED SCOTTISH RITE VALLEY OF CHARLESTON", "ASSOCIATION FOR ACADEMIC PSYCHIATRY LTD CO LISA HEDRICK", 
"TAMA TOLEDO COUNTRY CLUB", "HOSPICE OF SOUTHERN MAINE", "MASONIC FOUNDATION OF BISMARCK MAND", 
"SETON MEDICAL GROUP INC", "LAMBDA CHI ALPHA FRATERNITY INC", 
"HALOM HOUSE INC", "SHORT NORTH FOUNDATION", "ADCARE EDUCATIONAL INSTITUTE OF MAINE INC", 
"EMPLOYEE CONTRIBUTION VEBA TRUST REED COLLEGE", "MAKE A JOYFUL NOISE INC", 
"INTERNATONAL BROTHERHOOD OF ELEC- TRICAL WORKERS 150 VACATION FUND", 
"BROOKFIELD SWIMMING CLUB INC", "INTERNATIONAL ASSOCIATION OF ICE CREAM DISTRIBUTORS AND VENDORS", 
"AMERICAN LEGION POST 122", "NATIONAL CHAPTER OF GARY DISTRICT HIGH ALUMNI ASSOCIATION", 
"KING MANOR ASSOCIATION OF LONG ISLAND INC", "OTSEGO POST NO 3030 VETERANS OF FOREIGN WARS OF THE USA", 
"HEMET POLICE ACTIVITIES LEAGUE", "TRANSITION PROJECTS INC", 
"DEKALB HUMANE SOCIETY INC", "JAPAN-AMERICA SOCIETY OF PENNSYLVANIA INC", 
"AMERICAS HEROES FIRST FOUNDATION", "MARION COUNTY HISTORICAL SOCIETY", 
"COMMUNITY OUTREACH SERVICES INC", "TRAINING LEADERS THROUGH ATHLETICS INC", 
"WAYNE FOUNDATION INC", "FATHERS FIELD MINISTRIES", "WOODLAWN CEMETERY ASSOCIATION", 
"GOODWILL INDUSTRIES OF CONEMAUGH VALLEY INC", "LITTLE GARDEN CLUB OF RYE INC", 
"A NEW WAY MINISTRIES INC", "WOOD DALE BASEBALL ASSOCIATION", 
"BLACK ROCK WATER SUPPLY CORPORATION", "AMERICAN FEDERATION OF STATE COUNTY AND MUNCIPAL EMPLOYEES AFL-CIO L566", 
"HALL HAGLER CHAPTER 15 DISABLED AMERICAN VETERANS", "ERIC BERRY FOUNDATION INC", 
"NORTHEAST FLORIDA BUILDERS ASSOCIATION BUILDERS CARE INC", "ST CHARLES COUNTY USBC ASSOCIATION", 
"SAVANNAH POSTAL CREDIT UNION", "CHAMPION FISHERMANS CLUB INC", 
"CEA FOUNDATION", "BOYS AND GIRLS CLUB OF SILOAM SPRINGS", "SMITH COUNTY FAIR INC", 
"PALOMINO HORSE BREEDERS OF AMERICA YOUTH SCHOLARSHIP AND EDUCATIONAL FUND INC", 
"HAMPTON ROADS NAVAL HISTORICAL FOUNDATION", "TEXAS ASSOCIATION FOR BILINGUAL EDUCATION", 
"INTERNATIONAL ASSOCIATION OF LIONS CLUBS", "EDINA FIREFIGHTERS RELIEF ASSOCIATION", 
"UNITED FAMILY INTERNATIONAL INC", "TALKS YOUTH DEVELOPMENT INC NFP", 
"PTA CARRBORO HIGH SCHOOL PTSA NORTH CAROLINA CONGRESS", "AFRICAN CHRISTIAN FELLOWSHIP MISSIO", 
"SARA BEEGLE CHILD CARE CENTER", "CATALYST HOUSING INC", "NATURE INSTITUTE INC", 
"ALABAMA CABLE TELECOMMUNICATIONS ASSOCIATION", "TENNESSEE EARTH SCIENCE TEACHERS", 
"MENDOTA LUTHERAN HOME", "BRUSH STREET RESIDENCE", "TUCSON MUSEUM OF ART AND HISTORIC BLOCK AND HISTORIC BLOCK INC", 
"SOUTH COUNTY HOSPITAL HEALTHCARE SYSTEM", "MCKENDREE UNIVERSITY", 
"UTILITIES CREDIT UNION", "BIRMINGHAM UNITED SOCCER ASSOCIATION INC", 
"CARDEN SCHOOL OF FRESNO", "METROPOLITAN NY CHAPTER-- APPRAISAL INSTITUTE", 
"MONTGOMERY BASKETBALL ASSOCIATION CO AJ SANTYE AND CO", "HACIENDA HEIGHTS AREA CHINESE SCHOOL INC", 
"NORTHWEST ARKANSAS CHILD CARE RESOURCE AND REFERRAL CENTER INC", 
"PAXTON HOUSE HISTORICAL SOCIETY", "BROOKSIE WAY", "RICHARD MURPHY MEMORIAL FOUNDATION", 
"FRIENDS OF THE EARTH ACTION INC", "CLINICA BETESDA CORP", "PARSONS CHAMBER OF COMMERCE", 
"DIAMOND1 INC", "HILLSBOROUGH LITTLE LEAGUE INC", "NORTHWEST TECHNICAL COLLEGE-BEMIDJI FOUNDATION", 
"KIWANIS CLUB OF SUMTER", "ST LOUIS RAMS FOUNDATION", "LIBRARY FOUNDATION OF NEEDHAM INC", 
"FIELD CLUB OF OMAHA", "WATERFALL ARTS", "COLORADO BAR OWNERS ASSOCIATION", 
"SOUTH CENTRE CORRIDORS RESOURCE CONSERVATION AND DEVELOPMENT INC", 
"MIDDLETOWN SYMPHONY INC", "ALMADEN GOLF AND COUNTRY CLUB", "HUMBOLDT LIONS CHARITIES", 
"NIAGARA AMATEUR HOCKEY LEAGUE INC DBA NIAGARA JR PURPLE EAGLES", 
"LEARN A LOT CHRISTIAN MINISTRY INC", "STUDIO 23", "BENNIES BARN INC", 
"GLOBAL ACTION", "MONTEZUMA LAND CONSERVANCY", "MISSOURI ASSOCIATION OF COUNTY DEVELOPMENTAL DISABILITIES SERVICES INC", 
"COMMONWORKS", "CHARLESTON PIPE BAND", "CENTER FOR HUMAN DEVELOPMENT", 
"MINOA FREE LIBRARY", "INTERNATIONAL SOCIETY OF PSYCHIATRIC GENETICS", 
"SHMIRA VOLUNTEER PATROL CORPDBA WILLIAMSBURG SAFETY PATROL", 
"FAMILIES AND COMMUNITIES TOGETHER INC", "ALLEGANY CATTARAUGUS LEGAL SERVICES INC", 
"AMERICAN LEGION POST 276 YORK", "GREATER SPRINGFIELD CHAMBER OF COMMERCE", 
"GREATER SUSQUEHANNA VALLEY CHAMBER OF COMMERCE", "SWAN VIEW THERAPEUTIC RIDING CENTER", 
"MANUFACTURERS ALLIANCE FOUNDATION", "SUGAR RAY LEONARD FOUNDATION", 
"BRONZE BLUES AND BREWS", "FAMILIES 4 CHANGE", "TEXAS ADAPTIVE AQUATICS INC", 
"YELLOWSTONE HISTORIC CENTER INC", "ROTARY INTERNATIONAL DBA RICHMOND ROTARY CLUB", 
"NATL CAUCUS AND CTR ON BLACK AGED HOUSING SVCS OF MS III - UNITA BLACKWELL EST", 
"SOUTH FRANKLIN CIRCLE", "PACE - THC INC", "LATINO CAUCUS FOUNDATION", 
"CITY OF NATCHITOCHES FESTIVALS INC", "COMMUNITY ACTION OF GREENE COUNTY", 
"FUND OF HOPE INC", "GROWING IN FAITH TOGETHER LEARNING CENTER", 
"ROTARY INTERNATIONAL WARRENTON", "MALIBU FOUNDATION FOR YOUTH AND FAMILIES", 
"AMERICAN ACADEMY OF PEDIATRICS MAINE CHAPTER INC", "SOLARIS FOUNDATION INC", 
"MAX AND FRIENDS INC", "CHAMPIONS OF HONOR INC", "POLISH NATIONAL CREDIT UNION", 
"ST CLAIRE REGIONAL MEDICAL CENTER", "LITTLE GIRAFFE FOUNDATION LTD", 
"MENORAH PARK", "HIDE AND SEEK FOUNDATION", "KLINE CW-FSUB 1170000377", 
"BROADWAY BOUND CHILDRENS THEATRE", "FAIRFIELD SUISUN BOBBY SOX", 
"INDIANA UNIVERSITY CREDIT UNION", "METRO PARK WAREHOUSES INC EMPLOYEE BENEFIT PLAN", 
"WASHINGTON FARM BUREAU LEGAL FOUNDATION", "MICHIGAN HUNTING DOG FEDERATION", 
"BAPTIST HISTORY AND HERITAGE SOCIETY", "SPRINKLER FITTERS AND APPR LOCAL UNION 550", 
"A SAFE HAVEN FOUNDATION", "NATIONAL FOUNDATION FOR AFFORDABLE HOUSING SOLUTIONS INC", 
"LEAVES INC", "SIGMA ALPHA MU FRATERNITY", "CAMP CORRAL", "VISITING NURSE ASSOCIATION OF CENTRAL JERSEY COMMUNITY HEALTH CENTER INC", 
"FAITH AND JUSTICE COALITION INC CO JAIL PROGRAMS CAROLYN FENDER", 
"NATIONAL GUARD ASSOCIATION OF MISSI", "GOODWILL OF SOUTHERN INDIANA INC", 
"VETERANS OF FOREIGN WARS OF US DEPT OF MASS DR GEORGE 996 VFW-MASS", 
"HAPPY DAYS HUMANE SOCIETY", "EMBASSY OF THE BLESSED KINGDOM OF GOD FOR ALL NATI", 
"CIVIL SERVICE EMPLOYEES ASSOCIATION LOCAL 425", "LEGACY PREGNANCY RESOURCE CENTER INC", 
"JOHNSON-NEMAHA HABITAT FOR HUMANITY", "WINERY LOOP ASSOCIATION", 
"CASAGAL OF HANCOCK COUNTY", "SUBMISSION MINISTRIES AND FELLOWSHI", 
"NEXT GENERATION MINISTRIES", "ST LOUIS WHEEL CHAIR ATHLETIC ASSOCIATION", 
"BATTERED WOMENS LEGAL ADVOCACY PROJECT", "HOPEBUILDERS HOME REPAIR INC", 
"ROTARY CLUB OF WYTHEVILLE VIRGINIA", "PTON LAKE NORMAN CHARTER SCHOOL", 
"SIOUX EMPIRE FAIR ASSOCIATION INC", "LITTLE BEES DAYCARE CENTER", 
"HAWAII PREPARATORY ACADEMY", "EAST BRUNSWICK SOCCER CLUB", "INDIAN CULTURAL SOCIETY OF JACKSONV", 
"WATER FOR ISHMAEL", "AURORA LIVING RESOURCES INC", "ISO NEW ENGLAND INC", 
"WELF BENTR FOR COLLBARGEM METLIFE PAR", "QUINCY QUARRY AND GRANITE WORKERS MUSEUM INC", 
"GRAND RAPIDS AREA CHAMBER OF COMMERCE", "DELAWARE VALLEY REEF CLUB", 
"NATL POSTAL MAIL HANDLERS UNION LOCAL 315", "YOUTH WITH A MISSION-NASHVILLE INC", 
"TOMCHEI TORAH BERETZ YISRAEL INC TOMCHEI TORAH BERET", "NEW HORIZONS SHELTER AND OUTREACH CENTERS INC", 
"ITALIAN GARDENS INC", "LARAMIE MONTESSORI SCHOOL INC", "WYA FOUNDATION INC DBA WORLD YOUTH ALLIANCE", 
"PLATO FOUNDATION", "SOUTHMINSTER INC", "NATIONAL CONFERENCE OF STANDARDS LABORATORIES INC", 
"THAMES VALLEY MUSIC SCHOOL INC", "CAMP SUNSHINE DAY INC DBA REEDS ACADEMY", 
"BALTIMORE COLLEGE OF DENTAL SURGERY INC NATIONAL ALUMNI FUND", 
"AMERICAN FEDERATION OF STATE COUNTY AND MUNICIPAL EMPLOYEES", 
"IDIIL LEARNING FOUNDATION INC", "WEST END SCHOOL INC", "BETHANY WEST RECREATIONAL ASSOCIATION INC", 
"WHITE COUNTY CHILD DEVELOPMENT INC", "MASONIC CENTER FOR HEALTH AND REHAB INC", 
"EPHRATAH ROD GUN CLUB", "FORT WORTH MODERN ART MUSEUM ASSOCIATION", 
"AMERICAN MEDICAL ASSOCIATION ALLIANCE INC", "NEUROSPRING INC FKA NORTHERN NEUROSCIENCES INC", 
"SIOUX RAPIDS FIRE FIGHTERS ASSOCIATION INCORPORATED", "MSU BENEFITS GROUP INC", 
"PINE VIEW ASSOCIATION INC", "THE ROCKY MTN CHAPTER OF THE AMYOTROPHIC", 
"CHURCH OF THE CROSS OF LEE COUNTY LAND STEWARDSHIP MINISTRIES INC", 
"GREENS FARMS ACADEMY INC", "PHI GAMMA DELTA FRATERNITY UNIVERSITY OF NORTH CAROLINA-CHAPEL HILL", 
"LABORERS LOCAL 159 477 AND 703 CENTRAL BUILD OF AGC AND AGC OF IL SUBSTANCE ABUSE", 
"CONNECTICUT FEDERATION OF SCHOOL ADMINISTRATORS", "LANGLEY PARK BOYS AND GIRLS CLUB INC", 
"ROCK CREEK RIDING CLUB INC", "ISLAND OUTREACH INC", "KANSAS STATE COUNCIL OF FIRE FIGHT", 
"PARTNERSHIP FOR PREVENTION", "ILLINOIS SELECTED MORTICIANS ASSOCIATION", 
"WEST COAST HONOR CAMP", "FAMILY FOUNDATION OF SOUTHWEST LOUISIANA", 
"HARBORCREEK YOUTH SERVICES INC", "PURE HANDS INC", "KANSAS PEDIATRIC FOUNDATION", 
"RED LODGE FIRE - EMS FOUNDATION", "LADIES OF THE ORIENTAL SHRINE OF NORTH AMERICA", 
"GEORGE W GIBBS JR ELEMENTARY PTSA", "TICONDEROGA HISTORICAL SOCIETY", 
"RETURNING HOME INC", "AHEPA 29 INC", "CHINESE AMERICAN FORUM INC", 
"CASA HELOTES SENIOR CITIZENS", "MANKATO AREA HOCKEY ASSOCIATION", 
"PTAC CAPRI", "DISTRICT ONE COMMUNITY EDUCATION CENTER INC", 
"NATIONAL CORN GROWERS ASSOCIATION", "HOSANNA BROADCASTING FOUNDATI", 
"TURNAROUND MINISTRIES INC", "MILL MOUNTAIN LODGE INC", "LOUIE STREET APARTMENTS INC", 
"FAIRHAVEN LITTLE BASEBALL LEAGUE", "CAPITAL DISTRICT CHILD CARE COORDINATING COUNCIL INC", 
"HOME ALLIANCE INC", "OREGON BANKERS ASSOCIATION", "STAR OF TEXAS CREDIT UNION", 
"DIEU EST ESPOIR INC DBA LIVING HOPE HAITI CHRISTIAN MISSION", 
"FAMILY GUIDANCE CENTER OF ALABAMA INC", "DALLAS GEOPHYSICAL SOCIETY", 
"CASCADE COUNTY COALITION HEALTHY MOTHERS HEALTHY BABIES", "OKTOBERFEST COMMITTEE", 
"COLORADO SPRINGS SPORTS CORPORATION", "AVILA FOUNDATION INC", 
"UNITED FOOD AND COMMERCIAL WORKERS INTERNATIONAL UNION", "FAMILY GUIDANCE CENTER INC", 
"HEDWIG HOUSE", "SENIOR OPTIONS INC", "DISCIPLE HOMES CORPORATION DBA BETHANY TOWERS", 
"UNIVERSITY SCHOOL OF NASHVILLE", "PLAINS BETTER HOMETOWN PROGRAM", 
"ERIE COUNTY ASSOCIATION OF SCHOOL BOARDS", "GEORGIA HISTORICAL SOCIETY", 
"FRATERNAL ORDER OF EAGLES AERIE 4428", "BESSIE SMITH CULTURAL CENTER", 
"INTERNATIONAL TOWING AND RECOVERY HAL", "TWIN PEAKS ELEMENTARY SCHOOL PARENT", 
"GRAND PERFORMANCES", "BENFICA USA INC", "ROTARY CLUB OF ROCKFORD MICHIGAN", 
"CHRISTIAN WOMENS CENTER INC", "ASIAN WOMEN FOR HEALTH INC", 
"BEAVERTON LIBRARY FOUNDATION", "SO2SPEAKORG", "WOMEN EMPOWERED INC - AFGHANISTAN", 
"SOUTHEAST TEXAS ASSOCIATION OF PUBLIC PURCHASING", "MOUNT CARMEL IN THE DESERT INC", 
"MOOSE PASS SPORTSMENS CLUB", "ROOSEVELT JAZZ BOOSTER CLUB ATTN TREASURER", 
"CENTER FOR CHILDREN AND FAMILIES INC", "BLUEGRASS DIVE CLUB INC", 
"KNIGHTS OF COLUMBUS OF WYOMING", "MONTANA EDUCATORS CREDIT UNION", 
"TRI COUNTY DRIFTHOPPERS INC", "CHILDRENS ADVOCACY CENTER OF BELL C", 
"MAIN-FERRY HOUSING DEVELOPMENT FUND COMPANY INC", "MILK AND HONEY MINISTRIES INC", 
"LONSDALE FIREMANS RELIEF ASSOCIATION", "EAGLES UNLIMITED INC", 
"BRADFORD HEIGHTS HEALTH AND REHAB CENTER INC", "CHURCH OF JESUS CHRIST THE LORD", 
"SAM-CMMW CARE INTERNATIONAL PREVIOUSLY SAM-CMMW CARE INC", "VISION ORLANDO INC", 
"PROFESSIONAL RACERS OWNERS ORGANIZATION", "BIBLE MINISTRY FOUNDATION OF USA INC", 
"REED UNION SCHOOL DISTRICT PTA", "ST TERESA OF AVILA VILLA INC", 
"NAPA VALLEY VINE TRAIL COALITION", "CHILDRENS SPECIALIZED HOSPITAL FOUNDATION INC", 
"PRAIRIE WOODS ENVIRONMENTAL LEARNING CENTER", "ARC OF MCLENNAN COUNTY", 
"ALIVE INC", "VILLA SERRA", "KENYA VILLAGE CONNECTION", "BRONX LIGHTHOUSE CHARTER SCHOOL", 
"ST MARY CATHOLIC HOUSING CORPORATION", "CONOVAL INC", "WIREGRASS SPAYNEUTER ALLIANCE INC", 
"HALEY HOUSE", "SCOLIOSIS RESEARCH SOCIETY", "PA DENTAL HYGIENISTS ASSOCIATION INC - GROUP RETURN FOR AFFILIATES", 
"BROCKTON INTERFAITH COMMUNITY INC", "ABATE OF ALASKA INC", "UPLAND NATIONAL LITTLE LEAGUE", 
"TACOMA LONGSHOREMEN CREDIT UNION", "MICHIGAN FAMILY MEDICAL RELIEF", 
"NEW UNION VOLUNTEER FIRE DEPT", "WISCONSIN ALPHA OF PI BETA PHI HOUSE CORPORATION", 
"SYRACUSE SOUNDS OF MUSIC ASSOCIATION INC", "GFWC OF TENNESSEE", 
"HORIZON HOUSE INC", "MERCY FRANCISCAN SENIOR HEALTH AND HOUSING SERVICES INC", 
"GLOBAL RELIEF INTERNATIONAL", "TEXAS CHORAL CONSORT", "IDAHO SUGARBEET GROWERS ASSOCIATION INC", 
"CHADRON CHAMBER OF COMMERCE", "REGIONAL CENTER OF ORANGE COUNTY INC", 
"CHEROKEE TOWN AND COUNTRY CLUB INC", "STONEY INC", "GRACELAND UNIVERSITY", 
"ASHEVILLE DOWNTOWN ASSOCIATION FOUNDATION INC", "EAST QUOGUE FIRE DEPARTMENT INC", 
"NCR OF CHILLICOTHE OHIO", "AMERICAN OTOLOGICAL SOCIETY RESEARCH FOUNDATION", 
"BLUE LAKE PRESCHOOL INC", "BETHANY LUTHERAN FOUNDATION INC", 
"CENTRALIA CHAMBER OF COMMERCE", "UNITED WAY OF COLES COUNTY INC", 
"REACH 4A STAR RIDING ACADEMY INC", "WARREN PLAZA INC", "1000 GENERATIONS INC", 
"BLUE AND GRAY R AND P CLUB", "DELAVAN AND DELAVAN LAKE AREA", 
"CHARITY BAPTIST CHURCH OF CHRIST", "MANAV SEVA MANDIR", "ARTISTS STRIVING TO END POVERTY INC", 
"SIGMA DELTA HOLDING CORPORATION", "OHIO MUNICIPAL LEAGUE", "DREW ENTERPRISES INC", 
"ETC HOUSING CORPORATION EVERGREEN TOWNHOUSE COMMUNITY", "CONNECTICUT ASSOCIATION OF FOSTER AND ADOPTIVE PARENTS INC", 
"M AND M GYMNASTICS BOOSTER CLUB LTD", "CHEYENNE DEPOT MUSEUM INC", 
"JHA PROPERTIES INC", "FRATERNAL ORDER OF EAGLES AER 2371 AERIE", 
"NONPROFIT ASSOCIATION OF OREGON", "VOLUNTEER AND EXEMPT FIREMENS BENEVOLENT", 
"INDEPENDENCE CARE SYSTEM INC", "VILLA WEST II INC", "ADULT COMMUNITY EDUCATION FORMERLY LITERACY VOLUNTEERS OF LOU", 
"UNITED IRISH COUNTIES ASSOCIATION OF NEW YORK INC", "GRACELAND CEMETERY", 
"AMAZING THINGS ART CENTER INC", "VIETNAM VETERANS OF SAN DIEGO DBA VETERANS VILLAGE OF SAN DIEGO", 
"WOODLANDS GIRLS LACROSSE ASSOCIATION", "IOWA WILDLIFE CENTER", 
"TIMOTHY AYCOCK MELANOMA RESEARCH FOUNDATION", "PREGNANCY CENTER OF OWENSBORO INC", 
"ST THEODORES APARTMENTS HOUSING DEVELOPMENT FUND COMPANY INC", 
"CONCORD REGIONAL VISITING NURSE ASSOCIATION INC", "FRIENDS AND SUPPORTERS OF TROOP 30 INCORPORATED", 
"ANGOLA KIDS LEAGUE INC", "OMNI BEHAVIORAL HEALTH", "INDEPENDENT PRODUCTION FUND INC", 
"UNITED WAY OF GREATER HAZLETON INC", "BIBLICAL ALTERNATIVES FELLOWSHIP INC", 
"ASSOCIATION FOR RESEARCH IN OTOLARYNGOLOGY", "GALLOPING GRACE YOUTH RANCH", 
"ACTA MATERIALIA INC", "ALABAMA ASSOCIATION OF RESOURCE CONSERVATION AND DEVELOPMENT COUNCILS", 
"PELHAM NEW HAMPSHIRE COUNCIL ON AGING CORP", "MADISON COMMUNITY FOUNDATION INC", 
"ROTARY INTERNATIONAL- CAPE CORAL GOLDCOAST", "KATALLASSO INC", 
"GROUP FOR THE EAST END INC", "HILLTOP COMMUNITY CHILDRENS CENTER", 
"BALTIMORE WASHINGTON HEALTHCARE SERVICES INC", "MILESTONE CENTERS INC", 
"OCL PROPERTIES V INC", "ACTION MINISTRIES INC", "OPERATION OPEN ARMS INC", 
"LUTHERAN CAMP ASSOCIATION", "AAHOA CHARITABLE FUND INC", "UVALDE HOSPICE FOUNDATION", 
"NORTHEAST ROTARY FOUNDATION", "WESTCHESTER COUNTY CORRECTIONS SUPERIOR OFFICERS ASSOC BENEFIT FUND", 
"AMERICAN LEGION POST 573 T SGT JACK D LEVY", "HEALTHCARE GROUP PURCHASING INDUSTRY INITIATIVE", 
"DISARM EDUCATION FUND INC", "DRYWALL FINISHERS LOCAL 1955", 
"PORTAGE LIBRARY ASSOCIATION", "FIRST UNITED METHODIST CHURCH OF WACO FOUNDATION", 
"UNIOTO ATHLETIC BOOSTERS INC", "ABC WORKFORCE DEVELOPMENT", 
"CARMEL SYMPHONY ORCHESTRA INC", "ROSEBURG SENIOR CENTER", "MACON COUNTRY CLUB", 
"PROACTIVE HEALTH LABS", "FOUNDATION OF THE AMERICAN COLLEGE OF HEALTHCARE EXECUTIVES", 
"CONCEPT WEB 40 INC", "EAGLES HEART CHARITY CHG TO EAGLES HEART GROUP MINISTRIES", 
"ALEPH SOCIETY INC", "PARASPORTS SPOKANE", "PINEY WOODS COUNTRY LIFE SCHOOL", 
"TIMBERHILL RACQUET CLUB INC", "CARRFOUR SUPPORTIVE HOUSING INC", 
"CLEAN FUELS FOUNDATION INC", "GLENDORA HIGH SCHOOL CHORAL BOOSTERS", 
"AGENDA PROJECT INC", "OREGON SENIOR CITIZENS CENTER INC", "UNITED CHURCH HOMES OF READING INC", 
"EAA WARBIRDS OF AMERICA INC", "WEST ORANGE JUNIOR SERVICE LEAGUE", 
"NEW HAVEN COUNTY BAR ASSOCIATION INC", "LIFE SUPPORT COUNSELING AND RESEARCH DBA THERAPEUTIC FAMILY LIFE", 
"STRAY RESCUE OF ST LOUIS", "PROVO NONPROFIT HOUSING DEVELOPMENT CORPORATION", 
"NORTH SHORE-LONG ISLAND JEWISH HEALTH SYSTEM FOUNDATION", "WACO DOWNTOWN FARMERS MARKET", 
"L-O DEL MAR HOLDING INC", "JACKSON COUNTY INTERFAITH VOLUNTEER CAREGIVERS INC", 
"MISSOURI ATHLETIC CLUB PRESERVATION FOUNDATION", "EARTH CHILD-EARTH MOTHER INC", 
"GYMSTARS BOOSTER CLUB", "DRAGOON TRAIL CHAPTER INC OF THE IZAAK WALTON LEAGUE", 
"CORNERSTONE ASSISTED LIVING INC", "DOCOMOMO US", "P2 COLLABORATIVE OF WNY INC", 
"MUSICAL FUND SOCIETY OF PHILADELPHIA EDWARD GARRETT MCCOLLIN MEMORIAL FUND", 
"UNITED IRISH COUNTIES ASSOCIATION OF NEW YORK INC", "ANDREWS CENTER", 
"JIREH LATIN AMERICAN MINISTRIES INC HARVILL", "AMERICAN FEDERATION OF LABOR AND CONGRESS", 
"MINISTERIO MONTE DE ALABANZA", "MOST WORSHIPFUL UNION GRAND LODGE FOUNDATION INC", 
"MARCO ISLAND HOSPITAL INC", "ELEVATE VENTURES INC", "VAN NUYS ROTARY CLUB FOUNDATION", 
"KEIZER SOCCER CLUB", "WEST BAY LOCAL DEVELOPMENT CORPORATION", 
"DELAWARE TRIAL LAWYERS ASSOCIATION", "BLOOD CENTER PROPERTIES INC", 
"NEW BOSTON PRIDE COMMITTEE INC", "WALL STREET TAX ASSOCIATION INC", 
"SCURRY COMMUNITY SERVICES INCORPORATED", "CALIFORNIA MENTAL HEALTH ADVOCATES FOR CHILDREN AND YOUTH CMHACY", 
"ST MICHAELS UNIVERSITY SCHOOL SOCIETY", "NH WOMANS CHRISTIAN TEMPERANCE UNION", 
"NATIONAL ASSOCIATION OF CHAPTER 13 TRUSTEES", "MUSKEGON BOWMEN INC", 
"KIDS OF THE FUTURE MINISTRY INC", "PEACE RIVER CENTER PROPERTIES INC", 
"HEART OF THE VALLEY INC", "SIOUXLAND COMMUNITY FOUNDATION", 
"PHI DELT ALUMNI ASSOCIATION", "GODMAN GUILD ASSOCIATION", "OBEDIENCE TRAINING CLUB OF PALM BEACH COUNTY", 
"SEVERN SCHOOL INC", "GILFORD YOUTH BASEBALL ASSOCIATION INC", 
"CHARLES RIVER CONSERVANCY INC", "RAINBOW DAYS INC", "AIDS COMMUNITY RESOURCES INC", 
"C I M ALUMNI SCHOLARSHIP FUND", "NORTH RANDALL COUNTY BASEBALL ASSOCIATION", 
"UNITED WAY OF SOUTHERN ILLINOIS INC", "COACHES OF EXCELLENCE INSTITUTE", 
"CHAMPAIGN COUNTY FARM BUREAU", "URBANA MUNICIPAL EMPLOYEES CREDIT UNION", 
"PENDLETON APARTMENTS INC", "COMMUNITY CREDIT UNION", "PTOM CHELMSFORD HIGH SCHOOL INC", 
"DAYTON VETERANS AFFAIRS RESEARCH AND", "AMERICAN LEGION POST 105 YOUTH BASEBALL INC", 
"MALONE CHAPTER NO 285 WOTM 285", "CHURCH PLANTING LEADERSHIP INC", 
"DURHAM COMMUNITY PENALTIES PROGRAM INC", "DIABETES ASSOC OF PIERCE COUNTY", 
"MHAC - STATE STREET INC", "MOTHER OF MERCY FOUNDATION", "DANCE THEATRE OF HARLEM INC", 
"RANGERS SCHOLARSHIP FUND INC", "OWENSVILLE HOUSING DEVELOPMENT FUND COMPANY INC", 
"CENTER FOR INDEPENDENT EMPLOYEES", "ODEON PRESERVATION ASSOCIATION", 
"SAFE HARBOR CRISIS NURSERY", "FANNIN COUNTY HISTORICAL MUSEUM SOCIETY INC", 
"LONGMONT HUMANE SOCIETY INC", "INDIANA SOCIETY FOR HEALTHCARE HUMAN RESOURCES", 
"FAMILY ENRICHMENT NETWORK", "CHICOPEE PORTUGUESE AMERICAN CLUB INC", 
"LA MAUM CENTER", "INTERNATIONAL SOCIETY FOR COMPUTERS AND THEIR APPLICATIONS", 
"CALIFORNIA ASSOCIATION OF FLOWER GROWERS AND SHIPPERS", "NORTHWEST ASSISTANCE MINISTRIES", 
"ORAL CANCER AWARENESS FOUNDATION", "PREGNANCY CENTER OF GRAND JUNCTION INC", 
"NORTH CENTRAL SIGHT SERVICES INC", "GREEN MOUNTAIN CONSERVANCY INCORPORATED", 
"HAMILTON SENIOR CENTER INC", "IRVINGTON FACULTY ASSOCIATION - DENTAL OPTICAL", 
"ALLEN COUNTY ANIMAL RESCUE FOUNDATION", "WISCONSIN POLICE LEADERSHIP FOUNDATION INC", 
"TOWN HALL OF CLEVELAND", "ELK GROVE TOWNSHIP COMMUNITY DAY CARE CENTER", 
"SUNNYBROOK CHILDRENS HOME INC", "SOUTH ATLANTIC AND GULF COAST DISTRIC", 
"ASPEN GLOBAL CHANGE INSTITUTE INC", "BUILDING PARTNERS FOR AFFORDABLE HOUSING", 
"VIRGINIA NO TILL ALLIANCE INC CO TREASURER", "SHAMROCK COMMUNITY EDUCATION FOUNDATION", 
"COMPASS CORPORATION FOR RECOVERY SERVICES", "TUNA VALLEY TRAIL ASSOCIATION", 
"HAWKINS HELPING HAND", "SOROPTIMIST INTERNATIONAL - ALHAMBRA SAN GABRIEL SAN MARINO", 
"ST PATRICKS HOUSING INC", "UNITED CEREBRAL PALSY OF MOBILE INC", 
"LAMAR STATE COLLEGE - ORANGE FOUNDATION", "LAURA CRANDALL BROWN OVARIAN CANCER FOUNDATION", 
"POLYPHONY FOUNDATION", "LIBRARY ASSOCIATION OF RINGWOOD", "CASTLEWOOD HISTORICAL SOCIETY", 
"GALLIA MEIGS COMMUNITY ACTION AGENCY INC", "COMMUNITY TRANSPORTATION ASSOCIATION OF IDAHO INC", 
"SAFARI CLUB INTERNATIONAL- MICHIGAN CHAPTER", "PRAXIS PROJECT INC", 
"ST JOHNS MERCY REGIONAL FOUNDATION", "PTOP SPRING FORGE INTERMEDIATE", 
"ST CHARLES HOSPITAL FOUNDATION", "SOLANO ECONOMIC DEVCO CORP", 
"VALLEY MENTAL HEALTH INCORPORATED", "LEAGUE OF HUMAN DIGNITY CENTRAL NEBRASKA HOUSING CORP", 
"OLDE ENGLISH CONSORTIUM", "ROCKERS FOR KNOCKERS INC", "CANON CITY PREGNANCY CENTER", 
"DEAN BROWN LEADERSHIP FOUNDATION", "ARC HOUSING DEVELOPMENT CORP OF LINCOLN", 
"HEARTLAND HEALTH CARE PLAN TRUST FUND", "THANKSGIVINGS HEROES LLC", 
"KLEIN OAK FOOTBALL BOOSTER CLUB INC", "BENEVOLENT AND PROTECTIVE ORDER OF ELKS OF THE UNITED STATES OF AMERICA", 
"WILSON SCHOOL", "ST ELIZABETH COMMUNITY WORKERS COMPENSATION TRUST", 
"FREE AND ACCEPTED MASONS OF OHIO 769 ", "PATHWAYS ACADEMY SCHOOL FOR DYSLEXIAINC", 
"ST LOUIS CHRISTIAN COLLEGE", "DIGITAL VIBEZ INC", "COALITION FOR RESPONSIBLE WASTE INCINERATION", 
"KEEP BREVARD BEAUTIFUL INC", "RCAP SOLUTIONS INC FORMERLY RURAL HOUSING IMPROVEMENT INC", 
"TREBOL SOCCER CLUB OF LAFAYETTE INC", "BUCKVILLE VOLUNTEER FIRE DEPARTMENT", 
"SELF REGIONAL HEALTHCARE AUXILIARY", "MISSION LEXINGTON INC", 
"PHI BETA KAPPA FOUNDATION", "Y-CENTER OF BATTLE CREEK", "HFA ENDOWMENT FUND", 
"UNITED STRUCTURES OF AMERICA INC HEALTH PLAN AND TRUST", "MG CHARITIES", 
"HUNT-HOLT KIWANIS CHARITIES", "SERVICE EMPLOYEES INTERNATIONAL UNION 300 SEIU", 
"FED-SPACE ASSOCIATION", "LEGACY FUND INC", "COURANTE DANCE FOUNDATION INC", 
"KAPPA DELTA SORORITY - BETA UPSILON CHAPTER", "COASTLANDS MINISTRIES", 
"CAUSEWAVE COMMUNITY PARTNERS INC", "GREATER HARBOR SPRINGS AREA PLANNING RESOURCE GROUP", 
"CASCADIA WILDLANDS", "ASSOCIATION FOR THE BLIND AND VISUALLY IMPAIRED", 
"SUSQUEHANNA WALDORF SCHOOL", "UPPER SCIOTO VALLEY AMBULANCE DISTRICT", 
"SCHOOL OF PROPHETS MINISTRY", "IMPACT FUND", "ROCKFORD RAPTORS BASEBALL INC", 
"WAPELLO COUNTY AGRICULTURAL FAIR ASSOCIATION", "BENTON COUNTY ELKS LODGE 2783", 
"CHRISTIANS IN VISUAL ARTS", "ORLEANS CHAMBER OF COMMERCEINC", 
"SANTA CLARITA VALLEY YOUTH ORCHESTRA CO COLLEGE OF THE CANYONS", 
"OHIO CITIZENS COMMITTEE FOR THE ARTS INC", "UNITED PROFESSIONAL PRO FORCE OF SAVANNAH RIVER", 
"NEPALESE ASSOCIATION OF CONNECTICUT", "PARIS LODGE NUMBER 1915 LOYAL ORDER OF MOOSE", 
"SOUTH VENICE CIVIC ASSOCIATION", "HEARTLAND CHRISTIAN COLLEGE", 
"COMMUNITY ACTION AGENCY OF", "EASTERN OHIO AREA HEALTH EDUCATION NETWORK INC", 
"SHALOM HOMES INC", "SOUTH PADRE ISLAND ECONOMIC DEVELOPMENT CORPORATION", 
"JT TOWNSEND FOUNDATION INC", "FRIENDS OF THE HOLYOKE COUNCIL ON AGING INC", 
"COMMUNITY DAY CENTER FOR CHILDREN", "HOPA MOUNTAIN FOUNDATION", 
"ILLINOIS FIRE CHIEFS ASSOCIATION EDUCATIONAL AND RESEARCH FOUNDATION", 
"INTERNATIONAL COLLEGE OF PROSTHODONTISTS I C P", "WATERFORD SPRINGS CORPORATION", 
"SELF-INSURERS SECURITY FUND", "MENTAL HEALTH AMERICA OF BEAUFORT-JASPER", 
"AFCEA EDUCATIONAL FOUNDATION", "FINANCIAL MENTORS OF AMERICA INC", 
"COOS FOREST PROTECTIVE ASSOCIATION", "MOLINE POLICE BENEVOLENT AND PROTECTIVE ASSOCIATION", 
"AMERICAN SOCIETY OF LANDSCAPE ARCHITECTS TEXAS CHAPTER", "WIN HOUSING DEVELOPMENT FUND COMPANY INC", 
"HOPE INTERNATIONAL UNIVERSITY", "HUMAN RESOURCES INC", "CENTER FOUNDATION", 
"TENNESSEE CHILDRENS HOME FOUNDATION INC", "PI BETA PHI FRATERNITY HOUSING CORPORATION", 
"ROTARY CLUB OF RICHMOND INC", "ORESTIMBA SCHOLARSHIP COMMUNITY ASSOCIATION - TOSCA", 
"PARK SLOPE CHILD CARE COLLECTIVE INC", "HALE MAHAOLU AKAHI INC", 
"LITTLEST TUMOR FOUNDATION INC", "CALIFORNIA PROBATION PAROLE AND CORRECTION ASSOCIATION", 
"FRIENDS OF BIG BAND JAZZ", "JOHNSIE AND AUBARY MONTGOMERY INST", 
"EDWARDSBURG PUBLIC SCHOOLS FOUNDATION INC", "INDIANA ROOFING CONTRACTORS ASSOC BILL BUBENZER", 
"BILLINGS PRESERVATION SOCIETY ENDOWMENT TRUST", "IDAHO ASSOCIATION OF CHIROPRACTIC PHYSICIANS", 
"CREATEACTIVITY INC", "WEST CALDWELL HEALTH COUNCIL INC", "SAINT JAMES SCHOOL", 
"GREEN MOUNTAIN COLLEGE", "BATON ROUGE COMMUNITY COLLEGE FOUNDATION", 
"NORTH ALLEY ASSOCIATION", "LABORERS INTERNATIONAL UNION OF N AMERICA LOCAL 368", 
"NINEVEH PUBLIC LIBRARY OF COLESVILLE TOWNSHIP", "ADVANCE DELAWARE OPPORTUNITIES OBJECTIVES INC", 
"OHIO VALLEY FLOORING GROUP MEDICAL BENEFIT TRUST", "MAGIC CITY SOCCER CLUB INC", 
"NEW MILFORD JUNIOR FOOTBALL LEAGUE", "CEDARBURG CHAMBER OF COMMERCE", 
"HAWAIIAN GARDENS FOOD BANK INC", "AGC SCHOLARSHIP", "BELL ACTION NETWORK", 
"LUBBOCK AREA FOUNDATION INC", "HOUSING OPPORTUNITIES OF NORTHERN DELAWARE", 
"NORTH PENN LACROSSE CLUB", "MAHBER KUDUS MICHAEL SEGHENEITY FOU", 
"ADOPTION CONSULTANTS", "ISLAND TREES PTSA", "FRIENDS OF THE LAWRENCE PUBLIC LIBRARY INC", 
"LANDLORDSBUSINESS ASSOCIATION INC", "YOAKUM COMMUNITY HOSPITAL", 
"GLOBAL IMPACT", "WESTSIDE CENTER FOR INDEPENDENT LIVING", "AMERICAN LEGION POST 67 990", 
"HART COUNTY FARM BUREAU INC", "PTOI NEW LENOX ", "LOWCOUNTRY DOWN SYNDROME SOCIETY", 
"SETA FOUNDATION", "RESIDENCY UNLIMITED INC", "MATERIAL HANDLING EQUIPMENT DISTRIBUTORS ASSOCIATION", 
"PECKHAM NONPROFIT HOUSING CORPORATION", "FRATERNAL ORDER OF POLICE PASSAIC VALLEY LODGE 181", 
"TENNESSEE CHAPTER OF THE AMERICAN ACADEMY OF PEDIATRICS", "AUGUSTA ELKS LODGE N0205 BPOE", 
"DEPOT THEATER COMPANY", "WALSH UNIVERSITY INC", "SOUTH WHIDBEY ROTARY FOUNDATION", 
"SCOTTISH RITE BODIES IN THE VALLEY OF SPARTANBURG", "SOUTH END COMMUNITY CENTER OF SPFLD INC", 
"IBEW LOCAL 300", "ANCIENT EGYPTIAN ARABIC ORDER 12 ARABIA TEMPLE", 
"HOLLAND PUBLIC SCHOOLS PTO INC", "MARINA DEL REY OUTRIGGER CANOE CLUB", 
"GWINNETT MEDICAL PROPERTIES INC", "AMERICAN FRIENDS OF BIRKAS RIFKA INC", 
"SIERRA NEVADA JOURNEYS", "SILICON VALLEY SOCIAL VENTURE FUND", 
"FLORIDA SUNSHINE ENTERTAINMENT ASSOCIATION INC", "ALANO CLUB OF SONORA", 
"POSITIVE ATTITUDE YOUTH CENTER INC", "PILGRIM SOCIETY", "DUNNELLON AREA CHAMBER OF COMMERCE INC", 
"MASJID-AL-NOOR THE ISLAMIC BULLITIN INC", "WESTMINSTER CANTERBURY OF WINCHESTER INC", 
"SOUTH KANSAS CITY AA GROUP INC", "CLERMONT FOUNDATION", "WORKING AGAINST VIOLENCE INC", 
"RECOVERY AND EMPOWERMENT FOR FAMILY", "CENTRAL KY VOLKSWAGEN CLUB INC", 
"PHOENIX ROTARY CLUB CHARITIES", "UNITED WAY OF LAWRENCE COUNTY INC", 
"CORPUS CHRISTI FIREFIGHTERS ASSOCIATION HEALTH AND BENEFITS TRUST", 
"BRIDGE STRUCTURAL AND ORNAMENTAL IRON WORKERS LABOR TEMPLE CORP", 
"TWIN COUNTY UNITED WAY INC", "CIGAR ASSOCIATION OF NEW HAMPSHIRE", 
"DUQC HOUSING II NFP INC", "TRINITY PINES RETIREMENT CENTER INC", 
"COMMUNICATION WORKERS OF AMERICA 59054 CWA NABET LOCAL", "RED RIVER VALLEY VETERANS MEMORIAL INC", 
"BUILDING STANDARDS INSTITUTE", "RAINBOW HIL APARTMENTS INC", 
"NEWSWOMENS CLUB OF NEW YORK INC", "CHURCH OF THE OLIVE BRANCH", 
"AMTRAK POLICE LODGE 189 INC", "WISCONSIN CONCRETE PIPE ASSOCIATION", 
"INDEPENDENT ORDER OF ODD FELLOWS LODGE 323", "RELIANCE MINISTRIES INC", 
"DOMESTIC VIOLENCE ACTION CENTER", "MANHASSET BOOSTER CLUB INC", 
"BGCSF FULTON STREET CLUBHOUSE INC", "FRANCES E MADDOX FOUNDATION", 
"WILLCOX HISTORIC THEATER PRESERVATION INC", "HUMANINET", "RONALD MCDONALD HOUSE CHARITIES OF THE OZARKS INC", 
"ROBERTSON RESIDENTIAL CENTER INC", "DEVILS LAKE YOUTH ACTIVITIES ASSN", 
"SYNTIRO", "PEARL RIVER VALLEY ELECTRIC POWER ASSOCIATION", "WOMENS CENTER FOR ADVANCEMENT", 
"JAPAN-AMERICA SOCIETY OF PENNSYLVANIA INC", "MISSISSIPPI ALPHA CHAPTER OF PHI DELTA THETA", 
"BROWN COUNTY DEVELOPMENTAL SERVICES INC", "CHARITABLE CONSORTIUM INC", 
"AMYOTROPHIC LATERAL SCLEROSIS KENTUCKY CHAPTER", "LIONS CLUB OF KOTZEBUE ALASKA", 
"LAGUARDIA HIGH SCHOOL PARENTS ASSOC INC", "TRAVIS COUNTY CREDIT UNION", 
"NEW LIFE CENTER FOR CHILDREN AND MOTHERS", "KIWANIS CLUB OF THE DELTA ANTIOCH FOUNDATION", 
"MIAMI DADE COLLEGE FOUNDATION INC", "AMERICAN PEANUT PRODUCT MANUFACTURERS INC", 
"SAN DIEGO COUNTY BUILDING TRADES COUNCIL FAMILY HOUSING CORP NO 3", 
"WOMENS RESOURCE CENTER GRAND TRAVERSE AREA", "GRANDMAS GIFTS INC", 
"BANGLADESHI ADVENTIST SOCIETY OF AMERICA", "CAITLIN ELIZABETH RUSSELL FOUNDATION INC", 
"SARAH BETH COYOTE FOUNDATION", "ORGANIZACION INTERNACIONAL DE LATINOS EN EL EXTERIOR OF SWFL INC", 
"BETA ZETA FOUNDATION OF DELTA TAU DELTA", "HUNTLEY CHEER ASSOCIATION INC", 
"NORTHEASTERN SOCIETY OF ORTHODONTISTS", "CROTON TEACHERS ASSOCIATION BENEFIT TRUST FUND", 
"HOUSTON COUNCIL OF THE BLIND", "OCHSNER COMMUNITY HOSPITALS", 
"SWANSEA INDEPENDENT BASEBALL LEAGUE INC", "COLORADO HIGH SCHOOL CHARTER INC", 
"DISTRICT OF COLUMBIA ACCESS TO JUSTICE FOUNDATION", "DAYTON BALLROOM DANCE CLUB", 
"DA CAPO CHORALE INC", "SHAPING THE YOU IN YOUTH", "JACKSON COMMUNITY COLLEGE FOUNDATION DBA JACKSON COLLEGE FOUNDATION", 
"ILLINOIS TELECOMMUNICATIONS ASSOCIATION INC", "NORTH COLORADO HEALTH ALLIANCE", 
"DWANA SMALLWOOD PERFORMING ARTS CENTER INC", "PORT WASHINGTON CHILDRENS CENTER INC", 
"ARMENIAN RELIEF SOCIETY OF EASTERN USA INC", "ARNOT-OGDEN AUXILIARY", 
"NORTH CAROLINA COALITION TO END HOMELESSNESS INC", "LIVONIA HOCKEY ASSOCIATION", 
"UC RIVERSIDE FOUNDATION", "MICH COUNCIL OF NURSE PRACTIONERS", 
"NORTHEAST ENERGY EFFICIENCY PARTNERSHIPS INC", "GALLOWAY HOMES INC", 
"GRAND COUNCIL OF INDEPEND ORDER OF", "ALL AUSTIN COOPERATIVE SCHOOL", 
"BENJAMIN AND LILLIAN ROCHKIND YESHIVA OF VIRGINIA INC", "CARLOCK PRUETT ATHLETIC FOUNDATION", 
"FIX OUR FERALS", "FRIENDS OF THE FLETCHER FREE LIBRARY", "SIMON WIESENTHAL CENTER INC", 
"TEAM MEMBER ACTIVITIES ASSOCIATION TEXAS", "IOFEMI A GIFT OF LOVE", 
"KAMPUCHEA FOR CHRIST USA DBA TRANSFORM ASIA", "BYUNP INC", "AMERICAN LEGION HOME ASSOCIATION POST 273", 
"BARBARA C HARRIS CENTER", "HABITAT FOR HUMANITY OF NEW YORK STATE INC", 
"EXTENDED HOUSING INC", "CONQUER CANCER FOUNDATION OF THE AMERICAN SOCIETY OF CLINICAL ONCOLOGY", 
"COMMUNITY ACTION AND DEVELOPMENT INC", "FRIENDS OF THE MANSION AT SMITHVILLE", 
"CHANNAHON PROF FIREFIGHTERS ASSOC", "CABOT AFTER SCHOOL INC", 
"AMERICAN LAWMEN INC", "EAST TEXAS WILDLIFE REHABILITATION", 
"CHADRON HOUSING FOUNDATION", "TRI COMMUNITY LODGE", "INDEPENDENT LIVING CENTER OF MID-MO", 
"WESTWOOD LACROSSE CLUB BOOSTER ORG AKA WESTWOOD PARENTS CLUB", 
"YOUNG MENANDAPOSS CHRISTIAN ASSOCIATION OF GREATER LOUISVILLE", 
"SAFE BABIES HEALTHY FAMILIES INC", "LAW LIBRARY ASSOCIATION OF ST LOUIS", 
"LOCAL 434 HEALTH AND WELFARE FUND", "GRAPEVINE OUTREACH", "LANDLORDS ASSOCIATION OF INDEPENDENCE", 
"EAST BALTIMORE DEVELOPMENT INC", "WALL STREET CARES INC", "SONGTIME INC", 
"UNITED STEELWORKERS 7-00001 LOCAL", "PLANK ROAD STATION", "HILLSIDE HAVEN", 
"BALLET SAN JOSE SILICON VALLEY", "GOOD SAMARITAN INN INC", "UNIVERSITY CLUB OF MICHIGAN STATE UNIVERSITY", 
"TECHNOLOGY ACCESS FOUNDATION", "INDIA ASSOCIATION OF KC INC", 
"117 ELECTRICAL WORKERS ASSOCIATION", "BETA EPSILON HOUSING CORPORATION", 
"MUSICAL CLUB OF HARTFORD INC CO LEAN-CHENG TAN", "US SENIOR VETS", 
"CARPENTER TECHNOLOGY CORP VEBA TRUST", "STIRRUPS N STRIDES THERAPEUTIC RIDING CENTER INC", 
"LUCAS HOUSING SERVICES CORPORATION", "FINDLAY ROTARY FOUNDATION INC", 
"JAMESTOWN SOCCER FOUNDATION", "MICE MESZAROS INTERNATIONAL CENTER OF ENTREPRENEURSHIP INC", 
"TRIWAY ATHLETIC BOOSTER CLUB", "USA MARITIME", "HOWARD J SCHROEDER LEGION CLUB INC", 
"GAINESVILLE AREA CHAMBER OF COMMERCE INC", "LUTHER HEIGHTS INC CO HOMZ MANAGEMENT", 
"MASON CITY CHAMBER OF COMMERCE", "COLES MOULTRIE ELECTRIC COOPERATIVE", 
"MOUNTAINEER LITTLE LEAGUE INC", "SISTERS OF THE ROAD INC", "WYOMING MEDICAL CENTER AUXILARY", 
"KEMAH VOLUNTEER FIRE DEPARTMENT", "OPERA IDAHO INC", "SDC TREVOSE METROPOLITAN BUSINESS CENTER - PENNSYLVANIA INC", 
"MONROE COUNTY COUNCIL ON AGING INC", "SOUTHWEST BADGER RESOURCE CONSERVATION AND DEVELOPMENT COUNCIL", 
"URBAN LEAGUE OF GREATER COLUMBUS INC", "REGION 5 OVEREATERS ANONYMOUS C O GERRI MARTIN", 
"HEKETI COMMUNITY CHARTER SCHOOL", "GOLDEN APPLE FOUNDATION OF ROCKFORD", 
"SISTERS ENLISTED TO EMPOWER DREAMS", "ANITA HOUSING CORPORATION", 
"FORGET ME NOT FAMILIES INC", "BIG ISLAND RENDEZVOUS AND FESTIVAL INCARTM", 
"MICHAEL P SCHAAB FOUNDATION", "HOLY GHOST SOCIETY OF SEBASTOPOL", 
"SOUTH CAROLINA RESPITE COALITION", "RIVERSIDE BLUFFS REALTY CORPORATION CO JP MORGAN INVESTMENT MANAGEMENT INC", 
"NEBRASKA RIGHT TO LIFE INC", "NORTHEASTERN JUNIOR COLLEGE FOUNDATION INC", 
"CORTLAND FUNDING FACILITIES FOR THE HANDICAPPED INC", "AMERICAN POSTAL WORKERS AFL-CIO E MONTGOMERY COUNTY AREA LOCAL 2233", 
"GOOD NEWS FOUNDATION INTERNATIONAL INC", "CHARIS HILLS INC", 
"MIDDLE EAST MEDIA", "BROWN COUNTY FARM BUREAU", "KEEP THE COUNTRY DANCING INC", 
"COMMUNICATIONS WORKERS OF AMERICA LOCAL UNION 3204", "HONORS CHOIRS OF SOUTHEAST MINNESOTA", 
"SOJOURNER HOUSE", "COMMUNITY CARE PRESCHOOL AND CHILDCARE INC", 
"SERVANT CONNECTION", "COLEMAN UNIVERSITY", "JEWISH FEDERATION OF SOUTHERN ARIZONA", 
"WILMINGTON SOCCER ACADEMY PORT CITY SOCCER", "KIWANIS INTERNATIONAL KO1688 WEATHERFORD", 
"OHIO PODIATRY INSTITUTE INC", "PI BETA PHI FRATERNITY OKLAHOMA ALPHA CHAPTER", 
"OPPORTUNITIES FOR ACCESS", "AMERICAN COWBOY TEAM ROPING ASSN WY", 
"SPRINGFIELD EXECUTIVE BREAKFAST", "TRANSIT MANAGERIAL BENEVOLENT ASSOCIATION INC", 
"AHIMSA HOUSE INC", "HILLCREST COMMUNITY SCHOOL PTSA", "ST ANNS COUNCIL 2853 KNIGHTS OF COLUMBUS", 
"INDEPENDENCE BOWL FOUNDATION INC", "THRESHOLD INCORPORATED", 
"WYOMING TERRITORIAL PARK HISTORIC ASSOCIATION INC", "SAFARI CLUB INTL SE MI BOWHUNTERS", 
"SOUTHWEST YOUTH SERVICES COLLABORATIVE", "GREATER BINGHAMTON EDUCATION OUTREACH PROGRAM INC", 
"PUBLIC REVIEW BOARD INTERNATIONAL UNION UAW", "BERKS BALLET THEATRE CO GOGGLE WORKS CENTER FOR THE ART", 
"ONEONTA ROTARY FUND INC", "LAMBDA ALPHA OF CHI OMEGA HOUSE CORP INC", 
"PTA PLAYA DEL REY ELEMENTARY SCHOOL", "FRIENDS OF FORT HUNT PARK INC", 
"INTERNATIONAL PEDIATRIC RESEARCH FOUNDATION INC", "UNION COUNCIL NO 8 IFPTEAFL-CIO CLC", 
"CHRISTIAN HANDS IN ACTION", "AMERICAN LEGION POST 47", "MUSICAL TRADITIONS", 
"KRIS WILSON FOUNDATION II", "CHRISTOPHERS WAY", "BOYS AND GIRLS CLUB OF TRUCKEE MEADOWS", 
"NEW JERSEY CHARTER SCHOOLS ASSOC", "KIDS PLACE A PLACE FOR KIDS TO BE II INC", 
"TRAIL BLAZERS MINISTRY", "OROVILLE HOSPITAL", "SHOCK TRAUMA ASSOCIATES PA", 
"PATH OF LIFE ENTERPRISES", "MODIFIED MOTORCYCLE ASSOCIATION OF MASSACHUSETTS INC", 
"POWELL COUNTY MEMORIAL HOSPITAL ASSOC", "DANCE ENTROPY INC", 
"MASONIC TEMPLE ASSOCIATION OF PHOENIX INC", "SAN ANTONIO CHAPTER AMERICAN CONCRETE INSTITUTE INC", 
"SEATTLE NEIGHBORHOOD GROUP", "OXNARD FEDERATION OF TEACHERS AND SCHOOL SCHOOL EMPLOYEES AFT LOCAL 1273", 
"WELLNESS WORKS COMMUNITY HEALTH CENTER", "NORTH SHORE COUNTRY CLUB INC", 
"CUDDLYBEAR THRIFT STORE INC", "GENERAL FEDERATION OF WOMENS CLUBS - AZ", 
"BUTLER MEALS ON WHEELS INC", "STANFORD AMERICAN EDUCATION FOUNDATION INC", 
"ACARE HUMAN SERVICES INC CO HANS GIPLAYE", "ELKHART TEACHERS ASSOCIATION INC", 
"CLAYTON YACHT CLUB INC", "CARDINAL GLENNON CHILDRENS FOUNDATION", 
"HOUSING VENTURES INC", "PTAM LINCOLN CENTER", "NORTHWEST KANSAS AREA AGENCY ON AGING INC", 
"CASCADE BULLDOG RESCUEREHOME", "NORTHEAST TEXAS CHILDRENS MUSEUM", 
"CENTRAL FLORIDA HIGH SCHOOL HOCKEY LEAGUE", "REDWOOD AREA CHAMBER AND TOURISM", 
"CHINESE HOSPITAL MEDICAL STAFF"), SubmittedOn = c("2016-02-03", 
"2017-02-10", "2011-12-30", "2018-06-04", "2018-03-09", "2013-09-25", 
"2015-10-14", "2011-06-07", "2017-03-31", "2014-03-19", "2017-10-20", 
"2017-01-24", "2018-03-02", "2013-11-13", "2016-02-10", "2011-09-07", 
"2014-07-02", "2016-01-26", "2018-03-16", "2017-11-17", "2012-11-06", 
"2012-09-28", "2014-03-27", "2017-09-14", "2016-01-14", "2014-12-29", 
"2013-12-20", "2015-07-18", "2017-04-18", "2015-07-09", "2018-01-29", 
"2013-06-26", "2016-09-28", "2012-11-07", "2014-10-17", "2017-10-26", 
"2015-08-06", "2014-01-04", "2011-11-14", "2011-12-28", "2015-08-05", 
"2011-11-29", "2018-03-14", "2013-06-26", "2012-11-02", "2015-02-04", 
"2018-03-26", "2014-10-27", "2012-10-13", "2017-08-10", "2016-08-02", 
"2012-11-02", "2012-11-15", "2016-02-18", "2016-11-07", "2016-08-23", 
"2012-01-03", "2012-11-20", "2016-02-08", "2018-05-30", "2014-10-15", 
"2013-07-30", "2015-02-23", "2012-03-26", "2017-10-27", "2015-07-08", 
"2016-07-12", "2016-08-18", "2018-04-23", "2017-02-15", "2018-04-23", 
"2013-03-27", "2016-07-20", "2017-09-28", "2012-09-27", "2013-05-15", 
"2016-02-26", "2017-12-26", "2018-01-09", "2018-01-09", "2015-07-27", 
"2018-01-10", "2017-09-06", "2017-08-09", "2013-09-16", "2011-09-28", 
"2012-12-27", "2016-12-07", "2016-08-29", "2016-08-04", "2015-10-06", 
"2014-10-10", "2014-10-14", "2014-07-15", "2013-09-16", "2018-01-09", 
"2013-12-26", "2015-01-29", "2016-11-07", "2017-07-20", "2011-11-04", 
"2013-12-23", "2018-04-03", "2016-02-09", "2016-02-09", "2016-11-07", 
"2017-04-03", "2016-08-22", "2012-05-30", "2017-10-18", "2011-11-08", 
"2015-08-03", "2011-10-12", "2018-01-29", "2014-12-29", "2011-11-17", 
"2017-01-03", "2011-09-28", "2012-09-24", "2012-04-03", "2012-10-15", 
"2017-10-11", "2011-09-23", "2013-07-22", "2015-05-15", "2015-03-03", 
"2015-07-14", "2014-12-29", "2016-03-23", "2014-09-23", "2014-10-10", 
"2015-07-06", "2014-01-03", "2016-08-17", "2016-09-01", "2013-11-14", 
"2013-07-11", "2018-04-12", "2017-09-25", "2011-09-15", "2016-08-08", 
"2015-07-30", "2012-10-30", "2011-11-03", "2014-08-18", "2015-05-26", 
"2013-08-08", "2011-10-21", "2012-09-20", "2018-01-02", "2017-10-12", 
"2013-05-07", "2016-10-06", "2014-12-10", "2012-12-10", "2014-12-31", 
"2014-12-29", "2013-02-13", "2016-01-28", "2015-07-10", "2017-08-14", 
"2017-03-29", "2017-09-25", "2012-03-27", "2018-02-13", "2014-09-26", 
"2017-10-26", "2016-03-18", "2016-08-23", "2015-10-05", "2017-01-03", 
"2015-07-09", "2016-10-12", "2015-09-24", "2016-02-24", "2017-10-19", 
"2014-05-22", "2012-10-03", "2015-09-21", "2014-12-29", "2013-05-30", 
"2012-09-20", "2017-10-10", "2013-09-19", "2014-12-29", "2013-11-19", 
"2017-11-21", "2014-08-01", "2017-10-13", "2012-12-31", "2017-09-13", 
"2012-11-05", "2017-02-13", "2016-02-08", "2016-01-20", "2013-03-09", 
"2014-12-29", "2012-11-16", "2013-09-26", "2017-10-18", "2014-12-30", 
"2013-12-19", "2017-04-01", "2011-03-17", "2018-03-13", "2018-01-03", 
"2013-07-15", "2015-07-06", "2016-08-04", "2016-09-28", "2013-12-23", 
"2017-10-18", "2011-10-11", "2014-12-29", "2016-03-25", "2012-11-21", 
"2017-08-07", "2014-10-09", "2017-11-06", "2011-10-11", "2011-10-05", 
"2017-07-26", "2017-12-29", "2016-01-20", "2014-01-08", "2016-09-26", 
"2014-12-29", "2012-10-23", "2012-01-05", "2014-10-15", "2015-01-27", 
"2017-09-15", "2017-09-13", "2017-08-03", "2014-12-31", "2012-11-06", 
"2012-10-25", "2013-08-21", "2018-01-05", "2011-09-23", "2013-08-14", 
"2017-11-08", "2014-09-16", "2014-01-03", "2012-12-03", "2012-12-21", 
"2017-10-06", "2012-12-31", "2012-10-11", "2013-08-01", "2017-10-26", 
"2015-07-02", "2017-10-11", "2017-09-13", "2016-12-09", "2013-09-23", 
"2013-11-21", "2012-10-16", "2017-06-30", "2018-01-10", "2017-01-04", 
"2014-12-11", "2017-09-25", "2014-11-21", "2014-12-29", "2014-03-27", 
"2016-12-27", "2016-07-26", "2014-12-30", "2017-02-15", "2012-10-24", 
"2017-03-30", "2014-04-28", "2015-08-24", "2012-10-31", "2014-12-30", 
"2012-10-03", "2016-08-02", "2016-07-26", "2016-07-27", "2014-09-23", 
"2014-09-24", "2017-12-28", "2012-11-16", "2017-10-13", "2011-06-06", 
"2014-12-29", "2011-01-31", "2015-07-14", "2012-11-07", "2016-02-06", 
"2017-08-10", "2017-10-06", "2014-12-29", "2017-10-25", "2016-02-24", 
"2013-08-13", "2015-02-04", "2017-12-27", "2011-04-13", "2014-12-29", 
"2012-10-16", "2017-07-13", "2018-05-23", "2017-08-11", "2017-11-28", 
"2012-09-20", "2014-01-08", "2018-01-11", "2012-12-07", "2018-04-20", 
"2018-03-14", "2013-12-17", "2011-01-27", "2012-04-04", "2012-12-31", 
"2013-02-15", "2012-12-18", "2012-12-14", "2012-12-14", "2015-08-05", 
"2013-12-23", "2016-07-07", "2011-11-18", "2014-10-30", "2016-07-08", 
"2014-12-03", "2015-03-03", "2017-08-23", "2016-12-12", "2017-07-11", 
"2014-11-24", "2011-12-10", "2014-05-27", "2018-05-30", "2017-08-01", 
"2015-07-13", "2016-01-14", "2015-09-11", "2013-03-25", "2011-12-13", 
"2012-10-30", "2014-10-01", "2014-10-16", "2012-10-22", "2014-10-16", 
"2011-04-12", "2017-01-03", "2017-08-11", "2014-10-22", "2013-12-30", 
"2013-08-01", "2012-06-06", "2013-12-20", "2014-09-25", "2013-08-06", 
"2015-06-26", "2016-12-22", "2015-07-09", "2017-10-13", "2016-03-18", 
"2012-12-04", "2014-10-29", "2017-11-20", "2016-01-12", "2014-12-04", 
"2015-09-16", "2015-06-25", "2014-10-20", "2016-08-05", "2016-03-24", 
"2014-12-29", "2011-11-07", "2014-12-29", "2017-02-23", "2014-11-20", 
"2012-03-22", "2012-10-30", "2014-05-27", "2017-10-10", "2015-06-26", 
"2015-07-20", "2015-05-14", "2018-01-12", "2012-10-24", "2011-10-12", 
"2018-01-05", "2016-07-25", "2013-07-24", "2016-08-02", "2017-11-13", 
"2013-02-25", "2018-03-13", "2017-01-20", "2013-08-07", "2011-11-23", 
"2016-12-28", "2016-07-26", "2015-08-06", "2015-10-05", "2014-07-02", 
"2011-01-27", "2012-12-06", "2018-03-26", "2013-11-18", "2013-03-15", 
"2011-11-23", "2011-09-15", "2011-09-07", "2012-06-21", "2012-10-11", 
"2016-07-07", "2014-10-15", "2015-08-06", "2018-02-13", "2013-06-26", 
"2014-10-15", "2016-09-01", "2012-10-11", "2017-10-04", "2017-01-10", 
"2012-10-24", "2014-11-18", "2013-07-19", "2015-01-27", "2016-07-15", 
"2012-10-12", "2016-03-25", "2015-06-15", "2017-07-31", "2014-10-14", 
"2012-12-31", "2014-12-29", "2017-10-25", "2013-12-04", "2012-10-27", 
"2012-07-10", "2011-03-11", "2013-09-27", "2012-10-20", "2015-07-29", 
"2015-10-05", "2016-02-11", "2015-02-03", "2016-01-27", "2013-08-19", 
"2013-12-30", "2017-09-14", "2018-03-26", "2012-10-30", "2015-07-17", 
"2014-09-24", "2016-08-29", "2018-01-09", "2014-09-22", "2014-10-10", 
"2014-12-29", "2016-12-27", "2017-11-15", "2011-06-07", "2015-07-17", 
"2016-07-21", "2014-10-01", "2011-12-30", "2013-07-10", "2011-12-06", 
"2017-02-06", "2017-08-02", "2017-09-06", "2017-08-31", "2015-10-02", 
"2012-10-18", "2015-07-07", "2018-03-05", "2014-10-14", "2012-10-03", 
"2017-02-06", "2017-10-17", "2013-08-14", "2015-07-06", "2015-07-22", 
"2017-04-05", "2012-10-22", "2014-12-29", "2015-09-17", "2017-11-30", 
"2017-08-23", "2011-06-01", "2011-03-01", "2011-12-08", "2018-01-10", 
"2018-02-01", "2016-10-21", "2011-11-21", "2014-11-24", "2017-07-11", 
"2017-11-02", "2014-09-23", "2014-11-24", "2018-01-04", "2017-01-12", 
"2011-11-22", "2018-01-04", "2018-01-04", "2015-02-04", "2017-10-19", 
"2012-11-21", "2014-03-28", "2017-07-03", "2012-08-29", "2016-12-13", 
"2016-11-01", "2015-01-27", "2017-12-28", "2012-05-25", "2012-10-02", 
"2011-11-07", "2015-05-26", "2017-11-02", "2014-10-28", "2014-12-29", 
"2014-10-23", "2013-06-26", "2014-12-03", "2017-08-17", "2012-06-11", 
"2014-12-29", "2015-08-20", "2012-11-08", "2014-12-03", "2011-11-18", 
"2013-05-15", "2016-07-07", "2018-05-24", "2016-12-20", "2015-09-29", 
"2016-12-06", "2016-02-20", "2012-11-21", "2014-01-03", "2014-11-14", 
"2014-03-31", "2012-09-21", "2012-05-25", "2015-02-18", "2014-01-31", 
"2012-10-05", "2012-11-21", "2017-08-01", "2014-09-24", "2015-08-24", 
"2015-08-17", "2013-11-19", "2016-01-20", "2013-08-19", "2018-03-07", 
"2013-07-29", "2011-03-02", "2015-08-04", "2012-09-21", "2014-12-04", 
"2014-03-17", "2012-05-30", "2015-06-26", "2016-01-20", "2012-10-17", 
"2012-10-12", "2016-09-27", "2012-12-27", "2012-12-10", "2013-08-09", 
"2013-08-01", "2014-03-18", "2012-09-20", "2014-09-19", "2017-02-10", 
"2015-07-02", "2013-08-09", "2012-10-26", "2017-09-15", "2017-10-25", 
"2015-07-22", "2017-10-06", "2018-03-14", "2015-07-27", "2018-03-27", 
"2013-02-12", "2014-11-24", "2011-10-13", "2017-04-17", "2013-12-03", 
"2012-12-21", "2015-07-23", "2012-10-23", "2014-03-20", "2014-09-24", 
"2018-01-03", "2015-07-07", "2016-01-11", "2015-10-05", "2016-08-16", 
"2012-10-26", "2016-12-14", "2016-01-11", "2016-12-27", "2012-12-17", 
"2012-06-11", "2012-10-15", "2012-10-22", "2011-06-06", "2013-11-13", 
"2014-12-05", "2015-08-12", "2011-12-23", "2011-09-15", "2014-12-29", 
"2015-07-14", "2014-12-29", "2015-08-03", "2011-04-18", "2014-12-29", 
"2018-05-24", "2018-03-19", "2011-11-02", "2015-10-08", "2013-12-10", 
"2012-10-25", "2013-07-11", "2012-01-04", "2016-08-29", "2017-10-02", 
"2017-03-31", "2013-03-13", "2016-10-05", "2014-09-19", "2018-03-14", 
"2014-03-14", "2016-07-20", "2015-07-06", "2016-12-29", "2017-02-21", 
"2016-01-26", "2011-12-16", "2011-11-10", "2018-06-07", "2013-12-20", 
"2018-05-23", "2012-11-26", "2017-09-20", "2018-01-12", "2013-07-25", 
"2014-12-30", "2016-02-25", "2012-01-04", "2016-09-01", "2016-10-20", 
"2017-01-25", "2011-09-23", "2015-08-20", "2017-10-16", "2014-12-04", 
"2014-10-09", "2017-07-25", "2017-12-26", "2015-08-12", "2014-09-18", 
"2017-02-10", "2017-10-24", "2018-04-02", "2017-10-12", "2011-03-24", 
"2016-07-18", "2013-03-27", "2017-01-19", "2017-02-09", "2013-12-14", 
"2018-01-04", "2011-10-20", "2017-10-26", "2016-03-23", "2014-01-08", 
"2015-06-26", "2013-12-10", "2016-11-08", "2018-05-16", "2011-10-17", 
"2011-12-06", "2017-09-25", "2016-01-08", "2018-04-04", "2013-03-12", 
"2015-03-19", "2018-05-31", "2017-10-23", "2014-12-29", "2012-05-29", 
"2016-01-12", "2016-02-25", "2017-04-05", "2015-07-21", "2013-08-10", 
"2018-05-24", "2013-07-17", "2016-09-02", "2014-09-11", "2014-08-29", 
"2017-01-23", "2015-01-29", "2012-04-03", "2011-11-15", "2013-07-11", 
"2011-12-09", "2016-12-28", "2014-10-28", "2017-02-21", "2014-12-29", 
"2014-10-14", "2012-09-25", "2016-09-02", "2014-10-21", "2013-07-19", 
"2011-11-03", "2011-03-17", "2016-01-21", "2017-10-05", "2015-09-18", 
"2014-01-10", "2014-10-14", "2013-10-25", "2016-08-04", "2015-02-24", 
"2011-06-01", "2016-12-13", "2017-08-25", "2013-11-13", "2011-09-12", 
"2014-10-23", "2016-01-27", "2016-08-16", "2016-07-15", "2018-01-04", 
"2013-08-06", "2013-12-31", "2014-12-03", "2017-07-17", "2017-02-09", 
"2012-08-30", "2013-08-14", "2017-02-27", "2016-10-04", "2014-11-18", 
"2012-10-30", "2017-10-06", "2017-02-03", "2016-08-16", "2015-02-24", 
"2013-07-16", "2012-10-01", "2014-03-14", "2012-03-23", "2014-09-30", 
"2014-07-09", "2012-11-01", "2014-01-09", "2017-09-28", "2017-09-08", 
"2014-09-17", "2012-10-03", "2017-02-06", "2011-09-06", "2012-12-20", 
"2014-12-01", "2016-07-13", "2014-09-10", "2017-01-03", "2013-07-25", 
"2017-07-13", "2016-10-12", "2015-07-16", "2012-10-18", "2016-08-30", 
"2016-08-31", "2015-10-07", "2013-12-04", "2012-10-23", "2015-07-31", 
"2016-07-19", "2011-11-29", "2017-08-30", "2012-10-31", "2013-11-05", 
"2014-10-15", "2017-04-10", "2012-12-31", "2017-10-20", "2015-06-29", 
"2012-09-20", "2017-11-30", "2018-03-19", "2016-02-26", "2017-09-06", 
"2012-11-20", "2014-10-28", "2014-07-02", "2014-06-30", "2016-09-01", 
"2016-08-25", "2014-03-27", "2011-04-12", "2017-09-14", "2014-04-03", 
"2016-08-05", "2011-12-07", "2012-09-21", "2014-12-29", "2016-02-04", 
"2016-08-19", "2015-08-21", "2015-07-08", "2015-06-26", "2013-12-21", 
"2018-03-16", "2017-02-23", "2013-02-27", "2013-03-13", "2014-10-09", 
"2011-09-16", "2011-10-05", "2016-08-16", "2016-08-02", "2011-09-22", 
"2014-10-22", "2016-02-03", "2015-02-06", "2017-11-17", "2016-01-12", 
"2016-12-14", "2016-01-11", "2016-03-25", "2015-09-10", "2011-11-03", 
"2012-01-03", "2011-11-08", "2014-01-09", "2017-07-20", "2014-10-25", 
"2014-09-18", "2017-01-23", "2011-06-10", "2015-08-13", "2016-12-28", 
"2012-10-23", "2017-10-25", "2016-12-14", "2018-01-03", "2017-01-17", 
"2016-07-19", "2014-09-18", "2014-09-15", "2017-12-29", "2014-10-15", 
"2012-01-05", "2014-09-17", "2017-09-07", "2011-11-30", "2016-09-08", 
"2014-09-22", "2014-03-21", "2017-09-12", "2016-09-01", "2013-11-20", 
"2012-04-04", "2011-11-29", "2015-01-27", "2017-09-28", "2016-07-25", 
"2015-08-10", "2016-09-15", "2017-07-18", "2015-08-11", "2015-08-20", 
"2015-09-30", "2011-10-05", "2014-09-11", "2015-09-21", "2011-10-19", 
"2016-08-23", "2017-10-23", "2013-08-07", "2012-11-20", "2014-09-24", 
"2017-12-27", "2018-01-08", "2012-12-14", "2017-10-23", "2016-02-26", 
"2018-01-25", "2017-02-08", "2017-03-08", "2013-07-11", "2016-12-14", 
"2011-12-09", "2016-07-13", "2014-04-09", "2017-12-26", "2017-09-11", 
"2017-10-06", "2017-09-05", "2017-10-30", "2018-01-09", "2013-11-14", 
"2017-09-28", "2012-07-06", "2016-02-11", "2016-09-29", "2011-10-05", 
"2017-10-19", "2012-10-19", "2015-07-30", "2015-02-02", "2014-03-20", 
"2017-04-01", "2018-01-08", "2017-12-27", "2011-11-21", "2011-12-07", 
"2016-07-12", "2015-03-10", "2017-12-04", "2016-01-21", "2011-09-29", 
"2016-12-19", "2014-03-13", "2016-08-31", "2011-09-09", "2013-08-07", 
"2017-10-04", "2012-10-15", "2017-11-21", "2017-07-12", "2015-07-22", 
"2018-03-05", "2015-09-11", "2015-03-03", "2017-01-12", "2016-10-17", 
"2017-01-18", "2016-03-28", "2015-08-14", "2016-01-15", "2017-09-12", 
"2011-12-30", "2014-12-09", "2012-07-02", "2012-09-22", "2013-07-25", 
"2016-08-09", "2018-01-24", "2016-08-02", "2017-02-13", "2015-02-09", 
"2012-10-23", "2013-07-17", "2015-07-29", "2016-08-18", "2016-08-02", 
"2013-08-22", "2017-09-13", "2017-11-07", "2016-08-02", "2017-12-27", 
"2018-01-09", "2012-11-06", "2012-04-03", "2015-08-20", "2017-10-04", 
"2016-04-06", "2013-12-24", "2016-09-15", "2012-12-14", "2011-11-21", 
"2013-02-25", "2012-10-26", "2017-08-14", "2013-07-31", "2012-10-09", 
"2015-06-26", "2016-12-20", "2018-01-04", "2016-10-21", "2017-09-20", 
"2012-12-20", "2011-10-27", "2014-04-07", "2016-02-25", "2011-09-22", 
"2015-09-22", "2017-07-22", "2015-08-10", "2012-12-19"), ObjectId = c("201513169349305886", 
"201623199349201442", "201143199349201639", "201801009349200435", 
"201800439349200110", "201312149349200501", "201532719349300408", 
"201101339349300330", "201623209349312437", "201430359349200423", 
"201701789349300215", "201622919349300892", "201820249349200607", 
"201322689349300317", "201533209349306138", "201112279349304181", 
"201431349349306183", "201513229349300521", "201800159349300715", 
"201712789349301211", "201242939349300619", "201201299349200915", 
"201410539349200706", "201701679349200320", "201512859349200106", 
"201413189349304486", "201313189349305476", "201540569349300719", 
"201613429349300226", "201511769349300811", "201713139349302521", 
"201310739349200721", "201601379349308115", "201242869349200204", 
"201402249349300925", "201732559349300548", "201531349349300348", 
"201313199349306491", "201102909349200000", "201123149349301327", 
"201511129349200516", "201113159349303751", "201800459349300420", 
"201330679349200123", "201242239349301564", "201500219349300725", 
"201800529349300995", "201431929349300003", "201201369349203765", 
"201730669349200528", "201601399349301010", "201232289349304503", 
"201242299349200714", "201533209349302528", "201622289349302937", 
"201610759349300721", "201113199349305626", "201232589349200513", 
"201543169349300549", "201841359349311574", "201442589349200344", 
"201341649349200014", "201520439349300412", "201210629349301671", 
"201702299349200015", "201501669349300825", "201610439349303171", 
"201620579349300022", "201821029349200527", "201643069349300509", 
"201811029349300701", "201340729349300119", "201620549349200302", 
"201701339349300870", "201211239349300631", "201331209349300963", 
"201503579349300100", "201723139349301357", "201733179349306543", 
"201733189349301148", "201521319349303172", "201723149349200042", 
"201701299349301835", "201700689349300715", "201332079349301203", 
"201101229349200350", "201233209349307458", "201612159349200006", 
"201631029349200438", "201631319349303083", "201542339349300514", 
"201421339349301222", "201402729349300145", "201421679349200347", 
"201322129349300512", "201703479349300105", "201343179349202479", 
"201540159349301194", "201612299349300746", "201740389349301204", 
"201131319349201653", "201313179349302121", "201820719349200642", 
"201513089349200206", "201513079349200401", "201612289349200041", 
"201623209349307122", "201640789349300859", "201200889349200230", 
"201722279349200697", "201121819349300807", "201531319349301538", 
"201141299349200914", "201713139349303406", "201433219349309688", 
"201102599349300700", "201612359349200616", "201120889349300722", 
"201220909349200747", "201240719349200504", "201211359349305311", 
"201731749349200603", "201122519349200612", "201321269349200527", 
"201530989349300313", "201540489349200004", "201501559349300725", 
"201413589349300501", "201620199349200507", "201432379349200368", 
"201431759349200028", "201501269349300970", "201323199349303717", 
"201620859349300312", "201601329349303730", "201332219349200723", 
"201311119349300201", "201840759349301109", "201711359349309666", 
"201132239349301223", "201641349349301124", "201521339349300632", 
"201221369349306152", "201142659349300714", "201410739349300836", 
"201531059349200008", "201331899349300038", "201111539349300106", 
"201220749349300707", "201703199349317800", "201712289349301201", 
"201321079349200312", "201621449349200217", "201423169349202482", 
"201223209349200942", "201403229349200640", "201443189349303219", 
"201320249349200307", "201513159349301866", "201501329349301440", 
"201740729349301054", "201613209349309981", "201721359349302077", 
"201230629349200953", "201713499349200421", "201412279349303841", 
"201742239349200514", "201610149349300126", "201620579349200612", 
"201532299349301533", "201632289349201368", "201511569349300816", 
"201601629349300015", "201522269349300537", "201503569349300035", 
"201702309349200500", "201421129349200312", "201201289349300925", 
"201512329349300206", "201433219349200548", "201331229349300723", 
"201210809349300041", "201711319349202926", "201322179349300512", 
"201433189349308783", "201302219349200245", "201741009349300509", 
"201422039349200612", "201722019349200327", "201243489349300509", 
"201721609349300032", "201212279349304166", "201633169349200538", 
"201533089349200243", "201513179349304926", "201340449349301009", 
"201443219349311749", "201202419349200420", "201302539349300225", 
"201742229349200419", "201403219349305705", "201313189349200751", 
"201623209349312257", "201110469349200616", "201830319349300103", 
"201743179349307454", "201330639349200608", "201521269349300212", 
"201601349349303240", "201641339349200634", "201333129349301838", 
"201701959349300445", "201141319349201674", "201433189349309418", 
"201630209349200803", "201203129349301405", "201710619349300106", 
"201431549349200008", "201702629349200210", "201131339349303828", 
"201111249349300441", "201710469349303506", "201703199349312440", 
"201503209349306160", "201323459349300642", "201631339349302923", 
"201433219349303003", "201211999349300021", "201113399349300061", 
"201442679349300539", "201540159349300329", "201741359349307044", 
"201731469349200113", "201740459349302704", "201403219349204000", 
"201232999349200023", "201212159349200316", "201341339349303404", 
"201703189349308835", "201120689349200717", "201321309349201867", 
"201722709349301022", "201431339349202373", "201323199349204172", 
"201243119349301164", "201213209349304296", "201711319349201591", 
"201223539349301007", "201221369349306337", "201301349349204220", 
"201711939349300426", "201531249349201203", "201742409349300119", 
"201741569349300214", "201612049349200006", "201302199349300930", 
"201322939349200402", "201232859349301163", "201730099349300803", 
"201743189349313094", "201632399349300403", "201433179349306953", 
"201711359349311986", "201442819349301309", "201413189349200406", 
"201410459349302341", "201621379349307067", "201630799349300208", 
"201403219349305615", "201613409349200251", "201232159349300708", 
"201613199349311176", "201430989349300223", "201512189349301081", 
"201222239349301652", "201413189349306576", "201211309349200631", 
"201611339349305676", "201620909349300612", "201641099349300009", 
"201442329349200109", "201431189349200933", "201733199349308088", 
"201212409349300316", "201712019349300536", "201111319349300536", 
"201423219349302552", "201130129349200628", "201501349349309215", 
"201242899349200449", "201523209349306897", "201720689349200222", 
"201721309349301707", "201433189349304798", "201711789349200516", 
"201543439349200114", "201341919349300034", "201520209349300617", 
"201743119349302334", "201120219349301002", "201443219349203189", 
"201221639349200212", "201710259349200436", "201821239349200447", 
"201700749349200645", "201732969349301358", "201230769349300038", 
"201323459349300512", "201733219349300148", "201203179349201150", 
"201841019349300849", "201820459349301507", "201302839349301160", 
"201100129349300770", "201200759349200420", "201243489349300624", 
"201300149349200100", "201203199349300940", "201203199349305460", 
"201203209349308360", "201541339349302464", "201333169349301883", 
"201630409349300318", "201112909349200231", "201402599349200610", 
"201630399349200238", "201413149349302661", "201510449349200311", 
"201741039349300739", "201632179349300913", "201730179349301408", 
"201423089349300937", "201103199349305790", "201401199349301100", 
"201801319349304105", "201710519349300701", "201501349349304185", 
"201503249349300225", "201531359349308523", "201310619349300811", 
"201133199349203013", "201201439349300300", "201402349349300205", 
"201422539349200712", "201212219349301406", "201412739349200016", 
"201140269349300904", "201612379349200406", "201720809349200712", 
"201432269349200888", "201343189349204104", "201301359349201430", 
"201211079349300231", "201323169349301452", "201441359349203469", 
"201301339349202655", "201511259349300546", "201621369349300742", 
"201511689349300321", "201702409349300800", "201610149349301501", 
"201243109349300509", "201422169349300707", "201702929349301500", 
"201502819349300425", "201403169349303215", "201502199349300330", 
"201500929349200600", "201401539349300420", "201611369349301251", 
"201600219349300210", "201423219349307592", "201142629349300509", 
"201443219349301964", "201623149349303052", "201402879349300735", 
"201240669349301124", "201212239349200626", "201421279349301807", 
"201711319349303296", "201541209349300409", "201501349349305875", 
"201500819349300250", "201713199349302506", "201232149349300778", 
"201141339349202624", "201733149349303798", "201640679349200434", 
"201341269349301089", "201601339349200625", "201712589349200961", 
"201330079349301473", "201820149349200402", "201612869349200131", 
"201311349349302796", "201113129349300806", "201631399349200023", 
"201601029349200430", "201541349349302504", "201532299349303678", 
"201421349349302872", "201130129349301218", "201213109349301381", 
"201810609349300711", "201322219349200957", "201340459349200339", 
"201103149349302020", "201142439349300509", "201102289349301135", 
"201201329349300840", "201201329349301250", "201620429349302317", 
"201412069349200321", "201521119349300017", "201723549349200822", 
"201330979349300353", "201442669349300134", "201601329349304605", 
"201211359349300806", "201741319349301824", "201622539349301037", 
"201232279349301928", "201412899349300706", "201321759349300132", 
"201510159349300121", "201630439349200643", "201202759349300115", 
"201600259349300755", "201530909349300798", "201710459349301236", 
"201402549349300500", "201223459349300512", "201413379349300711", 
"201702299349200820", "201312919349200401", "201201509349300105", 
"201221669349300307", "201130429349300113", "201312359349300006", 
"201242069349300404", "201520589349300517", "201542299349302259", 
"201543079349300009", "201500129349300405", "201502949349200630", 
"201341349349305444", "201343189349300134", "201731649349300828", 
"201820489349300607", "201202279349200140", "201521349349303252", 
"201431019349300933", "201601079349300205", "201743209349301804", 
"201411129349200706", "201421359349309132", "201433429349200603", 
"201641349349304484", "201732699349300858", "201101339349302395", 
"201531329349301413", "201610849349300601", "201401339349302525", 
"201133199349204058", "201321099349200212", "201113199349302401", 
"201603189349200220", "201720459349201472", "201701309349302030", 
"201711219349300036", "201542299349303604", "201242199349300004", 
"201531599349300348", "201800409349300525", "201431999349200013", 
"201222719349300127", "201603199349200540", "201701819349200005", 
"201321359349302212", "201521269349201292", "201501339349201050", 
"201623209349316812", "201242229349301374", "201413219349304336", 
"201542239349201079", "201732849349300628", "201720989349200307", 
"201121339349304212", "201021693492003210", "201103209349300320", 
"201713199349304226", "201743129349300444", "201611949349300841", 
"201113089349200401", "201403089349301405", "201730139349301268", 
"201742309349200124", "201432279349300708", "201433089349301308", 
"201713189349312521", "201642599349301094", "201113159349300516", 
"201703179349203175", "201713199349300221", "201500209349300840", 
"201721989349301907", "201232569349300328", "201430489349200403", 
"201740129349300304", "201242239349300714", "201632149349201058", 
"201631909349300013", "201540129349300869", "201743109349201099", 
"201241039349300544", "201211229349200621", "201102719349300230", 
"201521019349300312", "201732289349200503", "201402199349300435", 
"201413219349305071", "201422269349202167", "201330709349300433", 
"201403179349306415", "201700969349301305", "201221259349300432", 
"201403219349305245", "201511359349304671", "201232339349300118", 
"201413179349202716", "201113089349301706", "201341229349300519", 
"201640419349300929", "201840469349200639", "201602259349201525", 
"201502469349300800", "201632169349301303", "201543209349310889", 
"201242519349200124", "201313199349305376", "201432949349300428", 
"201410489349301751", "201220749349200432", "201210899349300921", 
"201530339349300078", "201333549349300808", "201231299349301418", 
"201243079349200619", "201710529349301011", "201421359349202827", 
"201532189349301803", "201501389349300005", "201302219349200010", 
"201503179349302340", "201341349349201309", "201800169349300425", 
"201341689349300174", "201025493492000010", "201532029349301203", 
"201220709349200317", "201403169349203100", "201430289349300818", 
"201200979349200420", "201511209349300626", "201503209349304025", 
"201222839349200072", "201212729349200306", "201641319349301474", 
"201233199349303448", "201203199349202300", "201301759349300220", 
"201331939349200203", "201410599349300226", "201230819349300303", 
"201441339349201369", "201623199349201922", "201501279349302025", 
"201321769349300547", "201212279349201636", "201701709349300235", 
"201701889349300420", "201501339349303695", "201731289349201578", 
"201830249349300003", "201521319349201582", "201810529349300201", 
"201330249349300213", "201423079349300117", "201101369349306610", 
"201633289349300603", "201312269349301666", "201213209349300536", 
"201540989349200019", "201221889349200307", "201410499349301591", 
"201401299349201230", "201713199349206881", "201541269349201299", 
"201512999349300006", "201542309349300329", "201610619349300211", 
"201221809349200722", "201612239349300136", "201543009349300939", 
"201631379349302693", "201223319349300322", "201231079349300623", 
"201222789349300112", "201211989349300401", "201131319349302833", 
"201332599349300648", "201413179349306011", "201501359349309295", 
"201143199349303204", "201132279349300603", "201413189349302151", 
"201521669349200302", "201433189349203563", "201531339349200543", 
"201140289349300204", "201413219349311456", "201810439349200511", 
"201820199349300812", "201111809349300901", "201532739349300548", 
"201342269349301579", "201241529349200229", "201341769349300714", 
"201103389349300300", "201631099349300968", "201701359349307915", 
"201623199349312602", "201340529349300209", "201631469349300638", 
"201401289349301135", "201840469349301624", "201430299349300208", 
"201620579349300117", "201511199349300216", "201642279349300509", 
"201603089349302005", "201523209349303112", "201123199349304667", 
"201132769349300768", "201820579349200302", "201313179349303756", 
"201831229349200508", "201233079349200413", "201731379349300503", 
"201713199349305341", "201321199349200442", "201403189349304960", 
"201513569349300301", "201143399349300004", "201631269349300018", 
"201631799349200628", "201612989349301121", "201120299349200602", 
"201501359349308170", "201721879349200227", "201403119349301655", 
"201411349349307611", "201710459349201326", "201723189349202422", 
"201501399349300905", "201401249349200800", "201633199349201663", 
"201722069349200407", "201800729349300930", "201732279349300038", 
"201100749349200525", "201640439349302519", "201330679349300068", 
"201602869349300445", "201613209349201371", "201302219349300725", 
"201743189349307124", "201121589349300672", "201721779349301207", 
"201620209349300012", "201303469349300840", "201531259349300238", 
"201342259349300839", "201622279349200127", "201811169349300946", 
"201121679349200602", "201103189349300315", "201701359349304855", 
"201522879349300147", "201800669349300710", "201330429349300233", 
"201530659349300718", "201841359349313459", "201711809349200016", 
"201413529349300726", "201210989349300601", "201532759349300733", 
"201533579349200113", "201623209349313132", "201531879349200403", 
"201331769349300108", "201820229349200662", "201341029349200419", 
"201601339349302885", "201411299349200336", "201440849349200609", 
"201602959349300700", "201520129349301487", "201200829349300820", 
"201142949349301009", "201320609349300012", "201113189349307431", 
"201641359349200509", "201412809349300651", "201633079349300443", 
"201413219349311546", "201432529349200023", "201231109349300103", 
"201611329349304421", "201432239349201313", "201341889349200004", 
"201122699349300327", "201130429349200903", "201533489349200313", 
"201731259349200508", "201502199349200515", "201322889349201002", 
"201411999349200026", "201332359349200108", "201601349349307835", 
"201520429349300027", "201121339349302332", "201632169349200533", 
"201731079349300713", "201342689349300124", "201122289349201307", 
"201411849349300611", "201513139349302376", "201620439349302547", 
"201620469349300227", "201733149349201493", "201311349349304406", 
"201303199349301805", "201413179349300141", "201730259349300913", 
"201623189349200612", "201202279349303330", "201321339349303577", 
"201623139349300617", "201631379349201448", "201412849349200201", 
"201202269349200880", "201711319349303576", "201603209349200510", 
"201640509349301009", "201540379349300619", "201341779349300244", 
"201241309349200834", "201410289349300211", "201230679349300728", 
"201422319349200102", "201401539349200720", "201222289349302522", 
"201333409349200303", "201701329349305855", "201721429349300112", 
"201411259349300116", "201201249349200005", "201643029349301094", 
"201112209349300501", "201213209349309171", "201402959349300235", 
"201600479349302200", "201421279349200442", "201622359349300622", 
"201321229349301122", "201740139349200839", "201631629349300008", 
"201520559349200507", "201201729349300225", "201601199349200710", 
"201641169349200304", "201532369349200128", "201342259349200999", 
"201222219349300212", "201511039349200416", "201620509349301102", 
"201123159349200412", "201701239349300430", "201222289349302857", 
"201312419349200621", "201432109349200523", "201643199349306409", 
"201203559349200610", "201731809349300748", "201501269349301750", 
"201210749349301031", "201732579349300203", "201820199349300717", 
"201543439349300239", "201701299349300305", "201242409349200509", 
"201422129349300732", "201411329349303016", "201401359349303935", 
"201620959349300132", "201600919349200500", "201420459349200022", 
"201110289349300406", "201731739349200008", "201430739349300118", 
"201611349349304591", "201113189349202486", "201230809349200318", 
"201443219349201019", "201533149349303068", "201640749349201004", 
"201541559349300224", "201521739349200002", "201531149349300118", 
"201303189349301750", "201800159349200115", "201623149349302787", 
"201320339349200002", "201310469349302116", "201421719349200102", 
"201142349349200104", "201101319349201205", "201630639349200723", 
"201611379349300891", "201100609349300600", "201401969349301115", 
"201543109349300329", "201540289349300709", "201722779349301572", 
"201532789349301253", "201612249349301636", "201513029349300631", 
"201610299349301041", "201512249349301371", "201112659349300611", 
"201143229349300419", "201121369349303837", "201313299349300211", 
"201700449349300200", "201402029349300310", "201421219349301232", 
"201612929349200616", "201131369349306733", "201502059349200520", 
"201631389349200773", "201221949349300812", "201732239349200328", 
"201622189349300802", "201723199349310442", "201612729349300801", 
"201640439349302239", "201412209349300206", "201441219349200134", 
"201713179349200536", "201441349349302819", "201143489349300619", 
"201431349349203508", "201701299349202155", "201113159349201701", 
"201601339349200045", "201442279349301219", "201400459349200010", 
"201721599349301202", "201621249349301082", "201332249349301133", 
"201210729349301556", "201123159349201757", "201530159349200418", 
"201711359349307146", "201620749349200712", "201532119349301163", 
"201621329349301942", "201740239349200679", "201521079349200102", 
"201501359349309750", "201532539349300703", "201101319349301615", 
"201402309349301100", "201522299349202322", "201141439349300319", 
"201620989349300312", "201702239349300840", "201311359349307611", 
"201202409349300035", "201411349349201686", "201733199349317643", 
"201703479349300110", "201213209349202376", "201701889349200305", 
"201523349349200202", "201713129349200626", "201613219349200806", 
"201603209349312240", "201310639349300011", "201622219349300422", 
"201113189349307601", "201600449349200205", "201440449349302289", 
"201723139349200917", "201701399349300025", "201721299349302632", 
"201741249349301704", "201702269349302885", "201733179349306733", 
"201322749349200412", "201701329349201525", "201241329349302954", 
"201543099349300229", "201601349349203385", "201101319349200510", 
"201721819349200107", "201222209349300137", "201511009349200011", 
"201520209349200107", "201440469349300404", "201623199349311852", 
"201703149349200820", "201723189349308662", "201113069349300906", 
"201113199349305726", "201600469349302380", "201530449349301183", 
"201712899349201151", "201503209349205315", "201121169349200622", 
"201632219349301808", "201410159349301036", "201610999349300306", 
"201102279349302365", "201322049349200502", "201721319349301662", 
"201222789349300422", "201711299349302601", "201740179349200414", 
"201531849349300103", "201800229349301000", "201541359349301779", 
"201500489349300245", "201612609349200401", "201601819349300420", 
"201602799349300245", "201630299349301313", "201521289349200827", 
"201502579349300220", "201741529349300504", "201133189349202758", 
"201423169349300892", "201211369349305811", "201240839349300704", 
"201341279349301014", "201631389349201058", "201743199349202889", 
"201631329349303013", "201643209349201549", "201500239349200010", 
"201212019349300111", "201341069349300109", "201511069349200206", 
"201630609349300958", "201601349349306500", "201342109349200204", 
"201721609349301007", "201732379349300543", "201641319349301499", 
"201733139349302358", "201733189349204148", "201222899349300827", 
"201210819349300216", "201511359349205056", "201731289349300503", 
"201600409349301505", "201333189349200733", "201601339349306235", 
"201233199349201758", "201143059349301409", "201340079349301014", 
"201231509349300738", "201730889349301113", "201300849349200610", 
"201202699349200410", "201511259349301301", "201642219349200814", 
"201713199349202656", "201621899349300207", "201741359349301319", 
"201213209349308581", "201102079349300815", "201440779349200309", 
"201523499349300757", "201140499349300109", "201502269349300535", 
"201700379349200540", "201541349349202689", "201213209349200301"
), LastUpdated = c("2016-03-21T17:23:53", "2017-03-10T21:54:49", 
"2016-03-21T17:23:53", "2018-06-14T18:24:07.6405411Z", "2018-03-14T23:04:38", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2017-04-10T21:32:14", "2016-03-21T17:23:53", "2017-12-28T23:29:57", 
"2017-02-11T02:39:43", "2018-03-14T23:04:38", "2016-03-21T17:23:53", 
"2016-04-29T13:40:20", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2018-06-14T18:24:07.3795411Z", "2017-12-23T00:14:50", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2017-10-13T18:40:16", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2017-05-10T18:54:12", 
"2016-03-21T17:23:53", "2018-03-14T23:04:38", "2016-03-21T17:23:53", 
"2016-10-12T19:43:10", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2017-12-23T00:14:50", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2018-04-10T21:12:59", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2018-04-10T21:12:59", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2017-09-14T01:02:35", 
"2016-09-09T23:14:27", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2016-04-29T13:40:20", "2016-11-10T02:07:58", "2016-10-12T19:43:10", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2016-04-29T13:40:20", 
"2018-06-14T18:24:08.0335411Z", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2017-12-23T00:14:50", 
"2016-03-21T17:23:53", "2016-09-09T23:14:27", "2016-10-12T19:43:10", 
"2018-05-14T19:36:11", "2017-03-10T21:54:49", "2018-05-14T19:36:11", 
"2016-03-21T17:23:53", "2016-09-09T23:14:27", "2017-10-13T18:40:16", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2017-12-28T23:29:57", "2018-01-18T23:53:03", "2018-01-19T00:05:21", 
"2016-03-21T17:23:53", "2018-01-18T23:53:03", "2017-09-14T01:02:35", 
"2017-09-13T21:58:58", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2016-12-15T17:30:33", "2016-10-12T19:43:10", 
"2016-09-09T23:14:27", "2016-04-29T13:40:20", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2018-01-19T00:05:21", "2016-03-21T17:23:53", "2016-04-29T13:40:20", 
"2016-11-10T02:07:58", "2017-09-13T21:58:58", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2018-04-10T21:12:59", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2016-11-10T02:07:58", "2017-04-10T21:32:14", 
"2016-10-12T19:43:10", "2016-03-21T17:23:53", "2017-12-23T00:14:50", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2018-03-14T23:04:38", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2017-01-11T22:15:15", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2017-10-13T18:40:16", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2016-04-29T13:40:20", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2016-10-12T19:43:10", 
"2016-10-12T19:43:10", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2018-05-14T19:36:11", "2017-10-13T18:40:16", "2016-03-21T17:23:53", 
"2016-09-09T23:14:27", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2018-01-18T21:29:43", "2017-11-15T14:51:02", "2016-03-21T17:23:53", 
"2016-10-12T19:43:10", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2017-09-14T01:02:35", 
"2017-04-10T21:32:14", "2017-10-13T18:40:16", "2016-03-21T17:23:53", 
"2018-03-14T23:04:38", "2016-03-21T17:23:53", "2017-12-23T00:14:50", 
"2016-04-29T13:40:20", "2016-10-12T19:43:10", "2016-04-29T13:40:20", 
"2017-01-11T22:15:15", "2016-03-21T17:23:53", "2016-11-10T02:07:58", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2017-12-28T23:29:57", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2017-10-13T18:40:16", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2017-12-28T23:29:57", "2016-03-21T17:23:53", 
"2017-12-23T00:14:50", "2016-03-21T17:23:53", "2017-10-13T18:40:16", 
"2016-03-21T17:23:53", "2017-03-10T21:54:49", "2016-04-29T13:40:20", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2017-12-23T00:14:50", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2017-04-10T21:32:14", 
"2016-03-21T17:23:53", "2018-06-14T18:24:07.8665411Z", "2018-01-18T21:29:43", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2016-09-09T23:14:27", 
"2016-10-12T19:43:10", "2016-03-21T17:23:53", "2017-12-28T23:29:57", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2016-04-29T13:40:20", 
"2016-03-21T17:23:53", "2017-09-13T21:58:58", "2016-03-21T17:23:53", 
"2017-12-28T23:29:57", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2017-09-13T21:58:58", "2018-01-18T21:29:43", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2016-10-12T19:43:10", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2016-04-29T13:40:20", "2017-10-13T18:40:16", "2017-10-13T18:40:16", 
"2017-09-13T21:58:58", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2018-01-18T21:29:43", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2017-12-28T23:29:57", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2017-10-13T18:40:16", "2016-03-11T01:42:01", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2017-12-23T00:14:50", 
"2016-03-21T17:23:53", "2017-10-13T18:40:16", "2017-10-13T18:40:16", 
"2016-12-15T17:30:33", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2017-07-11T21:14:10", "2018-01-18T23:53:03", 
"2017-01-11T22:15:15", "2016-03-21T17:23:53", "2017-10-13T18:40:16", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2017-01-11T22:15:19.3760064Z", "2016-09-09T23:14:27", "2016-03-21T17:23:53", 
"2017-03-10T21:54:49", "2016-03-21T17:23:53", "2017-04-10T21:32:14", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2016-09-09T23:14:27", 
"2016-09-09T23:14:27", "2016-09-09T23:14:27", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2018-01-18T21:40:07", "2016-03-21T17:23:53", 
"2017-11-15T14:51:02", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2017-09-14T01:02:35", "2017-10-13T18:40:16", 
"2016-03-21T17:23:53", "2017-12-23T00:14:50", "2016-04-29T13:40:20", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2018-01-18T21:29:43", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2016-03-11T01:11:51", 
"2017-09-13T21:58:58", "2018-06-14T18:24:08.0515411Z", "2017-09-14T01:02:35", 
"2017-12-23T00:14:50", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2018-01-19T00:05:21", "2016-03-21T17:23:53", "2018-05-14T19:36:11", 
"2018-04-10T21:12:59", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2016-04-29T13:40:20", "2016-03-21T17:23:53", "2016-09-09T23:14:27", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2016-09-09T23:14:27", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2017-09-14T01:02:35", 
"2016-12-15T17:30:33", "2017-07-11T21:14:10", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2018-06-14T18:24:07.5715411Z", 
"2017-09-13T21:58:58", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2017-01-11T22:15:15", "2017-09-14T01:02:35", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2017-01-11T22:15:19.5350064Z", "2016-03-21T17:23:53", 
"2017-11-15T14:51:02", "2016-04-29T13:40:20", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2017-12-23T00:14:50", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2016-09-09T23:14:27", "2016-04-29T13:40:20", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2017-03-10T21:54:49", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2017-10-13T18:40:16", 
"2016-04-29T13:40:20", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2018-01-18T23:53:03", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2018-01-18T21:29:43", "2016-09-09T23:14:27", "2016-03-21T17:23:53", 
"2016-09-09T23:14:27", "2017-12-28T23:29:57", "2016-03-21T17:23:53", 
"2018-04-10T21:12:59", "2017-02-11T02:39:43", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2017-01-11T22:15:19.2820064Z", "2016-09-09T23:14:27", 
"2016-03-21T17:23:53", "2016-04-29T13:40:20", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2018-04-10T21:12:59", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2016-09-09T23:14:27", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2018-03-14T23:04:38", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2016-10-12T19:43:10", "2016-03-21T17:23:53", 
"2017-10-13T18:40:16", "2017-02-11T02:39:43", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2016-09-09T23:14:27", "2016-03-11T01:11:51", "2016-04-29T13:40:20", 
"2016-03-21T17:23:53", "2017-09-13T21:58:58", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2017-12-23T00:14:50", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2016-04-29T13:40:20", "2016-04-29T13:40:20", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2017-10-13T18:40:16", "2018-04-10T21:12:59", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2016-10-12T19:43:10", "2018-01-19T00:05:21", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2017-01-11T22:15:19.4660064Z", 
"2017-12-23T00:14:50", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2016-09-09T23:14:27", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2017-02-11T02:39:43", 
"2017-09-13T21:58:58", "2017-09-14T01:02:35", "2017-09-14T01:02:35", 
"2016-04-29T13:40:20", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2018-03-14T23:04:38", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2017-02-11T02:39:43", "2017-12-23T00:14:50", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2017-04-10T21:32:14", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2016-04-29T13:40:20", 
"2017-12-28T23:29:57", "2017-09-14T01:02:35", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2018-01-18T23:53:03", 
"2018-03-14T23:04:38", "2016-11-10T02:07:58", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2017-09-13T21:58:58", "2017-12-23T00:14:50", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2018-01-18T21:29:43", 
"2017-02-11T02:39:43", "2016-03-21T17:23:53", "2018-01-18T21:29:43", 
"2018-01-18T21:29:43", "2016-03-21T17:23:53", "2017-12-23T00:14:50", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2017-07-11T21:14:10", 
"2016-03-21T17:23:53", "2016-12-15T17:30:33", "2016-11-10T02:07:58", 
"2016-03-21T17:23:53", "2018-01-19T00:05:21", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2017-12-23T00:14:50", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2017-09-14T01:02:35", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2016-09-09T23:14:27", 
"2018-06-14T18:24:06.9255411Z", "2017-01-11T22:15:18.4310064Z", 
"2016-03-21T17:23:53", "2016-12-15T17:30:33", "2016-04-29T13:40:20", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2017-09-13T21:58:58", "2016-03-21T17:23:53", 
"2016-04-29T13:40:20", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2018-03-14T23:04:38", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2016-10-12T19:43:10", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2017-03-10T21:54:49", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2017-10-13T18:40:16", 
"2017-12-23T00:14:50", "2016-03-21T17:23:53", "2017-10-13T18:40:16", 
"2018-04-10T21:12:59", "2016-03-21T17:23:53", "2018-04-10T21:12:59", 
"2016-03-11T01:11:51", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2017-05-10T18:54:12", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2018-01-18T21:29:43", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2016-04-29T13:40:20", "2016-09-09T23:14:27", 
"2016-03-21T17:23:53", "2017-01-11T22:15:15.7200064Z", "2016-03-21T17:23:53", 
"2017-01-11T22:15:19.4310064Z", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2016-04-29T13:40:20", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2018-06-14T18:24:07.9485411Z", 
"2018-04-10T21:12:59", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2016-10-12T19:43:10", "2017-10-13T18:40:16", 
"2017-04-10T21:32:14", "2016-03-21T17:23:53", "2016-10-12T19:43:10", 
"2016-03-21T17:23:53", "2018-06-14T18:24:07.7925411Z", "2016-03-21T17:23:53", 
"2016-09-09T23:14:27", "2016-03-21T17:23:53", "2017-01-11T22:15:19.6760064Z", 
"2017-03-10T21:54:49", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2018-06-14T18:24:07.8615411Z", "2016-03-21T17:23:53", 
"2018-06-14T18:24:06.9465411Z", "2016-03-21T17:23:53", "2017-10-13T18:40:16", 
"2018-01-18T23:53:03", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2016-10-12T19:43:10", 
"2016-11-10T02:07:58", "2017-02-11T02:39:43", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2017-11-15T14:51:02", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2017-09-13T21:58:58", "2017-12-28T23:29:57", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2017-03-10T21:54:49", 
"2017-12-23T00:14:50", "2018-04-10T21:12:59", "2017-11-15T14:51:02", 
"2016-03-21T17:23:53", "2016-09-09T23:14:27", "2016-03-21T17:23:53", 
"2017-02-11T02:39:43", "2017-02-11T02:39:43", "2016-03-21T17:23:53", 
"2018-01-18T21:29:43", "2016-03-21T17:23:53", "2017-12-23T00:14:50", 
"2016-04-29T13:40:20", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2016-11-10T02:07:58", "2018-06-14T18:24:07.3635411Z", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2017-10-13T18:40:16", 
"2016-03-21T17:23:53", "2018-04-10T21:12:59", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2018-06-14T18:24:07.2525411Z", "2017-12-28T23:29:57", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2016-04-29T13:40:20", "2017-04-10T21:32:14", "2016-04-29T13:40:20", 
"2016-03-21T17:23:53", "2018-06-14T18:24:08.0475411Z", "2016-03-21T17:23:53", 
"2016-10-12T19:43:10", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2017-02-11T02:39:43", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2017-01-11T22:15:19.2820064Z", "2016-03-21T17:23:53", "2017-03-10T21:54:49", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2016-10-12T19:43:10", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2017-10-13T18:40:16", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2016-09-09T23:14:27", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2016-12-15T17:30:33", 
"2017-09-14T01:02:35", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2016-09-09T23:14:27", 
"2016-09-09T23:14:27", "2018-01-18T21:29:43", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2017-09-13T21:58:58", 
"2017-02-11T02:39:43", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2017-03-10T21:54:49", "2016-10-12T19:43:10", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2017-10-13T18:40:16", "2017-02-11T02:39:43", 
"2016-09-09T23:14:27", "2016-04-29T13:40:20", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2017-10-13T18:40:16", "2017-09-14T01:02:35", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2017-02-11T02:39:43", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2016-09-09T23:14:27", "2016-03-21T17:23:53", "2017-01-11T22:15:15", 
"2016-03-21T17:23:53", "2017-09-13T21:58:58", "2016-11-10T02:07:58", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2016-10-12T19:43:10", 
"2016-10-12T19:43:10", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2016-09-09T23:14:27", 
"2016-03-21T17:23:53", "2017-09-14T01:02:35", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2017-05-10T18:54:12", 
"2016-03-21T17:23:53", "2017-12-23T00:14:50", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2017-12-23T00:14:50", "2018-04-10T21:12:59", 
"2016-04-29T13:40:20", "2017-09-14T01:02:35", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2016-10-12T19:43:10", "2016-10-12T19:43:10", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2017-10-13T18:40:16", "2016-03-21T17:23:53", 
"2016-09-09T23:14:27", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2016-04-29T13:40:20", "2016-10-12T19:43:10", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2018-06-14T18:24:07.9635411Z", "2017-03-10T21:54:49", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2016-09-09T23:14:27", 
"2016-09-09T23:14:27", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2016-04-29T13:40:20", "2017-12-23T00:14:50", 
"2016-03-21T17:23:53", "2017-01-11T22:15:18.0080064Z", "2016-03-21T17:23:53", 
"2016-04-29T13:40:20", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2017-09-13T21:58:58", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2017-02-11T02:39:43", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2017-01-11T22:15:19.3670064Z", "2016-03-21T17:23:53", "2017-12-23T00:14:50", 
"2016-12-15T17:30:33", "2018-01-18T21:29:43", "2017-02-11T02:39:43", 
"2016-09-09T23:14:27", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2018-01-18T21:29:43", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2017-09-14T01:02:35", "2016-03-21T17:23:53", 
"2016-10-12T19:43:10", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2017-10-13T18:40:16", "2016-10-12T19:43:10", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2017-10-13T18:40:16", "2016-09-09T23:14:27", "2016-03-21T17:23:53", 
"2016-10-12T19:43:10", "2017-09-13T21:58:58", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2016-04-29T13:40:20", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2016-10-12T19:43:10", "2017-12-23T00:14:50", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2017-12-28T23:29:57", 
"2018-01-18T21:40:07", "2016-03-21T17:23:53", "2017-12-23T00:14:50", 
"2016-03-21T17:23:53", "2018-03-14T23:04:38", "2017-02-11T02:39:43", 
"2017-03-10T21:54:49", "2016-03-21T17:23:53", "2016-12-15T17:30:33", 
"2016-03-21T17:23:53", "2016-09-09T23:14:27", "2016-03-21T17:23:53", 
"2017-12-28T23:29:57", "2017-09-14T01:02:35", "2017-10-13T18:40:16", 
"2017-09-14T01:02:35", "2017-12-28T23:29:57", "2018-01-18T23:53:03", 
"2016-03-21T17:23:53", "2017-10-13T18:40:16", "2016-03-21T17:23:53", 
"2016-04-29T13:40:20", "2016-10-12T19:43:10", "2016-03-21T17:23:53", 
"2017-12-23T00:14:50", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2017-04-10T21:32:14", 
"2018-01-18T21:29:43", "2017-12-28T23:29:57", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2016-09-09T23:14:27", "2016-03-21T17:23:53", 
"2017-12-23T00:14:50", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2017-01-11T22:15:18.7970064Z", "2016-03-21T17:23:53", "2016-10-12T19:43:10", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2017-10-13T18:40:16", 
"2016-03-21T17:23:53", "2017-12-28T23:29:57", "2017-09-13T21:58:58", 
"2016-04-29T13:40:20", "2018-03-14T23:04:38", "2016-04-29T13:40:20", 
"2016-03-21T17:23:53", "2017-02-11T02:39:43", "2016-11-10T02:07:58", 
"2017-02-11T02:39:43", "2016-04-29T13:40:20", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2017-10-13T18:40:16", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2016-09-09T23:14:27", "2018-03-14T23:04:38", 
"2016-09-09T23:14:27", "2017-03-10T21:54:49", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2016-10-12T19:43:10", "2016-09-09T23:14:27", "2016-03-21T17:23:53", 
"2017-10-13T18:40:16", "2017-12-23T00:14:50", "2016-09-09T23:14:27", 
"2017-12-28T23:29:57", "2018-01-18T23:53:03", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2017-10-13T18:40:16", 
"2016-04-29T13:40:20", "2016-03-21T17:23:53", "2016-10-12T19:43:10", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2017-09-14T01:02:35", "2016-03-21T17:23:53", 
"2016-03-11T01:42:01", "2016-03-21T17:23:53", "2017-01-11T22:15:19.6430064Z", 
"2018-01-18T21:29:43", "2016-11-10T02:07:58", "2017-10-13T18:40:16", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2016-03-21T17:23:53", "2016-03-21T17:23:53", "2016-03-21T17:23:53", 
"2017-09-13T21:58:58", "2016-03-21T17:23:53", "2016-03-21T17:23:53"
), TaxYear = c(2014, 2015, 2010, 2017, 2016, 2012, 2014, 2009, 
2015, 2012, 2016, 2015, 2016, 2012, 2014, 2010, 2012, 2014, 2016, 
2016, 2011, 2011, 2012, 2016, 2014, 2013, 2012, 2014, 2015, 2014, 
2016, 2012, 2015, 2011, 2013, 2016, 2014, 2012, 2010, 2010, 2014, 
2010, 2016, 2012, 2011, 2013, 2016, 2013, 2011, 2016, 2014, 2011, 
2011, 2014, 2015, 2015, 2010, 2011, 2014, 2017, 2013, 2012, 2013, 
2010, 2016, 2014, 2014, 2015, 2016, 2015, 2016, 2011, 2014, 2016, 
2011, 2011, 2014, 2016, 2016, 2016, 2014, 2016, 2015, 2015, 2012, 
2010, 2011, 2015, 2015, 2014, 2013, 2013, 2013, 2012, 2012, 2016, 
2012, 2013, 2015, 2015, 2010, 2012, 2016, 2014, 2014, 2015, 2015, 
2015, 2010, 2016, 2010, 2014, 2010, 2016, 2013, 2010, 2015, 2010, 
2011, 2010, 2011, 2016, 2010, 2012, 2013, 2013, 2014, 2013, 2014, 
2013, 2013, 2014, 2012, 2015, 2015, 2012, 2012, 2016, 2015, 2009, 
2014, 2014, 2011, 2010, 2013, 2013, 2012, 2010, 2011, 2016, 2016, 
2011, 2015, 2013, 2011, 2013, 2013, 2011, 2014, 2013, 2015, 2015, 
2016, 2010, 2016, 2012, 2016, 2014, 2015, 2014, 2015, 2013, 2015, 
2014, 2014, 2016, 2012, 2011, 2014, 2013, 2011, 2011, 2016, 2012, 
2013, 2012, 2015, 2012, 2016, 2011, 2016, 2011, 2015, 2014, 2014, 
2011, 2013, 2011, 2012, 2016, 2013, 2012, 2015, 2009, 2016, 2016, 
2012, 2014, 2014, 2015, 2012, 2015, 2010, 2013, 2014, 2011, 2016, 
2013, 2016, 2010, 2010, 2015, 2016, 2014, 2012, 2015, 2013, 2011, 
2010, 2013, 2013, 2016, 2016, 2015, 2013, 2011, 2011, 2012, 2016, 
2010, 2012, 2016, 2013, 2012, 2011, 2011, 2016, 2011, 2011, 2012, 
2015, 2013, 2016, 2016, 2015, 2012, 2012, 2011, 2014, 2016, 2015, 
2013, 2015, 2013, 2013, 2012, 2015, 2014, 2013, 2015, 2011, 2015, 
2012, 2014, 2011, 2013, 2011, 2014, 2014, 2014, 2013, 2013, 2016, 
2011, 2016, 2009, 2013, 2009, 2013, 2011, 2014, 2016, 2016, 2013, 
2016, 2014, 2012, 2013, 2016, 2009, 2013, 2011, 2015, 2016, 2016, 
2016, 2011, 2012, 2016, 2011, 2016, 2016, 2012, 2009, 2010, 2011, 
2011, 2011, 2011, 2011, 2014, 2012, 2014, 2010, 2013, 2014, 2013, 
2013, 2016, 2015, 2015, 2013, 2010, 2012, 2016, 2015, 2013, 2014, 
2013, 2011, 2010, 2011, 2013, 2013, 2011, 2013, 2009, 2015, 2016, 
2013, 2012, 2012, 2010, 2012, 2013, 2011, 2014, 2015, 2014, 2016, 
2014, 2011, 2013, 2016, 2014, 2013, 2014, 2014, 2013, 2014, 2014, 
2013, 2010, 2013, 2015, 2013, 2010, 2011, 2012, 2015, 2014, 2014, 
2013, 2016, 2011, 2010, 2016, 2014, 2012, 2014, 2016, 2011, 2016, 
2015, 2011, 2010, 2015, 2014, 2014, 2014, 2012, 2009, 2011, 2016, 
2012, 2011, 2010, 2010, 2010, 2010, 2011, 2014, 2013, 2014, 2016, 
2012, 2013, 2015, 2011, 2016, 2015, 2011, 2013, 2012, 2013, 2014, 
2011, 2014, 2014, 2015, 2013, 2011, 2011, 2016, 2012, 2011, 2010, 
2009, 2012, 2011, 2014, 2014, 2014, 2013, 2014, 2012, 2012, 2016, 
2016, 2011, 2013, 2013, 2015, 2016, 2013, 2013, 2013, 2015, 2016, 
2009, 2013, 2014, 2013, 2010, 2012, 2010, 2015, 2015, 2016, 2015, 
2014, 2011, 2014, 2016, 2013, 2011, 2015, 2016, 2011, 2014, 2014, 
2014, 2011, 2013, 2014, 2016, 2015, 2009, 2009, 2010, 2016, 2016, 
2015, 2010, 2013, 2015, 2016, 2012, 2013, 2016, 2015, 2010, 2016, 
2016, 2013, 2015, 2011, 2012, 2015, 2010, 2015, 2015, 2013, 2016, 
2010, 2011, 2010, 2013, 2016, 2012, 2013, 2013, 2012, 2013, 2015, 
2010, 2013, 2013, 2011, 2013, 2010, 2011, 2014, 2017, 2015, 2014, 
2015, 2014, 2011, 2012, 2013, 2012, 2011, 2010, 2013, 2012, 2011, 
2011, 2015, 2013, 2013, 2014, 2012, 2014, 2012, 2016, 2012, 2009, 
2014, 2010, 2013, 2012, 2010, 2013, 2014, 2011, 2011, 2015, 2011, 
2011, 2012, 2012, 2012, 2011, 2013, 2015, 2013, 2011, 2011, 2016, 
2016, 2014, 2016, 2016, 2014, 2016, 2011, 2013, 2010, 2015, 2012, 
2011, 2014, 2011, 2012, 2013, 2016, 2013, 2014, 2014, 2015, 2011, 
2015, 2014, 2015, 2011, 2010, 2011, 2011, 2009, 2012, 2013, 2014, 
2010, 2010, 2013, 2014, 2013, 2014, 2009, 2013, 2017, 2016, 2010, 
2014, 2012, 2011, 2012, 2010, 2015, 2016, 2015, 2011, 2015, 2013, 
2016, 2012, 2014, 2014, 2015, 2015, 2014, 2010, 2010, 2017, 2012, 
2016, 2011, 2016, 2016, 2012, 2013, 2014, 2010, 2015, 2015, 2015, 
2010, 2013, 2016, 2013, 2013, 2015, 2016, 2014, 2013, 2015, 2016, 
2016, 2015, 2009, 2014, 2011, 2015, 2015, 2012, 2016, 2010, 2016, 
2014, 2012, 2014, 2011, 2015, 2016, 2010, 2010, 2015, 2014, 2016, 
2011, 2013, 2016, 2015, 2013, 2010, 2014, 2014, 2015, 2014, 2012, 
2017, 2012, 2015, 2013, 2013, 2015, 2013, 2010, 2010, 2012, 2010, 
2015, 2013, 2015, 2013, 2013, 2011, 2015, 2013, 2012, 2010, 2009, 
2014, 2016, 2014, 2012, 2013, 2012, 2014, 2013, 2009, 2015, 2015, 
2012, 2010, 2013, 2014, 2015, 2014, 2016, 2011, 2012, 2013, 2015, 
2015, 2010, 2011, 2015, 2015, 2013, 2011, 2015, 2015, 2015, 2013, 
2012, 2011, 2012, 2010, 2013, 2012, 2011, 2011, 2016, 2016, 2013, 
2011, 2015, 2010, 2011, 2013, 2014, 2013, 2015, 2012, 2015, 2015, 
2014, 2011, 2015, 2015, 2014, 2012, 2011, 2014, 2014, 2010, 2015, 
2011, 2012, 2013, 2015, 2011, 2016, 2013, 2011, 2016, 2016, 2014, 
2016, 2011, 2013, 2012, 2012, 2015, 2015, 2012, 2009, 2015, 2012, 
2014, 2010, 2011, 2013, 2014, 2015, 2014, 2014, 2014, 2012, 2016, 
2015, 2011, 2011, 2013, 2010, 2010, 2015, 2014, 2010, 2013, 2014, 
2013, 2016, 2014, 2015, 2014, 2014, 2014, 2010, 2010, 2010, 2012, 
2015, 2013, 2013, 2015, 2009, 2014, 2015, 2011, 2015, 2015, 2016, 
2015, 2014, 2013, 2013, 2016, 2013, 2010, 2013, 2016, 2010, 2015, 
2012, 2012, 2016, 2015, 2011, 2010, 2010, 2013, 2015, 2014, 2014, 
2015, 2015, 2014, 2013, 2014, 2010, 2012, 2014, 2010, 2015, 2016, 
2012, 2011, 2013, 2016, 2016, 2011, 2016, 2014, 2016, 2015, 2015, 
2012, 2014, 2010, 2014, 2012, 2016, 2015, 2015, 2016, 2015, 2016, 
2012, 2016, 2010, 2014, 2015, 2010, 2016, 2011, 2014, 2013, 2012, 
2015, 2016, 2016, 2010, 2010, 2014, 2013, 2016, 2014, 2010, 2014, 
2012, 2015, 2010, 2012, 2015, 2011, 2016, 2015, 2014, 2016, 2013, 
2013, 2015, 2015, 2015, 2014, 2014, 2014, 2016, 2010, 2013, 2010, 
2011, 2012, 2014, 2016, 2014, 2015, 2013, 2011, 2012, 2014, 2015, 
2014, 2012, 2016, 2016, 2014, 2016, 2016, 2010, 2010, 2014, 2016, 
2014, 2012, 2015, 2011, 2010, 2011, 2011, 2016, 2012, 2011, 2014, 
2015, 2016, 2014, 2016, 2011, 2010, 2012, 2014, 2010, 2014, 2015, 
2014, 2011)), .Names = c("EIN", "TaxPeriod", "DLN", "FormType", 
"URL", "OrganizationName", "SubmittedOn", "ObjectId", "LastUpdated", 
"TaxYear"), row.names = c(1236493L, 1578275L, 9260L, 2073042L, 
2001815L, 524780L, 1116408L, 33502L, 1616254L, 691685L, 1904658L, 
1533475L, 2003072L, 552877L, 1246640L, 41725L, 720561L, 1224198L, 
2068096L, 1876411L, 299533L, 245798L, 696676L, 1745950L, 1206226L, 
908705L, 549593L, 1033911L, 1640544L, 1022949L, 2022730L, 631793L, 
1448312L, 303795L, 969918L, 1889623L, 1063984L, 661438L, 111556L, 
9863L, 1062003L, 173009L, 2039782L, 460814L, 355754L, 977769L, 
2056343L, 808446L, 257528L, 1728223L, 1330534L, 291612L, 304501L, 
1302923L, 1466301L, 1357908L, 196736L, 318182L, 1303108L, 2078772L, 
786588L, 486978L, 984956L, 203017L, 1853010L, 1158981L, 1384046L, 
1414477L, 2071675L, 1578616L, 2064743L, 451469L, 1339025L, 1770646L, 
375554L, 623398L, 1261939L, 1923645L, 1951944L, 1948625L, 1044739L, 
1981601L, 1709105L, 1696973L, 588016L, 63084L, 382321L, 1479176L, 
1373480L, 1351345L, 1118850L, 780255L, 876013L, 726723L, 522433L, 
1942882L, 422770L, 976257L, 1466936L, 1742080L, 167697L, 547412L, 
2044229L, 1193927L, 1286442L, 1468274L, 1613411L, 1363495L, 208669L, 
1834502L, 99906L, 1058724L, 70728L, 2014389L, 658077L, 120900L, 
1528506L, 143122L, 235600L, 206956L, 360172L, 1808286L, 139576L, 
612194L, 1178910L, 1147375L, 1026473L, 846497L, 1294011L, 761034L, 
781700L, 1014057L, 660653L, 1349926L, 1411563L, 556078L, 468615L, 
2039569L, 1763882L, 150085L, 1391345L, 1139841L, 342887L, 99019L, 
873617L, 1003041L, 587067L, 78437L, 232668L, 1960231L, 1808769L, 
454207L, 1421875L, 833015L, 180592L, 881101L, 648073L, 430607L, 
1230908L, 1158870L, 1693577L, 1614547L, 1788123L, 202424L, 2023286L, 
768609L, 1832846L, 1279612L, 1414474L, 1119459L, 1522468L, 1179743L, 
1434013L, 1105948L, 1261022L, 1903566L, 709175L, 243652L, 1102385L, 
653041L, 457414L, 376894L, 1815702L, 577590L, 646953L, 558849L, 
1912198L, 902622L, 1824739L, 191912L, 1750596L, 338519L, 1595793L, 
1307215L, 1268125L, 444275L, 947428L, 306946L, 530163L, 1883555L, 
962101L, 414400L, 1622827L, 21502L, 2075704L, 1951913L, 593472L, 
1014097L, 1352848L, 1436955L, 584919L, 1879382L, 75432L, 957184L, 
1290351L, 338813L, 1671535L, 922234L, 1926107L, 71051L, 133818L, 
1738597L, 1907507L, 1210385L, 671836L, 1415803L, 941558L, 276337L, 
193771L, 931192L, 974238L, 1747691L, 1810799L, 1677997L, 876808L, 
295693L, 278959L, 517909L, 1958463L, 61222L, 515459L, 1918299L, 
748956L, 661011L, 328794L, 182600L, 1793194L, 320536L, 368361L, 
571774L, 1872034L, 1012247L, 1791727L, 1754478L, 1481724L, 526096L, 
569564L, 262172L, 1649885L, 1944426L, 1526539L, 833741L, 1764506L, 
835356L, 850899L, 694867L, 1513301L, 1404671L, 881075L, 1601207L, 
388513L, 1628366L, 707602L, 1168179L, 291942L, 910122L, 248376L, 
1384719L, 1321424L, 1320839L, 754383L, 919208L, 1907083L, 308757L, 
1822721L, 135298L, 656456L, 11497L, 1131365L, 358208L, 1195898L, 
1722010L, 1795647L, 646700L, 1862008L, 1256066L, 506367L, 977649L, 
1935832L, 26230L, 655734L, 367928L, 1653759L, 2079131L, 1710463L, 
1876959L, 372639L, 874856L, 1950531L, 180565L, 2072822L, 2057862L, 
563777L, 10688L, 207810L, 390215L, 625747L, 331218L, 332727L, 
337127L, 1063418L, 550537L, 1333415L, 124843L, 822307L, 1343949L, 
864444L, 990283L, 1701645L, 1487875L, 1664757L, 896516L, 160326L, 
708394L, 2071840L, 1677935L, 1025655L, 1206387L, 1093338L, 449742L, 
143996L, 292311L, 773932L, 902829L, 274348L, 792794L, 154535L, 
1530574L, 1696821L, 801748L, 427890L, 493421L, 212362L, 551377L, 
762933L, 491319L, 1009397L, 1515272L, 1021530L, 1800357L, 1280560L, 
328840L, 818658L, 1855821L, 1203557L, 961308L, 1099287L, 1008130L, 
964627L, 1375216L, 1316632L, 896206L, 172068L, 847914L, 1574599L, 
823032L, 200617L, 341331L, 912934L, 1786768L, 1189021L, 1035984L, 
1000977L, 1981131L, 281129L, 74136L, 1958494L, 1408008L, 474773L, 
1321180L, 1895841L, 438872L, 2034495L, 1564033L, 504226L, 118702L, 
1511647L, 1320429L, 1149456L, 1118705L, 718093L, 10867L, 335572L, 
2030511L, 555006L, 445709L, 125104L, 49504L, 42266L, 217866L, 
250456L, 1332372L, 785389L, 1175880L, 2017297L, 459583L, 785997L, 
1390194L, 256331L, 1780082L, 1586165L, 382214L, 824910L, 611996L, 
974855L, 1331406L, 251037L, 1295662L, 1127214L, 1670911L, 790455L, 
192141L, 851296L, 1836856L, 413024L, 277421L, 369863L, 22645L, 
534238L, 268103L, 1172387L, 1121785L, 1242923L, 978643L, 1289093L, 
509903L, 420037L, 1808277L, 2056372L, 291121L, 1174591L, 758771L, 
1376142L, 1998721L, 871910L, 782427L, 652913L, 1514509L, 1890137L, 
33278L, 1032301L, 1383660L, 771409L, 9429L, 465086L, 160684L, 
1537045L, 1751795L, 1712866L, 1746024L, 1121707L, 261871L, 1017403L, 
2021590L, 775580L, 373458L, 1534689L, 1859354L, 510590L, 1139453L, 
1040293L, 1623275L, 271690L, 651519L, 1100417L, 1896891L, 1712150L, 
31986L, 14692L, 114904L, 1966757L, 2017118L, 1459425L, 161893L, 
836893L, 1654640L, 1835826L, 752447L, 840209L, 1948676L, 1543324L, 
124234L, 1949992L, 1953892L, 979192L, 1888741L, 318709L, 694052L, 
1650421L, 228721L, 1475406L, 1466086L, 974123L, 1903416L, 209742L, 
244548L, 159593L, 1172124L, 1854863L, 815405L, 852512L, 813453L, 
459955L, 864903L, 1742478L, 401939L, 881065L, 1080642L, 295503L, 
862705L, 123751L, 624727L, 1337600L, 2060597L, 1503560L, 1134159L, 
1485603L, 1311809L, 358180L, 660193L, 822516L, 697569L, 366385L, 
380366L, 983970L, 886924L, 242483L, 352390L, 1672920L, 752836L, 
1185199L, 1075884L, 558837L, 1285200L, 513925L, 2009857L, 616826L, 
17414L, 1145380L, 233531L, 864593L, 691176L, 210790L, 1007796L, 
1285071L, 265804L, 254440L, 1433898L, 322694L, 179506L, 501611L, 
491610L, 868951L, 366877L, 937635L, 1568652L, 1010924L, 579935L, 
277358L, 1744580L, 1860620L, 1155877L, 1805720L, 2042102L, 1046019L, 
2036864L, 428937L, 836438L, 73342L, 1646947L, 613800L, 401135L, 
1044644L, 370387L, 694351L, 756331L, 1969166L, 1016798L, 1270173L, 
1192214L, 1355524L, 282706L, 1470482L, 1273079L, 1514112L, 349106L, 
386903L, 405928L, 274910L, 33961L, 587687L, 909127L, 1072015L, 
103974L, 147377L, 644090L, 1127655L, 645882L, 1056096L, 28127L, 
652793L, 2077178L, 2034065L, 132056L, 1107448L, 636179L, 393811L, 
464484L, 193098L, 1371318L, 1774639L, 1616207L, 442346L, 1415514L, 
965410L, 2074189L, 692818L, 1347008L, 1167117L, 1517197L, 1584339L, 
1224801L, 3677L, 174908L, 2075593L, 548930L, 2060963L, 318429L, 
1806974L, 1971641L, 474404L, 876767L, 1259185L, 195453L, 1378706L, 
1458291L, 1539960L, 61159L, 1082378L, 1819201L, 866678L, 777022L, 
1662169L, 1922526L, 1072081L, 746455L, 1596473L, 1866408L, 2033286L, 
1819486L, 23477L, 1343411L, 451682L, 1563805L, 1579098L, 560091L, 
1947678L, 138978L, 1839982L, 1316367L, 873668L, 1166330L, 605273L, 
1446827L, 2067799L, 74621L, 95613L, 1762013L, 1264328L, 2032842L, 
443266L, 1161475L, 2065604L, 1902961L, 910664L, 400755L, 1201665L, 
1309567L, 1626785L, 1185193L, 592392L, 2079060L, 474058L, 1392306L, 
871044L, 732990L, 1534578L, 975366L, 207172L, 170102L, 463361L, 
117058L, 1511630L, 892698L, 1597539L, 653806L, 784558L, 239766L, 
1389053L, 801513L, 619797L, 99505L, 21962L, 1271796L, 1784790L, 
1102332L, 674420L, 783165L, 534895L, 1356089L, 987609L, 31751L, 
1487660L, 1723115L, 607203L, 47110L, 813350L, 1225577L, 1355653L, 
1343384L, 1967380L, 496882L, 419375L, 912511L, 1738082L, 1582893L, 
229843L, 512862L, 1600217L, 1430162L, 889485L, 295076L, 1814460L, 
1595611L, 1400960L, 985602L, 618603L, 246851L, 691434L, 202681L, 
955354L, 957383L, 404881L, 669262L, 1772158L, 1702332L, 747371L, 
242784L, 1533702L, 42795L, 403671L, 859620L, 1336473L, 910944L, 
1526245L, 472983L, 1738583L, 1436029L, 1175528L, 258195L, 1361990L, 
1380686L, 1107294L, 410266L, 370446L, 1055186L, 1342761L, 1120L, 
1718115L, 288614L, 541918L, 921390L, 1633817L, 327562L, 1848254L, 
1157379L, 231560L, 1891564L, 2034066L, 1315082L, 1710312L, 313633L, 
818612L, 719188L, 883409L, 1372555L, 1363026L, 697418L, 27437L, 
1748263L, 703779L, 1387216L, 95196L, 373664L, 657429L, 1250817L, 
1424452L, 1149178L, 1178974L, 1009600L, 544735L, 2077460L, 1605232L, 
435399L, 443014L, 776292L, 156885L, 128029L, 1349267L, 1331197L, 
176171L, 795809L, 1236576L, 979641L, 1886018L, 1202677L, 1499198L, 
1268199L, 1296830L, 1180349L, 99456L, 193328L, 101795L, 665045L, 
1664000L, 807897L, 912911L, 1535391L, 36870L, 1074108L, 1513112L, 
269532L, 1885244L, 1474294L, 1968876L, 1545089L, 1324587L, 887435L, 
741672L, 1908039L, 789428L, 195175L, 919391L, 1712961L, 770L, 
1396691L, 925064L, 693251L, 1790704L, 1380482L, 565011L, 208281L, 
3391L, 1127797L, 1797374L, 1332812L, 1144146L, 1419798L, 1648052L, 
1181070L, 1080759L, 1110938L, 69439L, 880389L, 1104404L, 155298L, 
1437426L, 1871882L, 505465L, 317100L, 754035L, 1926471L, 1939582L, 
310786L, 1836013L, 1282749L, 1996728L, 1595030L, 1626979L, 468018L, 
1474165L, 163747L, 1342824L, 702199L, 1928344L, 1724123L, 1793273L, 
1708619L, 1903487L, 1957677L, 582560L, 1767349L, 222318L, 1305123L, 
1424144L, 178136L, 1834137L, 274077L, 1054785L, 976858L, 934272L, 
1612925L, 1966186L, 1928552L, 118965L, 161733L, 1325122L, 994451L, 
1874051L, 1214834L, 136673L, 1506656L, 881162L, 1380313L, 47386L, 
579970L, 1788861L, 368075L, 1925168L, 1686411L, 1185179L, 2022699L, 
1093675L, 990075L, 1560493L, 1439199L, 1541660L, 1316095L, 1172441L, 
1208699L, 1722331L, 146118L, 829023L, 219617L, 234563L, 478884L, 
1378636L, 2018477L, 1332862L, 1578365L, 982672L, 276813L, 592732L, 
1135363L, 1418804L, 1324951L, 606595L, 1794561L, 1852893L, 1326444L, 
1916981L, 1964421L, 295749L, 207050L, 1170398L, 1787997L, 1294069L, 
421577L, 1400197L, 313051L, 120242L, 623494L, 285371L, 1673139L, 
482511L, 257706L, 1009554L, 1516584L, 1967537L, 1438261L, 1802761L, 
184453L, 83527L, 700586L, 1277144L, 158017L, 1106942L, 1753608L, 
1152627L, 329785L), class = "data.frame")


