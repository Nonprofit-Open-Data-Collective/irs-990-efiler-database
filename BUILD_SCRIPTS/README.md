# Build Scripts


Here is some test code to demo the functions in the database build scripts in this repository.



## LOAD REQUIRED PACKAGES

```r
library( xmltools )
library( purrr )
library( xml2 )
library( dplyr )

# build functions
source( "https://raw.githubusercontent.com/Nonprofit-Open-Data-Collective/irs-990-efiler-database/master/BUILD_SCRIPTS/build_efile_database_functions.R" )

```



## EXAMPLE ORGANIZATIONS FROM EACH PERIOD

There were major changes to the e-file forms in 2013, so it's good to test code on periods before and after this point. 

These are randomly selected orgs to demo 990 v 990-EZ fields. 

```r
V_990_2014 <- "https://s3.amazonaws.com/irs-form-990/201543089349301829_public.xml"

V_990_2012 <- "https://s3.amazonaws.com/irs-form-990/201322949349300907_public.xml"

V_990EZ_2014 <- "https://s3.amazonaws.com/irs-form-990/201513089349200226_public.xml"

V_990EZ_2012 <- "https://s3.amazonaws.com/irs-form-990/201313549349200311_public.xml"
```

Read these files into R as XML docs:

```r
### GENERATE ALL XPATHS: V 990 2014
url1 <- V_990_2014
doc1 <- read_xml( url1 )
xml_ns_strip( doc1 )
# doc1 %>% xml_find_all( '//*') %>% xml_path()



### GENERATE ALL XPATHS: V 990 2012
url2 <- V_990_2012
doc2 <- read_xml( url2 )
xml_ns_strip( doc2 )
# doc2 %>% xml_find_all( '//*') %>% xml_path()



### GENERATE ALL XPATHS: V 990EZ 2014
url3 <- V_990EZ_2014
doc3 <- read_xml( url3 )
xml_ns_strip( doc3 )
# doc3 %>% xml_find_all( '//*') %>% xml_path()



### GENERATE ALL XPATHS: V 990EZ 2012
url4 <- V_990EZ_2012
doc4 <- read_xml( url4 )
xml_ns_strip( doc4 )
# doc4 %>% xml_find_all( '//*') %>% xml_path()

### EXAMPLE WITH SCHEDULE N

V_990_w_SN1 <- "https://s3.amazonaws.com/irs-form-990/201512539349301111_public.xml"
V_990_w_SN2 <- "https://s3.amazonaws.com/irs-form-990/201520729349200532_public.xml"

```




## GENERATE DOC META-DATA: 

Nice function to create the list of xpaths for fields in an XML form to better understand the structure.

```r
### GENERATE ALL XPATHS: 
doc <- read_xml( V_990_w_SN1 )
xml_ns_strip( doc )

### GENERATE VARIABLE NAMES INSTEAD OF XPATHS
doc %>% xml_find_all( '//*') %>% xml_name()
```





## CREATING ONE-TO-ONE TABLES (each field occurs once on a return)

This is old code to create dataset with 200 common fields.

```
scrapeXML( doc1, url1 )
scrapeXML( doc2, url2 )
scrapeXML( doc3, url3 )
scrapeXML( doc4, url4 )
```



### Creating a new build script from the concordance file:

The **create_code_chunks()** function uses a table from the Master Concordance file to generate a new build script. 

Note all one-to-one tables on the Master Concordance are numbered table-00. 

```r
partvii.table00 <- partvii[ partvii$rdb_table == "F9-P07-TABLE-00-COMP-OVERVIEW" , ]
names( partvii.table00 ) <- toupper( names( partvii.table00 ) )
create_code_chunks( partvii.table00 )  
```

This was used to create the function buildPartVII(), for example.

**demo build function for table-00 part vii:**

```
buildPartVII( doc1, url1 )
buildPartVII( doc2, url2 )
buildPartVII( doc3, url3 )
buildPartVII( doc4, url4 )
```




## RELATIONAL DATABASE FUNCTIONS

### COMBINE CONCORDANCE TABLES

I have included a few small test sections from the Master Concordance file in the dataset_build_scripts.R file in this repo. After you have sourced the file, the dataframes **schedj** and **partvii** should exist. These contain compensation fields. 

```r
names( schedj ) <- toupper( names( schedj ) )
names( partvii ) <- toupper( names( partvii ) )
concordance <- bind_rows( schedj, partvii )
```

Now you can create some compendation tables, which are one-to-many tables. Each 990 return can contain many entries for a field (board members, salaries of employees, etc). 

```r
# part vii compensation table

find_group_names( partvii, "F9-P07-TABLE-01-DTK-COMPENSATION" )
get_var_map( partvii, table.name="F9-P07-TABLE-01-DTK-COMPENSATION" )

partvii.01.group.names <- find_group_names( partvii, "F9-P07-TABLE-01-DTK-COMPENSATION" )
partvii.01.v.map       <- get_var_map( partvii, table.name="F9-P07-TABLE-01-DTK-COMPENSATION" )

build_rdb_table( doc1, url=V_990_2014, group.names=partvii.01.group.names, v.map=partvii.01.v.map )
build_rdb_table( doc2, url=V_990_2012, group.names=partvii.01.group.names, v.map=partvii.01.v.map )
build_rdb_table( doc3, url=V_990EZ_2014, group.names=partvii.01.group.names, v.map=partvii.01.v.map )
build_rdb_table( doc4, url=V_990EZ_2012, group.names=partvii.01.group.names, v.map=partvii.01.v.map )


# part vii contractors

find_group_names( concordance, "F9-P07-TABLE-02-CONTXR-COMPENSATION" )
get_var_map( concordance, table.name="F9-P07-TABLE-02-CONTXR-COMPENSATION" )

partvii.02.group.names <- find_group_names( concordance, "F9-P07-TABLE-02-CONTXR-COMPENSATION" )
partvii.02.v.map <- get_var_map( concordance, table.name="F9-P07-TABLE-02-CONTXR-COMPENSATION" )

build_rdb_table( doc1, url=V_990_2014, group.names=partvii.02.group.names, v.map=partvii.02.v.map )
build_rdb_table( doc2, url=V_990_2012, group.names=partvii.02.group.names, v.map=partvii.02.v.map )
build_rdb_table( doc3, url=V_990EZ_2014, group.names=partvii.02.group.names, v.map=partvii.02.v.map )
build_rdb_table( doc4, url=V_990EZ_2012, group.names=partvii.02.group.names, v.map=partvii.02.v.map )



# build table 01 for part vii (directors, trustees, and key employees)

list.of.tables <- list()

for( i in 1:100 )
{
   url <- index$URL[i]
   doc <- read_xml( url ) %>% xml_ns_strip()
   list.of.tables[[i]] <- build_rdb_table( doc, url, group.names=partvii.01.group.names, v.map=partvii.01.v.map )
}

# convert list to data frame
dd <- bind_rows( list.of.tables )




# build table 02 for part vii (contractors)

list.of.tables <- list()

for( i in 1:100 )
{
   url <- index$URL[i]
   doc <- read_xml( url ) %>% xml_ns_strip()
   list.of.tables[[i]] <- build_rdb_table( doc, url, group.names=partvii.02.group.names, v.map=partvii.02.v.map )
}

# convert list to data frame
dd <- bind_rows( list.of.tables )

```




# BUILD FULL DATABASE

Please note, this code is still really buggy. I am just posting it as an example.

The challenge is that you will loop through 3 million XML documents on AWS, so there is a chance that you get a bad internet connection or AWS sends an error message for some of the files. This code splits the task into 1,000 parts in case you are interrupted in the middle of the build so that you can re-start from where you left off. 

It takes approximately 1-2 days to loop through all public charity returns for a single filing year. You can run multiple filing years in separate R instances, though. 

We were interested in creating a new build every few months, and saving as CSV files to share. So this code was not designed for speed, and was intentionally kept simple so that more people can use it. 

The process would be greatly simplified, though, if you download all XML files from AWS to a local machine, convert them into a MongoDB database, then re-engineer the build code to query the database directly. 


```r

library( xmltools )
library( purrr )
library( xml2 )
library( dplyr )


# build functions
source( "https://raw.githubusercontent.com/Nonprofit-Open-Data-Collective/irs-990-efiler-database/master/BUILD_SCRIPTS/build_efile_database_functions.R" )

names( schedj )  <- toupper( names( schedj ) )
names( partvii ) <- toupper( names( partvii ) )
concordance      <- bind_rows( schedj, partvii )




### BUILD FULL YEAR

index <- buildIndex()

dir.create( "BUILD" )
setwd( "./BUILD" )



# split full year into 1000 parts

buildYear <- function ( year, index=NULL )
{

	if( is.null(index) ){ index <- buildIndex() }

	dir.create( as.character(year) )
	setwd( paste( "./", year, sep="" ) )

	# create list of failed xml doc urls
	write.table( index[NULL,], file="FAIL_LIST.csv", sep=",", append=F, col.names=T, row.names=F ) 
	
	
	dd <- index[ index$TaxYear == year & index$FormType %in% c("990","990EZ") , ]

	nrow( dd )  

	# build in 100 small increments and save to file

	num.breaks <- round( (nrow(dd)/1000), 0 )

	breaks <- round( seq( from = 0, to = nrow(dd), length.out = num.breaks ), 0 )

	file.name <- paste( year, "-build.txt", sep="" )
	zz <- file( file.name, open = "wt" )
	sink( zz, split=T )
	sink( zz, type = "message", append=TRUE )
	
	print( paste( "There are", length(breaks), "loops." ) )

	redo.these <- 1:num.breaks

	for( i in 1:num.breaks )
	{
	   loop <- formatC( i, width = 3, format = "d", flag = "0" )
	   print( paste( "Loop-", loop, ": ", format(Sys.time(), "%b %d %X"), sep="" ) )
	   d.sub <- dd[ (breaks[i]+1):( breaks[i+1] ) , ]

	   try( {

		buildDatabase( index=d.sub, year=year, form.type=c("990","990EZ") )

		redo.these[i] <- NA

	   } )



	}


	redo.these

	sink(type = "message")

	sink() # close sink
	close(zz)
	file.show( file.name )

	savehistory( file=paste( year, "-build.Rhistory", sep="" ) )


}







buildYear( year=2009, index=index )
setwd( ".." )
getwd()  # should be back to BUILD


buildYear( year=2010, index=index )
buildYear( year=2011, index=index )
buildYear( year=2012, index=index )
buildYear( year=2013, index=index )
buildYear( year=2014, index=index )
buildYear( year=2015, index=index )
buildYear( year=2016, index=index )
buildYear( year=2017, index=index )





#######################
#####
#####  COMBINE FILES
#####
#######################


bind_data <- function( year, table.name, name.out=NULL )
{

   these <- grepl( table.name, dir() ) & grepl( "*.rds", dir() )
   loop.list <- ( dir() )[ these ]

   d <- NULL

   for( j in loop.list )
   {
       d.j <- readRDS( j )

       d <- bind_rows( d, d.j )

   }


   if( is.null(name.out) ){ name.out <- table.name  }
   write.csv( d, paste0( year, "-", name.out, ".csv"), row.names=F )
   saveRDS( d, paste0( year, "-", name.out, ".rds") )

}



year <- 2009

setwd( "C:/Users/jdlecy/Dropbox/00 - Nonprofit Open Data/14 - Revisions to Concordance/DEV/BUILD" )
setwd( paste0( "./", year ) )
dir()

bind_data( year=2009, table.name="-CORE", name.out="EFILE-CORE" )
bind_data( year=2009, table.name="PART-VII-TABLE-00" )
bind_data( year=2009, table.name="PART-VII-TABLE-01" )
bind_data( year=2009, table.name="SCHED-J-TABLE-00" )
bind_data( year=2009, table.name="SCHED-J-TABLE-01" )
bind_data( year=2009, table.name="SCHED-J-TABLE-02" )
bind_data( year=2009, table.name="SCHED-N-TABLE-00" )
bind_data( year=2009, table.name="SCHED-N-TABLE-01" )
bind_data( year=2009, table.name="SCHED-N-TABLE-02" )

setwd( ".." )
getwd()  # should be back to BUILD


```


 


