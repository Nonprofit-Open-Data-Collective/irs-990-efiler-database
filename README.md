*Note, this project is in process of being migrated and extended from https://lecy.github.io/Open-Data-for-Nonprofit-Research/*.

# Organization of the Repository

This repository is organized as follows:



## The Master Concordance File

Most of the work represented in this repository has been done to create the "MASTER-CONCORDANCE.csv" file. This file contains all of the important information needed to make the E-Filer XML files usable.

The Master Concordance file...explanation of importance and use.

First complete public version released Aug. 31, 2017. Fields are being validated and updated through an iterative process of build datasets using the MASTER CONCORDANCE file. It is names Version 0 (MASTER_CONCORDANCE_V0.csv) to indicate that some basic information (like field names) might change during this process. In other words, it is in the beta testing phase. 


## E-Filer Documentation

The EFILER_DOCUMENATION folder contains technical IRS documentation on the architecture and versioning of e-filer forms, including XSD and schema files, and change logs.

This folder also contains scripts for generating useful meta-data related to e-filer forms, xpath directories, and e-filer submission patterns. 



## 990 Background Material

General background material on the IRS 990 Forms and the history of efforts to expand e-filing and release e-filer data in a public format.



## Build Scripts

Scripts in R and Python used for translating AWS XML e-filer returns to a unified relational databases.



# IRS 990 E-Filer Data


The IRS has released all nonprofit 990 tax data that has been e-filed through their online system, approximately 60-65% of all 990-PC and 990-EZ filers. It is available for years 2012 to current years with a small set of returns avaialable for 2010 and 2011. 

The data has been posted as XML files in an [Amazon Web Server (AWS) Cloud Server](https://aws.amazon.com/public-datasets/irs-990/). 

For more details about 990 data and the push to have the IRS make it public see the "990_BACKGROUND" folder. 


## Conversion from XML

In order to support use of this data, we have created this repository as a guide to convert the XML files into a relational database that can be used for commercial or research purposes. This process has been a joint effort by Charity Navigator, Guidestar, The Urban Institute, The Aspen Institute, Boardsource, Syracuse University, Indiana University, Arizona State University, and numerous other actors. 






# E-Filer Databases

The Master Concordance file provides the architecture for a relational database that combines the IRS Form 990-PC (public charity) and IRS 990-EZ forms into a unified database. It currently does not include data from Schedules, nor does it cover the IRS Form 990-PF (private foundations). 

Data is generated using the scripts in the Build folder in this repository and released as a set of relational tables for public use. The database consists of the following tables:

* **Table 1** - Core fields contained on the 990 and 990-EZ forms (600 variables)
* **Table 2** - Board members, managers, and key employees including compensation (12 variables)
* **Table 3** - etc.

The data is available for download from the following sites:

list here...



# Available Data

E-Filing began in 2010, but some data has been filed retroactively. There has been a steady increase in e-filers each year. This table represents the approximate number of returns available in the AWS files (as of Dec 2019):

|      |  2009|   2010|   2011|   2012|   2013|   2014|   2015|   2016|   2017|
|:-----|-----:|------:|------:|------:|------:|------:|------:|------:|------:|
|990   | 33360| 123107| 159539| 179674| 198738| 218614| 233431| 242093| 122730|
|990EZ | 15500|  63253|  82066|  93769| 104538| 116461| 124869| 128950|  87746|
|990PF |  2352|  25275|  34597|  39936|  45897|  53443|  58802|  62606|  48305|


Nonprofits can submit 990 filings throughout the year. The IRS releases new data each month based upon recent returns. You can generate the Index file of all current and available e-filer returns using [this script](https://github.com/Nonprofit-Open-Data-Collective/irs-990-efiler-database/blob/master/Build-Efiler-Index.RMD).

Get the most recent statistics:

```r
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

d <- buildIndex()
table( d$FormType, d$TaxYear )
```

|      |  2009|   2010|   2011|   2012|   2013|   2014|   2015|   2016|   2017|
|:-----|-----:|------:|------:|------:|------:|------:|------:|------:|------:|
|990   | 33360| 123107| 159539| 179674| 198738| 218614| 233431| 242093| 122730|
|990EZ | 15500|  63253|  82066|  93769| 104538| 116461| 124869| 128950|  87746|
|990PF |  2352|  25275|  34597|  39936|  45897|  53443|  58802|  62606|  48305|



# License Info

GNU General Public License v2.0
