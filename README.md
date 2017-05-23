# IRS 990 E-Filer Data


The IRS has released all nonprofit 990 tax data that has been e-filed through their online system, approximately 60-65% of all 990-PC and 990-EZ filers. It is available for years 2012 to current years with a small set of returns avaialable for 2010 and 2011. 

The data has been posted as XML files in an [Amazon Web Server (AWS) Cloud Server](https://aws.amazon.com/public-datasets/irs-990/). 

For more details about 990 data and the push to have the IRS make it public see the "RESOURCES" folder. 


## Conversion from XML

In order to support use of this data, we have created this repository as a guide to convert the XML files into a relational database that can be used for commercial or research purposes. This process has been a joint effort by Charity Navigator, Guidestar, The Urban Institute, The Aspen Institute, Boardsource, Syracuse University, Indiana University, Arizona State University, and numerous other actors. 


## Available Data

E-Filing began in 2010, but some data has been filed retroactively. There has been a steady increase in e-filers each year. This table represents the approximate number of returns available in the AWS files (as of March 2017):

 **FORM** | 2009 |  2010 |  2011  | 2012  | 2013  | 2014 |  2015
-------|-------|--------|-------|-------|------|-------|------- 
**990**  | 33,360 | 123,107 | 159,539 | 179,675 | 198,615 | 215,764 | 73,233
**990EZ** | 15,500 | 63.253 |  82,066 |  93,769 | 104,425  | 114,822  | 60,967
**990PF**  | 2,352 | 25,275  | 34,597  | 39,936 | 45,870  | 52,617  | 34,387


# Organization of the Repository

This repository is organized as follows:

## The Master Concordance

Most of the work represented in this repository has been done to create the "MASTER-CONCORDENCE.csv" file. This file contains all of the important information needed to make the E-Filer XML files usable.

The Master Concordance 

## Documentation

k

## Resources



## Build Scripts


