# Resources

This folder contains some useful background material to facilitate the use of IRS 990 Data and the scripts available in this repository.



### Example 990 Forms

* [990](https://github.com/lecy/Open-Data-for-Nonprofit-Research/blob/master/Resources/Form%20990-PC%202015.pdf)
* [990-EZ](https://github.com/lecy/Open-Data-for-Nonprofit-Research/blob/master/Resources/Form%20990-EZ%202015.pdf)
* [990-PF](https://github.com/lecy/Open-Data-for-Nonprofit-Research/blob/master/Resources/Form%20990-PF%202015.pdf)
* [990-N Postcard](https://github.com/lecy/Open-Data-for-Nonprofit-Research/blob/master/Resources/Information%20Needed%20to%20File%20e-Postcard.pdf)

*These examples represent the 2016 versions of the Form 990.*

You can find previous versions of the 990 forms [HERE](https://apps.irs.gov/app/picklist/list/priorFormPublication.html;jsessionid=mV+XU5sTBU3CpbIhdbcxrw__?resultsPerPage=200&sortColumn=sortOrder&indexOfFirstRow=0&criteria=formNumber&value=990&isDescending=false).



## Liberating the 990 E-Filer Data

For some background on the campaigns to open access to IRS 990 E-Filer data, see these articles and blogs:

* [Liberating 990 Data](http://ssir.org/articles/entry/liberating_990_data): Stanford Social Innovation Review
* [The Nonprofit Data Project Blog](https://www.aspeninstitute.org/programs/program-on-philanthropy-and-social-innovation-psi/nonprofit-data-project-updates/): The Aspen Institute
* [IRS Plans to Begin Releasing Electronically Filed Nonprofit Tax Data](https://philanthropy.com/article/IRS-Plans-to-Begin-Releasing/231265): Chronicle of Philanthropy
* [Mandatory E-Filing: Toward a More Transparent Nonprofit Sector](http://www.urban.org/research/publication/mandatory-e-filing-toward-more-transparent-nonprofit-sector): The Urban Institute
* [Recommendations for Improving the Effectiveness of the 990 Form for Reporting](https://github.com/lecy/Open-Data-for-Nonprofit-Research/blob/master/Resources/IRS%20ACT%20Report%202015.pdf): Advisory Committee on Tax-Exempt and Government Entities (ACT) Report




## Working With 990 Data


Form 990: A Guide for Newcomers to Nonprofit Research [ [LINK](http://blog.boardsource.org/blog/form-990-a-guide-for-newcomers-to-nonprofit-research) ]

A History of the Tax Exempt Sector: An SOI Perspective [ [LINK](https://www.irs.gov/pub/irs-soi/tehistory.pdf) ]

A Guided Tour of the 990 Form by GuideStar [ [LINK](https://learn.guidestar.org/help/highlights-of-irs-form-990) ]

Revised Form 990: The Evolution of Governance and the Nonprofit World [ [LINK](http://www.thetaxadviser.com/issues/2009/aug/revisedform990theevolutionofgovernanceandthenonprofitworld.html) ]

Wikipedia: History of the 990 [ [LINK](https://en.wikipedia.org/wiki/Form_990#History) ]




## Working with XML in R

Do you work in R, but you are new to the XML data format? 

Check out a quick guide: [ [HTML](Quick_Guide_to_XML_in_R.html) ]  [ [PDF](https://github.com/lecy/Open-Data-for-Nonprofit-Research/blob/master/Resources/Quick_Guide_to_XML_in_R.pdf) ]




## External Resources for the AWS Data

Charity Navigator has created an open-source [990 Toolkit](http://990.charitynavigator.org/) that allows you to set up an Amazon EC2 instance and clone the full IRS dataset as a relational database. You can read their press release about the project [here](http://www.charitynavigator.org/index.cfm?bay=content.view&cpid=4669).

Chad Kruse at SmarterGiving has a script to convert 990-PF XML files into a MongoDB database on GitHub [here](https://github.com/smartergiving/irs-990-fetch).

You can find some useful scripts here for running queries directly within the cloud and downloading data as CSV files, for example [this GitHub gist](https://gist.github.com/ryankanno/a5da4c6f1f8e0136db9623ae1903d23d#form-990).

If you are more comfortable in Python, check out Yash Nanavati's [GitHup repo](https://github.com/yashvardhannanavati/IRS990-Data-Analysis).

There are some forums on using the E-Filer data, for example [this reddit forum](https://www.reddit.com/r/aws/comments/4p772f/how_the_heck_do_i_view_the_990_documents_on/).
