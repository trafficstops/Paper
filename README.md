Here is the guide to replicate the results of the paper "Weather impact on racial composition and citation activity of traffic stops in the United States." The analysis is divided into a Veil of Darkness (VOD) part (with limited observations) and a model making use of the complete data set but separated by jurisdiction (SEP). 

The data for the VOD analysis is directly available in VODPolicingRegData.RData. For the SEP Analysis, the data needs to be assembled first due to file size limitations associated with GitHub. This can easily be done with an R-Script excuting the following:

1. Create a data frame which concatonates the .RData files listed in the sheet CITYLIST in the Excel file Policing.xlsx. That data frame should be called dfcity. 
2. Create a data frame which concatonates the .RData files listed in the sheet STATEFILES in the Excel file Policing.xlsx. That data frame should be called dfstate. Note that unlike the city data, the state data is separated by jurisdiction and year.  

The folder "Data" contains all the data separated by the two model parts, i.e., VOD and SEP. It also contains an Excel file that links the jurisdiction codes to the city and state names. The files VODPolicingMain and SEPPolicingMain are the files to execute in order to get the main results of the paper. Some of the results are also summarized in the Excel file "Results.xlsx." 


