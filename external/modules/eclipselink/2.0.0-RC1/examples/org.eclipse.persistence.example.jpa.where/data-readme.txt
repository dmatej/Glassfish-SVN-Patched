This folder is where all of the GeoNames data files are expected to exist. To run the database creation 
and population utility you will need to download the following files into this folder from:

http://download.geonames.org/export/dump/

The files required are:

admin1CodesASCII.txt                       15-Apr-2008 04:07  152K  
admin2Codes.txt                            15-Apr-2008 04:07  550K  
countryInfo.txt                            15-Apr-2008 03:20   20K  
featureCodes.txt                           15-Apr-2008 03:20   54K  
iso-languagecodes.txt                      15-Apr-2008 03:20  121K  
timeZones.txt                              15-Apr-2008 04:19  9.9K  

To load data for a particular country you will also need to download that country's zip file and place its 
XX.txt file in this folder as well. 

You will then need to use the 2-letter country code as an argument to the utility in order to load its data.
