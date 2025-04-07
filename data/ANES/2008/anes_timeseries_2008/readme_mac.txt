The SPSS, Stata, and SAS code must be changed slightly if you are using a Mac, but it is quite straightforward.  

Create the default directory through the user name, using the same folder names as given in the PC directions 
(file: readme.txt).  NOTE: slashes in the file path must be changed from back- to forward-slashes.
Ex: '/Users/username/anes2008TS/20150519'

Just as with the PC directions, place the unzipped files in the default directory
File paths will look like this example for SPSS: 

  file handle rawdata  /name='/Users/username/anes2008TS/20150519/anes_timeseries_2008_rawdata.txt' LRECL=2753.
  file handle readdata /name='/Users/username/anes2008TS/20150519/anes_timeseries_2008_columns.sps'.
  file handle varlab   /name='/Users/username/anes2008TS/20150519/anes_timeseries_2008_varlabels.sps'.
  file handle codelab  /name='/Users/username/anes2008TS/20150519/anes_timeseries_2008_codelabelsassign.sps'.

  include file=readdata.
  include file=varlab.
  include file=codelab.
  save outfile= '/Users/username/anes2008TS/20150519/anes_timeseries_2008.sav'.
