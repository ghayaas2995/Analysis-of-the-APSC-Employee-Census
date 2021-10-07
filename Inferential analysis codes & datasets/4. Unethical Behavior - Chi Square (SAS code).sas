
* importing the two encoded datasets;
PROC IMPORT OUT=data_2020_encoded
	DATAFILE='/home/u52182037/DANA 4801 (2nd sem)/Team project/dataset 2020 encoded.csv'
	DBMS=CSV REPLACE;
	GETNAMES=YES;
	DATAROW=2;
RUN;

PROC IMPORT OUT=data_2014_encoded
	DATAFILE='/home/u52182037/DANA 4801 (2nd sem)/Team project/dataset 2014 encoded.csv'
	DBMS=CSV REPLACE;
	GETNAMES=YES;
	DATAROW=2;
RUN;
********************************************************************************************************************************;

* checking the measures of central tendencies;
title 'DATA 2020';
PROC MEANS DATA=data_2020_encoded n nmiss mean median min max skewness kurtosis std mode range;
VAR _numeric_;
RUN;

title 'DATA 2014';
PROC MEANS DATA=data_2014_encoded n nmiss mean median min max skewness kurtosis std mode range;
VAR _numeric_;
RUN;

********************************************************************************************************************************;

title 'DATA 2020'; 
proc freq data=data_2020_encoded order=freq ;
run;

title 'DATA 2014';
proc freq data=data_2014_encoded order=freq ;
run;

********************************************************************************************************************************;

* 6. 58 and 61: if you responded yes to discrimination, were you also subjected to bullying and harassment?;

ods noproctitle;

proc freq data=WORK.DATA_2020_ENCODED;
	tables  ('58. During the last 12 months a'n) 
		*('61. During the last 12 months,'n) / chisq cmh expected norow nocol nocum 
		plots(only)=(freqplot mosaicplot);
run;

********************************************************************************************************************************;

*2.	58 and 63: if you were subjected to discrimination, did you experience corrupt behavious in the agency?

ods noproctitle;

proc freq data=WORK.DATA_2020_ENCODED;
	tables  ('58. During the last 12 months a'n) 
		*('63. Excluding behaviour reporte'n) / chisq cmh expected norow nocol nocum 
		plots(only)=(freqplot mosaicplot);
run;

********************************************************************************************************************************;
















































