/*libname mydata 'C:\Users\reggie\Dropbox\Research\earnings forecasting\data';*/
/*libname mydata 'C:\Users\Reginald\Dropbox\Research\earnings forecasting\data';*/
libname mydata 'C:\Users\reggie\Dropbox\Research\earnings forecasting\data';

/*Unemployment*/
PROC IMPORT OUT= WORK.unemployment
            DATAFILE= "C:\Users\reggie\Dropbox\Research\earnings forecasting\data\macro\unemployment2.csv" 
/*			DATAFILE= "C:\Users\Reginald\Dropbox\Research\earnings forecasting\data\macro\unemployment2.csv" */
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;

proc means data=unemployment print;run;

/*convert unemployment rate to decimal from percentage*/
data unemployment2;
set unemployment;
unemployment = unemployment/100;
if month = 'Jan' then mo = 1;
if month = 'Feb' then mo = 2;
if month = 'Mar' then mo = 3;
if month = 'Apr' then mo = 4;
if month = 'May' then mo = 5;
if month = 'Jun' then mo = 6;
if month = 'Jul' then mo = 7;
if month = 'Aug' then mo = 8;
if month = 'Sep' then mo = 9;
if month = 'Oct' then mo = 10;
if month = 'Nov' then mo = 11;
if month = 'Dec' then mo = 12;

yearmo = year*100+mo;
run;

proc means data=unemployment2 print;run;

   /* define titles and footnote */
title1 'US Unemployment rate';
title2 h=4 'Jan 1948 to Jul 2014';

footnote j=l ' Source: FRED';

   /* define symbol characteristics */
symbol1 interpol=join;

   /* generate plot of two variables */
proc gplot data=unemployment2  ;
   plot unemployment*yearmo / haxis=194801 to 201407 by 120
                    vaxis=0 to .15 by .005
                    hminor=3
                    vminor=1
/*                    vref=1000*/
/*                    lvref=2*/
;
run;


/*Inflation (PPI)*/

PROC IMPORT OUT= WORK.inflation
            DATAFILE= "C:\Users\reggie\Dropbox\Research\earnings forecasting\data\macro\ppi.csv" 
/*			DATAFILE= "C:\Users\Reginald\Dropbox\Research\earnings forecasting\data\macro\ppi.csv" */
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;

proc means data=inflation print;run;

data inflation2;
set inflation;

if month = 'Jan' then mo = 1;
if month = 'Feb' then mo = 2;
if month = 'Mar' then mo = 3;
if month = 'Apr' then mo = 4;
if month = 'May' then mo = 5;
if month = 'Jun' then mo = 6;
if month = 'Jul' then mo = 7;
if month = 'Aug' then mo = 8;
if month = 'Sep' then mo = 9;
if month = 'Oct' then mo = 10;
if month = 'Nov' then mo = 11;
if month = 'Dec' then mo = 12;

yearmo = year*100+mo;
run;

proc means data=inflation2 print;run;

proc sort data=unemployment2; by yearmo; run;
/*804*/
proc sort data=inflation2; by yearmo; run;
/*1219*/
proc sql;
create table macro_monthly as select a.*, b.*
from unemployment2 a, inflation2 b
where a.yearmo = b.yearmo;
quit;
/*799*/

data macro_monthly;
	merge unemployment2 (in=a)
	inflation2 (in=b);
	by yearmo;
	if a or b;

	if month = 'Jan' then quarter = 1;
	if month = 'Feb' then quarter = 1;
	if month = 'Mar' then quarter = 1;
	if month = 'Apr' then quarter = 2;
	if month = 'May' then quarter = 2;
	if month = 'Jun' then quarter = 2;
	if month = 'Jul' then quarter = 3;
	if month = 'Aug' then quarter = 3;
	if month = 'Sep' then quarter = 3;
	if month = 'Oct' then quarter = 4;
	if month = 'Nov' then quarter = 4;
	if month = 'Dec' then quarter = 4;

	yearq = year*100+quarter;

run;
/*1224*/
proc means data=macro_monthly print;run;


/*Quarterly dat*/
/*GDP*/
PROC IMPORT OUT= WORK.gdp
            DATAFILE= "C:\Users\reggie\Dropbox\Research\earnings forecasting\data\macro\nominal gdp quarterly yoy chg.csv" 
/*			DATAFILE= "C:\Users\Reginald\Dropbox\Research\earnings forecasting\data\macro\nominal gdp quarterly yoy chg.csv" */
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;
/*270*/

data gdp2;
set gdp;
gdp_pc1 = gdp_pc1/100;
yearq = 100*year+quarter;
run;

proc means data=gdp2 print;run;

data macro_quarterly;
set gdp2;
run;
/*270 x 4*/
proc sort data=macro_monthly; by yearq; run;
proc sort data=macro_quarterly; by yearq; run;
data macro;
merge macro_monthly (in=a) macro_quarterly (in=b);
by yearq;
if a or b;
run;
/*1224 x 11*/

data macro2;
set macro;
if year > 1970;
drop month observation_date mo quarter;
run;

PROC EXPORT DATA= WORK.macro2
            OUTFILE= "C:\Users\reggie\Dropbox\Research\earnings forecasting\data\macro\macro.csv" 
/*			OUTFILE= "C:\Users\Reginald\Dropbox\Research\earnings forecasting\data\macro\macro.csv" */
            DBMS=csv REPLACE;
     PUTNAMES=YES;
RUN;

proc corr data=macro; var gdp_pc1 unemployment ppipct ppiaco;run;
proc means print data=macro;run;
