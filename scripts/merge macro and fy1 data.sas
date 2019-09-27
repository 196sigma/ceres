/*libname mydata 'C:\Users\reggie\Dropbox\Research\earnings forecasting\data';*/
libname mydata 'C:\Users\Reginald\Dropbox\Research\earnings forecasting\data';

data macro;
/*           infile "C:\Users\reggie\Dropbox\Research\earnings forecasting\data\macro\macro.csv" */
		   infile "C:\Users\Reginald\Dropbox\Research\earnings forecasting\data\macro\macro.csv" 
            dlm=',' dsd ;
/*input year	$ month	$ unemployment	$ mo	$ yearmo	$ observation_date	$ quarter	$ PPIACO	$ ppipct	$ yearq	$ GDP_PC1;*/
input year	 month	 unemployment	 mo	 yearmo	 observation_date	 quarter	 PPIACO	 ppipct	 yearq	 GDP_PC1;
RUN;

proc means print data=macro;var unemployment ppipct gdp_pc1; run;

PROC IMPORT OUT= WORK.fy1
/*            DATAFILE= "C:\Users\reggie\Dropbox\Research\earnings forecasting\data\fy1-modeling.txt" */
			DATAFILE= "C:\Users\Reginald\Dropbox\Research\earnings forecasting\data\fy1-modeling.txt" 
            DBMS=TAB REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;

proc contents data=fy1;run;

proc sort data=fy1; by yearmo yearq; run;
proc sort data=macro; by yearmo yearq; run;

data fy1_macro;
merge fy1 (in=a) macro(in=b);
by yearmo yearq;
if a;
run;
/*17,830 x 112*/
proc sort data=fy1_macro; by oftic yearmo; run;
proc expand data=fy1_macro out=fy1_macro_2 method = none; 
  by oftic; 
  convert gdp_pc1 = gdp_pc1_lag1   / transformout=(lag 1);
  convert unemployment = unemployment_lag1   / transformout=(lag 1);
  convert ppipct = ppipct_lag1   / transformout=(lag 1);
run;


PROC EXPORT DATA= WORK.fy1_macro
/*            OUTFILE= "C:\Users\reggie\Dropbox\Research\earnings forecasting\data\ibescrspcomp2.txt" */
			OUTFILE= "C:\Users\Reginald\Dropbox\Research\earnings forecasting\data\fy1-modeling2.txt" 
            DBMS=TAB REPLACE;
     PUTNAMES=YES;
RUN;
