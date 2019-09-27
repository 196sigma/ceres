options nocenter nodate source;

%let wrds = wrds.wharton.upenn.edu 4016;
options comamid = TCP remote = WRDS;
signon username=reggie09 pw=WtRuP5rT;

/*libname rwork slibref=work server=wrds;*/
libname mydata 'C:\Users\Reginald\Dropbox\Research\earnings forecasting\data';
libname mydata 'C:\Users\reggie\Dropbox\Research\earnings forecasting\data';
/*libname home remote "~" server=wrds;*/
/*libname ibes '/wrds/ibes/sasdata' server=wrds;*/
/*libname crsp '/wrds/crsp/sasdata/a_stock' server=wrds;*/
/*libname compx '/wrds/comp/sasdata/naa' server=wrds;*/


rsubmit;
/*libname home remote "~" ;*/
libname ibes '/wrds/ibes/sasdata' ;
libname crsp '/wrds/crsp/sasdata/a_stock';
libname compx '/wrds/comp/sasdata/naa' ;
endrsubmit;

PROC IMPORT OUT= WORK.forecasts 
            DATAFILE= "C:\Users\reggie\Dropbox\Research\earnings forecast
ing\data\all_forecasts.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;

rsubmit;
proc upload data=work.forecasts out=forecasts_returns;run;
endrsubmit;

rsubmit;
data forecasts_returns2;
set forecasts_returns;
startdt = statpers;
enddt1_fy1 = intnx('month',startdt,13);
enddt2_fy1 = intnx('month',startdt,14);

enddt1_fy2 = intnx('month',startdt,25);
enddt2_fy2 = intnx('month',startdt,26);

enddt1_fy3 = intnx('month',startdt,37);
enddt2_fy3 = intnx('month',startdt,38);

format startdt date9.;
format enddt1_fy1 date9.;
format enddt2_fy1 date9.;

format enddt1_fy2 date9.;
format enddt2_fy2 date9.;

format enddt1_fy3 date9.;
format enddt2_fy3 date9.;
run;

/*data temp; set forecasts_returns2 (obs=3);run;*/

proc sql;
create table forecasts_returns3 as select a.*, b.PERMNO, b.ticker
/*create table forecasts_returns3 as select a.*, b.**/
from forecasts_returns2 as a
left join crsp.stocknames as b
on a.oftic = b.ticker;
quit;

proc sort data=forecasts_returns3 out=forecasts_returns4 nodupkey; by oftic statpers; run;

proc sql;
create table forecasts_returns5_1_fy1 as
select a.*, b.ret
from forecasts_returns4 as a
left join crsp.dsf as b
on a.permno = b.permno
where b.date <= a.enddt1_fy1 and b.date >= a.startdt
order by permno;

create table forecasts_returns5_2_fy1 as
select a.*, b.ret
from forecasts_returns4 as a
left join crsp.dsf as b
on a.permno = b.permno
where b.date <= a.enddt2_fy1 and b.date >= a.startdt
order by permno;

create table forecasts_returns6_1_fy1 as
select oftic, fyear, statpers, statest1, statest2, statest3, medest1, medest2, 
medest3, diff1, diff2, diff3, startdt, enddt1_fy1, enddt2_fy1, 
PERMNO, (exp(sum(log(1+ret)))-1 ) as bahr1
from forecasts_returns5_1_fy1
group by oftic, fyear, statpers, statest1, statest2, statest3, medest1, medest2, 
medest3, diff1, diff2, diff3, startdt, enddt1_fy1, enddt2_fy1, PERMNO;

create table forecasts_returns6_2_fy1 as
select oftic,  fyear, statpers, statest1, statest2, statest3, medest1, medest2, 
medest3, diff1, diff2, diff3, startdt, enddt1_fy1, enddt2_fy1,
PERMNO, (exp(sum(log(1+ret)))-1 ) as bahr2
from forecasts_returns5_2_fy1
group by oftic,  fyear, statpers, statest1, statest2, statest3, medest1, medest2, 
medest3, diff1, diff2, diff3, startdt, enddt1_fy1, enddt2_fy1, PERMNO;

create table forecasts_returns7_fy1 as
select a.*, b.bahr2
from forecasts_returns6_1_fy1 as a
left join
forecasts_returns6_2_fy1 as b
on a.oftic = b.oftic and a.startdt = b.startdt;

create table forecasts_returns5_1_fy2 as
select a.*, b.ret
from forecasts_returns4 as a
left join crsp.dsf as b
on a.permno = b.permno
where b.date <= a.enddt1_fy2 and b.date >= a.startdt
order by permno;

create table forecasts_returns5_2_fy2 as
select a.*, b.ret
from forecasts_returns4 as a
left join crsp.dsf as b
on a.permno = b.permno
where b.date <= a.enddt2_fy2 and b.date >= a.startdt
order by permno;

create table forecasts_returns6_1_fy2 as
select oftic, fyear, statpers, statest1, statest2, statest3, medest1, medest2, 
medest3, diff1, diff2, diff3, startdt, enddt1_fy2, enddt2_fy2, 
PERMNO, (exp(sum(log(1+ret)))-1 ) as bahr1
from forecasts_returns5_1_fy2
group by oftic, fyear, statpers, statest1, statest2, statest3, medest1, medest2, 
medest3, diff1, diff2, diff3, startdt, enddt1_fy2, enddt2_fy2, PERMNO;

create table forecasts_returns6_2_fy2 as
select oftic,  fyear, statpers, statest1, statest2, statest3, medest1, medest2, 
medest3, diff1, diff2, diff3, startdt, enddt1_fy2, enddt2_fy2,
PERMNO, (exp(sum(log(1+ret)))-1 ) as bahr2
from forecasts_returns5_2_fy2
group by oftic,  fyear, statpers, statest1, statest2, statest3, medest1, medest2, 
medest3, diff1, diff2, diff3, startdt, enddt1_fy2, enddt2_fy2, PERMNO;

create table forecasts_returns7_fy2 as
select a.*, b.bahr2
from forecasts_returns6_1_fy2 as a
left join
forecasts_returns6_2_fy2 as b
on a.oftic = b.oftic and a.startdt = b.startdt;

create table forecasts_returns5_1_fy3 as
select a.*, b.ret
from forecasts_returns4 as a
left join crsp.dsf as b
on a.permno = b.permno
where b.date <= a.enddt1_fy3 and b.date >= a.startdt
order by permno;

create table forecasts_returns5_2_fy3 as
select a.*, b.ret
from forecasts_returns4 as a
left join crsp.dsf as b
on a.permno = b.permno
where b.date <= a.enddt2_fy3 and b.date >= a.startdt
order by permno;

create table forecasts_returns6_1_fy3 as
select oftic, fyear, statpers, statest1, statest2, statest3, medest1, medest2, 
medest3, diff1, diff2, diff3, startdt, enddt1_fy3, enddt2_fy3, 
PERMNO, (exp(sum(log(1+ret)))-1 ) as bahr1
from forecasts_returns5_1_fy3
group by oftic, fyear, statpers, statest1, statest2, statest3, medest1, medest2, 
medest3, diff1, diff2, diff3, startdt, enddt1_fy3, enddt2_fy3, PERMNO;

create table forecasts_returns6_2_fy3 as
select oftic,  fyear, statpers, statest1, statest2, statest3, medest1, medest2, 
medest3, diff1, diff2, diff3, startdt, enddt1_fy3, enddt2_fy3,
PERMNO, (exp(sum(log(1+ret)))-1 ) as bahr2
from forecasts_returns5_2_fy3
group by oftic,  fyear, statpers, statest1, statest2, statest3, medest1, medest2, 
medest3, diff1, diff2, diff3, startdt, enddt1_fy3, enddt2_fy3, PERMNO;

create table forecasts_returns7_fy3 as
select a.*, b.bahr2
from forecasts_returns6_1_fy3 as a
left join
forecasts_returns6_2_fy3 as b
on a.oftic = b.oftic and a.startdt = b.startdt;
quit;

proc download data=forecasts_returns7_fy1 out=work.forecasts_returns_fy1;run;
proc download data=forecasts_returns7_fy2 out=work.forecasts_returns_fy2;run;
proc download data=forecasts_returns7_fy3 out=work.forecasts_returns_fy3;run;
endrsubmit;

proc sort data=forecasts_returns_fy1; by oftic;run;
proc sort data=forecasts_returns_fy3; by oftic;run;
proc sort data=forecasts_returns_fy2; by oftic;run;

data forecasts_returns;
	merge forecasts_returns_fy1 (in=a)
		forecasts_returns_fy2 (in=b)
		forecasts_returns_fy3 (in=c);
	by oftic;
	if a;
run;

PROC EXPORT DATA= WORK.forecasts_returns
            OUTFILE= "C:\Users\Lance\Dropbox\Research\earnings forecasti
ng\data\returns-all.txt" 
            DBMS=TAB REPLACE;
     PUTNAMES=YES;
RUN;
