options nocenter nodate source;

%let wrds = wrds.wharton.upenn.edu 4016;
options comamid = TCP remote = WRDS;
signon username=reggie09 pw=WtRuP5rT;


libname mydata 'C:\Users\reggie\Dropbox\Research\earnings forecasting\data';

libname rwork slibref=work server=wrds;
/*libname mydata 'C:\Users\Reginald\Dropbox\Research\earnings forecasting\data';*/
/*libname home remote "~" server=wrds;*/
/*libname ibes '/wrds/ibes/sasdata' server=wrds;*/
/*libname crsp '/wrds/crsp/sasdata/a_stock' server=wrds;*/
/*libname compx '/wrds/comp/sasdata/naa' server=wrds;*/

libname crspx '/wrds/crsp/sasdata/a_ccm' server=wrds;
rsubmit;
/*libname home remote "~" ;*/
libname ibes '/wrds/ibes/sasdata' ;
libname crsp '/wrds/crsp/sasdata/a_stock';
libname compx '/wrds/comp/sasdata/naa' ;
libname crspx '/wrds/crsp/sasdata/a_ccm';
endrsubmit;

rsubmit;
endrsubmit;
rsubmit;
%let bdate=01jan1980;        /*start calendar date of fiscal period end*/
%let edate=31dec2013; 
endrsubmit;

rsubmit;
/*Get analyst forecasts from IBES */
proc sort nodupkey data=ibes.idsum out=first
          (keep= oftic ticker cname cusip);
 by cusip;
run;
/*225,369 x 4*/
proc sql;
  create table last as select
  statsum.*, first.oftic, first.cname
  from ibes.statsum_epsus statsum, first
  where statsum.cusip = first.cusip
/*  choose one year ahead forecasts (FY1)*/
  and fpi="1" and fiscalp = "ANN" and fpedats between "&bdate"d and "&edate"d
  order by cusip, statpers;
quit;
/*1,656,320 x 26*/
data ibes;
set last;
where actual ne . and numest>0 and oftic ne "" and usfirm=1;
run;
/*1,550,253 x 26*/

/*Get previous EPS figures*/
proc sql;
create table prior_eps as 
select distinct cusip, actual, anndats_act
from ibes
order by cusip, anndats_act;
quit;
/*146,367 x 3*/

proc expand data=prior_eps out=prior_eps2 method = none; 
  by cusip;
  convert actual = actual_lag1   / transformout=(lag 1); 
  convert actual = actual_lag2   / transformout=(lag 2); 
  convert actual = actual_lag3   / transformout=(lag 3); 
run; 
/*146,367 x 7*/

/*Merge lagged EPS figures back*/
proc sql;
create table ibes2 as
select a.*, b.*
from prior_eps2 as a, ibes as b
where a.cusip = b.cusip and 
a.anndats_act = b.anndats_act and 
a.actual=b.actual and 
a.time>0;
quit;
/*1,395,805 x 30*/

/*Merge with CRSP data*/
/*get permno*/
/*exclude utilities and financial firms*/
proc sql;
create table ibescrsp as 
select a.*, b.siccd, b.permno, (permno>0) as match
from ibes2 as a
left join
crsp.stocknames as b
on a.cusip = b.ncusip
and a.statpers between b.namedt and b.nameenddt
where substr(put(siccd, best4.), 1, 1) not in ('4', '6')
order by cusip, anndats_act;
quit;
run;
/*1,013,057 x 33*/

/*BOTTLENECK*/
proc sql;
  create table ibescrsp2 as
/*  should fyear = year(fpedats)?*/
  select dsf.prc, dsf.shrout, ibescrsp.*, year(ibescrsp.anndats_act) as fyear
  from crsp.dsf, ibescrsp
  where dsf.permno = ibescrsp.permno and dsf.date = ibescrsp.statpers
and not missing(dsf.prc); 
quit;
/*943,216 x 37*/
endrsubmit;

/*might need to go through ccm table first*/
/*Merge in data from Compustat*/
/*BOTTLENECK*/
rsubmit;
proc sql;
	create table ibescrspcomp as
	select funda.SALE, funda.AT, funda.BKVLPS, funda.CSHO, funda.tic, funda.ibcom, 
	funda.re, funda.ebit, funda.lt, funda.gvkey,
	funda.ceq, funda.act, funda.dlc, funda.ap, funda.che, funda.rect, funda.invt, 
	funda.chech, funda.lct, funda.dp, funda.dvt, funda.optdr, 
/*	substr(funda.cusip,1,8) as cusip8,*/
	abs(ibescrsp2.prc*ibescrsp2.shrout) as mktval, ibescrsp2.*
	from compx.funda, ibescrsp2 
	where substr(funda.cusip,1,8) = ibescrsp2.cusip 
		and funda.fyear = ibescrsp2.fyear
		and funda.final='Y' 
		and funda.AT>0 
		and funda.BKVLPS>0
		and funda.tic = ibescrsp2.oftic
	order by cusip, statpers;
quit;
/*432681 x 58*/
endrsubmit;

rsubmit;
proc download data=ibescrspcomp out=work.ibescrspcomp;run;
endrsubmit;

/*signoff;*/

/********************************** DONE WITH DATA EXTRACTION ************************************/

data ibescrspcomp; set mydata.ibescrspcomp;run;
/*432681 x 59*/

data mydata.ibescrspcomp;
set ibescrspcomp;
run;
proc contents data=ibescrspcomp;run;

proc sql;
/*
Take earliest analyst forecast for baseline comparion
Exclude small-cap firms
*/
create table temp as 
select cusip, fyear, min(statpers) as statpers format=date9.
from ibescrspcomp 
group by cusip, fyear;
/*51,256 x 3*/

create table ibescrspcomp2 as select b.*
from temp a
left join ibescrspcomp b
on a.cusip=b.cusip and a.statpers=b.statpers
where b.mktval > 500000
order by cusip, statpers;
quit;
/*17639 x 58*/

data temp; 
set ibescrspcomp2; 
act = max(sum(che, invt, rect), act);
lct = max(lct, sum(dlc,ap));
run;
proc sort data=temp; by cusip;run;
proc expand data=temp out=ibescrspcomp3 method = none; 
  by cusip; 
  convert ceq = ceq_lag1   / transformout=(lag 1);
  convert act = act_lag1   / transformout=(lag 1);
  convert lct = lct_lag1   / transformout=(lag 1);
  convert dp = dp_lag1   / transformout=(lag 1);
  convert at = totalassets_lag1 / transformout=(lag 1);
  convert prc = prc_lag1   / transformout=(lag 1);
  convert ap = ap_lag1 / transformout=(lag 1);
  convert rect = rect_lag1 / transformout=(lag 1);
  convert sale = sale_lag1 / transformout=(lag 1);
run;
/*17639 x 67*/

data ibescrspcomp4;
set ibescrspcomp3;
btm = BKVLPS/abs(PRC);

if dvt ne . then dividends = dvt/csho;
if dvt = . then dividends=0;
dividendpayer=(optdr>0);

roe = ibcom/(.5*(sum(ceq_lag1, ceq)));
chgcurrassets = sum(act, -act_lag1);
chgcurrliabs = sum(lct, -lct_lag1);
chgstdebt = sum(dp, -dp_lag1);
acc = sum(chgcurrassets, -chgcurrliabs, -chech, chgstdebt, -dp)/totalassets_lag1;
chgsale = sum(sale, -sale_lag1);
chgap = sum(ap, -ap_lag1);
chgrect = sum(rect, -rect_lag1);
zscore = sum(1.2*(sum(act,-lct) / at), 
	1.4*(re/ at), 
	3.3*(ebit/at), 
	0.6*(mktval/lt), 
	1.0*(sale/at));

stockret_lag1 = log(prc/prc_lag1);
run;
/*17639 x 80*/

proc expand data=ibescrspcomp4 out=ibescrspcomp5 method = none; 
  by cusip; 
  convert roe = roe_lag1   / transformout=(lag 1);
  convert acc = acc_lag1   / transformout=(lag 1);
  convert dividends = dividends_lag1   / transformout=(lag 1);
  convert dividendpayer = dividendpayer_lag1   / transformout=(lag 1);

	convert chgsale = chgsale_lag1   / transformout=(lag 1);
	convert chgap = chgap_lag1   / transformout=(lag 1);
	convert chgrect = chgrect_lag1   / transformout=(lag 1);
run;
/*17639 x 89*/

data ibescrspcomp6;
set ibescrspcomp5;
/*Get yoy change in stock price*/
stockprcchg = prc/prc_lag1 - 1;
pe = prc/actual;
neg_actual_lag1 = (actual_lag1<0);
epsgrowth = actual/actual_lag1 - 1;
run;
/*17639 x 93*/

proc expand data=ibescrspcomp6 out=ibescrspcomp7 method = none; 
by cusip; 
convert stockprcchg = stockprcchg_lag1   / transformout=(lag 1);
convert epsgrowth = epsgrowth_lag1   / transformout=(lag 1);
/*  pe*/
convert pe = pe_lag1 / transformout=(lag 1);
run;
/*17639 x 126*/

data ibescrspcomp8;
set ibescrspcomp7;
/*Needed to merge macro data*/
mo = month(statpers);
quarter = qtr(statpers)

year = year(statpers);
yearmo = year*100 + mo;
yearq = year*100 + quarter;
run;
/*17639 x 131*/

proc sort data=ibescrspcomp8; by cusip statpers;run;


proc sql;
create table temp as
select * from ibescrspcomp8;

create table firmyears as select distinct(cusip) as cusip, count(cusip) as nyears
from temp
group by cusip
order by nyears;
/*2,325 x 3*/

create table ibescrspcomp9 as 
select a.*, b.nyears
from temp as a
left join
firmyears as b
on a.cusip = b.cusip;
/*17639 x 132*/
quit;

/*
. Drop unneeded variables
. Limit to firms with at least 15 years of earnings history
*/

proc sort data=ibescrspcomp9; by cusip statpers fyear;run;
data ibescrspcomp10;
set ibescrspcomp9;
drop match   
time   
ibcom   
dlc   
chech   
mktval   
curcode   
anntims_act   
re   
ap   
lct   
measure   
curr_act   
ebit   
che   
dp   
shrout   
actdats_act   
csho   
ceq   
rect   
dvt   
/*ticker   */
acttims_act   
lt   
oftic   
act   
invt   
optdr   
estflag   
usfirm ;

/*if nyears > 14;*/

if at > 0 then logtotalassets = log(at); else logtotalassets=0;
if totalassets_lag1 > 0 then logtotalassets_lag1 = log(totalassets_lag1); else logtotalassets_lag1  =0;
if rect_lag1 > 0 then logrect_lag1 = log(rect_lag1); else logrect_lag1 = 0;
logsale_lag1  = log(sale_lag1);
if ap_lag1  > 0 then logap_lag1  = log(ap_lag1  ); else logap_lag1  = 0;
run; 
/*17639 x 76*/

data mydata.ibescrspcomp10;
set ibescrspcomp10;
run;

/*
* merge macro data
*/

data macro;
           infile "C:\Users\reggie\Dropbox\Research\earnings forecasting\data\macro\macro.csv" 
/*		   infile "C:\Users\Reginald\Dropbox\Research\earnings forecasting\data\macro\macro.csv" */
            dlm=',' dsd ;
input year unemployment yearmo PPIACO ppipct yearq GDP_PC1;
RUN;

data fy1; set ibescrspcomp10;run;

proc sort data=fy1; by yearmo yearq; run;
proc sort data=macro; by yearmo yearq; run;

data fy1_macro;
merge fy1 (in=a) macro(in=b);
by yearmo yearq;
if a;
run;
/*17639 x 80*/

proc sort data=fy1_macro; by cusip yearmo; run;
proc expand data=fy1_macro out=fy1_macro_2 method = none; 
  by cusip; 
  convert gdp_pc1 = gdp_pc1_lag1   / transformout=(lag 1);
  convert unemployment = unemployment_lag1   / transformout=(lag 1);
  convert ppipct = ppipct_lag1   / transformout=(lag 1);
run;
/*7757 x 119*/

/*
Add industry classifications
Need to run macro in separate 'ff12 industries.sas' file
*/
data fy1_macro_2;
set mydata.fy1_modeling;
run;
proc contents data=fy1_macro_2;run;
%ind_ff12(fy1_macro_2, temp, siccd, i, ind);

data mydata.fy1_modeling;
set temp;
run;
/*17639 x 84*/

/*proc means print data=fy1_macro;run;*/
/*proc contents data=fy1_macro;run;*/
signoff;
