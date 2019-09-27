-- Reginald Edwards
-- 12 June 2018
-- 4 July 2018
-- PostgresQL
-- run in postgresql via "\include code/get-fundamentals.sql"



-- drop table firm_quarters;
-- drop table long_firm_quarters;
-- drop table long_firms;

drop view v1;
drop table fundq_short;

create view v1 as select * 
from import.comp_fundq 
where indfmt = 'INDL' 
  and datafmt = 'STD'
  and popsrc = 'D'
  and consol = 'C'
  and finalq = 'Y';

-- create table firm_quarters as select gvkey, count(*) as nquarters from v1 group by gvkey;
-- create table long_firm_quarters as select * from firm_quarters where nquarters >= 40;
-- create table long_firms as select a.*, b.nquarters from import.comp_fundq as a, long_firm_quarters as b 
-- where a.gvkey = b.gvkey order by nquarters desc;

-- get relevant ratio variables
create table fundq_short as
select gvkey, fyr, fyearq, datacqtr, datafqtr, datadate, tic, conm,
apq,
ajexq,
atq,
capxy,
cheq,
cshoq,
ceqq,
cogsq,
actq,
lctq,
dlcq,
txdbq,
txditcq,
dpcy,
dpq,
dvpq,
dvpy,
oepsxy,
epspxq,
epsfxq,
epsfiq,
opepsq,
xidoq,
ibq,
ibadjq,
ibcomq,
ibcy,
txtq,
txpq,
xintq,
invtq,
icaptq,
ltq,
dlttq,
niq,
miiq,
mibq,
nopiq,
oancfy,
xoprq,
oiadpq,
oibdpq,
pstkrq,
pstkq,
piq,
prccq,
ppentq,
rectq,
xrdq,
revtq,
saleq,
xsgaq,
spiq,
seqq
from v1
order by gvkey, datacqtr;
-- 1,541,162