/*
THIS PROGRAM READS IN ALL CONFINEMENT INFORMATION FOR ELIGIBLE PATIENTS. 
THE OUTPUT DATA SET WILL BE USED IN FURTHER ANALYSIS IN R.
*/

/*options symbolgen user="X:\DVT\dm";*/
options symbolgen user=work;
options fullstimer msglevel=i ;
proc options group=memory; run;
proc options option=utilloc; run;
proc options option=threads; run;


libname data "..\data";
libname conf "X:\DVT\Inp_vte";
libname member "X:\DVT\Member_vte";



proc import datafile="C:\Users\mengbing\Box Sync\OptumInsight_DataManagement\data\subset_ac_days_multi_cancers.xlsx"
     out=patinfo
     dbms=xlsx
     replace;
     getnames=yes;
run;

*** READ IN ELIGIBLE PATIENT INFORMATION AND EXTRACT DISTINCT PATIDS ***;
proc sql;
	create table patids as
	select distinct patid, index_dt format=YYMMDD10.
	from patinfo (keep=patid index_dt);
quit;



%macro create_conf(yr=);
	select strip(put(patid,20.)) as patid, conf_id admit_date format=YYMMDD10. as admit_dt label="Admit Date", 
		disch_date format=YYMMDD10. as disch_dt label="Discharge Date", diag1, diag2, diag3, diag4, diag5,
		charge, coins, copay, deduct, los, pos, prov
	from conf.conf_dvt_&yr(keep=patid admit_date diag1-diag5 disch_date charge coins copay deduct los pos prov)
	where calculated patid in (select distinct strip(patid) from patids)
%mend;

proc sql noprint;
*** READ IN CONFINEMENT INFORMATION FOR ELIGIBLE PATIENTS OVER ALL YEARS ***;
	create table confs as
	%create_conf(yr=2007)
	outer union corr
	%create_conf(yr=2008)
	outer union corr
	%create_conf(yr=2009)
	outer union corr
	%create_conf(yr=2010)
	outer union corr
	%create_conf(yr=2011)
	outer union corr
	%create_conf(yr=2012)
	outer union corr
	%create_conf(yr=2013)
	outer union corr
	%create_conf(yr=2014)
	outer union corr
	%create_conf(yr=2015)
	outer union corr
	%create_conf(yr=2016);

*** ADD INDEX VTE DATE ***;
	create table conf_vte as
	select a.*, b.index_dt
	from confs a left join patids b
	on a.patid = b.patid;
quit;


proc export data=conf_vte
   outfile='C:\Users\mengbing\Box Sync\OptumInsight_DataManagement\data\prog2_conf.xlsx'
   dbms=xlsx
   replace;
   putnames=yes;
run;


proc datasets library=user nolist kill;
quit; run;
