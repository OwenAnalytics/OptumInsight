/****************************************************************************
* Program: prog2_get_confinement.sas			   
*
* Location: ~\Box Sync\OptumInsight_DataManagement\preprocessing\prep
*
* Author: Mengbing Li 
*
* Created: Wednesday, Feb 27, 2018 
*
* Description: This program extracts all records from confinement claims data
* of patients included in analysis.
*
* Revisions: 09/19/2018 - Revise header information 
*****************************************************************************/

/*options symbolgen user="X:\DVT\dm";*/
options symbolgen user=work;
options fullstimer msglevel=i ;
proc options group=memory; run;
proc options option=utilloc; run;
proc options option=threads; run;

libname prep "C:\Users\mengbing\Box Sync\OptumInsight_DataManagement\preprocessing\prep";
libname conf "X:\DVT\Inp_vte";
libname member "X:\DVT\Member_vte";


proc import datafile="C:\Users\mengbing\Box Sync\OptumInsight_DataManagement\preprocessing\subsetting\subset_3_removeNoACPeriod_multipleCancers.xlsx"
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
	select strip(put(patid,20.)) as patid, conf_id, admit_date format=YYMMDD10. as admit_dt label="Admit Date", 
		disch_date format=YYMMDD10. as disch_dt label="Discharge Date", diag1, diag2, diag3, diag4, diag5,
		charge, coins, copay, deduct, los, pos, prov
	from conf.conf_dvt_&yr(keep=patid conf_id admit_date diag1-diag5 disch_date charge coins copay deduct los pos prov)
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
   outfile='C:\Users\mengbing\Box Sync\OptumInsight_DataManagement\preprocessing\prep\prog_2_get_confinement.txt'
   dbms=tab
   replace;
   putnames=yes;
run;


proc datasets library=user nolist kill;
quit; run;
