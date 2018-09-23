/****************************************************************************
* Program: prog15_get_facility.sas			   
*
* Location: ~\Box Sync\OptumInsight_DataManagement\preprocessing\prep
*
* Author: Mengbing Li 
*
* Created: Wednesday 9/19/2018 
*
* Description: This program extracts proc_cd from facility claims data of
* patients included in the final analysis. 
*
* Revisions: 
*****************************************************************************/

options symbolgen user="X:\DVT\dm";
/*options symbolgen user=work;*/
options fullstimer msglevel=i ;
proc options group=memory; run;
proc options option=utilloc; run;
proc options option=threads; run;


libname subset "C:\Users\mengbing\Box Sync\OptumInsight_DataManagement\preprocessing\subsetting";
libname medical "X:\DVT\Medical_vte";
libname facility "X:\DVT\Facility_vte";
libname member "X:\DVT\Member_vte";


*** EXTRACT PATID OF ELIGIBLE PATIENTS ***;
proc import datafile="C:\Users\mengbing\Box Sync\OptumInsight_DataManagement\preprocessing\subsetting\subset_3_removeNoACPeriod_multipleCancers.xlsx"
     out=patinfo
     dbms=xlsx
     replace;
     getnames=yes;
run;

proc sql;
	create table patids as
	select distinct patid, index_dt format=YYMMDD10.
	from patinfo (keep=patid index_dt);
quit;



*** READ IN FACILITY DATA FOR ELIGIBLE PATIENTS ***;
%macro create_fac(yr=);
	select strip(put(patid,20.)) as patid, clmid, clmseq, fst_dt format=YYMMDD10., proc_cd
	from facility.fac_dvt_&yr(keep=patid clmid clmseq fst_dt proc_cd) 
	where (calculated patid in (select patid from patids))
%mend;

proc sql;
*** CREATE TABLE WITH INFORMATION ON PRESCRIPTION OVER ALL YEARS ***;
	create table fac(drop=patid2) as
	%create_fac(yr=2007)
	outer union corr
	%create_fac(yr=2008)
	outer union corr
	%create_fac(yr=2009)
	outer union corr
	%create_fac(yr=2010)
	outer union corr
	%create_fac(yr=2011)
	outer union corr
	%create_fac(yr=2012)
	outer union corr
	%create_fac(yr=2013)
	outer union corr
	%create_fac(yr=2014)
	outer union corr
	%create_fac(yr=2015)
	outer union corr
	%create_fac(yr=2016);
quit;


*** ADD INDEX VTE DATE TO FACILITY DATA ***;
proc sql;
	create table final as
	select fac.*, patids.index_dt
	from fac a left join patids b
	on a.patid = b.patid
	order by patid, fst_dt;
quit;


proc export data=final
   outfile='C:\Users\mengbing\Box Sync\OptumInsight_DataManagement\preprocessing\prep\prog15_get_facility.txt'
   dbms=tab
   replace;
   putnames=yes;
run;

proc datasets library=user nolist kill;
quit; run;

