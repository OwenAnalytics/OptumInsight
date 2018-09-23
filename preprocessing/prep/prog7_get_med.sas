/*
THIS PROGRAM EXTRACTS MEDICAL DATA SETS 2007-2016 OF ELIGIBLE PATIENTS.
THE OUTPUT DATA SET WILL BE FOR FURTHER USE.
*/

options symbolgen user="X:\DVT\dm";
/*options symbolgen user=work;*/
options fullstimer msglevel=i ;
proc options group=memory; run;
proc options option=utilloc; run;
proc options option=threads; run;


libname data "C:\Users\mengbing\Box Sync\OptumInsight_DataManagement\preprocessing\data";
libname medical "X:\DVT\Medical_vte";
libname member "X:\DVT\Member_vte";

proc import datafile="C:\Users\mengbing\Box Sync\OptumInsight_DataManagement\preprocessing\subsetting\subset_3_removeNoACPeriod_multipleCancers.xlsx"
     out=patinfo
     dbms=xlsx
     replace;
     getnames=yes;
run;


*** EXTRACT PATID OF ELIGIBLE PATIENTS ***;
proc sql;
	create table patids as
	select distinct patid, index_dt format=YYMMDD10.
	from patinfo (keep=patid index_dt);
quit;



/*
	1. LOOK INTO MEDICAL DATA SET
*/

%macro create_med(yr=);
	select strip(put(patid2,20.)) as patid, *
	from medical.med_dvt_&yr(keep=patid conf_id copay diag1-diag25 fst_dt lst_dt pos proc_cd prov rename=(patid=patid2)) 
	where (calculated patid in (select distinct patid from patids))
%mend;

proc sql;
*** CREATE TABLE WITH INFORMATION ON PRESCRIPTION OVER ALL YEARS ***;
	create table med(drop=patid2) as
	%create_med(yr=2007)
	outer union corr
	%create_med(yr=2008)
	outer union corr
	%create_med(yr=2009)
	outer union corr
	%create_med(yr=2010)
	outer union corr
	%create_med(yr=2011)
	outer union corr
	%create_med(yr=2012)
	outer union corr
	%create_med(yr=2013)
	outer union corr
	%create_med(yr=2014)
	outer union corr
	%create_med(yr=2015)
	outer union corr
	%create_med(yr=2016);

	create table final as
	select distinct a.*, b.index_dt
	from med as a left join patids as b
	on a.patid = b.patid
	order by patid, fst_dt;
quit;

data data.prog7_get_med;
	set final;
run;

proc export data=final
   outfile='C:\Users\mengbing\Box Sync\OptumInsight_DataManagement\preprocessing\prep\prog7_get_med.txt'
   dbms=tab
   replace;
   putnames=yes;
run;

/*proc datasets library=user nolist kill;*/
/*quit; run;*/

