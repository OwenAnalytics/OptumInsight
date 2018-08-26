/*
THIS PROGRAM ADDS ADDITIONAL INFORMATION FROM RAW DATA TO THE EXISTING SUBSET DATA subset_ac_days_multi_cancers.csv
1. ADD INR TEST INFORMATION.
*/

options symbolgen user="X:\DVT\dm";
/*options symbolgen user=work;*/
options fullstimer msglevel=i ;
proc options group=memory; run;
proc options option=utilloc; run;
proc options option=threads; run;


libname data "C:\Users\mengbing\Box Sync\OptumInsight_DataManagement\data";
libname lab "X:\DVT\Lab_vte";
libname member "X:\DVT\Member_vte";


proc import datafile="C:\Users\mengbing\Box Sync\OptumInsight_DataManagement\data\subset_ac_days_multi_cancers.xlsx"
     out=patinfo
     dbms=xlsx
     replace;
     getnames=yes;
run;



*** EXTRACT DISTINCT PATIDS ***;
proc sql;
	create table patids as
	select distinct patid, index_dt format=YYMMDD10.
	from patinfo (keep=patid index_dt);
quit;

%macro create_lab(yr=);
	select strip(put(patid,20.)) as patid, tst_desc , fst_dt format=YYMMDD10. as lab_dt label="Test Date", rslt_nbr,
			Low_Nrml, Hi_Nrml, Rslt_Unit_Nm
	from lab.lab_dvt_&yr(keep=patid fst_dt tst_desc rslt_nbr low_nrml hi_nrml Rslt_Unit_Nm)
	where calculated patid in (select distinct strip(patid) from patids)
%mend;

proc sql noprint;
*** CREATE TABLE WITH INFORMATION ON PRESCRIPTION OVER ALL YEARS ***;
	create table lab_inr as
	%create_lab(yr=2007)
	outer union corr
	%create_lab(yr=2008)
	outer union corr
	%create_lab(yr=2009)
	outer union corr
	%create_lab(yr=2010)
	outer union corr
	%create_lab(yr=2011)
	outer union corr
	%create_lab(yr=2012)
	outer union corr
	%create_lab(yr=2013)
	outer union corr
	%create_lab(yr=2014)
	outer union corr
	%create_lab(yr=2015)
	outer union corr
	%create_lab(yr=2016);

*** ADD INDEX VTE DATE ***;
	create table lab_inr2 as
	select a.patid, b.index_dt, a.tst_desc, a.lab_dt, a.rslt_nbr, a.low_nrml, a.hi_nrml, a.Rslt_Unit_Nm
	from lab_inr a left join patids b
	on a.patid = b.patid;

*** KEEP ONLY INR TESTS ***;
	create table lab_inr3 as
	select patid, index_dt, tst_desc, lab_dt as inr_dt, rslt_nbr, low_nrml, hi_nrml, Rslt_Unit_Nm
	from lab_inr2
	where ((upcase(strip(tst_desc)) like trim('%PROTHROMBIN TIME%') )
    	or ( upcase(strip(tst_desc)) like trim('%I.N.R%') )
	    or ( upcase(strip(tst_desc)) like trim('%INR%') )
	    or ( upcase(strip(tst_desc)) like trim('%NORMALIZED RATIO%') )
	    or ( upcase(strip(tst_desc)) like trim('%INTR%NORM%RATIO%') ));

	create table lab_inr4 as
	select *
	from lab_inr3
	where index_dt <= inr_dt
	order by patid, inr_dt;
quit;


proc export data=lab_inr4
   outfile='C:\Users\mengbing\Box Sync\OptumInsight_DataManagement\data\prog1_inr_lab.xlsx'
   dbms=xlsx
   replace;
   putnames=yes;
run;


*** KEEP ALL LAB TESTS OF ELIGIBLE PATIENTS ***;
proc sql;
	create table lab_all as
	select * 
	from lab_inr2
	where index_dt <= lab_dt
	order by patid, lab_dt;
quit;

proc export data=lab_all
   outfile='C:\Users\mengbing\Box Sync\OptumInsight_DataManagement\data\prog1_lab_all.txt'
   dbms=tab
   replace;
   putnames=yes;
run;

/*proc datasets library=user nolist kill;*/
/*quit; run;*/
