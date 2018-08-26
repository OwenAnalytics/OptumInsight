/*
THIS PROGRAM EXTRACT LAB INFORMATION ON ALL LAB TESTS WITHIN 30 DAYS PRIOR TO INDEX VTE DATE
FOR ELIGIBLE PATIENTS.
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



*** THIS MACRO IS TO READ IN LAB DATA SETS ***;
%macro create_lab(yr=);
	select strip(put(patid,20.)) as patid, tst_desc , fst_dt format=YYMMDD10. as lab_dt label="Lab Test Date", rslt_nbr,
			low_nrml, hi_nrml, rslt_unit_nm
	from lab.lab_dvt_&yr(keep=patid fst_dt tst_desc rslt_nbr low_nrml hi_nrml rslt_unit_nm)
	where (calculated patid in (select distinct strip(patid) from patids))
%mend;


proc sql noprint;
*** CREATE TABLE WITH INFORMATION ON PRESCRIPTION OVER ALL YEARS ***;
	create table labs1 as
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
	create table labs2 as
	select distinct a.patid, b.index_dt, a.tst_desc, a.lab_dt, a.rslt_nbr, a.low_nrml, a.hi_nrml, a.rslt_unit_nm
	from labs1 a left join patids b
	on a.patid = b.patid;

	create table labs3 as
	select *
	from labs2
	where index_dt - 30 <= lab_dt < index_dt
	order by patid, lab_dt;
quit;


	
proc export data = labs3
	outfile="C:\Users\mengbing\Box Sync\OptumInsight_DataManagement\data\prog3_labs.xlsx"
	dbms=xlsx replace;
	sheet="all_labs";
run;
	


/*proc datasets library=user nolist kill; */
/*quit; run;*/

