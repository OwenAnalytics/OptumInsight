/*
THIS PROGRAM LOOKS FOR ANTIPLATELETS WITHIN 12 MONTHS PRIOR TO INDEX VTE DATE FOR
ELIGIBLE PATIENTS. THE ANTIPLATELETS COME FROM OUTPATIENT PHARMA DATA. 
*/

options symbolgen user="X:\DVT\dm";
/*options symbolgen user=work;*/
options fullstimer msglevel=i ;
proc options group=memory; run;
proc options option=utilloc; run;
proc options option=threads; run;


libname data "C:\Users\mengbing\Box Sync\OptumInsight_DataManagement\data";
libname pharma "X:\DVT\Pharma_vte"; 

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



%macro create_pharm(yr=);
	select strip(put(patid,20.)) as patid, strip(put(pat_planid,19.)) as planid,
		copay, fill_dt format=YYMMDD10., days_sup, quantity, strength, ndc
	from pharma.pharm_dvt_&yr(keep=patid pat_planid copay fill_dt days_sup quantity strength ndc)
	where calculated patid in (select patid from patids) 
%mend;

proc sql;
*** CREATE TABLE WITH INFORMATION ON PRESCRIPTION OVER ALL YEARS ***;
	create table pharm as
	%create_pharm(yr=2007)
	outer union corr
	%create_pharm(yr=2008)
	outer union corr
	%create_pharm(yr=2009)
	outer union corr
	%create_pharm(yr=2010)
	outer union corr
	%create_pharm(yr=2011)
	outer union corr
	%create_pharm(yr=2012)
	outer union corr
	%create_pharm(yr=2013)
	outer union corr
	%create_pharm(yr=2014)
	outer union corr
	%create_pharm(yr=2015)
	outer union corr
	%create_pharm(yr=2016);
quit;


*** MACRO TO READ IN NDC CODES ***;
%macro read_code(outname=,sheet=);
	proc import out = &outname
	    datafile = 'C:\Users\mengbing\Box Sync\OptumInsight_DataManagement\others\All_codes_ICD9_NDC_HCPCS.xlsx'
	    dbms = xlsx
		replace;
	    sheet = &sheet;
	    getnames = yes;
	run;
%mend read_code;

%read_code(outname=antiplatelets, sheet="NDC_Antiplatelets");




proc sql;
*** ADD INDEX VTE DATE ***;
	create table pharm2 as
	select a.*, b.index_dt
	from pharm a left join patids b
	on a.patid = b.patid;

*** FIND ANTIPLATELETS WITHIN 365 DAYS PRIOR TO INDEX VTE ***;
	create table pharm3 as
	select a.*, b.category
	from pharm2 a left join antiplatelets b
	on a.ndc = b.NDC_Antiplatelets
	where a.ndc in (select NDC_Antiplatelets from antiplatelets)
		and (index_dt - 365 <= fill_dt <= index_dt)
	order by patid, fill_dt;
quit;



proc export data=pharm3
	outfile="C:\Users\mengbing\Box Sync\OptumInsight_DataManagement\data\prog9_antiplatelets.xlsx"
	dbms=xlsx replace;
	putnames=yes;
run;

proc datasets library=user nolist kill;
quit; run;

