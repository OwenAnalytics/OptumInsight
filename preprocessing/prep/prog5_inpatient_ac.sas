/*
THIS PROGRAM ADDS ADDITIONAL INFORMATION FROM RAW DATA TO THE EXISTING SUBSET DATA SUBSET_AC_ACTIVE_NODUP.CSV
1. ADD INR TEST INFORMATION.

NOTE: THIS PROGRAM STOPS AT READING IN CONFINEMENT INFORMATION BECAUSE PATIENTS WITH AC RECORDS FROM MEDICAL
DATA SETS DO NOT APPEAR IN CONFINEMENT.
*/

/*options symbolgen user="X:\DVT\dm";*/
/*options symbolgen user=work;*/
options fullstimer msglevel=i ;
proc options group=memory; run;
proc options option=utilloc; run;
proc options option=threads; run;


libname data "..\data";
libname med "X:\DVT\Medical_vte";
libname conf "X:\DVT\Inp_vte";


*** READ IN ELIGIBLE PATIENT INFORMATION AND EXTRACT DISTINCT PATIDS ***;
proc sql;
	create table patids as
	select distinct patid, index_dt format=YYMMDD10.
	from data.subset_ac_active_nodup (keep=patid index_dt);
quit;



*** MACRO TO READ IN ICD9, NDC, HCPC CODES ***;
%macro read_code(outname=,sheet=);
	proc import out = &outname
	    datafile = 'C:\Users\mengbing\Box Sync\Optum Insight - Data Management\others\All_codes_ICD9_NDC_HCPCS.xlsx'
	    dbms = xlsx
		replace;
	    sheet = &sheet;
	    getnames = yes;
	run;
%mend read_code;

%read_code(outname=ndc_ac_raw, sheet="NDC_Anti_coagulants");
proc sql noprint;
*** CONVERT NUMERIC NDC CODES INTO CHARACTERS ***;
	create table ndc_ac as
	select strip(put(ndc_ac_codes,11.)) as ndc_ac_code, brand_name, category
	from ndc_ac_raw;

	select quote(strip(ndc_ac_code)) into :ac_codes separated by ', '
	from ndc_ac;
quit;



*** THIS MACRO READS ONLY MED INFORMATION OF PATIENTS APPEARING IN FINAL1 ***;
%macro create_med(yr=);
	select strip(put(patid2,20.)) as patid, fst_dt, ndc 
	from med.med_dvt_&yr(keep=patid fst_dt ndc rename=(patid=patid2)) 
	where (calculated patid in (select patid from patids))
		and (ndc in (select ndc_ac_code from ndc_ac))
%mend;

proc sql;
*** CREATE TABLE WITH INFORMATION ON PRESCRIPTION OVER ALL YEARS ***;
	create table med_ac as
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

	select count(*), count(distinct patid)
		into :numobs_med_ac, :numsubjs_med_ac
	from med_ac;
quit;

%put The number of rows in the med_ac dataset is &numobs_med_ac;
%put The number of patients who had an anti-coagulant from medical 
data sets is &numsubjs_med_ac;


%macro create_conf(yr=);
	select strip(put(patid,20.)) as patid, admit_date format=YYMMDD10. as admit_dt label="Admit Date", 
		diag1, diag2, diag3, diag4, diag5, disch_date format=YYMMDD10. as disch_dt label="Discharge Date"
	from conf.conf_dvt_&yr(keep=patid admit_date diag1-diag5 disch_date)
	where calculated patid in (select distinct strip(patid) from med_ac)
%mend;

proc sql noprint;
*** CREATE TABLE WITH INFORMATION ON PRESCRIPTION OVER ALL YEARS ***;
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

	select count(*), count(distinct patid)
		into :numobs_final, :numsubjs_final
	from confs;
quit;

%put The number of rows in the final dataset is &numobs_final;
%put The number of patients in the final dataset is &numsubjs_final;


