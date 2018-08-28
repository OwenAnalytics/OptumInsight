/* 
THIS PROGRAM IMPLEMENTS THE INCLUSION AND EXCLUSION CRITERIA.
/* 1. EXCLUDE PATIENTS WHO NEVER GOT ANTI-COAGULANT PRIOR TO INDEX VTE DATE 
2. FIND THE INDEX CANCER: THE FIRST CANCER OCCURRENCE AMONG ALL CANCER DIAGNOSIS; USING
  MEDICAL DATA AND CONFINEMENT DATA
3. EXCLUDE PATIENTS WHO DO NOT HAVE AN ACTIVE CANCER: NO HCPCS/NDC AFTER THE CANCER 
  DIAGNOSIS AND PRIOR TO INDEX VTE. HCPCS CODES COME FROM MEDICAL. NDC CODES COME 
  FROM MEDICAL AND PHARMA.

ADDITIONAL MANIPULATION:
5. CHECK FOR DUPLICATES: 
  1) IF A PATIENT HAD TWO RECORDS ON THE SAME DATE FOR THE SAME AC, THEN KEEY ONLY
  ONE OF THEM.
  2) IF A PATIENT GOT WARFARIN AND LMWH ON THE SAME DATE, THEN ONLY
  KEEP THE ROW FOR WARFARIN AND REMOVE THE ROW FOR LMWH. OUTPUT THESE TO A NO_DUP 
  DATA SET.
*/;

options symbolgen user="X:\DVT\dm";
/*options symbolgen user=work;*/
options fullstimer msglevel=i ;

libname member "X:\DVT\Member_vte";
libname medical "X:\DVT\Medical_vte";
libname pharma "X:\DVT\Pharma_vte"; 
libname conf "X:\DVT\Inp_vte";
libname data "C:\Users\mengbing\Box Sync\OptumInsight_DataManagement\data";


/*libname subsetting ".";*/
/*libname member "\\mcomm-schaefer-turbo.turbo.storage.umich.edu\mcomm-schaefer-turbo\DVT\Member_vte";*/
/*libname medical "\\mcomm-schaefer-turbo.turbo.storage.umich.edu\mcomm-schaefer-turbo\DVT\Medical_vte";*/
/*libname pharma "\\mcomm-schaefer-turbo.turbo.storage.umich.edu\mcomm-schaefer-turbo\DVT\Pharma_vte";*/

proc options group=memory; run;
proc options option=utilloc; run;
proc options option=threads; run;


%macro create_pharm(yr=);
	select strip(put(patid,20.)) as patid, clmid,
		copay, fill_dt format=YYMMDD10. as fst_dt, days_sup, quantity, strength, ndc, npi
	from pharma.pharm_dvt_&yr(keep=patid clmid copay fill_dt days_sup quantity strength ndc npi)
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

proc sql;
*** READ IN MEMBER DATASET FOR INDEX VTE ***;
	create table member as
	select distinct strip(put(patid,20.)) as patid, index_dt format=YYMMDD10.
	from member.member(keep = patid index_dt);
quit;


/* 
1. EXCLUDE PATIENTS WHO NEVER GOT ANTI-COAGULANT
*/;

*** MACRO TO READ IN ICD9, NDC, HCPC CODES ***;
%macro read_code(outname=,sheet=);
	proc import out = &outname
	    datafile = 'C:\Users\mengbing\Box Sync\OptumInsight_DataManagement\others\All_codes_ICD9_NDC_HCPCS.xlsx'
	    dbms = xlsx
		replace;
	    sheet = &sheet;
	    getnames = yes;
	run;
%mend read_code;

%read_code(outname=ndc_ac_raw, sheet="NDC_Anti_coagulants");

proc sql noprint;
*** READ IN NDC CODES ***;
	create table ndc_ac as
	select strip(ndc_ac_codes) as ndc_ac_code, brand_name, category
	from ndc_ac_raw;

*** KEEP ONLY PATIENTS IN THE MEMBER DATASET ***;
	create table patinfo1 as
	select b.*, a.index_dt
	from member a, pharm b 
	where a.patid = b.patid;

*** SELECT PATIENTS WHO GOT ANTI-COAGULANTS ***;
	create table final1 as 
	select *
	from patinfo1
	where ndc in (select ndc_ac_code from ndc_ac);

	select count(*), count(distinct patid)
		into :numobs_had_ac, :numsubjs_had_ac
	from final1;
quit;

%put The number of patients who had at least one AC ever is &numsubjs_had_ac, and the number of observations is &numobs_had_ac;


proc datasets library=user nolist;
   delete ndc_ac_raw patinfo1 ac_patinfo;
quit; run;






/* 
	2. FIND INDEX CANCER DATE AND TYPE 
*/;

*** THIS MACRO READS ONLY MED INFORMATION OF PATIENTS APPEARING IN FINAL1 ***;
%macro create_med(yr=);
	select strip(put(patid2,20.)) as patid, *
	from medical.med_dvt_&yr(keep=patid diag1-diag25 fst_dt ndc 
		rename=(patid=patid2)) 
	where calculated patid in (select distinct patid from final1)
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
quit;


*** READ IN ICD9 CODES FOR CANCERS ***;
%read_code(outname=icd9_cancer, sheet="ICD9_Cancers");

proc sql noprint;
*** STORE ICD9 CANCER CODES INTO A MACRO VARIABLE ***;
	create table cancer_codes as
	select strip(put(icd9_cancers,3.)) as icd_code, cancer_type as type
	from icd9_cancer;

	select quote(strip(icd_code)) into :cancers separated by ', '
	from cancer_codes;
quit;


*** CHECK WHETHER PATIENTS EVER HAD A CANCER DIAGNOSIS AND 
	CREATE AN OBSERVATION NUMBER FOR EACH ROW FROM MEDICAL DATA ***;
data patinfo2_med;
	set med;
	length cancer_diag $30 cancer_ct 3;
	cancer_ct = 0;
	array diag{25} diag1-diag25;
	do i=1 to 25;
		sub_diag = strip(diag[i]);
		if sub_diag ne "" then do;
			if substr(sub_diag,1,3) in (&cancers) then do;
				cancer_diag=catx(', ', strip(cancer_diag), substr(sub_diag,1,3));
				cancer_ct +1;
			end;
		end;
	end;
	source = "medical";
	drop i diag1-diag25 sub_diag;
run;




%macro create_conf(yr=);
	select strip(put(patid,20.)) as patid, admit_date format=YYMMDD10. as fst_dt label="First Service Date", 
		diag1, diag2, diag3, diag4, diag5
	from conf.conf_dvt_&yr(keep=patid admit_date diag1-diag5)
	where calculated patid in (select distinct patid from final1)
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
quit;


*** CHECK WHETHER PATIENTS EVER HAD A CANCER DIAGNOSIS AND 
	CREATE AN OBSERVATION NUMBER FOR EACH ROW FROM CONFINEMENT DATA ***;
data patinfo2_conf;
	set confs;
	length cancer_diag $30 cancer_ct 3;
	cancer_ct = 0;
	array diag{5} diag1-diag5;
	do i=1 to 5;
		sub_diag = strip(diag[i]);
		if sub_diag ne "" then do;
			if substr(sub_diag,1,3) in (&cancers) then do;
				cancer_diag=catx(', ', strip(cancer_diag), substr(sub_diag,1,3));
				cancer_ct +1;
			end;
		end;
	end;
	source = "confinement";
	drop i diag1-diag5 sub_diag;
run;


*** COMBINE CANCER DIAGNOSES FROM MEDICAL AND CONFINEMENT DATA ***;
proc sql;
	create table patinfo2 as
	select * from patinfo2_med
	outer union corr
	select * from patinfo2_conf
	order by patid, fst_dt;
quit;


proc datasets library=user nolist;
   delete med confs patinfo2_med patinfo2_conf;
quit; run;
%symdel cancers;


proc sql;
*** SELECT PATIENTS WHO EVER HAD A CANCER DIAGNOSIS ***;
	create table patinfo3 (drop=cancer_total) as
	select *, sum(cancer_ct) as cancer_total
	from patinfo2 
	group by patid
	having calculated cancer_total >= 1;
quit;

proc datasets library=user nolist;
   delete patinfo2;
quit; run;

proc sql;
*** SELECT OBSERVATIONS HAVING A CANCER DIAGNOSIS ***;
	create table cancerobs (drop=cancer_ct) as
	select *
	from patinfo3
	where cancer_ct >= 1;

*** IDENTIFY THE INDEX CANCER DATE FOR EACH PATIENT ***;
	create table cancerobs2 as
	select *
	from cancerobs
	group by patid
	having fst_dt = min(fst_dt);
quit;

proc datasets library=user nolist;
   delete cancerobs;
quit; run;


*** REPLACE ICD CODES WITH CANCER TYPES ***;
*** SELECT DISTINCT ROWS OF CANCER DIAGNOSIS OF EACH PATIENT ***;
proc sql;
	create table cancerobs3 as
	select distinct patid, fst_dt as index_cancer_dt label="Index Cancer Date", cancer_diag
	from cancerobs2;
quit;

proc datasets library=user nolist;
   delete cancerobs2;
quit; run;


data cancerobs3_long;	
	set cancerobs3;
	length diag_code $30;
	do i=1 to (count(cancer_diag, ', ')+1);
		diag_code = strip(scan(cancer_diag,i,','));
		output;
	end;
	drop i;
run;

proc sql;
	create table cancerobs4 as 
	select distinct patid, diag_code
	from cancerobs3_long;
quit;

proc datasets library=user nolist;
   delete cancerobs3_long;
quit; run;

*** ADD CANCER TYPES MATCHING CANCER CODES ***;
proc sql;
	create table cancerobs4_type as
	select distinct a.patid, b.type
	from cancerobs4 a left join cancer_codes b
	on strip(a.diag_code) = strip(b.icd_code)
	order by patid;
quit;

proc datasets library=user nolist;
   delete cancerobs4 cancer_codes;
quit; run;

*** TRANSFORM SEMI INTO WIDE FORMAT BASED ON INDEX CANCER TYPES:
	CANCEROBS5 CONTAINS ONLY PATID AND INDEX CANCER TYPES ***;
data cancerobs5;
	length cancer_type $200;
	do until (last.patid);
		set cancerobs4_type;
		by patid;
		cancer_type = catx(', ', strip(cancer_type), type);
	end;
	drop type;
run;

proc datasets library=user nolist;
   delete cancerobs4_type;
quit; run;

proc sql noprint;
*** INCLUDE EXCLUSIVELY INDEX VTE, INDEX CANCER DATE FOR EACH DISTINCT PATIENT ***;
	create table cancerobs6 as
	select distinct a.patid, a.cancer_type, b.index_cancer_dt, c.index_dt
	from cancerobs5 a,
		cancerobs3 (keep = patid index_cancer_dt) b,
		member c
	where a.patid = b.patid = c.patid
		and c.index_dt >= b.index_cancer_dt;

	select count(*), count(distinct patid)
		into :numobs_prior_cancer, :numsubjs_prior_cancer
	from cancerobs6;
quit;

%put The number of patients who had cancer prior to index VTE &numsubjs_prior_cancer, and the number of observations is &numobs_prior_cancer;


proc datasets library=user nolist;
   delete cancerobs5 cancerobs3 member;
quit; run;

/*
EXCLUDE PATIENTS WHOSE CANCERS WERE NOT ACTIVE
*/;

/*proc import datafile="C:\Users\mengbing\Box Sync\OptumInsight_DataManagement\data\check_2_proccd.xlsx"*/
/*     out=proccd*/
/*     dbms=xlsx*/
/*     replace;*/
/*     getnames=yes;*/
/*run;*/


*** SELECT ONLY HCPCS CHEMO PROCEDURES ***;
proc sql;
*** SELECT PATIENTS WHOSE HCPCS CHEMO PROCEDURES WERE PRIOR TO INDEX VTE ***;
	create table hcpcs_chemo as
	select a.patid, a.cancer_type, a.index_cancer_dt, a.index_dt, b.fst_dt
	from cancerobs6 a, data.check_2_proccd b 
	where a.patid = b.patid and b.procedure = "chemo"
		and a.index_cancer_dt <= b.fst_dt <= a.index_dt;
quit;



*** SELECT PATIENTS WHOSE NDC CHEMO DRUGS WERE PRIOR TO INDEX VTE ***;
%read_code(outname=ndc_chemo, sheet="NDC_Chemo_Drugs");

proc sql noprint;
*** READ IN NDC CODES ***;
	create table chemo_codes as
	select strip(ndc_chemo) as ndc
	from ndc_chemo;
quit;

proc sql noprint;
*** MERGE INFORMATION ON CHEMO DRUG FROM MED AND PHARM ***;
	create table ctinfo as
	select patid, fst_dt, strip(ndc) as ndc
	from patinfo3 (keep = patid ndc fst_dt)
	where patid in (select distinct patid from cancerobs6)
	outer union corr
	select patid, fst_dt, strip(ndc) as ndc
	from pharm (keep = patid ndc fst_dt)
	where patid in (select distinct patid from cancerobs6);

***	ADD INDEX VTE AND INDEX CANCER DATE TO CTINFO ***;
	create table ctinfo2 as
	select a.*, b.index_dt, b.index_cancer_dt, b.cancer_type
	from ctinfo a, cancerobs6 b
	where a.patid = b.patid
		and b.index_cancer_dt <= a.fst_dt <= b.index_dt
		and ndc in (select ndc from chemo_codes);
quit;


*** KEEP PATIENTS THAT HAVE EITHER HCPCS CHEMO CODES OR NDC CHEMO CODES ***;
proc sql noprint;
	create table patinfo4 as
	select *
	from cancerobs6
	where patid in (select distinct patid from hcpcs_chemo)
		or patid in (select distinct patid from ctinfo2);

	select count(*), count(distinct patid)
		into :numobs_active_cancer, :numsubjs_active_cancer
	from patinfo4;
quit;

%put Among those who had at least one record of AC, the number of patients who had an active
	 cancer is &numsubjs_active_cancer, and the number of rows is &numobs_active_cancer;

proc datasets library=user nolist;
   delete hcpcs_chemo ndc_chemo ctinfo2 cancerobs6;
quit; run;


*** MERGE THE DATA SETS RESULTING FROM THE TWO STEPS ***;
proc sql noprint;
	create table final2 as
	select distinct patid, index_cancer_dt, cancer_type
	from patinfo4;

*** COMBINE INFORMATION FROM FINAL1 AND FINAL 2 AND KEEP ONLY PATIENTS APPEARING IN BOTH DATASETS ***;
	create table semifinal as
	select a.*, b.index_cancer_dt, b.cancer_type
	from final1 a, final2 b
	where a.patid = b.patid;

*** READ IN ANTICOAGULANT CODES ***;
	create table ac as 
	select *
	from ndc_ac;

*** ADD AC TYPE, REORDER COLUMNS AND LABEL VARIABLES IN THE FINAL DATASET ***;
	create table final_withdup as
	select a.patid, a.clmid, a.fst_dt as fill_dt, b.category label="AC Category", b.brand_name label"AC Brand Name",
		copay, a.index_dt label="Index VTE Date", a.index_cancer_dt,
		a.cancer_type label="Index Cancer Types", days_sup, quantity, strength, npi
	from semifinal a left join ac b
	on a.ndc = b.ndc_ac_code
	order by patid, fill_dt, category;

	select count(*), count(distinct patid)
		into :numobs_final_dup, :numsubjs_final_dup
	from final_withdup;
quit;

%put The number of patients in the final dataset with possible multiple claimIDs is &numsubjs_final_dup, 
and the number of rows is &numobs_final_dup;


proc export data=final_withdup
   outfile='C:\Users\mengbing\Box Sync\OptumInsight_DataManagement\data\subset_ac_active_withclm.xlsx'
   dbms=xlsx
   replace;
   putnames=yes;
run;



/* 
	5. MERGE ROWS WITH THE SAME AC ON THE SAME DATE BUT ONLY DIFFER IN PLANID 
*/;
proc sql noprint;
	create table final as
	select distinct patid, clmid, fill_dt, category, brand_name, copay, index_dt, index_cancer_dt,
		cancer_type, days_sup, quantity, strength, npi
	from final_withdup
	order by patid, fill_dt;

	select count(*), count(distinct patid)
		into :numobs_final, :numsubjs_final
	from final;
quit;

%put The number of rows in the final dataset is &numobs_final;
%put The number of patients in the final dataset is &numsubjs_final;

/*data data.subset_ac_active_nodup;*/
/*	set final;*/
/*run;*/
/**/
proc export data=final
   outfile='C:\Users\mengbing\Box Sync\OptumInsight_DataManagement\data\subset_ac_active_nodup.xlsx'
   dbms=xlsx
   replace;
   putnames=yes;
run;
/**/
/*proc datasets library=user nolist kill;*/
/*run; quit; */

