/* 
THIS PROGRAM CHECKS WHETHER THE ICD-9 CANCER CODES FROM MEDICAL DATA AND CONFINEMENT 
DATA OVERLAP. THE FINAL OUTPUT CONTAINS ELIGIBLE PATIENTS THAT APPEAR IN ONLY ONE
OF MEDICAL DATA AND CONFINEMENT DATA, BUT NOT BOTH.
*/;

options symbolgen user="X:\DVT\dm";
/*options symbolgen user=work;*/
options fullstimer msglevel=i ;

libname member "X:\DVT\Member_vte";
libname medical "X:\DVT\Medical_vte";
libname pharma "X:\DVT\Pharma_vte"; 
libname conf "X:\DVT\Inp_vte";
libname data "..\data";


/*libname subsetting ".";*/
/*libname member "\\mcomm-schaefer-turbo.turbo.storage.umich.edu\mcomm-schaefer-turbo\DVT\Member_vte";*/
/*libname medical "\\mcomm-schaefer-turbo.turbo.storage.umich.edu\mcomm-schaefer-turbo\DVT\Medical_vte";*/
/*libname pharma "\\mcomm-schaefer-turbo.turbo.storage.umich.edu\mcomm-schaefer-turbo\DVT\Pharma_vte";*/

proc options group=memory; run;
proc options option=utilloc; run;
proc options option=threads; run;


%macro create_pharm(yr=);
	select strip(put(patid,20.)) as patid, strip(put(pat_planid,19.)) as planid,
		copay, fill_dt format=YYMMDD10. as fst_dt, days_sup, quantity, strength, ndc
	from pharma.pharm_dvt_&yr(keep=patid pat_planid copay fill_dt days_sup quantity strength ndc)
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
	    datafile = 'C:\Users\mengbing\Box Sync\Optum Insight - Data Management\others\All_codes_ICD9_NDC_HCPCS.xlsx'
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


proc sql noprint;
*** KEEP RECORDS THAT APPEAR IN ONLY ONE OF MEDICAL AND CONFINEMENT DATA SETS, NOT BOTH ***;
	create table patinfo2 as
	select * 
	from patinfo2_med
	where patid not in (select distinct patid from patinfo2_conf)
	outer union corr
	select * 
	from patinfo2_conf
	where patid not in (select distinct patid from patinfo2_med);

*** KEEP ROWS WITH DISTINCT INFORMATION ***;
	create table final as 
	select distinct patid, fst_dt, cancer_diag, source
	from patinfo2
	where cancer_diag ne ""
	order by patid, fst_dt;

	select count(*), count(distinct patid)
		into :numobs_final, :numsubjs_final
	from final;
quit;

%put The number of patients that have at least one cancer diagnosis that appeared in 
only one of medical or confinement data set but not both is &numobs_final;
%put The number of rows is &numsubjs_final;

proc export data=final
   outfile='..\data\check_3_cancer_overlap.xlsx'
   dbms=xlsx
   replace;
   putnames=yes;
run;

proc datasets library=user nolist kill;
run; quit; 

