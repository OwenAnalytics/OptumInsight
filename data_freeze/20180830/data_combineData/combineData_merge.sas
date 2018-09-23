/****************************************************************************
* Program: combineData_merge.sas			   
*
* Location: ~\Box Sync\OptumInsight_DataManagement\data_freeze\20180830\data_combineData
*
* Author: Mengbing Li 
*
* Created: Wednesday 9/21/2018 
*
* Description: This program completes the last step in combineData.R because the data sets
* are too large for memory.
*
* Revisions: 
*****************************************************************************/

options symbolgen user="X:\DVT\dm";
/*options symbolgen user=work;*/
options fullstimer msglevel=i ;
proc options group=memory; run;
proc options option=utilloc; run;
proc options option=threads; run;


libname data "C:\Users\mengbing\Box Sync\OptumInsight_DataManagement\data_freeze\20180830\data_combineData";

proc import datafile='C:\Users\mengbing\Box Sync\OptumInsight_DataManagement\data_freeze\20180830\data_combineData\med_conf_fac_ac.csv'
   out=med_conf_fac_ac
   dbms=csv
   replace;
run;

proc import datafile='C:\Users\mengbing\Box Sync\OptumInsight_DataManagement\data_freeze\20180830\data_combineData\baseline.txt'
   out=baseline
   dbms=tab
   replace;
run;

*** CONVERT PATID FROM NUMERIC TO CHARACTER ***;
data baseline;
	set baseline (drop = cancer_type cancer_type_combined comorbidities 
		ac3mo_copay ac3mo_copay_rng ac4mo_copay ac4mo_copay_rng pos);
	format patid $15.;
run;

data med_conf_fac_ac;
	set med_conf_fac_ac;
	format patid $15.;
run;

data med_conf_fac_ac;
	set med_conf_fac_ac;
	format patid $15.;
run;

*** MERGE COMBINED DATA AND BASELINE COVARIATES ***;
proc sql;
	create table mydata as
	select a.*, b.*
	from med_conf_fac_ac a 
		left join
		baseline b
	on a.patid = b.patid
	order by patid, fst_dt, lst_dt;
quit;



/*
Build a tree structure of ICD-9 codes
The basic icd-9 code consists of 3 digits, while some are more specific
with 4 or five digits subdivision.
E codes and V codes represent external causes of injury and
supplemental classification
V codes format: VXX(.XX)
E codes format: EXXX(.X)
*/
	
data mydata;
	set mydata;
	format icd9_3digits $3. icd9_4digits $4. icd9_5digits $5.;

	*** SUBSTRING FIRST 3 DIGITS OF ICD9 ***;
	if substr(icd9_raw, 1, 1) = "E" then icd9_3digits = substr(icd9_raw, 1, 4);
	else icd9_3digits = substr(icd9_raw, 1, 3);
	
	*** SUBSTRING FIRST 4 DIGITS OF ICD9 ***;
	if substr(icd9_raw, 1, 1) = "E" then
		if length(icd9_raw) < 5 then icd9_4digits = "";
		else icd9_4digits = substr(icd9_raw, 1, 5);
	else 
		if length(icd9_raw) < 4 then icd9_4digits = "";
		else icd9_4digits = substr(icd9_raw, 1, 4);
		
	*** SUBSTRING FIRST 4 DIGITS OF ICD9 ***;
	if substr(icd9_raw, 1, 1) = "E" then icd9_5digits = "";
	else 
		if length(icd9_raw) < 5 then icd9_5digits = "";
		else icd9_5digits = substr(icd9_raw, 1, 5);
run;



*** ADD INSURANCE COVERAGE DETAILS ***;
proc import datafile='C:\Users\mengbing\Box Sync\OptumInsight_DataManagement\data_freeze\20180830\member.txt'
   out=member
   dbms=tab
   replace;
run;

data member;
	set member (keep = patid eligeff eligend enroll_length);
	format patid $15.;
run;

proc sql;
	create table member2 as
	select *
	from member
	where patid in (select patid from baseline)
	order by patid, eligeff;
quit;

*** ADD INSURANCE COVERAGE TO MYDATA ***;
proc sql;
	create table mydata2 as
	select a.*, b.eligeff, b.eligend, b.enroll_length
	from mydata a
		left join
		member b
	on a.patid = b.patid and b.eligeff <= a.fst_dt <= b.eligend
	order by patid, fst_dt, lst_dt;
quit;



*** RE-ORDER COLUMNS ***;
data mydata2;
	format patid source copay fst_dt 
		conf_id lst_dt diag icd9_raw icd9_3digits icd9_4digits icd9_5digits
			los proc_cd prov
		gen_name days_sup quantity strength npi
		eligeff eligend enroll_length;
	set mydata2;
run;



proc export data=mydata2 
	dbms=csv
	outfile="C:\Users\mengbing\Box Sync\OptumInsight_DataManagement\data_freeze\20180830\data_combineData\combineData.csv"
	replace;
run;

