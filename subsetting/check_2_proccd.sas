/*
THIS PROGRAM CHECKS WHETHER THE MEDICAL CLAIMS AND FACILITY CLAIMS ARE DISJOINT.
SPECIFICALLY, IF A ROW IN MEDICAL CLAIM HAS LOC_CD = 1 (FROM FACILITY) HAS A NONTRIVIAL
PROC_CD (NE 000000), AND IF THAT PROC_CD APPEARS IN FACILITY CLAIM, THEN OUTPUT.
WE ONLY CHECK THIS FOR HCPCS CODES FOR CHEMO DRUG AND CPT CODES FOR SMOKING TO 
SAVE RUNNING TIME. WE ALSO ONLY LOOK AT PATIENTS WITH NON-MISSING INDEX VTE DATE
FROM MEMBER DATA SET, AND RECORDS WITH SERVICE DATE PRIOR TO INDEX VTE. 
*/

options symbolgen user="X:\DVT\dm";
/*options symbolgen user=work;*/
options fullstimer msglevel=i ;
proc options group=memory; run;
proc options option=utilloc; run;
proc options option=threads; run;


libname data "C:\Users\mengbing\Box Sync\OptumInsight_DataManagement\data";
libname medical "X:\DVT\Medical_vte";
libname facility "X:\DVT\Facility_vte";
libname member "X:\DVT\Member_vte";


/*
	1. READ IN MEMBER DATA SET FOR INDEX VTE DATE
*/
proc sql;
*** READ IN MEMBER DATASET FOR INDEX VTE ***;
	create table member as
	select distinct strip(put(patid,20.)) as patid, index_dt format=YYMMDD10.
	from member.member(keep = patid index_dt);
quit;



/*
	2. READ IN MEDICAL DATA SET FOR PATIENTS APPEARING IN MEMBER AND
	FST_DT AFTER INDEX_DT
*/

%macro create_med(yr=);
	select strip(put(patid,20.)) as patid, fst_dt format=YYMMDD10., loc_cd,
		proc_cd, "medical" as source
	from medical.med_dvt_&yr(keep=patid fst_dt loc_cd proc_cd) 
	where (calculated patid in (select patid from member))
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

*** KEEP RECORDS PRIOR TO INDEX VTE ***;
proc sql;
	create table med2 as
	select med.patid, member.index_dt, med.fst_dt, med.loc_cd, med.proc_cd, med.source
	from member right join med
	on member.patid = med.patid
	where member.index_dt >= med.fst_dt;
quit;





/*
	3. READ IN FACILITY DATA SETS FOR PATIENTS APPEARING IN MEMBER AND
	FST_DT AFTER INDEX_DT
*/

%macro create_fac(yr=);
	select strip(put(patid,20.)) as patid, fst_dt format=YYMMDD10.,
		proc_cd, "facility" as source
	from facility.fac_dvt_&yr(keep=patid fst_dt proc_cd) 
	where (calculated patid in (select patid from member))
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


*** KEEP RECORDS PRIOR TO INDEX VTE ***;
proc sql;
	create table fac2 as
	select fac.patid, member.index_dt, fac.fst_dt, fac.proc_cd, fac.source
	from member right join fac
	on member.patid = fac.patid
	where index_dt >= fst_dt;
quit;


*** CONCATENATE MEDICAL AND FACILITY DATA ***;
proc sql;
	create table procinfo as
	select *
	from med2 
	outer union corr
	select*
	from fac2;
quit;





/*
	4. KEEP ONLY HCPCS CODES FOR CHEMO, AND CPT CODES FOR SMOKING
*/

*** MACRO TO READ IN ICD9, NDC, HCPC CODES ***;
%macro read_code(outname=,sheet=);
	proc import out = &outname
	    datafile = '..\others\All_codes_ICD9_NDC_HCPCS.xlsx'
	    dbms = xlsx
		replace;
	    sheet = &sheet;
	    getnames = yes;
	run;
%mend read_code;

%read_code(outname=hcpcs_chemo, sheet="HCPCS_Chemo");
%read_code(outname=smoking, sheet="Smoking");

proc sql noprint;
*** CONCATENATE ALL CODES ***;
	create table all_codes as
	select strip(hcpcs) as code, "HCPCS" as code_type, "chemo" as procedure
	from hcpcs_chemo
	outer union corr
	select strip(code) as code, code_type, "smoking" as procedure
	from smoking
	where code_type in ("CPT", "HCPCS");
quit;


*** FILTER OUT RELEVANT CODE INFORMATION ***;
proc sql noprint;
	create table procinfo2 as
	select a.*, b.code_type, b.procedure
	from procinfo a left join all_codes b
	on a.proc_cd = b.code
	where proc_cd in (select code from all_codes) 
		or 20000 <= input(proc_cd, best8.0) <= 69990;
quit;

proc sql noprint;
	create table procinfo3 as 
	select distinct patid, index_dt, fst_dt, loc_cd, proc_cd, source,
		case 
			when code_type2 ne "" then code_type2
			else "CPT/HCPCS"
		end as code_type,
		case
			when procedure2 ne "" then procedure2
			else "Surgery"
		end as procedure
	from procinfo2 (rename=(code_type = code_type2 procedure = procedure2));

	create table final as
	select *
	from procinfo3
	where procedure = "chemo"
		or (procedure = "smoking" and index_dt - 28 <= fst_dt <= index_dt)
		or (procedure = "Surgery" and index_dt - 28 <= fst_dt <= index_dt);

	select count(*), count(distinct patid)
	into :numobs_final, :numsubjs_final
	from final;
quit;


/**/
/**/
/**** OUTPUT SMOKING AND CHEMO CODES INTO FINAL1, AND SURGERY CODES INTO FINAL2 ***;	*/
/*data chemo_smoke surgery;*/
/*	set procinfo2;*/
/*	if procedure in ("chemo", "smoking") then output chemo_smoke;*/
/*	if procedure = "" then output surgery;*/
/*run;*/
/**/
/**** REFINE THE TWO SETS OF CODES ***;*/
/*proc sql noprint;*/
/*	create table final1 as*/
/*	select distinct **/
/*	from chemo_smoke;*/
/*	*/
/*	create table final2 as*/
/*	select distinct *, "CPT/HCPCS" as code_type, "Surgery" as procedure*/
/*	from surgery (drop=code_type procedure)*/
/*	where index_dt - 30 <= fst_dt <= index_dt;*/
/**/
/*	select count(*), count(distinct patid)*/
/*	into :numobs_final1, :numsubjs_final1*/
/*	from final1;*/
/**/
/*	select count(*), count(distinct patid)*/
/*	into :numobs_final2, :numsubjs_final2*/
/*	from final2;*/
/*quit;*/

	


%put The number of patients who had an HCPCS chemo procedure and a CPT smoking code
	before index VTE date (or within 30 days prior to index VTE) is &numsubjs_final;
%put The number of rows in the final dataset is &numobs_final;


data data.check_2_proccd;
	set final;
run;


/*proc export data=final*/
/*	outfile="..\data\check_2_proccd.xlsx"*/
/*	dbms=xlsx replace;*/
/*	putnames=yes;*/
/*run;*/

proc export data=final outfile='..\data\check_2_proccd_20000surgery.txt' dbms=tab replace;
putnames=yes;
run;

proc datasets library=user nolist;
save final;
quit; run;

