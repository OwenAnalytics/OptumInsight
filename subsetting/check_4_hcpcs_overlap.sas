/*
THIS PROGRAM CHECKS WHETHER THE HCPCS CODES FROM MEDICAL DATA AND FACILITY DATA OVERLAP
*/

options symbolgen user="X:\DVT\dm";
/*options symbolgen user=work;*/
options fullstimer msglevel=i ;
proc options group=memory; run;
proc options option=utilloc; run;
proc options option=threads; run;


libname data "C:/Users/mengbing/Box Sync/Optum Insight - Data Management\data";
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
	create table med as
	%create_med(yr=2007)
	outer union corr
	%create_med(yr=2008)
	outer union corr
	%create_med(yr=2009)
	outer union corr
	%create_med(yr=2010);
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
	create table fac as
	%create_fac(yr=2007)
	outer union corr
	%create_fac(yr=2008)
	outer union corr
	%create_fac(yr=2009)
	outer union corr
	%create_fac(yr=2010);
quit;


*** KEEP RECORDS PRIOR TO INDEX VTE ***;
proc sql;
	create table fac2 as
	select fac.patid, member.index_dt, fac.fst_dt, fac.proc_cd, fac.source
	from member right join fac
	on member.patid = fac.patid
	where index_dt >= fst_dt;
quit;



proc sql;
	create table med_not_fac as 
	select qry.*, monotonic() as seq_num
	from (select * from med2 (drop=loc_cd where=(strip(proc_cd) not in ("", "00000")))
	except
	select * from fac2(where=(strip(proc_cd) not in ("", "00000")) )) as qry;
quit;

proc sql;
	create table fac_not_med as 
	select qry.*, monotonic() as seq_num
	from (select * from fac2 (where=(strip(proc_cd) not in ("", "00000")))
	except
	select * from med2(drop=loc_cd where=(strip(proc_cd) not in ("", "00000")) )) as qry;
quit;

proc sql;
	create table procinfo as 
	select distinct a.patid, a.index_dt, a.fst_dt, a.proc_cd, "Med Not Fac" as raw_src
	from med_not_fac as a
	union
	select distinct b.patid, b.index_dt, b.fst_dt, b.proc_cd, "Fac Not Med" as raw_src
	from fac_not_med as b
	order by patid, fst_dt, proc_cd;
quit;


/*
	4. KEEP ONLY HCPCS CODES FOR CHEMO, AND CPT CODES FOR SMOKING
*/

*** MACRO TO READ IN ICD9, NDC, HCPC CODES ***;
%macro read_code(outname=,sheet=);
	proc import out = &outname
	    datafile = 'C:/Users/mengbing/Box Sync/Optum Insight - Data Management\others\All_codes_ICD9_NDC_HCPCS.xlsx'
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
	create table final as
	select distinct a.*, b.code_type, b.procedure
	from procinfo a left join all_codes b
	on a.proc_cd = b.code
	where proc_cd in (select code from all_codes) 
	order by patid, fst_dt, proc_cd, raw_src;

	select count(*), count(distinct patid)
		into :numobs_final, :numsubjs_final
	from final;
quit;

%put The number of patients who had a mismatched HCPCS chemo procedure or smoking CPT code from medical data and
facility data after index VTE date is &numsubjs_final;
%put The number of rows in the final dataset is &numobs_final;



proc export data=final
	outfile="C:/Users/mengbing/Box Sync/Optum Insight - Data Management\data\check_4_hcpcs_overlap_copy.xlsx"
	dbms=xlsx replace;
	putnames=yes;
run;

proc datasets library=user nolist kill;
quit; run;

