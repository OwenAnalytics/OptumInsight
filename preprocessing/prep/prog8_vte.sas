/*
THIS PROGRAM LOOKS INTO MEDICAL DATA SETS FOR INDEX VTE TYPE AND VTE HISTORY
  FOR ELIGIBLE PATIENTS DURING 12 MONTHS PRIOR TO INDEX VTE. THIS INFORMATION 
  IS USED IN CALCULATING TABLE 1 STATISTICS.
1. FIND IF A PATIENT HAS VTE HISTORY: ICD-9 CODE = V12.51
2. FIND THE INDEX VTE TYPE OF EACH PATIENT FROM MEDICAL DATA SETS. THE ICD-9 
	CODES COME FROM THE ORIGINAL LIST USED BY TANIMA.
3. FIND THE INDEX VTE TYPE FOR PATIENTS WHOSE INDEX VTE DATE IS THE FIRST
  ADMIT_DT FROM CONF DATA SETS
*/

/*options symbolgen user="X:\DVT\dm";*/
options symbolgen user=work;
options fullstimer msglevel=i ;
proc options group=memory; run;
proc options option=utilloc; run;
proc options option=threads; run;


libname data "C:\Users\mengbing\Box Sync\OptumInsight_DataManagement\data";
libname medical "X:\DVT\Medical_vte";
libname member "X:\DVT\Member_vte";
/*libname medical "\\mcomm-schaefer-turbo.turbo.storage.umich.edu\mcomm-schaefer-turbo\DVT\Medical_vte";*/
/*libname member "\\mcomm-schaefer-turbo.turbo.storage.umich.edu\mcomm-schaefer-turbo\DVT\Member_vte";*/

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



/*
FIND IF A PATIENT HAS VTE HISTORY: ICD-9 CODE = V12.51 AND INDEX VTE TYPE
*/


/*
	1. LOOK INTO MEDICAL DATA SET
*/
proc sql;
	create table med as
	select * 
	from data.prog7_get_med (where=(index_dt-365 <= fst_dt <= index_dt)); *data.prog7_get_med;
quit;

proc sql;
*** ADD INDEX VTE DATE. THIS WILL BE USED TO LOOK FOR VTE HISTORY ***;
	create table medinfo as
	select a.*, b.index_dt
	from med a, patids b
	where a.patid = b.patid
		and b.index_dt >= a.fst_dt >= b.index_dt - 365
	order by patid;
quit;


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

%read_code(outname=icd9_vte_codes_raw, sheet="ICD9_VTE");

proc sql noprint;
*** STORE ICD9 VTE CODES INTO A MACRO VARIABLE ***;
	create table icd9_vte_codes as
	select strip(put(icd9_vte,5.)) as icd9_vte, vte_type
	from icd9_vte_codes_raw;

	select quote(strip(icd9_vte)) into :vte separated by ', '
	from icd9_vte_codes;
quit;




%macro genvte(indata=, src=, dt_name=, diag_length=);

*** CHECK WHICH DIAG CORRESPONDS TO A VTE HISTORY AND VTE ***;
data &indata.2;
	set &indata;
	vte_history = 0;
	length index_vte_code $30;
	array diag{&diag_length} diag1-diag&diag_length;
	do i=1 to &diag_length;
		if strip(diag[i]) = "V1251" then vte_history = 1;
		if diag[i] in (&vte) then index_vte_code=catx(', ', strip(index_vte_code), diag[i]);
	end;
	drop i diag1-diag&diag_length;
run;


*** IDENTIFY THE PRESENCE OF VTE HISTORY ***;
proc sql;
	create table patinfo_vte_history_&src as
	select distinct patid, vte_history
	from &indata.2
	group by patid
	having vte_history = max(vte_history);
quit;


*** IDENTIFY VTE ON INDEX VTE DATE ***;
proc sql;
	create table &indata.3 as
	select distinct patid, index_dt, index_vte_code, pos
	from &indata.2
	where &dt_name = index_dt and index_vte_code ne "";
quit;

*** CONVERT MULTIPLE VTE TYPE INTO ONE VTE PER ROW ***;
data &indata.3_long;	
	set &indata.3;
	length diag_code $30;
	do i=1 to (count(index_vte_code, ', ')+1);
		diag_code = strip(scan(index_vte_code,i,','));
		output;
	end;
	drop i;
run;

proc sql;
	create table patinfo_vte_type_&src as 
	select distinct patid, index_dt, diag_code, pos
	from &indata.3_long;
quit;

%mend;

%genvte(indata=medinfo, src=med, dt_name=fst_dt, diag_length=25);



/*
	2. LOOK INTO CONFINEMENT DATA SET
*/
proc import datafile="C:\Users\mengbing\Box Sync\OptumInsight_DataManagement\data\prog2_conf.xlsx"
     out=confinfo
     dbms=xlsx
     replace;
     getnames=yes;
run;

%genvte(indata=confinfo, src=conf, dt_name=admit_dt, diag_length=5);




/*
	3. MERGE VTE HISTORY FROM MEDICAL AND CONFINEMENT; 
	  MERGE INDEX VTE TYPE FROM MEDICAL AND CONFINEMENT
*/


/*
	3.1 MERGE VTE HISTORY FROM MEDICAL AND CONFINEMENT;
*/
proc sql noprint;
*** MERGE VTE HISTORY FROM MED AND CONF ***;
	create table patinfo_vte_history as
	select *
	from patinfo_vte_history_med
	outer union corr
	select *
	from patinfo_vte_history_conf
	order by patid;

*** KEEP DISTINCT ROWS ***;
	create table patinfo_vte_history2 as
	select distinct patid, vte_history
	from patinfo_vte_history
	group by patid
	having vte_history = max(vte_history);

	select count(distinct patid)
		into :numsubjs_history
	from patinfo_vte_history2;
quit;


/*
	3.2 MERGE INDEX VTE TYPE FROM MEDICAL AND CONFINEMENT
*/
proc sql;
*** MERGE INDEX VTE TYPES FROM MED AND CONF ***;
	create table patinfo_vte_type as
	select *
	from patinfo_vte_type_med
	outer union corr
	select *
	from patinfo_vte_type_conf;

*** ADD VTE TYPES MATCHING VTE CODES ***;
	create table patinfo_vte_type2 as
	select distinct a.patid, a.index_dt, a.diag_code, b.vte_type, a.pos
	from patinfo_vte_type a left join icd9_vte_codes b
	on strip(a.diag_code) = strip(b.icd9_vte)
	order by patid;
quit;


/* ADD PLACE OF SERVICE INFORMATION */
proc import datafile="X:\DVT\Document\CDM Data Dictionary 7.0 SES.xls"
     out=pos_info
     dbms=xls
     replace;
	 sheet="POS";
     getnames=yes;
	 namerow=6;
	 datarow=7;
	 endrow=45;
run;

data pos_info;
	set pos_info;
	if POS = "NONE" then POS = "";
	drop D;
run;


proc sql;
*** ADD VTE TYPES MATCHING VTE CODES ***;
	create table patinfo_vte_type3 as
	select distinct a.*, b.description as pos_desc label="POS description",
		b.category as pos_category label="POS category"
	from patinfo_vte_type2 a left join pos_info b
	on strip(a.pos) = strip(b.pos)
	order by patid;
quit;


*** OUTPUT THE POS INFORMATION FOR FURTHER REVIEW ***;
proc export data=patinfo_vte_type3
	outfile="C:\Users\mengbing\Box Sync\OptumInsight_DataManagement\data\prog8_vte.xlsx"
	dbms=xlsx replace;
	putnames=yes;
	sheet="raw_VTE_POS";
run;


*** OUTPUT MULTIPLE POS FOR FURTHER REVIEW ***;
proc sql;
	create table multipos as
	select *
	from patinfo_vte_type3
	group by patid
	having count(distinct pos) > 1;
quit;

proc export data=multipos
	outfile="C:\Users\mengbing\Box Sync\OptumInsight_DataManagement\data\prog8_vte.xlsx"
	dbms=xlsx replace;
	putnames=yes;
	sheet="multiple_POS";
run;


*** COMBINE MULTIPLE VTE TYPES ***;
proc sql noprint;
*** SCORE EACH VTE TYPE ***;
	create table patinfo_vte_type4 as
	select patid, pos, VTE_type,   
		case
			when VTE_type in ("Lower extremity DVT", "Pulmonary embolism") then 10
			else 1
		end as vte_score,
		sum(calculated vte_score) as vte_sum
	from patinfo_vte_type3
	group by patid;

*** COMBINE MULTIPLE INDEX VTE TYPES ***;
	create table patinfo_vte_type5 as 
	select distinct patid, pos, 
		case
			when vte_sum = 20 then "Lower extremity DVT + Pulmonary embolism"
			when vte_sum in (1,10) then VTE_type
			else "Multiple VTE types"
		end as index_vte_type
	from patinfo_vte_type4
	order by patid;

	select count(distinct patid)
		into :numsubjs_type
	from patinfo_vte_type5;
quit;



*** CONCATENATE MULTIPLE ROWS OF POS INTO ONE ROW ***;
data patinfo_vte_type6;
	length pos_all $20.;
		do until (last.patid);
			set patinfo_vte_type5;
			by patid notsorted;
			pos_all = catx(',', pos_all, pos);
		end;
run;

*** KEEP ONLY DISTINCT ROWS AFTER CONCATENATION ***;
proc sql;
	create table patinfo_vte_type7 as 
	select distinct patid, index_vte_type, pos_all as pos
	from patinfo_vte_type6
	order by patid;
quit;




%put The number of subjects with index VTE type is &numsubjs_type;
%put The number of subjects with VTE history record, i.e. the number of rows, 
(either 0 or 1) (ICD = V12.51) is &numsubjs_history;



/*
	4. COMBINE VTE HISTORY AND INDEX VTE TYPE
*/
data final;
	merge patinfo_vte_type7 patinfo_vte_history2;
	by patid;
run;


proc export data=final
	outfile="C:\Users\mengbing\Box Sync\OptumInsight_DataManagement\data\prog8_vte.xlsx"
	dbms=xlsx replace;
	putnames=yes;
run;

proc datasets library=user nolist kill;
quit; run;

