/*
THIS PROGRAM LOOKS FOR COMORBIDITIES WITHIN 12 MONTHS PRIOR TO INDEX VTE DATE FOR
ELIGIBLE PATIENTS. THE COMORBIDITIES COME FROM BOTH MEDICAL AND CONFINEMENT DATA.
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



/* THIS PROCEDURE SHOULD BE USED INSTEAD */;
proc sql;
	create table med as 
	select *
	from data.prog7_get_med (where=(index_dt-365 <= fst_dt <= index_dt));
quit;

proc sql;
*** ADD INDEX VTE DATE. THIS WILL BE USED TO LOOK FOR VTE HISTORY ***;
	create table medinfo as
	select a.*, b.index_dt
	from med a, patids b
	where a.patid = b.patid
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

%read_code(outname=icd9_comorb, sheet="Charlson_Comorbidity");

proc sql noprint;
*** STORE ICD9 VTE CODES INTO A MACRO VARIABLE ***;
	select quote(strip(icd9_comorbidity)) into :comorb separated by ', '
	from icd9_comorb;
quit;




%macro gencomorb(indata=, src=, dt_name=, diag_length=);

*** CHECK WHICH DIAG CORRESPONDS TO A VTE HISTORY AND VTE ***;
data &indata.2;
	set &indata;
	length comorb_code $50;
	array diag{&diag_length} diag1-diag&diag_length;
	do i=1 to &diag_length;
		if diag[i] ne "" then do;
			if (strip(diag[i]) in (&comorb)) 
			  or (substr(strip(diag[i]),1, min(4, length(strip(diag[i])))) in (&comorb))
			  or (substr(strip(diag[i]),1, min(3, length(strip(diag[i])))) in (&comorb))
			then comorb_code=catx(', ', strip(comorb_code), diag[i]);
		end;
	end;
	drop i diag1-diag&diag_length;
run;

*** IDENTIFY VTE ON INDEX VTE DATE ***;
proc sql;
	create table &indata.3 as
	select distinct patid, comorb_code
	from &indata.2
	where comorb_code ne "";
quit;

*** CONVERT MULTIPLE VTE TYPE INTO ONE VTE PER ROW ***;
data &indata.3_long;	
	set &indata.3;
	length diag_code $30;
	do i=1 to (count(comorb_code, ', ')+1);
		diag_code = strip(scan(comorb_code,i,','));
		output;
	end;
	drop i;
run;

proc sql;
	create table patinfo_comorb_&src as 
	select distinct patid, diag_code
	from &indata.3_long;
quit;

%mend;

%gencomorb(indata=medinfo, src=med, dt_name=fst_dt, diag_length=25);



/*
	2. LOOK INTO CONFINEMENT DATA SET
*/
proc import datafile="C:\Users\mengbing\Box Sync\OptumInsight_DataManagement\data\prog2_conf.xlsx"
     out=confinfo_raw
     dbms=xlsx
     replace;
     getnames=yes;
run;

proc sql;
	create table confinfo as
	select * 
	from confinfo_raw
	where index_dt >= admit_dt >= index_dt - 365;
quit;

%gencomorb(indata=confinfo, src=conf, dt_name=admit_dt, diag_length=5);




/*
	3. MERGE COMORBIDITY TYPE FROM MEDICAL AND CONFINEMENT
*/

proc sql noprint;
*** MERGE INDEX VTE TYPES FROM MED AND CONF ***;
	create table patinfo_comorb_code as
	select *
	from patinfo_comorb_med
	outer union corr
	select *
	from patinfo_comorb_conf
	order by patid;

*** ADD VTE TYPES MATCHING VTE CODES ***;
	create table final as
	select distinct a.patid, b.category
	from patinfo_comorb_code a left join icd9_comorb b
	on strip(a.diag_code) = strip(b.icd9_comorbidity)
		or substr(strip(a.diag_code),1, min(4, length(strip(a.diag_code)))) = strip(b.icd9_comorbidity)
		or substr(strip(a.diag_code),1, min(3, length(strip(a.diag_code)))) = strip(b.icd9_comorbidity)
	order by patid;

	select count(distinct patid), count(*)
		into :numsubjs, :numobs
	from final;
quit;


%put The number of subjects with at least one comorbidity is &numsubjs;
%put The number of rows is &numobs;


proc export data=final
	outfile="C:\Users\mengbing\Box Sync\OptumInsight_DataManagement\data\prog10_comorbidity.xlsx"
	dbms=xlsx replace;
	putnames=yes;
run;

proc datasets library=user nolist kill;
quit; run;

