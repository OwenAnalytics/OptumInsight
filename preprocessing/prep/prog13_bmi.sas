/*
THIS PROGRAM EXTRACTS BMI INFORMATION OF ALL PATIENTS. THE BMI RECORD OF INTEREST
IS THE MOST RECENT ONE BEFORE INDEX VTE DATE.
*/

/*options symbolgen user="X:\DVT\dm";*/
options symbolgen user=work;
options fullstimer msglevel=i ;
proc options group=memory; run;
proc options option=utilloc; run;
proc options option=threads; run;


libname data "C:\Users\mengbing\Box Sync\OptumInsight_DataManagement\data";
libname member "X:\DVT\Member_vte"; 
libname document "X:\DVT\Document";

proc import datafile="C:\Users\mengbing\Box Sync\OptumInsight_DataManagement\data\subset_ac_days_multi_cancers.xlsx"
     out=patinfo
     dbms=xlsx
     replace;
     getnames=yes;
run;


proc sql noprint;
	create table patinfo2 as
	select distinct patid, index_dt
	from patinfo;
quit;


proc sql noprint;
*** READ IN MEMBER DATA OF ELIGIBLE PATIENTS ***;
	create table member as
	select distinct strip(put(patid,20.)) as patid, h_bmi as bmi, hra_compltd_dt
	from member.ses_hra_dvt
	where calculated patid in (select patid from patinfo2)
	order by patid;

	select count(distinct patid)
		into: n_patients
	from member;
quit;

proc sql;
*** ADD INDEX VTE DATE TO MEMBER DATA SET ***;
	create table bmi_info as
	select a.patid, a.index_dt, b.bmi, b.hra_compltd_dt
	from patinfo2 as a left join member as b
	on a.patid = b.patid
	order by patid;

*** KEEP BMI RECORDS BEFORE INDEX VTE DATE AND FIND THE MAXIMUM BMI DATE ***;
	create table bmi_info2 as 
	select *
	from bmi_info
	where index_dt > hra_compltd_dt;

	create table bmi_info3 as
	select *
	from bmi_info2
	group by patid
	having hra_compltd_dt = max(hra_compltd_dt);
quit;


*** SOME PATIENTS ONLY HAVE BMI RECORDS AFTER INDEX VTE DATE. 
THESE PATIENTS ARE SELETED SEPARATELY AND ADDED BACK TO BMI INFORMATION DATA ***;
proc sql;
	create table bmi_onlyAfterVTE as
	select distinct patid, index_dt, . as bmi, . as hra_compltd_dt
	from bmi_info
	where patid not in (select patid from bmi_info2);
quit;


*** CONCATENATE BMI_INFO AND BMI_ONLYAFTERVTE ***;
proc sql;
	create table final as
	select *
	from bmi_info3
	outer union corr
	select *
	from bmi_onlyAfterVTE
	order by patid;
quit;

proc sql noprint;
	select count(patid)
		into: pat_count
	from final;

	select count(patid)
		into: pat_count_noMissing
	from final
	where bmi ne .;
quit;

%put The final data set has &pat_count patients;
%put The final data set has &pat_count_noMissing patients with non-missing BMI before index VTE date;

proc export data=final
	outfile="C:\Users\mengbing\Box Sync\OptumInsight_DataManagement\data\prog13_bmi.txt"
	dbms=tab replace;
	putnames=yes;
run;
