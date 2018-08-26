/*
THIS PROGRAM EXTRACTS SES INFORMATION OF ELIGIBLE PATIENTS. THE SOURCE DATA IS SES_MEMBER.
*/

/*options symbolgen user="X:\DVT\dm";*/
options symbolgen user=work;
options fullstimer msglevel=i ;
proc options group=memory; run;
proc options option=utilloc; run;
proc options option=threads; run;


libname data "C:\Users\mengbing\Box Sync\OptumInsight_DataManagement\data";
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


proc sql noprint;
	create table final as
	select strip(put(patid,15.)) as patid, D_EDUCATION_LEVEL_CODE as education,
		D_FED_POVERTY_STATUS_CODE as fed_poverty, D_HOME_OWNERSHIP_CODE as home_ownership,
		D_HOUSEHOLD_INCOME_RANGE_CODE as income_range, D_NETWORTH_RANGE_CODE as networth_range,
		D_OCCUPATION_TYPE_CODE as occupation, D_RACE_CODE as race, NUM_ADULTS as num_adults
	from member.ses_member
	where calculated patid in (select patid from patids);

	select count(distinct patid), count(*)
		into :numsubjs, :numobs
	from final;
quit;

%put The number of subjects with ses information available is &numsubjs;
%put The number of rows is &numobs;


proc export data=final
	outfile="C:\Users\mengbing\Box Sync\OptumInsight_DataManagement\data\prog11_ses.txt"
	dbms=tab replace;
	putnames=yes;
run;

proc datasets library=user nolist kill;
quit; run;

