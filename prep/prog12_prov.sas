/*
THIS PROGRAM EXTRACTS NPI FOR ANTICOAGULANT FILLS OF ELIGIBLE PATIENTS FROM PHARM, AND LINKS
NPI TO PROVIDER UNIQUE THROUGH PROVIDER_BRIDGE. IT THEN ADDS COVARIATES OF PROVIDER CHARACTERISTICS 
TO THE FINAL OUTPUT DATA SET.
*/

/*options symbolgen user="X:\DVT\dm";*/
options symbolgen user=work;
options fullstimer msglevel=i ;
proc options group=memory; run;
proc options option=utilloc; run;
proc options option=threads; run;


libname data "C:\Users\mengbing\Box Sync\OptumInsight_DataManagement\data";
libname pharma "X:\DVT\Pharma_vte"; 
libname document "X:\DVT\Document";

proc import datafile="C:\Users\mengbing\Box Sync\OptumInsight_DataManagement\data\subset_ac_days_multi_cancers.xlsx"
     out=patinfo
     dbms=xlsx
     replace;
     getnames=yes;
run;


proc sql noprint;
	select count(distinct patid)
	into :n_patid
	from patinfo;
quit;

%put &n_patid;
/* READING FROM RAW PROVIDER DATA IS WAY TOO SLOW */
/*proc sql;*/
/*	create table ses_provider_bridge as*/
/*	select **/
/*	from document.ses_provider_bridge;*/
/**/
/*	create table ses_provider as*/
/*	select **/
/*	from document.ses_provider;*/
/*quit;*/

proc sql;
	create table ses_provider_bridge as
	select *
	from data.ses_provider_bridge;

	create table ses_provider as
	select *
	from data.ses_provider;
quit;


*** FLAG TRIVIAL NPI VALUES FROM PHARM ***;
proc sql;
	create table patinfo2 as 
	select *, case
		when npi in ("L", "LLLLLLLLLL") then "trivial"
		when npi = "" then "missing"
		else npi
		end as npi2
	from patinfo (keep=patid clmid fill_dt category brand_name copay days_sup quantity strength npi);
quit;


proc sql;
	create table patinfo3 as
	select distinct a.*, b.prov_unique
	from patinfo2 as a 
	left join 
	ses_provider_bridge (where=(npi ne "")) as b
	on a.npi2 = b.npi
	order by patid, fill_dt;
quit;

proc sql;
	create table patinfo4 as
	select a.*, b.Bed_Sz_Range, b.prov_state, b.provcat, b.taxonomy1, b.taxonomy2
	from patinfo3 as a left join ses_provider as b
	on a.prov_unique = b.prov_unique
	order by patid, fill_dt;
quit;


/* ADD PROVIDER CATEGORY INFORMATION */
proc import datafile="X:\DVT\Document\CDM Data Dictionary 7.0 SES.xls"
     out=provcat_info
     dbms=xls
     replace;
	 sheet="PROVCAT";
     getnames=yes;
	 namerow=6;
	 datarow=7;
	 endrow=6809;
run;

proc sql;
	create table patinfo5 as
	select a.*, b.description, b.CATGY_ROL_UP_1_DESC, b.CATGY_ROL_UP_2_DESC,
		b.CATGY_ROL_UP_3_DESC, b.CATGY_ROL_UP_4_DESC
	from patinfo4 as a left join provcat_info as b
	on a.provcat = b.provcat
	order by patid, fill_dt;
quit;



proc export data=patinfo5
	outfile="C:\Users\mengbing\Box Sync\OptumInsight_DataManagement\data\prog12_prov.txt"
	dbms=tab replace;
	putnames=yes;
run;
