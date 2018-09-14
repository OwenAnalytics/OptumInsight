/****************************************************************************
* Program: prog14_get_member_details.sas			   
*
* Location: ~\Box Sync\OptumInsight_DataManagement\preprocessing\prep
*
* Author: Mengbing Li 
*
* Created: Wednesday, 09/12/2018 
*
* Description: This program extracts rows of final patients from mem_details
* data set. 
*
* Revisions:
*****************************************************************************/

/*options symbolgen user="X:\DVT\dm";*/
options symbolgen user=work;
options fullstimer msglevel=i ;
proc options group=memory; run;
proc options option=utilloc; run;
proc options option=threads; run;


libname prep "C:\Users\mengbing\Box Sync\OptumInsight_DataManagement\preprocessing\prep";
libname subset "C:\Users\mengbing\Box Sync\OptumInsight_DataManagement\preprocessing\subsetting";
libname member "X:\DVT\Member_vte"; 
libname document "X:\DVT\Document";

proc import datafile="C:\Users\mengbing\Box Sync\OptumInsight_DataManagement\preprocessing\subsetting\subset_3_removeNoACPeriod_multipleCancers.xlsx"
     out=patinfo
     dbms=xlsx
     replace;
	 sheet="subset_3_removeNoACPeriod";
     getnames=yes;
run;


proc sql noprint;
	create table patinfo2 as
	select distinct patid, index_dt
	from patinfo;
quit;


proc sql noprint;
*** READ IN MEMBER DATA OF ELIGIBLE PATIENTS ***;
	create table mem_detail as
	select strip(put(patid,20.)) as patid, pat_planid, aso, bus, cdhp, division, eligeff, eligend, 
		gdr_cd, group_nbr, health_exch, lis, product, yrdob
	from member.mem_detail_dvt
	where calculated patid in (select patid from patinfo2)
	order by patid;

	select count(distinct patid)
		into: n_patients
	from mem_detail;
quit;


proc export data=mem_detail
	outfile="C:\Users\mengbing\Box Sync\OptumInsight_DataManagement\preprocessing\prep\prog14_get_member_details.txt"
	dbms=tab replace;
	putnames=yes;
run;
