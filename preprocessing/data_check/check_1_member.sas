/* 
*DO NOT RUN FOR SUBSETTING PURPOSES.*
THIS ONE-TIME PROGRAM CHECKS WHETHER PATIENTS IN MEMBER AND MEMBER_DETAILS ARE IDENTICAL,
IN ORDER TO VERIFY THE FEASIBILITY OF OBTAINING INDEX VTE DATE WITH NEW ICD-9 CODES.
*/;

/****************************************************************************
* Program: check_1_member.sas			   
*
* Location: ~\Box Sync\OptumInsight_DataManagement\preprocessing\data_check 
*
* Author: Mengbing Li 
*
* Created: Friday, May 18, 2018 
*
* Description: *DO NOT RUN FOR SUBSETTING PURPOSES.*
* This one-time program is a preliminary check on whether patients in member
* and member_details data sets are identical. 
*
* Revisions: 09/05/2018 - Revise header information 
*****************************************************************************/

options symbolgen user="X:\DVT\dm";
/*options symbolgen user=work;*/
options fullstimer msglevel=i ;

libname member "X:\DVT\Member_vte";
libname data "..\data";

proc options group=memory; run;
proc options option=utilloc; run;
proc options option=threads; run;


proc sql noprint;
*** READ IN MEMBER DETAILS ***;
	create table final as
	select *
	from member.mem_detail_dvt
	where patid not in (select distinct patid from member.member)
	order by patid;

	select count(*), count(distinct patid)
		into :numobs_final, :numsubjs_final
	from final;
quit;

%put The number of rows in the final dataset is &numobs_final;
%put The number of patients in the final dataset is &numsubjs_final;


proc export data=final
   outfile='C:\Users\mengbing\Box Sync\Optum Insight - Data Management\data\check_member.xlsx'
   dbms=xlsx replace;
   putnames=yes;
run;



proc datasets library=user nolist kill;
run; quit; 

