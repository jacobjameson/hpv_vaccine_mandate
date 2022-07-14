* MarketScan Project: HPV Virginia Project                                                      *
* Programmer: Jacob Jameson                                                                     *
* Date: 7/14/22                                                                                 *
* Plan: Look for HPV vaccines - ages 9-20 -- 2007-2018.  								                      	*
*       Marketscan data -- Commerical sample													                          *
*       ††HPV, 3 doses required, starting age 11: 												                      *
*	          (1) initial dose between age 11-12 yo 												                      *
*           (2) second dose 1-2 months after initial dose and 								                	*
*           (3) third dose six months after the initial dose								                    *
*************************************************************************************************;


libname plan 'X:\MarketScan';
libname out 'X:\staff\jjameson\HPV Vaccine Access\data';
libname temp 'X:\staff\jjameson\HPV Vaccine Access\temp';


%macro xx(yr,n);

libname in1 'X:\MarketScan';

title1 "Find Vaccines using Commercial Outpatient Services file";

DATA temp1;
  set in1.ccaeo&yr&n;
  where rx="1" & eidflag="1" & (proctyp="1" and proc1 in ("90649","90650")); 
RUN;

PROC means data=temp1;
  class sex;
  var age;
  title2"check age of patients receiving HPV vaccines in 20&yr";
RUN;

PROC sort data=temp1; by enrolid svcdate proc1;
RUN;

/*
/* summarize over ID, date and procedure to get costs */
DATA temp2(drop=copay deduct coins cob netpay pay);
  set temp1;
  by enrolid svcdate proc1;

  retain tot_copay tot_deduct tot_coins tot_cob tot_net tot_pay;
  if first.proc1 then do;
    tot_copay=0;
    tot_deduct=0;
    tot_coins=0;
    tot_cob=0;
    tot_net=0;
    tot_pay=0;
  end;
  tot_copay=tot_copay+copay;
  tot_deduct=tot_deduct+deduct;
  tot_coins=tot_coins+coins;
  tot_cob=tot_cob+cob;
  tot_net=tot_net+netpay;
  tot_pay=tot_pay+pay;

  if last.proc1 then output;
RUN;

DATA out.hpv_cs&yr;
    Set temp2;
RUN;


title2 "Find Number of people per age in VA, RI, DC";

PROC data = temp1;
  where (age LE 20 and age GE 9) and (egeoloc in ("31","38","08")); 
RUN;


ods excel file = "X:\staff\jjameson\HPV Vaccine Access\data\hpv_state_numbers&yr._.xlsx";
PROC freq data=temp1;
  class egeoloc;
  var age;
  title2"check age of patients in 20&yr";
RUN;
ods excel close;


%mend;

%xx(07,1);
%xx(08,1);
%xx(09,1);
%xx(10,1);
%xx(11,1);
%xx(12,1);
%xx(13,1);
%xx(14,1);
%xx(15,1);
%xx(16,1);
%xx(17,1);
%xx(18,1);
