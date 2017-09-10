%let year = 2015;
%let drive = C:\Users\username\Documents\CE\&year;
%let stub = &drive\csxistub.txt;

%let yr1 = %substr(&year, 3, 2);
%let yr2 = %substr(%eval(&year + 1), 3, 2);
libname ce "&drive";
libname i&yr1 "&drive\intrvw&yr1";

* Set reporting variable information (must be a categorical variable);
%let reportvar_varlist = %str(purchased1-purchased3);
%let reportvar_nlevels = 3;
%let reportvar_labels =  %str(
    purchased1 = 'Did not purchase a vehicle since survey'
    purchased2 = 'Purchased a vehicle since survey'
    purchased3 = 'All CUs');
proc format; * Create a summary format;
    value $reportvar_levels (multilabel) '1' = '1' '2' = '2' '1', '2', ' ' = '3'
;
run;

/* BLS code (modified to use generic report variables) starts here;

/* reads in the stub parameter file and creates line numbers for uccs */
/* a unique line number is assigned to each expenditure line item     */
data stubfile (keep= count type level title ucc survey group line);
  infile "&stub."
  pad missover;
  input @1 type $1. @ 4 level $1. @7 title $char60. @70 ucc $6.
        @83 survey $1. @89 group $7.;
  if (type = '1');
  if group in ('CUCHARS' 'FOOD' 'EXPEND' 'INCOME');

    retain count 9999;
    count + 1;
    line = put(count, $5.)||level ;
run;

  /* maps line numbers to uccs */
data aggfmt1 (keep= ucc line line1-line10);
  set stubfile;
  length line1-line10 $6.;
    array lines(9) line1-line9;
      if (ucc > 'A') then
        lines(substr(line,6,1)) = line;
      retain line1-line9;
      if (ucc < 'A')  then 
        line10 = line;
  if (line10);
run;

proc sort data= aggfmt1 (rename=(line= compare));
  by ucc;
run;

proc transpose data= aggfmt1 out= aggfmt2 (rename=(col1= line));
  by ucc compare;
  var line1-line10;
run;

  /* aggregation file. extraneous mappings are deleted            */
  /* proc sql will aggregate line#/ucc pairs for proc format */
data aggfmt (keep= ucc line);
  set aggfmt2;
    if line;
    if substr(compare,6,1) > substr(line,6,1) or compare=line;
run;

proc sql noprint;
    select ucc, line, count(*) into :uccs separated by " ", :lines separated by 
        " ", :cnt from aggfmt;
quit;

run;

%macro mapping;
    %do i=1 %to  &cnt;
        "%scan(&uccs,&i,%str( ))"="%scan(&lines,&i,%str( ))"
    %end;
%mend mapping;

proc format;
    value $aggfmt (multilabel) %mapping other='other';
run;

/* label file. line numbers are assigned a text label */
/* dataset constructed to be read into a proc format  */
data lblfmt (rename=(line=start title=label));
    set stubfile (keep=line title);
    retain fmtname 'lblfmt' type 'C';
run;

proc format library=work cntlin=lblfmt;
run;

/* 1 read in the interview and diary fmly files & create mo_scope variable */
/* 2 read in the interview mtab/itab and diary expn/dtab files             */
/* 3 merge fmly and expenditure files to derive weighted expenditures      */
data fmly (keep=newid &reportvar. wtrep01-wtrep44 finlwt21 repwt1-repwt45);
    set ce.fmli_ovb;
    by newid;
    tempvar = put(&reportvar., 1.);
    drop &reportvar.;
    rename tempvar = &reportvar.;

    if firstqtr = 1 then
        mo_scope=(qintrvmo - 1);
    else if lastqtr = 1 then
        mo_scope=(4 - qintrvmo);
    else mo_scope=3;
    array reps_a(45) wtrep01-wtrep44 finlwt21;
    array reps_b(45) repwt1-repwt45;

    do i=1 to 45;
        if reps_a(i) > 0 then
            reps_b(i)=(reps_a(i) * mo_scope / 12);
        else reps_b(i)=0;
    end;

run;

   /* read in mtab and itab expenditure and income data */
   /* adjust ucc 710110 to annualize                    */
data expend (keep=newid ucc cost);
  set i&yr1..mtbi&yr1.1x
      i&yr1..mtbi&yr1.2
      i&yr1..mtbi&yr1.3
      i&yr1..mtbi&yr1.4
      i&yr1..mtbi&yr2.1

      i&yr1..itbi&yr1.1x (rename=(value=cost))
      i&yr1..itbi&yr1.2  (rename=(value=cost))
      i&yr1..itbi&yr1.3  (rename=(value=cost))
      i&yr1..itbi&yr1.4  (rename=(value=cost))
      i&yr1..itbi&yr2.1  (rename=(value=cost));
  
   if refyr = "&year" or  ref_yr = "&year";
   if ucc = '710110'  then  
      cost = (cost * 4); 
run;

proc sort data=expend;
    by newid;
run;

data pubfile (keep = newid ucc rcost1-rcost45 &reportvar.);
    merge fmly (in = infam) expend (in = inexp);
    by newid;

    if (inexp and infam);

    if (cost=.) then
        cost=0;
    array reps_a(45) wtrep01-wtrep44 finlwt21;
    array reps_b(45) rcost1-rcost45;

    do i=1 to 45;
        if reps_a(i) > 0 then
            reps_b(i)=(reps_a(i) * cost);
        else reps_b(i)=0;
    end;
run;

/* step3: calculate populations                                            */
/* sum all 45 weight variables to derive replicate populations            */
/* formats for correct column classifications                             */
proc summary nway data=fmly sumsize=max;
    class &reportvar. / mlf;
    var repwt1-repwt45;
    format &reportvar. $reportvar_levels.;
    output out=pop (drop=_type_ _freq_) sum=rpop1-rpop45;
run;

/* step4: calculate weighted aggregate expenditures                        */
/* sum the 45 replicate weighted expenditures to derive aggregates/ucc    */
* formats for correct column classifications;
proc summary nway data=pubfile sumsize=max completetypes;
    class ucc &reportvar. / mlf;
    var rcost1-rcost45;
    format &reportvar. $reportvar_levels.;
    output out=agg (drop=_type_ _freq_) sum=rcost1-rcost45;
run;

/* step5: calculate mean expenditures                                     */
/* 2 read in aggregate expenditures from agg dataset                       */
/* 3 calculate means by dividing aggregates by correct source populations  */
/* 4 sum expenditure means per ucc into correct line item aggregations     */

data avgs1 (keep=&reportvar ucc mean1-mean45);
   /* reads in pop dataset. _temporary_ loads populations into system memory  */
    array fizz{1:&reportvar_nlevels, 45} _temporary_;

    if _n_=1 then
        do i=1 to &reportvar_nlevels.;
            set pop;
            array reps{45} rpop1--rpop45;

            do j=1 to 45;
                fizz{&reportvar, j}=reps{j};
            end;
        end;

    /* reads in agg dataset and calculates means by dividing by populations  */
    set agg (keep=ucc &reportvar. rcost1-rcost45);

    array aggs(45) rcost1-rcost45;
    array avgs(45) mean1-mean45;

    do k=1 to 45;
        if aggs(k)=. then
            aggs(k)=0;
        avgs(k)=aggs(k) / fizz{&reportvar., k};
    end;
run;

/* sum ucc means to create aggregation scheme */
proc summary data=avgs1 nway completetypes;
    class &reportvar. ucc / mlf;
    var mean1-mean45;
    format ucc $aggfmt.;
    output out=avgs2 (drop=_type_ _freq_ rename=(ucc=line)) sum=;
run;

/*  calculate standard errors using replicate formula                      */
data se (keep=&reportvar. line mean se);
    set avgs2;
    array rmns(44) mean1-mean44;
    array diff(44) diff1-diff44;

    do i=1 to 44;
        diff(i)=(rmns(i) - mean45)**2;
    end;

    mean=mean45;
    se=sqrt((1/44)*sum(of diff(*)));
run;

/* step7: tabulate expenditures                                            */
/*arrange data into tabular form */
proc sort data=se;
    by line &reportvar.;

proc transpose data = se out = tab1 name = estimate prefix = &reportvar.;
    by line;
    var mean se;
run;

/* set aside populations from interview */
proc transpose data=pop (keep=rpop45) out=cus name=line prefix=&reportvar.;
    var rpop45;
run;

/* insert population line item into table and assign line number */
data tab2;
    set cus tab1;

    if line='rpop45' then
        do;
            line='100001';
            estimate='n';
        end;
run;

proc sort data = tab2;
    by line;
run;

/* merge stubfile back into table to insert expenditure lines */
/* that had zero expenditures for the year                    */
data tab;
    merge tab2 stubfile;
    by line;

    if line ne '100001' then
        do;
            if survey='s' then
                delete;
        end;

    array cntrl(&reportvar_nlevels) &reportvar_varlist;

    do i=1 to &reportvar_nlevels.;
        if cntrl(i)=. then
            cntrl(i)=0;

        if sum(of cntrl(*))=0 then
            estimate='Mean';
    end;

    if group in ('CUCHARS' 'INCOME') then
        do;
            if lag(line)=line then
                delete;
        end;

    if title="Percent distribution:" then
        delete;
run;

proc tabulate data = tab;
    class line / groupinternal order=data;
    class estimate;
    var &reportvar_varlist.;
    format line $lblfmt.;

    table (line * estimate), (&reportvar_varlist.) *sum='' / rts=25;
        label estimate=estimate line=line &reportvar_labels.;
        options nodate nocenter nonumber ls=167 ps=max;
        where line ne 'other';
        title "Integrated Expenditures for &year";
run;
