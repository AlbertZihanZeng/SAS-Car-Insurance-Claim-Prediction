*
Car Insurance Claim Prediction

Data Science Project by SAS
Authors: Albert Zihan Zeng
Date: 2024-01-06

Dataset downloaded from:
https://www.kaggle.com/datasets/sagnik1511/car-insurance-data/data


*********************   Data Import   *****************************;
proc import
 datafile="C:\Users\zihan\Documents\Metro College\14. DS Project by SAS\Project
\Car_Insurance_Claim.csv"
 out=data0
 dbms=csv replace;
 getnames=Yes;
run;
proc print data=data0 (obs=50); run;
proc contents data=data0; run;



*********************   Univariate Analysis   **********************;
proc means data=data0 n nmiss mean min q1 median q3 max stddev lclm uclm maxdec=3;
 var id credit_score annual_mileage speeding_violations duis past_accidents;
run;
proc freq data=data0 (drop=id credit_score);
 table _all_ /missing;
run;


***** Credit Score;
proc sgplot data=data0;
 title "Credit Score Distribution";
 histogram credit_score;
 density credit_score;
 density credit_score / type=kernel;
 keylegend / location=inside position=topright;
run; title;
proc sgplot data=data0;
 title "Credit Score by Outcome";
 vbox credit_score / category=outcome;
run; title;
proc univariate data=data0 (drop=id) normal plot;
 class outcome;
 var credit_score;
 histogram credit_score;
run;

***** Catagorical Variables;
*store all catagorical variables into list VAR_CAT;
proc sql noprint;
 select name into :VAR_CAT separated by " "
 from dictionary.columns
 where libname = 'WORK' and memname = 'DATA0'
  and name not in ('ID','CREDIT_SCORE')
;quit;
%put &VAR_CAT;

*macro to plot each variable;
%macro univar_plot(var,table=data0);
proc sgplot data=&table;
 title "&var Distribution";
 vbar &var / group=outcome groupdisplay=stack;
run; title;
%mend;

*macro to loop over variables;
%macro univar(vlist/*list by space*/,table=data0/*table name*/);
%let nvar=%sysfunc(countw(&vlist));
%do i=1 %to &nvar;
	%let var=%scan(&vlist,&i);
	%univar_plot(&var,table=&table);
%end;
%mend;

%univar(&VAR_CAT);

*annual_mileage;
proc univariate data=data0 normal;
 var annual_mileage;
 class outcome;
 histogram annual_mileage;
run;



*********************   Bivariate Analysis   **********************;
***** Categorical vs. Categorical Variables;
%macro bivar_chisq(var1,var2,table=data0);
proc freq data=&table;
 title "Chi-Square Test for &var1 vs. &var2";
 table &var1*&var2 /chisq nopercent norow;
run; title;
%mend;

%macro bivar_cat(var1,vlist=&var_cat,table=data0);
%let nvar=%sysfunc(countw(&vlist));
%do i=1 %to &nvar;
	%let var2=%scan(&vlist,&i);
	%if %sysfunc(prxmatch(s/^&var1$//,&var2))=0
		%then %bivar_chisq(&var1,&var2,table=&table);
%end;
%mend;

%bivar_cat(OUTCOME);
* OUTCOME has no significant relationship with RACE by chi-square test;
%bivar_cat(RACE);
%bivar_cat(GENDER);

***** Continuous Variables;
%let VAR_CAT_BI =GENDER RACE VEHICLE_OWNERSHIP VEHICLE_YEAR
	MARRIED CHILDREN VEHICLE_TYPE OUTCOME;
%let VAR_CAT_MULTI =AGE DRIVING_EXPERIENCE EDUCATION INCOME POSTAL_CODE
	ANNUAL_MILEAGE SPEEDING_VIOLATIONS DUIS PAST_ACCIDENTS;
%let VAR_CONT =CREDIT_SCORE;

%macro bivar_ttest(var1,var2,table=data0);
proc ttest data=&table;
 class &var2;
 var &var1;
 title "T-test for &var1 by &var2";
run; title;
%mend;
%macro bivar_anova(var1,var2,table=data0);
proc glm data=&table;
 class &var2;
 model &var1=&var2;
 means &var2;
 title "Anova Test for &var1 by &var2";
run; title;
%mend;
%macro bivar_cont(var1,vlist2=&var_cat_bi /*variable list for ttest*/,
	vlist3=&var_cat_multi /*variable list for anova test*/,table=data0,graph=off);
ods graphics &graph;
%if %length(&vlist2)=0 %then %let nvar2=0;
%else %let nvar2=%sysfunc(countw(&vlist2));
%do i=1 %to &nvar2;
	%let var2=%scan(&vlist2,&i);
	%bivar_ttest(&var1,&var2,table=&table);
%end;
%if %length(&vlist3)=0 %then %let nvar3=0;
%else %let nvar3=%sysfunc(countw(&vlist3));
%do j=1 %to &nvar3;
	%let var3=%scan(&vlist3,&j);
	%bivar_anova(&var1,&var3,table=&table);
%end;
ods graphics off;
%mend;

%bivar_cont(CREDIT_SCORE);
* CREDIT_SCORE has no significant relationship with VEHICLE_TYPE, POSTAL_CODE;
* ANNUAL_MILEAGE, SPEEDING_VIOLATIONS, DUIS, PAST_ACCIDENTS have too many levels,
 these variables will be checked again after reducing levels;
%bivar_cont(CREDIT_SCORE,vlist2=VEHICLE_TYPE,vlist3=POSTAL_CODE,graph=on);



*********************   Preparation   **********************;
***** Outliers;
%univar(DUIS PAST_ACCIDENTS SPEEDING_VIOLATIONS);

* Reducing levels in DUIS PAST_ACCIDENTS SPEEDING_VIOLATIONS;
data data1;
 set data0;
 if duis>=2 then DUIS_GRP="2+";
 else duis_grp=duis;
 if past_accidents>=4 then PAST_ACCIDENTS_GRP="4+";
 else past_accidents_grp=past_accidents;
 if speeding_violations>=3 then SPEEDING_VIOLATIONS_GRP="3+";
 else speeding_violations_grp=speeding_violations;
 drop duis past_accidents speeding_violations;
run;
proc print data=data1 (obs=50); run;

%univar(DUIS_GRP PAST_ACCIDENTS_GRP SPEEDING_VIOLATIONS_GRP, table=data1);


***** Feature engineering;
* Grouping ANNUAL_MILEAGE;
proc sql;
 create table data_am as
 select annual_mileage, count(*) as n, sum(outcome) as n_claim,
  sum(outcome)/count(*) as claim_rate
 from data1
 where annual_mileage>0
 group by annual_mileage
 order by annual_mileage
;quit;
proc print data=data_am; title "Claims by Annual Mileage";run;title;
proc sgplot data=data_am;
 title 'Claim Rate by Annual Mileage';
 vbar annual_mileage /response=claim_rate;
run;title;
data data1;
 set data1;
 length ANNUAL_MILEAGE_GRP $12;
 if missing(annual_mileage) then ANNUAL_MILEAGE_GRP="";
 else if annual_mileage<=8000 then ANNUAL_MILEAGE_GRP="0-8000";
 else if annual_mileage<=12000 then ANNUAL_MILEAGE_GRP="9000-12000";
 else if annual_mileage<=16000 then ANNUAL_MILEAGE_GRP="13000-16000";
 else ANNUAL_MILEAGE_GRP="16000+";
 drop annual_mileage;
run;

%univar(ANNUAL_MILEAGE_GRP, table=data1);


***** Missing values in ANNUAL_MILEAGE;
proc sql noprint;
 select name into :VAR_CATX1 separated by " "
 from dictionary.columns
 where libname = 'WORK' and memname = 'DATA1'
  and name not in ('ID','CREDIT_SCORE','OUTCOME')
;quit;
%let VAR_CATX1_AM=%sysfunc(prxchange(s/ANNUAL_MILEAGE_GRP//,-1,&var_catx1));
%put &var_catx1;
%put &var_catx1_am;

* check correlations;
ods graphics on;
proc logistic data=data1 plots(only)=(effect oddsratio);
 class &var_catx1_am;
 model  annual_mileage_grp = &var_catx1_am credit_score;
run; quit;
ods graphics off;

* impute missing values;
ods select misspattern;
proc mi data=data1 nimpute=1 out=data1_imp1 seed=42;
 class annual_mileage_grp driving_experience married children postal_code;
 var annual_mileage_grp driving_experience married children postal_code;
 fcs logistic;
run;
ods select all;

* result compare;
proc freq data=data1;
 table annual_mileage_grp;
run;
proc freq data=data1_imp1;
 table annual_mileage_grp;
run;
%univar(annual_mileage_grp,table=data1_imp1);


***** Missing values in CREDIT_SCORE;
* check credit_score correlations;
ods graphics on;
proc glm data=data1_imp1;
 class &var_catx1;
 model  credit_score = &var_catx1 / solution ss3;
 *lsmeans &var_catx1 /pdiff stderr cl;
run; quit;
ods graphics off;
* highly correlated with GENDER INCOME PAST_ACCIDENTS_GRP;

* impute missing values;
ods select misspattern;
proc mi data=data1_imp1 nimpute=1 out=data1_imp2 seed=42;
 class gender income past_accidents_grp;
 var credit_score gender income past_accidents_grp;
 fcs logistic regpmm;
run;
ods select all;

* result compare;
proc means data=data1 missing;
 var credit_score;
run;
proc means data=data1_imp2 missing;
 var credit_score;
run;
proc univariate data=data1_imp2;
 var credit_score;
 histogram credit_score;
 class outcome;
run;



***** Split train and test datasets;
proc sort data=data1_imp2;
 by outcome;
run;
proc surveyselect
 data=data1_imp2 rate=0.8 seed=42
 out=surv_result outall;
 strata outcome;
run;
data traindata testdata;
 set surv_result;
 if selected=1 then output traindata;
 else output testdata;
 drop selected;
run;




*********************   Modeling   **********************;
***** Logistic regression;
ods graphics on;
proc logistic data=traindata plots=ROC;
 class &var_catx1;
 model outcome(event="1")=&var_catx1 credit_score /details lackfit;
 score data=testdata out=testpred outroc=vroc;
 roc; roccontrast;
 output out=outputdata p=prob_predicted xbeta=linpred;
run; quit;
ods graphics off;

*lasso;
ods graphics on;
proc hpgenselect data=traindata;
 class &var_catx1;
 model outcome(event="1")=&var_catx1 credit_score /distribution=binary cl;
 selection method=lasso (choose=SBC) details=all;
 output out=output1 p=prodlasso;
run; quit;
ods graphics off;

*gam;
ods graphics on;
proc gam data=traindata plots=all;
 class &var_catx1;
 model outcome(event="1")=param(&var_catx1 credit_score) /dist=binomial;
 score data=testdata out=output_gam;
 output out=outputdata_gam p=prob_predicted_gam all;
run; quit;
ods graphics off;

***** Model evaluation;
proc sort data=testpred;
 by descending i_outcome descending f_outcome;
run;
proc freq data=testpred order=data;
 tables f_outcome*i_outcome /senspec;
run;
