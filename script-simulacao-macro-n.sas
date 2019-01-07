proc delete data=_all_;run;

%macro ODSOff(); /* call prior to BY-group processing */
ods graphics off;
ods exclude all; /* all open destinations */
ods results off; /* no updates to tree view */
options nonotes; /* optional, but sometimes useful */
%mend;

%macro ODSOn(); /* call after BY-group processing */
ods graphics on;
ods exclude none;
ods results on;
options notes;
%mend;

%include "D:\BETA_TRABALHO\scripts\score-fisher.sas";

%let mu  	= 0.05;
%let phi 	= 50;
%let B 		= 10;

%macro montecarlo;
	%do n=20 %to 100 %by 20; 
	/*Simulando dados*/
			data simulation;
				do b=1 to &B;
					do i=1 to &n;
						x = rand("BETA", &mu*&phi, (1-&mu)*&phi);
						if(find(x,1) = 1) then x = 0.999999999;
						if(x < 1e-16) then x=1e-16;
						output;
					end;
				end;
			run;
	/*Obtendo estimativas.*/
			proc nlmixed data=simulation df=1e6 technique=quanew update=bfgs;
				parms mu=&mu, phi=&phi;
				loglikehood=logpdf('BETA',x, mu*phi, (1-mu)*phi);
				model x ~ general(loglikehood);
				contrast "H0" mu-&mu, phi-&phi;
				by b;
				ods output Parameters=TRV1(drop=mu phi rename=(NegLogLike=lik_H0));
				ods output IterHistory=TRV2(keep=iter negloglike rename=(NegLogLike=lik_H1));
				ods output Contrasts=Wald(keep=b ProbF rename=(probf=pvalue_wald));
				ods output ConvergenceStatus=Conv(drop=reason);
			run;
	/*Ajustando teste da razão de verossimilhança.*/
			data trv2;
				set trv2 end=last;
				if not last then set trv2(firstobs=2 keep=iter rename=(iter=niter));
				if niter eq 1 or last then output;
				drop niter iter;
			run;
	/*Ajustando caso haja erro de otimização*/
			data trv2(drop=b status);
				merge trv2 conv;
				if(status=1) then delete;
			run;
			data trv(keep=b pvalue_trv);
				merge trv1 trv2;
				LR = 2*(-lik_H1 + lik_H0);
				pvalue_trv = 1-CDF('CHISQUARE', LR, 2); 
			run;
	/*Ajustando caso haja erro de otimização*/
			data trv(drop=status);
				merge trv conv;
				if(status=1) then delete;
			run;
	/*Ajustando teste escore.*/
			proc iml;
				load module = _all_; *lendo as funções escore e informção de fisher;
				use simulation;
				read all;
				close simulation;
				th 				= {&mu, &phi};
				p1 				= do(1,  &B*&n, &n);
				p2 				= do(&n, &B*&n, &n);
				pvalue_escore	= j(&B,1);
				do k=1 to &B;
					yy 					= x[p1[k]:p2[k]];
					T3 		 			= T(U(th,yy))*inv(If(th,yy))*U(th,yy);
					pvalue_escore[k] 	= 1 - CDF("CHISQUARE", T3, 2);
				end;
				create Escore var {"pvalue_escore"}; /* name the vars */
				append; /* write the data */
				close Escore;
			quit;
	/*Ajustando caso haja erro de otimização*/
			data escore(drop=status);
				merge escore conv;
				if(status=1) then delete;
			run;
			data results;
				merge trv wald escore;
				tipo1_trv    = (pvalue_trv <= 0.05);
				tipo1_wald   = (pvalue_wald <= 0.05);
				tipo1_escore = (pvalue_escore <= 0.05);
			run;
			proc freq data=results;
				tables tipo1_trv tipo1_wald tipo1_escore / nocum binomial(level="1" p=0.05);
				ods output binomial=dados&n(keep = table nvalue1 label1 where=(Label1='Proportion'));
			run;
			%if &n=20 %then %do; 
				data dados(drop=label1);
					set dados&n;
					n   = &n;
					phi = &phi;
					mu  = &mu;
				run;
			%end;
			%else %do;
				data dados&n(drop=label1);
					set dados&n;
					n   = &n;
					phi = &phi;
					mu  = &mu;
				run;
				data dados;
					set dados dados&n;
				run;
			%end;
	%end;
%mend;

%montecarlo;

proc export data = dados 
            outfile= "C:\Users\André Felipe\SkyDrive\Projeto-Beta\sim69.txt"
            dbms=dlm replace;
			delimiter=",";
			putnames=yes;
run;
