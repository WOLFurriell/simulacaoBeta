proc delete data=_all_;run;
*options nonotes nosource nosource2 errors=20;
%include "F:\BETA_TRABALHO\scripts\score-fisher.sas";

%let mu  = 0.95;
%let phi = 5;
%let n = 20;
%let B = 2;
/*Simulando dados*/
data simulation;
	call streaminit(1299);
	do b=1 to &B;
		do i=1 to &n;
			mu=&mu;
			phi=&phi;
			x = rand("BETA", &mu*&phi, (1-&mu)*&phi);
			if find(x,1) then x=0.999;
			output;
		end;
	end;
run;
/*Estimando parametros*/
proc nlmixed data=simulation df=1e6 technique=quanew update=bfgs;
	parms mu=&mu, phi=&phi;
	loglikehood=logpdf('BETA',x, mu*phi, (1-mu)*phi);
	model x ~ general(loglikehood);
	contrast "H0" mu-&mu, phi-&phi; 
	by b;
	ods output Parameters=TRV1(drop=mu phi rename=(NegLogLike=lik_H0));
	ods output IterHistory=TRV2(keep=iter negloglike rename=(NegLogLike=lik_H1));
	ods output ParameterEstimates=Par(keep=parameter estimate);
	ods output Contrasts=Wald(keep=b ProbF rename=(probf=pvalue_wald));
run;
/*Ajustando o teste da razão de verossimilhança*/
data trv2;
	set trv2 end=last;
	if not last then set trv2(firstobs=2 keep=iter rename=(iter=niter));
	if niter eq 1 or last then output;
	drop niter iter;
run;
data trv(keep=b pvalue_trv);
	merge trv1 trv2;
	LR = 2*(-lik_H1 + lik_H0);
	pvalue_trv = 1-CDF('CHISQUARE', LR, 2); 
run;
/*Calculando o teste escore*/
proc iml;
	load module = _all_; *lendo as funções escore e informção de fisher;
	use simulation;
	read all;
	close simulation;
	th 				= {&mu, &phi};
	p1 				= do(1,  &B*&n, &n);
	p2 				= do(&n, &B*&n, &n);
	pvalue_escore	= j(&B,1);
	T3	= j(&B,1);
	do k=1 to &B;
		yy 					= x[p1[k]:p2[k]];
		T3 		 			= T(U(th,yy))*inv(If(th,yy))*U(th,yy);
		pvalue_escore[k] 	= 1 - CDF("CHISQUARE", T3, 2);
	end;
	create Escore var {"pvalue_escore"}; /* name the vars */
	append; /* write the data */
	close Escore;
quit;
/*Ajustando todos os testes.*/
data results;
	merge trv wald escore;
	tipo1_trv    = (pvalue_trv <= 0.05);
	tipo1_wald   = (pvalue_wald <= 0.05);
	tipo1_escore = (pvalue_escore <= 0.05);
run;
/*Calculando a taxa do erro tipo I dos testes e aplicando o teste binomial exato.*/
proc freq data=results;
	tables tipo1_trv tipo1_wald tipo1_escore / nocum binomial(level="1" p=0.05);
	ods output binomial=dados1(keep = table nvalue1 label1 where=(Label1='Proportion'));
run;
data dados(drop=label1);
	set results;
	n  	= &n;
	mu 	= &mu;
	phi = &phi;
run;









	

