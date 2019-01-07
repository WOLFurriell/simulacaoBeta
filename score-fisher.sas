proc iml;
	/*Função Escore*/
	start U(theta, x);
		  n 	= nrow(x);
		  mu 	= theta[1];
		  phi 	= theta[2];
		  Es 	= j(2,1);
		  Ad 	= digamma(mu*phi);
		  Bd 	= digamma(phi-phi*mu);
		  Es[1] = n*phi*Bd - n*phi*Ad + phi*sum(log(x)) - phi*sum(log(1-x));
		  Es[2] = n*digamma(phi) + (n*(mu -1))*Bd - n*mu*Ad + mu*sum(log(x)) + (1-mu)*sum(log(1-x));
		return(Es);
	finish;
	/*Matriz de informação esperada*/
	start If(theta, x);
		 n 		= nrow(x);
		 mu		= theta[1];
		 phi	= theta[2];
		 K		= j(2,2);
		 At		= trigamma(mu*phi);
		 Bt		= trigamma(phi-phi*mu);
		 K[1,1] = n*(phi**2)*At + n*(phi**2)*Bt;
		 K[1,2] = n*mu*phi*At + n*digamma(mu*phi) - n*(1-mu)*phi*Bt - n*digamma(phi-phi*mu) - sum(log(x)) + sum(log(1-x));
		 K[2,1] = K[1,2];
		 K[2,2] = n*(mu**2)*At + n*((mu-1)**2)*Bt - n*trigamma(phi);
		return(K);
	finish;
	store _all_ module=_all_;
quit;
