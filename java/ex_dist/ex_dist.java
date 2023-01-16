/* ex_dist.java

	Test built-in distribution functions and the callback interface
	for user-defined distributions. The use of different sampling
	modes are also shown.
*/


import com.lindo.*;

class ex_userdata
{
	int counter;
}


public class ex_dist extends Lindo
{
	private static int nErrorCode[] = new int[1];
	private static StringBuffer cErrorMessage = new StringBuffer(LS_MAX_ERROR_MESSAGE_LENGTH);
	private static StringBuffer cLicenseKey = new StringBuffer(LS_MAX_ERROR_MESSAGE_LENGTH);
	private static Object pSamp = null;
	/* declare an instance of the LINDO environment object */
	private static Object /*long*/ pEnv = null;


	// Generalized error Reporting function
	private static void APIErrorCheck(Object /*long*/ pEnv )
	{
		if(0 != nErrorCode[0])
		{
			System.out.printf("\nError=%d",nErrorCode[0]);
			try
			{
				LSgetErrorMessage(pEnv, nErrorCode[0], cErrorMessage);
				System.out.println("\nError " + nErrorCode[0] + ": " + cErrorMessage);
			}
			catch (Exception e)
			{
				System.out.println("\nError " + nErrorCode[0] + ": " + " Unknown error");
			}
			System.out.println();
			System.exit(1);
		}
	}


	// Version Reporting function
	private static void APIVERSION()
	{
		StringBuffer szVersion = new StringBuffer(255);
		StringBuffer szBuild   = new StringBuffer(255);
		LSgetVersionInfo(szVersion, szBuild);
		System.out.println("\nLINDO API Version "+szVersion.toString() + " built on " + szBuild.toString());
		System.out.println();
	}

	static
	{
		// The runtime system executes a class's static
		// initializer when it loads the class.
		System.loadLibrary("lindojni");
	}

	// A user defined distribution -  a built-in funciton is used inside to keep it simple
	private static int  UserDistr(Object pSample, int nFuncType, double padInput[], int nInput, double pdOutput[], Object userData)
	{
		if (pSamp == null)
		{
			pSamp = LSsampCreate(pEnv, LSDIST_TYPE_NORMAL, nErrorCode);
			if (nErrorCode[0]!=LSERR_NO_ERROR) return nErrorCode[0];
			nErrorCode[0] = LSsampSetDistrParam(pSamp, 0,  2.0);
			if (nErrorCode[0]!=LSERR_NO_ERROR) return nErrorCode[0];
			nErrorCode[0] = LSsampSetDistrParam(pSamp, 1, 10.0);
			if (nErrorCode[0]!=LSERR_NO_ERROR) return nErrorCode[0];
		}

		if (nFuncType == LS_CDFINV)
			nErrorCode[0] = LSsampEvalDistr(pSamp,LS_CDFINV, padInput[0], pdOutput);
		else if (nFuncType == LS_CDF)
			nErrorCode[0] = LSsampEvalDistr(pSamp,LS_CDF, padInput[0], pdOutput);
		else if (nFuncType == LS_PDF)
			nErrorCode[0] = LSsampEvalDistr(pSamp,LS_PDF, padInput[0], pdOutput);
		else if (nFuncType == LS_PDFDIFF)
			nErrorCode[0] = LSsampEvalDistr(pSamp,LS_PDFDIFF, padInput[0], pdOutput);

		return nErrorCode[0];
	}

	public static void main (String[] args)
	{
		ex_dist ls = new ex_dist();

		/* >>> Step 1 <<< Read license file and create a LINDO environment. */
		nErrorCode[0] = ls.LSloadLicenseString("../../license/lndapi140.lic",cLicenseKey);
		APIErrorCheck(pEnv);

		APIVERSION();
		pEnv = ls.LScreateEnv(nErrorCode, cLicenseKey.toString());
		APIErrorCheck(pEnv);


		Object paSample[] = null, pUserSample = null, pRG = null;

		int itmp[], i, j;
		int DISCRETE=0; /*set this to 1 to use the distribution specified below */
		int nDistType = LSDIST_TYPE_NORMAL;
		int nSamp = 10, nDim = 3;
		double dMean = 2.0, dXbar[];
		double dVar = 10.0, dStd[], utmp[], dtmp[];
		double pX[]=null;
		/* A PDF table for illustrating sampling from a discrete
		    distribution */
		/*
		    double adPdfTable[] = new double []  { 0.05, 0.05, 0.05, // .15
		 0.10, 0.15, 0.15, // .40
		 0.08, 0.07, 0.25, // .40
		 0.05 };           //1.00
			*/

		double adPdfTable[] = new double []  {0.02, 0.07, 0.09,
				0.12, 0.20, 0.20,
				0.18, 0.10, 0.01,
				0.01
		};

		int anCntTable[][] = new int[10][3];

		paSample = new Object[nDim];
		pX = new double [nSamp];
		itmp = new int[1];
		utmp = new double[1];
		dtmp = new double[1];

		pRG = LScreateRG(pEnv, LS_RANDGEN_LINDO2);
		LSsetRGSeed(pRG, 1031);

		System.out.printf("\nTesting uniform double generation in (0,1)\n");
		for (i=0; i<10; i++)
		{
			utmp[0] = LSgetDoubleRV(pRG);
			nErrorCode[0] = LSsampEvalDistr(pUserSample,LS_CDFINV,utmp[0],dtmp);
			System.out.printf("%d: ux=%13.7f cdf=%13.7f\n",i,utmp[0],dtmp[0]);
		}

		System.out.printf("\nTesting discrete uniform generation in U[100,200]\n");
		for (i=0; i<10; i++)
		{
			itmp[0] = LSgetInt32RV(pRG,100,200);
			System.out.printf("%d: ix=%13d\n",i,itmp[0]);
		}

		System.out.printf("\nTesting sampling from distributions\n");
		for (j=0; j<nDim; j++)
		{
			if (DISCRETE==0)
			{
				paSample[j] = LSsampCreate(pEnv, nDistType, nErrorCode);
				APIErrorCheck(pEnv);


				nErrorCode[0] = LSsampSetDistrParam(paSample[j], 0, dMean);
				APIErrorCheck(pEnv);


				nErrorCode[0] = LSsampSetDistrParam(paSample[j], 1, dVar);
				APIErrorCheck(pEnv);

			}
			else
			{
				for (i=0; i<10; i++) anCntTable[i][j] = 0;

				paSample[j] = LSsampCreate(pEnv, LSDIST_TYPE_DISCRETE, nErrorCode);
				APIErrorCheck(pEnv);


				nErrorCode[0] = LSsampLoadDiscretePdfTable(paSample[j], 10, adPdfTable,null);
				APIErrorCheck(pEnv);

			}

			nErrorCode[0] = LSsampSetRG(paSample[j],pRG);
			APIErrorCheck(pEnv);

		}

		nErrorCode[0] = LSsampGenerate(paSample[0], LS_LATINSQUARE, nSamp);
		APIErrorCheck(pEnv);

		nErrorCode[0] = LSsampGenerate(paSample[1], LS_ANTITHETIC, nSamp);
		APIErrorCheck(pEnv);

		nErrorCode[0] = LSsampGenerate(paSample[2], LS_MONTECARLO, nSamp);
		APIErrorCheck(pEnv);


		for (j=0; j<nDim; j++)
		{
			nErrorCode[0] = LSsampGetPoints(paSample[j],itmp,pX);
			System.out.printf("\n%8s %3s %13s %13s %13s\n","sample","k","x","u=CDF(x)","x'=CDFINV(u)");
			for (i=0; i<nSamp; i++)
			{
				APIErrorCheck(pEnv);
				;
				nErrorCode[0] = LSsampEvalDistr(paSample[j],LS_CDF,pX[i],utmp);
				nErrorCode[0] = LSsampEvalDistr(paSample[j],LS_CDFINV,utmp[0],dtmp);
				System.out.printf("%8d %3d %13.7f %13.7f %13.7f\n",j,i,pX[i],utmp[0],dtmp[0]);
				if (DISCRETE==1)
				{
					itmp[0] = (int) pX[i];
					anCntTable[itmp[0]][j]++;
				}
			}
		}

		if (DISCRETE==1)
		{
			dMean = 0.0;
			dVar = 0;
			for (j=0; j<10; j++)
			{
				dMean += adPdfTable[j]*j;        //E[x]
				dVar += Math.pow(j,2)*adPdfTable[j];  //E[x^2]
			}
			dVar -= dMean*dMean;
			dVar = Math.sqrt(dVar);  //population stdev : sqrt(E[x^2] - E[x]^2)
		}


		dXbar = new double[1];
		dStd = new double[1];
		System.out.print("\nSample info\n");
		for (j=0; j<nDim; j++)
		{
			nErrorCode[0] = LSsampGetInfo(paSample[j],LS_DINFO_SAMP_MEAN,dXbar);
			nErrorCode[0] = LSsampGetInfo(paSample[j],LS_DINFO_SAMP_STD,dStd);
			System.out.printf("Sample :%d,  xbar=%13.7f (err=%%%7e), stdev=%13.7f (err=%%%7e)\n",
				j,dXbar[0],Math.abs(dMean-dXbar[0])*100/dMean,dStd[0],Math.abs(dStd[0]-dVar)*100/dVar);
			//j,dXbar,dMean,dStd,dVar);
		}

		if (DISCRETE==1)
		{
			System.out.print("\nObservation counts\n");
			for (i=0; i<10; i++)
			{
				System.out.printf("%2d: ",i);
				for (j=0; j< nDim; j++)
				{
					System.out.printf("%6d ",anCntTable[i][j]);
				}
				System.out.print("\n");
			}
		}
		/// test user cdfinv
		if (1==1)
		{
			System.out.printf("\nTesting user distribution function through callback interface\n");
			pUserSample = LSsampCreate(pEnv, LSDIST_TYPE_USER, nErrorCode);
			APIErrorCheck(pEnv);
			nErrorCode[0] = LSsampSetRG(pUserSample,pRG);
			APIErrorCheck(pEnv);
			nErrorCode[0] = LSsampSetUserDistr(pUserSample,"UserDistr",ls);
			APIErrorCheck(pEnv);
			System.out.print("Installed user distribution\n");
			nErrorCode[0] = LSsampGenerate(pUserSample, LS_LATINSQUARE, nSamp);
			APIErrorCheck(pEnv);
			nErrorCode[0] = LSsampGetPoints(pUserSample,itmp,pX);
			System.out.printf("\n%s %3s %13s %13s %13s\n"," ","idx","x","u=CDF(x)","x'=CDFINV(u)");
			for (i=0; i<nSamp; i++)
			{
				APIErrorCheck(pEnv);
				;
				LSsampEvalDistr(pUserSample,LS_CDF,pX[i],utmp);
				LSsampEvalDistr(pUserSample,LS_CDFINV,utmp[0],dtmp);
				System.out.printf("%d:%3d %13.7f %13.7f %13.7f\n",j,i,pX[i],utmp[0],dtmp[0]);
			}
		}
		System.out.print("End of run\n");

		LSdisposeRG(pRG);

		LSsampDelete(pSamp);
		LSsampDelete(pUserSample);
		LSsampDelete(paSample[0]);
		LSsampDelete(paSample[1]);
		LSsampDelete(paSample[2]);
		/* >>> Step 6 <<< Delete the LINDO environment */
		nErrorCode[0] = ls.LSdeleteEnv( pEnv);

	}
}
