/*
###################################################################
#                       LINDO-API
#                    Sample Programs
#                  Copyright (c) 2001-2002
#
#         LINDO Systems, Inc.           312.988.7422
#         1415 North Dayton St.         info@lindo.com
#         Chicago, IL 60622             http://www.lindo.com
###################################################################

  File   : ex_sp_smps.java

  Purpose: Read an SP model from an SMPI/SMPS file and solve it.

  Models:

  1) lotsize.* : stochastic lotsizing with discrete and continuous
  			random parameters (sampling is required).

  2) gascompany.* : stochastic model with discrete random parameters

*/

import com.lindo.*;

class ex_userdata
{
	int numCback;
	int numBestK;
}



public class ex_sp_smps extends Lindo
{
	private static int nErrorCode[] = new int[1];
	private static StringBuffer cErrorMessage = new StringBuffer(LS_MAX_ERROR_MESSAGE_LENGTH);
	private static StringBuffer cLicenseKey = new StringBuffer(LS_MAX_ERROR_MESSAGE_LENGTH);

	private static int iter[] = new int[1];
	private static int n_vars[] = new int[1];
	private static double pobj[] = new double[1];
	private static double bestbound[] = new double[1];
	private static double pinf[] = new double[1];

	private static  Object pEnv = null;
	private static  Object pModel = null;

	private static ex_userdata mydata = new ex_userdata();


	private static void jLogback(Object pMod, String szMessage, Object pls)
	{
		System.out.print(szMessage);
	}

	/* A callback function for continuous models */
	private static int jCallback(Object pMod, int nLoc, Object pls)
	{
		int ncalls = 0;
		ex_sp_smps nls=null;
		ex_userdata _mydata;
		try
		{
			nls = (ex_sp_smps) pls;
			_mydata = (ex_userdata) nls.mydata;

			_mydata.numCback++;
			ncalls = _mydata.numCback;
		} catch (Exception e)
		{
			System.out.println(e.toString());
		}


		LSgetCallbackInfo(pMod,0,LS_DINFO_PINFEAS,pinf);
		LSgetCallbackInfo(pMod,0,LS_DINFO_POBJ,pobj);
		LSgetCallbackInfo(pMod,0,LS_IINFO_SIM_ITER,iter);
		System.out.printf("\n@callback calls=%3d, iter=%8d, pobj = %+.6e,  pinf = %.6e",
			ncalls,iter[0],pobj[0],pinf[0]);
		return 0;
	}

	/* A callback function for integer models */
	private static int jMIPCallback(Object pMod, int nLoc, Object pls)
	{
		int ncalls = 0;
		ex_sp_smps nls=null;
		ex_userdata _mydata;
		try
		{
			nls = (ex_sp_smps) pls;
			_mydata = (ex_userdata) nls.mydata;

			_mydata.numCback++;
			ncalls = _mydata.numCback;
		} catch (Exception e)
		{
			System.out.println(e.toString());
		}

		// return if not calling from MIP optimizer
		if (nLoc != LSLOC_MIP) return 0;

		LSgetCallbackInfo(pMod,0,LS_DINFO_MIP_BESTBOUND,bestbound);
		LSgetCallbackInfo(pMod,0,LS_DINFO_MIP_OBJ,pobj);
		LSgetCallbackInfo(pMod,0,LS_IINFO_MIP_SIM_ITER,iter);
		System.out.printf("\n@callback calls=%3d, iter=%8d, pobj = %+.6e,  bestbnd = %.6e",
			ncalls,iter[0],pobj[0],bestbound[0]);
		return 0;
	}

	/* A callback function to be called at every new integer solution (for integer models) */
	private static int jNewMIPCallback(Object pMod, Object pls, double obj, double x[])
	{
		int ncalls = 0;
		ex_sp_smps nls=null;
		ex_userdata _mydata;
		try
		{
			nls = (ex_sp_smps) pls;
			_mydata = (ex_userdata) nls.mydata;

			_mydata.numCback++;
			ncalls = _mydata.numCback;
		} catch (Exception e)
		{
			System.out.println(e.toString());
		}

		LSgetCallbackInfo(pMod,0,LS_DINFO_MIP_BESTBOUND,bestbound);
		LSgetCallbackInfo(pMod,0,LS_DINFO_MIP_OBJ,pobj);
		LSgetCallbackInfo(pMod,0,LS_IINFO_MIP_SIM_ITER,iter);
		System.out.printf("\n@new integer solution=%3d, iter=%8d, pobj = %+.6e,  bestbnd = %.6e (*)",
			ncalls,iter[0],pobj[0],bestbound[0]);
		return 0;
	}

	/* A callback function to be called at next best integer solution (for integer models) */
	private static int jNextMIPCallback(Object pMod, Object pls, double obj, double x[])
	{
		int ncalls = 0;
		int nbestk = 0;
		ex_sp_smps nls=null;
		ex_userdata _mydata;
		try
		{
			nls = (ex_sp_smps) pls;
			_mydata = (ex_userdata) nls.mydata;

			_mydata.numCback++;
			_mydata.numBestK++;
			nbestk=_mydata.numBestK;
		} catch (Exception e)
		{
			System.out.println(e.toString());
		}

		LSgetCallbackInfo(pMod,0,LS_DINFO_MIP_BESTBOUND,bestbound);
		LSgetCallbackInfo(pMod,0,LS_DINFO_MIP_OBJ,pobj);
		LSgetCallbackInfo(pMod,0,LS_IINFO_MIP_SIM_ITER,iter);
		System.out.printf("\n@next best integer solution=%3d, iter=%8d, pobj = %+.6e,  bestbnd = %.6e (*)",
			nbestk,iter[0],pobj[0],bestbound[0]);
		return 0;
	}

	// Generalized error Reporting function
	private static void APIErrorCheck(Object pEnv )
	{
		if(0 != nErrorCode[0])
		{
			LSgetErrorMessage(pEnv, nErrorCode[0], cErrorMessage);
			System.out.println("\nError " + nErrorCode[0] + ": " + cErrorMessage);
			System.out.println();
			System.exit(1);
		}
	}

	static
	{
		// The runtime system executes a class's static initializer when it loads the class.
		System.loadLibrary("lindojni");
	}


	// Version Reporting function
	private static void APIVERSION()
	{
		StringBuffer szVersion = new StringBuffer(1024);
		StringBuffer szBuild   = new StringBuffer(1024);
		LSgetVersionInfo(szVersion, szBuild);
		System.out.println("\nLINDO API Version "+szVersion.toString() + " built on " + szBuild.toString());
		System.out.println();
	}

	// The main program
	public static void main(String[] args)
	{
		int i,j;
        int numScens[] = new int[1];
        double dProb[] = new double[1];
        int numStocPars[] = new int[1];

		double dObj[]=new double[1],dEvpi[]=new double[1];
		int nStatus[] = new int[1];
		int verbose = 1;

		// construct a new instance of the class
		ex_sp_smps ls = new ex_sp_smps();

		// Read license file and create a LINDO environment.
		nErrorCode[0] = ls.LSloadLicenseString("../../license/lndapi150.lic",cLicenseKey);
		APIErrorCheck(pEnv);

		APIVERSION();
		pEnv = LScreateEnv(nErrorCode, cLicenseKey.toString());
		APIErrorCheck(pEnv);

		pModel = ls.LScreateModel(pEnv,nErrorCode);
		APIErrorCheck(pEnv);


		nErrorCode[0] = LSsetModelIntParameter(pModel,LS_IPARAM_NLP_LINEARZ,1);
		APIErrorCheck(pEnv);

		nErrorCode[0] = LSsetModelIntParameter(pModel,LS_IPARAM_STOC_DEBUG_MASK,0);
		APIErrorCheck(pEnv);

		nErrorCode[0] = LSsetModelIntParameter(pModel,LS_IPARAM_STOC_PRINT_LEVEL,2);
		APIErrorCheck(pEnv);

		System.out.println("\nOptimizing Stochastic Newsboy Model\n");
		if (1==1)
		{
			nErrorCode[0] = ls.LSsetModelLogfunc(pModel,"jLogback",ls);
		}
		else
		{
			nErrorCode[0] = ls.LSsetCallback(pModel,"jMIPCallback",ls);
		}
		APIErrorCheck(pEnv);

		mydata.numCback = 0; // total number of callbacks
		mydata.numBestK = 0; // number of best-k integer solutions (for integer models)

		/*
		* Read CORE, TIME and STOC files in SMPI format
		*
		* Edit the MPI file to see how stochastic parameters are
		* marked with EP_PUSH_SPAR macro.
		*/
		nErrorCode[0] = LSreadSMPIFile(pModel,"ex_sp_smps/lotsize.mpi","ex_sp_smps/lotsize.time","ex_sp_smps/lotsize_mix.stoch");
		//nErrorCode[0] = LSreadSMPIFile(pModel,"ex_sp_smps/gascompany.mpi","ex_sp_smps/gascompany.time","ex_sp_smps/gascompany.stoch");
		APIErrorCheck(pEnv);


		/* Load sample sizes per stage */
		// Try different seeds when repeating runs with sampling
		nErrorCode[0] = LSsetModelIntParameter(pModel,LS_IPARAM_STOC_RG_SEED,1033);
		APIErrorCheck(pEnv);

		int   panSampleSize[]   = new int[] { 0, 2, 2, 2, 2, 2};
		// (no sampling needed for gascompany model)*/
		nErrorCode[0] = LSloadSampleSizes(pModel,panSampleSize);
		APIErrorCheck(pEnv);

		nErrorCode[0] = LSsolveSP(pModel,nStatus);
		APIErrorCheck(pEnv);

        nErrorCode[0] = LSgetInfo(pModel,LS_IINFO_NUM_SPARS,numStocPars);
        System.out.printf("\n\nNumber of stoch. params = %d\n" , numStocPars[0]);
        double padOutcome[] = new double[numStocPars[0]];
		nErrorCode[0] = LSgetStocInfo(pModel,LS_IINFO_STOC_NUM_SCENARIOS, 0, numScens);
		APIErrorCheck(pEnv);
		/*
		*     Access the final solution if optimal or feasible
		*/
		if (nStatus[0] == LS_STATUS_OPTIMAL ||
			nStatus[0] == LS_STATUS_BASIC_OPTIMAL ||
			nStatus[0] == LS_STATUS_LOCAL_OPTIMAL ||
			nStatus[0] == LS_STATUS_FEASIBLE)
		{
			/* E[objective value] */
			nErrorCode[0] = LSgetStocInfo(pModel,LS_DINFO_STOC_EVOBJ,0,dObj);
			APIErrorCheck(pEnv);

			/* E[value of perfect information] */
			nErrorCode[0] = LSgetStocInfo(pModel,LS_DINFO_STOC_EVPI,0,dEvpi);
			System.out.printf("Objective = %g\n" , dObj[0]);
			System.out.printf("EVPI      = %g\n" , dEvpi[0]);
			System.out.printf("Status    = %d\n" , nStatus[0]);

			System.out.printf("\nDisplaying results for %d scenarios\n",numScens[0]);
			for (j=0; j<numScens[0]; j++)
			{
				if (1==1) nErrorCode[0] = LSwriteScenarioSolutionFile(pModel,j,null);
				if (1==0) nErrorCode[0] = LSwriteScenarioMPIFile(pModel,j,null);
				if (numStocPars[0]>0)
				{
					nErrorCode[0] = LSgetStocParOutcomes(pModel,j,padOutcome,dProb);
					APIErrorCheck(pEnv);

					System.out.printf("scenario %d has probability %g\n",j,dProb[0]);
					for (i=0; i<numStocPars[0]; i++)
					{
						System.out.printf("\t outcome[%d] = %10g\n",i,padOutcome[i]);
					}
				}
			}
		}
		else
		{
			System.out.printf ("\n Optimization failed. nStatus = %d ",nStatus[0]);
			APIErrorCheck(pEnv);
		}

		System.out.println("\nDone!");

		System.out.printf("Total callbacks = %d\n" , mydata.numCback);

		nErrorCode[0] = ls.LSdeleteModel( pModel);

		nErrorCode[0] = ls.LSdeleteEnv( pEnv);


	}/*main*/
}
