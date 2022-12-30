/*
###################################################################
#                       LINDO-API
#                    Sample Programs
#                  Copyright (c) 2001-2011
#
#         LINDO Systems, Inc.           312.988.7422
#         1415 North Dayton St.         info@lindo.com
#         Chicago, IL 60622             http://www.lindo.com
###################################################################

  File   : ex_gop.java

  Purpose: Read a model from an MPI file and optimize with global solver.
           Local variable 'verbose' toggles the source of progress log.
           verbose = 0 (no log)
                   > 2 (jLogback produced log piping progress info from LINDO)
                   > 1 (jNextGOPCallback produced log)
                   > 0 (jCallback produced log)
*/

import com.lindo.*;

class ex_userdata
{
	int numCback;
	int numBestK;
}



public class ex_gop extends Lindo
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

	/* A standard callback function */
	private static int jCallback(Object pMod, int nLoc, Object pls)
	{
		int ncalls = 0;
		int nbestk = 0;
		ex_gop nls=null;
		ex_userdata _mydata;
		try
		{
			nls = (ex_gop) pls;
			_mydata = (ex_userdata) nls.mydata;

			_mydata.numCback++;
			_mydata.numBestK++;
			nbestk=_mydata.numBestK;
		} catch (Exception e)
		{
			System.out.println(e.toString());
		}

		LSgetCallbackInfo(pMod,0,LS_DINFO_GOP_BESTBOUND,bestbound);
		LSgetCallbackInfo(pMod,0,LS_DINFO_GOP_OBJ,pobj);
		LSgetCallbackInfo(pMod,0,LS_IINFO_GOP_SIM_ITER,iter);
		System.out.printf("\n@next callback solution=%3d, iter=%8d, pobj = %+.6e,  bestbnd = %.6e (*)",
			nbestk,iter[0],pobj[0],bestbound[0]);
		return 0;
	}


	/* A callback function to be called at next incumbent solution (for global solver) */
	private static int jNextGOPCallback(Object pMod, Object pls, double obj, double x[])
	{
		int ncalls = 0;
		int nbestk = 0;
		ex_gop nls=null;
		ex_userdata _mydata;
		try
		{
			nls = (ex_gop) pls;
			_mydata = (ex_userdata) nls.mydata;

			_mydata.numCback++;
			_mydata.numBestK++;
			nbestk=_mydata.numBestK;
		} catch (Exception e)
		{
			System.out.println(e.toString());
		}

		LSgetCallbackInfo(pMod,0,LS_DINFO_GOP_BESTBOUND,bestbound);
		LSgetCallbackInfo(pMod,0,LS_DINFO_GOP_OBJ,pobj);
		LSgetCallbackInfo(pMod,0,LS_IINFO_GOP_SIM_ITER,iter);
		System.out.printf("\n@next best global solution=%3d, iter=%8d, pobj = %+.6e,  bestbnd = %.6e (*)",
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

		int i,neq,nle,nge;
		int m[] = new int[1];
		int n[] = new int[1];
		int nbin[] = new int[1];
		int ngin[] = new int[1];
		int ncont[] = new int[1];
		int nStatus[] = new int[1];
		StringBuffer csense = new StringBuffer();
		int verbose = 3;

		// construct a new instance of the class
		ex_gop ls = new ex_gop();

		// Read license file and create a LINDO environment.
		nErrorCode[0] = ls.LSloadLicenseString("../../license/lndapi140.lic",cLicenseKey);
		APIErrorCheck(pEnv);

		APIVERSION();
		pEnv = LScreateEnv(nErrorCode, cLicenseKey.toString());
		APIErrorCheck(pEnv);

		pModel = ls.LScreateModel(pEnv,nErrorCode);
		APIErrorCheck(pEnv);

		if (args.length == 0)
		{
			System.out.println("\n\nusage: ex_gop [filename] [verbose=1 or 2 or 3]\n");
			nErrorCode[0] = ls.LSdeleteEnv( pEnv);
			return;
		} else if (args.length > 1)
		{
			verbose = Integer.parseInt(args[1]);
			System.out.printf("\nVerbosity: %s",args[1]);
	    }

		System.out.printf("\nReading %s as MPI file.",args[0]);
		nErrorCode[0] = ls.LSreadMPIFile( pModel, args[0]);
		APIErrorCheck(pEnv);

		nErrorCode[0] = ls.LSgetInfo(pModel, LS_IINFO_NUM_VARS,n);
		APIErrorCheck(pEnv);

		nErrorCode[0] = ls.LSgetInfo(pModel, LS_IINFO_NUM_CONS,m);
		APIErrorCheck(pEnv);


		nErrorCode[0] = ls.LSgetLPData(pModel, null,null,null,null,csense,
			null,null,null,null,null,null);
		APIErrorCheck(pEnv);

		neq=0;
		nle=0;
		nge=0;
		for (i=0;i<m[0];i++)
		{
			if (csense.charAt(i) == 'E')
				neq++;
			else if (csense.charAt(i) == 'L')
				nle++;
			else
				nge++;
		}

		nErrorCode[0] = ls.LSgetInfo(pModel,LS_IINFO_NUM_CONT,ncont);
		APIErrorCheck(pEnv);

		nErrorCode[0] = ls.LSgetInfo(pModel,LS_IINFO_NUM_BIN,nbin);
		APIErrorCheck(pEnv);

		nErrorCode[0] = ls.LSgetInfo(pModel,LS_IINFO_NUM_INT,ngin);
		APIErrorCheck(pEnv);

		mydata.numCback = 0; // total number of callbacks
		mydata.numBestK = 0; // number of best-k integer solutions (for integer models)

		System.out.println("\n\nModel statistics");
		System.out.println("\t constraints        = "+m[0]);
		System.out.println("\t     + equalities   = "+neq);
		System.out.println("\t     + inequalities = "+(nle+nge));
		System.out.println("\n");
		System.out.println("\t variables          = "+n[0]);
		System.out.println("\t     + binary int   = "+nbin[0]);
		System.out.println("\t     + general int  = "+ngin[0]);
		System.out.println("\t     + continuous   = "+ncont[0]);

		//nErrorCode[0] = ls.LSsetModelIntParameter(pModel,LS_IPARAM_LP_PRELEVEL,0);
		//nErrorCode[0] = ls.LSsetModelIntParameter(pModel,LS_IPARAM_LP_PRINTLEVEL,verbose);
		//nErrorCode[0] = LSsetModelIntParameter(pModel,LS_IPARAM_GOP_PRINTLEVEL,2);
		//APIErrorCheck(pEnv);

		if (verbose>2)
			nErrorCode[0] = ls.LSsetModelLogfunc(pModel,"jLogback",ls);
		else if (verbose>1)
			nErrorCode[0] = ls.LSsetGOPCallback(pModel,"jNextGOPCallback",ls);
		else if (verbose>0)
			nErrorCode[0] = ls.LSsetCallback(pModel,"jCallback",ls);

		APIErrorCheck(pEnv);
		nErrorCode[0] = ls.LSsolveGOP( pModel, nStatus);
		APIErrorCheck(pEnv);

		System.out.printf("\nDone!");
		System.out.printf("Status = %d\n" , nStatus[0]);
		System.out.printf("Total callbacks = %d\n" , mydata.numCback);

		double adx[] = new double[n[0]];
		double ady[] = new double[m[0]];
		double adDecObj[] = new double[n[0]];
		double adIncObj[] = new double[n[0]];
		double adDecRhs[] = new double[m[0]];
		double adIncRhs[] = new double[m[0]];
		double obj[] = new double[1];

		if ((nbin[0]+ngin[0])==0)
		{
			ls.LSgetInfo(pModel,LS_DINFO_POBJ,obj);
			ls.LSgetPrimalSolution(pModel,adx);
			ls.LSgetDualSolution(pModel,ady);
		}
		else
		{
			ls.LSgetInfo(pModel,LS_DINFO_MIP_OBJ,obj);
			ls.LSgetMIPPrimalSolution(pModel,adx);
		}

		nErrorCode[0] = ls.LSdeleteModel( pModel);

		nErrorCode[0] = ls.LSdeleteEnv( pEnv);


	}/*main*/
}
