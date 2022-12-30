/*
###################################################################
#                       LINDO-API
#                    Sample Programs
#                  Copyright (c) 2006
#
#         LINDO Systems, Inc.           312.988.7422
#         1415 North Dayton St.         info@lindo.com
#         Chicago, IL 60622             http://www.lindo.com
###################################################################

  File   : ex_addrows.c

  Purpose: Build a model row-wise.

  Steps:
  	1. Set up a source for generating rows. In this sample, it
  	   is a native Lindo Model object (pSourceModel).
  	2. Set up a target model (pTargetModel) which is initally
  	   an empty LP model loaded with LSloadLPData. The arguments
  	   passed in the initial call are required when constructing
  	   an empty LP.
  	3. Retrieve new rows (from pSourceModel) and add them to
  	   pTargetModel. In this sample, getNextRowSet() function
  	   mimics generation of rows by user application. Note,
  	   constraints can be added one at a time, or in bundles.
  	   Here, we add one row at a time. Adding in bundles can use
  	   the same data structures, so it is a trivial extension of
  	   the current implementation.
*/

import com.lindo.*;

class ex_userdata
{
	int counter;
}



public class ex_addrows extends Lindo
{
	private static int nErrorCode[] = new int[1];
	private static StringBuffer cErrorMessage = new StringBuffer(LS_MAX_ERROR_MESSAGE_LENGTH);
	private static StringBuffer cLicenseKey = new StringBuffer(LS_MAX_ERROR_MESSAGE_LENGTH);

	private static int iter[] = new int[1];
	private static int n_vars[] = new int[1];
	private static double pobj[] = new double[1];
	private static double bestbound[] = new double[1];
	private static double pinf[] = new double[1];
	private static Object pEnv = null;
	private static Object pTargetModel = null;
	private static Object pSourceModel = null;

	private static ex_userdata mydata = new ex_userdata();


	private static void jLogback(Object pMod, String szMessage, Object pls)
	{
		System.out.print(szMessage);
	}

	/* A callback function that will be called by the LINDO solver */
	private static int jCallback(Object pMod, int nLoc, Object pls)
	{
		int ncalls = 0;
		try
		{
			ex_addrows nls = (ex_addrows) pls;
			ex_userdata _mydata = (ex_userdata) nls.mydata;

			_mydata.counter++;
			ncalls = _mydata.counter;
		} catch (Exception e)
		{
			System.out.println(e.toString());
		}


		LSgetCallbackInfo(pMod,0,LS_DINFO_PINFEAS,pinf);
		LSgetCallbackInfo(pMod,0,LS_DINFO_POBJ,pobj);
		LSgetCallbackInfo(pMod,0,LS_IINFO_SIM_ITER,iter);
		System.out.println("@callback " + ncalls + "; " +
			"iter = " + iter[0] + "; " +
			"obj = " + pobj[0] + "; " +
			"pinf = " + pinf[0]);
		return 0;
	}

	private static int jMIPCallback(Object pMod, int nLoc, Object pls)
	{
		ex_addrows nls = (ex_addrows) pls;
		ex_userdata _mydata = (ex_userdata) nls.mydata;

		// return if not calling from MIP optimizer
		if (nLoc != LSLOC_MIP) return 0;

		_mydata.counter++;

		LSgetCallbackInfo(pMod,0,LS_DINFO_MIP_BESTBOUND,bestbound);
		LSgetCallbackInfo(pMod,0,LS_DINFO_MIP_OBJ,pobj);
		LSgetCallbackInfo(pMod,0,LS_IINFO_MIP_SIM_ITER,iter);
		System.out.println("@callback " + _mydata.counter + "; " +
			"iter = " + iter[0] + "; " +
			"obj = " + pobj[0] + "; " +
			"lower bound = " + bestbound[0]);
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

	// A template to return a dummy cost vector
	private static double[] getCost(Object pSourceModel, int nLen)
	{
		int i;
		double [] dtmp = new double[nLen];
		nErrorCode[0] = LSgetLPData(pSourceModel, null,null,dtmp,null,null,
			null,null,null,null,null,null);
		return dtmp;
	}

	// A template to return a dummy cost vector
	private static double[] getLBound(Object pSourceModel, int nLen)
	{
		int i;
		double [] dtmp = new double[nLen];
		nErrorCode[0] = LSgetLPData(pSourceModel, null,null,null,null,null,
			null,null,null,null,dtmp,null);
		return dtmp;
	}

	// A template to return a dummy cost vector
	private static double[] getUBound(Object pSourceModel, int nLen)
	{
		int i;
		double [] dtmp = new double[nLen];
		nErrorCode[0] = LSgetLPData(pSourceModel, null,null,null,null,null,
			null,null,null,null,null,dtmp);
		return dtmp;
	}

	// A template to return column offsets for an empty matrix
	private static int[] getColStart(Object pSourceModel, int nLen)
	{
		int i;
		int [] itmp = new int[nLen];
		for (i=0; i<nLen; i++) itmp[i] = 0;
		return itmp;
	}

	// A dummy source to get model rows
	private static Object getSourceModel(Object pEnv, String modelFile)
	{
		pSourceModel = LScreateModel(pEnv,nErrorCode);
		APIErrorCheck(pEnv);

		System.out.println("\n\nReading "+modelFile);
		nErrorCode[0] = LSreadMPSFile( pSourceModel, modelFile, 0);
		if (nErrorCode[0] != LSERR_NO_ERROR)
		{
			nErrorCode[0] = LSreadLINDOFile( pSourceModel, modelFile);
			APIErrorCheck(pEnv);
		}
		return pSourceModel;
	}

	// A template to return number of columns in the model
	private static int numRows(Object pSourceModel)
	{
		int itmp[] = new int[1];
		nErrorCode[0] = LSgetInfo(pSourceModel, LS_IINFO_NUM_CONS,itmp);
		APIErrorCheck(pEnv);

		return itmp[0];
	}

	// A template to return number of columns in the model
	private static int numColumns(Object pSourceModel)
	{
		int itmp[] = new int[1];
		nErrorCode[0] = LSgetInfo(pSourceModel, LS_IINFO_NUM_VARS,itmp);
		APIErrorCheck(pEnv);

		return itmp[0];
	}

	// A template to return number of columns in the model
	private static int numNonz(Object pSourceModel)
	{
		int itmp[] = new int[1];
		nErrorCode[0] = LSgetInfo(pSourceModel, LS_IINFO_NUM_NONZ,itmp);
		APIErrorCheck(pEnv);

		return itmp[0];
	}

	// A template to create the next row
	private static int getNextRowSet(Object pSourceModel,int IdxCons, StringBuffer acNewConSense,
		StringBuffer szNewConName,int[] aiNewColIdx,int[] aiNewRowBeg,
		double[] adNewCoef,double[] adNewRhs)
	{
		int itmp[] = new int[1];

		nErrorCode[0] = LSgetLPConstraintDatai(pSourceModel,IdxCons,acNewConSense,
			adNewRhs, itmp, aiNewColIdx,adNewCoef);

		aiNewRowBeg[0] = 0;
		aiNewRowBeg[1] = itmp[0]; // nonzeros in the row


		if (false)
			nErrorCode[0] = LSgetConstraintNamei(pSourceModel, IdxCons ,szNewConName);

		return nErrorCode[0];
	}

	// Transpose (ka,ia,a) into (kat,iat,at)
	private static void transA( int m, int n,
	                            int[]ka, int[]ia, double []a,
	                            int[]kat,int[]iat, double []at)
	{
		int errorcode;
		int i,j,k,row,addr;
		int []iwork = new int[m];

		for (k=0; k<ka[n]; k++) {
			row = ia[k];
			iwork[row]++;
		}
		kat[0] = 0;
		for (i=0; i<m; i++) {
			kat[i+1] = kat[i] + iwork[i];
			iwork[i] = 0;
		}
		for (j=0; j<n; j++) {
			for (k=ka[j]; k<ka[j+1]; k++) {
				row = ia[k];
				addr = kat[row] +iwork[row];
				iwork[row]++;
				iat[addr] = j;
				at[addr]  = a[k];
			}
		}
	}

	// A template to display model stats
	private static void printModelStats(Object pModel)
	{
		int ncont[] = new int[1];
		int nbin[] = new int[1];
		int ngin[] = new int[1];
		int nge[] = new int[1];
		int nle[] = new int[1];
		int neq[] = new int[1];

		nErrorCode[0] = LSgetInfo(pModel,Lindo.LS_IINFO_NUM_CONT,ncont);
		APIErrorCheck(pEnv);

		nErrorCode[0] = LSgetInfo(pModel,Lindo.LS_IINFO_NUM_BIN,nbin);
		APIErrorCheck(pEnv);

		nErrorCode[0] = LSgetInfo(pModel,Lindo.LS_IINFO_NUM_INT,ngin);
		APIErrorCheck(pEnv);

		nErrorCode[0] = LSgetInfo(pModel,Lindo.LS_IINFO_NUM_CONS_L,nle);
		APIErrorCheck(pEnv);

		nErrorCode[0] = LSgetInfo(pModel,Lindo.LS_IINFO_NUM_CONS_E,neq);
		APIErrorCheck(pEnv);

		nErrorCode[0] = LSgetInfo(pModel,Lindo.LS_IINFO_NUM_CONS_G,nge);
		APIErrorCheck(pEnv);

		System.out.println("\n\nModel statistics");
		System.out.println("\t constraints        = "+numRows(pModel));
		System.out.println("\t     + equalities   = "+neq[0]);
		System.out.println("\t     + inequalities = "+(nle[0]+nge[0]));
		System.out.println("\n");
		System.out.println("\t variables          = "+numColumns(pModel));
		System.out.println("\t     + binary int   = "+nbin[0]);
		System.out.println("\t     + general int  = "+ngin[0]);
		System.out.println("\t     + continuous   = "+ncont[0]);
	}

	// Load rows one at a time
	private static void loadRowsOneByOne(Object pSourceModel, Object pTargetModel)
	{
		int i;
		long start;
		long elapsed;
		int nRows = numRows(pSourceModel);
		int nCols = numColumns(pSourceModel);
		int nNonz = 0;

		// Set up 'pTargetModel' row-wise (by adding rows) from 'pSourceModel'.
		int nNewConIdx;
		StringBuffer acNewConSense = new StringBuffer();
		StringBuffer szNewConName = null;
		int aiNewColIdx[] = new int [nCols];
		int aiNewRowBeg[] = new int [2];
		double adNewAcoef[] = new double [nCols];
		double adNewRhs[] = new double [1];
		String[] szTempName = null;

		elapsed = 0;
		for (i=0; i<nRows; i++)
		{
			nNewConIdx = i;
			// get the data representing a row
			nErrorCode[0] = getNextRowSet(pSourceModel,nNewConIdx,
				acNewConSense,szNewConName,aiNewColIdx,aiNewRowBeg,adNewAcoef,adNewRhs);
			APIErrorCheck(pEnv);

			start = System.currentTimeMillis();
			// add the data representing the row
			nErrorCode[0] = LSaddConstraints(pTargetModel, 1,
				acNewConSense.toString(),szTempName,aiNewRowBeg,adNewAcoef,aiNewColIdx,adNewRhs);
			APIErrorCheck(pEnv);
			elapsed += System.currentTimeMillis()-start;

			// clear string buffers
			acNewConSense.setLength(0);
			if (false) szNewConName.setLength(0);
		}
		System.out.println("(" + elapsed/1000F + " secs.)");


	}

	// Load all rows in one call. Here, the LP matrix is obtained from pSourceModel
	// column-wise and transposed into row-wise format so that it could be loaded
	// via LSaddConstraints.
	private static void loadRowsAll(Object pSourceModel, Object pTargetModel)
	{
		int i;
		long start;
		long elapsed;
		int nRows = numRows(pSourceModel);
		int nCols = numColumns(pSourceModel);
		int nNonz = numNonz(pSourceModel);;

		// Set up 'pTargetModel' row-wise (by adding all rows) from 'pSourceModel'.
		StringBuffer acConSense = new StringBuffer();
		String[] szTempName = null;

		int aiColIdx[] = new int [nNonz];
		int aiRowBeg[] = new int [nRows+1];

		int aiRowIdx[] = new int [nNonz];
		int aiColBeg[] = new int [nCols+1];
		int acColCnt[] = null;

		double adA[] = new double [nNonz];
		double adAt[] = new double [nNonz];
		double adRhs[] = new double [nRows];
		double adL[] = null;
		double adU[] = null;

		elapsed = 0;
		start = System.currentTimeMillis();

		nErrorCode[0] = LSgetLPData(pSourceModel,null,null,null,
			adRhs,acConSense,aiColBeg,acColCnt,adA,aiRowIdx,adL,adU);
		APIErrorCheck(pEnv);

		transA(nRows,nCols,aiColBeg,aiRowIdx,adA,aiRowBeg,aiColIdx,adAt);

		// add the data representing the row
		nErrorCode[0] = LSaddConstraints(pTargetModel, nRows,
				acConSense.toString(),szTempName,aiRowBeg,adAt,aiColIdx,adRhs);
		APIErrorCheck(pEnv);
		elapsed += System.currentTimeMillis()-start;
		System.out.println("(" + elapsed/1000F + " secs.)");


	}

	// The main program
	public static void main(String[] args)
	{
		int i;
		int modelType[] = new int[1];
		int nStatus[] = new int[1];
		StringBuffer csense = new StringBuffer();
		int verbose = 2;

		boolean isLoadOneByOne = false;

		// construct a new instance of the class
		ex_addrows ls = new ex_addrows();

		if (args.length == 0)
		{
			System.out.println("\n\nUsage: ex_addrows [filename]\n");
			nErrorCode[0] = ls.LSdeleteEnv( pEnv);
			return;
		}

		/* >>> Step 1 <<< Read license file and create a LINDO environment. */
		nErrorCode[0] = ls.LSloadLicenseString("../../license/lndapi140.lic",cLicenseKey);
		APIErrorCheck(pEnv);

		APIVERSION();
		pEnv = ls.LScreateEnv(nErrorCode, cLicenseKey.toString());
		APIErrorCheck(pEnv);

		pTargetModel = ls.LScreateModel(pEnv,nErrorCode);
		APIErrorCheck(pEnv);

		System.out.println("\n\nBuilding a new model row-wise");

		// Get handle for a source
		pSourceModel = getSourceModel(pEnv, args[0]);
		printModelStats(pSourceModel);


		// Set up an empty LP matrix for 'pTargetModel'
		int nRows = numRows(pSourceModel);
		int nCols = numColumns(pSourceModel);
		int nNonz = 0;
		int objSense = Lindo.LS_MIN;
		double objConst = 0.0;
		double[] adCost = getCost(pSourceModel,nCols);
		double[] adRHS = null;
		int[] aiColBeg = getColStart(pSourceModel,nCols+1);
		String conSense = null;
		double[] adA = null;
		int[] aiRowIdx=null;
		int[] anColLen=null;
		double [] pdLower=getLBound(pSourceModel,nCols);
		double [] pdUpper=getUBound(pSourceModel,nCols);

		// Load an empty LP matrix (0 rows, nCols columns, 0 Nonzeros)
		// Note adCost, adL and adU vectors (common length is nCols) are required.
		nErrorCode[0] = ls.LSloadLPData( pTargetModel, 0, nCols, objSense,
			objConst, adCost, adRHS, conSense, nNonz, aiColBeg,
			anColLen, adA, aiRowIdx, pdLower, pdUpper);
		APIErrorCheck(pEnv);

		if (isLoadOneByOne)
		{
			System.out.print("\nLoading the model "+args[0]+" by adding one row at a time...");
			loadRowsOneByOne(pSourceModel,pTargetModel);
		}
		else
		{
			System.out.print("\nLoading the model "+args[0]+" by adding all rows in one shot...");
			loadRowsAll(pSourceModel,pTargetModel);
		}

		// Note:: Load integer restrictions, if any.

		printModelStats(pTargetModel);

		nErrorCode[0] = ls.LSgetInfo(pTargetModel,LS_IINFO_MODEL_TYPE, modelType);
		APIErrorCheck(pEnv);


		if (modelType[0]==Lindo.LS_LP ||    // Linear model
			modelType[0]==Lindo.LS_QP ||    // Quadratic model
			modelType[0]==Lindo.LS_SOCP ||  // Conic model
			modelType[0]==Lindo.LS_NLP )  // Nonlinear model
			{
			nErrorCode[0] = ls.LSsetModelIntParameter(pTargetModel,LS_IPARAM_LP_PRINTLEVEL,verbose);
			APIErrorCheck(pEnv);

			System.out.println("\nOptimizing continuous model\n");
			if (verbose>1)
				nErrorCode[0] = ls.LSsetModelLogfunc(pTargetModel,"jLogback",ls);
			else if (verbose>0)
				nErrorCode[0] = ls.LSsetCallback(pTargetModel,"jCallback",ls);
			APIErrorCheck(pEnv);
			nErrorCode[0] = ls.LSoptimize( pTargetModel, 0, nStatus);
			APIErrorCheck(pEnv);

		}
		else
		{
			nErrorCode[0] = ls.LSsetModelIntParameter(pTargetModel,LS_IPARAM_LP_PRINTLEVEL,0);
			APIErrorCheck(pEnv);

			nErrorCode[0] = ls.LSsetModelIntParameter(pTargetModel,LS_IPARAM_MIP_PRINTLEVEL,verbose);
			APIErrorCheck(pEnv);

			System.out.println("\nOptimizing integer model\n");
			if (verbose>1)
				nErrorCode[0] = ls.LSsetModelLogfunc(pTargetModel,"jLogback",ls);
			else if (verbose>0)
				nErrorCode[0] = ls.LSsetCallback(pTargetModel,"jMIPCallback",ls);
			APIErrorCheck(pEnv);
			nErrorCode[0] = ls.LSsolveMIP( pTargetModel, nStatus);
			APIErrorCheck(pEnv);
		}
		System.out.println("\nDone!");

		System.out.println("Status = " + nStatus[0]);
		System.out.println("Total callbacks = " + mydata.counter);

		double adx[] = new double[nCols];
		double ady[] = new double[nRows];
		double adDecObj[] = new double[nCols];
		double adIncObj[] = new double[nCols];
		double adDecRhs[] = new double[nRows];
		double adIncRhs[] = new double[nRows];
		double obj[] = new double[1];


		if (modelType[0]==Lindo.LS_LP ||    // Linear model
			modelType[0]==Lindo.LS_QP ||    // Quadratic model
			modelType[0]==Lindo.LS_SOCP ||  // Conic model
			modelType[0]==Lindo.LS_NLP )  // Nonlinear model
			{
			ls.LSgetInfo(pTargetModel,LS_DINFO_POBJ,obj);
			ls.LSgetPrimalSolution(pTargetModel,adx);
			ls.LSgetDualSolution(pTargetModel,ady);
			if (false) {
				ls.LSgetConstraintRanges(pTargetModel,adDecRhs,adIncRhs);
				ls.LSgetObjectiveRanges(pTargetModel,adDecObj,adIncObj);
				System.out.println( "\n\nPrimal Solution (with ranges)\n");
				for (i = 0; i < nCols; i++)
					System.out.println( adx[i]+"\t"+adDecObj[i]+"\t"+adIncObj[i]);

				System.out.println( "\n\nDual Solution (with ranges)\n");
				for (i = 0; i < nRows; i++)
					System.out.println( ady[i]+"\t"+adDecRhs[i]+"\t"+adIncRhs[i]);
			}

		}
		else
		{
			ls.LSgetInfo(pTargetModel,LS_DINFO_MIP_OBJ,obj);
			ls.LSgetMIPPrimalSolution(pTargetModel,adx);
		}

		nErrorCode[0] = ls.LSdeleteEnv( pEnv);


	}/*main*/
}
