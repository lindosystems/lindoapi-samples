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

  File   : ex_iostream.c

  Purpose: Input a model from a char stream and optimize.
*/

import com.lindo.*;
import java.io.*;

class ex_userdata
{
	int counter;
}



public class ex_iostream extends Lindo
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
	private static Object pModel = null;

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
			ex_iostream nls = (ex_iostream) pls;
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
		ex_iostream nls = (ex_iostream) pls;
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

	public static void LindoFile2Buffer(String modelFile, StringBuffer modelBuffer)
	{
		try
		{
			/*	Sets up a file reader to read the file passed on the command
			line one character at a time */
			FileReader input = new FileReader(modelFile);

			/* Filter FileReader through a Buffered read to read a line at a
			   time */
			BufferedReader bufRead = new BufferedReader(input);

			String line; 	// String that holds current file line
			int count = 0;	// Line number of count

			// Read first line
			line = bufRead.readLine();
			count++;

			// Read through file one line at time. Print line # and line
			while (line != null){
				modelBuffer.append(line+"\n");
				line = bufRead.readLine();
				count++;
			}

			bufRead.close();

		}catch (ArrayIndexOutOfBoundsException e){
			/* If no file was passed on the command line, this expception is
			generated. A message indicating how to the class should be
			called is displayed */
			System.out.println("\n\nUsage: ex_iostream [filename]\n");

		}catch (IOException e){
			// If another exception is generated, print a stack trace
			e.printStackTrace();
		}

	}// end File2Buffer

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
		StringBuffer modelBuffer = new StringBuffer();
		int verbose = 2;
	    long start;
	    long elapsed;



		// construct a new instance of the class
		ex_iostream ls = new ex_iostream();

		/* >>> Step 1 <<< Read license file and create a LINDO environment. */
		nErrorCode[0] = ls.LSloadLicenseString("../../license/lndapi150.lic",cLicenseKey);
		APIErrorCheck(pEnv);

		APIVERSION();
		pEnv = ls.LScreateEnv(nErrorCode, cLicenseKey.toString());
		APIErrorCheck(pEnv);

		pModel = ls.LScreateModel(pEnv,nErrorCode);
		APIErrorCheck(pEnv);

		if (args.length == 0)
		{
			System.out.println("\n\nUsage: ex_iostream [filename]\n");
			nErrorCode[0] = ls.LSdeleteEnv( pEnv);
			return;
		}

		start = System.currentTimeMillis();
		// read model into a string buffer
		System.out.print("\nReading the model "+args[0]+" into a string buffer...");
		LindoFile2Buffer(args[0],modelBuffer);
		elapsed = System.currentTimeMillis()-start;
		System.out.println("("+elapsed/1000F+" secs.)");

		start = System.currentTimeMillis();
	    System.out.print("\nLoading the string buffer to the solver...");
		nErrorCode[0] = ls.LSreadLINDOStream( pModel, modelBuffer.toString(), modelBuffer.length());
		elapsed = System.currentTimeMillis()-start;
		System.out.println("("+elapsed/1000F+" secs.)");
		APIErrorCheck(pEnv);


		// clear the buffer
		modelBuffer.setLength(0);

		//nErrorCode[0] = ls.LSgetDimensions(pModel, n, m, null, null, null, null, null, null);
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

		mydata.counter = 0;

		System.out.println("\n\nModel statistics");
		System.out.println("\t constraints        = "+m[0]);
		System.out.println("\t     + equalities   = "+neq);
		System.out.println("\t     + inequalities = "+(nle+nge));
		System.out.println("\n");
		System.out.println("\t variables          = "+n[0]);
		System.out.println("\t     + binary int   = "+nbin[0]);
		System.out.println("\t     + general int  = "+ngin[0]);
		System.out.println("\t     + continuous   = "+ncont[0]);

		if ((nbin[0]+ngin[0])==0)
		{
			nErrorCode[0] = ls.LSsetModelIntParameter(pModel,LS_IPARAM_LP_PRINTLEVEL,verbose);
			APIErrorCheck(pEnv);

			System.out.println("\nOptimizing continuous model\n");
			if (verbose>1)
				nErrorCode[0] = ls.LSsetModelLogfunc(pModel,"jLogback",ls);
			else if (verbose>0)
				nErrorCode[0] = ls.LSsetCallback(pModel,"jCallback",ls);
			APIErrorCheck(pEnv);
			nErrorCode[0] = ls.LSoptimize( pModel, 0, nStatus);
			APIErrorCheck(pEnv);

		}
		else
		{
			nErrorCode[0] = ls.LSsetModelIntParameter(pModel,LS_IPARAM_LP_PRINTLEVEL,0);
			nErrorCode[0] = ls.LSsetModelIntParameter(pModel,LS_IPARAM_MIP_PRINTLEVEL,verbose);
			APIErrorCheck(pEnv);

			System.out.println("\nOptimizing integer model\n");
			if (verbose>1)
				nErrorCode[0] = ls.LSsetModelLogfunc(pModel,"jLogback",ls);
			else if (verbose>0)
				nErrorCode[0] = ls.LSsetCallback(pModel,"jMIPCallback",ls);
			APIErrorCheck(pEnv);
			nErrorCode[0] = ls.LSsolveMIP( pModel, nStatus);
			APIErrorCheck(pEnv);
		}
		System.out.println("\nDone!");

		System.out.println("Status = " + nStatus[0]);
		System.out.println("Total callbacks = " + mydata.counter);

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
			if (false) {
				ls.LSgetConstraintRanges(pModel,adDecRhs,adIncRhs);
				ls.LSgetObjectiveRanges(pModel,adDecObj,adIncObj);
				System.out.println( "\n\nPrimal Solution (with ranges)\n");
				for (i = 0; i < n[0]; i++)
					System.out.println( adx[i]+"\t"+adDecObj[i]+"\t"+adIncObj[i]);

				System.out.println( "\n\nDual Solution (with ranges)\n");
				for (i = 0; i < m[0]; i++)
					System.out.println( ady[i]+"\t"+adDecRhs[i]+"\t"+adIncRhs[i]);
			}

		}
		else
		{
			ls.LSgetInfo(pModel,LS_DINFO_MIP_OBJ,obj);
			ls.LSgetMIPPrimalSolution(pModel,adx);
		}

		nErrorCode[0] = ls.LSdeleteEnv( pEnv);


	}/*main*/
}
