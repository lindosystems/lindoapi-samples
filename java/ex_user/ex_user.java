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

  @file   : ex_user.java

  @purpose: Solve an NLP that uses two black-box functions within
  the instruction-list interface.

            minimize f(x) * x
                G(x) <= 100
             0 <= x  <= 10

  The black-box functions are

    f(x)   the expression sin(pi*x)+cos(pi*x)
    G(x)   the integral[g(x),a,b)], where a,b constants specifying
           the limits of the integral.

  @remark : This application uses the Instruction Style Interface,
  where the instructions are imported from ex_user.mpi file.

  @remark : EP_USER operator is used in the instruction list to
  identify each black-box function and specify the number of
  arguments they take. For each function, the first argument
  is reserved to identify the function, whereas the rest are the
  actual arguments for the associated function.

  @remark : LSsetUsercalc() is used to set the user-defined
  MyUserFunc() function  as the gateway to the black-box functions.

*/


import com.lindo.*;

class ex_userdata
{
	int counter;
}



public class ex_user extends Lindo
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

	/* A callback function that will be called by the LINDO solver */
	private static int jCallback(Object pMod, int nLoc, Object pls)
	{
		int ncalls = 0;
		ex_user nls=null;
		ex_userdata _mydata;
		try
		{
			nls = (ex_user) pls;
			_mydata = (ex_userdata) nls.mydata;

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


	/***************************************************************
	 *          Black-box function #1 -- f(x).
	 */
	private static double fox(double a, double b)
	{
	  return Math.sin(a) + Math.cos(b);
	}

	/***************************************************************
	 *         Function g(t) to integrate over [a,b] */
	private static double gox(double x, double t)
	{
		double g;
		g = Math.exp(x*Math.cos(t));
		return(g);
	}

	/***************************************************************
	 *          Black-box #2 -- G(x)
	 *          Calculated by Simpson's Rule.
	 */
	private static double Gox(int n          /* Maximum number of steps (even) n */,
			   double x)
	{
		int c,k=1;            /* Counters in the algorithm        */
		double a=0;           /* Lower limit x=0                  */
		double b=8*Math.atan(1);   /* Upper limit x=2*pi               */
		double h,dsum;

		dsum=gox(x,a);        /* Initial function value */
		c=2;
		h=(b-a)/n;            /* Step size h=(b-a)/n */
		while (k <= n-1)      /* Steps through the iteration */
		{
			c=6-c;            /* gives the 4,2,4,2,... */
			dsum = dsum +
			  c*gox(x,a+k*h); /* Adds on the next area */
			k++;              /* Increases k value by +1 */
		}
		return ((dsum + gox(x,b))*h/3);
	}

    /*
     *  Callback function to evaluate functional values
     */
    private static int MyUserFunc (Object pModel,
                                 int      nRow  ,
                                 double  argval[],
                                 Object pUserData,
                                 double   pdFuncVal[])
    {
		int ncalls = 0;
		ex_user nls=null;
		ex_userdata _mydata;
		try
		{
			nls = (ex_user) pUserData;
			_mydata = (ex_userdata) nls.mydata;

			_mydata.counter++;
			ncalls = _mydata.counter;
		} catch (Exception e)
		{
			System.out.println(e.toString());
		}

		double f=0;
		if (argval[0]==1.) /* argval[0] is the function ID. */
		{
		  double a = argval[1];
		  double b = argval[2];
		  f = fox(a,b);
		}
		else if (argval[0]==2.)
		{
		  f = Gox((int)argval[1],argval[2]);
		}

		pdFuncVal[0] = f;

		return (0);


    } /*MyUserFunc*/


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
		int verbose = 1;

		// construct a new instance of the class
		ex_user ls = new ex_user();


		/* >>> Step 1 <<< Read license file and create a LINDO environment. */
		nErrorCode[0] = ls.LSloadLicenseString("../../license/lndapi130.lic",cLicenseKey);
		APIErrorCheck(pEnv);

		APIVERSION();
		pEnv = LScreateEnv(nErrorCode, cLicenseKey.toString());
		APIErrorCheck(pEnv);

		pModel = ls.LScreateModel(pEnv,nErrorCode);
		APIErrorCheck(pEnv);


    /*
    * >>>> Step 3 <<< Set up the instruction list of the model.
    */

        int nLinearz, nAutoDeriv, nConvexRelax, nCRAlgReform;


        /* Set linearization level, before a call to LSloadNLPCode.
        * If not specified, the solver will decide */
        nLinearz = 1;
        nErrorCode[0] = ls.LSsetModelIntParameter (pModel,
            LS_IPARAM_NLP_LINEARZ, nLinearz);
        APIErrorCheck(pEnv);

        /* Select algebraic reformulation level in convex relaxation*/
        nCRAlgReform = 1;
        nErrorCode[0] = ls.LSsetModelIntParameter (pModel,
            LS_IPARAM_NLP_CR_ALG_REFORM, nCRAlgReform);
        APIErrorCheck(pEnv);

        /* Select convex relax level */
        nConvexRelax = 0;
        nErrorCode[0] = ls.LSsetModelIntParameter (pModel,
            LS_IPARAM_NLP_CONVEXRELAX, nConvexRelax);
        APIErrorCheck(pEnv);

        /*
        * Set up automatic differentiation, before a call to LSreadMPIFile.
        * If not specified, the numerical derivative will be applied
        */
        nAutoDeriv = 0;
        nErrorCode[0] = ls.LSsetModelIntParameter (pModel,
            LS_IPARAM_NLP_AUTODERIV, nAutoDeriv);
        APIErrorCheck(pEnv);

        /* Set up MyUserFunc() as the user functionas */
        nErrorCode[0] = ls.LSsetUsercalc (pModel,"MyUserFunc", ls);
        APIErrorCheck(pEnv);

        nErrorCode[0] = ls.LSsetCallback(pModel,"jCallback",ls);
        APIErrorCheck(pEnv);


        /* Read instructions from an MPI-file */
        //nErrorCode[0] = loadModel(pModel);
        nErrorCode[0] = ls.LSreadMPIFile (pModel,"ex_user/ex_user.mpi");
        APIErrorCheck(pEnv);


		/*
		* >>> Step 5 <<< Perform the optimization using the
		*                multi-start solver
		*/

		/* set multi-start as the current NLP solver */
		nErrorCode[0] = ls.LSsetModelIntParameter (pModel,
			LS_IPARAM_NLP_SOLVER, LS_NMETHOD_MSW_GRG);
		APIErrorCheck(pEnv);

		nErrorCode[0] = ls.LSoptimize(pModel, LS_METHOD_FREE, nStatus);
		APIErrorCheck(pEnv);


		/*
		* >>> Step 6 <<< Retrieve the solution
		*/
        int nN[] = new int[1];
        ls.LSgetInfo(pModel, LS_IINFO_NUM_VARS,nN);

        /* >>> Step 5 <<< Retrieve the solution */
        double adX[] = new double[nN[0]], dObj[] = new double[1];

        /* Get the value of the objective */
        nErrorCode[0] = ls.LSgetInfo(pModel, LS_DINFO_POBJ, dObj) ;
        APIErrorCheck(pEnv);


        System.out.print("Objective Value = " + dObj[0] + "\n");

        /* Get the variable values */
        nErrorCode[0] = ls.LSgetPrimalSolution (pModel, adX);
        APIErrorCheck(pEnv);

        System.out.println("Primal values");
        for (i = 0; i < nN[0]; i++) System.out.println("\tx["+i+"] = "+adX[i]);
        System.out.print("\n");

		nErrorCode[0] = ls.LSdeleteModel( pModel);

		nErrorCode[0] = ls.LSdeleteEnv( pEnv);


	}/*main*/

	private static int loadModel(Object pModel)
	{ // begin core instructions
	  int      nVars          = 1;
	  int      nCons          = 1;
	  int      nNumbers       = 4;
	  int      paiVars[]      = null;
	  double   padVarVal[]    = null;
	  double   padNumVal[]    =
	  {
		1,  2,  20,  100,   -1
	  };
	  int      nObjs          = 1;
	  int      panObjSense[]  =
	  {
		LS_MIN,   -1
	  };
	  int      panObjLen[]    =
	  {
		15,   -1
	  };
	  int      paiObjBeg[]    =
	  {
		0,  15,   -1
	  };
	  int      paiConBeg[]    =
	  {
		15,  26,   -1
	  };
	  int      panConLen[]    =
	  {
		11,   -1
	  };
	  int      nInstruct      = 26;
	  int      panInstruct[]  =
	  {
		  1063,       0,    1062,       0,    1063,       0,    1022,    1003,    1063,       0,    1022,
		  1003,    1054,       3,    1003,
		  1062,       1,    1062,       2,    1063,       0,    1054,       3,    1062,       3,    1002,
		 -1
	  };
	  String   pszVarType  = "C";
	  String   pszConType  = "L";
	  double   padLB[]   =
	  {
		0,   -1
	  };
	  double   padUB[]   =
	  {
		10,   -1
	  };


	  /* Load core instructions... */
	  return Lindo.LSloadInstruct(pModel,nCons,nObjs,nVars,nNumbers,panObjSense,pszConType,
		pszVarType,panInstruct,nInstruct,paiVars,padNumVal,padVarVal,paiObjBeg,panObjLen,
		paiConBeg,panConLen,padLB,padUB);

	} // end core instructions


}

