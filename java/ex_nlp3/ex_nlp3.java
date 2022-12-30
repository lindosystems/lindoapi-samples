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

  File   : ex_nlp3.java

          (continous version of ex_nlp1.java)

  Purpose: Solve a NLP using the black-box style interface.

  Model  : A nonlinear model with multiple local minimizers.

        minimize  f(x,y) =  3*(1-x)^2*exp(-(x^2) - (y+1)^2)
                         - 10*(x/5 - x^3 - y^5).*exp(-x^2-y^2)
                         - 1/3*exp(-(x+1)^2 - y^2);
        subject to
                         x^2 + y   <=  6;
                         x   + y^2 <=  6;
*/

import com.lindo.*;

class ex_userdata
{
    int counter;
}

public class ex_nlp3 extends Lindo
{
    private static int nErrorCode[] = new int[1];
    private static StringBuffer cErrorMessage = new StringBuffer(LS_MAX_ERROR_MESSAGE_LENGTH);
    private static StringBuffer cLicenseKey = new StringBuffer(LS_MAX_ERROR_MESSAGE_LENGTH);

    private static ex_userdata kkt = new ex_userdata();
    private static ex_userdata fde = new ex_userdata();

    /* declare an instance of the LINDO environment object */
    private static Object pEnv = null;

    /* declare an instance of the LINDO model object */
    private static Object pModel = null;

    /*
     *  Standard callback function to display local solutions
     */
    private static int  local_sol_log(Object model, int iLoc, Object pls)
    {
      ex_nlp3 nls = (ex_nlp3) pls;
      ex_userdata _kkt = (ex_userdata) nls.kkt;

      int iter[] = new int[1];
      double pobj[] = new double[1];

      if (iLoc==LSLOC_LOCAL_OPT)
      {
        LSgetCallbackInfo(pModel,iLoc,LS_IINFO_NLP_ITER,iter);
        LSgetCallbackInfo(pModel,iLoc,LS_DINFO_POBJ,pobj);
        System.out.println("@local solution " + _kkt.counter++ +
                           " iter = " + iter[0] +
                           " obj = " + pobj[0] +
                           " loc = " + iLoc);
      }
      else if (iLoc==LSLOC_CONOPT)
      {
        LSgetCallbackInfo(pModel,iLoc,LS_IINFO_NLP_ITER,iter);
        LSgetCallbackInfo(pModel,iLoc,LS_DINFO_POBJ,pobj);
        System.out.println("                " +
                           " iter = " + iter[0] +
                           " obj = " + pobj[0] +
                           " loc = " + iLoc);
      }
      return 0;
    } /*local_sol_log*/

    private static double  g1( double X, double Y)
    {
      return  Math.exp( -Math.pow(X  ,2) - Math.pow(Y+1,2) );
    }

    private static double  g2( double X, double Y)
    {
      return  Math.exp( -Math.pow(X  ,2) - Math.pow(Y,2) );
    }

    private static double  g3( double X, double Y)
    {
      return  Math.exp( -Math.pow(X+1  ,2) - Math.pow(Y,2) );
    }

    private static double  f1( double X, double Y)
    {
      return   Math.pow(1-X  ,2) ;
    }

    private static double  f2( double X, double Y)
    {
      return  ( X/5 - Math.pow(X  ,3) - Math.pow(Y,5) );
    }

    /*
     *  Callback function to evaluate functional values
     */
    private static int Funcalc8 (Object pModel, Object pUserData,
                                 int      nRow  ,double  pdX[],
                                 int      nJDiff,double  dXJBase,
                                 double   pdFuncVal[],int  pReserved[])
    {
      ex_nlp3 nls = (ex_nlp3) pUserData;
      ex_userdata _fde = (ex_userdata) nls.fde;
      _fde.counter++;

      double val=0.0, X = pdX[0], Y = pdX[1];
      int    nerr=0;
      /* compute objective's functional value*/
      if (nRow==-1)
        val = 3*f1(X,Y)*g1(X,Y) - 10*f2(X,Y)*g2(X,Y) - g3(X,Y)/3;
      /* compute constaint 0's functional value */
      else if (nRow==0)
        val = X*X + Y - 6.0;
      /* compute constaint 1's functional value */
      else if (nRow==1)
        val = X + Y*Y - 6.0;

      pdFuncVal[0]=val;

      return nerr;
    } /*Funcalc8*/


    // Generalized error Reporting function
    private static void APIErrorCheck(Object pEnv1 )
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
      // The runtime system executes a class's static
      // initializer when it loads the class.
      System.loadLibrary("lindojni");
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


    public static void main (String[] args)
    {

        ex_nlp3 ls = new ex_nlp3();

        /* Number of constraints */
        int nM = 2;

        /* Number of variables */
        int nN = 2;

        /* The number of nonzeros in the constraint matrix */
        int nNZ = 4;


        int nStatus[] = new int[1];

        /* >>> Step 1 <<< Read license file and create a LINDO environment. */
        nErrorCode[0] = ls.LSloadLicenseString("../../license/lndapi140.lic",cLicenseKey);


        APIVERSION();
        pEnv = ls.LScreateEnv(nErrorCode, cLicenseKey.toString());
        if ( nErrorCode[0] == ls.LSERR_NO_VALID_LICENSE)
        {
            System.out.println("Invalid License Key!\n");
            System.exit(1);
        }
        APIErrorCheck(pEnv);

        /* >>> Step 2 <<< Create a model in the environment. */
        pModel = ls.LScreateModel ( pEnv, nErrorCode);
        APIErrorCheck(pEnv);

        /* >>> Step 3 <<< Specify the model.

        /* The direction of optimization */
        int nDir = ls.LS_MIN;

        /* The objective's constant term */
        double dObjConst = 0.;

        /* The coefficients of the objective function */
        double adC[] = new double[] { 0, 0};

        /* The right-hand sides of the constraints */
        double adB[] = new double[] { 0, 0};

        /* The indices of the first nonzero in each column */
        int anBegCol[] = new int[]{ 0, 2, 4};

        /* The length of each column.  Since we aren't leaving
        any blanks in our matrix, we can set this to NULL */

        int anLenCol[] = new int[] { 2 , 2};

        /* The nonzero coefficients */
        double adA[] = new double[] { 0, 1, 1, 0};

        /* The row indices of the nonzero coefficients */
        int anRowX[] = new int[]{ 0, 1, 0, 1};

        /* Simple upper and lower bounds on the variables.
        By default, all variables have a lower bound of zero
        and an upper bound of infinity.  Therefore pass NULL
        pointers in order to use these default values. */

        double pdLower[] = new double[] { -3, -3};
        double pdUpper[] = new double[] { +3, +3};

        /* The constraint types */
        String acConTypes = "LL";
        String acVarTypes = "CC";

        /* We have now assembled a full description of the model.
        We pass this information to LSloadLPData with the
        following call. */
        nErrorCode[0] = ls.LSloadLPData( pModel, nM, nN, nDir,
            dObjConst, adC, adB, acConTypes, nNZ, anBegCol,
            anLenCol, adA, anRowX, pdLower, pdUpper);
        APIErrorCheck(pEnv);

        ls.LSloadVarType(pModel,acVarTypes);

        /* The number of nonlinear variables in each column */
        anLenCol[0]=1; anLenCol[1]=1;

        /* The indices of the first nonlinear variable in each column */
        anBegCol[0]=0; anBegCol[1]=1; anBegCol[2]=2;

        /* The indices of nonlinear constraints */
        anRowX[0]=0;
        anRowX[1]=1;

        /* The indices of variables that are nonlinear in the objective*/
        int Nobjndx[] = new int[]{ 0, 1};

        /* Number nonlinear variables in cost. */
        int Nnlobj = 2;

        /* Load the nonlinear structure */
        nErrorCode[0]=ls.LSloadNLPData(pModel,anBegCol,anLenCol,null,
                                          anRowX,Nnlobj,Nobjndx,null);
        APIErrorCheck(pEnv);

       /* Install a callback function  */
        nErrorCode[0] = ls.LSsetCallback(pModel,"local_sol_log",ls);
        APIErrorCheck(pEnv);

        /* Set the print level to 1 */
        nErrorCode[0] = ls.LSsetModelIntParameter(pModel,ls.LS_IPARAM_NLP_PRINTLEVEL,1);
        APIErrorCheck(pEnv);

        /* Install the routine that will calculate the function values. */
        nErrorCode[0] = ls.LSsetFuncalc(pModel,"Funcalc8",ls);
        APIErrorCheck(pEnv);

        nErrorCode[0] = ls.LSsetModelIntParameter(pModel,ls.LS_IPARAM_NLP_SOLVER,ls.LS_NMETHOD_MSW_GRG);

        nErrorCode[0] = ls.LSsetModelIntParameter(pModel,ls.LS_IPARAM_NLP_MAXLOCALSEARCH,5);


        /* >>> Step 4 <<< Perform the optimization */
        nErrorCode[0] = ls.LSsolveMIP( pModel, nStatus);
        System.out.println("Status = " + nStatus[0]);
        APIErrorCheck(pEnv);

        /* >>> Step 5 <<< Retrieve the solution */
        int i;
        double adX[] = new double[2], dObj[] = new double[1];

        /* Get the value of the objective */
        nErrorCode[0] = ls.LSgetInfo(pModel, LS_DINFO_MIP_OBJ, dObj) ;
        APIErrorCheck(pEnv);


        System.out.print("Objective Value = " + dObj[0] + "\n");

        /* Get the variable values */
        nErrorCode[0] = ls.LSgetMIPPrimalSolution (pModel, adX);
        APIErrorCheck(pEnv);

        System.out.println("Primal values");
        for (i = 0; i < nN; i++) System.out.println("\tx["+i+"] = "+adX[i]);
        System.out.print("\n");


        /* >>> Step 6 <<< Delete the LINDO environment */
        nErrorCode[0] = ls.LSdeleteEnv( pEnv);

        System.out.println("Number of function evaluations = " + fde.counter);
        System.out.println("Number of KKT points found = " + kkt.counter);
        System.out.println("Done!\n");

    }
}
