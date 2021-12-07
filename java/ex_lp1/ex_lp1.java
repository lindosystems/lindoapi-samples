/* ex_lp1.java

  A Java programming example of interfacing with the
  LINDO API.

  The problem:

     MAX = 20 * A + 30 * C
     S.T.       A +  2 * C  <= 120
                A           <=  60
                         C  <=  50

   Solving such a problem with the LINDO API involves
   the following steps:

      1. Create a LINDO environment.
      2. Create a model in the environment.
      3. Specify the model.
      4. Perform the optimization.
      5. Retrieve the solution.
      6. Delete the LINDO environement.
*/

import com.lindo.*;

class ex_userdata
{
    int counter;
}


public class ex_lp1 extends Lindo
{
    private static int nErrorCode[] = new int[1];
    private static StringBuffer cErrorMessage = new StringBuffer(LS_MAX_ERROR_MESSAGE_LENGTH);
    private static StringBuffer cLicenseKey = new StringBuffer(LS_MAX_ERROR_MESSAGE_LENGTH);


    // Generalized error Reporting function
    private static void APIErrorCheck(Object /*long*/ pEnv )
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

    public static void main (String[] args)
    {

        ex_lp1 ls = new ex_lp1();

        /* Number of constraints */
        int nM = 3;

        /* Number of variables */
        int nN = 2;

        /* declare an instance of the LINDO environment object */
        Object /*long*/ pEnv = null;

        /* declare an instance of the LINDO model object */
        Object /*long*/ pModel = null;

        int nSolStatus[] = new int[1];

        /* >>> Step 1 <<< Read license file and create a LINDO environment. */
        nErrorCode[0] = ls.LSloadLicenseString("../../license/lndapi130.lic",cLicenseKey);
        APIErrorCheck(pEnv);

        APIVERSION();
        pEnv = ls.LScreateEnv(nErrorCode, cLicenseKey.toString());
        APIErrorCheck(pEnv);

        /* >>> Step 2 <<< Create a model in the environment. */
        pModel = ls.LScreateModel ( pEnv, nErrorCode);
        APIErrorCheck(pEnv);

        /* >>> Step 3 <<< Specify the model.

        /* The direction of optimization */
        int nDir = ls.LS_MAX;

        /* The objective's constant term */
        double dObjConst = 0.;

        /* The coefficients of the objective function */
        double adC[] = new double[] { 20., 30.};

        /* The right-hand sides of the constraints */
        double adB[] = new double[] { 120., 60., 50.};

        /* The constraint types */
        String acConTypes = "LLL";


        /* The number of nonzeros in the constraint matrix */
        int nNZ = 4;

        /* The indices of the first nonzero in each column */
        int anBegCol[] = new int[]{ 0, 2, nNZ};

        /* The length of each column.  Since we aren't leaving
        any blanks in our matrix, we can set this to NULL */
        int pnLenCol[] = null;

        /* The nonzero coefficients */
        double adA[] = new double[] { 1., 1., 2., 1.};

        /* The row indices of the nonzero coefficients */
        int anRowX[] = new int[]{ 0, 1, 0, 2};

        /* Simple upper and lower bounds on the variables.
        By default, all variables have a lower bound of zero
        and an upper bound of infinity.  Therefore pass NULL
        pointers in order to use these default values. */
        double pdLower[] = null,
               pdUpper[] = null;

        String varnames[] = {"Variable1","Variable2"};
        String connames[] = {"Constraint1","Constraint2","Constraint3"};

        /* We have now assembled a full description of the model.
        We pass this information to LSloadLPData with the
        following call. */
        nErrorCode[0] = ls.LSloadLPData( pModel, nM, nN, nDir,
            dObjConst, adC, adB, acConTypes, nNZ, anBegCol,
            pnLenCol, adA, anRowX, pdLower, pdUpper);
        APIErrorCheck(pEnv);

        nErrorCode[0] = ls.LSloadNameData(pModel, "MyTitle","MyObj",null,null,
        null,connames,varnames,null);
        APIErrorCheck(pEnv);

        /* >>> Step 4 <<< Perform the optimization */
        nErrorCode[0] = ls.LSoptimize( pModel, ls.LS_METHOD_PSIMPLEX, nSolStatus);
        APIErrorCheck(pEnv);

        /* >>> Step 5 <<< Retrieve the solution */
        int i;
        double adX[] = new double[2],
           adR[] = new double[2],
           adS[] = new double[3],
           adY[] = new double[3],
           dObj[] = new double[1];

        /* Get the value of the objective */
        nErrorCode[0] = ls.LSgetInfo(pModel, LS_DINFO_POBJ, dObj) ;
        APIErrorCheck(pEnv);

        System.out.print("Objective Value = " + dObj[0] + "\n");

        /* Get the variable values */
        nErrorCode[0] = ls.LSgetPrimalSolution (pModel, adX);
        APIErrorCheck(pEnv);

        /* Get the slack values */
        nErrorCode[0] = ls.LSgetSlacks (pModel, adS);
        APIErrorCheck(pEnv);

        /* Get the variable values */
        nErrorCode[0] = ls.LSgetDualSolution (pModel, adY);
        APIErrorCheck(pEnv);

        /* Get the slack values */
        nErrorCode[0] = ls.LSgetReducedCosts (pModel, adR);
        APIErrorCheck(pEnv);

        StringBuffer varName=new StringBuffer();
        int varIdx[] = new int[1];
        System.out.print("\n");
        System.out.println("Primal solution");
        for (i = 0; i < nN; i++)
        {
          nErrorCode[0]=LSgetVariableNamej(pModel, i ,varName);
          System.out.println(varName.toString() + "\t" + adX[i] + "\t" + adR[i]);
          //nErrorCode[0]=LSgetVariableIndex(pModel,varName.toString(),varIdx);
          //System.out.println(varName.toString() + "\t" + varIdx[0]);
          varName.setLength(0);
        }
        System.out.print("\n");

        System.out.println("Dual solution");
        for (i = 0; i < nM; i++) System.out.println(connames[i] + "\t" + adY[i] + "\t" + adS[i]);
        System.out.print("\n");

        /* >>> Step 6 <<< Delete the LINDO environment */
        nErrorCode[0] = ls.LSdeleteModel( pModel);

        nErrorCode[0] = ls.LSdeleteEnv( pEnv);

    }
}
