import com.lindo.*;


public class ex_modify extends Lindo
{

    /* ex_modify.java

    A Java programming example of interfacing with the
    LINDO API demonstrating

      1) Adding new variables and constraints.
      2) Modifying objective coefficients

    Original problem:

       MAX = 20 * A + 30 * C
       S.T.       A +  2 * C  <= 120
                  A           <=  60
                           C  <=  50

    Modified problem after adding a variable:

       MAX = 20 * A + 30 * C  - 2 *D
       S.T.       A +  2 * C          <= 120
                  A                   <=  60
                           C          <=  50

    Modified problem after adding a constraint:

       MAX = 20 * A + 30 * C  - 2 *D
       S.T.       A +  2 * C          <= 120
                  A                   <=  60
                           C          <=  50
           [               C  + D     >=  50]  (*)


    Modified problem after changing objective
    coefficients.

       MAX= -10 * A + 30 * C  - 3 *D
       S.T.       A +  2 * C          <= 120
                  A                   <=  60
                           C          <=  50
                           C  + D     >=  50


     Solving such a problem with the LINDO API involves
     the following steps:

        1.  Create a LINDO environment.
        2.  Create a model in the environment.
        3.  Specify the model.
        4a. Add a new variable
        4b. Add a new constraint
        4c. Modify obj coeff
        5.  Perform the optimization.
        6.  Retrieve the solution.
        7.  Delete the LINDO environement.
    */

    private static int nErrorCode[] = new int[1];
    private static StringBuffer cErrorMessage = new StringBuffer(LS_MAX_ERROR_MESSAGE_LENGTH);
    private static StringBuffer cLicenseKey = new StringBuffer(LS_MAX_ERROR_MESSAGE_LENGTH);
    double[][] array = null;
    double[] adC = null;
    double[] adB = null;

    private static Object pEnv = null;
    private static Object pModel = null;

    private static Object[] a = null;
    /* Number of constraints, Number of variables  */static int nM = 0, nN = 0;
    /* The length of each column.  Since we aren't leaving any blanks in our matrix, we can set this to null */
    static int pnLenCol[] = null;

    /* The nonzero coefficients */static double adA[] = null;

    /* The row indices of the nonzero coefficients */static int anRowX[] = null;

    /* Simple upper and lower bounds on the variables.
         By default, all variables have a lower bound of zero
         and an upper bound of infinity.  Therefore pass null
         pointers in order to use these default values. */
    static double pdLower[] = null, pdUpper[] = null;

    /* The number of nonzeros in the constraint matrix */static int nNZ = 0;
    /* The indices of the first nonzero in each column */static int anBegCol[] = null;
    /* The length of each column. Since there are no balnks can be null */static int[]
        Alencol = null;
    /* LB and UB of the variable, null means 0 LB and infinity UB */static double[]
        lb = null, ub = null;

    static String varnames[] = null, connames[] = null;

    static {
      // The runtime system executes a class's static
      // initializer when it loads the class.

      System.loadLibrary("lindojni");
      nErrorCode[0] = LSloadLicenseString("../../license/lndapi150.lic", cLicenseKey);
      APIErrorCheck(pEnv);

      pEnv = LScreateEnv(nErrorCode, cLicenseKey.toString());
      APIErrorCheck(pEnv);

      pModel = LScreateModel(pEnv, nErrorCode);
    }

    // Generalized error Reporting function
    private static void APIErrorCheck(Object pEnv) {
      if (0 != nErrorCode[0]) {
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


    public static void  main(String[] argv)
    {

    /* Number of constraints */
     int nM = 3;

    /* Number of variables */
     int nN = 2;


    /* >>> Step 3 <<< Specify the model.

    To specify our model, we make a call to LSloadLPData,
    passing it:

    - A pointer to the model which we are specifying(pModel)
    - The number of constraints in the model
    - The number of variables in the model
    - The direction of the optimization (i.e. minimize or
    -  maximize)
    - The value of the constant term in the objective (may
      be zero)
    - The coefficients of the objective function
    - The right-hand sides of the constraints
    - The types of the constraints
    - The number of nonzeros in the constraint matrix
    - The indices of the first nonzero in each column
    - The length of each column
    - The nonzero coefficients
    - The row indices of the nonzero coefficients
    - Simple upper and lower bounds on the variables
    */

    /* The direction of optimization */
        int nDir = LS_MAX;

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
        int anBegCol[] = new int[] { 0, 2, nNZ};

    /* The length of each column.  Since we aren't leaving
      any blanks in our matrix, we can set this to null */
        int pnLenCol[] = null;

    /* The nonzero coefficients */
        double adA[] = new double[]{ 1., 1., 2., 1.};

    /* The row indices of the nonzero coefficients */
        int anRowX[] = new int[] { 0, 1, 0, 2};

    /* Simple upper and lower bounds on the variables.
      By default, all variables have a lower bound of zero
      and an upper bound of infinity.  Therefore pass null
      pointers in order to use these default values. */
        double pdLower[] = null, pdUpper[] = null;

     APIVERSION();

    /* We have now assembled a full description of the model.
      We pass this information to LSloadLPData with the
      following call. */
        nErrorCode[0] = LSloadLPData(pModel, nM, nN, nDir,
         dObjConst, adC, adB, acConTypes, nNZ, anBegCol,
         pnLenCol, adA, anRowX, pdLower, pdUpper);
        APIErrorCheck(pEnv);

       String varnames[] = {"Variable1","Variable2"};
       String connames[] = {"Constraint1","Constraint2","Constraint3"};

       nErrorCode[0] = LSloadNameData(pModel, "MyTitle","MyObj",null,null,
          null,connames,varnames,null);
       APIErrorCheck(pEnv);

       nErrorCode[0] = LSwriteLINDOFile(pModel,"ex_modify/original.ltx");
       APIErrorCheck(pEnv);

    /* >>> Step 4.a <<< Add a new variable */
       {
         int nA = 1;
         String achVtype = "C";
         String apszVname[] = {"NEW_VAR"};
         double c[]= new double [] {-2.0};

         /* It is assumed that the new variable has no nonzeros in the existing
         constraints, therefore, sparse matrix data are set to NULL.*/
         nErrorCode[0] = LSaddVariables(pModel, nA,achVtype,apszVname,null,null,null,
                                     null,c,pdLower, pdUpper);

         System.out.println("Called  LSaddVariables to add new variables.\n");
         nErrorCode[0] =LSwriteLINDOFile(pModel,"ex_modify/modified-1.ltx");
         APIErrorCheck(pEnv);

       }

      /* >>> Step 4.b <<< Add a new constraint */
       {
         int nCon = 1;
         String achCtype = "G";
         String apszCname[] = {"NEW_ROW"};
         int ia[]= new int [] {1,2};
         int ka[]= new int [] {0,2};
         double a[]= new double []{1, 1};
         double rhs[]= new double [] {50.0};

         /* It is assumed that the new variable has no nonzeros in the existing
         constraints, therefore, sparse matrix data are set to NULL.*/
         nErrorCode[0] = LSaddConstraints(pModel, nCon,achCtype,apszCname,ka,a,ia,rhs);

         System.out.println("Called  LSaddConstraints to add new constraints.\n");

         nErrorCode[0] =LSwriteLINDOFile(pModel,"ex_modify/modified-2.ltx");
         APIErrorCheck(pEnv);

       }


      /* >>> Step 4.c <<< Modify coeff of variables */
       {
         int i;
         double padC   [] = new double[2];
         int    paiVars[] = new int[2];

         /* indices of the variables to be modified */
         paiVars[0] = 0;
         paiVars[1] = 1;

         /* new obj coefficients of these variables */
         padC[0] = -10;
         padC[1] = -3;

         nErrorCode[0] = LSmodifyObjective(pModel,2, paiVars, padC);
         APIErrorCheck(pEnv);

         nErrorCode[0] = LSwriteLINDOFile(pModel,"ex_modify/modified-3.ltx");
         APIErrorCheck(pEnv);
       }

       if (true) {
      /* >>> Step 6 <<< Retrieve the solution */
          int i;
          double adX[] = new double[100], dObj[]= new double[1];
          double adDec[] = new double[100], adInc[]= new double[100];

      /* >>> Step 5 <<< Perform the optimization */
         nErrorCode[0] = LSoptimize( pModel, LS_METHOD_PSIMPLEX, null);
         APIErrorCheck(pEnv);

      /* Get the value of the objective */
          nErrorCode[0] = LSgetInfo(pModel,LS_DINFO_POBJ, dObj);
         APIErrorCheck(pEnv);

          System.out.println( "Objective Value =\n"+dObj[0]);

      /* Get the variable values */
          nErrorCode[0] = LSgetPrimalSolution ( pModel, adX);
          APIErrorCheck(pEnv);

          int[] nVar = new int[1];
          nErrorCode[0] = LSgetInfo ( pModel, LS_IINFO_NUM_VARS, nVar);
          System.out.println("Primal values = ");
          for (i = 0; i < nN; i++) System.out.println( "x["+i+"]="+adX[i]);
          System.out.println("\n");

          /* >>> Step 7 <<< Delete the LINDO environment */
          nErrorCode[0] = LSdeleteEnv( pEnv);

       }
    }
}

