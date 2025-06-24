/* ex_markow.cs
#################################################################
#                       LINDO-API
#                    Sample Programs
#                  Copyright (c) 2006
#
#         LINDO Systems, Inc.           312.988.7422
#         1415 North Dayton St.         info@lindo.com
#         Chicago, IL 60622             http://www.lindo.com
#################################################################
  File   : ex_markow.cs
  Purpose: Solve a quadratic programming problem.
  Model  : The Markowitz Portfolio Selection Model

           MAXIMIZE  r(1)w(1) + ... +r(n)w(n)
           st.       sum_{ij} Q(i,j)w(i)w(j) <= K
                         w(1) + ..... + w(n)  = 1
                         w(1),         ,w(n) >= 0
           where
           r(i)  : return on asset i
           Q(i,j): covariance between the returns of i^th and
                   j^th assets.
           K     : a scalar denoting the level of risk of loss.
           w(i)  : proportion of total budget invested on asset i

  Covariance Matrix:
          w1    w2    w3    w4
     w1 [ 1.00  0.64  0.27  0.    ]
     w2 [ 0.64  1.00  0.13  0.    ]
     w3 [ 0.27  0.13  1.00  0.    ]
     w4 [ 0.    0.    0.    1.00  ]

  Returns Vector:
          w1    w2    w3    w4
  r =   [ 0.30  0.20 -0.40  0.20  ]

  Risk of Loss Factor:
  K = 0.4
*/

using System;
using System.Text;
using System.IO;
using System.Runtime.InteropServices;
using System.Runtime.Remoting;
using System.Windows.Forms;


[StructLayout(LayoutKind.Sequential)]
public class CallbackData
{
    public int count;

    // Constructor:    
    public CallbackData()
    {
        count = 0;
    }
}

public class ex_markow
{
    //
    // Please refer to your MSDN user manual for further information
    // on Marshal methods for C#
    //
    public static int MyCallback(IntPtr pMod, int nLoc, IntPtr myData)
    {
        int iter = 0;
        double pinf = 0, pobj = 0;

        CallbackData cb = new CallbackData();

        Marshal.PtrToStructure(myData, cb);

        lindo.LSgetCallbackInfo(pMod, 0, lindo.LS_DINFO_PINFEAS, ref pinf);
        lindo.LSgetCallbackInfo(pMod, 0, lindo.LS_DINFO_POBJ, ref pobj);
        lindo.LSgetCallbackInfo(pMod, 0, lindo.LS_IINFO_SIM_ITER, ref iter);
        cb.count++;
        Console.WriteLine("callback @iter={0}, obj={1}, pinf={2}, mydata={3}", iter, pobj, pinf, cb.count);
        

        Marshal.StructureToPtr(cb, myData, true);

        return 0;
    }

    public static void APIErrorCheck(IntPtr pEnv, int nErr)
    {
        if (nErr > 0)
        {
            StringBuilder cMessage = new StringBuilder(lindo.LS_MAX_ERROR_MESSAGE_LENGTH);
            lindo.LSgetErrorMessage(pEnv, nErr, cMessage);
            MessageBox.Show(cMessage.ToString());
        }
    }
    
	public static void DisplayVersionInfo()
	{
        StringBuilder cMessage1 = new StringBuilder(lindo.LS_MAX_ERROR_MESSAGE_LENGTH);
        StringBuilder cMessage2 = new StringBuilder(lindo.LS_MAX_ERROR_MESSAGE_LENGTH);
        lindo.LSgetVersionInfo(cMessage1, cMessage2);
        Console.WriteLine(cMessage1);
        Console.WriteLine(cMessage2);
	}     

    public static void Main(string[] args)
    {

        int nErrorCode = lindo.LSERR_NO_ERROR;


        int nCons = 2;      /* Number of constraints */
        int nVars = 4;      /* Number of assets */

        double K = 0.20; /* 1/2 of the risk level*/

        /* declare an instance of the LINDO environment object */
        IntPtr pEnv = (IntPtr)0;

        /* declare an instance of the LINDO model object */
        IntPtr pModel = (IntPtr)0;

        /* initialize the counter that counts the number of times 
        the callback function is called */
        CallbackData cbData = new CallbackData();
		cbData.count = 0;
		IntPtr myData = Marshal.AllocHGlobal(Marshal.SizeOf(cbData));
		Marshal.StructureToPtr(cbData, myData, true);
		
        int nSolStatus = lindo.LS_STATUS_UNKNOWN;

        StringBuilder LicenseKey = new StringBuilder(lindo.LS_MAX_ERROR_MESSAGE_LENGTH);

        /* >>> Step 1 <<< Create a LINDO environment. Note:
        MY_LICENSE_KEY must be defined to be the license key
        shipped with your software. */

        string LicenseFile = System.Environment.GetEnvironmentVariable("LINDOAPI_HOME") + "\\license\\lndapi160.lic";
        
        nErrorCode = lindo.LSloadLicenseString(LicenseFile, LicenseKey);
        APIErrorCheck(pEnv,nErrorCode);


        pEnv = lindo.LScreateEnv(ref nErrorCode, LicenseKey.ToString());
        if (nErrorCode == lindo.LSERR_NO_VALID_LICENSE)
        {
            Console.WriteLine("Invalid License Key!\n");
            return;
        }
        APIErrorCheck(pEnv, nErrorCode);

        /* >>> Step 2 <<< Create a model in the environment. */
        pModel = lindo.LScreateModel(pEnv, ref nErrorCode);
        APIErrorCheck(pEnv, nErrorCode);

        /*****************************************************************
        * Step 3: Specify and load the LP portion of the model.
        *****************************************************************/
        /* The direction of optimization */
        int objsense = lindo.LS_MAX;
        /* The objective's constant term */
        double objconst = 0.0;
        /* The coefficients of the objective function are the expected
        returns*/
        double[] reward = new double[] { 0.3, 0.2, -0.4, 0.2 };
        /* The right-hand sides of the constraints */
        double[] rhs = new double[] { K, 1.0 };
        /* The constraint types */
        string contype = "LE";
        /* The number of nonzeros in the constraint matrix */
        int Anz = 4;
        /* The indices of the first nonzero in each column */
        int[] Abegcol = new int[] { 0, 1, 2, 3, Anz };
        /* The length of each column.  Since we aren't leaving
        * any blanks in our matrix, we can set this to NULL */
        int[] Alencol = null;
        /* The nonzero coefficients */
        double[] A = new double[] { 1.0, 1.0, 1.0, 1.0 };
        /* The row indices of the nonzero coefficients */
        int[] Arowndx = new int[] { 1, 1, 1, 1 };
        /* By default, all variables have a lower bound of zero
        * and an upper bound of infinity.  Therefore pass NULL
        * pointers in order to use these default values. */

        double[] lb = new double[] { 0.0, 0.0, 0.0, 0.0 };
        double[] ub = new double[] { lindo.LS_INFINITY, lindo.LS_INFINITY, lindo.LS_INFINITY, lindo.LS_INFINITY };
        /*****************************************************************
        * Step 4: Specify and load the quadratic matrix
        *****************************************************************/
        /* The number of nonzeros in the quadratic matrix */
        int Qnz = 7;
        /* The nonzero coefficients in the Q-matrix */
        double[] Q = new double[]{ 1.00, .64, .27,
				              1.00, .13,
				              1.00,
				              1.00};
        /* Specify the row indices of the nonzero coefficients in the
         Q-matrix. */
        int[] Qrowndx = new int[] { 0, 0, 0, 0, 0, 0, 0 };
        /* The indices of variables in the Q-matrix */
        int[] Qcolndx1 = new int[] { 0, 1, 2, 1, 2, 2, 3 };
        int[] Qcolndx2 = new int[] { 0, 0, 0, 1, 1, 2, 3 };
        /* Pass the linear portion of the data to problem structure
        * by a call to lindo.LSloadLPData() */
        nErrorCode = lindo.LSloadLPData(pModel, nCons, nVars, objsense, objconst,
                    reward, rhs, contype,
                    Anz, Abegcol, Alencol, A, Arowndx,
                    lb, ub);
        APIErrorCheck(pEnv, nErrorCode);
        /* Pass the quadratic portion of the data to problem structure
        * by a call to lindo.LSloadQCData()  */
        nErrorCode = lindo.LSloadQCData(pModel, Qnz, Qrowndx,
                     Qcolndx1, Qcolndx2, Q);
        APIErrorCheck(pEnv, nErrorCode);

        lindo.typCallback cb = new lindo.typCallback(ex_markow.MyCallback);
        nErrorCode = lindo.LSsetCallback(pModel, cb, cbData);
        APIErrorCheck(pEnv, nErrorCode);

        /* >>> Step 4 <<< Perform the optimization */
        nErrorCode = lindo.LSoptimize(pModel, lindo.LS_METHOD_BARRIER, ref nSolStatus);
        APIErrorCheck(pEnv, nErrorCode);

        /***************************************************************
        * Step 6: Retrieve the solution
        ***************************************************************/
        int i;
        double[] W = new double[4];
        double dObj = lindo.LS_INFINITY;
        /* Get the value of the objective */
        nErrorCode = lindo.LSgetInfo(pModel, lindo.LS_DINFO_POBJ, ref dObj);
        APIErrorCheck(pEnv, nErrorCode);
        Console.WriteLine("* Objective Value = {0}\n\n", dObj);
        /* Get the portfolio */
        nErrorCode = lindo.LSgetPrimalSolution(pModel, W);
        APIErrorCheck(pEnv, nErrorCode);
        Console.WriteLine("* Optimal Portfolio : \n");
        for (i = 0; i < nVars; i++)
            Console.WriteLine("Invest {0} percent of total budget in asset {1}.\n",
                   100 * W[i], i + 1);
        Console.WriteLine("\n");


        Console.WriteLine("\nDisplaying raw solution vectors \n");
        double[] adX = new double[nVars];
        double[] adR = new double[nVars];
        double[] adS = new double[nCons];
        double[] adY = new double[nCons];

        /* Get the value of the objective */
        nErrorCode = lindo.LSgetInfo(pModel, lindo.LS_DINFO_POBJ, ref dObj);
        APIErrorCheck(pEnv, nErrorCode);
        
        Console.WriteLine("Objective Value = {0}, callbacks {1}\n", dObj, cbData.count);

        /* Get the variable values */
        nErrorCode = lindo.LSgetPrimalSolution(pModel, adX);
        APIErrorCheck(pEnv, nErrorCode);

        /* Get the slack values */
        nErrorCode = lindo.LSgetSlacks(pModel, adS);
        APIErrorCheck(pEnv, nErrorCode);

        /* Get the variable values */
        nErrorCode = lindo.LSgetDualSolution(pModel, adY);
        APIErrorCheck(pEnv, nErrorCode);

        /* Get the slack values */
        nErrorCode = lindo.LSgetReducedCosts(pModel, adR);
        APIErrorCheck(pEnv, nErrorCode);

        Console.WriteLine("Primal solution");
        StringBuilder namebuf = new StringBuilder(lindo.LS_MAX_ERROR_MESSAGE_LENGTH);
        for (i = 0; i < nVars; i++)
        {
            nErrorCode = lindo.LSgetVariableNamej(pModel, i, namebuf);
            Console.WriteLine("{0} {1} {2} {3} {4}", namebuf, "\t", adX[i], "\t", adR[i]);
        }
        Console.WriteLine("\n");

        Console.WriteLine("Dual solution");
        for (i = 0; i < nCons; i++)
        {
            nErrorCode = lindo.LSgetConstraintNamei(pModel, i, namebuf);
            Console.WriteLine("{0} {1} {2} {3} {4}", namebuf, "\t", adY[i], "\t", adS[i]);
        }

        Console.WriteLine("\n");
        
        Marshal.FreeHGlobal(myData); 

        /* >>> Step 6 <<< Delete the LINDO environment */
        nErrorCode = lindo.LSdeleteModel(ref pModel);

        nErrorCode = lindo.LSdeleteEnv(ref pEnv);

    }
}
