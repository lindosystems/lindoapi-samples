//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////
////    LINDO API Version 5.0
////    Copyright (c; 2000-2007
////
////    LINDO Systems, Inc.            312.988.7422
////    1415 North Dayton St.          info@lindo.com
////    Chicago, IL 60622              http://www.lindo.com
////
////    @ex_lp1.cs 
////
////    last updated: 04-03-2007
////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/* ex_lp1.cs

  A C# programming example of interfacing with the
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

using System;
using System.Text;
using System.IO;
using System.Runtime.InteropServices;
using System.Runtime.Remoting;
using System.Windows.Forms;


[ StructLayout( LayoutKind.Sequential )]
public class CallbackData
{
	public int count;

	// Constructor:    
	public CallbackData() 
	{
        count=0;
    }
}

public class ex_lp1
{
    //
    // Please refer to your MSDN user manual for further information
    // on Marshal methods for C#
    //
    public static int MyCallback(IntPtr pMod, int nLoc,  IntPtr  myData)  
    {
	  int iter=0;
	  double pinf=0, pobj=0;   
      
      CallbackData cb = new CallbackData();      
                 
      Marshal.PtrToStructure (myData, cb);
      
      lindo.LSgetCallbackInfo(pMod,0,lindo.LS_DINFO_PINFEAS, ref pinf);
      lindo.LSgetCallbackInfo(pMod,0,lindo.LS_DINFO_POBJ,ref pobj);
      lindo.LSgetCallbackInfo(pMod,0,lindo.LS_IINFO_SIM_ITER, ref iter);
      Console.WriteLine("callback @iter={0}, obj={1}, pinf={2}, mydata={3}" ,iter,pobj,pinf,cb.count );
      cb.count++;
            
      Marshal.StructureToPtr (cb,myData,true);

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

	public static void Main (string[] args)
	{

		int nErrorCode = lindo.LSERR_NO_ERROR;

		/* Number of constraints */
		int nCons = 3;

		/* Number of variables */
		int nVars = 2;

		/* declare an instance of the LINDO environment object */
		IntPtr pEnv = (IntPtr) 0;

		/* declare an instance of the LINDO model object */
		IntPtr pModel = (IntPtr) 0;        
                
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

        nErrorCode = lindo.LSloadLicenseString("..\\..\\..\\..\\license\\lndapi150.lic", LicenseKey);        
		APIErrorCheck(pEnv,nErrorCode);
        
		DisplayVersionInfo();
		
		pEnv = lindo.LScreateEnv(ref nErrorCode, LicenseKey.ToString()); 
		if ( nErrorCode == lindo.LSERR_NO_VALID_LICENSE)  
		{
			Console.WriteLine("Invalid License Key!\n");
			return;
		}
		APIErrorCheck(pEnv,nErrorCode);        
        

		/* >>> Step 2 <<< Create a model in the environment. */
		pModel = lindo.LScreateModel ( pEnv, ref nErrorCode);
		APIErrorCheck(pEnv,nErrorCode);

		/* >>> Step 3 <<< Specify the model.

		/* The direction of optimization */
		int nDir = lindo.LS_MAX;

		/* The objective's constant term */
		double dObjConst = 0.0;

		/* The coefficients of the objective function */
		double [] adC = new double[] { 20.0, 30.0};

		/* The right-hand sides of the constraints */
		double [] adB = new double[] { 120.0, 60.0, 50.0};

		/* The constraint types */
		string acConTypes = "LLL";


		/* The number of nonzeros in the constraint matrix */
		int nNZ = 4;

		/* The indices of the first nonzero in each column */
		int [] anBegCol = new int[]{ 0, 2, nNZ};

		/* The length of each column.  Since we aren't leaving
		any blanks in our matrix, we can set this to NULL */
		int [] pnLenCol = new int[]{2, 2};

		/* The nonzero coefficients */
		double [] adA = new double[] { 1.0, 1.0, 2.0, 1.0};

		/* The row indices of the nonzero coefficients */
		int [] anRowX = new int[]{ 0, 1, 0, 2};

		/* Simple upper and lower bounds on the variables.
		By default, all variables have a lower bound of zero
		and an upper bound of infinity.  Therefore pass NULL
		pointers in order to use these default values. */
		double [] pdLower = new double[] {0.0, 0.0};
		double [] pdUpper = new double[] {lindo.LS_INFINITY,lindo.LS_INFINITY};

		string [] varnames = new string[] {"Variable1","Variable2"};
		string [] connames = new string[] {"Constraint1","Constraint2","Constraint3"};
	        	        

		/* We have now assembled a full description of the model.
		We pass this information to LSloadLPData with the
		following call. */
		nErrorCode = lindo.LSloadLPData( pModel, nCons, nVars, nDir,
			dObjConst, adC, adB, acConTypes, nNZ, anBegCol,
		    pnLenCol, adA, anRowX, pdLower, pdUpper);
		APIErrorCheck(pEnv,nErrorCode);


		nErrorCode = lindo.LSloadNameData(pModel, "MyTitle","MyObj",null,null,
		null,connames,varnames,null);
        APIErrorCheck(pEnv,nErrorCode);

        lindo.typCallback cb = new lindo.typCallback(ex_lp1.MyCallback);
	    nErrorCode = lindo.LSsetCallback(pModel,cb, cbData);
		APIErrorCheck(pEnv,nErrorCode);

		/* >>> Step 4 <<< Perform the optimization */
		nErrorCode = lindo.LSoptimize( pModel, lindo.LS_METHOD_FREE, ref nSolStatus);
		APIErrorCheck(pEnv,nErrorCode);

		/* >>> Step 5 <<< Retrieve the solution */
		int i=0;
		double [] adX = new double[2];
		double [] adR = new double[2];
		double [] adS = new double[3];
		double [] adY = new double[3];
		double dObj=0.0;
                      
		/* Get the value of the objective */
		nErrorCode = lindo.LSgetInfo(pModel, lindo.LS_DINFO_POBJ, ref dObj) ;
		APIErrorCheck(pEnv,nErrorCode);

		Console.WriteLine("Objective Value = {0}, #callbacks: {1}\n",dObj,cbData.count );

		/* Get the variable values */
		nErrorCode = lindo.LSgetPrimalSolution (pModel, adX);
		APIErrorCheck(pEnv,nErrorCode);

		/* Get the slack values */
		nErrorCode = lindo.LSgetSlacks (pModel, adS);
		APIErrorCheck(pEnv,nErrorCode);

		/* Get the variable values */
		nErrorCode = lindo.LSgetDualSolution (pModel, adY);
		APIErrorCheck(pEnv,nErrorCode);

		/* Get the slack values */
		nErrorCode = lindo.LSgetReducedCosts (pModel, adR);
		APIErrorCheck(pEnv,nErrorCode);
                
		Console.WriteLine("Primal solution");
		StringBuilder namebuf = new StringBuilder(lindo.LS_MAX_ERROR_MESSAGE_LENGTH);
		for (i = 0; i < nVars; i++) 
		{
		  nErrorCode = lindo.LSgetVariableNamej(pModel, i, namebuf);
		  Console.WriteLine("{0} {1} {2} {3} {4}",namebuf , "\t" , adX[i] , "\t" , adR[i]);
		}
		Console.WriteLine("\n");

		Console.WriteLine("Dual solution");
		for (i = 0; i < nCons; i++) 
		{
		  nErrorCode = lindo.LSgetConstraintNamei(pModel, i, namebuf);
		  Console.WriteLine("{0} {1} {2} {3} {4}",namebuf , "\t" , adY[i] , "\t" , adS[i]);
		}
		
		Console.WriteLine("\n");
		
		Marshal.FreeHGlobal(myData); 

		/* >>> Step 6 <<< Delete the LINDO environment */
		nErrorCode = lindo.LSdeleteModel( ref pModel);

		nErrorCode = lindo.LSdeleteEnv( ref pEnv);

	}
}
