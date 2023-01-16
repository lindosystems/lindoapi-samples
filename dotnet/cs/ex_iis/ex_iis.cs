//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////
////    LINDO API Version 8.0
////    Copyright (c; 2000-2014
////
////    LINDO Systems, Inc.            312.988.7422
////    1415 North Dayton St.          info@lindo.com
////    Chicago, IL 60622              http://www.lindo.com
////
////    @ex_iis.cs 
////
////    last updated: 07-25-2014
////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

using System;
using System.Text;
using System.IO;

using System.Runtime.InteropServices;

public struct CallbackData
{
	public int calls;
}



public class ex_iis
{

    public static int n_cons=0;

	public static int n_vars=0;

    public static int MyCallbackLog(IntPtr pMod, string Msg, IntPtr myData)
    {
        int iter = 0;
        double pinf = 0, pobj = 0;

        // copy the user data in the unmanaged code into a local structure
        CallbackData cb = (CallbackData)Marshal.PtrToStructure(myData, typeof(CallbackData));

        // increment the number of calls to the callback function
        cb.calls++;

        Console.Write("{0}", Msg.ToString());

        // copy the user data in the local structure back to the unmanaged code
        Marshal.StructureToPtr(cb, myData, false);

        return 0;
    }

	public static int MyCallbackCount(IntPtr pMod, int nLoc,  IntPtr  myData)  
	{
		int iter=0;
		double pinf=0, pobj=0;   
      
		// copy the user data in the unmanaged code into a local structure
		CallbackData cb = (CallbackData) Marshal.PtrToStructure (myData, typeof(CallbackData));          
		
		// increment the number of calls to the callback function
		cb.calls++;     			
      
		lindo.LSgetCallbackInfo(pMod,0,lindo.LS_DINFO_PINFEAS, ref pinf);
		lindo.LSgetCallbackInfo(pMod,0,lindo.LS_DINFO_POBJ,ref pobj);
		lindo.LSgetCallbackInfo(pMod,0,lindo.LS_IINFO_SIM_ITER, ref iter);
		Console.WriteLine("callback @iter={0}, obj={1}, pinf={2}, ncalls={3}" ,iter,pobj,pinf,cb.calls );
            
		// copy the user data in the local structure back to the unmanaged code
		Marshal.StructureToPtr (cb,myData,false);

		return 0;
	}


    public static int MyCallback(IntPtr model, int loc, IntPtr nvCbData ) 
	{
        int it=0, nErr=lindo.LSERR_NO_ERROR;
        double ob=0.0;
        // get iterations
        nErr = lindo.LSgetCallbackInfo(model, loc, lindo.LS_IINFO_SIM_ITER, ref it);
        // get primal objective
        nErr = lindo.LSgetCallbackInfo(model, loc, lindo.LS_DINFO_POBJ, ref ob);

        Console.WriteLine("callback @iter={0}, obj={1}",it,ob);

        return 0;
    }

	public static void DisplayVersionInfo()
	{
        StringBuilder cMessage1 = new StringBuilder(lindo.LS_MAX_ERROR_MESSAGE_LENGTH);
        StringBuilder cMessage2 = new StringBuilder(lindo.LS_MAX_ERROR_MESSAGE_LENGTH);
        lindo.LSgetVersionInfo(cMessage1, cMessage2);
        Console.WriteLine(cMessage1);
        Console.WriteLine(cMessage2);
	}     


    public static void CheckErr(IntPtr env, int errorcode)
	{

        // Checks for an error condition.  If one exists, the
        //  error message is displayed then the application
        //  terminates.

        if (errorcode > 0) 
		{
            StringBuilder message = new StringBuilder(256);
            lindo.LSgetErrorMessage(env, errorcode, message);
            Console.WriteLine(message);
			return;
        }

    }
   
    public static void Main(string[] args)    
	{
        IntPtr env = (IntPtr) 0;
        IntPtr pModel = (IntPtr) 0;        
        int errorcode = lindo.LSERR_NO_ERROR;
		int nStatus = 0, customLogLevel=1;
		int i = 0;

		// number of constraints
		int m = 0;

		// number of variables
		int n = 0;

		// number of continous variables
		int ncont = 0;

		// objective value
        double obj = 0 ;        

        StringBuilder LicenseKey = new StringBuilder(lindo.LS_MAX_ERROR_MESSAGE_LENGTH);

        DisplayVersionInfo();
        if (args.Length != 1) 
		{
            Console.WriteLine("Usage: ex_iis filename");
            return;
        }


        // Read license key from file
        string LicenseFile = System.Environment.GetEnvironmentVariable("LINDOAPI_HOME") + "\\license\\lndapi140.lic";
        
        errorcode = lindo.LSloadLicenseString(LicenseFile, LicenseKey);
        if (errorcode > 0) { CheckErr(env, errorcode); return;}        

        // Create a LINDO environment.
        env = lindo.LScreateEnv(ref errorcode, LicenseKey.ToString());
        if (errorcode > 0) {
            Console.WriteLine("Unable to create environment.");
            return;
        }

        // Create a model in the environment.
        pModel = lindo.LScreateModel(env, ref errorcode);
        CheckErr(env, errorcode);

        // Optionally, declare user's callback data 
        // (total number of calls to the callback function)
        CallbackData cbData;
        cbData.calls = 0;
        IntPtr myData = Marshal.AllocHGlobal(Marshal.SizeOf(cbData));
        Marshal.StructureToPtr(cbData, myData, true);

        // Declare callback function
        lindo.typCallback cbFunc = new lindo.typCallback(ex_iis.MyCallbackCount);
        lindo.typModelLOG cbLog = new lindo.typModelLOG(ex_iis.MyCallbackLog);

        // Set callback or log function
        if (customLogLevel==3)
        {
            // set callback function and pass user data
            errorcode = lindo.LSsetCallback(pModel, cbFunc, myData);
        }
        else if (customLogLevel==1)
        {   // set log function and pass user data
            errorcode = lindo.LSsetModelLogfunc(pModel, cbLog, myData);
        }
        CheckErr(env, errorcode);

      
        // Read input model
        Console.WriteLine();        
        Console.WriteLine("Reading {0}.",args[0]);
        errorcode = lindo.LSreadMPSFile( pModel, args[0], lindo.LS_UNFORMATTED_MPS);
        if (errorcode>0) {
        	errorcode = lindo.LSreadLINDOFile( pModel, args[0]);       
        }
        CheckErr(env, errorcode);

		// get model stats
		errorcode = lindo.LSgetInfo(pModel, lindo.LS_IINFO_NUM_VARS, ref n);
		errorcode = lindo.LSgetInfo(pModel, lindo.LS_IINFO_NUM_CONS, ref m);
		errorcode = lindo.LSgetInfo(pModel, lindo.LS_IINFO_NUM_CONT, ref ncont);

		ex_iis.n_vars = n;
		ex_iis.n_cons = m;

		double [] x = new double[n];
		double [] y = new double[m];

		// Perform the optimization.
		Console.WriteLine("Solving...");
        if (n==ncont) 
            errorcode = lindo.LSoptimize(pModel, lindo.LS_METHOD_FREE, ref nStatus);
        else
            errorcode = lindo.LSsolveMIP(pModel, ref nStatus);

		CheckErr(env, errorcode);
        if (nStatus == lindo.LS_STATUS_BASIC_OPTIMAL || nStatus == lindo.LS_STATUS_OPTIMAL)
        {
            Console.WriteLine("\tThe model is solved to optimality.\n");
            // Retrieve the solution and print
            if (n == ncont)
            {
                errorcode = lindo.LSgetInfo(pModel, lindo.LS_DINFO_POBJ, ref obj);
                CheckErr(env, errorcode);
                errorcode = lindo.LSgetPrimalSolution(pModel, x);
                CheckErr(env, errorcode);
                errorcode = lindo.LSgetDualSolution(pModel, y);
                CheckErr(env, errorcode);
            }
            else
            {                    
                errorcode = lindo.LSgetInfo(pModel, lindo.LS_DINFO_MIP_OBJ, ref obj);
                CheckErr(env, errorcode);
                errorcode = lindo.LSgetMIPPrimalSolution(pModel, x);
                CheckErr(env, errorcode);
                errorcode = lindo.LSgetMIPDualSolution(pModel, y);
                CheckErr(env, errorcode);
            }
            Console.WriteLine();
            Console.WriteLine("Objective value = {0} ", obj);         

        }
       /***************************************************************
        * Step 6: Debug the model if unbounded or infeasible
        ***************************************************************/
        else if (nStatus == lindo.LS_STATUS_UNBOUNDED)
        {
            Console.WriteLine("\nThe model is unbounded.. Analyzing...\n");
            int nLevel = lindo.LS_NECESSARY_COLS + lindo.LS_SUFFICIENT_COLS;

            /*** Step 6.1: Find IIS ***/
            errorcode = lindo.LSfindIUS(pModel, nLevel);
            CheckErr(env, errorcode);
        }
        else if (nStatus == lindo.LS_STATUS_INFEASIBLE)
        {
            Console.WriteLine("\nThe model is infeasible.. Analyzing...\n");
            int nLevel = lindo.LS_NECESSARY_ROWS + lindo.LS_SUFFICIENT_ROWS;
            int nSuf_r = 0, nIIS_r = 0, nSuf_c = 0, nIIS_c = 0;
            int[] aiRows = new int[m];
            int[] aiCols = new int[n];
            int[] anBnds = new int[n];

            /*** Step 6.1: Find IIS ***/
            errorcode = lindo.LSfindIIS(pModel, nLevel);
            CheckErr(env, errorcode);

            errorcode = lindo.LSgetIIS(pModel, ref nSuf_r, ref nIIS_r, aiRows, ref nSuf_c, ref nIIS_c, aiCols, anBnds);
            CheckErr(env, errorcode);
            StringBuilder varname = new StringBuilder(256);
            String bndtype;

            if (customLogLevel == 2)
            {
                Console.WriteLine("\n\t ***  LSfindIIS Summary ***\n");
                Console.WriteLine("\t Number of Sufficient Rows = {0}", nSuf_r);
                Console.WriteLine("\t Number of Sufficient Cols = {0}", nSuf_c);
                Console.WriteLine("\t Number of Necessary  Rows = {0}", nIIS_r - nSuf_r);
                Console.WriteLine("\t Number of Necessary  Cols = {0}", nIIS_c - nSuf_c);
                Console.WriteLine("\n");

                /*** Step 6.2: Display row index sets ***/
                Console.WriteLine("\n IIS Rows");
                for (int j = 0; j < nIIS_r; j++)
                {
                    errorcode = lindo.LSgetConstraintNamei(pModel, aiRows[j], varname);
                    CheckErr(env, errorcode);

                    if (j < nSuf_r)
                        Console.WriteLine("{0}] ({1}) is in the sufficient set.\n", j, varname);
                    else
                        Console.WriteLine("{0}] ({1}) is in the necessary set.\n", j, varname);
                }//for-j

                /*** Step 6.3: Display column index sets ***/
                Console.WriteLine("\n IIS Column Bounds\n");
                for (int j = 0; j < nIIS_c; j++)
                {
                    if (anBnds[j] < 0)
                        bndtype = "Lower";
                    else
                        bndtype = "Upper";

                    errorcode = lindo.LSgetVariableNamej(pModel, aiCols[j], varname);
                    CheckErr(env, errorcode);
                    if (j < nSuf_r)
                        Console.WriteLine("{0}] %s bound of ({1}) is in the sufficient set.\n", j, bndtype, varname);
                    else
                        Console.WriteLine("{0}] %s bound of ({1}) is in the necessary set.\n", j, bndtype, varname);
                }//for-j
            }
        }//if
        
        Console.WriteLine();
	    cbData = (CallbackData) Marshal.PtrToStructure(myData,typeof(CallbackData));
        Console.WriteLine("Total callbacks : {0}" ,cbData.calls );
               
		// free user data in global heap
        Marshal.FreeHGlobal(myData);           

        // Delete the LINDO environment.
        lindo.LSdeleteEnv(ref env);

		Console.WriteLine("Press Enter..."); 
		Console.ReadLine();

     }
}
