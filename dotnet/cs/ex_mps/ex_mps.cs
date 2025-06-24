//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////
////    LINDO API Version 5.0
////    Copyright (c; 2000-2007
////
////    LINDO Systems, Inc.            312.988.7422
////    1415 North Dayton St.          info@lindo.com
////    Chicago, IL 60622              http://www.lindo.com
////
////    @ex_mps.cs 
////
////    last updated: 04-03-2007
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



public class ex_mps
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

	public static int MyMipCallback(IntPtr model, IntPtr myData, double ob, IntPtr adX ) 
	{
		int it=0;	    
		int nErr=lindo.LSERR_NO_ERROR;
		double [] x = new double[ex_mps.n_vars];

		Marshal.Copy(adX,x,0,ex_mps.n_vars);

		// copy the user data in the unmanaged code into a local structure
		CallbackData cb = (CallbackData) Marshal.PtrToStructure (myData, typeof(CallbackData));               

		// increment the number of calls to the callback function
		cb.calls++;     			

		// get iterations 
		nErr = lindo.LSgetMIPCallbackInfo(model, lindo.LS_IINFO_MIP_SIM_ITER, ref it);

		Console.WriteLine("callback @iter={0}, obj={1} ",it,ob);

		// copy the user data in the local structure back to the unmanaged code
		Marshal.StructureToPtr (cb,myData,false);

		return 0;
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
        IntPtr prob = (IntPtr) 0;        
        int errorcode = lindo.LSERR_NO_ERROR;
		int nStatus = 0;
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
        StringBuilder LibVersion = new StringBuilder(lindo.LS_MAX_ERROR_MESSAGE_LENGTH);
        StringBuilder LibBuilded = new StringBuilder(lindo.LS_MAX_ERROR_MESSAGE_LENGTH);                
		
        
        if (args.Length != 1) 
		{
            Console.WriteLine("Usage: ex_mps filename");
            return;
        }


        // Read license key from file
        string LicenseFile = System.Environment.GetEnvironmentVariable("LINDOAPI_HOME") + "\\license\\lndapi160.lic";
        
        errorcode = lindo.LSloadLicenseString(LicenseFile, LicenseKey);
        if (errorcode > 0) {        
           CheckErr(env, errorcode);
           return;
        }

        // Create a LINDO environment.
        env = lindo.LScreateEnv(ref errorcode, LicenseKey.ToString());
        if (errorcode > 0) {
            Console.WriteLine("Unable to create environment.");
            return;
        }

        // Create a model in the environment.
        prob = lindo.LScreateModel(env, ref errorcode);
        CheckErr(env, errorcode);

        // Display API version        
        Console.WriteLine();
        lindo.LSgetVersionInfo(LibVersion,null);
        Console.WriteLine("Lindo API version {0}",LibVersion);
      
        // Read input model
        Console.WriteLine();        
        Console.WriteLine("Reading {0}.",args[0]);
        errorcode = lindo.LSreadMPSFile( prob, args[0], lindo.LS_UNFORMATTED_MPS);
        CheckErr(env, errorcode);

		// get model stats
		errorcode = lindo.LSgetInfo(prob, lindo.LS_IINFO_NUM_VARS, ref n);
		errorcode = lindo.LSgetInfo(prob, lindo.LS_IINFO_NUM_CONS, ref m);
		errorcode = lindo.LSgetInfo(prob, lindo.LS_IINFO_NUM_CONT, ref ncont);

		ex_mps.n_vars = n;
		ex_mps.n_cons = m;

		double [] x = new double[n];
		double [] y = new double[m];

		// Optionally, declare user's callback data 
		// (total number of calls to the callback function)
		CallbackData cbData;
		cbData.calls = 0;
		IntPtr myData = Marshal.AllocHGlobal(Marshal.SizeOf(cbData));
		Marshal.StructureToPtr(cbData, myData, true);

        lindo.typModelLOG cbLog = new lindo.typModelLOG(ex_mps.MyCallbackLog);

		// if all variables are continous call LP optimizer, ...
		if (ncont == n)
		{
			// Declare callback function
			lindo.typCallback cbFunc = new lindo.typCallback(ex_mps.MyCallbackCount);                        

			// set callback function and pass user data
            if (0>1) {
                errorcode = lindo.LSsetCallback(prob, cbFunc,  myData);
                CheckErr(env, errorcode);
            } else {
                errorcode = lindo.LSsetModelLogfunc(prob, cbLog, myData);
                CheckErr(env, errorcode);
            }

			// Perform the optimization.
			Console.WriteLine("Solving...");        
			errorcode = lindo.LSoptimize(prob, lindo.LS_METHOD_FREE, ref nStatus);
			CheckErr(env, errorcode);
			//errorcode = lindo.LSsolveGOP(prob, ref nStatus);

			// Retrieve the solution and print
			errorcode = lindo.LSgetInfo(prob, lindo.LS_DINFO_POBJ, ref obj);
			CheckErr(env, errorcode);
	                		
			errorcode = lindo.LSgetPrimalSolution(prob, x);
			CheckErr(env, errorcode);
	        
			errorcode = lindo.LSgetDualSolution(prob, y);
			CheckErr(env, errorcode); 
		}
		// otherwise call MIP optimizer
		else
		{
			// Declare callback function
			lindo.typMIPCallback mcbFunc = new lindo.typMIPCallback(ex_mps.MyMipCallback);

			// set callback function and pass user data
            if (0>1) {
                errorcode = lindo.LSsetMIPCallback(prob, mcbFunc,  myData);
                CheckErr(env, errorcode);
            } else {
                errorcode = lindo.LSsetModelLogfunc(prob, cbLog, myData);
                CheckErr(env, errorcode);                
            }

			// Perform the optimization.
			Console.WriteLine("Solving...");        
			errorcode = lindo.LSsolveMIP(prob, ref nStatus);
			CheckErr(env, errorcode);

			// Retrieve the solution and print
			errorcode = lindo.LSgetInfo(prob, lindo.LS_DINFO_MIP_OBJ, ref obj);
			CheckErr(env, errorcode);
	                			
			errorcode = lindo.LSgetMIPPrimalSolution(prob, x);
			CheckErr(env, errorcode);

			errorcode = lindo.LSgetMIPDualSolution(prob, y);
			CheckErr(env, errorcode); 	        
		}

        
        Console.WriteLine();
		cbData = (CallbackData) Marshal.PtrToStructure(myData,typeof(CallbackData));
        Console.WriteLine("Total callbacks : {0}" ,cbData.calls );

        Console.WriteLine();
        Console.WriteLine("Objective value = {0} ",obj);         
		Console.WriteLine();
		Console.WriteLine("Status: {0}",nStatus);
        
        StringBuilder StrName  = new StringBuilder(256);;
        
        Console.WriteLine();
        Console.WriteLine("Primal Solution");
        for (i=0; i<n; i++)
		{
           errorcode = lindo.LSgetVariableNamej(prob,i, StrName);
           Console.WriteLine("{0} = {1} ",StrName.ToString(),x[i]);
        }
        
        Console.WriteLine();
        Console.WriteLine("Dual Solution");
        for (i=0; i<m; i++)
		{
           errorcode = lindo.LSgetConstraintNamei(prob,i, StrName);
           Console.WriteLine("{0} = {1} ",StrName.ToString(),y[i]);
		}
        
		// free user data in global heap
        Marshal.FreeHGlobal(myData);           

        // Delete the LINDO environment.
        lindo.LSdeleteEnv(ref env);

		Console.WriteLine("Press Enter..."); 
		Console.ReadLine();

     }
}
