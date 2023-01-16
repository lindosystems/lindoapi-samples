///////////////////////////////////////////////////////////////////////////////////////////////////
////
////    LINDO API Version 6.1
////    Copyright (c; 2000-2010
////
////    LINDO Systems, Inc.            312.988.7422
////    1415 North Dayton St.          info@lindo.com
////    Chicago, IL 60622              http://www.lindo.com
////
////    @ex_mt1.cs 
////
////    last updated: 06-26-2010
////
///////////////////////////////////////////////////////////////////////////////////////////////////
#define USE_GCH

using System;
using System.Text;
using System.IO;
using System.Threading;

using System.Runtime.InteropServices;



public struct CallbackData
{
    public int calls;
}



public class ex_mt1
{
    static private string fileName;
    /// <summary>
    /// A callback function to display progress for continuous models
    /// </summary>
    /// <param name="pMod"></param>
    /// <param name="nLoc"></param>
    /// <param name="myData"></param>
    /// <returns></returns>
    public static int MyCallbackCount(IntPtr pMod, int nLoc, IntPtr myData)
    {
        int iter = 0;
        double pinf = 0, pobj = 0;

        // copy the user data in the unmanaged code into a local structure
        CallbackData cb = (CallbackData)Marshal.PtrToStructure(myData, typeof(CallbackData));

        // increment the number of calls to the callback function
        cb.calls++;

        lindo.LSgetCallbackInfo(pMod, 0, lindo.LS_DINFO_PINFEAS, ref pinf);
        lindo.LSgetCallbackInfo(pMod, 0, lindo.LS_DINFO_POBJ, ref pobj);
        lindo.LSgetCallbackInfo(pMod, 0, lindo.LS_IINFO_SIM_ITER, ref iter);
        Console.WriteLine("{0,10}: iter={1,10}, obj={2,14:e6}, pinf={3,10:e3}, ncalls={4,5}", Thread.CurrentThread.Name, iter, pobj, pinf, cb.calls);

        // copy the user data in the local structure back to the unmanaged code
        Marshal.StructureToPtr(cb, myData, false);

        return 0;
    }

    /// <summary>
    /// A generic callback function to display callbacks
    /// </summary>
    /// <param name="model"></param>
    /// <param name="loc"></param>
    /// <param name="nvCbData"></param>
    /// <returns></returns>
    public static int MyCallback(IntPtr model, int loc, IntPtr nvCbData)
    {
        int it = 0, nErr = lindo.LSERR_NO_ERROR;
        double ob = 0.0;
        // get iterations
        nErr = lindo.LSgetCallbackInfo(model, loc, lindo.LS_IINFO_SIM_ITER, ref it);
        // get primal objective
        nErr = lindo.LSgetCallbackInfo(model, loc, lindo.LS_DINFO_POBJ, ref ob);

        Console.WriteLine("{0,10}: iter={1,10}, obj={2,14:e6}", Thread.CurrentThread.Name, it, ob);

        return 0;
    }

    /// <summary>
    /// A callback function to display progress for integer models
    /// </summary>
    /// <param name="model"></param>
    /// <param name="myData"></param>
    /// <param name="ob"></param>
    /// <param name="adX"></param>
    /// <returns></returns>
    public static int MyMipCallback(IntPtr model, IntPtr myData, double ob, IntPtr adX)
    {
        int it = 0, n = 0, m = 0;
        int nErr = lindo.LSERR_NO_ERROR;

        nErr = lindo.LSgetInfo(model, lindo.LS_IINFO_NUM_VARS, ref n);
        nErr = lindo.LSgetInfo(model, lindo.LS_IINFO_NUM_CONS, ref m);


        double[] x = new double[n];

        Marshal.Copy(adX, x, 0, n);

        // copy the user data in the unmanaged code into a local structure
        CallbackData cb = (CallbackData)Marshal.PtrToStructure(myData, typeof(CallbackData));

        // increment the number of calls to the callback function
        cb.calls++;

        // get iterations 
        nErr = lindo.LSgetMIPCallbackInfo(model, lindo.LS_IINFO_MIP_SIM_ITER, ref it);

        Console.WriteLine("{0,10}: iter={1,10}, obj={2,14:e6}", Thread.CurrentThread.Name, it, ob);

        // copy the user data in the local structure back to the unmanaged code
        Marshal.StructureToPtr(cb, myData, false);

        return 0;
    }

    /// <summary>
    /// A basic routine to handle errors returned by LINDO API
    /// </summary>
    /// <param name="env"></param>
    /// <param name="errorcode"></param>
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

    /// <summary>
    /// Main thread
    /// </summary>
    /// <param name="args"></param>
    public static void Main(string[] args)
    {
        int nThreads;
        ParameterizedThreadStart job=null;
        if (args.Length != 2)
        {
            Console.WriteLine("Usage: ex_mt1 [filename] [threads]");
            return;
        }
        
        fileName = args[0];
        nThreads = Int32.Parse(args[1]);
        if (nThreads <= 0) nThreads = 1;

        Thread[] thread = new Thread[nThreads];
        job = new ParameterizedThreadStart(ThreadJob);

        for (int i = 0; i < nThreads; i++)
        {
            Console.WriteLine("Running thread: {0}", i);
            thread[i] = new Thread(job);
            thread[i].Name = String.Format("Thread_{0}", i);
            thread[i].Start(thread[i].Name);
        }
        for (int i = 0; i < nThreads; i++)
        {
            thread[i].Join();
            Console.WriteLine("Joined thread " + i + "...");
        }
        Console.WriteLine("Done...");
    }



    public static void ThreadJob(object dummy)
    {
        IntPtr env = (IntPtr)0;
        IntPtr prob = (IntPtr)0;
        int errorcode = lindo.LSERR_NO_ERROR;
        int nStatus = 0;
        int i = 0;
        lindo.typCallback cbFunc = null;
        lindo.typMIPCallback mcbFunc = null;
#if USE_GCH                    
        GCHandle gch;
#endif

        // number of constraints
        int m = 0;

        // number of variables
        int n = 0;

        // number of continous variables
        int ncont = 0;

        // objective value
        double obj = 0;

        StringBuilder LicenseKey = new StringBuilder(lindo.LS_MAX_ERROR_MESSAGE_LENGTH);
        StringBuilder LibVersion = new StringBuilder(lindo.LS_MAX_ERROR_MESSAGE_LENGTH);
        StringBuilder LibBuilded = new StringBuilder(lindo.LS_MAX_ERROR_MESSAGE_LENGTH);


        // Read license key from file
        errorcode = lindo.LSloadLicenseString("../../../../../../license/lndapi140.lic", LicenseKey);
        if (errorcode > 0)
        {
            //CheckErr(env, errorcode);
            errorcode = lindo.LSloadLicenseString("../../../../license/lndapi140.lic", LicenseKey);
            if (errorcode > 0) { CheckErr(env, errorcode); return; }
        }

        // Create a LINDO environment.
        env = lindo.LScreateEnv(ref errorcode, LicenseKey.ToString());
        if (errorcode > 0)
        {
            Console.WriteLine("Unable to create environment.");
            return;
        }

        // Create a model in the environment.
        prob = lindo.LScreateModel(env, ref errorcode);
        CheckErr(env, errorcode);

        // Display API version        
        Console.WriteLine();
        lindo.LSgetVersionInfo(LibVersion, LibBuilded);
        Console.WriteLine("Lindo API version {0} {1}", LibVersion, LibBuilded);

        // Read input model
        Console.WriteLine();
        Console.WriteLine("Reading {0}.", fileName);
        errorcode = lindo.LSreadMPSFile(prob, fileName, lindo.LS_UNFORMATTED_MPS);
        CheckErr(env, errorcode);
        if (errorcode > 0)
        {
            errorcode = lindo.LSreadLINDOFile(prob, fileName);
            CheckErr(env, errorcode);
            if (errorcode > 0)
            {
                errorcode = lindo.LSreadMPIFile(prob, fileName);
                CheckErr(env, errorcode);
            }
        }


        // get model stats
        errorcode = lindo.LSgetInfo(prob, lindo.LS_IINFO_NUM_VARS, ref n);
        errorcode = lindo.LSgetInfo(prob, lindo.LS_IINFO_NUM_CONS, ref m);
        errorcode = lindo.LSgetInfo(prob, lindo.LS_IINFO_NUM_CONT, ref ncont);

        double[] x = new double[n];
        double[] y = new double[m];

        // Optionally, declare user's callback data (total number of calls to the callback function)
        CallbackData cbData;
        cbData.calls = 0;        
        IntPtr myData = Marshal.AllocHGlobal(Marshal.SizeOf(cbData));
        Marshal.StructureToPtr(cbData, myData, true);

        // if all variables are continous call LP optimizer, ...
        if (ncont == n)
        {
            // Declare callback function
            cbFunc = new lindo.typCallback(ex_mt1.MyCallbackCount);
#if USE_GCH                        
            gch = GCHandle.Alloc(cbFunc);
#endif
            // set callback function and pass user data
            errorcode = lindo.LSsetCallback(prob, cbFunc, myData);
            CheckErr(env, errorcode);

            // Perform the optimization.
            Console.WriteLine("Solving...");
            errorcode = lindo.LSoptimize(prob, lindo.LS_METHOD_FREE, ref nStatus);
            CheckErr(env, errorcode);

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
            mcbFunc = new lindo.typMIPCallback(ex_mt1.MyMipCallback);
#if USE_GCH            
            gch = GCHandle.Alloc(mcbFunc);
#endif
            // set callback function and pass user data
            errorcode = lindo.LSsetMIPCallback(prob, mcbFunc, myData);
            CheckErr(env, errorcode);

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
        cbData = (CallbackData)Marshal.PtrToStructure(myData, typeof(CallbackData));
        Console.WriteLine("Total callbacks : {0}", cbData.calls);

        Console.WriteLine();
        Console.WriteLine("Objective value = {0} ", obj);

        StringBuilder StrName = new StringBuilder(256); ;

        if (false)
        {
            Console.WriteLine();
            Console.WriteLine("Primal Solution");
            for (i = 0; i < n; i++)
            {
                errorcode = lindo.LSgetVariableNamej(prob, i, StrName);
                Console.WriteLine("{0} = {1} ", StrName.ToString(), x[i]);
            }

            Console.WriteLine();
            Console.WriteLine("Dual Solution");
            for (i = 0; i < m; i++)
            {
                errorcode = lindo.LSgetConstraintNamei(prob, i, StrName);
                Console.WriteLine("{0} = {1} ", StrName.ToString(), y[i]);
            }
        }

        // free user data in global heap
        Marshal.FreeHGlobal(myData);
#if USE_GCH                    
        gch.Free();
#endif
        // Delete the LINDO environment.
        lindo.LSdeleteEnv(ref env);

        return;

    }
}
