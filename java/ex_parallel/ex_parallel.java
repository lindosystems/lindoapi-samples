/*
###################################################################
#                       LINDO-API
#                    Sample Programs
#                  Copyright (c) 2001-2002
#
#         LINDO Systems, Inc.           312.988.7422
#         1415 North Dayton St.         info@lindo.com
#         Chicago, IL 60622             http://www.lindo.com
###################################################################

  File   : ex_parallel.java

  Purpose: Read a model from an MPS (or LINDO, LP, LINDO) file and optimize
           with a parallel algorithm.
*/

import com.lindo.*;

class ex_userdata
{
    int numCback;
    int numBestK;
}



public class ex_parallel extends Lindo
{
    private static int nErrorCode[] = new int[1];
    private static StringBuffer cErrorMessage = new StringBuffer(512);
    private static StringBuffer cLicenseKey = new StringBuffer(512);
    public static int numThreads = 3;
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

    /* A callback function for continuous models */
    private static int jCallback(Object pMod, int nLoc, Object pls)
    {
        int ncalls = 0;
        ex_parallel nls=null;
        ex_userdata _mydata;
        try
        {
            nls = (ex_parallel) pls;
            _mydata = (ex_userdata) nls.mydata;

            _mydata.numCback++;
            ncalls = _mydata.numCback;
        } catch (Exception e)
        {
            System.out.println(e.toString());
        }


        LSgetCallbackInfo(pMod,0,LS_DINFO_PINFEAS,pinf);
        LSgetCallbackInfo(pMod,0,LS_DINFO_POBJ,pobj);
        LSgetCallbackInfo(pMod,0,LS_IINFO_SIM_ITER,iter);
        System.out.printf("\n@callback calls=%3d, iter=%8d, pobj = %+.6e,  pinf = %.6e",
            ncalls,iter[0],pobj[0],pinf[0]);
        return 0;
    }

    /* A callback function for continuous models */
    private static int jStrategy(Object pMod, int nRunId, Object pls)
    {
        int ncalls = 0;
        ex_parallel nls=null;
        ex_userdata _mydata;
        try
        {
            nls = (ex_parallel) pls;
            _mydata = (ex_userdata) nls.mydata;

            _mydata.numCback++;
            ncalls = _mydata.numCback;
        }
        catch (Exception e)
        {
            System.out.println(e.toString());
        }

        // define a strategy for each run
        if (nRunId==0) {
            LSsetModelIntParameter(pMod,LS_IPARAM_MIP_STRONGBRANCHLEVEL,0);
        } else if (nRunId==1) {
            LSsetModelIntParameter(pMod,LS_IPARAM_MIP_STRONGBRANCHLEVEL,2);
        } else if (nRunId==numThreads-1) {
            LSsetModelIntParameter(pMod,LS_IPARAM_MIP_STRONGBRANCHLEVEL,15);
        } else {
            LSsetModelIntParameter(pMod,LS_IPARAM_MIP_STRONGBRANCHLEVEL,5);
        }
		System.out.printf("\nSet parameters for concurrent run #%d",nRunId);


        return 0;
    }

    /* A callback function for integer models */
    private static int jMIPCallback(Object pMod, int nLoc, Object pls)
    {
        int ncalls = 0;
        ex_parallel nls=null;
        ex_userdata _mydata;
        try
        {
            nls = (ex_parallel) pls;
            _mydata = (ex_userdata) nls.mydata;

            _mydata.numCback++;
            ncalls = _mydata.numCback;
        } catch (Exception e)
        {
            System.out.println(e.toString());
        }

        // return if not calling from MIP optimizer
        if (nLoc != LSLOC_MIP) return 0;

        LSgetCallbackInfo(pMod,0,LS_DINFO_MIP_BESTBOUND,bestbound);
        LSgetCallbackInfo(pMod,0,LS_DINFO_MIP_OBJ,pobj);
        LSgetCallbackInfo(pMod,0,LS_IINFO_MIP_SIM_ITER,iter);
        System.out.printf("\n@callback calls=%3d, iter=%8d, pobj = %+.6e,  bestbnd = %.6e",
            ncalls,iter[0],pobj[0],bestbound[0]);
        return 0;
    }

    /* A callback function to be called at every new integer solution (for integer models) */
    private static int jNewMIPCallback(Object pMod, Object pls, double obj, double x[])
    {
        int ncalls = 0;
        ex_parallel nls=null;
        ex_userdata _mydata;
        try
        {
            nls = (ex_parallel) pls;
            _mydata = (ex_userdata) nls.mydata;

            _mydata.numCback++;
            ncalls = _mydata.numCback;
        } catch (Exception e)
        {
            System.out.println(e.toString());
        }

        LSgetCallbackInfo(pMod,0,LS_DINFO_MIP_BESTBOUND,bestbound);
        LSgetCallbackInfo(pMod,0,LS_DINFO_MIP_OBJ,pobj);
        LSgetCallbackInfo(pMod,0,LS_IINFO_MIP_SIM_ITER,iter);
        System.out.printf("\n@new integer solution=%3d, iter=%8d, pobj = %+.6e,  bestbnd = %.6e (*)",
            ncalls,iter[0],pobj[0],bestbound[0]);
        return 0;
    }

    /* A callback function to be called at next incumbent solution (for global solver) */
    private static int jNextMIPCallback(Object pMod, Object pls, double obj, double x[])
    {
        int ncalls = 0;
        int nbestk = 0;
        ex_parallel nls=null;
        ex_userdata _mydata;
        try
        {
            nls = (ex_parallel) pls;
            _mydata = (ex_userdata) nls.mydata;

            _mydata.numCback++;
            _mydata.numBestK++;
            nbestk=_mydata.numBestK;
        } catch (Exception e)
        {
            System.out.println(e.toString());
        }

        LSgetCallbackInfo(pMod,0,LS_DINFO_MIP_BESTBOUND,bestbound);
        LSgetCallbackInfo(pMod,0,LS_DINFO_MIP_OBJ,pobj);
        LSgetCallbackInfo(pMod,0,LS_IINFO_MIP_SIM_ITER,iter);
        System.out.printf("\n@next best integer solution=%3d, iter=%8d, pobj = %+.6e,  bestbnd = %.6e (*)",
            nbestk,iter[0],pobj[0],bestbound[0]);
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
        int verbose = 2;

        // construct a new instance of the class
        ex_parallel ls = new ex_parallel();

        // Read license file and create a LINDO environment.
        nErrorCode[0] = ls.LSloadLicenseString("../../license/lndapi160.lic",cLicenseKey);
        APIErrorCheck(pEnv);

        APIVERSION();
        pEnv = LScreateEnv(nErrorCode, cLicenseKey.toString());
        APIErrorCheck(pEnv);

        pModel = ls.LScreateModel(pEnv,nErrorCode);
        APIErrorCheck(pEnv);

        if (args.length == 0)
        {
            System.out.println("\n\nusage: ex_parallel [filename]\n");
            nErrorCode[0] = ls.LSdeleteEnv( pEnv);
            return;
        }

        System.out.printf("\nReading %s as MPS file.",args[0]);
        nErrorCode[0] = ls.LSreadMPSFile( pModel, args[0],0);
        if (nErrorCode[0] != LSERR_NO_ERROR)
        {
            System.out.printf("..Failed\nReading %s as LINDO formatted file. ",args[0]);
            nErrorCode[0] = ls.LSreadLINDOFile( pModel, args[0]);
            if (nErrorCode[0] != LSERR_NO_ERROR)
            {
                System.out.printf("..Failed\nReading %s as MPI file.",args[0]);
                nErrorCode[0] = ls.LSreadMPIFile( pModel, args[0]);
                APIErrorCheck(pEnv);
            }
        }

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

        mydata.numCback = 0; // total number of callbacks
        mydata.numBestK = 0; // number of best-k integer solutions (for integer models)

        System.out.println("\n\nModel statistics");
        System.out.println("\t constraints        = "+m[0]);
        System.out.println("\t     + equalities   = "+neq);
        System.out.println("\t     + inequalities = "+(nle+nge));
        System.out.println("\n");
        System.out.println("\t variables          = "+n[0]);
        System.out.println("\t     + binary int   = "+nbin[0]);
        System.out.println("\t     + general int  = "+ngin[0]);
        System.out.println("\t     + continuous   = "+ncont[0]);

        //nErrorCode[0] = ls.LSsetModelIntParameter(pModel,LS_IPARAM_LP_PRELEVEL,0);
        //nErrorCode[0] = ls.LSsetModelIntParameter(pModel,LS_IPARAM_LP_PRINTLEVEL,verbose);
        APIErrorCheck(pEnv);

        if ((nbin[0]+ngin[0])==0)
        {
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
            System.out.println("\nOptimizing integer model\n");
            if (verbose>1)
                nErrorCode[0] = ls.LSsetModelLogfunc(pModel,"jLogback",ls);
            else if (verbose>0)
            {
                //nErrorCode[0] = ls.LSsetCallback(pModel,"jMIPCallback",ls);
                APIErrorCheck(pEnv);
                nErrorCode[0] = ls.LSsetMIPCallback(pModel,"jNewMIPCallback",ls);
                APIErrorCheck(pEnv);
            }
            APIErrorCheck(pEnv);

			// set number of threads
			LSsetModelIntParameter(pModel,LS_IPARAM_NUM_THREADS,numThreads);
			// enable concurrent solver
			LSsetModelIntParameter(pModel,LS_IPARAM_MULTITHREAD_MODE,LS_MTMODE_CC);

			// set concurrent strategy defining callbacks
			for (i = 0; i<numThreads; i++) {
				nErrorCode[0] = ls.LSsetMIPCCStrategy(pModel,"jStrategy",i,null,ls);
				APIErrorCheck(pEnv);
		    }

			// solve
            nErrorCode[0] = ls.LSsolveMIP( pModel, nStatus);
            APIErrorCheck(pEnv);

        }
        System.out.println("\nDone!");

        System.out.printf("Status = %d\n" , nStatus[0]);
        System.out.printf("Total callbacks = %d\n" , mydata.numCback);

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

        nErrorCode[0] = ls.LSdeleteModel( pModel);

        nErrorCode[0] = ls.LSdeleteEnv( pEnv);


    }/*main*/
}
