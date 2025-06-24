/* ex_tuner.java

    Test built-in distribution functions and the callback interface
    for user-defined distributions. The use of different sampling
    modes are also shown.
*/


import com.lindo.*;

class ex_userdata
{
    int counter;
}


public class ex_tuner extends Lindo
{
    private static int nErrorCode[] = new int[1];
    private static StringBuffer cErrorMessage = new StringBuffer(512);
    private static StringBuffer cLicenseKey = new StringBuffer(512);
    private static Object pSamp = null;
    /* declare an instance of the LINDO environment object */
    private static Object /*long*/ pEnv = null;


    // Generalized error Reporting function
    private static void APIErrorCheck(Object /*long*/ pEnv )
    {
        if(0 != nErrorCode[0])
        {
            try
            {
                LSgetErrorMessage(pEnv, nErrorCode[0], cErrorMessage);
                System.out.println("\nError " + nErrorCode[0] + ": " + cErrorMessage);
            }
            catch (Exception e)
            {
                System.out.println("\nError " + nErrorCode[0] + ": " + " Unknown error");
            }
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
        ex_tuner ls = new ex_tuner();

        /* >>> Step 1 <<< Read license file and create a LINDO environment. */
        nErrorCode[0] = ls.LSloadLicenseString("../../license/lndapi160.lic",cLicenseKey);
        APIErrorCheck(pEnv);

        APIVERSION();
        pEnv = ls.LScreateEnv(nErrorCode, cLicenseKey.toString());
        APIErrorCheck(pEnv);

        String dataPath;

        dataPath = System.getenv("LINDOAPI_HOME");
        if (dataPath==null) {
			dataPath = "../../samples/data";
		} else {
			dataPath += "/samples/data";
		}


	   /* Tuner instances */

	   nErrorCode[0] = ls.LSaddTunerInstance(pEnv, dataPath+"/bm23.mps.gz"); APIErrorCheck(pEnv);


	   nErrorCode[0] = ls.LSaddTunerInstance(pEnv, dataPath+"/p0033.mps.gz"); APIErrorCheck(pEnv);


	   nErrorCode[0] = ls.LSaddTunerInstance(pEnv, dataPath+"/p0201.mps.gz"); APIErrorCheck(pEnv);


	   nErrorCode[0] = ls.LSaddTunerInstance(pEnv, dataPath+"/p0282.mps.gz"); APIErrorCheck(pEnv);

	   /* Tuner options */
	   nErrorCode[0] = ls.LSaddTunerOption(pEnv,"max_parsets",6); APIErrorCheck(pEnv);
	   nErrorCode[0] = ls.LSaddTunerOption(pEnv,"time_limit",10); APIErrorCheck(pEnv);
	   nErrorCode[0] = ls.LSaddTunerOption(pEnv,"ntrials",2); APIErrorCheck(pEnv);
	   nErrorCode[0] = ls.LSaddTunerOption(pEnv,"nthreads",1); APIErrorCheck(pEnv);
	   nErrorCode[0] = ls.LSaddTunerOption(pEnv,"seed",1032); APIErrorCheck(pEnv);
	   nErrorCode[0] = ls.LSaddTunerOption(pEnv,"criterion",1); APIErrorCheck(pEnv);

	   /* Tuner dynamic parameters */
	   nErrorCode[0] = ls.LSaddTunerZDynamic(pEnv,LS_IPARAM_LP_SCALE); APIErrorCheck(pEnv);
	   nErrorCode[0] = ls.LSaddTunerZDynamic(pEnv,LS_IPARAM_MIP_PRELEVEL); APIErrorCheck(pEnv);
	   nErrorCode[0] = ls.LSaddTunerZDynamic(pEnv,LS_IPARAM_MIP_BRANCHDIR); APIErrorCheck(pEnv);
	   nErrorCode[0] = ls.LSaddTunerZDynamic(pEnv,LS_IPARAM_MIP_BRANCHRULE); APIErrorCheck(pEnv);
	   nErrorCode[0] = ls.LSaddTunerZDynamic(pEnv,LS_IPARAM_MIP_FP_MODE); APIErrorCheck(pEnv);
	   nErrorCode[0] = ls.LSaddTunerZDynamic(pEnv,LS_DPARAM_SOLVER_FEASTOL); APIErrorCheck(pEnv);

	   /* Tuner static groups and parameters */
	   nErrorCode[0] = ls.LSaddTunerZStatic(pEnv,1,LS_IPARAM_MIP_NODESELRULE,4); APIErrorCheck(pEnv);
	   nErrorCode[0] = ls.LSaddTunerZStatic(pEnv,1,LS_DPARAM_MIP_RELINTTOL,0.0001); APIErrorCheck(pEnv);
	   nErrorCode[0] = ls.LSaddTunerZStatic(pEnv,1,LS_DPARAM_SOLVER_OPTTOL,1e-006); APIErrorCheck(pEnv);
	   nErrorCode[0] = ls.LSaddTunerZStatic(pEnv,2,LS_IPARAM_MIP_NODESELRULE,1); APIErrorCheck(pEnv);
	   nErrorCode[0] = ls.LSaddTunerZStatic(pEnv,2,LS_DPARAM_MIP_RELINTTOL,0.001); APIErrorCheck(pEnv);
	   nErrorCode[0] = ls.LSaddTunerZStatic(pEnv,2,LS_DPARAM_SOLVER_OPTTOL,1e-005); APIErrorCheck(pEnv);
	   nErrorCode[0] = ls.LSaddTunerZStatic(pEnv,3,LS_IPARAM_MIP_NODESELRULE,3); APIErrorCheck(pEnv);
	   nErrorCode[0] = ls.LSaddTunerZStatic(pEnv,3,LS_DPARAM_MIP_RELINTTOL,1e-005); APIErrorCheck(pEnv);
	   nErrorCode[0] = ls.LSaddTunerZStatic(pEnv,3,LS_DPARAM_SOLVER_OPTTOL,0.0001); APIErrorCheck(pEnv);

	   //nErrorCode[0] = ls.LSsetTunerStrOption(pEnv,"xdll","*"); APIErrorCheck(pEnv);


	   if (0>1) {
		nErrorCode[0] = ls.LSprintTuner(pEnv);
		APIErrorCheck(pEnv);
	   }
	   StringBuffer szJson = new StringBuffer();
	   nErrorCode[0] = ls.LSgetTunerConfigString(pEnv,szJson);
	   APIErrorCheck(pEnv);
	   if (nErrorCode[0]==0) {
	      nErrorCode[0] = ls.LSwriteTunerConfigString(pEnv,szJson.toString(),"lindo_tuner.json~");
	      APIErrorCheck(pEnv);
   		}

	   if (2>1) {
		 nErrorCode[0] = ls.LSrunTuner(pEnv);
		 APIErrorCheck(pEnv);

		 nErrorCode[0] = ls.LSdisplayTunerResults(pEnv);
		 APIErrorCheck(pEnv);

		 int mCriterion = -1; //selected criterion
		 int jInstance  = -1; //avg instance
		 nErrorCode[0] = ls.LSwriteTunerParameters(pEnv,"lindo_tuned.par",jInstance,mCriterion);
		 APIErrorCheck(pEnv);
	   }

	   nErrorCode[0] = ls.LSclearTuner(pEnv);
	   APIErrorCheck(pEnv);

        /* >>> Step 6 <<< Delete the LINDO environment */
        nErrorCode[0] = ls.LSdeleteEnv( pEnv);

    }
}
