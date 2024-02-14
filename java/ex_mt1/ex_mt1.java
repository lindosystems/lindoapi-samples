import com.lindo.*;

class ex_userdata
{
	int threadId;
	int modelType;
}

class NewThread extends Thread {
	String inputFileName;
	public Thread t;
	private int nErrorCode[] = new int[1];
	private StringBuffer cErrorMessage = new StringBuffer(Lindo.LS_MAX_ERROR_MESSAGE_LENGTH);
	private Object pModel = null;
	private Object _pEnv;
	private ex_userdata mydata = new ex_userdata();
	private	int iter[] = new int[1];
	private	int niter[] = new int[1];
	private	int n_vars[] = new int[1];
	private	double pobj[] = new double[1];
	private	double bestbound[] = new double[1];
	private	double pinf[] = new double[1];
	private	int nStatus[] = new int[1];


	private static int jCallback(Object pMod, int nLoc, Object pls)
	{
		int modelType=-1;
		int threadId = 0;
		NewThread nls=null;
		ex_userdata _mydata;
		int iter[] = new int[1];
		int niter[] = new int[1];
		int n_vars[] = new int[1];
		double pobj[] = new double[1];
		double bestbound[] = new double[1];
		double pinf[] = new double[1];
		int nStatus[] = new int[1];

		try
		{
			nls = (NewThread) pls;
			_mydata = (ex_userdata) nls.mydata;
			threadId = _mydata.threadId;
			modelType = _mydata.modelType;

		} catch (Exception e)
		{
			System.out.println(e.toString());
			return 0;
		}

		// return if not calling from MIP or GOP optimizer
		if (nLoc != Lindo.LSLOC_MIP && nLoc != Lindo.LSLOC_GOP) return 0;

		if (modelType==Lindo.LS_LP ||
			modelType==Lindo.LS_QP ||
			modelType==Lindo.LS_NLP ||
			modelType==Lindo.LS_SOCP)
		{
			Lindo.LSgetCallbackInfo(pMod,0,Lindo.LS_DINFO_GOP_BESTBOUND,bestbound);
			Lindo.LSgetCallbackInfo(pMod,0,Lindo.LS_DINFO_GOP_OBJ,pobj);
			Lindo.LSgetCallbackInfo(pMod,0,Lindo.LS_IINFO_GOP_SIM_ITER,iter);
			Lindo.LSgetCallbackInfo(pMod,0,Lindo.LS_IINFO_GOP_NLP_ITER,niter);
		}
		else
		{
			Lindo.LSgetCallbackInfo(pMod,0,Lindo.LS_DINFO_MIP_BESTBOUND,bestbound);
			Lindo.LSgetCallbackInfo(pMod,0,Lindo.LS_DINFO_MIP_OBJ,pobj);
			Lindo.LSgetCallbackInfo(pMod,0,Lindo.LS_IINFO_MIP_SIM_ITER,iter);
			Lindo.LSgetCallbackInfo(pMod,0,Lindo.LS_IINFO_MIP_NLP_ITER,niter);
		}

		try
		{
			if (iter[0] % 10 == 0)
			{
				System.out.printf("Thread%03d: Obj:%13.5e, Bound:%13.5e, Gap:%.3e, Iters:%6d\n",
					threadId,pobj[0],bestbound[0],Math.abs(pobj[0]-bestbound[0]),iter[0]+niter[0]);

			}
		} catch (Exception e)
		{
			System.out.println(e.toString());
		}

		return 0;
	}

	public NewThread(Object pEnv, String threadname, int ThreadId) {
		inputFileName=threadname;
		_pEnv = pEnv;
		t=new Thread(this, inputFileName);
		System.out.println("New Thread: (" +ThreadId+") " + t );
		mydata.threadId=ThreadId;
		t.start();
	}

	// Generalized error Reporting function
	private  void ReturnOnError(Object _pEnv )
	{
		if(0 != nErrorCode[0])
		{
			Lindo.LSgetErrorMessage(_pEnv, nErrorCode[0], cErrorMessage);
			System.out.println(inputFileName+": Error " + nErrorCode[0] + ": " + cErrorMessage);
			Lindo.LSdeleteModel(pModel);
			System.exit(1);
		}
	}

    private static int indexOf(String str, String searchStr, int startPos) {
      if (str == null || searchStr == null) {
          return -1;
      }

      if (searchStr.length() == 0 && startPos >= str.length()) {
          return -1;
      }
      return str.indexOf(searchStr, startPos);
    }

	public void run() {
		int modelType[] = new int[1];

		pModel = Lindo.LScreateModel ( _pEnv, nErrorCode);
		ReturnOnError(_pEnv);

		//Lindo.LSsetModelIntParameter(_pEnv, Lindo.LS_IPARAM_NUM_THREADS, 4);
		if (0>1) {
			nErrorCode[0] = Lindo.LSsetModelIntParameter(pModel, 1059, 14);
			nErrorCode[0] = Lindo.LSsetModelIntParameter(pModel,Lindo.LS_IPARAM_SPLEX_USE_EXTERNAL,14);
			ReturnOnError(_pEnv);
		}


		// choose reader based on extension
		{
			if (inputFileName.lastIndexOf(".mps")>0)
			{
				System.out.println(t + " reading in MPS format");
				nErrorCode[0] = Lindo.LSreadMPSFile(pModel,inputFileName,Lindo.LS_UNFORMATTED_MPS);
			}
			else if (inputFileName.lastIndexOf(".ltx")>0)
			{
				System.out.println(t + " reading in LINDO (LTX) format");
				nErrorCode[0] = Lindo.LSreadLINDOFile( pModel, inputFileName);
			}
			else if (inputFileName.lastIndexOf(".mpi")>0)
			{
				System.out.println(t + " reading in MPI format");
				nErrorCode[0] = Lindo.LSreadMPIFile( pModel, inputFileName);
			}
			else
			{
				nErrorCode[0] = Lindo.LSERR_ERROR_IN_INPUT;
			}
			ReturnOnError(_pEnv);
		}


		nErrorCode[0] = Lindo.LSgetInfo(pModel,Lindo.LS_IINFO_MODEL_TYPE,modelType);
		mydata.modelType=modelType[0];

		nErrorCode[0] = Lindo.LSsetCallback(pModel,"jCallback",this);
		ReturnOnError(_pEnv);

		if (modelType[0]==Lindo.LS_LP ||
			modelType[0]==Lindo.LS_QP ||
			modelType[0]==Lindo.LS_NLP ||
			modelType[0]==Lindo.LS_SOCP)
		{
			nErrorCode[0] = Lindo.LSsolveGOP( pModel, nStatus);
			ReturnOnError(_pEnv);
			nErrorCode[0] = Lindo.LSgetInfo(pModel,Lindo.LS_DINFO_GOP_OBJ,pobj);
			nErrorCode[0] = Lindo.LSgetInfo(pModel,Lindo.LS_DINFO_GOP_BESTBOUND,bestbound);
			nErrorCode[0] = Lindo.LSgetInfo(pModel,Lindo.LS_IINFO_GOP_SIM_ITER,iter);
			nErrorCode[0] = Lindo.LSgetInfo(pModel,Lindo.LS_IINFO_GOP_NLP_ITER,niter);
			ReturnOnError(_pEnv);
		}
		else
		{
			nErrorCode[0] = Lindo.LSsolveMIP( pModel, nStatus);
			ReturnOnError(_pEnv);
			nErrorCode[0] = Lindo.LSgetInfo(pModel,Lindo.LS_DINFO_MIP_OBJ,pobj);
			nErrorCode[0] = Lindo.LSgetInfo(pModel,Lindo.LS_DINFO_MIP_BESTBOUND,bestbound);
			nErrorCode[0] = Lindo.LSgetInfo(pModel,Lindo.LS_IINFO_MIP_SIM_ITER,iter);
			nErrorCode[0] = Lindo.LSgetInfo(pModel,Lindo.LS_IINFO_MIP_NLP_ITER,niter);
			ReturnOnError(_pEnv);
		}

		System.out.printf("Thread%03d: Obj:%13.5f, Bound:%13.5f, Gap:%.3e, Iters:%6d, Status:%4d\n",
			mydata.threadId,pobj[0],bestbound[0],Math.abs(pobj[0]-bestbound[0]),iter[0]+niter[0],nStatus[0]);

		nErrorCode[0] = Lindo.LSdeleteModel( pModel);
		ReturnOnError(_pEnv);
	}
}

class ex_mt1 {

	private static int nErrorCode[] = new int[1];
	private static StringBuffer cErrorMessage = new StringBuffer(Lindo.LS_MAX_ERROR_MESSAGE_LENGTH);
	private static StringBuffer cLicenseKey = new StringBuffer(Lindo.LS_MAX_ERROR_MESSAGE_LENGTH);

	static
	{
		// The runtime system executes a class's static
		// initializer when it loads the class.
		System.loadLibrary("lindojni");
	}

	// Generalized error Reporting function
	private static void APIErrorCheck(Object pEnv)
	{
		if(0 != nErrorCode[0])
		{
			Lindo.LSgetErrorMessage(pEnv, nErrorCode[0], cErrorMessage);
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
		Lindo.LSgetVersionInfo(szVersion, szBuild);
		System.out.println("\nLINDO API Version "+szVersion.toString() + " built on " + szBuild.toString());
		System.out.println();
	}

	public static void main(String args[])
	{
		Object pEnv = null;
		int maxThreads=10;

		NewThread t[] = new NewThread[maxThreads];

		/* >>> Step 1 <<< Read license file and create a LINDO environment. */
		nErrorCode[0] = Lindo.LSloadLicenseString("../../license/lndapi150.lic",cLicenseKey);

		APIVERSION();
		pEnv= Lindo.LScreateEnv(nErrorCode, cLicenseKey.toString());
		APIErrorCheck(pEnv);

        if (0>1) {
			nErrorCode[0] = Lindo.LSsetXSolverLibrary(pEnv,14,"liblindohighs.dll");
			APIErrorCheck(pEnv);
        }

		/* callback at every iteration */
		//Lindo.LSsetEnvDouParameter(pEnv, Lindo.LS_DPARAM_CALLBACKFREQ, 0.5);

		if (1==1)
		{
			t[0] = new NewThread(pEnv,"../data/bm23.ltx",0);
			t[1] = new NewThread(pEnv,"../data/bm23.mps",1);
			t[2] = new NewThread(pEnv,"../data/testgop.mpi",2);
			t[3] = new NewThread(pEnv,"../data/testgop.mpi",3);
			t[4] = new NewThread(pEnv,"../data/testmip.mps",4);
		}
		else
		{
			t[0] = new NewThread(pEnv,"../data/bm23.ltx",0);
			t[1] = new NewThread(pEnv,"../data/bm23.mps",1);
			t[2] = new NewThread(pEnv,"../data/bm23.mps",2);
			t[3] = new NewThread(pEnv,"../data/bm23.mps",3);
			t[4] = new NewThread(pEnv,"../data/bm23.mps",4);
		}

		try {
			/* join threads to finalize */
			for (int i=0; i<maxThreads; i++) {
				if (t[i]!=null) t[i].t.join();
			}
		}
		catch (InterruptedException e) {
			System.out.println("Main Thread Interrupted.");
		}
		System.out.println("main Thread Exiting.");

		if (pEnv!= null)
			nErrorCode[0] = Lindo.LSdeleteEnv( pEnv);

	}

}
