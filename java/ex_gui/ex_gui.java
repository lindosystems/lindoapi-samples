import java.awt.GridLayout;
import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.BufferedReader;
import java.io.FileReader;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

import com.lindo.*;

class ex_userdata
{
    int numCback;
    int numBestK;
	int neq,nle,nge;
	int m[] = new int[1];
	int n[] = new int[1];
	int nbin[] = new int[1];
	int ngin[] = new int[1];
	int ncont[] = new int[1];
	int nStatus[] = new int[1];
    StringBuffer csense = new StringBuffer();

    int iter[] = new int[1];
    int n_vars[] = new int[1];
    double pobj[] = new double[1];
    double bestbound[] = new double[1];
    double pinf[] = new double[1];

}

interface LindoInterface {
    public void doLindoStuff();
}

public class ex_gui extends JFrame implements LindoInterface {

    private static final long serialVersionUID = 1L;
	private static ex_gui ls;
    private static JTextField fileNameField;
	private static JTextField fileParamField;
    private static JTextField param1Field; //
    private static JTextField param2Field;
    private static JTextField param3Field;
    private static JTextField param4Field;
	private static JTextField param5Field;
	

    private JButton runButton;
    private JButton stopButton;
    private JButton quitButton;

    private static int nErrorCode[] = new int[1];
    private static StringBuffer cErrorMessage = new StringBuffer(512);
    private static StringBuffer cLicenseKey = new StringBuffer(512);

	private static int ParamAccess[] =  {
		Lindo.LS_IPARAM_NUM_THREADS,
		Lindo.LS_IPARAM_MULTITHREAD_MODE,
		Lindo.LS_DPARAM_SOLVER_FEASTOL,
		Lindo.LS_DPARAM_SOLVER_OPTTOL
		};

    private static  Object pEnv = null;
    private static  Object pModel = null;
	private static  int has_user_stop = 0;

    private static ex_userdata mydata = new ex_userdata();
	private static int verbose = 2;    // 0:none	1:Lindo generated logs		2: custom logs created in callbacks

	private Runnable cb = new Runnable() {
			public void run() {
				// Callback method to be called when the thread is finished
				runButton.setEnabled(true);
				stopButton.setEnabled(false);
				System.out.printf("\nFinished solving..");
				has_user_stop = 0;
			}
		};

	private void solveModel(String fileName) {
		int i;
		if (pModel != null) {
			System.out.println("\nDeleting previous pModel instance.");
			nErrorCode[0] = Lindo.LSdeleteModel( pModel);
		}

        pModel = Lindo.LScreateModel(pEnv,nErrorCode);
        APIErrorCheck(pEnv);

        System.out.printf("\nReading %s as MPS file.",fileName);
        nErrorCode[0] = Lindo.LSreadMPSFile( pModel, fileName,0);
        if (nErrorCode[0] != Lindo.LSERR_NO_ERROR)
        {
            System.out.printf("..Failed\nReading %s as LINDO formatted file. ",fileName);
            nErrorCode[0] = Lindo.LSreadLINDOFile( pModel, fileName);
            if (nErrorCode[0] != Lindo.LSERR_NO_ERROR)
            {
                System.out.printf("..Failed\nReading %s as MPI file.",fileName);
                nErrorCode[0] = Lindo.LSreadMPIFile( pModel, fileName);
                APIErrorCheck(pEnv);
            }
        }

		String paramFile = fileParamField.getText();
		if (paramFile.length()>0) {
			System.out.printf("\nReading %s as parameter file.",paramFile);
			nErrorCode[0] = Lindo.LSreadModelParameter( pModel, paramFile);
			if (nErrorCode[0]==0) {
				System.out.printf(" ok.");
			} else {
				System.out.printf(" failed, error:%d.",nErrorCode[0]);
			}
		}
			
        String param1 = param1Field.getText();
        String param2 = param2Field.getText();
        String param3 = param3Field.getText();
        String param4 = param4Field.getText();
		String param5 = param5Field.getText();
		Lindo.LSsetModelIntParameter(pModel,ParamAccess[0],Integer.parseInt(param1));
		Lindo.LSsetModelIntParameter(pModel,ParamAccess[1],Integer.parseInt(param2));
		Lindo.LSsetModelDouParameter(pModel,ParamAccess[2],Double.parseDouble(param3));
		Lindo.LSsetModelDouParameter(pModel,ParamAccess[3],Double.parseDouble(param4));
		verbose = Integer.parseInt(param5);
		
        nErrorCode[0] = Lindo.LSgetInfo(pModel, Lindo.LS_IINFO_NUM_VARS,mydata.n);
        APIErrorCheck(pEnv);

        nErrorCode[0] = Lindo.LSgetInfo(pModel, Lindo.LS_IINFO_NUM_CONS,mydata.m);
        APIErrorCheck(pEnv);

        nErrorCode[0] = Lindo.LSgetLPData(pModel, null,null,null,null,mydata.csense,
            null,null,null,null,null,null);
        APIErrorCheck(pEnv);

        mydata.neq=0;
        mydata.nle=0;
        mydata.nge=0;
        for (i=0;i<mydata.m[0];i++)
        {
            if (mydata.csense.charAt(i) == 'E')
                mydata.neq++;
            else if (mydata.csense.charAt(i) == 'L')
                mydata.nle++;
            else
                mydata.nge++;
        }

        nErrorCode[0] = Lindo.LSgetInfo(pModel,Lindo.LS_IINFO_NUM_CONT,mydata.ncont);
        APIErrorCheck(pEnv);

        nErrorCode[0] = Lindo.LSgetInfo(pModel,Lindo.LS_IINFO_NUM_BIN,mydata.nbin);
        APIErrorCheck(pEnv);

        nErrorCode[0] = Lindo.LSgetInfo(pModel,Lindo.LS_IINFO_NUM_INT,mydata.ngin);
        APIErrorCheck(pEnv);

        mydata.numCback = 0; // total number of callbacks
        mydata.numBestK = 0; // number of best-k integer solutions (for integer models)

        System.out.println("\n\nModel statistics");
        System.out.println("\t constraints        = "+mydata.m[0]);
        System.out.println("\t     + equalities   = "+mydata.neq);
        System.out.println("\t     + inequalities = "+(mydata.nle+mydata.nge));
        System.out.println("\n");
        System.out.println("\t variables          = "+mydata.n[0]);
        System.out.println("\t     + binary int   = "+mydata.nbin[0]);
        System.out.println("\t     + general int  = "+mydata.ngin[0]);
        System.out.println("\t     + continuous   = "+mydata.ncont[0]);

		if ((mydata.nbin[0]+mydata.ngin[0])==0) {
			System.out.println("\nOptimizing continuous model\n");
            nErrorCode[0] = Lindo.LSsetCallback(pModel,"jCallback",ls);
			APIErrorCheck(pEnv);

			if (verbose==1) // enable solver logs
                nErrorCode[0] = Lindo.LSsetModelLogfunc(pModel,"jLogback",ls);

			// solve
			nErrorCode[0] = Lindo.LSoptimize( pModel, 0, mydata.nStatus);

		} else {
            System.out.println("\nOptimizing integer model\n");
			//nErrorCode[0] = Lindo.LSsetCallback(pModel,"jMIPCallback",ls);
			nErrorCode[0] = Lindo.LSsetCallback(pModel,"jCallback",ls);
			APIErrorCheck(pEnv);
			nErrorCode[0] = Lindo.LSsetMIPCallback(pModel,"jNewMIPCallback",ls);
			APIErrorCheck(pEnv);

			if (verbose==1) // enable solver logs
                nErrorCode[0] = Lindo.LSsetModelLogfunc(pModel,"jLogback",ls);

			// solve
            nErrorCode[0] = Lindo.LSsolveMIP( pModel, mydata.nStatus);
		}
		APIErrorCheck(pEnv);
	}


    private static void jLogback(Object pMod, String szMessage, Object pls)
    {
        System.out.print(szMessage);
    }

	private static double MIP_RELGAP(double bestbound,double MIPobjval) {
		return Math.abs(bestbound-MIPobjval)/Math.max(Math.max(Math.abs(MIPobjval),Math.abs(bestbound)),1.0);
	}

    /* A callback function for continuous models */
    private static int jCallback(Object pMod, int nLoc, Object pls)
    {
		int retval = 0;
        int ncalls = 0;
        ex_gui nls=null;
        ex_userdata _mydata;
        try
        {
            nls = (ex_gui) pls;
            _mydata = (ex_userdata) nls.mydata;

            _mydata.numCback++;
            ncalls = _mydata.numCback;
        } catch (Exception e)
        {
            System.out.println(e.toString());
        }

		if ((mydata.nbin[0]+mydata.ngin[0])==0) {
			Lindo.LSgetCallbackInfo(pMod,0,Lindo.LS_DINFO_PINFEAS,mydata.pinf);
			Lindo.LSgetCallbackInfo(pMod,0,Lindo.LS_DINFO_POBJ,mydata.pobj);
			Lindo.LSgetCallbackInfo(pMod,0,Lindo.LS_IINFO_SIM_ITER,mydata.iter);
			if (verbose==2) {
				System.out.printf("\n@callback calls=%3d, iter=%8d, pobj = %+.6e,  pinf = %.6e",
					ncalls,mydata.iter[0],mydata.pobj[0],mydata.pinf[0]);
			}
		} else {
			retval = jMIPCallback(pModel,nLoc,pls);
		}

        return retval+has_user_stop; // quick hack to nonzero retval
    }

    /* A callback function for integer models */
    private static int jMIPCallback(Object pMod, int nLoc, Object pls)
    {	// /tmp/prob/miplib3/bm23.mps
        int ncalls = 0;
		int retval = 0;
        ex_gui nls=null;
        ex_userdata _mydata;
        try
        {
            nls = (ex_gui) pls;
            _mydata = (ex_userdata) nls.mydata;

            _mydata.numCback++;
            ncalls = _mydata.numCback;
        } catch (Exception e)
        {
            System.out.println(e.toString());
        }

        // return if not calling from MIP optimizer
        if (nLoc != Lindo.LSLOC_MIP) return 0;

        Lindo.LSgetCallbackInfo(pMod,0,Lindo.LS_DINFO_MIP_BESTBOUND,mydata.bestbound);
        Lindo.LSgetCallbackInfo(pMod,0,Lindo.LS_DINFO_MIP_OBJ,mydata.pobj);
        Lindo.LSgetCallbackInfo(pMod,0,Lindo.LS_IINFO_MIP_SIM_ITER,mydata.iter);
		double gap = MIP_RELGAP(mydata.bestbound[0],mydata.pobj[0]);
		if (verbose==2) {
			System.out.printf("\n@callback calls=%3d, iter=%8d, pobj = %+.6e,  bestbnd = %.6e, gap: %g",
				ncalls,mydata.iter[0],mydata.pobj[0],mydata.bestbound[0],gap);
		}
        return retval+has_user_stop; // quick hack to nonzero retval
    }

    /* A callback function to be called at every new integer solution (for integer models) */
    private static int jNewMIPCallback(Object pMod, Object pls, double obj, double x[])
    {
		int retval = 0;
        int ncalls = 0;
        ex_gui nls=null;
        ex_userdata _mydata;
        try
        {
            nls = (ex_gui) pls;
            _mydata = (ex_userdata) nls.mydata;

            _mydata.numCback++;
            ncalls = _mydata.numCback;
        } catch (Exception e)
        {
            System.out.println(e.toString());
        }

        Lindo.LSgetCallbackInfo(pMod,0,Lindo.LS_DINFO_MIP_BESTBOUND,mydata.bestbound);
        Lindo.LSgetCallbackInfo(pMod,0,Lindo.LS_DINFO_MIP_OBJ,mydata.pobj);
        Lindo.LSgetCallbackInfo(pMod,0,Lindo.LS_IINFO_MIP_SIM_ITER,mydata.iter);
		if (verbose==2) {
			System.out.printf("\n@new integer solution=%3d, iter=%8d, pobj = %+.6e,  bestbnd = %.6e (*)",
				ncalls,mydata.iter[0],mydata.pobj[0],mydata.bestbound[0]);
		}
        return retval+has_user_stop; // quick hack to nonzero retval
    }

    /* A callback function to be called at next incumbent solution (for global solver) */
    private static int jNextMIPCallback(Object pMod, Object pls, double obj, double x[])
    {
		int retval = 0;
        int ncalls = 0;
        int nbestk = 0;
        ex_gui nls=null;
        ex_userdata _mydata;
        try
        {
            nls = (ex_gui) pls;
            _mydata = (ex_userdata) nls.mydata;

            _mydata.numCback++;
            _mydata.numBestK++;
            nbestk=_mydata.numBestK;
        } catch (Exception e)
        {
            System.out.println(e.toString());
        }

        Lindo.LSgetCallbackInfo(pMod,0,Lindo.LS_DINFO_MIP_BESTBOUND,mydata.bestbound);
        Lindo.LSgetCallbackInfo(pMod,0,Lindo.LS_DINFO_MIP_OBJ,mydata.pobj);
        Lindo.LSgetCallbackInfo(pMod,0,Lindo.LS_IINFO_MIP_SIM_ITER,mydata.iter);
		if (verbose==2) {
			System.out.printf("\n@next best integer solution=%3d, iter=%8d, pobj = %+.6e,  bestbnd = %.6e (*)",
				nbestk,mydata.iter[0],mydata.pobj[0],mydata.bestbound[0]);
		}
        return retval+has_user_stop; // quick hack to nonzero retval
    }

    // Generalized error Reporting function
    private static void APIErrorCheck(Object pEnv )
    {
        if(0 != nErrorCode[0])
        {
			cErrorMessage.setLength(0);
            Lindo.LSgetErrorMessage(pEnv, nErrorCode[0], cErrorMessage);
            System.out.println("\nError " + nErrorCode[0] + ": " + cErrorMessage);
            System.out.println();
            //System.exit(1);
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
        Lindo.LSgetVersionInfo(szVersion, szBuild);
        System.out.println("\nLINDO API Version "+szVersion.toString() + " built on " + szBuild.toString());
        System.out.println();
    }

    public ex_gui() {
        super("File Reader");

        JPanel contentPane = new JPanel();
        contentPane.setLayout(new GridLayout(0, 2));

        JLabel fileNameLabel = new JLabel("model file:");
        fileNameField = new JTextField(20);

        JLabel fileParamLabel = new JLabel("param file:");
        fileParamField = new JTextField(20);
		
        JLabel param1Label = new JLabel("NUM_THREADS:");
        param1Field = new JTextField(10);

        JLabel param2Label = new JLabel("MULTITHREAD_MODE:");
        param2Field = new JTextField(10);

        JLabel param3Label = new JLabel("SOLVER_FEASTOL:");
        param3Field = new JTextField(10);

        JLabel param4Label = new JLabel("SOLVER_OPTTOL:");
        param4Field = new JTextField(10);

        JLabel param5Label = new JLabel("verbose:");
        param5Field = new JTextField(10);
		
        contentPane.add(fileNameLabel);
        contentPane.add(fileNameField);
        contentPane.add(fileParamLabel);
        contentPane.add(fileParamField);		
        contentPane.add(param1Label);
        contentPane.add(param1Field);
        contentPane.add(param2Label);
        contentPane.add(param2Field);
        contentPane.add(param3Label);
        contentPane.add(param3Field);
        contentPane.add(param4Label);
        contentPane.add(param4Field);
        contentPane.add(param5Label);
        contentPane.add(param5Field);
		
        runButton = new JButton("Run");
        stopButton = new JButton("Stop");
        stopButton.setEnabled(false);
        quitButton = new JButton("Quit");

        JPanel bottomPanel = new JPanel();
        bottomPanel.add(runButton);
        bottomPanel.add(stopButton);
        bottomPanel.add(quitButton);
		
        runButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                startSolving(cb);
            }
        });

        stopButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                stopSolving();
            }
        });

        quitButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
				System.out.println("\nDeleting pEnv instance.");
				nErrorCode[0] = Lindo.LSdeleteEnv( pEnv);
                System.exit(0);
            }
        });

		//setContentPane(contentPane);
		
        add(contentPane, BorderLayout.CENTER);
        add(bottomPanel, BorderLayout.SOUTH);
		        
		
        pack();
        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        setVisible(true);
    }

	private void startSolving(final Runnable callback) {
		String fileName = fileNameField.getText();
        if (fileName.isEmpty()) {
			//fileName = "/tmp/prob/miplib3/pk1.mps";
            System.out.printf("\nEnter a file name..");
            return;
        }

		runButton.setEnabled(false);
		stopButton.setEnabled(true);

		final String fileName_ = fileName; //nasty

		// Create a new thread to run the method
		Thread thread = new Thread(new Runnable() {
			public void run() {
				try {
					solveModel(fileName_);
				} catch (Exception e) {
					e.printStackTrace();
				} finally {
					// Call the callback method when the thread is finished
					callback.run();
				}
			}
		});
		thread.start(); // Start the thread

		// Update the UI
		runButton.setEnabled(false);
		stopButton.setEnabled(true);
	}

    private void stopSolving() {
        has_user_stop = 1;
    }

    public void doLindoStuff() {
        // implement the methods from the LindoInterface here
    }

    private static void loadDefaultParamValues() {
        
		fileNameField.setText("/tmp/prob/miplib3/bm23.mps");
		param1Field.setText(getEnvIntParam(ParamAccess[0]));
		param2Field.setText(getEnvIntParam(ParamAccess[1]));
		param3Field.setText(getEnvDouParam(ParamAccess[2]));
		param4Field.setText(getEnvDouParam(ParamAccess[3]));
		param5Field.setText(Integer.toString(verbose));
		System.out.printf("\n");
    }

	private static String getEnvIntParam(Integer id) {
		int ival[] = new int[1];
		int	ierr;
		String sval = "N/A";
		if (pEnv!=null) {
			ierr = Lindo.LSgetEnvIntParameter(pEnv,id,ival);
			if (ierr==0) {
				sval = Integer.toString(ival[0]);
				System.out.printf("\nLoaded default value for int param(%d): %s ",id,sval);
			} else {
				System.out.printf("\nWarning: Failed to retrieve env param (%d), error:%d",id,ierr);
			}
		} else {
			System.out.printf("\nError: pEnv is nil, query (%d)",id);
		}
		return sval;
	}

	private static String getEnvDouParam(Integer id) {
		double dval[] = new double[1];
		int	ierr;
		String sval = "N/A";
		if (pEnv!=null) {
			ierr = Lindo.LSgetEnvDouParameter(pEnv,id,dval);
			if (ierr==0) {
				sval = Double.toString(dval[0]);
				System.out.printf("\nLoaded default value for dou param(%d): %s ",id,sval);
			} else {
				System.out.printf("\nWarning: Failed to retrieve env param (%d), error:%d",id,ierr);
			}
		} else {
			System.out.printf("\nError: pEnv is nil, query (%d)",id);
		}
		return sval;
	}

    public static void main(String[] args) {

		ls = new ex_gui();

        // Read license file and create a LINDO environment.
        nErrorCode[0] = Lindo.LSloadLicenseString("../../license/lndapi140.lic",cLicenseKey);
        APIErrorCheck(pEnv);

        APIVERSION();
        pEnv = Lindo.LScreateEnv(nErrorCode, cLicenseKey.toString());
        APIErrorCheck(pEnv);

		loadDefaultParamValues();

    }
}
