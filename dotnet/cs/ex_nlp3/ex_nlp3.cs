//'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
//'
//'      Set up and solve the following nonlinear model using LINDO API's
//'      multi-start nonlinear optimizer.
//'
//'      minimize  f(x,y) =  3*(1-x).^2.*exp(-(x.^2) - (y+1).^2) ...
//'                       - 10*(x/5 - x.^3 - y.^5).*exp(-x.^2-y.^2) ...
//'                       - 1/3*exp(-(x+1).^2 - y.^2);
//'      subject to
//'                       x^2 + y   <=  6;
//'                       x   + y^2 <=  6;
//'
//'      1. Create an environment and a model space
//'      2. Specify and load the LP portion of the model
//'      3. Specify and load the NLP portion of the model
//'      4. Specify the function evaluator function
//'      5. Specify solver options and optimize
//'      6. Terminate
//'
//'    @ex_nlp3.cs  
//'
//'    last updated: 04-03-2007
//'
//''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

using System;
using System.Text;
using System.IO;

using System.Runtime.InteropServices;
using System.Runtime.Remoting;
using System.Windows.Forms;

namespace ex_nlp3
{
        

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


    public class nlp
    {
        public nlp()
        {
        }
    
        private static double  g1( double X, double Y)
        {
            return  Math.Exp( -Math.Pow(X  ,2) - Math.Pow(Y+1,2) );
        }

        private static double  g2( double X, double Y)
        {
            return  Math.Exp( -Math.Pow(X  ,2) - Math.Pow(Y,2) );
        }

        private static double  g3( double X, double Y)
        {
            return  Math.Exp( -Math.Pow(X+1  ,2) - Math.Pow(Y,2) );
        }

        private static double  f1( double X, double Y)
        {
            return   Math.Pow(1-X  ,2) ;
        }

        private static double  f2( double X, double Y)
        {
            return  ( X/5 - Math.Pow(X  ,3) - Math.Pow(Y,5) );
        }
    
        // callback function to compute functional values
        //
        private static int Funcalc8 (IntPtr pModel, IntPtr pUserData,
            int nRow  , IntPtr  pdX,
            int nJDiff, double  dXJBase,
            ref double pdFuncVal, IntPtr  pReserved)
        {
            double val=0.0;
            int    nerr=0;
            double [] x = new double[2];

            Marshal.Copy(pdX,x,0,2);

            double X= x[0];
            double Y= x[1];
        
            /* compute objective's functional value*/
            if (nRow==-1)
                val = 3*f1(X,Y)*g1(X,Y) - 10*f2(X,Y)*g2(X,Y) - g3(X,Y)/3;
                /* compute constaint 0's functional value */
            else if (nRow==0)
                val = X*X + Y - 6.0;
                /* compute constaint 1's functional value */
            else if (nRow==1)
                val = X + Y*Y - 6.0;

            pdFuncVal=val;

            return nerr;
        } /*Funcalc8*/

        // generic callback function
        //
        public static int MyCallback(IntPtr pMod, int nLoc,  IntPtr  myData)  
        {
            int iter=0;
            double pinf=0, pobj=0;   
    
            CallbackData cb = new CallbackData();      
                
            Marshal.PtrToStructure (myData, cb);
    
            lindo.LSgetCallbackInfo(pMod,nLoc,lindo.LS_IINFO_NLP_ITER, ref iter);
            lindo.LSgetCallbackInfo(pMod,0,lindo.LS_DINFO_POBJ,ref pobj);
            Console.WriteLine("callback @iter={0}, obj={1}, pinf={2}, mydata={3}" ,iter,pobj,pinf,cb.count );
            cb.count++;
        
            Marshal.StructureToPtr (cb,myData,true);       
                           
            return 0;
        }

        // error checking 
        //
        public static void APIErrorCheck(IntPtr pEnv, int nErr)
        {
            if (nErr > 0)
            {
                StringBuilder cMessage = new StringBuilder(lindo.LS_MAX_ERROR_MESSAGE_LENGTH);
                lindo.LSgetErrorMessage(pEnv, nErr, cMessage);
                MessageBox.Show(cMessage.ToString());
            }
        }

        // load form and start
        public static void start (ref double obj, ref int iter)
        {
                
            int nErrorCode = lindo.LSERR_NO_ERROR;

            /* Number of constraints */
            int nCons = 2;

            /* Number of variables */
            int nVars = 2;

            /* The number of nonzeros in the constraint matrix */
            int nNZ = 4;

            IntPtr pEnv = (IntPtr) 0;

            IntPtr pModel = (IntPtr) 0;        
            
            /* initialize the counter that counts the number of times 
            the callback function is called */
            CallbackData cbData = new CallbackData();            
		    IntPtr myData = Marshal.AllocHGlobal(Marshal.SizeOf(cbData));
		    Marshal.StructureToPtr(cbData, myData, true);
            StringBuilder LicenseKey = new StringBuilder(lindo.LS_MAX_ERROR_MESSAGE_LENGTH);

            /* >>> Step 1 <<< Create a LINDO environment. */
            // Read license key from file
            string LicenseFile = System.Environment.GetEnvironmentVariable("LINDOAPI_HOME") + "\\license\\lndapi160.lic";

            nErrorCode = lindo.LSloadLicenseString(LicenseFile, LicenseKey);
            if (nErrorCode > 0) {        
               APIErrorCheck(pEnv, nErrorCode);
               return;
            }            
            pEnv = lindo.LScreateEnv(ref nErrorCode, LicenseKey.ToString());            
            APIErrorCheck(pEnv,nErrorCode);

            /* >>> Step 2 <<< Create a model in the environment. */
            pModel = lindo.LScreateModel ( pEnv, ref nErrorCode);
            APIErrorCheck(pEnv,nErrorCode);

            /* >>> Step 3 <<< Specify the model.
            /* The direction of optimization */
            int nDir = lindo.LS_MIN;

            /* The objective's constant term */
            double dObjConst = 0.0;

            /* The coefficients of the objective function */
            double[] adC = new double[] { 0, 0};

            /* The right-hand sides of the constraints */
            double[] adB = new double[] { 0, 0};

            /* The indices of the first nonzero in each column */
            int[] anBegCol = new int[]{ 0, 2, 4};

            /* The length of each column.  Since we aren't leaving
            any blanks in our matrix, we can set this to NULL */
            int[] anLenCol = new int[] { 2 , 2};

            /* The nonzero coefficients */
            double[] adA = new double[] { 0, 1, 1, 0};

            /* The row indices of the nonzero coefficients */
            int[] anRowX = new int[]{ 0, 1, 0, 1};

            /* Simple upper and lower bounds on the variables. */                       
            double[] pdLower = new double[] { -3, -3};
            double[] pdUpper = new double[] { +3, +3};

            /* The constraint types */
            string acConTypes = "LL";

            /* The variable types */
            StringBuilder acVarTypes = new StringBuilder("CC");
    
            /* Constraint and variable names */
            string [] varnames = new string[] {"Variable1","Variable2"};
            string [] connames = new string[] {"Constraint1","Constraint2"};

            /* We have now assembled a full description of the model.
            We pass this information to LSloadLPData with the
            following call. */
            nErrorCode = lindo.LSloadLPData( pModel, nCons, nVars, nDir,
                dObjConst, adC, adB, acConTypes, nNZ, anBegCol,
                anLenCol, adA, anRowX, pdLower, pdUpper);
            APIErrorCheck(pEnv,nErrorCode);

            /* load variable type */
            nErrorCode = lindo.LSloadVarType(pModel,acVarTypes.ToString());
            APIErrorCheck(pEnv,nErrorCode);
    
            /* The number of nonlinear variables in each column */
            anLenCol[0]=1; anLenCol[1]=1;

            /* The indices of the first nonlinear variable in each column */
            anBegCol[0]=0; anBegCol[1]=1; anBegCol[2]=2;

            /* The indices of nonlinear constraints */
            anRowX[0]=0;
            anRowX[1]=1;

            /* The indices of variables that are nonlinear in the objective*/
            int[] Nobjndx = new int[]{ 0, 1};

            /* Number nonlinear variables in cost. */
            int Nnlobj = 2;

            /* Load the nonlinear structure */
            nErrorCode = lindo.LSloadNLPData(pModel,anBegCol,anLenCol,null,
                anRowX,Nnlobj,Nobjndx,null);
            APIErrorCheck(pEnv,nErrorCode);

            nErrorCode = lindo.LSloadNameData(pModel, "MyTitle","MyObj",null,null,
                null,connames,varnames,null);
            APIErrorCheck(pEnv,nErrorCode);

            /* Set up callback functions*/
            lindo.typCallback cb = new lindo.typCallback(nlp.MyCallback);
            nErrorCode = lindo.LSsetCallback(pModel,cb, cbData);
            APIErrorCheck(pEnv,nErrorCode);

            nErrorCode = lindo.LSsetModelIntParameter(pModel,lindo.LS_IPARAM_NLP_PRINTLEVEL,1);
            APIErrorCheck(pEnv,nErrorCode);

            /* Install the routine that will calculate the function values. */
            lindo.typFuncalc fc = new lindo.typFuncalc(nlp.Funcalc8);
            nErrorCode = lindo.LSsetFuncalc(pModel,fc,null);
            APIErrorCheck(pEnv,nErrorCode);

            nErrorCode = lindo.LSsetModelIntParameter(pModel,lindo.LS_IPARAM_NLP_SOLVER,
                lindo.LS_NMETHOD_MSW_GRG);
            APIErrorCheck(pEnv,nErrorCode);

            /* >>> Step 4 <<< Perform the optimization */

            int nStatus = lindo.LS_STATUS_UNKNOWN;
            nErrorCode = lindo.LSsolveMIP( pModel,ref nStatus);
            //      nErrorCode = lindo.LSoptimize( pModel, lindo.LS_METHOD_FREE, ref nStatus);
            APIErrorCheck(pEnv,nErrorCode);

            /* >>> Step 5 <<< Retrieve the solution */
            int i;
            double[] adX = new double[2];
            double[] dObj = new double [1];

            /* Get the value of the objective */
            nErrorCode = lindo.LSgetInfo(pModel, lindo.LS_DINFO_MIP_OBJ, ref dObj[0]) ;
            nErrorCode = lindo.LSgetInfo(pModel, lindo.LS_IINFO_MIP_NLP_ITER, ref iter) ;
            //          nErrorCode = lindo.LSgetInfo(pModel, lindo.LS_DINFO_POBJ, ref dObj[0]) ;
            APIErrorCheck(pEnv,nErrorCode);
            obj = dObj[0];

            Console.WriteLine("Objective Value = " + dObj[0] + "\n");

            /* Get the variable values */
            nErrorCode = lindo.LSgetMIPPrimalSolution(pModel, adX);
            //          nErrorCode = lindo.LSgetPrimalSolution(pModel, adX);
            APIErrorCheck(pEnv,nErrorCode);

            Console.WriteLine("Primal values");
            for (i = 0; i < nVars; i++)
            {
                Console.WriteLine("\tx["+i+"] = "+adX[i]);
            }

            Marshal.FreeHGlobal(myData); 
            
            /* >>> Step 6 <<< Delete the LINDO environment */
            nErrorCode = lindo.LSdeleteModel( ref pModel);

            nErrorCode = lindo.LSdeleteEnv( ref pEnv);
                           
        }
    }
}
