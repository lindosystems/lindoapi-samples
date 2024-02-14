using System;
using System.Text;
using System.IO;
using System.Runtime.InteropServices;
using System.Runtime.Remoting;
using System.Windows.Forms;
using System.Diagnostics;

public class ex_qp2
{
		public static void CheckErr(IntPtr pEnv, int nErr)
		{
			if (nErr > 0)
			{
				StringBuilder cMessage = new StringBuilder(lindo.LS_MAX_ERROR_MESSAGE_LENGTH);
				lindo.LSgetErrorMessage(pEnv, nErr, cMessage);
				MessageBox.Show(cMessage.ToString());
			}
		}

        public static void Main (string[] args)
        {
            IntPtr env = (IntPtr) 0;
            IntPtr pModel = (IntPtr) 0;
            int errorcode = lindo.LSERR_NO_ERROR;
		    int nStatus = 0;

            StringBuilder LicenseKey = new StringBuilder(lindo.LS_MAX_ERROR_MESSAGE_LENGTH);
            StringBuilder LibVersion = new StringBuilder(lindo.LS_MAX_ERROR_MESSAGE_LENGTH);
            StringBuilder LibBuilded = new StringBuilder(lindo.LS_MAX_ERROR_MESSAGE_LENGTH);

			errorcode = lindo.LSloadLicenseString("..\\..\\..\\..\\license\\lndapi150.lic", LicenseKey);        
			CheckErr(env,errorcode);			
			
            // Create a LINDO environment.
            env = lindo.LScreateEnv(ref errorcode, LicenseKey.ToString());
            if (errorcode > 0)
            {
                Console.WriteLine("Unable to create environment.");
                return;
            }

            // Create a model in the environment.
            pModel = lindo.LScreateModel(env, ref errorcode);
            CheckErr(env, errorcode);

            // Display API version        
            Console.WriteLine();
            lindo.LSgetVersionInfo(LibVersion,null);
            Console.WriteLine("Lindo API version {0}",LibVersion);
			
			
			// Read Linear Portion of the model
			errorcode = lindo.LSreadLINDOFile(pModel,"ex_lp2.ltx");
			CheckErr(env, errorcode);			
			
			// NOTE: Alternatively, it could be set up via LSloadLPData() call as in ex_lp1.cs
            
            //QP from page 381
            int nQCnnz = 9;
            //                                   1   2   3   4   5   6   7   8   9
            int[] paiQCrows  = new int[]      { -1, -1, -1, -1, -1,  0,  0,  1,  1};
            int[] paiQCcols1 = new int[]      {  0,  0,  1,  1,  2,  0,  1,  1,  2};
            int[] paiQCcols2 = new int[]      {  0,  2,  1,  2,  2,  0,  1,  1,  2};
            double[] padQCcoef = new double[] { -4,  2, -6,  5, -8,  2,  2,  2,  2};

            // >>>>>>>>>>>>>>I get error 2049 from this line<<<<<<<<<<<<<<<<<<<<<<
            errorcode = lindo.LSloadQCData(pModel, nQCnnz, paiQCrows, paiQCcols1, paiQCcols2, padQCcoef);
			CheckErr(env, errorcode);
			
			errorcode = lindo.LSwriteMPSFile(pModel,"ex_qp2.mps",0);
			CheckErr(env, errorcode);
			
			Console.WriteLine("Optimizing...");
            //errorcode = lindo.LSoptimize(pModel, lindo.LS_METHOD_FREE, ref nStatus);
			CheckErr(env, errorcode);			
            lindo.LSoptimizeQP(pModel, ref nStatus);
			Console.WriteLine("Finished.. Status: " + nStatus);

            // Retrieve the solution and print
            double obj = 0;
            errorcode = lindo.LSgetInfo(pModel, lindo.LS_DINFO_POBJ, ref obj);
            CheckErr(env, errorcode);
			Console.WriteLine("Objective: " + obj);

            // number of constraints
            int m = 4;

            // number of variables
            int n = 3;

            double[] x = new double[n];
            errorcode = lindo.LSgetPrimalSolution(pModel, x);
            CheckErr(env, errorcode);

            double[] y = new double[m];
            errorcode = lindo.LSgetMIPDualSolution(pModel, y);
            CheckErr(env, errorcode);

            // delete the environment
            errorcode = lindo.LSdeleteEnv(ref env);
/*
            //Verify the results
            double[] expected = new double[] {0, 0, 0};
            Assert.AreNotEqual<double>(expected[0], x[0]);
            Assert.AreNotEqual<double>(expected[1], x[1]);
            Assert.AreNotEqual<double>(expected[2], x[2]);
*/
        }
}