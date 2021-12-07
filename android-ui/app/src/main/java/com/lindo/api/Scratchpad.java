/*
	Lindo API for Android
	Lindo Systems, Inc.
	Copyright 2017

	Design by Joseph Rios, Littlepancake Software, info@littlepancake.com
	Copyright 2014,
*/
package com.lindo.api;

import com.lindo.*;

import java.io.File;
import java.io.FileWriter;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.text.DecimalFormat;
import java.io.BufferedReader;

import android.content.Context;
import android.support.v4.app.Fragment;
import android.util.Log;
import android.app.ProgressDialog;
import android.content.res.AssetManager;
import android.content.res.Resources;
import android.os.AsyncTask;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.MenuItem;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.EditText;
import android.os.Environment;

public class Scratchpad extends Fragment implements OnClickListener {

	private final String tag = getClass().getSimpleName();
	private static String tempFileName = "temp.lp";
	private static String defaultFileName = "afiro.lp";
	private static String solutionFileName = "temp.sol";
	private static String inputFileExt = "";
	private static int nErrorCode[] = new int[1];
	private static StringBuffer cErrorMessage = new StringBuffer(Lindo.LS_MAX_ERROR_MESSAGE_LENGTH);
	public static String inputFileName = "";
	public static String loadedFileName = "";
	private static int buff_size = 1024;
	private static int resetCounter = 0;

	private int numVars[] = new int[1];	// number of variables
	private int numContVars[] = new int[1]; //number of continuous variables
	private int numCons[] = new int[1]; // number of constraints
	double dObj[] = new double[1];

	int nSolStatus[] = new int[1];

	private Resources resources_;
	private AssetManager assetManager_;
	private EditText padEditText;
	private Button buttonSolve;
	private Button buttonLoad;
	private Button buttonClear;
	private Button buttonReset;

	public static String getSolutionFileName() {
		return solutionFileName;
	}

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
	}

	@Override
	public void onResume() {
		super.onResume();
		if(padEditText.getText().toString().length() <= 0 ) {
			appendInitialProblem();
		}
	}

	@Override
	public View onCreateView(LayoutInflater inflater, ViewGroup container,
			Bundle savedInstanceState) {
		View v = inflater.inflate(R.layout.main_activity, container, false);
		padEditText = (EditText) v.findViewById(R.id.padEditText);
		buttonLoad = (Button) v.findViewById(R.id.buttonLoad);
		buttonSolve = (Button) v.findViewById(R.id.buttonSolve);
		buttonClear = (Button) v.findViewById(R.id.buttonClear);
		buttonReset = (Button) v.findViewById(R.id.buttonComments);

		buttonSolve.setOnClickListener(this);
		buttonLoad.setOnClickListener(this);
		buttonClear.setOnClickListener(this);
		buttonReset.setOnClickListener(this);

		resources_ = getResources();
		assetManager_ = resources_.getAssets();

		String fileList[] = null;
		try {
			fileList = assetManager_.list("");
		} catch (IOException e) {
			e.printStackTrace();
		}

		return v;
	}

	private String getFileExtension(String inputFileName_) {
		return inputFileName_.substring(inputFileName_.lastIndexOf(".") + 1, inputFileName_.length());
	}

	private String getFileBase(String inputFileName_) {
		return inputFileName_.substring(0, inputFileName_.lastIndexOf("."));
	}

	private String parseTextArea()
	{
		String editText_ =  padEditText.getText().toString();
		String inputFileName_;
		if (editText_.length()<=1) {
			// empty text, fallback to defaultFileName
			inputFileName_ = defaultFileName;
		} else if (editText_.length()<=16 && editText_.indexOf('.')>=0) {
			// looks like a filename
			inputFileName_ = editText_.replace("\n", "");
		} else {
			// looks like a model, write to a temp file
			inputFileName_ = tempFileName;
			if (loadedFileName.length()>0) {
				String ext_ = getFileExtension(loadedFileName);
				if (ext_.equals("mpi")) {
					inputFileName_ = getFileBase(inputFileName_) + '.' + ext_;
				}
			}
			try {
				writeStream2File(inputFileName_, editText_.toString());
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
		inputFileExt = getFileExtension(inputFileName_);
		File file = new File(getActivity().getFilesDir(), inputFileName_);
		touchFile(file);
		return inputFileName_;
	}

	// Generalized error Reporting function
	private static void APIErrorCheck(Object pEnv )
	{
		if(0 != nErrorCode[0])
		{
			cErrorMessage.setLength(0);
			Lindo.LSgetErrorMessage(pEnv, nErrorCode[0], cErrorMessage);
		}
	}

	private void touchFile(File file) {

		if (!file.exists()) {
			InputStream in = null;

			OutputStream out = null;
			try {
				in = assetManager_.open(file.getName());
				/* Note that "MODE_WORLD_READABLE" is "dangerous". For an alternative ..
				 * See:
				 *  https://github.com/commonsguy/cw-omnibus/tree/master/ContentProvider/Files
				 * for the proper, code-intensive way to handle this.
				 */
				out = getActivity().openFileOutput(file.getName(), Context.MODE_WORLD_READABLE);
				byte[] buffer = new byte[buff_size];
				int read;
				while ((read = in.read(buffer)) != -1) {
					out.write(buffer, 0, read);
				}
				in.close();
				out.flush();
				out.close();
				out = null;
				in = null;
			} catch (IOException e) {
				Log.e("tag", e.getMessage());
			}
		}
		return;
	}


	private class SolveModel extends AsyncTask<String, Void, Integer> {

		Object pEnv_ = MainActivity.pEnv;
        Object pModel = null;
		private ProgressDialog pdia;
		private String modelFileName;


		private void jLogback(Object dummy, String szMessage, Object pls)
		{
			//Log.e("SolveModel: ",szMessage);
		}

		@Override
		protected Integer doInBackground(String... params) {

			try {

				cErrorMessage.setLength(0);

				if (1>0) {
					File sdcard = Environment.getExternalStorageDirectory();
					modelFileName = sdcard.getAbsolutePath() + "/Download/" + inputFileName;
				} else {
					modelFileName = getActivity().getFilesDir() + "/" + inputFileName;
				}


			 /* >>> Step 1 <<< Create model instance */
				pModel = Lindo.LScreateModel(pEnv_, nErrorCode);
				APIErrorCheck(pEnv_);
				if (0 != nErrorCode[0]) {
					return nErrorCode[0];
				}

				//nErrorCode[0] = Lindo.LSsetModelLogfunc(pModel,"jLogback",this);
				APIErrorCheck(pEnv_);
				if (0 != nErrorCode[0]) {
					return nErrorCode[0];
				}

				if (inputFileExt.equals("mpi")) {
					nErrorCode[0] = Lindo.LSreadMPIFile(pModel, modelFileName);
				} else if (inputFileExt.equals("mpx")) {
					nErrorCode[0] = Lindo.LSreadMPXFile(pModel, modelFileName);
				} else if (inputFileExt.equals("lp")) {
					nErrorCode[0] = Lindo.LSreadLPFile(pModel, modelFileName);
					if (0 != nErrorCode[0]) {
						nErrorCode[0] = Lindo.LSreadMPXFile(pModel, modelFileName);
					}
				} else {
                    nErrorCode[0] = Lindo.LSERR_BAD_MPI_FILE;
					cErrorMessage.append("\n\\\\ Error: unknown extension");
					return nErrorCode[0];
                }
				APIErrorCheck(pEnv_);
				if (0 != nErrorCode[0]) {
					return nErrorCode[0];
				}

			/* >>> Step 3 <<< Get some model stats */
				nErrorCode[0] = Lindo.LSgetInfo(pModel, Lindo.LS_IINFO_NUM_VARS, numVars);
				APIErrorCheck(pEnv_);
				if (0 != nErrorCode[0]) {
					return nErrorCode[0];
				}

				nErrorCode[0] = Lindo.LSgetInfo(pModel, Lindo.LS_IINFO_NUM_CONT, numContVars);
				APIErrorCheck(pEnv_);
				if (0 != nErrorCode[0]) {
					return nErrorCode[0];
				}

        	/* >>> Step 4 <<< Perform the optimization with appropriate solver*/
				if (numContVars[0] == numVars[0]) {
					// it is a continuous model (LP or NLP)
					nErrorCode[0] = Lindo.LSoptimize(pModel, Lindo.LS_METHOD_FREE, nSolStatus);
				} else {
					// it is a integer model
					nErrorCode[0] = Lindo.LSsolveMIP(pModel, nSolStatus);
				}
				APIErrorCheck(pEnv_);
				if (0 != nErrorCode[0]) {
					return nErrorCode[0];
				}

        	/* Get the value of the objective */
				if (numContVars[0] == numVars[0]) {
					nErrorCode[0] = Lindo.LSgetInfo(pModel, Lindo.LS_DINFO_POBJ, dObj);
				} else {
					nErrorCode[0] = Lindo.LSgetInfo(pModel, Lindo.LS_DINFO_MIP_OBJ, dObj);
				}
				APIErrorCheck(pEnv_);
			} catch (Exception e) {
				cErrorMessage.append(e.getMessage());
			}

			return nErrorCode[0];
		}

		@Override
		protected void onPostExecute(Integer optimErrCode) {
			int nStatus_[] = new int[1];
			padEditText.setText("");
			if( optimErrCode == 0 ) {
				nErrorCode[0]=Lindo.LSwriteSolution(pModel,solutionFileName);
				if (numContVars[0]==numVars[0]) {
					nErrorCode[0] = Lindo.LSgetInfo(pModel, Lindo.LS_IINFO_PRIMAL_STATUS, nStatus_);
				} else {
					nErrorCode[0] = Lindo.LSgetInfo(pModel, Lindo.LS_IINFO_MIP_STATUS, nStatus_);
				}
				APIErrorCheck(pEnv_);
				if (0 != nErrorCode[0]) {
					appendError("\\\\ Error: " + cErrorMessage.toString() + " \n");
					nErrorCode[0] = Lindo.LSdeleteModel(pModel);
					return;
				}
				if( nStatus_[0] == Lindo.LS_STATUS_BASIC_OPTIMAL ||
						nStatus_[0] == Lindo.LS_STATUS_OPTIMAL ||
						nStatus_[0] == Lindo.LS_STATUS_LOCAL_OPTIMAL ||
						nStatus_[0] == Lindo.LS_STATUS_FEASIBLE ) {
					appendSolution(pModel);
				} else if( nStatus_[0] == Lindo.LS_STATUS_INFEASIBLE ) {
					appendMessage("\\\\ The model is infeasible. \n");
				}
				else if( nStatus_[0] == Lindo.LS_STATUS_UNBOUNDED){
					appendMessage("\\\\ The model has an unbounded solution. \n");
				}
				else if( nStatus_[0] == Lindo.LS_STATUS_INFORUNB){
					appendMessage("\\\\ The model is infeasible or unbounded. \n");
				}
				else {
					appendMessage("\\\\ No errors, but no optimal solution were found.\n");
					appendMessage("\\\\ Model status   : "+nSolStatus[0] + "\n");
				}
			}
			else {
				appendError(cErrorMessage.toString());
			}
			nErrorCode[0] = Lindo.LSdeleteModel(pModel);
			//appendMessage("\\\\ Deleted handle to model "+ inputFileName + " \n");
			pdia.dismiss();
		}

		@Override
		protected void onPreExecute() {
			pdia = new ProgressDialog(getActivity());
			pdia.setMessage("Solving... "+inputFileName + " please wait.");
			pdia.show();
		}

		@Override
		protected void onProgressUpdate(Void... values) {}
	}


	@Override
	public boolean onOptionsItemSelected(MenuItem item) {
		int id = item.getItemId();
		if (id == R.id.action_settings) {
			return true;
		}
		return super.onOptionsItemSelected(item);
	}

	@Override
	public void onClick(View v) {
		int id = v.getId();

		try {
			if (id == buttonSolve.getId()) {
				Log.d(tag, "Solve Button pressed.");
				removeComments();
				inputFileName = parseTextArea();
				new SolveModel().execute("");
			} else if (id == buttonClear.getId()) {
				loadedFileName = "";
				Log.d(tag, "Clear Button pressed.");
				padEditText.setText("");
			} else if (id == buttonLoad.getId()) {
				removeComments();
				inputFileName = parseTextArea();
				Log.d(tag, "Load Button pressed with " + inputFileName);
				readFile2TextArea(inputFileName);
			} else if (id == buttonReset.getId()) {
				Log.d(tag, "Reset Button pressed.");
				padEditText.setText("");
				loadedFileName = "";
				appendInitialProblem();
			}
		} catch (Exception e){
			padEditText.setText("");
			padEditText.append(e.getMessage());
		}
		return;
	}

	private void removeComments() {
		String s = padEditText.getText().toString();
		String[] lines = s.split("\n");
		StringBuilder sb = new StringBuilder();
		for( String line : lines ) {
			if( !line.startsWith("\\") ) {
				sb.append(line+"\n");
			}
		}
		padEditText.setText(sb.toString());
		return;
	}

	public void readFile2TextArea(String inputFileName)
	{
		loadedFileName="";
		StringBuffer sb = new StringBuffer();
		try {
			InputStream in = assetManager_.open(inputFileName);
			byte[] buffer = new byte[1024];
			int read;
			while ((read = in.read(buffer)) != -1) {
				sb.append(new String(buffer, 0, read));
				Log.d("tag", "Read and wrote " + read + " bytes.");
			}
			in.close();
			loadedFileName = inputFileName;
			padEditText.setText("\\\\ Loaded " + inputFileName + "\n");
		} catch (IOException e) {

			File sdcard = Environment.getExternalStorageDirectory();
			//Get the text file
			File file = new File(sdcard+"/Download",inputFileName);
			try {
				BufferedReader br = new BufferedReader(new FileReader(file));
				String line;
				while ((line = br.readLine()) != null) {
					sb.append(line);
					sb.append('\n');
				}
				br.close();
				loadedFileName = inputFileName;
				padEditText.setText("\\\\ Loaded " + inputFileName + "\n");
			}
			catch (IOException e2) {
				Log.e("tag", e2.getMessage());
				padEditText.setText("\\\\ Error loading file " + e.getMessage());
			}

		}
		padEditText.append(sb.toString());
		return;
	}

	private void appendInitialProblem0() {
		StringBuffer sb = new StringBuffer();
		sb.append("  \n");
		sb.append(" maximize 2x1 + 3x2 - x3 \n");
		sb.append(" subject to \n");
		sb.append("  x1 + x2 <= 1 \n");
		sb.append("  x1 + x2 + x3 <= 0 \n");
		sb.append("  x2 - 2x3 <= 2 \n");
		sb.append(" bounds \n");
		sb.append("  x1 free \n");
		sb.append("  x2 free \n");
		sb.append("  x3 free \n");
		sb.append(" end \n");
		padEditText.append(sb.toString());
		return;
	}

	private void appendInitialProblem1() {
		StringBuffer sb = new StringBuffer();
		sb.append("  \n");
		sb.append("Maximize \n");
		sb.append(" obj: x1 + 2x2 + 3x3 + x4 \n");
		sb.append("Subject To \n");
		sb.append("c1: - x1 + x2 + x3 + 10x4 <= 20 \n");
		sb.append("c2: x1 - 3x2 + x3 <= 30 \n");
		sb.append("c3: x2 - 3.5x4 = 0 \n");
		sb.append("Bounds \n");
		sb.append("0 <= x1 <= 40 \n");
		sb.append("2 <= x4 <= 3 \n");
		sb.append("General \n");
		sb.append("x4 \n");
		sb.append("End \n");
		padEditText.append(sb.toString());
		return;
	}

	private void appendInitialProblem2() {
		StringBuffer sb = new StringBuffer();
		sb.append("  \n");
		sb.append("Maximize 20 astro + 30 cosmo\n");
		sb.append("Subject To \n");
		sb.append("astro + 2 cosmo <= 120 \n");
		sb.append("astro <= 120 \n");
		sb.append("End \n");
		padEditText.append(sb.toString());
		return;
	}

	private void appendInitialProblem3() {
		StringBuffer sb = new StringBuffer();
		sb.append("\\\\ box.mpx  \n");
		sb.append("Minimize 2*( .05*(d*w + d*h) +.1*w*h);\n");
		sb.append("subject to\n");
		sb.append("2*(h*d + h*w + d*w) >= 888;\n");
		sb.append("h*d*w >= 1512;\n");
		sb.append("h/w <= .718;\n");
		sb.append("h/w >= .518;\n");
		sb.append("d*w <= 252;\n");
		sb.append("End \n");
		padEditText.append(sb.toString());
		return;
	}
    private void appendInitialProblem4() {
        StringBuffer sb = new StringBuffer();
        sb.append("\\\\ ex_nlp4.mpx  \n");
        sb.append("minimize   X0;\n");
        sb.append("-X1^2*X2 >= -675;\n");
        sb.append("-0.1*X1^2*X3^2 >= -0.419;\n");
        sb.append("0.201*X1^4*X2*X3^2 + 100*X0 = 0;\n");
        sb.append("BOUNDS\n");
        sb.append("X1<=10;\n");
        sb.append("X2<=10;\n");
        sb.append("X3<=10;\n");
        sb.append("-inf <= X0 <= +inf\n");
        sb.append("END\n");
        padEditText.append(sb.toString());
        return;
    }

	int Math_floorMod(int a, int b) {
		return a % b;
	}
	private void appendInitialProblem() {
		int maxCounter=5, idx;
		resetCounter++;
		idx = Math_floorMod(resetCounter,maxCounter);
		if (idx==0) {
			appendInitialProblem0();
		} else if (idx==1) {
			appendInitialProblem1();
		} else if (idx==2) {
			appendInitialProblem2();
		} else if (idx==3) {
			appendInitialProblem3();
        } else if (idx==4) {
            appendInitialProblem4();
		}
	}

	private void appendSolution(Object pModel) {
		DecimalFormat df = new DecimalFormat("0.0000");
		StringBuffer sb = new StringBuffer();
		double adX[] = new double[numVars[0]];

        /* Get the variable values */
		if (numContVars[0]==numVars[0]) {//LP/NLP
			nErrorCode[0] = Lindo.LSgetPrimalSolution(pModel, adX);
		} else { //MILP/MINLP
			nErrorCode[0] = Lindo.LSgetMIPPrimalSolution(pModel, adX);
		}
		APIErrorCheck(MainActivity.pEnv);
		if (0 != nErrorCode[0]) {
			sb.append("Error:" + cErrorMessage.toString());
			return;
		}

		sb.append("\\\\ Model    : " + inputFileName + "\n");
		sb.append("\\\\ Solution : \n");
		sb.append("\\\\ Objective Value = "+df.format(dObj[0])+"\n");
		sb.append("\\\\ Non-zero vars : \n");
		StringBuffer varName=new StringBuffer();
		for( int i = 0; i < numVars[0]; i++ ) {
			if( adX[i] != 0.0 ) {
				nErrorCode[0]=Lindo.LSgetVariableNamej(pModel, i ,varName);
				sb.append("\\\\ "+varName.toString()+"  = "+ df.format(adX[i])+"\n");
				varName.setLength(0);
			}
		}
		sb.append("\\\\ End solution \n");
		padEditText.append(sb.toString());
	}
	private void appendError(String errorString) {
		padEditText.append("\\\\ "+errorString+" \n");
	}

	private void appendMessage(String message) {
		padEditText.append(message);
	}

	public void writeStream2File(String filename, String lines) throws IOException {
		if( filename == null ) filename = tempFileName;
		String fullFileName = getActivity().getFilesDir()+"/"+filename;
		PrintWriter pw = new PrintWriter(new FileWriter(fullFileName));
		pw.write(lines);
		pw.flush();
		pw.close();

		File sdcard = Environment.getExternalStorageDirectory();
		fullFileName = sdcard.getAbsolutePath()+"/Download/"+filename;
		pw = new PrintWriter(new FileWriter(fullFileName));
		pw.write(lines);
		pw.flush();
		pw.close();

	}
}
