/*
	Lindo API for Android
	Lindo Systems, Inc.
	Copyright 2017

	Design by Joseph Rios, Littlepancake Software, info@littlepancake.com
	Copyright 2014,
*/
package com.lindo.api;

import com.lindo.Lindo;

import android.content.res.AssetManager;
import android.content.res.Resources;
import android.os.Bundle;
import android.os.Environment;
import android.support.v4.app.FragmentActivity;
import android.support.v4.app.FragmentTabHost;
import android.util.Log;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;

public class MainActivity extends FragmentActivity {
	public static final String TAB_1 = "Scratchpad";
	public static final String TAB_2 = "About";

    public static Object pEnv = null;
    private static StringBuffer cLicenseKey = new StringBuffer(Lindo.LS_MAX_ERROR_MESSAGE_LENGTH);
    private static StringBuffer cLicenseKey2 = null;
    private static StringBuffer cErrorMessage = new StringBuffer(Lindo.LS_MAX_ERROR_MESSAGE_LENGTH);
    private static String licenseFile;
    private static int nErrorCode[] = new int[1];

    private FragmentTabHost mTabHost;
    private Resources resources_;
    private AssetManager assetManager_;

    public StringBuffer readExternalLicenseFile()
    {
        StringBuffer sb = null;

        sb = new StringBuffer(Lindo.LS_MAX_ERROR_MESSAGE_LENGTH);

        File sdcard = Environment.getExternalStorageDirectory();
        //Get the text file
        File file = new File(sdcard+"/Download","lndapi130.lic");
        try {
            BufferedReader br = new BufferedReader(new FileReader(file));
            String line;
            while ((line = br.readLine()) != null) {
                sb.append(line);
                sb.append('\n');
            }
            br.close();
        }
        catch (IOException e2) {
            Log.e("tag", e2.getMessage());
            return null;
        }

        return sb;
    }

    // Lindo environment gets created here
    private Object createLindoEnv() {
        //licenseFile = About.openLicenseFile(null);

        cLicenseKey = new StringBuffer(
                "R$S5-Yp@g-f?x2-3*GR-zr#q-VG%g-CZGk-aQQj-z9Vs-VNY4-" +
                "Ytbr-vzuE-q7hS-8oq@-qgcD-Lu*4-BUdY-t6*Q-a5sp-4bcy-" +
                "Jp6X-SdJc-CVtj-aS&R-drPj-c2aN-LTqF-TDPr-gwjG-LMWK-" +
                "u5Xo-e$hD-$3nv-dDcg-W2oP-dMyH-er%?-sRRf-9vQi-2C%y-" +
                "kvCP-dJHx-qJpQ-kM3o-DA98-NvNY-xMgZ-uFiQ-FY3X-72CE-" +
                "T5JC-3Fxj-$sfV-e?Et-QZg8-QWLz-EtYB-WRSe-g5jH-#Ted-" +
                "nVJe-&ax2-mfMA-Kxi@-aJTM-GLwt-a%bd-LmX6-3bX?-p4wH-" +
                "Tte8-b&nk-NyBY-Pr#D-J2RQ-tP%T-aW5W-9XS8-FhaL-rCRa-" +
                "n5Q7-jFGg-BTiK-Ago5-mPrX-$DG"
        );

        cLicenseKey2 = null;
        cLicenseKey2 = readExternalLicenseFile();

         /* >>> Step 1 <<< Re-read license key from a license file  */
        if (cLicenseKey2==null) {
            cLicenseKey2 = cLicenseKey;
        }

        /* >>> Step 2 <<< Create a LINDO environment */
        pEnv = Lindo.LScreateEnv(nErrorCode, cLicenseKey2.toString());
        if(0 != nErrorCode[0])
        {
            cErrorMessage.setLength(0);
            Lindo.LSgetErrorMessage(pEnv, nErrorCode[0], cErrorMessage);
        }

        return pEnv;
    }

    // Version reporting function
    public static String getLindoVersion()
    {
        StringBuffer szVersion = new StringBuffer(255);
        StringBuffer szBuild   = new StringBuffer(255);
        Lindo.LSgetVersionInfo(szVersion, szBuild);
        String versionString = "LINDO API Version "+szVersion.toString() + "\nBuilt on " + szBuild.toString();
        return versionString;
    }

    /*
      Adding onStart() to have a hook into the Activity lifecycle.  Re/creating the env
      for Lindo here.
     */
    @Override
    protected void onStart() {
        super.onStart();

        if (pEnv==null) {
            pEnv = createLindoEnv();
            if (pEnv == null) {
                return;
            }
            Log.d("MainActivity", "Created pEnv in onStart()");
        }
    }

    /*
        Adding onResume() to have a hook into the Activity lifecycle.  Re/creating the env
        for Lindo here.
     */
    protected void onResume() {
        super.onResume();

        if (pEnv==null) {
            pEnv = createLindoEnv();
            if (pEnv == null) {
                return;
            }
        }

    }

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        resources_ = getResources();
        assetManager_ = resources_.getAssets();

        setContentView(R.layout.tab_layout);
        mTabHost = (FragmentTabHost) findViewById(R.id.tabhost);
        mTabHost.setup(this, getSupportFragmentManager(), R.id.tabcontent);

        mTabHost.addTab(
                mTabHost.newTabSpec(TAB_1).setIndicator(TAB_1,
                        getResources().getDrawable(android.R.drawable.star_on)),
                        Scratchpad.class, null);

        mTabHost.addTab(
                mTabHost.newTabSpec(TAB_2).setIndicator(TAB_2,
                        getResources().getDrawable(android.R.drawable.star_on)),
                About.class, null);

        /* Creation of the environment to onStart() to have it more
            reliably be created in the Activity lifecycle.
         */
//        if (pEnv==null) {
//            pEnv = createLindoEnv();
//            if (pEnv == null) {
//                return;
//            }
//        }
    }

    /* Moved deletion after call to the super per best practices.
     */
    @Override
    public void onDestroy() {
        super.onDestroy();
        nErrorCode[0] = Lindo.LSdeleteEnv( pEnv);
        pEnv = null;
    }

    /* Added onStop() to ensure the Lindo environment is properly deleted when the activity is
        stopped.
     */
    @Override
    public void onStop() {
        super.onStop();
        nErrorCode[0] = Lindo.LSdeleteEnv(pEnv);
        pEnv = null;
    }

	static {
        System.loadLibrary("lindo");
	}
}