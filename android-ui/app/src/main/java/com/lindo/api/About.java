/*
	Lindo API for Android
	Lindo Systems, Inc.
	Copyright 2017

	Design by Joseph Rios, Littlepancake Software, info@littlepancake.com
	Copyright 2014,
*/
package com.lindo.api;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import android.annotation.SuppressLint;
import android.content.Context;
import android.content.Intent;
import android.content.res.AssetManager;
import android.net.Uri;
import android.os.Bundle;
import android.os.Environment;
import android.support.v4.app.Fragment;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.TextView;


@SuppressLint("WorldReadableFiles")
@SuppressWarnings("deprecation")
public class About extends Fragment implements OnClickListener {

	private final String manualName = "lindoapi.pdf";
	private Button readManualButton, viewLicButton;
	private static int buff_size = 1024;
	private AssetManager assetManager_;


	@Override
	public View onCreateView(LayoutInflater inflater, ViewGroup container,
			Bundle savedInstanceState) {
		View v = inflater.inflate(R.layout.about_layout, container, false);
		TextView tv1 = (TextView) v.findViewById(R.id.textView1);
		tv1.setText(MainActivity.getLindoVersion());
		readManualButton = (Button) v.findViewById(R.id.readButton);
		readManualButton.setOnClickListener(this);
		viewLicButton = (Button) v.findViewById(R.id.viewLicense);
		viewLicButton.setOnClickListener(this);
		assetManager_ = getActivity().getAssets();
		return v;
	}

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
	}

	/* Taken from here:
	 * http://stackoverflow.com/questions/17085574/read-a-pdf-file-from-assets-folder
	 * with a few small changes.
	 */
	private void copyReadAsset(File file) {

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


	private void openManualPdf()
	{
		File file = new File(getActivity().getFilesDir(), manualName);
		copyReadAsset(file);

		Intent intent = new Intent(Intent.ACTION_VIEW);
		intent.setDataAndType(
				Uri.parse("file://" + getActivity().getFilesDir() + "/"+ manualName),
				"application/pdf");
		startActivity(intent);
	}

	private void openLicenseFile()
	{
		File sdcard = Environment.getExternalStorageDirectory();

		Intent intent = new Intent(Intent.ACTION_VIEW);
		intent.setDataAndType(
				Uri.parse("file://" +sdcard+"/Download" + "/"+"lndapi130.lic"),
				"text/plain");
		startActivity(intent);
	}


	@Override
	public void onClick(View v) {
		int id = v.getId();
		if( id == readManualButton.getId() ) {
			openManualPdf();
		}
		else if( id == viewLicButton.getId() ) {
			openLicenseFile();
		}
	}
}
