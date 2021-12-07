
- Targetting Win64 (x64) platform

The sample applications target the Win32 platform by default. If your LINDO API installation is for 
the Win64 platform, follow these steps to configure the sample Visual Studio projects for Win64 (x64) 
platform.

a.  Click on Menu item “Build|Configuration Manager” and 
b.  Select “x64” or “Any CPU” in “Active Solution Platform” combo box.
c.  If x64 is not shown, click <New> in the combo box and select “x64” in the list that pops up
d.  Click Ok to finish this step.
e.  Make sure the “Build|Configuration Manager” ? Active Solution Platform is now set to “x64” or “Any CPU”.
f.  Edit “lindo.cs” file and add the following statement at the top of the file 
	
			#define LSWIN64
    
    After the modification, the top of the file should look like as follows
    
			#define LSWIN64
			using System;
			using System.Text;
			using System.Runtime.InteropServices;

You should now be able to build in x64 mode.
