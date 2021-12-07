
1. Open IIS Manager and add a new application to the default web site. 
Set 'ex_nlp3' as the name of the application.

2. Set physical path of the application to 
$(LINDOAPI_HOME)\samples\dotnet\cs\ex_asp\WebSite1

3. Select 'DefaultAppPool' as the application pool of 'ex_nlp3'.
Make sure DefaultAppPool exists and runs on .NET v2.0. Also make sure
DefaultAppPool's '32-bit Applications' property is enabled.

4. Modify 'ex_nlp3.cs' so that LSloadLicenseString() call points to the
correct license file on your installation.

5. Point your web browser to http://localhost/ex_nlp3/ and click solve.



