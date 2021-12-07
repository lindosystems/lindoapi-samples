rem ----------------------------------------------------
rem     		LINDO API sample web application
rem     		(requires Tomcat server)
rem ----------------------------------------------------


rem Stop Tomcat
rem
rem net stop "Apache Tomcat 7.0 Tomcat7" 

rem Compile classes (requires log4j-1.2.17.jar)
rem Visit http://logging.apache.org/log4j/2.x/download.html
rem
%JAVA_HOME%\javac.exe -classpath "%CATALINA_HOME%\lib\log4j-1.2.17.jar";"%CATALINA_HOME%\lib\servlet-api.jar";%LINDOAPI_HOME%\lib\win32\lindo9_0.jar SolveModel.java

rem Copy classes to WEB-INF\classes
rem
copy *.class "%CATALINA_HOME%\webapps\examples\WEB-INF\classes"
copy SolveModel.properties "%CATALINA_HOME%\webapps\examples\WEB-INF\classes"

rem Copy jar files to WEB-INF\lib
rem
copy %LINDOAPI_HOME%\lib\win32\lindo9_0.jar "%CATALINA_HOME%\webapps\examples\WEB-INF\lib"
copy "%CATALINA_HOME%\lib\log4j-1.2.17.jar" "%CATALINA_HOME%\webapps\examples\WEB-INF\lib"

rem Update WEB-INF\web.xml with the following tags
rem
rem Add servlet node
rem 
rem    <servlet>
rem        <servlet-name>SolveModel</servlet-name>
rem        <servlet-class>SolveModel</servlet-class>
rem    </servlet>
rem
rem    
rem    
rem Add servlet-mapping node
rem
rem    <servlet-mapping>
rem        <servlet-name>SolveModel</servlet-name>
rem        <url-pattern>/servlets/servlet/SolveModel</url-pattern>
rem    </servlet-mapping>	
rem 

rem Append the following to "%CATALINA_HOME%\webapps\examples\WEB-INF\classes\LocalStrings.properties"
rem 
rem solvemodel.title=Solve Model Example
rem solvemodel.params-in-req=Parameters in this request:
rem solvemodel.no-params=Enter Model Name and Path
rem solvemodel.modelname=Model Name:
rem solvemodel.modelpath=Model Path:

rem Start Tomcat
rem 
rem net start "Apache Tomcat 7.0 Tomcat7" 

rem Test with URL
rem http://localhost:8080/examples/servlets/servlet/SolveModel
rem Results are written to %CATALINA_HOME%\logs\test