'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
''
''    LINDO API Version 4.0
''    Copyright (c) 2000-2005
''
''    LINDO Systems, Inc.            312.988.7422
''    1415 North Dayton St.          info@lindo.com
''    Chicago, IL 60622              http://www.lindo.com
''
''    @ex_mps.vb  
''
''    last updated: 1-31-2003
''
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''/

Option Explicit On 

Imports System
Imports System.IO
Imports System.Text
Imports System.Runtime.InteropServices

Public Class clsCallback
    Public Shared Function func(ByVal model As IntPtr, _
                                ByVal loc As Integer, _
                                ByVal nvCbData As IntPtr) As Integer
        Dim it, nErr As Integer
        Dim ob As Double
        ' get iterations
        nErr = Lindo.LSgetCallbackInfo(model, loc, Lindo.LS_IINFO_SIM_ITER, it)
        ' get primal objective
        nErr = Lindo.LSgetCallbackInfo(model, loc, Lindo.LS_DINFO_POBJ, ob)

        Console.WriteLine("callback @iter={0}, obj={1}",it,ob)

        func = 0

    End Function
End Class


Public Class App

    Public Shared Sub CheckErr(ByVal env As IntPtr, ByVal errorcode As Integer)

        ' Checks for an error condition.  If one exists, the
        '  error message is displayed then the application
        '  terminates.

        If (errorcode > 0) Then
            Dim message As New StringBuilder(256)
            Call Lindo.LSgetErrorMessage(env, errorcode, message)
            Console.WriteLine(message)
            End
        End If

    End Sub
   
    Public Shared Sub Main(args() As String)    

        Dim env As IntPtr
        Dim prob As IntPtr        
        Dim errorcode As Integer

        Dim i As Integer
        Dim m As Integer
        Dim n As Integer

        Dim obj As Double

        Dim LicenseKey As New StringBuilder(lindo.LS_MAX_ERROR_MESSAGE_LENGTH)
        Dim LibVersion As New StringBuilder(256)
        Dim LibBuilded As New StringBuilder(256)                
        
        If args.Length <> 1 Then
            Console.WriteLine("Usage: ex_mps filename")
            Return
        End If        


        ' Read license key from file
        errorcode = Lindo.LSloadLicenseString("../../../license/lndapi140.lic", LicenseKey)
        If (errorcode > 0) Then
        	errorcode = Lindo.LSloadLicenseString("../../../../license/lndapi140.lic", LicenseKey)
        	If (errorcode > 0) Then
            	Console.WriteLine("License file does not exist.")
            	Call CheckErr(env, errorcode)
            End If
        End If

        ' Create a LINDO environment.
        env = Lindo.LScreateEnv(errorcode, LicenseKey.ToString())
        If (errorcode > 0) Then
            Console.WriteLine("Unable to create environment.")
            End
        End If

        ' Create a model in the environment.
        prob = Lindo.LScreateModel(env, errorcode)
        Call CheckErr(env, errorcode)

        ' Display API version        
        Console.WriteLine()
        Lindo.LSgetVersionInfo(LibVersion,Nothing)
        Console.WriteLine("Lindo API version {0}",LibVersion)
      
        ' Read input model
        Console.WriteLine()        
        Console.WriteLine("Reading {0}.",args(0))
        errorcode = Lindo.LSreadMPSFile( prob, args(0), 0)
        Call CheckErr(env, errorcode)

        ' Set callback function
        Dim cb As Lindo.typCallback
        cb = AddressOf clsCallback.func
        errorcode = Lindo.LSsetCallback(prob, cb, Nothing)
        Call CheckErr(env, errorcode)

        ' Perform the optimization.
        Console.WriteLine()        
        errorcode = Lindo.LSoptimize(prob, Lindo.LS_METHOD_FREE, 0)
        Call CheckErr(env, errorcode)

        ' Retrieve the solution and print
        errorcode = Lindo.LSgetInfo(prob, Lindo.LS_DINFO_POBJ, obj)
        Call CheckErr(env, errorcode)
        
        errorcode = Lindo.LSgetInfo(prob, Lindo.LS_IINFO_NUM_VARS, n)
        errorcode = Lindo.LSgetInfo(prob, Lindo.LS_IINFO_NUM_CONS, m)
        
        Dim x(n) as double        
        errorcode = Lindo.LSgetPrimalSolution(prob, x)
        Call CheckErr(env, errorcode)
        
        Dim y(m) as double
        errorcode = Lindo.LSgetDualSolution(prob, y)
        Call CheckErr(env, errorcode)        
        
        Console.WriteLine()
        Console.WriteLine("Objective value = {0} ",obj)         
        
        Dim StrName As New StringBuilder(256)
        
        Console.WriteLine()
        Console.WriteLine("Primal Solution")
        for i=0 to n-1
           errorcode = Lindo.LSgetVariableNamej(prob,i, StrName)
           Console.WriteLine("{0} = {1} ",StrName.Tostring,x(i))         
        next i
        
        Console.WriteLine()
        Console.WriteLine("Dual Solution")
        for i=0 to m-1
           errorcode = Lindo.LSgetConstraintNamei(prob,i, StrName)
           Console.WriteLine("{0} = {1} ",StrName.ToString,y(i))         
        next i
           

        ' Delete the LINDO environment.
        Call Lindo.LSdeleteEnv(env)

     End Sub
    
End Class
