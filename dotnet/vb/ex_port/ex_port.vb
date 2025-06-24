'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
''
''    LINDO API Version 9.0
''    Copyright (c) 2000-2014
''
''    LINDO Systems, Inc.            312.988.7422
''    1415 North Dayton St.          info@lindo.com
''    Chicago, IL 60622              http://www.lindo.com
''
''  File: ex_port.vb  
''
''  Purpose: Solve a quadratic mixed integer programming problem.
''  Model  : Portfolio Selection Problem with a Restriction on
''           the Number of Assets
''
''           MINIMIZE   0.5 w'Q w
''           s.t.   sum_i  w(i)              =  1
''                  sum_i  r(i)w(i)         >=  R
''                  for_i  w(i) - u(i) x(i) <=  0   i=1...n
''                  sum_i  x(i)             <=  K
''                  for_i  x(i) are binary          i=1...n
''           where
''           r(i)  : return on asset i.
''           u(i)  : an upper bound on the proportion of total budget
''                   that could be invested on asset i.
''           Q(i,j): covariance between the returns of i^th and j^th
''                   assets.
''           K     : max number of assets allowed in the portfolio
''           w(i)  : proportion of total budget invested on asset i
''           x(i)  : a 0-1 indicator if asset i is invested on.
''
''  Data:
''  Covariance Matrix:
''               A1      A2      A3      A4      A5      A6      A7
''       A1 [  1.00    0.11    0.04    0.02    0.08    0.03    0.10 ]
''       A2 [  0.11    1.00    0.21    0.13    0.43    0.14    0.54 ]
''       A3 [  0.04    0.21    1.00    0.05    0.16    0.05    0.20 ]
''   Q = A4 [  0.02    0.13    0.05    1.00    0.10    0.03    0.12 ]
''       A5 [  0.08    0.43    0.16    0.10    1.00    0.10    0.40 ]
''       A6 [  0.03    0.14    0.05    0.03    0.10    1.00    0.12 ]
''       A7 [  0.10    0.54    0.20    0.12    0.40    0.12    1.00 ]
''
''  Returns Vector:
''               A1      A2      A3      A4      A5      A6      A7
''    r =   [  0.14    0.77    0.28    0.17    0.56    0.18    0.70 ]
''
''  Maximum Proportion of Total Budget to be Invested on Assets
''               A1      A2      A3      A4      A5      A6      A7
''    u =   [  0.04    0.56    0.37    0.32    0.52    0.38    0.25 ]
''
''  Target Return:
''  R = 0.30
''
''  Maximum Number of Assets:
''  K = 3
''
''  Last updated: 10-15-2014
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

        Console.WriteLine("callback @iter={0}, MipObj={1}",it,ob)

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
   
    Public Shared Sub SetupModel(ByVal env As IntPtr, ByVal pModel As IntPtr)

        ''' begin LP data

        '''''''' Model sizes, objective sense and constant
        Dim errorcode As Integer
        Dim nCons As Integer
        Dim nVars As Integer
        Dim dObjSense As String
        Dim dObjConst As Double
        errorcode = 0
        nCons = 10
        nVars = 14
        dObjSense = lindo.LS_MIN
        dObjConst = 0
        '''''''' Objective coefficients
        Dim padC As Double() = New Double() _
        { _
                       0, 0, 0, 0, 0, 0, _
                       0, 0, 0, 0, 0, 0, _
                       0, 0, -1 _
        }

        '''''''' RHS values
        Dim padB As Double() = New Double() _
        { _
                       1, 0.3, 0, 0, 0, 0, _
                       0, 0, 0, 3, -1 _
        }

        '''''''' Constraint types
        Dim pszConTypes As String
        pszConTypes = _
            "EGLLLL" + _
            "LLLL"

        ''''''''  Total nonzeros in A matrix
        Dim nAnnz As Integer
        nAnnz = 35
        ''''''''  Column offset
        Dim paiAcols As Integer() = New Integer() _
        { _
                       0, 3, 6, 9, 12, 15, _
                      18, 21, 23, 25, 27, 29, _
                      31, 33, 35, -1 _
        }

        ''''''''  Column counts
        Dim panAcols As Integer() = New Integer() _
        { _
                       3, 3, 3, 3, 3, 3, _
                       3, 2, 2, 2, 2, 2, _
                       2, 2, -1 _
        }

        ''''''''  A coeff
        Dim padAcoef As Double() = New Double() _
        { _
                       1, 0.14, 1, 1, 0.77, 1, _
                       1, 0.28, 1, 1, 0.17, 1, _
                       1, 0.56, 1, 1, 0.18, 1, _
                       1, 0.7, 1, -0.04, 1, -0.56, _
                       1, -0.37, 1, -0.32, 1, -0.52, _
                       1, -0.38, 1, -0.25, 1, -1 _
        }

        ''''''''' Row indices
        Dim paiArows As Integer() = New Integer() _
        { _
                       0, 1, 2, 0, 1, 3, _
                       0, 1, 4, 0, 1, 5, _
                       0, 1, 6, 0, 1, 7, _
                       0, 1, 8, 2, 9, 3, _
                       9, 4, 9, 5, 9, 6, _
                       9, 7, 9, 8, 9, -1 _
        }

        '''''''' Lower bounds
        Dim padL As Double() = New Double() _
        { _
                       0, 0, 0, 0, 0, 0, _
                       0, 0, 0, 0, 0, 0, _
                       0, 0, -1 _
        }

        '''''''' Upper bounds
        Dim padU As Double() = New Double() _
        { _
                  1.0E+30, 1.0E+30, 1.0E+30, 1.0E+30, 1.0E+30, 1.0E+30, _
                  1.0E+30, 1, 1, 1, 1, 1, _
                       1, 1, -1 _
        }

        '''''''' Load LP data
        errorcode = Lindo.LSloadLPData(pModel, _
                     nCons, nVars, dObjSense, dObjConst, padC, padB, _
                     pszConTypes, nAnnz, paiAcols, panAcols, padAcoef, paiArows, padL, padU)
        Call CheckErr(env, errorcode)
        '' end LP/QP/CONE data




        '' begin INTEGER data

        ''''''''  Variable type
        Dim pszVarTypes As String
        pszVarTypes = _
            "CCCCCC" + _
            "CBBBBB" + _
            "BB"

        errorcode = Lindo.LSloadVarType(pModel, pszVarTypes)
        Call CheckErr(env, errorcode)
        '' end INTEGER data



        '' begin QCP data

        ''' QCP data 

        Dim QCnonzeros As Integer
        QCnonzeros = 28
        ''''''''' QCP Row indices
        Dim QCrowndx As Integer() = New Integer() _
        { _
                      -1, -1, -1, -1, -1, -1, _
                      -1, -1, -1, -1, -1, -1, _
                      -1, -1, -1, -1, -1, -1, _
                      -1, -1, -1, -1, -1, -1, _
                      -1, -1, -1, -1, -1 _
        }

        ''''''''' QCP Col1 indices
        Dim QCcolndx1 As Integer() = New Integer() _
        { _
                       0, 0, 0, 0, 0, 0, _
                       0, 1, 1, 1, 1, 1, _
                       1, 2, 2, 2, 2, 2, _
                       3, 3, 3, 3, 4, 4, _
                       4, 5, 5, 6, -1 _
        }

        ''''''''' QCP Col2 indices
        Dim QCcolndx2 As Integer() = New Integer() _
        { _
                       0, 1, 2, 3, 4, 5, _
                       6, 1, 2, 3, 4, 5, _
                       6, 2, 3, 4, 5, 6, _
                       3, 4, 5, 6, 4, 5, _
                       6, 5, 6, 6, -1 _
        }

        ''''''''' QCP values
        Dim QCcoef As Double() = New Double() _
        { _
                       1, 0.11, 0.04, 0.02, 0.08, 0.03, _
                     0.1, 1, 0.21, 0.13, 0.43, 0.14, _
                    0.54, 1, 0.05, 0.16, 0.05, 0.2, _
                       1, 0.1, 0.03, 0.12, 1, 0.1, _
                     0.4, 1, 0.12, 1, -1 _
        }

        errorcode = Lindo.LSloadQCData(pModel, QCnonzeros, QCrowndx, QCcolndx1, QCcolndx2, QCcoef)
        Call CheckErr(env, errorcode)
        '' end QCP data

    End Sub
    
    Public Shared Sub Main(args() As String)    

        Dim env As IntPtr
        Dim pModel As IntPtr
        Dim errorcode As Integer

        Dim i As Integer
        Dim m As Integer
        Dim n As Integer

        Dim MipObj As Double

        Dim LicenseKey As New StringBuilder(lindo.LS_MAX_ERROR_MESSAGE_LENGTH)
        Dim LibVersion As New StringBuilder(256)
        Dim LibBuilded As New StringBuilder(256)


        ' Read license key from file
        errorcode = Lindo.LSloadLicenseString("../../../license/lndapi160.lic", LicenseKey)
        If (errorcode > 0) Then
            errorcode = Lindo.LSloadLicenseString("../../../../license/lndapi160.lic", LicenseKey)
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
        pModel = Lindo.LScreateModel(env, errorcode)
        Call CheckErr(env, errorcode)

        ' Display API version        
        Console.WriteLine()
        Lindo.LSgetVersionInfo(LibVersion, Nothing)
        Console.WriteLine("Lindo API version {0}", LibVersion)

        ' Load input model
        Console.WriteLine()
        Console.WriteLine("Setting up model")
        SetupModel(env, pModel)

        ' Set callback function
        Dim cb As Lindo.typCallback
        cb = AddressOf clsCallback.func
        'errorcode = Lindo.LSsetCallback(pModel, cb, Nothing)
        'Call CheckErr(env, errorcode)

        ' Perform the optimization.
        Console.WriteLine()
        errorcode = Lindo.LSsolveMIP(pModel, 0)
        Call CheckErr(env, errorcode)

        ' Retrieve the solution and print
        errorcode = Lindo.LSgetInfo(pModel, Lindo.LS_DINFO_MIP_OBJ, MipObj)
        Call CheckErr(env, errorcode)

        errorcode = Lindo.LSgetInfo(pModel, Lindo.LS_IINFO_NUM_VARS, n)
        errorcode = Lindo.LSgetInfo(pModel, Lindo.LS_IINFO_NUM_CONS, m)

        Dim x(n) As Double
        errorcode = Lindo.LSgetMIPPrimalSolution(pModel, x)
        Call CheckErr(env, errorcode)

        Console.WriteLine()
        Console.WriteLine("*** Optimal Portfolio Objective = {0} ", MipObj)

        Dim StrName As New StringBuilder(256)

        Console.WriteLine()
        Console.WriteLine("Primal Solution")
        For i = 0 To n/2
            Console.WriteLine("Invest {0:F2} percent of total budget in asset {1} ", 100*x(i),i+1)
        Next i

           

        ' Delete the LINDO environment.
        Call Lindo.LSdeleteEnv(env)

     End Sub
    
End Class
