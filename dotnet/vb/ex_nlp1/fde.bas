Attribute VB_Name = "fde"
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
''
''    LINDO API Version 4.0
''    Copyright (c) 2006
''
''    LINDO Systems, Inc.            312.988.7422
''    1415 North Dayton St.          info@lindo.com
''    Chicago, IL 60622              http://www.lindo.com


' Static evaluation counter
Public Evals As Long




' This is the callback function that evaluates functional values at each solution (pdX)
' passed by the solver. Since this function is going to be called several times during
' the solution process it should be implemented as efficient as possible.

' pdX is a pointer to the solution for which functional values are requested by the solver.
' For efficiency reasons we use a local array (temp) and have its data field temporarily
' point to pdX. This trick makes the 'temp' array the solution vector. Note, since no large
' blocks of memory had to be copied in the process, the approach produces the best achievable
' result in terms of speed.  See MyAttachArray and MyDetachArray functions in swap.bas


' Refer to LINDO API user manual for details on LSsetFuncalc() routine

Public Function fdeCallback(ByVal nModel As Long, _
                            ByVal ndata As Long, _
                            ByVal nRow As Long, _
                            ByVal pdX As Long, _
                            ByVal nJDiff As Long, _
                            ByVal dXJDiff As Double, _
                            ByRef pdFuncVal As Double, _
                            ByVal pReserved As Long _
                            ) As Long


Dim temp() As Double ' The temporary array that will keep the current solution
                     ' throughout the computations


Dim f As Double      ' Functional value at current solution (x,y)
Dim x As Double      ' x := temp(0)
Dim y As Double      ' y := temp(1)


Dim pvData As Long   ' Pointer to the initial temp() values
Dim nVars As Long    ' Number of variables in the model.
Dim Iters As Long
Dim nEvals As Long


nVars = 2

' Array size should match the number of variables in the model
ReDim temp(nVars) As Double


' Attach pdX to temp() array
Call MyAttachArray(temp, ByVal pdX, pvData)


 x = temp(0)
 y = temp(1)

' Compute objective's functional value*/
If nRow = -1 Then
   f = 3 * (1 - x) ^ 2 * Exp(-(x ^ 2) - (y + 1) ^ 2) _
   - 10 * (x / 5 - x ^ 3 - y ^ 5) * Exp(-(x ^ 2) - (y ^ 2)) _
   - 1 / 3 * Exp(-((x + 1) ^ 2) - y ^ 2)
   Evals = Evals + 1

' Compute constaint 0's functional value
ElseIf nRow = 0 Then
   f = x * x + y - 6
   Evals = Evals + 1

' Compute constaint 1's functional value
ElseIf nRow = 1 Then
   f = x + y * y - 6
   Evals = Evals + 1
   
End If

' Pass f(x,y) back to API
pdFuncVal = f

' A nonzero return value interrupts the solver
fdeCallback = 0

' detach pdX from temp() array
Call MyDetachArray(temp, pvData)

If (Evals Mod 10) = 0 Then
   nErr = LSgetInfo(nModel, LS_IINFO_NLP_ITER, Iters)
   Form1.Label18 = Iters
   Form1.Label16 = Evals
End If
   


End Function

