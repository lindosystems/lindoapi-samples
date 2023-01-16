VERSION 5.00
Begin VB.Form Form1 
   Caption         =   "Staff Scheduling Example Using the LINDO API"
   ClientHeight    =   7020
   ClientLeft      =   60
   ClientTop       =   345
   ClientWidth     =   6870
   LinkTopic       =   "Form1"
   ScaleHeight     =   7020
   ScaleWidth      =   6870
   StartUpPosition =   3  'Windows Default
   Begin VB.CommandButton Exit 
      Caption         =   "Exit"
      Height          =   375
      Left            =   4920
      TabIndex        =   10
      Top             =   2520
      Width           =   1095
   End
   Begin VB.CommandButton Solve 
      Caption         =   "Solve"
      Height          =   375
      Left            =   4920
      TabIndex        =   9
      Top             =   2040
      Width           =   1095
   End
   Begin VB.TextBox Needs 
      Alignment       =   2  'Center
      BeginProperty DataFormat 
         Type            =   0
         Format          =   "0"
         HaveTrueFalseNull=   0
         FirstDayOfWeek  =   0
         FirstWeekOfYear =   0
         LCID            =   1033
         SubFormatType   =   0
      EndProperty
      Height          =   375
      Index           =   0
      Left            =   1440
      TabIndex        =   1
      Text            =   "0"
      Top             =   2400
      Width           =   975
   End
   Begin VB.TextBox Needs 
      Alignment       =   2  'Center
      Height          =   375
      Index           =   6
      Left            =   1440
      TabIndex        =   8
      Text            =   "0"
      Top             =   5280
      Width           =   975
   End
   Begin VB.TextBox Needs 
      Alignment       =   2  'Center
      Height          =   375
      Index           =   5
      Left            =   1440
      TabIndex        =   7
      Text            =   "0"
      Top             =   4800
      Width           =   975
   End
   Begin VB.TextBox Needs 
      Alignment       =   2  'Center
      Height          =   375
      Index           =   4
      Left            =   1440
      TabIndex        =   5
      Text            =   "0"
      Top             =   4320
      Width           =   975
   End
   Begin VB.TextBox Needs 
      Alignment       =   2  'Center
      Height          =   375
      Index           =   3
      Left            =   1440
      TabIndex        =   4
      Text            =   "0"
      Top             =   3840
      Width           =   975
   End
   Begin VB.TextBox Needs 
      Alignment       =   2  'Center
      Height          =   375
      Index           =   2
      Left            =   1440
      TabIndex        =   3
      Text            =   "0"
      Top             =   3360
      Width           =   975
   End
   Begin VB.TextBox Needs 
      Alignment       =   2  'Center
      Height          =   375
      Index           =   1
      Left            =   1440
      TabIndex        =   2
      Text            =   "0"
      Top             =   2880
      Width           =   975
   End
   Begin VB.Label Label3 
      Caption         =   "Label3"
      Height          =   1095
      Left            =   600
      TabIndex        =   37
      Top             =   720
      Width           =   5415
   End
   Begin VB.Label Label2 
      Caption         =   "Label2"
      Height          =   375
      Left            =   720
      TabIndex        =   36
      Top             =   120
      Width           =   5415
   End
   Begin VB.Label Total 
      Alignment       =   2  'Center
      Caption         =   "0"
      Height          =   255
      Left            =   2640
      TabIndex        =   35
      Top             =   6000
      Width           =   855
   End
   Begin VB.Label Start 
      Alignment       =   2  'Center
      Caption         =   "0"
      Height          =   255
      Index           =   6
      Left            =   2640
      TabIndex        =   34
      Top             =   5280
      Width           =   855
   End
   Begin VB.Label Start 
      Alignment       =   2  'Center
      Caption         =   "0"
      Height          =   255
      Index           =   5
      Left            =   2640
      TabIndex        =   33
      Top             =   4800
      Width           =   855
   End
   Begin VB.Label Start 
      Alignment       =   2  'Center
      Caption         =   "0"
      Height          =   255
      Index           =   4
      Left            =   2640
      TabIndex        =   32
      Top             =   4320
      Width           =   855
   End
   Begin VB.Label Start 
      Alignment       =   2  'Center
      Caption         =   "0"
      Height          =   255
      Index           =   3
      Left            =   2640
      TabIndex        =   31
      Top             =   3840
      Width           =   855
   End
   Begin VB.Label Start 
      Alignment       =   2  'Center
      Caption         =   "0"
      Height          =   255
      Index           =   2
      Left            =   2640
      TabIndex        =   30
      Top             =   3360
      Width           =   855
   End
   Begin VB.Label Start 
      Alignment       =   2  'Center
      Caption         =   "0"
      Height          =   255
      Index           =   1
      Left            =   2640
      TabIndex        =   29
      Top             =   2880
      Width           =   855
   End
   Begin VB.Label Start 
      Alignment       =   2  'Center
      Caption         =   "0"
      Height          =   255
      Index           =   0
      Left            =   2640
      TabIndex        =   28
      Top             =   2400
      Width           =   855
   End
   Begin VB.Label OnDuty 
      Alignment       =   2  'Center
      Caption         =   "0"
      Height          =   255
      Index           =   6
      Left            =   3720
      TabIndex        =   27
      Top             =   5280
      Width           =   975
   End
   Begin VB.Label OnDuty 
      Alignment       =   2  'Center
      Caption         =   "0"
      Height          =   255
      Index           =   5
      Left            =   3720
      TabIndex        =   26
      Top             =   4800
      Width           =   975
   End
   Begin VB.Label OnDuty 
      Alignment       =   2  'Center
      Caption         =   "0"
      Height          =   255
      Index           =   4
      Left            =   3720
      TabIndex        =   25
      Top             =   4320
      Width           =   975
   End
   Begin VB.Label OnDuty 
      Alignment       =   2  'Center
      Caption         =   "0"
      Height          =   255
      Index           =   3
      Left            =   3720
      TabIndex        =   24
      Top             =   3840
      Width           =   975
   End
   Begin VB.Label OnDuty 
      Alignment       =   2  'Center
      Caption         =   "0"
      Height          =   255
      Index           =   2
      Left            =   3720
      TabIndex        =   23
      Top             =   3360
      Width           =   975
   End
   Begin VB.Label OnDuty 
      Alignment       =   2  'Center
      Caption         =   "0"
      Height          =   255
      Index           =   1
      Left            =   3720
      TabIndex        =   22
      Top             =   2880
      Width           =   975
   End
   Begin VB.Label OnDuty 
      Alignment       =   2  'Center
      Caption         =   "0"
      Height          =   255
      Index           =   0
      Left            =   3720
      TabIndex        =   21
      Top             =   2400
      Width           =   975
   End
   Begin VB.Label Label1 
      Caption         =   "Mon"
      Height          =   255
      Index           =   11
      Left            =   720
      TabIndex        =   20
      Top             =   2400
      Width           =   615
   End
   Begin VB.Label Label1 
      Caption         =   "Tue"
      Height          =   255
      Index           =   10
      Left            =   720
      TabIndex        =   19
      Top             =   2880
      Width           =   615
   End
   Begin VB.Label Label1 
      Caption         =   "Wed"
      Height          =   255
      Index           =   9
      Left            =   720
      TabIndex        =   18
      Top             =   3360
      Width           =   615
   End
   Begin VB.Label Label1 
      Caption         =   "Thu"
      Height          =   255
      Index           =   8
      Left            =   720
      TabIndex        =   17
      Top             =   3840
      Width           =   615
   End
   Begin VB.Label Label1 
      Caption         =   "Fri"
      Height          =   255
      Index           =   7
      Left            =   720
      TabIndex        =   16
      Top             =   4320
      Width           =   615
   End
   Begin VB.Label Label1 
      Caption         =   "Sat"
      Height          =   255
      Index           =   6
      Left            =   720
      TabIndex        =   15
      Top             =   4800
      Width           =   615
   End
   Begin VB.Label Label1 
      Caption         =   "Sun"
      Height          =   255
      Index           =   5
      Left            =   720
      TabIndex        =   14
      Top             =   5280
      Width           =   615
   End
   Begin VB.Label Label1 
      Alignment       =   2  'Center
      Caption         =   "Needs"
      Height          =   255
      Index           =   4
      Left            =   1440
      TabIndex        =   13
      Top             =   2040
      Width           =   975
   End
   Begin VB.Label Label1 
      Alignment       =   2  'Center
      Caption         =   "Start"
      Height          =   255
      Index           =   3
      Left            =   2640
      TabIndex        =   12
      Top             =   2040
      Width           =   855
   End
   Begin VB.Label Label1 
      Alignment       =   2  'Center
      Caption         =   "On Duty"
      Height          =   255
      Index           =   2
      Left            =   3720
      TabIndex        =   11
      Top             =   2040
      Width           =   975
   End
   Begin VB.Label Label1 
      Caption         =   "Total:"
      Height          =   255
      Index           =   1
      Left            =   1800
      TabIndex        =   6
      Top             =   5955
      Width           =   495
   End
   Begin VB.Label Label1 
      Caption         =   "Day"
      Height          =   255
      Index           =   0
      Left            =   720
      TabIndex        =   0
      Top             =   2040
      Width           =   495
   End
End
Attribute VB_Name = "Form1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
''
''    LINDO API Version 4.0
''    Copyright (c) 2000-2005
''
''    LINDO Systems, Inc.            312.988.7422
''    1415 North Dayton St.          info@lindo.com
''    Chicago, IL 60622              http://www.lindo.com

Option Explicit

Private Sub Exit_Click()
  End
End Sub

Private Sub Solve_Click()

Dim nErr As Long
Dim pEnv As Long

Dim LicenseKey As String * LS_MAX_ERROR_MESSAGE_LENGTH
  
nErr = LSloadLicenseString("..\..\..\license\lndapi140.lic", LicenseKey)
Call CheckErr(pEnv, nErr)

  '>>> Step 1 <<<:  Create a LINDO environment.
pEnv = LScreateEnv(nErr, LicenseKey)
If (nErr > 0) Then
   MsgBox ("Unable to create environment.")
   End
End If

'>>> Step 2 <<< create a model in the environment
Dim pMod As Long
pMod = LScreateModel(pEnv, nErr)
Call CheckErr(pEnv, nErr)

'>>> Step 3 <<< construct the model

'number of variables
Dim nVars As Long
nVars = 7

'number of constraints
Dim nRows As Long
nRows = 7

'direction of objective
Dim nDir As Long
nDir = LS_MIN

'objective constant term
Dim dObjConst As Double
dObjConst = 0

'objective coefficients
ReDim dObjCoef(nVars) As Double
Dim i As Integer
For i = 0 To nVars - 1
   dObjCoef(i) = 1
Next

'get the staffing needs for the model's right-hand sides
ReDim dB(nVars) As Double
For i = 0 To nVars - 1
   dB(i) = Needs(i)
Next

'define the constraint types
Dim cConTypes As String
For i = 0 To nRows - 1
  cConTypes = cConTypes & "G"
Next

'the number of nonzero coefficients
Dim nNZ As Long
nNZ = 35

'the array of column start indices
ReDim nBegCol(nVars + 1) As Long
For i = 0 To nVars
   nBegCol(i) = 5 * i
Next

'the nonzero coefficients
ReDim dA(nNZ) As Double
ReDim nRowX(nNZ) As Long
Dim j, k As Integer
   
k = 0
For i = 0 To nVars - 1
  For j = 0 To 4
    nRowX(k) = (j + i) Mod 7
    dA(k) = 1
    k = k + 1
  Next j
Next i

'load the problem
nErr = LSloadLPData(pMod, nRows, nVars, nDir, _
 dObjConst, dObjCoef(0), dB(0), cConTypes, nNZ, _
 nBegCol(0), ByVal 0, dA(0), nRowX(0), ByVal 0, _
 ByVal 0)
Call CheckErr(pEnv, nErr)

'integer restrictions on the variables
Dim cVarType As String
For i = 1 To nVars
  cVarType = cVarType & "I"
Next
nErr = LSloadVarType(pMod, cVarType)
Call CheckErr(pEnv, nErr)

nErr = LSsetMIPCallback(pMod, AddressOf MyCallback, ByVal 0)
Call CheckErr(pEnv, nErr)

'>>> Step 4 <<< solve the model
nErr = LSsolveMIP(pMod, ByVal 0)
Call CheckErr(pEnv, nErr)

'>>> Step 5 <<< retrieve the solution
ReDim dX(nVars) As Double
Dim dObj As Double
Dim dSlacks(7) As Double
nErr = LSgetInfo(pMod, LS_DINFO_MIP_OBJ, dObj)
Call CheckErr(pEnv, nErr)
nErr = LSgetMIPPrimalSolution(pMod, dX(0))
Call CheckErr(pEnv, nErr)
nErr = LSgetMIPSlacks(pMod, dSlacks(0))
Call CheckErr(pEnv, nErr)


'post solution in dialog box
Total = dObj
For i = 0 To nVars - 1
   OnDuty(i) = dB(i) - dSlacks(i)
   Start(i) = dX(i)
Next

End Sub

Public Sub CheckErr(pEnv As Long, nErr As Long)

' Checks for an error condition.  If one exists, the
'  error message is displayed then the application
'  terminates.

   If (nErr > 0) Then
      Dim cMessage As String
      cMessage = String(LS_MAX_ERROR_MESSAGE_LENGTH, _
       vbNullChar)
      Call LSgetErrorMessage(pEnv, nErr, cMessage)
      MsgBox (cMessage)
      End
   End If
   
End Sub


Private Sub Form_Load()
Dim szVernum As String * LS_MAX_ERROR_MESSAGE_LENGTH
Dim szBuildDate As String * LS_MAX_ERROR_MESSAGE_LENGTH
Call LSgetVersionInfo(szVernum, szBuildDate)

Label2.Caption = "LINDO API  Version  " & szVernum
Label3.Caption = "Built on  " & szBuildDate
End Sub

