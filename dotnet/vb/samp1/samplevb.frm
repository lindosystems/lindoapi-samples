VERSION 5.00
Begin VB.Form Form1 
   Caption         =   "Form1"
   ClientHeight    =   5055
   ClientLeft      =   5850
   ClientTop       =   4125
   ClientWidth     =   7470
   LinkTopic       =   "Form1"
   ScaleHeight     =   5055
   ScaleWidth      =   7470
   Begin VB.CommandButton Exit 
      Caption         =   "Exit"
      Height          =   615
      Left            =   3960
      TabIndex        =   3
      Top             =   4200
      Width           =   1335
   End
   Begin VB.CommandButton Command1 
      Caption         =   "Solve"
      Height          =   615
      Left            =   2160
      TabIndex        =   0
      Top             =   4200
      Width           =   1455
   End
   Begin VB.Label Label3 
      BorderStyle     =   1  'Fixed Single
      Height          =   855
      Left            =   480
      TabIndex        =   4
      Top             =   840
      Width           =   6615
   End
   Begin VB.Label Label2 
      BorderStyle     =   1  'Fixed Single
      Height          =   495
      Left            =   480
      TabIndex        =   2
      Top             =   240
      Width           =   6615
   End
   Begin VB.Label Label1 
      BackColor       =   &H80000009&
      BorderStyle     =   1  'Fixed Single
      BeginProperty Font 
         Name            =   "Courier New"
         Size            =   12
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   2055
      Left            =   1800
      TabIndex        =   1
      Top             =   1800
      Width           =   3975
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


'  A VB programming example of interfacing with the
'  LINDO API.
'
'  the problem:
'
'     Max = 20 * A + 30 * C
'     S.T.       A +  2 * C  <= 120
'                A           <=  60
'                         C  <=  50
'
'   Solving such a problem with the LINDO API involves
'   the following steps:
'
'      1. Create a LINDO environment.
'      2. Create a model in the environment.
'      3. Specify the model.
'      4. Perform the optimization.
'      5. Retrieve the solution.
'      6. Delete the LINDO environment.

Option Explicit

  
Private Sub Command1_Click()

  'Declarations
  Dim con_type As String
  Dim env As Long
  Dim errorcode As Long
  Dim i As Long
  Dim m As Long
  Dim n As Long
  Dim nz As Long
  Dim prob As Long
  Dim Abegcol() As Long
  Dim Arowndx() As Long
  Dim Acoef() As Double
  Dim b() As Double
  Dim c() As Double
  Dim obj As Double
  Dim x() As Double
  Dim LicenseKey As String * LS_MAX_ERROR_MESSAGE_LENGTH
  
  ' Name data
  Dim szTitle, szObjName, szRhsName, szRngName, szBndname As String
  Dim szConNames() As String
  Dim szVarNames() As String
  
  ' Auxiliary byte arrays for keeping variable and constraint names data for keeping
  Dim acConNames() As Byte
  Dim acVarNames() As Byte
  
  ' Pointer arrays for storing the address of each name within the byte arrays
  ' These pointers will be passed to LINDO API
  Dim pszConNames() As Long
  Dim pszVarNames() As Long
   
  
  '>>> Step 1 <<<:  Create a LINDO environment.
  errorcode = LSloadLicenseString("..\..\..\license\lndapi140.lic", LicenseKey)
  Call CheckErr(env, errorcode)

  env = LScreateEnv(errorcode, LicenseKey)
  If (errorcode > 0) Then
     MsgBox ("Unable to create environment.")
     End
  End If
      
      
  '>>> Step 2 <<<:  Create a model in the environment.
  prob = LScreateModel(env, errorcode)
  Call CheckErr(env, errorcode)
   
   
  '>>> Step 3 <<<:  Specify the model.
  
  'Set the problem sizes

  'number of constraints
  m = 3
  
  'number of variables
  n = 2

  'objective coefficients
  ReDim c(n)
  c(0) = 20
  c(1) = 30
  
  'right-hand-sides of constraints
  ReDim b(m)
  b(0) = 120
  b(1) = 60
  b(2) = 50

  'constraint types
  con_type = "LLL"
  
  'index of first nonzero in each column
  ReDim Abegcol(n + 1)
  Abegcol(0) = 0
  Abegcol(1) = 2
  Abegcol(2) = 4

  'number of nonzeros in constraint matrix
  nz = 4
  
  'the nonzero coefficients
  ReDim Acoef(nz)
  Acoef(0) = 1
  Acoef(1) = 1
  Acoef(2) = 2
  Acoef(3) = 1

  'the row indices of the nonzeros
  ReDim Arowndx(nz)
  Arowndx(0) = 0
  Arowndx(1) = 1
  Arowndx(2) = 0
  Arowndx(3) = 2
 
  ' Load LP data
  errorcode = LSloadLPData(prob, m, n, LS_MAX, 0, _
   c(0), b(0), con_type, nz, Abegcol(0), ByVal 0, _
   Acoef(0), Arowndx(0), ByVal 0, ByVal 0)
  Call CheckErr(env, errorcode)
    
  ' name data
  szTitle = "SAMP1"
  szObjName = "OBJ"
  szRhsName = "RHS"
  szRngName = "RNG"
  szBndname = "BND"
  
  ' local arrays for variable and constraint names
  ReDim szConNames(m)
  ReDim szVarNames(n)
  Dim szConNamesLen As Long, szVarNamesLen As Long
  
  szConNames(0) = "Cons0"
  szConNames(1) = "Cons1"
  szConNames(2) = "Cons2"
  For i = 0 To m - 1
    szConNamesLen = szConNamesLen + Len(szConNames(i)) + 1
  Next
  
  szVarNames(0) = "VarA"
  szVarNames(1) = "VarC"
  For i = 0 To n - 1
    szVarNamesLen = szVarNamesLen + Len(szVarNames(i)) + 1
  Next
    
  ' byte arrays to keep name data
  ReDim acConNames(szConNamesLen)
  ReDim acVarNames(szVarNamesLen)
          
  ' pointer arrays for keeping addresses of each name
  ' located in the byte arrays
  ReDim pszConNames(m)
  ReDim pszVarNames(n)

  ' parse string arrays to byte arrays and record pointers (source: Strutil.bas)
  Call NameToPtr(acConNames, pszConNames, szConNames, m)
  Call NameToPtr(acVarNames, pszVarNames, szVarNames, n)
    
  ' pass names
  'errorcode = LSloadNameData(prob, szTitle, szObjName, szRhsName, szRngName, szBndname, _
  pszConNames(0), pszVarNames(0), ByVal 0)
  Call CheckErr(env, errorcode)
    
  ' Export the model in LINDO File format
  Dim LindoFile As String
  LindoFile = "samp1.mps"
  Call LSwriteMPSFile(prob, LindoFile, LS_FORMATTED_MPS)
     
  '>>> Step 4 <<<:  Perform the optimization.
  errorcode = LSoptimize(prob, LS_METHOD_PSIMPLEX, ByVal 0)
  Call CheckErr(env, errorcode)
  
  '>>> Step 5 <<<:  Retrieve the solution.
  
  'Print the objective value and primals
  errorcode = LSgetInfo(prob, LS_DINFO_POBJ, obj)
  Call CheckErr(env, errorcode)
  
  ReDim x(n)
  errorcode = LSgetPrimalSolution(prob, x(0))
  Call CheckErr(env, errorcode)
  MsgBox ("Objective value: " & obj & vbCrLf & _
   "Primal values: A=" & x(0) & ", C=" & x(1))

  errorcode = LSsetModelIntParameter(prob, LS_IPARAM_SOL_REPORT_STYLE, 0)
  errorcode = LSwriteSolution(prob, "samp1.sol")
  
  Call LSdeleteModel(prob)

  '>>> Step 6 <<< Delete the LINDO environment.
  Call LSdeleteEnv(env)

End Sub

Public Sub CheckErr(env As Long, errorcode As Long)

' Checks for an error condition.  If one exists, the
'  error message is displayed then the application
'  terminates.

   If (errorcode > 0) Then
      Dim message As String
      message = String(LS_MAX_ERROR_MESSAGE_LENGTH, _
       vbNullChar)
      Call LSgetErrorMessage(env, errorcode, message)
      MsgBox (message)
      End
   End If
   
End Sub


Private Sub Exit_Click()
End
End Sub

Private Sub Form_Load()
Dim szVernum As String * LS_MAX_ERROR_MESSAGE_LENGTH
Dim szBuildDate As String * LS_MAX_ERROR_MESSAGE_LENGTH
Call LSgetVersionInfo(szVernum, szBuildDate)

Label2.Caption = "LINDO API  Version  " & szVernum
Label3.Caption = "Built on  " & szBuildDate
Label1.Caption = "Max = 20 A + 30 C         " & vbNewLine & vbNewLine & _
                 "S.T.     A +  2 C  <= 120 " & vbNewLine & _
                 "          A         <=  60 " & vbNewLine & _
                 "                 C  <=  50 " & vbNewLine & vbNewLine & _
                 "  A , C are nonnegative    "
End Sub





