VERSION 5.00
Begin VB.Form Form1 
   BackColor       =   &H00C0E0FF&
   Caption         =   "Form1"
   ClientHeight    =   7290
   ClientLeft      =   5850
   ClientTop       =   4125
   ClientWidth     =   8430
   LinkTopic       =   "Form1"
   ScaleHeight     =   7290
   ScaleWidth      =   8430
   Begin VB.Frame Frame1 
      BackColor       =   &H00C0E0FF&
      Caption         =   "LINDO API"
      Height          =   5415
      Left            =   480
      TabIndex        =   2
      Top             =   360
      Width           =   7215
      Begin VB.Frame Frame4 
         BackColor       =   &H00C0E0FF&
         Caption         =   "Progress"
         Height          =   1575
         Left            =   240
         TabIndex        =   18
         Top             =   3600
         Width           =   3255
         Begin VB.Label Label19 
            Alignment       =   2  'Center
            Caption         =   "Iters"
            BeginProperty Font 
               Name            =   "Courier New"
               Size            =   8.25
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   255
            Left            =   120
            TabIndex        =   22
            Top             =   360
            Width           =   1455
         End
         Begin VB.Label Label18 
            Alignment       =   1  'Right Justify
            Caption         =   "?"
            BeginProperty Font 
               Name            =   "Courier New"
               Size            =   8.25
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   255
            Left            =   1680
            TabIndex        =   21
            Top             =   360
            Width           =   1395
         End
         Begin VB.Label Label17 
            Alignment       =   2  'Center
            Caption         =   "Evals"
            BeginProperty Font 
               Name            =   "Courier New"
               Size            =   8.25
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   255
            Left            =   120
            TabIndex        =   20
            Top             =   720
            Width           =   1455
         End
         Begin VB.Label Label16 
            Alignment       =   1  'Right Justify
            Caption         =   "?"
            BeginProperty Font 
               Name            =   "Courier New"
               Size            =   8.25
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   255
            Left            =   1680
            TabIndex        =   19
            Top             =   720
            Width           =   1395
         End
      End
      Begin VB.Frame Frame3 
         BackColor       =   &H00C0E0FF&
         Caption         =   "Solution"
         Height          =   1575
         Left            =   3600
         TabIndex        =   9
         Top             =   3600
         Width           =   3375
         Begin VB.Label Label13 
            Alignment       =   1  'Right Justify
            Caption         =   "?"
            BeginProperty Font 
               Name            =   "Courier New"
               Size            =   8.25
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   255
            Left            =   1800
            TabIndex        =   17
            Top             =   1080
            Width           =   1400
         End
         Begin VB.Label Label12 
            Alignment       =   2  'Center
            Caption         =   "y"
            BeginProperty Font 
               Name            =   "Courier New"
               Size            =   8.25
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   255
            Left            =   240
            TabIndex        =   16
            Top             =   1080
            Width           =   1455
         End
         Begin VB.Label Label9 
            Alignment       =   1  'Right Justify
            Caption         =   "?"
            BeginProperty Font 
               Name            =   "Courier New"
               Size            =   8.25
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   255
            Left            =   1800
            TabIndex        =   13
            Top             =   720
            Width           =   1400
         End
         Begin VB.Label Label8 
            Alignment       =   2  'Center
            Caption         =   "x"
            BeginProperty Font 
               Name            =   "Courier New"
               Size            =   8.25
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   255
            Left            =   240
            TabIndex        =   12
            Top             =   720
            Width           =   1455
         End
         Begin VB.Label Label7 
            Alignment       =   1  'Right Justify
            Caption         =   "?"
            BeginProperty Font 
               Name            =   "Courier New"
               Size            =   8.25
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   255
            Left            =   1800
            TabIndex        =   11
            Top             =   360
            Width           =   1395
         End
         Begin VB.Label Label5 
            Alignment       =   2  'Center
            Caption         =   "f(x,y)"
            BeginProperty Font 
               Name            =   "Courier New"
               Size            =   8.25
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   255
            Left            =   240
            TabIndex        =   10
            Top             =   360
            Width           =   1455
         End
      End
      Begin VB.Frame Frame2 
         BackColor       =   &H00C0E0FF&
         Caption         =   "Nonlinear Model"
         Height          =   2055
         Left            =   240
         TabIndex        =   7
         Top             =   1320
         Width           =   6735
         Begin VB.Label Label6 
            BackColor       =   &H00C0E0FF&
            BeginProperty Font 
               Name            =   "Courier New"
               Size            =   8.25
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   1335
            Left            =   240
            TabIndex        =   8
            Top             =   600
            Width           =   6375
         End
      End
      Begin VB.Label Label4 
         BackColor       =   &H00C0E0FF&
         Height          =   375
         Left            =   1080
         TabIndex        =   6
         Top             =   720
         Width           =   5775
      End
      Begin VB.Label Label3 
         BackColor       =   &H00C0E0FF&
         Height          =   255
         Left            =   960
         TabIndex        =   5
         Top             =   360
         Width           =   1455
      End
      Begin VB.Label Label2 
         BackColor       =   &H00C0E0FF&
         Caption         =   "Build date:"
         Height          =   255
         Left            =   240
         TabIndex        =   4
         Top             =   720
         Width           =   855
      End
      Begin VB.Label Label1 
         BackColor       =   &H00C0E0FF&
         Caption         =   "Version:"
         Height          =   255
         Left            =   240
         TabIndex        =   3
         Top             =   360
         Width           =   615
      End
   End
   Begin VB.CommandButton Command2 
      BackColor       =   &H00C0C0FF&
      Caption         =   "Quit"
      Height          =   615
      Left            =   4440
      TabIndex        =   1
      Top             =   6240
      Width           =   1335
   End
   Begin VB.CommandButton Command1 
      BackColor       =   &H00C0C0FF&
      Caption         =   "Solve"
      Height          =   615
      Left            =   2280
      TabIndex        =   0
      Top             =   6240
      Width           =   1335
   End
   Begin VB.Label Label11 
      Caption         =   "Label5"
      Height          =   255
      Left            =   2520
      TabIndex        =   15
      Top             =   5160
      Width           =   1455
   End
   Begin VB.Label Label10 
      Caption         =   "Label5"
      Height          =   255
      Left            =   960
      TabIndex        =   14
      Top             =   5160
      Width           =   1455
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


'      Set up and solve the following nonlinear model using LINDO API's
'      multi-start nonlinear optimizer.
'
'      minimize  f(x,y) =  3*(1-x).^2.*exp(-(x.^2) - (y+1).^2) ...
'                       - 10*(x/5 - x.^3 - y.^5).*exp(-x.^2-y.^2) ...
'                       - 1/3*exp(-(x+1).^2 - y.^2);
'      subject to
'                       x^2 + y   <=  6;
'                       x   + y^2 <=  6;
'
'      1. Create an environment and a model space
'      2. Specify and load the LP portion of the model
'      3. Specify and load the NLP portion of the model
'      4. Specify the function evaluator function
'      5. Specify solver options and optimize
'      6. Terminate

Option Explicit
  
Private Sub Command1_Click()

  Dim env As Long
  Dim model As Long
  Dim errorcode As Long
  Dim Alencol(2) As Long
  Dim Arowndx(4) As Long
  Dim Nobjndx(1) As Long
  Dim Abegcol(3) As Long
  Dim A(4) As Double
  Dim Nnlobj As Double
  'Declarations
  Dim contype As String
  Dim i As Long
  Dim m As Long
  Dim n As Long
  Dim nz As Long
  Dim rhs(2) As Double
  Dim cost(2) As Double
  Dim obj As Double
  Dim x(3) As Double
  Dim LB(2) As Double
  Dim UB(2) As Double
  Dim LicenseKey As String * LS_MAX_ERROR_MESSAGE_LENGTH
  Dim nStatus As Long
  Dim niter As Long
  Dim biter As Long
  Dim siter  As Long
  Dim iter As Long


  Evals = 0

'*****************************************************************
'* Step 1: Create an environment and a model space
'*****************************************************************/
errorcode = LSloadLicenseString("..\..\..\license\lndapi140.lic", LicenseKey)
Call CheckErr(env, errorcode)

env = LScreateEnv(errorcode, LicenseKey)
Call CheckErr(env, errorcode)

model = LScreateModel(env, errorcode)
Call CheckErr(env, errorcode)


'*****************************************************************
'* Step 2: Specify and load the LP portion of the model
'*****************************************************************/
m = 2
n = 2
nz = 4
Abegcol(0) = 0
Abegcol(1) = 2
Abegcol(2) = 4

Alencol(0) = 2
Alencol(1) = 2

Arowndx(0) = 0
Arowndx(1) = 1
Arowndx(2) = 0
Arowndx(3) = 1

A(0) = 0
A(1) = 1
A(2) = 1
A(3) = 0

cost(0) = 0
cost(1) = 0

LB(0) = -3
UB(0) = 3
LB(1) = -3
UB(1) = 3

rhs(0) = 0
rhs(1) = 0

contype = "LL"


errorcode = LSloadLPData(model, m, n, LS_MIN, 0, _
   cost(0), rhs(0), contype, nz, Abegcol(0), Alencol(0), _
   A(0), Arowndx(0), LB(0), UB(0))
Call CheckErr(env, errorcode)



'*****************************************************************
'* Step 3: Specify and load the NLP portion of the model
'*****************************************************************/
'/* The number of nonlinear variables in each column */
Alencol(0) = 1
Alencol(1) = 1
'/* The indices of the first nonlinear variable in each column */
Abegcol(0) = 0
Abegcol(1) = 1
Abegcol(2) = 2
'/* The indices of nonlinear constraints */
Arowndx(0) = 0
Arowndx(1) = 1
'/* The indices of variables that are nonlinear in the objective*/
Nobjndx(0) = 0
Nobjndx(1) = 1
'/* Number nonlinear variables in cost. */
Nnlobj = 2

'/* Load the nonlinear structure */
errorcode = LSloadNLPData(model, Abegcol(0), Alencol(0), _
ByVal 0, Arowndx(0), Nnlobj, Nobjndx(0), ByVal 0)
Call CheckErr(env, errorcode)

'*****************************************************************
'* Step 4: Specify the function evaluator function
'*****************************************************************/
errorcode = LSsetFuncalc(model, AddressOf fdeCallback, ByVal 0)
Call CheckErr(env, errorcode)

'*****************************************************************
'* Step 5: Specify solver options and optimize
'*****************************************************************/
' Turn multistart search on
errorcode = LSsetModelIntParameter(model, LS_IPARAM_NLP_SOLVER, LS_NMETHOD_MSW_GRG)
Call CheckErr(env, errorcode)

' Set maximum number of local optimizations
errorcode = LSsetModelIntParameter(model, LS_IPARAM_NLP_MAXLOCALSEARCH, 5)
Call CheckErr(env, errorcode)

errorcode = LSoptimize(model, LS_METHOD_FREE, nStatus)
Call CheckErr(env, errorcode)

errorcode = LSgetInfo(model, LS_DINFO_POBJ, obj)
Call CheckErr(env, errorcode)

errorcode = LSgetPrimalSolution(model, x(0))
Call CheckErr(env, errorcode)

errorcode = LSgetInfo(model, LS_IINFO_NLP_ITER, niter)
errorcode = LSgetInfo(model, LS_IINFO_SIM_ITER, siter)
errorcode = LSgetInfo(model, LS_IINFO_BAR_ITER, biter)
iter = niter + biter + siter
Call CheckErr(env, errorcode)


Form1.Label7 = Format(obj, "######0.00000000")
Form1.Label9 = Format(x(0), "######0.00000000")
Form1.Label13 = Format(x(1), "######0.00000000")
Form1.Label18 = Format(iter)

'*****************************************************************
'* Step 6: Terminate
'*****************************************************************/
LSdeleteModel (model)
LSdeleteEnv (env)

End Sub

Public Sub CheckErr(env As Long, errorcode As Long)

' Checks for an error condition.  If one exists, the
'  error message is displayed then the application
'  terminates.

   If (errorcode > 0) Then
      Dim message As String
      message = String(LS_MAX_ERROR_MESSAGE_LENGTH, _
       vbNullChar)
      errorcode = LSgetErrorMessage(env, errorcode, message)
      MsgBox (message)
      End
   End If
   
End Sub


Private Sub Command2_Click()
    Unload Form1
    End
End Sub

Private Sub Form_Load()
    Dim errorcode As Long
    Dim szVersion As String * 255
    Dim szBuiltOn As String * 255
    
    '' display Lindo API version
    Call LSgetVersionInfo(szVersion, szBuiltOn)
    Label3.Caption = szVersion
    Label4.Caption = szBuiltOn
    Label6.Caption = "Minimize  f(x,y) =  3*(1-x).^2.*exp(-(x.^2) - (y+1).^2)" & vbCr & _
                     "                   - 10*(x/5 - x.^3 - y.^5).*exp(-x.^2-y.^2) " & vbCr & _
                     "                   - 1/3*exp(-(x+1).^2 - y.^2) " & vbCr & _
                     "Subject to  " & vbCr & _
                     "                   x ^ 2 + y     <= 6 " & vbCr & _
                     "                   x     + y ^ 2 <= 6 "
End Sub

