VERSION 5.00
Begin VB.Form Form1 
   BackColor       =   &H0080C0FF&
   Caption         =   "Form1"
   ClientHeight    =   7080
   ClientLeft      =   5850
   ClientTop       =   4125
   ClientWidth     =   8640
   BeginProperty Font 
      Name            =   "Fixedsys"
      Size            =   9
      Charset         =   0
      Weight          =   400
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   LinkTopic       =   "Form1"
   ScaleHeight     =   7080
   ScaleWidth      =   8640
   Begin VB.Frame Frame3 
      BackColor       =   &H0080C0FF&
      Caption         =   "Solution"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   2415
      Index           =   0
      Left            =   360
      TabIndex        =   18
      Top             =   4320
      Width           =   3615
      Begin VB.Label Label13 
         BackColor       =   &H0080C0FF&
         Caption         =   "Next Best Obj:"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   255
         Left            =   240
         TabIndex        =   30
         Top             =   1920
         Width           =   1215
      End
      Begin VB.Label Label12 
         Alignment       =   1  'Right Justify
         BackColor       =   &H0080C0FF&
         Caption         =   "***"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   255
         Left            =   1605
         TabIndex        =   29
         Top             =   1920
         Width           =   1305
      End
      Begin VB.Label labInfeas 
         Alignment       =   1  'Right Justify
         BackColor       =   &H0080C0FF&
         Caption         =   "***"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   255
         Left            =   1605
         TabIndex        =   28
         Top             =   960
         Width           =   1305
      End
      Begin VB.Label Label7 
         BackColor       =   &H0080C0FF&
         Caption         =   "Infeasibility:"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   255
         Left            =   240
         TabIndex        =   27
         Top             =   960
         Width           =   975
      End
      Begin VB.Label Label11 
         BackColor       =   &H0080C0FF&
         Caption         =   "Best Bound:"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   255
         Left            =   240
         TabIndex        =   26
         Top             =   660
         Width           =   975
      End
      Begin VB.Label labBound 
         Alignment       =   1  'Right Justify
         BackColor       =   &H0080C0FF&
         Caption         =   "***"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   255
         Left            =   1600
         TabIndex        =   25
         Top             =   660
         Width           =   1300
      End
      Begin VB.Label labStat 
         Alignment       =   1  'Right Justify
         BackColor       =   &H0080C0FF&
         Caption         =   "***"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   255
         Left            =   1605
         TabIndex        =   24
         Top             =   1560
         Width           =   1305
      End
      Begin VB.Label labIter 
         Alignment       =   1  'Right Justify
         BackColor       =   &H0080C0FF&
         Caption         =   "***"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   255
         Left            =   1605
         TabIndex        =   23
         Top             =   1260
         Width           =   1305
      End
      Begin VB.Label Label17 
         BackColor       =   &H0080C0FF&
         Caption         =   "Status:"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   255
         Left            =   240
         TabIndex        =   22
         Top             =   1560
         Width           =   975
      End
      Begin VB.Label Label16 
         BackColor       =   &H0080C0FF&
         Caption         =   "Iterations:"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   255
         Left            =   240
         TabIndex        =   21
         Top             =   1260
         Width           =   975
      End
      Begin VB.Label labObj 
         Alignment       =   1  'Right Justify
         BackColor       =   &H0080C0FF&
         Caption         =   "***"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   255
         Left            =   1600
         TabIndex        =   20
         Top             =   360
         Width           =   1300
      End
      Begin VB.Label Label14 
         BackColor       =   &H0080C0FF&
         Caption         =   "Best Objective:"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   255
         Left            =   240
         TabIndex        =   19
         Top             =   360
         Width           =   1215
      End
   End
   Begin VB.Frame Frame2 
      BackColor       =   &H0080C0FF&
      Caption         =   "Model"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   2295
      Left            =   360
      TabIndex        =   7
      Top             =   1800
      Width           =   6135
      Begin VB.TextBox Text1 
         BeginProperty Font 
            Name            =   "Courier New"
            Size            =   9
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   375
         Left            =   1200
         TabIndex        =   8
         Top             =   240
         Width           =   4335
      End
      Begin VB.Label labNonz 
         Alignment       =   1  'Right Justify
         BackColor       =   &H0080C0FF&
         Caption         =   "***"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   255
         Left            =   1600
         TabIndex        =   17
         Top             =   1740
         Width           =   1300
      End
      Begin VB.Label labInt 
         Alignment       =   1  'Right Justify
         BackColor       =   &H0080C0FF&
         Caption         =   "***"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   255
         Left            =   1600
         TabIndex        =   16
         Top             =   1440
         Width           =   1300
      End
      Begin VB.Label labCon 
         Alignment       =   1  'Right Justify
         BackColor       =   &H0080C0FF&
         Caption         =   "***"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   255
         Left            =   1600
         TabIndex        =   15
         Top             =   1080
         Width           =   1305
      End
      Begin VB.Label Label10 
         BackColor       =   &H0080C0FF&
         Caption         =   "Nonzeros:"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   255
         Left            =   240
         TabIndex        =   14
         Top             =   1740
         Width           =   855
      End
      Begin VB.Label Label9 
         BackColor       =   &H0080C0FF&
         Caption         =   "Integers:"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   255
         Left            =   240
         TabIndex        =   13
         Top             =   1440
         Width           =   855
      End
      Begin VB.Label Label8 
         BackColor       =   &H0080C0FF&
         Caption         =   "Constraints:"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
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
         Top             =   1140
         Width           =   855
      End
      Begin VB.Label labVar 
         Alignment       =   1  'Right Justify
         BackColor       =   &H0080C0FF&
         Caption         =   "***"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   255
         Left            =   1600
         TabIndex        =   11
         Top             =   840
         Width           =   1300
      End
      Begin VB.Label Label6 
         BackColor       =   &H0080C0FF&
         Caption         =   "Variables:"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
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
         Top             =   840
         Width           =   855
      End
      Begin VB.Label Label5 
         BackColor       =   &H0080C0FF&
         Caption         =   "File Name"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   255
         Left            =   240
         TabIndex        =   9
         Top             =   360
         Width           =   855
      End
   End
   Begin VB.Frame Frame1 
      BackColor       =   &H0080C0FF&
      Caption         =   "LINDO API"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   1455
      Left            =   360
      TabIndex        =   2
      Top             =   120
      Width           =   7815
      Begin VB.Label Label4 
         BackColor       =   &H0080C0FF&
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   615
         Left            =   1080
         TabIndex        =   6
         Top             =   720
         Width           =   6495
      End
      Begin VB.Label Label3 
         BackColor       =   &H0080C0FF&
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   255
         Left            =   960
         TabIndex        =   5
         Top             =   360
         Width           =   6375
      End
      Begin VB.Label Label2 
         BackColor       =   &H0080C0FF&
         Caption         =   "Build date:"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   255
         Left            =   240
         TabIndex        =   4
         Top             =   720
         Width           =   855
      End
      Begin VB.Label Label1 
         BackColor       =   &H0080C0FF&
         Caption         =   "Version:"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   255
         Left            =   240
         TabIndex        =   3
         Top             =   360
         Width           =   615
      End
   End
   Begin VB.CommandButton cmdQuit 
      Caption         =   "Quit"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   615
      Left            =   6840
      TabIndex        =   1
      Top             =   2760
      Width           =   1335
   End
   Begin VB.CommandButton Command1 
      Caption         =   "Solve"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   615
      Left            =   6840
      TabIndex        =   0
      Top             =   1920
      Width           =   1335
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
'  LINDO API using external input files.
'
'      1. Create a LINDO environment.
'      2. Create a iModel in the environment.
'      3. Read the iModel from an input file.
'      4. Perform the optimization.
'      5. Retrieve the solution.
'      6. Delete the LINDO environment.
'
' Modified 12-11-2003 (MKA)

Option Explicit

Private Type UserData
    flgMIP As Long
End Type
      
Private Sub Command1_Click()

  'Declarations
  Dim iEnv As Long
  Dim iModel As Long
  
  Dim InputFile As String
  Dim errorcode As Long
  Dim obj As Double
  Dim x() As Double
  
  Dim LicenseKey As String * LS_MAX_ERROR_MESSAGE_LENGTH
  Dim szVarName As String * 255
  Dim szConName As String * 255
  Dim szTitle As String * 255
  Dim szObjName As String * 255
  Dim pszConNames() As Long
  Dim pszVarNames() As Long
  Dim acVarNames() As Byte
  Dim acConNames() As Byte
  Dim szConNames() As String
  Dim szVarNames() As String
  
  Dim nVars As Long
  Dim nCons As Long
  Dim nCont As Long
  Dim nNonz As Long
  Dim nStatus As Long
  Dim j As Long
  Dim myData As UserData
  
  'Step 0: init global variables and retrieve license string
  iEnv = 0
  iModel = 0
  errorcode = LSloadLicenseString("..\..\..\license\lndapi140.lic", LicenseKey)
  If (errorcode > 0) Then
      Call CheckErr(iEnv, errorcode)
      End
  End If
        

  'Step 1:  Create a LINDO environment using LicenseKey.
  iEnv = LScreateEnv(errorcode, LicenseKey)
  If (errorcode > 0) Then
     MsgBox ("Unable to create environment.")
     End
  End If
  
  'Step 2:  Create a iModel in the environment.
  iModel = LScreateModel(iEnv, errorcode)
  Call CheckErr(iEnv, errorcode)
   
  'Step 3:  Read the iModel and get iModel size.
  InputFile = Text1.Text
  errorcode = LSreadMPSFile(iModel, InputFile, LS_UNFORMATTED_MPS)
  Call CheckErr(iEnv, errorcode)
      
  errorcode = LSgetInfo(iModel, LS_IINFO_NUM_VARS, nVars)
  Call CheckErr(iEnv, errorcode)
  
  errorcode = LSgetInfo(iModel, LS_IINFO_NUM_CONS, nCons)
  Call CheckErr(iEnv, errorcode)
  
  errorcode = LSgetInfo(iModel, LS_IINFO_NUM_NONZ, nNonz)
  Call CheckErr(iEnv, errorcode)
  
  errorcode = LSgetInfo(iModel, LS_IINFO_NUM_CONT, nCont)
  
  labVar = nVars
  labCon = nCons
  labNonz = nNonz
  labInt = nVars - nCont
  cmdQuit.Caption = "Interrupt"
  
  
  'ReDim achConNames(nCons) As String * 255
  'ReDim achVarNames(nVars) As String * 255
  
  ' Invoke the MIP Solver
  If nCont < nVars Then
            
        ' Remove comments or add more as needed
        errorcode = LSsetModelDouParameter(iModel, LS_DPARAM_CALLBACKFREQ, 2)
        'errorcode = LSsetModelDouParameter(iModel, LS_DPARAM_MIP_OPTTOL, 0.5)
        'errorcode = LSsetModelIntParameter(iModel,LS_IPARAM_MIP_TOPOPT,1)
        'errorcode = LSsetModelIntParameter(iModel,LS_IPARAM_MIP_PRELEVEL,0)
        'errorcode = LSsetModelIntParameter(iModel,LS_IPARAM_MIP_ITRLIM,100)
        'errorcode = LSsetModelIntParameter(iModel,LS_IPARAM_NLP_ITRLMT,2000)
        'errorcode = LSsetModelIntParameter(iModel,LS_IPARAM_LP_ITRLMT,2000)

        'MsgBox ("Reading MIP done..." & vbCrLf & "Click OK to solve")
        
        myData.flgMIP = True
        
        'Set callback function
        errorcode = LSsetCallback(iModel, AddressOf GeneralCallback, myData)
        
        ' Step 4:  Perform MIP optimization.
        errorcode = LSsolveMIP(iModel, nStatus)
        Call CheckErr(iEnv, errorcode)
        
        ' Step 5:  If optimal, retrieve the solution.
        If (nStatus = LS_STATUS_OPTIMAL Or _
            nStatus = LS_STATUS_FEASIBLE Or _
            nStatus = LS_STATUS_BASIC_OPTIMAL) Then
            'Get the objective value and primals
            errorcode = LSgetInfo(iModel, LS_DINFO_MIP_OBJ, obj)
            Call CheckErr(iEnv, errorcode)
            ReDim x(nVars)
            errorcode = LSgetMIPPrimalSolution(iModel, x(0))
            Call CheckErr(iEnv, errorcode)
        End If
        
        ' Get K-Best solutions (K=3)
        errorcode = LSsetCallback(iModel, vbNullString, myData)
        errorcode = LSgetKBestMIPSols(iModel, "best.sol", AddressOf NextMIPCallback, myData, 10)
        
  ' Invoke the LP Solver
  Else
  
        'MsgBox ("Reading LP done..." & vbCrLf & "Click OK to solve")
        
        ' Remove comments or add more as needed
        errorcode = LSsetModelDouParameter(iModel, LS_DPARAM_CALLBACKFREQ, 1)
        
        'Set callback function
        errorcode = LSsetCallback(iModel, AddressOf GeneralCallback, myData)
                    
        cmdQuit.Caption = "Interrupt"
                    
        ' Step 4:  Perform the optimization.
        errorcode = LSoptimize(iModel, LS_METHOD_PSIMPLEX, nStatus)
        Call CheckErr(iEnv, errorcode)
        
        ' Step 5:  If optimal, retrieve the solution.
        If (nStatus = LS_STATUS_OPTIMAL Or _
            nStatus = LS_STATUS_FEASIBLE Or _
            nStatus = LS_STATUS_BASIC_OPTIMAL) Then
            'Get the objective value and primals
            errorcode = LSgetInfo(iModel, LS_DINFO_POBJ, obj)
            Call CheckErr(iEnv, errorcode)
            ReDim x(nVars)
            errorcode = LSgetPrimalSolution(iModel, x(0))
            Call CheckErr(iEnv, errorcode)
        End If
        
  End If
  
  cmdQuit.Caption = "Quit"
      
  MsgBox ("Best objective value: " & obj)
        
  Dim nNameLenVar As Long
  Dim nNameLenCon As Long
  errorcode = LSgetInfo(iModel, LS_IINFO_LEN_VARNAMES, nNameLenVar)
  errorcode = LSgetInfo(iModel, LS_IINFO_LEN_CONNAMES, nNameLenCon)
  ReDim acVarNames(0 To nNameLenVar - 1)
  ReDim acConNames(0 To nNameLenCon - 1)
  ReDim pszConNames(0 To nCons - 1)
  ReDim pszVarNames(0 To nVars - 1)
  ReDim szConNames(0 To nCons - 1)
  ReDim szVarNames(0 To nVars - 1)
  
  errorcode = LSgetNameData(iModel, szTitle, szObjName, _
              vbNullString, vbNullString, vbNullString, _
              pszConNames(0), acConNames(0), pszVarNames(0), _
              acVarNames(0))
              
  Call PtrToName(acVarNames, pszVarNames, szVarNames, nVars)
  Call PtrToName(acConNames, pszConNames, szConNames, nCons)
              
  
  For j = 0 To nVars - 1
  '  errorcode = LSgetVariableNamej(iModel, j, szVarName)
  '  Debug.Print (szVarName)
  '  Debug.Print (x(j))
  Debug.Print szVarNames(j)
  Next j
  
  For j = 0 To nCons - 1
  '  errorcode = LSgetVariableNamej(iModel, j, szVarName)
  '  Debug.Print (szVarName)
  '  Debug.Print (x(j))
  Debug.Print szConNames(j)
  Next j
      
  ' errorcode = LSsetModelIntParameter(iModel, LS_IPARAM_SOL_REPORT_STYLE, 1)
  ' errorcode = LSwriteSolution(iModel, "out_1.txt")
   
   errorcode = LSsetModelIntParameter(iModel, LS_IPARAM_SOL_REPORT_STYLE, 0)
   errorcode = LSwriteSolution(iModel, "out_0.txt")
   
  
  'Step 6: Delete the LINDO environment.
  Call LSdeleteEnv(iEnv)

End Sub

Public Sub CheckErr(iEnv As Long, errorcode As Long)

' Checks for an error condition.  If one exists, the
'  error message is displayed then the application
'  terminates.

   If (errorcode > 0) Then
      Dim Message As String
      Message = String(LS_MAX_ERROR_MESSAGE_LENGTH, _
       vbNullChar)
      errorcode = LSgetErrorMessage(iEnv, errorcode, Message)
      MsgBox (Message)
      'End
   End If
   
End Sub


Public Sub cmdQuit_Click()
    Unload Form1
End Sub

Private Sub Form_Load()
    Dim errorcode As Long
    Dim szVersion As String * 255
    Dim szBuiltOn As String * 255
    
    '' display Lindo API version
    Call LSgetVersionInfo(szVersion, szBuiltOn)
    Label3.Caption = szVersion
    Label4.Caption = szBuiltOn
    Text1.Text = "..\..\..\samples\data\bm23.mps"
End Sub



