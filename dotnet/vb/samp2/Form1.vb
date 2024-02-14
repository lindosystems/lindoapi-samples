'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
''
''    LINDO API Version 4.0
''    Copyright (c) 2000-2005
''
''    LINDO Systems, Inc.            312.988.7422
''    1415 North Dayton St.          info@lindo.com
''    Chicago, IL 60622              http://www.lindo.com
''
''    @samp2.vb  
''
''    last updated: 1-31-2003
''
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''/
Option Explicit On 

Imports System
Imports System.IO
Imports System.Text
Imports System.Runtime.InteropServices
Imports Microsoft.VisualBasic


Public Class Form1
    Inherits System.Windows.Forms.Form



#Region " Windows Form Designer generated code "

    Public Sub New()
        MyBase.New()

        'This call is required by the Windows Form Designer.
        InitializeComponent()

        'Add any initialization after the InitializeComponent() call

    End Sub

    'Form overrides dispose to clean up the component list.
    Protected Overloads Overrides Sub Dispose(ByVal disposing As Boolean)
        If disposing Then
            If Not (components Is Nothing) Then
                components.Dispose()
            End If
        End If
        MyBase.Dispose(disposing)
    End Sub

    'Required by the Windows Form Designer
    Private components As System.ComponentModel.IContainer

    'NOTE: The following procedure is required by the Windows Form Designer
    'It can be modified using the Windows Form Designer.  
    'Do not modify it using the code editor.
    Friend WithEvents Button1 As System.Windows.Forms.Button
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.Button1 = New System.Windows.Forms.Button()
        Me.SuspendLayout()
        '
        'Button1
        '
        Me.Button1.Location = New System.Drawing.Point(80, 160)
        Me.Button1.Name = "Button1"
        Me.Button1.Size = New System.Drawing.Size(128, 40)
        Me.Button1.TabIndex = 0
        Me.Button1.Text = "Solve"
        '
        'Form1
        '
        Me.AutoScaleBaseSize = New System.Drawing.Size(5, 13)
        Me.ClientSize = New System.Drawing.Size(292, 273)
        Me.Controls.AddRange(New System.Windows.Forms.Control() {Me.Button1})
        Me.Name = "Form1"
        Me.Text = "Form1"
        Me.ResumeLayout(False)

    End Sub

#End Region
    Private Sub CheckErr(ByVal env As IntPtr, ByVal errorcode As Integer)

        ' Checks for an error condition.  If one exists, the
        '  error message is displayed then the application
        '  terminates.

        If (errorcode > 0) Then
            Dim message As New StringBuilder(1024)
            Call Lindo.LSgetErrorMessage(env, errorcode, message)
            MsgBox(message.ToString())
            End
        End If

    End Sub

    Private Sub Label1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)

    End Sub

    Public Sub Button1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button1.Click

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

        'Declarations
        Static Dim con_type As String
        Static Dim var_type As String
        Dim env As IntPtr
        Dim errorcode, i, j As Integer
        Dim nCons As Integer
        Dim nVars As Integer
        Dim nz As Integer
        Dim prob As IntPtr
        Dim Abegcol() As Integer
        Dim Arowndx() As Integer
        Dim Acoef() As Double
        Dim Acolcnt() As Integer
        Dim l(), u() As Double
        Dim b() As Double
        Dim c() As Double
        Dim obj As Double
        Dim x() As Double
        Dim LicenseKey As New StringBuilder(lindo.LS_MAX_ERROR_MESSAGE_LENGTH)
        Dim Afull(,) As Double = { _
        {-1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, _
        {-1.06, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, _
        {1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, _
        {0, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1.4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, _
        {0, 0, 0, 0, -1, -1, -1, -1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, _
        {0, 0, 0, 0, -1.06, -1.06, -0.96, -0.86, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, _
        {0, 0, 0, 0, 1, 0, 0, 0, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, _
        {0, 0, 0, 0, 0, 1, 0, 0, 0, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, _
        {0, 0, 0, 0, 0, 0, 1, 0, 0, 0, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, _
        {0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, _
        {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, _
        {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -0.43, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, _
        {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, _
        {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1.4, 0, 0, 0}, _
        {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -0.43, -0.43, -0.39, -0.37, 0, 0, 0, 0, 0, 0, 1, 0}, _
        {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, -1, 1, 0, 1}, _
        {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, -1, 0, 0, 0, 0, 0, 0, 0}, _
        {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, -1, 0, 0, 0, 0, 0, 0}, _
        {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, -1, 0, 0, 0, 0, 0}, _
        {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, -1, 0, 0, 0, 0}, _
        {0, 0, 0, 0, 0, 0, 0, 0, 2.364, 2.386, 2.408, 2.429, 0, 0, 0, 0, 0, 0, -1, 0, 0, 0, 0, 0, 2.191, 2.219, 2.249, 2.279, 0, 0, 0, 0}, _
        {0, 0, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.109, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, _
        {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, 0, 0, 0, 0, 0, 0, 0.109, 0.108, 0.108, 0.107, 0, 0, 0, 0, 0, 0, 0, 0}, _
        {0.301, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, _
        {0, 0, 0, 0, 0.301, 0.313, 0.313, 0.326, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, 0, 0}, _
        {0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, _
        {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0} _
        }





        ' read license key
        errorcode = Lindo.LSloadLicenseString("..\\..\\..\\..\\..\\license\\lndapi150.lic", LicenseKey)
        If (errorcode > 0) Then
            MsgBox("License file does not exist.")
            End
        End If
        

        '>>> Step 1 <<<:  Create a LINDO environment.
        env = Lindo.LScreateEnv(errorcode, LicenseKey.ToString())
        If (errorcode > 0) Then
            MsgBox("Unable to create environment.")
            End
        End If

        '>>> Step 2 <<<:  Create a model in the environment.
        prob = Lindo.LScreateModel(env, errorcode)
        Call CheckErr(env, errorcode)

        '>>> Step 3 <<<:  Specify the model.

        'Set the problem sizes

        'number of constraints
        nCons = 27

        'number of variables
        nVars = 32

        'objective coefficients
        ReDim c(nVars)
        var_type = ""
        For i = 0 To nVars - 1
            c(i) = -10
            If (i < 10) Then
                var_type = var_type + "I"  ' integer variable
            Else
                var_type = var_type + "C"  ' continuous variables
            End If
        Next


        'right-hand-sides of constraints
        ReDim b(nCons)
        con_type = ""        
        For i = 0 To nCons - 1
            b(i) = 100
            con_type = con_type + "G"
        Next



        'index of first nonzero in each column
        ReDim Abegcol(nVars + 1)

        ReDim Acolcnt(nVars)

        ReDim l(nVars)
        ReDim u(nVars)
        For i = 0 To nVars - 1
            l(i) = 0.0
            u(i) = lindo.LS_INFINITY
        Next


        'number of nonzeros in constraint matrix  
        nz = 0
        For j = 0 To nVars - 1
            For i = 0 To nCons - 1
                If (Math.Abs(Afull(i, j)) > 0.0000000000000001) Then
                    nz = nz + 1
                End If
            Next
        Next

        'the nonzero coefficients
        ReDim Acoef(nz)

        'the row indices of the nonzeros
        ReDim Arowndx(nz)

        'copy nonzero structure from Afull to Arowndx,Acoef,Abegcol
        nz = 0
        For j = 0 To nVars - 1
            Abegcol(j) = nz
            Acolcnt(j) = 0
            For i = 0 To nCons - 1
                If (Math.Abs(Afull(i, j)) > 0.0000000000000001) Then
                    Arowndx(nz) = i
                    Acoef(nz) = Afull(i, j)
                    Acolcnt(j) = Acolcnt(j) + 1
                    nz = nz + 1
                End If
            Next
        Next
        Abegcol(j) = nz

        ' Load LP data
        errorcode = lindo.LSloadLPData(prob, nCons, nVars, lindo.LS_MAX, 0, _
         c, b, con_type, nz, Abegcol, Acolcnt, _
         Acoef, Arowndx, l, u)
        Call CheckErr(env, errorcode)

        ' Load variable type (integer,continuous,binary)
        errorcode = lindo.LSloadVarType(prob, var_type)

        Dim LindoFile As String
        LindoFile = "lindo.ltx"
        Call lindo.LSwriteLINDOFile(prob, LindoFile)

        ' Declare callback function
        Dim cb As lindo.typCallback
        cb = AddressOf clsCallback.func

        ' Declate and initialize user data to pass to callback function
        Dim cbData As New CallbackData()
        cbData.count = 0

        ' allocate area in the unmanaged heap for user data
        Dim myData As New IntPtr()
        myData = Marshal.AllocHGlobal(Marshal.SizeOf(cbData))
        Marshal.StructureToPtr(cbData, myData, False)

        ' Set callback function along with user data (optional)
        errorcode = lindo.LSsetCallback(prob, cb, myData)
        Call CheckErr(env, errorcode)

        '>>> Step 4 <<<:  Perform the optimization.
        Dim nVarsCont As Integer
        lindo.LSgetInfo(prob, lindo.LS_IINFO_NUM_CONT, nVarsCont) ' count of continuous variables

        If nVarsCont = nVars Then
            errorcode = lindo.LSoptimize(prob, lindo.LS_METHOD_PSIMPLEX, 0)
        Else
            errorcode = lindo.LSsolveMIP(prob, 0)
        End If
        Call CheckErr(env, errorcode)

        '>>> Step 5 <<<:  Retrieve the solution.

        If (nVarsCont = nVars) Then
            'Print the objective value and primals      
            errorcode = lindo.LSgetInfo(prob, lindo.LS_DINFO_POBJ, obj)
            Call CheckErr(env, errorcode)
            ReDim x(nVars)
            errorcode = lindo.LSgetPrimalSolution(prob, x)
        Else
            'Print the objective value and primals      
            errorcode = lindo.LSgetInfo(prob, lindo.LS_DINFO_MIPOBJ, obj)
            Call CheckErr(env, errorcode)
            ReDim x(nVars)
            errorcode = lindo.LSgetMIPPrimalSolution(prob, x)
        End If

        Call CheckErr(env, errorcode)
        MsgBox("Objective value: " & obj & vbCrLf & _
         "Primal values: " & vbCrLf & x(0) & vbCrLf & x(1) & vbCrLf & "..." & vbCrLf & "...")

        ' marshal back to local area
        Marshal.PtrToStructure(myData, cbData)

        ' free unmanaged heap
        Marshal.FreeHGlobal(myData)

        '>>> Step 6 <<< Delete the LINDO environment.
        Call lindo.LSdeleteEnv(env)

    End Sub
End Class


<StructLayout(LayoutKind.Sequential)> _
Public Class CallbackData
    Public count As Integer
End Class




Public Class clsCallback
    Public Shared Function func(ByVal model As IntPtr, _
                                ByVal loc As Integer, _
                                ByVal cbData As IntPtr) As Integer
        Dim it, nErr As Integer
        Dim ob As Double
        Dim szMsg As String
        Dim szCount As String
        Dim md As New CallbackData()

        ' marshal user data from unmanaged code
        Marshal.PtrToStructure(cbData, md)

        ' update user data
        md.count = md.count + 1
        szCount = "Callback Count " & md.count

        ' get iterations
        nErr = Lindo.LSgetCallbackInfo(model, loc, Lindo.LS_IINFO_SIM_ITER, it)
        ' get primal objective
        nErr = Lindo.LSgetCallbackInfo(model, loc, Lindo.LS_DINFO_POBJ, ob)
        'display
        szMsg = szCount & vbCrLf & "Iteration: " _
         & it & vbCrLf & "Objective value:  " & ob
        MsgBox(szMsg)

        ' marshal user data to unmanaged code
        Marshal.StructureToPtr(md, cbData, False)

        func = 0

    End Function
End Class



