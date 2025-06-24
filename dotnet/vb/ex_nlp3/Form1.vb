'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'
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
'
'    @ex_nlp3.vb  
'
'    last updated: 08-05-2003
'
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
Option Explicit On 

Imports System
Imports System.IO
Imports System.Text
Imports System.Runtime.InteropServices
Imports Microsoft.VisualBasic

<StructLayout(LayoutKind.Sequential)> _
Public Class CallbackData    
    Public count As Integer
End Class


Public Class clsCallback
    Public Shared Function fde(ByVal model As IntPtr, _
                        ByVal cbData As IntPtr, _
                        ByVal nRow As Integer, _
                        ByVal adX As IntPtr, _
                        ByVal nJDiff As Integer, _
                        ByVal dXJBase As Double, _
                        ByRef adFuncVal As Double, _
                        ByVal pReserved As Integer) As Integer

        Dim f As Double, x As Double, y As Double
        Dim _adX(2) As Double

        ' make a local copy
        Marshal.Copy(adX, _adX, 0, 2)
        x = _adX(0)
        y = _adX(1)

        ' compute objective's functional value
        If nRow = -1 Then
            f = 3 * (1 - x) ^ 2 * Math.Exp(-(x ^ 2) - (y + 1) ^ 2) _
            - 10 * (x / 5 - x ^ 3 - y ^ 5) * Math.Exp(-(x ^ 2) - (y ^ 2)) _
            - 1 / 3 * Math.Exp(-((x + 1) ^ 2) - y ^ 2)

            ' compute constaint 0's functional value
        ElseIf nRow = 0 Then
            f = x * x + y - 6

            ' compute constaint 1's functional value
        ElseIf nRow = 1 Then
            f = x + y * y - 6

        End If

        ' pass f(x,y) back
        adFuncVal = f

        ' return a nonzero value to interrupt the solver
        fde = 0


    End Function



    Public Shared Function func(ByVal model As IntPtr, _
                                ByVal loc As Integer, _
                                ByVal cbData As IntPtr) As Integer
        Dim biter, niter, siter, nErr As Integer
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
        nErr = Lindo.LSgetCallbackInfo(model, loc, Lindo.LS_IINFO_SIM_ITER, siter)
        nErr = Lindo.LSgetCallbackInfo(model, loc, Lindo.LS_IINFO_NLP_ITER, niter)
        nErr = Lindo.LSgetCallbackInfo(model, loc, Lindo.LS_IINFO_BAR_ITER, biter)
        ' get primal objective
        nErr = Lindo.LSgetCallbackInfo(model, loc, Lindo.LS_DINFO_POBJ, ob)
        'display
        szMsg = szCount & vbCrLf & "Iteration: " _
         & siter + niter + biter & vbCrLf & "Objective value:  " & ob
        MsgBox(szMsg)

        ' marshal user data to unmanaged code
        Marshal.StructureToPtr(md, cbData, False)

        func = 0

    End Function
End Class

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
    Friend WithEvents Button2 As System.Windows.Forms.Button
    Friend WithEvents label5 As System.Windows.Forms.Label
    Friend WithEvents label4 As System.Windows.Forms.Label
    Friend WithEvents label3 As System.Windows.Forms.Label
    Friend WithEvents label2 As System.Windows.Forms.Label
    Friend WithEvents label1 As System.Windows.Forms.Label
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.Button1 = New System.Windows.Forms.Button()
        Me.Button2 = New System.Windows.Forms.Button()
        Me.label5 = New System.Windows.Forms.Label()
        Me.label4 = New System.Windows.Forms.Label()
        Me.label3 = New System.Windows.Forms.Label()
        Me.label2 = New System.Windows.Forms.Label()
        Me.label1 = New System.Windows.Forms.Label()
        Me.SuspendLayout()
        '
        'Button1
        '
        Me.Button1.Location = New System.Drawing.Point(48, 192)
        Me.Button1.Name = "Button1"
        Me.Button1.Size = New System.Drawing.Size(88, 32)
        Me.Button1.TabIndex = 0
        Me.Button1.Text = "Solve"
        '
        'Button2
        '
        Me.Button2.Location = New System.Drawing.Point(160, 192)
        Me.Button2.Name = "Button2"
        Me.Button2.Size = New System.Drawing.Size(88, 32)
        Me.Button2.TabIndex = 1
        Me.Button2.Text = "Exit"
        '
        'label5
        '
        Me.label5.Location = New System.Drawing.Point(120, 152)
        Me.label5.Name = "label5"
        Me.label5.Size = New System.Drawing.Size(144, 16)
        Me.label5.TabIndex = 11
        '
        'label4
        '
        Me.label4.Location = New System.Drawing.Point(120, 128)
        Me.label4.Name = "label4"
        Me.label4.Size = New System.Drawing.Size(144, 16)
        Me.label4.TabIndex = 10
        '
        'label3
        '
        Me.label3.Location = New System.Drawing.Point(48, 152)
        Me.label3.Name = "label3"
        Me.label3.Size = New System.Drawing.Size(60, 16)
        Me.label3.TabIndex = 9
        Me.label3.Text = "Iterations:"
        '
        'label2
        '
        Me.label2.Location = New System.Drawing.Point(48, 128)
        Me.label2.Name = "label2"
        Me.label2.Size = New System.Drawing.Size(60, 16)
        Me.label2.TabIndex = 8
        Me.label2.Text = "Objective:"
        '
        'label1
        '
        Me.label1.BackColor = System.Drawing.Color.Transparent
        Me.label1.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.label1.Location = New System.Drawing.Point(56, 24)
        Me.label1.Name = "label1"
        Me.label1.Size = New System.Drawing.Size(184, 64)
        Me.label1.TabIndex = 7
        Me.label1.Text = "Nonlinear Optimization with"
        Me.label1.TextAlign = System.Drawing.ContentAlignment.TopCenter
        '
        'Form1
        '
        Me.AutoScaleBaseSize = New System.Drawing.Size(5, 13)
        Me.ClientSize = New System.Drawing.Size(292, 255)
        Me.Controls.AddRange(New System.Windows.Forms.Control() {Me.label5, Me.label4, Me.label3, Me.label2, Me.label1, Me.Button2, Me.Button1})
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
            Dim message As New StringBuilder(256)
            Call Lindo.LSgetErrorMessage(env, errorcode, message)
            MsgBox(message)
            End
        End If

    End Sub

    Private Sub Label1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)

    End Sub

    Public Sub Button1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button1.Click

        'Declarations
        Dim env As IntPtr
        Dim Model As IntPtr
        Dim errorcode As Integer
        Dim Alencol(2) As Integer
        Dim Arowndx(4) As Integer
        Dim Nobjndx(1) As Integer
        Dim Abegcol(3) As Integer
        Dim Acoef(4) As Double
        Dim Nnlobj As Integer
        'Declarations
        Static Dim contype As String
        Dim i As Integer
        Dim m As Integer
        Dim n As Integer
        Dim nz As Integer
        Dim rhs(2) As Double
        Dim cost(2) As Double
        Dim obj As Double
        Dim x(3) As Double
        Dim LB(2) As Double
        Dim UB(2) As Double
        Dim LicenseKey As New StringBuilder(lindo.LS_MAX_ERROR_MESSAGE_LENGTH)
        Dim status As Integer


        '*****************************************************************
        '* Step 1: Create an environment and a model space
        '****************************************************************
        errorcode = Lindo.LSloadLicenseString("../../../../../license/lndapi160.lic", LicenseKey)
        Call CheckErr(env, errorcode)

        env = Lindo.LScreateEnv(errorcode, LicenseKey.ToString())
        Call CheckErr(env, errorcode)

        Model = Lindo.LScreateModel(env, errorcode)
        Call CheckErr(env, errorcode)


        '*****************************************************************
        '* Step 2: Specify and load the LP portion of the model
        '****************************************************************
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

        Acoef(0) = 0
        Acoef(1) = 1
        Acoef(2) = 1
        Acoef(3) = 0

        cost(0) = 0
        cost(1) = 0

        LB(0) = -3
        UB(0) = 3
        LB(1) = -3
        UB(1) = 3

        rhs(0) = 0
        rhs(1) = 0

        contype = "LL"


        errorcode = Lindo.LSloadLPData(Model, m, n, Lindo.LS_MIN, 0, _
           cost, rhs, contype, nz, Abegcol, Alencol, _
           Acoef, Arowndx, LB, UB)
        Call CheckErr(env, errorcode)



        '*****************************************************************
        '* Step 3: Specify and load the NLP portion of the model
        '****************************************************************
        ' The number of nonlinear variables in each column 
        Alencol(0) = 1
        Alencol(1) = 1
        ' The indices of the first nonlinear variable in each column 
        Abegcol(0) = 0
        Abegcol(1) = 1
        Abegcol(2) = 2
        ' The indices of nonlinear constraints 
        Arowndx(0) = 0
        Arowndx(1) = 1
        ' The indices of variables that are nonlinear in the objective
        Nobjndx(0) = 0
        Nobjndx(1) = 1
        ' Number nonlinear variables in cost. 
        Nnlobj = 2

        ' Load the nonlinear structure 
        errorcode = lindo.LSloadNLPData(Model, Abegcol, Alencol, _
          0, Arowndx, Nnlobj, Nobjndx, 0)
        Call CheckErr(env, errorcode)

        '*****************************************************************
        '* Step 4: Specify the function evaluator function

        ' Declate and initialize user data to pass to callback function
        Dim cbData As New CallbackData()
        cbData.count = 0

        ' allocate area in the unmanaged heap for user data
        Dim myData As New IntPtr()
        myData = Marshal.AllocHGlobal(Marshal.SizeOf(cbData))
        Marshal.StructureToPtr(cbData, myData, False)

        ' Declare callback function for function evals
        Dim fde As Lindo.typFuncalc
        fde = AddressOf clsCallback.fde

        ' Set callback function along with user data (optional)
        errorcode = Lindo.LSsetFuncalc(Model, fde, 0)
        Call CheckErr(env, errorcode)


        'Declare callback function
        Dim cb As Lindo.typCallback
        cb = AddressOf clsCallback.func

        'Set callback function along with user data (optional)
        'errorcode = Lindo.LSsetCallback(Model, cb, myData)
        'Call CheckErr(env, errorcode)


        '*****************************************************************
        '* Step 5: Specify solver options and optimize
        '****************************************************************
        ' Turn multistart search on
        errorcode = lindo.LSsetModelIntParameter(Model, lindo.LS_IPARAM_NLP_SOLVER, lindo.LS_NMETHOD_MSW_GRG)
        Call CheckErr(env, errorcode)

        ' Set maximum number of local optimizations
        errorcode = lindo.LSsetModelIntParameter(Model, lindo.LS_IPARAM_NLP_MAXLOCALSEARCH, 5)
        Call CheckErr(env, errorcode)

        errorcode = Lindo.LSoptimize(Model, Lindo.LS_METHOD_FREE, status)
        Call CheckErr(env, errorcode)

        errorcode = Lindo.LSgetInfo(Model, Lindo.LS_DINFO_POBJ, obj)
        Call CheckErr(env, errorcode)

        errorcode = Lindo.LSgetInfo(Model, Lindo.LS_IINFO_NLP_ITER, i)
        Call CheckErr(env, errorcode)

        errorcode = Lindo.LSgetPrimalSolution(Model, x)
        Call CheckErr(env, errorcode)

        ' marshal back to local area
        Marshal.PtrToStructure(myData, cbData)

        ' free unmanaged heap
        Marshal.FreeHGlobal(myData)

        '*****************************************************************
        '* Step 6: Terminate
        '****************************************************************
        Lindo.LSdeleteModel(Model)
        Lindo.LSdeleteEnv(env)

        Me.label4.Text = obj.ToString()
        Me.label5.Text = i.ToString()

    End Sub

    Private Sub Button2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button2.Click
        Application.Exit()
    End Sub

    Private Sub Form1_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        Dim LibVersion As New StringBuilder(Lindo.LS_MAX_ERROR_MESSAGE_LENGTH)
        Dim LibBuilton As New StringBuilder(Lindo.LS_MAX_ERROR_MESSAGE_LENGTH)

        Lindo.LSgetVersionInfo(LibVersion, LibBuilton)

        Me.Text = "Solving a sample Nonlinear Model."
        Me.label1.Text = Me.label1.Text & vbCrLf & vbCrLf & " LINDO API " & vbCrLf & vbCrLf & "Version " & LibVersion.ToString()
    End Sub
End Class





