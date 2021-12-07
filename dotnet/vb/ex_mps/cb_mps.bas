Attribute VB_Name = "Callback"
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
''
'' LINDO API Version 4.0
'' Copyright (c) 2006
''
'' The PeekMessage function is a WIN API function used to
'' check the thread message queue for a message and to
'' place the message (if any) in the specified structure.
'' Use of this function is essential for the implementation
'' of $Interrupt$ button.
''
'' Modified 12-11-2003 (MKA)


Private Const PM_REMOVE = &H1
Private Const PM_NOREMOVE = &H0

Private Type POINTAPI
    x As Long
    y As Long
End Type

Private Type Msg
    hWnd As Long
    Message As Long
    wParam As Long
    lParam As Long
    time As Long
    pt As POINTAPI
End Type


Private Declare Function PeekMessage Lib "user32" _
Alias "PeekMessageA" (lpMsg As Msg, _
                      ByVal hWnd As Long, _
                      ByVal wMsgFilterMin As Long, _
                      ByVal wMsgFilterMax As Long, _
                      ByVal wRemoveMsg As Long) As Long
                      
'' User structure carries a single flag indicating
'' if the underlying model is a MIP. This variable
'' was passed to LINDO API during LSsetCallback()
'' call.
Private Type UserData
    flgMIP As Long
End Type

'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
''
''    LINDO API Version 4.0
''    Copyright (c) 2006
''
''    LINDO Systems, Inc.            312.988.7422
''    1415 North Dayton St.          info@lindo.com
''    Chicago, IL 60622              http://www.lindo.com
''
''    Modified 12-11-2003 (MKA)

Public Function GeneralCallback(ByVal model As Long, _
 ByVal nLoc As Long, ByRef myData As UserData) As Long
 
    Dim nErr As Integer
    Dim it As Integer
    Dim status As Integer
    Dim obj As Double
    Dim bound As Double
    Dim pinfeas As Double
    Dim Message As Msg
    Dim flgPeek As Long
    Dim fmtDouble As String
    Dim fmtLong As String
    
    GeneralCallback = 0
    fmtDouble = "##,##0.00"
    fmtLong = "##,##0"
    
    If myData.flgMIP Then
        
        '' get callback data for MIPs
        
        nErr = LSgetMIPCallbackInfo(model, LS_IINFO_MIP_SIM_ITER, it)
        If nErr = LSERR_NO_ERROR Then
          Form1.labIter = Format(it, fmtLong)
        End If
    
        nErr = LSgetMIPCallbackInfo(model, LS_DINFO_MIP_BESTBOUND, bound)
        If nErr = LSERR_NO_ERROR Then
            Form1.labBound = Format(bound, fmtDouble)
        End If
        
        nErr = LSgetMIPCallbackInfo(model, LS_DINFO_MIP_OBJ, obj)
        If nErr = LSERR_NO_ERROR Then
            Form1.labObj = Format(obj, fmtDouble)
        End If
    
        nErr = LSgetMIPCallbackInfo(model, LS_IINFO_MIP_STATUS, status)
        If nErr = LSERR_NO_ERROR Then
            Form1.labStat = Format(status, fmtLong)
        End If
    Else
    
        '' get callback data for LPs
        
        nErr = LSgetCallbackInfo(model, nLoc, LS_IINFO_SIM_ITER, it)
        If nErr = LSERR_NO_ERROR Then
          Form1.labIter = Format(it, fmtLong)
        End If
    
        nErr = LSgetCallbackInfo(model, nLoc, LS_DINFO_PINFEAS, pinfeas)
        If nErr = LSERR_NO_ERROR Then
            Form1.labInfeas = Format(pinfeas, fmtDouble)
        End If
        
        nErr = LSgetCallbackInfo(model, nLoc, LS_DINFO_POBJ, obj)
        If nErr = LSERR_NO_ERROR Then
            Form1.labObj = Format(obj, fmtDouble)
        End If
    
        nErr = LSgetCallbackInfo(model, nLoc, LS_IINFO_STATUS, status)
        If nErr = LSERR_NO_ERROR Then
            Form1.labStat = Format(status, fmtLong)
        End If
            
    End If
        
    ' Check if cmdQuit is clicked
    flgPeek = PeekMessage(Message, Form1.cmdQuit.hWnd, WM_MOUSEFIRST, WM_MOUSELAST, PM_REMOVE)
    If flgPeek And Message.Message = 513 Then
        GeneralCallback = -1
    End If
            
    
    ' Another simple termination condition
    'If Abs(Abs(bound) - Abs(obj)) < Abs(obj) * 0.2 Then
      ' terminate signal
      'GeneralCallback = -1
    'Else
      ' continue signal
      'GeneralCallback = 0
    'End If
    
    Form1.Refresh
                                        
 
 End Function

Public Function NextMIPCallback(ByVal model As Long, _
 ByRef myData As UserData, ByVal dObj As Double, ByRef pX As Double) As Long
    Dim Message As Msg
    
    NextMIPCallback = 0
    
    Form1.Label12 = Format(dObj, fmtDouble)
    
    Form1.Refresh
    ' Check if cmdQuit is clicked
    flgPeek = PeekMessage(Message, Form1.cmdQuit.hWnd, WM_MOUSEFIRST, WM_MOUSELAST, PM_REMOVE)
    If flgPeek And Message.Message = 513 Then
        NextMIPCallback = -1
    End If
    
 End Function





