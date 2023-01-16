Attribute VB_Name = "swap"
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
''
''    LINDO API Version 4.0
''    Copyright (c) 2006
''
''    LINDO Systems, Inc.            312.988.7422
''    1415 North Dayton St.          info@lindo.com
''    Chicago, IL 60622              http://www.lindo.com


Type SAFEARRAYBOUND
  cElements As Long
  lLbound As Long
End Type

Type SAFEARRAY1D
  cDims As Integer
  fFeatures As Integer
  cbElements As Long
  cLocks As Long
  pvData As Long
  Bounds(0 To 0) As SAFEARRAYBOUND
End Type

Declare Sub CopyMemory Lib "kernel32" Alias _
"RtlMoveMemory" (pDst As Any, pSrc As Any, _
ByVal ByteLen As Long)

Declare Function VarPtrArray Lib "msvbvm60.dll" Alias "VarPtr" _
(Var() As Any) As Long

Public Function MyAttachArray(temp() As Double, ByVal pdX As Long, ByRef pvData As Long)

    Dim saPtr As Long
    Dim sa As SAFEARRAY1D

    ' retrieve the SA structure of X array
    CopyMemory saPtr, ByVal VarPtrArray(temp), 4
    CopyMemory sa, ByVal saPtr, Len(sa)

    ' record original data block
    pvData = sa.pvData
    
    ' temporarily point to pdX
    sa.pvData = pdX

    ' parse new SA
    CopyMemory ByVal saPtr, sa, Len(sa)

End Function

Public Function MyDetachArray(temp() As Double, ByVal pvData As Long)

    Dim saPtr As Long
    Dim sa As SAFEARRAY1D
    
    ' retrieve the SA structure of X array
    CopyMemory saPtr, ByVal VarPtrArray(temp), 4
    CopyMemory sa, ByVal saPtr, Len(sa)
        
    ' restore original block
    sa.pvData = pvData
    CopyMemory ByVal saPtr, sa, Len(sa)

End Function



