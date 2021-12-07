Attribute VB_Name = "Module3"
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
''
''    LINDO API Version 4.0
''    Copyright (c) 2006
''
''    LINDO Systems, Inc.            312.988.7422
''    1415 North Dayton St.          info@lindo.com
''    Chicago, IL 60622              http://www.lindo.com

Public Function MyCallback(ByVal model As Long, _
 ByRef myData As Long, ByVal obj As Double, _
 ByRef primals As Double) As Long
  
   MsgBox "New integer solution: Obj = " & obj
   
   MyCallback = 0

End Function



