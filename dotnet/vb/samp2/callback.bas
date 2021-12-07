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
 ByVal loc As Long, ByRef myData As Long) As Long
  
   Dim it As Long
   Dim ob As Double
        
   Call LSgetCallbackInfo(model, loc, LS_IINFO_SIM_ITER, it)
  
   Call LSgetCallbackInfo(model, loc, LS_DINFO_POBJ, ob)
  
   MsgBox "In MyCallback" & vbCrLf & "Iteration: " _
    & it & vbCrLf & "Objective value:  " & ob
    
   MyCallback = 0

End Function



