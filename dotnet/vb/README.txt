
VB.NET does not support Null values. Suppose you would like to be able to pass Null for
some of the arguments, say panAcols, padL and padU in LSloadLPData() call. To achieve this, you 
will need to edit include/lindo.vb and add an overloaded LSloadLPData() declaration which 
accepts "Byval (0)" for the arguments you wish to specify as Null. E.g. consider the following
overload of LSloadLPData


    Public Declare Function LSloadLPData Lib "lindo9_0.dll" _
      (ByVal pModel As IntPtr, _
                                          ByVal nCons As Integer, _
                                          ByVal nVars As Integer, _
                                          ByVal dObjSense As Integer, _
                                          ByVal dObjConst As Double, _
       <MarshalAs(UnmanagedType.LPArray)> ByVal padC As Double(), _
       <MarshalAs(UnmanagedType.LPArray)> ByVal padB As Double(), _
                                          ByVal pszConTypes As String, _
                                          ByVal nAnnz As Integer, _
       <MarshalAs(UnmanagedType.LPArray)> ByVal paiAcols As Integer(), _
                                          ByVal panAcols As Integer, _
       <MarshalAs(UnmanagedType.LPArray)> ByVal padAcoef As Double(), _
       <MarshalAs(UnmanagedType.LPArray)> ByVal paiArows As Integer(), _
                                          ByVal padL As Integer, _
                                          ByVal padU As Integer) As Integer

Note, panAcols, padL and padU are now defined as Integer. which will allow you to pass ‘ByVal (0)’.
