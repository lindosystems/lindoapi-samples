
#if 1
{
  int      errorcode      = 0;
  // Enable this section to include variable and constraint names
  errorcode = LSreadMPIFile(pModel,"smpi/newsboy.mpi");
  APIERRORCHECK2(errorcode);
}
#else
{ // Enable this section to load i-list with an API call
  // This option does not include variable and constraint names
  // An explict call to LSloadNameData() would be required
  // begin core instructions 
  int      errorcode      = 0;
  int      nVars          = 7;
  int      nCons          = 5;
  int      nNumbers       = 6;
  int      *paiVars       = NULL;
  double   *padVarVal     = NULL;
  double   padNumVal[]    = 
  {
    0,  1,  5,  10,  30,  60,   -1
  };
  int      nObjs          = 1;
  int      panObjSense[]  = 
  {
    LS_MAX,   -1
  };
  int      panObjLen[]    = 
  {
    2,   -1
  };
  int      paiObjBeg[]    = 
  {
    0,  2,   -1
  };
  int      paiConBeg[]    = 
  {
    2,  7,  18,  29,  40,  81,   -1
  };
  int      panConLen[]    = 
  {
    5,  11,  11,  11,  41,   -1
  };
  int      nInstruct      = 81;
  int      panInstruct[]  = 
  {
      1063,       6, 
      1063,       0,    1062,       1,    1002,    1063,       0,    1063,       1,    1002,    1063, 
         2,    1001,    1092,       0,    1002,    1063,       0,    1063,       1,    1002,    1063, 
         3,    1002,    1062,       0,    1002,    1063,       4,    1063,       1,    1002,    1063, 
         5,    1001,    1062,       0,    1002,    1062,       5,    1063,       3,    1003,    1062, 
         4,    1063,       0,    1003,    1002,    1062,       3,    1063,       1,    1003,    1002, 
      1062,       2,    1063,       2,    1003,    1002,    1063,       4,    1092,       1,    1003, 
      1001,    1062,       3,    1063,       5,    1003,    1002,    1063,       6,    1002,    1062, 
         0,    1002,   -1
  };
  char   pszVarType[]     = 
  {
    'C',  'C',  'C',  'C',  'C',  'C',  'C',   -1
  };
  char   pszConType[]   = 
  {
    'G',  'E',  'E',  'E',  'E',   -1
  };
  double   padLB[]   = 
  {
    0,  0,  0,  0,  0,  0,  0,   -1
  };
  double   padUB[]   = 
  {
    1e+030,  1e+030,  1e+030,  1e+030,  1e+030,  1e+030,  1e+030,   -1
  };


  /* Load core instructions... */ 
  errorcode = LSloadInstruct(pModel,nCons,nObjs,nVars,nNumbers,panObjSense,pszConType,
  	pszVarType,panInstruct,nInstruct,paiVars,padNumVal,padVarVal,paiObjBeg,panObjLen,
  	paiConBeg,panConLen,padLB,padUB);
  APIERRORCHECK2(errorcode);

} // end core instructions 
#endif

