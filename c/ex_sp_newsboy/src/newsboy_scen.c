

{ // begin-scenario
  int      errorcode   = 0;
  int      jScenario   = 0;
  int      iParentScen = -1;
  int      iStage      = 0;
  double   dProb       = 0.12;
  int      iModifyRule = LS_REPLACE;
  int      nElems      = 2;
  int      *paiRows   = NULL;
  int      *paiCols   = NULL;
  int      paiStvs[]   =
  {
     0,  1, -1
  };
  double  padVals[]   =
  {
     90, 9, -1
  };
  errorcode=LSaddScenario(pModel,jScenario,iParentScen,
       iStage,dProb,nElems,paiRows,paiCols,paiStvs,padVals,iModifyRule);

  APIERRORCHECK2(errorcode);
} // end-scenario




{ // begin-scenario
  int      errorcode   = 0;
  int      jScenario   = 1;
  int      iParentScen = 0;
  int      iStage      = 2;
  double   dProb       = 0.28;
  int      iModifyRule = LS_REPLACE;
  int      nElems      = 1;
  int      *paiRows   = NULL;
  int      *paiCols   = NULL;
  int      paiStvs[]   =
  {
    1,   -1
  };
  double  padVals[]   =
  {
           -15,   -1
  };
  errorcode=LSaddScenario(pModel,jScenario,iParentScen,
       iStage,dProb,nElems,paiRows,paiCols,paiStvs,padVals,iModifyRule);

  APIERRORCHECK2(errorcode);
} // end-scenario




{ // begin-scenario
  int      errorcode   = 0;
  int      jScenario   = 2;
  int      iParentScen = -1;
  int      iStage      = 1;
  double   dProb       = 0.15;
  int      iModifyRule = LS_REPLACE;
  int      nElems      = 2;
  int      *paiRows   = NULL;
  int      *paiCols   = NULL;
  int      paiStvs[]   =
  {
    0,   1, -1
  };
  double  padVals[]   =
  {
            60,   9, -1
  };
  errorcode=LSaddScenario(pModel,jScenario,iParentScen,
       iStage,dProb,nElems,paiRows,paiCols,paiStvs,padVals,iModifyRule);

  APIERRORCHECK2(errorcode);
} // end-scenario




{ // begin-scenario
  int      errorcode   = 0;
  int      jScenario   = 3;
  int      iParentScen = 2;
  int      iStage      = 2;
  double   dProb       = 0.15;
  int      iModifyRule = LS_REPLACE;
  int      nElems      = 1;
  int      *paiRows   = NULL;
  int      *paiCols   = NULL;
  int      paiStvs[]   =
  {
    1,   -1
  };
  double  padVals[]   =
  {
           -15,   -1
  };
  errorcode=LSaddScenario(pModel,jScenario,iParentScen,
       iStage,dProb,nElems,paiRows,paiCols,paiStvs,padVals,iModifyRule);
  APIERRORCHECK2(errorcode);
  
} // end-scenario




{ // begin-scenario
  int      errorcode   = 0;
  int      jScenario   = 4;
  int      iParentScen = -1;
  int      iStage      = 1;
  double   dProb       = 0.27;
  int      iModifyRule = LS_REPLACE;
  int      nElems      = 2;
  int      *paiRows   = NULL;
  int      *paiCols   = NULL;
  int      paiStvs[]   =
  {
    0,   1,  -1
  };
  double  padVals[]   =
  {
            30,   9,  -1
  };
  errorcode=LSaddScenario(pModel,jScenario,iParentScen,
       iStage,dProb,nElems,paiRows,paiCols,paiStvs,padVals,iModifyRule);

  APIERRORCHECK2(errorcode);
} // end-scenario




{ // begin-scenario
  int      errorcode   = 0;
  int      jScenario   = 5;
  int      iParentScen = 4;
  int      iStage      = 2;
  double   dProb       = 0.03;
  int      iModifyRule = LS_REPLACE;
  int      nElems      = 1;
  int      *paiRows   = NULL;
  int      *paiCols   = NULL;
  int      paiStvs[]   =
  {
    1,   -1
  };
  double  padVals[]   =
  {
           -15,   -1
  };
  errorcode=LSaddScenario(pModel,jScenario,iParentScen,
       iStage,dProb,nElems,paiRows,paiCols,paiStvs,padVals,iModifyRule);

  APIERRORCHECK2(errorcode);
} // end-scenario


