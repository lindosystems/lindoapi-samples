{// begin indep discrete event
  int      errorcode = 0;
  int      iRow      = 1;
  int      jCol      = -8;
  int      iStv      = 0;
  int      nRealizations = 3;
  int      iModifyRule = LS_REPLACE;
  double   padVals[]   =
  {
            90,          60,          30,   -1
  };
  double   padProbs[]   =
  {
           0.4,         0.3,         0.3,   -1
  };

  errorcode = LSaddDiscreteIndep(pModel,iRow,jCol,
        iStv,nRealizations,padProbs,padVals,iModifyRule);
  APIERRORCHECK2(errorcode);
} // end indep event




{// begin indep discrete event
  int      errorcode = 0;
  int      iRow      = 3;
  int      jCol      = -8;
  int      iStv      = 1;
  int      nRealizations = 2;
  int      iModifyRule = LS_REPLACE;
  double   padVals[]   =
  {
             9,         -15,   -1
  };
  double   padProbs[]   =
  {
           0.3,         0.7,   -1
  };

  errorcode = LSaddDiscreteIndep(pModel,iRow,jCol,
        iStv,nRealizations,padProbs,padVals,iModifyRule);
  APIERRORCHECK2(errorcode);
} // end indep event