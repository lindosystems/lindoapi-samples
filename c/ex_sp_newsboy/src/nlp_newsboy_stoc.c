   /* Load stochastic data */
   if (0==1)
   {// begin indep discrete event
     int      errorcode = 0;
     int      iRow      = 1;
     int      jCol      = -8;
     int      iStv      = 0;
     int      nRealizations = 5;
     int      iModifyRule = LS_REPLACE;
     double   padVals[]   =
     {
               77,          54,          63,          50,          81,   -1
     };
     double   padProbs[]   =
     {
              0.2,         0.2,         0.2,         0.2,         0.2,   -1
     };

     errorcode=LSaddDiscreteIndep(pModel,iRow,jCol,
           iStv,nRealizations,padProbs,padVals,iModifyRule);
     APIERRORCHECK2(errorcode);
   }
   else
   {
     // begin indep continuous event
     int      errorcode = 0;
     int      iRow      = 1;
     int      jCol      = -8;
     int      iStv      = 0;
     int      nDistType = LSDIST_TYPE_NORMAL;
     int      iModifyRule = LS_REPLACE;
     int      nParams  = 2;
     double   padParams[]   =
     {
              100,         10,   -1
     };
     int   panSampleSize[]   =
     {
                0,          30,         -1
     };

     errorcode=LSaddParamDistIndep(pModel,iRow,jCol,
           iStv,nDistType,nParams,padParams,iModifyRule);
     APIERRORCHECK2(errorcode);

     // Try different seeds when repeating runs with sampling
     LSsetModelIntParameter(pModel,LS_IPARAM_STOC_RG_SEED,1033);

     /* Load sample sizes per stage */
     errorcode=LSloadSampleSizes(pModel,panSampleSize);
      APIERRORCHECK2(errorcode);
   } // end event
