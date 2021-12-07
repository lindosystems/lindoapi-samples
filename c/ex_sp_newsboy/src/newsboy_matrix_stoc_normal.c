

{ /* Load discrete independent variables */
  int      errorcode = 0;
  int      iStv      = -99;                        // dummy index of stoc. param.

  // stochastic parameter 0
  int      iRow0      = 1;                        // RHS element (1) is random
  int      jCol0      = LS_JCOL_RHS;
  int      nDistType0 = LSDIST_TYPE_NORMAL;       // type of distribution
  int      nDistParams0 = 2;                      // number of distrib. params.
  double   padParams0[] = {45,     10};           // distrib. params (mu, sigma)

  // stochastic parameter 1
  int      iRow1      = 4;                        // Matrix element (4,4) is random
  int      jCol1      = 4;
  int      nDistType1 = LSDIST_TYPE_NORMAL;       // type of distribution
  int      nDistParams1 = 2;                      // number of distrib. params.
  double   padParams1[] = {-3,      2};           // distrib. params (mu, sigma)

  // load stoc. param. 0
  errorcode=LSaddParamDistIndep(pModel,iRow0,jCol0,iStv,
                               nDistType0,nDistParams0,padParams0,LS_REPLACE);
  if (errorcode !=0) { fprintf(stdout,"\nError=%d\n",errorcode); exit(1);}

  errorcode=LSaddParamDistIndep(pModel,iRow1,jCol1,iStv,
                               nDistType1,nDistParams1,padParams1,LS_REPLACE);
  if (errorcode !=0) { fprintf(stdout,"\nError=%d\n",errorcode); exit(1);}

  if (1>0)
  {
    int panSampleSize[] = {1, 6, 6};
    errorcode=LSloadSampleSizes(pModel,panSampleSize);
    if (errorcode !=0) { fprintf(stdout,"\nError=%d\n",errorcode); exit(1); }
  }

} // end indep event