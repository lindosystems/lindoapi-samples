

{ /* Load discrete independent variables */
  int      errorcode = 0;
  int      iRow      = -99;
  int      jCol      = -99;

  // stochastic parameter 0
  int      iStv0     = 0;                         // index of stoc. param.
  int      nDistType0 = LSDIST_TYPE_NORMAL;       // type of distribution
  int      nDistParams0 = 2;                      // number of distrib. params.
  double   padParams0[] = {45,     10};           // distrib. params (mu, sigma)

  // stochastic parameter 1
  int      iStv1      = 1;                        // index of stoc. param.
  int      nDistType1 = LSDIST_TYPE_NORMAL;       // type of distribution
  int      nDistParams1 = 2;                      // number of distrib. params.
  double   padParams1[] = {-3,      2};           // distrib. params (mu, sigma)

  // load stoc. param. 0
  errorcode=LSaddParamDistIndep(pModel,iRow,jCol,iStv0,
                               nDistType0,nDistParams0,padParams0,LS_REPLACE);
  if (errorcode !=0) { fprintf(stdout,"\nError=%d\n",errorcode); exit(1);}

  errorcode=LSaddParamDistIndep(pModel,iRow,jCol,iStv1,
                               nDistType1,nDistParams1,padParams1,LS_REPLACE);
  if (errorcode !=0) { fprintf(stdout,"\nError=%d\n",errorcode); exit(1);}

  if (1>0)
  {
    int panSampleSize[] = {1, 6, 6};
    errorcode=LSloadSampleSizes(pModel,panSampleSize);
    if (errorcode !=0) { fprintf(stdout,"\nError=%d\n",errorcode); exit(1); }
  }

} // end indep event