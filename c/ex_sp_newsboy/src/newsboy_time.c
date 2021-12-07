{ /* Load stage data */
  int errorcode = LSERR_NO_ERROR;
  int numStages = 3;
  int colStages[] = {0, 1, 1, 1, 1, 1, 2}; /* Stage
                                           indices of columns */
  int rowStages[] = {0, 1, 1, 1, 2 }; /* Stage
                                      indices of rows */
  int panSparStage[]= {1, 2 }; /* Stage indices of stochastic
                               parameters */
#if 1
  errorcode = LSloadStageData(pModel,numStages,rowStages,colStages);
  APIERRORCHECK2(errorcode);
#else
  //Deprecated.
  errorcode=LSsetNumStages(pModel,numStages);
  APIERRORCHECK2(errorcode);
  errorcode=LSloadVariableStages(pModel,colStages);
  APIERRORCHECK2(errorcode);
  errorcode=LSloadConstraintStages(pModel,rowStages);
  APIERRORCHECK2(errorcode);
#endif
  errorcode=LSloadStocParData(pModel,panSparStage,NULL);
  APIERRORCHECK2(errorcode);
}