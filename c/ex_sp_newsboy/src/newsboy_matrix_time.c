{ /* Load stage data */
  int errorcode = LSERR_NO_ERROR;
  int numStages = 3;
  int colStages[] = {0, 1, 1, 1, 1, 1, 2}; /* Stage
                                           indices of columns */
  int rowStages[] = {0, 1, 1, 1, 2 }; /* Stage
                                      indices of rows */

#if 1
  errorcode = LSloadStageData(pModel,numStages,rowStages,colStages);
  APIERRORCHECK(errorcode);
  errorcode = LSloadStocParData(pModel,NULL,NULL);
  APIERRORCHECK(errorcode);
#else
  //Deprecated.
  errorcode=LSsetNumStages(pModel,numStages);
  APIERRORCHECK2(errorcode);
  errorcode=LSloadVariableStages(pModel,colStages);
  APIERRORCHECK2(errorcode);
  errorcode=LSloadConstraintStages(pModel,rowStages);
  APIERRORCHECK2(errorcode);
#endif
}