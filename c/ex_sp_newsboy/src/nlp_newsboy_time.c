   /* Load stage/time structure for rows,columns and stochastic params */
   { // begin time data
     int      errorcode   = LSERR_NO_ERROR;
     int      numStages   = 2;

     /* Stage indices of columns */
     int      colStages[]   =
     {
       0,  1,  1,  1,  1,   -1
     };

     /* Stage indices of rows */
     int      rowStages[]   =
     {
       0,  1,  1,  1,  1,   -1
     };

     /* Stage indices of stochastic parameters */
     int      panSparStage[]   =
     {
       1,   -1
     };

     /* Default values of stochastic parameters (optional)*/
     double   padSparValue[]   =
     {
                0,   -1
     };

     /* Load stage data */
#if 1
     errorcode = LSloadStageData(pModel,numStages,rowStages,colStages);
     APIERRORCHECK2(errorcode);
#else
     //Deprecated
     errorcode=LSsetNumStages(pModel,numStages);
     APIERRORCHECK2(errorcode);

     errorcode=LSloadVariableStages(pModel,colStages);
     APIERRORCHECK2(errorcode);

     errorcode=LSloadConstraintStages(pModel,rowStages);
     APIERRORCHECK2(errorcode);
#endif
     errorcode=LSloadStocParData(pModel,panSparStage,padSparValue);
     APIERRORCHECK2(errorcode);

   } // end time data