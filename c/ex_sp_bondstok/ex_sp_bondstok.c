/*
###################################################################
#                       LINDO-API
#                    Sample Programs
#                  Copyright (c) 2011
#
#         LINDO Systems, Inc.           312.988.7422
#         1415 North Dayton St.         info@lindo.com
#         Chicago, IL 60622             http://www.lindo.com
###################################################################

  File   : ex_sp_bondstok.c

  Purpose: Build a SP model via an instruction list and solve it.

  Example:
  A four-period investment planning model to fund college education.
  Two investment type each stage: Stocks and Bonds (B,S).
  Ref: Birge & Louveau

 [ COST]       MIN = 4 * Y -  Z;
 [ STAGE1A]  + X1B +  X1S = 55;
 [ STAGE2A]  - R1B * X1B - R1S * X1S +  X2B +  X2S = 0;
 [ STAGE3A]  - R2B * X2B - R2S * X2S +  X3B +  X3S = 0;
 [ STAGE4A]  + R3B * X3B + R3S * X3S -  Z = 0;
 [ STAGE4B]  + R3B * X3B + R3S * X3S +  Y >= 80;


  Stochastic Parameters:
  Rtk : random return from invesment type k=B,S in stage, t=2,3,4.

  Initial wealth: $55K
  Target wealth: $80K

  Decision Variables:
  Xtk: Amount invested on investment type k=B,S in stage t, t=1,2,3,4;

  Z: total wealth ($K) at the end of period 4;

  Y: amount fell short from target wealth at the end of period 4;

  Objective: maximize the wealth at the end of period 4.

*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

/* LINDO API header file */
#include "lindo.h"

/* Define a macro to declare variables for
    error checking */
#define APIERRORSETUP  \
   int nErrorCode; \
   char cErrorMessage[LS_MAX_ERROR_MESSAGE_LENGTH] \

/* Define a macro to do our error checking */
#define APIERRORCHECK  \
   if (nErrorCode) \
   { \
      if ( pEnv) \
      { \
         LSgetErrorMessage( pEnv, nErrorCode, \
          cErrorMessage); \
         printf("Errorcode=%d:  %s\n", nErrorCode, \
          cErrorMessage); \
      } else {\
         printf( "Fatal Error\n"); \
      } \
      goto Terminate; \
   } \


#define APIVERSION \
{\
    char szVersion[255], szBuild[255];\
    LSgetVersionInfo(szVersion,szBuild);\
    printf("\nLINDO API Version %s built on %s\n",szVersion,szBuild);\
}\

/*
 * Callback function to display progress information.
 */
int  LS_CALLTYPE progress_log(pLSmodel model,int iLoc, void *cbData)
{
  static int iter=0;
  static double pfeas=0.0,pobj=0.0;
  static double bestbnd;
  static int nStatus;

  if (iLoc == LSLOC_MIP)
  {
    LSgetCallbackInfo(model,iLoc,LS_IINFO_MIP_STATUS,&nStatus);
    LSgetCallbackInfo(model,iLoc,LS_IINFO_MIP_SIM_ITER,&iter);
    LSgetCallbackInfo(model,iLoc,LS_DINFO_MIP_OBJ,&pobj);
    LSgetCallbackInfo(model,iLoc,LS_DINFO_MIP_BESTBOUND,&bestbnd);
    LSgetCallbackInfo(model,iLoc,LS_DINFO_PINFEAS,&pfeas);
    printf("%2d\tIt=%4d \tInfeas= %8.3e\tObj=%11.5e \tBestBound=%11.5e \tStat=%d\n",
      iLoc,iter,pfeas,pobj,bestbnd,nStatus);
  }

  else if ( iLoc == LSLOC_PRIMAL || iLoc ==LSLOC_DUAL ||
            iLoc ==LSLOC_CONOPT  || iLoc == LSLOC_BARRIER)
  {
    LSgetCallbackInfo(model,iLoc,LS_IINFO_STATUS,&nStatus);
    LSgetCallbackInfo(model,iLoc,LS_DINFO_PINFEAS,&pfeas);
    LSgetCallbackInfo(model,iLoc,LS_IINFO_SIM_ITER,&iter);
    printf("%2d\tIt=%4d \tInfeas= %8.3e\tObj=%11.5e \tStat=%d\n",
      iLoc,iter,pfeas,pobj,nStatus);
  }

  return 0;
} /*progress_log*/


/*
 * Log function to pipe messages from the solver.
 */
static void LS_CALLTYPE print_line_log(pLSmodel pModel, char *line, void *userdata)
{
  if (line)
  {
    printf("%s",line);
  } /*if*/
} /*print_line*/



/**
 *
 *      Main
 *
 */
int main(int argc, char **argv)
{
   APIERRORSETUP;
   int numCons, numVars; /* number of constraints and vars */
   int numCont=0, numBin=0, numInt=0; /* number of cont, bin. int vars*/
   int numStages, numScens,numCoreRows,numCoreCols;
   int numNodes, numDeqRows, numDeqCols,*numStageRows=NULL,*numStageCols=NULL, numStocPars=0;
   int counter = 0, nStatus;
   int i,j=0,i1=0,i2=0;

   char MY_LICENSE_KEY[1024];
   char *mpsfile=NULL, *basfile = NULL, deqFile[255]="", *ptr=NULL;
   char *coreFile,*timeFile,*stocFile, *soluFile;
   char strbuf[1024];

   double dObj,dEvpi, *padOutcome=NULL, dProb=0;

    /* Declare an instance of the LINDO environment object */
   pLSenv pEnv = NULL;

    /* Declare instances of the LINDO model object */
   pLSmodel pModel=NULL, edeModel=NULL, ideModel=NULL;


   /*
    *     Parse input spex file
    */
   memset(strbuf,0,sizeof(char)*1024);
   coreFile=&strbuf[0];
   timeFile=&strbuf[256];
   stocFile=&strbuf[256*2];
   soluFile=&strbuf[256*3];

   APIVERSION;
   /*
    *     Create a LINDO environment.
    */
   if (argc == 0) {
     printf("\nUsage: ex_sp_bondstok\n\n");
     exit(1);
   }

   nErrorCode = LSloadLicenseString("../../../license/lndapi130.lic",MY_LICENSE_KEY);
   APIERRORCHECK;

   pEnv = LScreateEnv ( &nErrorCode, MY_LICENSE_KEY);
   if ( nErrorCode == LSERR_NO_VALID_LICENSE) {
      printf( "Invalid License Key!\n");
      exit( 1);
   }
   APIERRORCHECK;


   /*
    *     Create a model in the environment.
    */
   pModel = LScreateModel ( pEnv, &nErrorCode);
   APIERRORCHECK;

   nErrorCode = LSsetModelIntParameter(pModel,LS_IPARAM_NLP_LINEARZ,1);
   APIERRORCHECK;

   if (1==1)
   {
     /* Install a log function */
     LSsetModelLogfunc(pModel, (printModelLOG_t) print_line_log, NULL);
   }
   else
   {
     /* Install a callback function */
     LSsetCallback(pModel,(cbFunc_t) progress_log, NULL);
   }


   /*
    * Read core model in MPI format
    *
    * Edit the MPI file to see how stochastic parameters {R1B, R1S, R2B, R2S,
    * R3B, R3S} marked with EP_PUSH_SPAR macro.
    */
   nErrorCode = LSreadMPIFile(pModel,"smpi/bondstok4.mpi");


   /* Load stage/time structure for rows,columns and stochastic params */
   { // begin time data
     int      errorcode   = LSERR_NO_ERROR;
     int      numStages   = 4;

     /* Stage indices of columns */
     int      colStages[]   =
     {
       3,  3,  0,  0,  1,  1,  2,  2,  3,   -1
     };

     /* Stage indices of rows */
     int      rowStages[]   =
     {
       0,  1,  2,  3,  3,   -1
     };

     /* Stage indices of stochastic parameters */
     int      panSparStage[]   =
     {
       1,  1,  2,  2,  3,  3,   -1
     };

     /* Default values of stochastic parameters (optional)*/
     double   padSparValue[]   =
     {
              -91,         -92,         -93,
              -94,         -95,         -96,   -1
     };

     /* Load stage data */
#if 1
     errorcode = LSloadStageData(pModel,numStages,rowStages,colStages);
     if (errorcode !=0) { fprintf(stdout,"\nError=%d\n",errorcode); exit(1);}
#else
     //Deprecated.
     errorcode=LSsetNumStages(pModel,numStages);
     if (errorcode !=0) { fprintf(stdout,"\nError=%d\n",errorcode); exit(1);}

     errorcode=LSloadVariableStages(pModel,colStages);
     if (errorcode !=0) { fprintf(stdout,"\nError=%d\n",errorcode); exit(1);}

     errorcode=LSloadConstraintStages(pModel,rowStages);
     if (errorcode !=0) { fprintf(stdout,"\nError=%d\n",errorcode); exit(1);}
#endif
     errorcode=LSloadStocParData(pModel,panSparStage,padSparValue);
     if (errorcode !=0) { fprintf(stdout,"\nError=%d\n",errorcode); exit(1);}

   } // end time data




   /* Load stochastic data in discrete blocks */
   { // begin-block
     int      errorcode   = 0;
     int      iStage      = 1;
     int      nBlockEvents= 2;
     int      iModifyRule = LS_REPLACE;
     double   padProb[]   =
     {
         0.5,   0.5,  -1
     };
     int      pakStart[]  =
     {
           0,     2,     4, -1
     };
     int      *paiRows   = NULL;
     int      *paiCols   = NULL;
     int      paiStvs[]   =
     {
           0,     1,     0,     1,  -1
     };
     double  padVals[]   =
     {
        1.14,  1.25,  1.12,  1.06,  -1
     };
     errorcode=LSaddDiscreteBlocks(pModel,iStage,nBlockEvents,
           padProb,pakStart,paiRows,paiCols,paiStvs,padVals,iModifyRule);

     if (errorcode !=0) { fprintf(stdout,"\nError=%d\n",errorcode); exit(1);}
   } // end-block




   { // begin-block
     int      errorcode   = 0;
     int      iStage      = 2;
     int      nBlockEvents= 2;
     int      iModifyRule = LS_REPLACE;
     double   padProb[]   =
     {
         0.5,   0.5,  -1
     };
     int      pakStart[]  =
     {
           0,     2,     4, -1
     };
     int      *paiRows   = NULL;
     int      *paiCols   = NULL;
     int      paiStvs[]   =
     {
           2,     3,     2,     3,  -1
     };
     double  padVals[]   =
     {
        1.14,  1.25,  1.12,  1.06,  -1
     };
     errorcode=LSaddDiscreteBlocks(pModel,iStage,nBlockEvents,
           padProb,pakStart,paiRows,paiCols,paiStvs,padVals,iModifyRule);

     if (errorcode !=0) { fprintf(stdout,"\nError=%d\n",errorcode); exit(1);}
   } // end-block




   { // begin-block
     int      errorcode   = 0;
     int      iStage      = 3;
     int      nBlockEvents= 2;
     int      iModifyRule = LS_REPLACE;
     double   padProb[]   =
     {
         0.5,   0.5,  -1
     };
     int      pakStart[]  =
     {
           0,     2,     4, -1
     };
     int      *paiRows   = NULL;
     int      *paiCols   = NULL;
     int      paiStvs[]   =
     {
           4,     5,     4,     5,  -1
     };
     double  padVals[]   =
     {
        1.14,  1.25,  1.12,  1.06,  -1
     };
     errorcode=LSaddDiscreteBlocks(pModel,iStage,nBlockEvents,
           padProb,pakStart,paiRows,paiCols,paiStvs,padVals,iModifyRule);

     if (errorcode !=0) { fprintf(stdout,"\nError=%d\n",errorcode); exit(1);}
   } // end-block

   // optionally specify a correlation structure (default none).
   if (0)
   {

      int errorcode   = 0;
      int nDim = 4;
      int QCnonzeros = 10;
      int QCvarndx1[] = {0, 0, 0, 0, 1, 1, 1, 3, 3, 4, -1};
      int QCvarndx2[] = {0, 1, 3, 4, 1, 3, 4, 3, 4, 4, -1};
      double QCcoef[] = {1, 0.5796, -0.953, 0.5409, 1, -0.4181, 0.6431, 1, -0.2616, 1, -1};

      nErrorCode = LSgetInfo(pModel,LS_IINFO_NUM_SPARS,&numStocPars);

      errorcode = LSloadCorrelationMatrix(pModel,nDim,LS_CORR_PEARSON,QCnonzeros,QCvarndx1,QCvarndx2,QCcoef);
   }


   /* Load sample sizes per stage (default none) */
   if (0)
   {
     int   errorcode         = 0;
     int   panSampleSize[]   =
     {
                0,           6,           6,      6,  -1
     };

     errorcode=LSloadSampleSizes(pModel,panSampleSize);

     if (errorcode !=0) { fprintf(stdout,"\nError=%d\n",errorcode); exit(1);}
   }



   /*
    *   Get and display SP statistics
    */
   nErrorCode = LSgetInfo(pModel,LS_IINFO_NUM_VARS,&numVars);
   APIERRORCHECK;
   nErrorCode = LSgetInfo(pModel,LS_IINFO_NUM_CONS,&numCons);
   APIERRORCHECK;
   nErrorCode = LSgetInfo(pModel,LS_IINFO_NUM_CONT,&numCont);
   APIERRORCHECK;
   nErrorCode = LSgetInfo(pModel,LS_IINFO_NUM_SPARS,&numStocPars);

   nErrorCode = LSgetStocInfo(pModel,LS_IINFO_STOC_NUM_STAGES   , 0, &numStages);
   APIERRORCHECK;
   nErrorCode = LSgetStocInfo(pModel,LS_IINFO_STOC_NUM_NODES    , 0, &numNodes);
   APIERRORCHECK;
   nErrorCode = LSgetStocInfo(pModel,LS_IINFO_STOC_NUM_SCENARIOS, 0, &numScens);
   APIERRORCHECK;
   nErrorCode = LSgetStocInfo(pModel,LS_IINFO_STOC_NUM_ROWS_DETEQI , 0, &numDeqRows);
   APIERRORCHECK;
   nErrorCode = LSgetStocInfo(pModel,LS_IINFO_STOC_NUM_COLS_DETEQI , 0, &numDeqCols);
   APIERRORCHECK;
   nErrorCode = LSgetStocInfo(pModel,LS_IINFO_STOC_NUM_ROWS_CORE, 0, &numCoreRows);
   APIERRORCHECK;
   nErrorCode = LSgetStocInfo(pModel,LS_IINFO_STOC_NUM_COLS_CORE, 0, &numCoreCols);
   APIERRORCHECK;
   numStageRows=malloc(sizeof(int)*numStages*2);
   numStageCols=numStageRows+numStages;
   for (i=0; i<numStages;i++)
   {
     LSgetStocInfo(pModel,LS_IINFO_STOC_NUM_ROWS_STAGE, i, &numStageRows[i]);
     LSgetStocInfo(pModel,LS_IINFO_STOC_NUM_COLS_STAGE, i, &numStageCols[i]);
   }

   if (0)
   {
     printf("\nStochastic Model Stats:\n");
     printf("Number of stages     = %d\n",numStages);
     printf("Number of nodes      = %d\n",numNodes);
     printf("Number of scenarios  = %d\n",numScens);
     printf("Core model (rows,cols) = (%d,%d)\n",numCoreRows,numCoreCols);
     for (i=0; i<numStages;i++)
     {
       printf("Core model (rows,col) in stage %d: (%d,%d)\n",i,numStageRows[i],numStageCols[i]);
     }
     printf("Deterministic eq. (rows,col) = (%d,%d)\n",numDeqRows,numDeqCols);
     if (0)
     {
       printf("\n");
       for (i=0; i<numNodes;i++)
       {
         LSgetStocInfo(pModel,LS_IINFO_STOC_NUM_ROWS_BEFORE_NODE, i, &i1);
         LSgetStocInfo(pModel,LS_IINFO_STOC_NUM_COLS_BEFORE_NODE, i, &i2);
         printf("node %d: offset rows = %d, cols=%d.\n",i,i1,i2);
       }
     }
   }


   /*
    *     Write model in SMPS/SMPI format
    */
   if (0)
   {
     if (numStocPars)
     {
       nErrorCode =  LSwriteSMPIFile(pModel,"out/foo.mpi","out/foo.time","out/foo.stoc");
     }
     else
     {
       nErrorCode =  LSwriteSMPSFile(pModel,"out/foo.mps","out/foo.time","out/foo.stoc",LS_FORMATTED_MPS);
     }
     APIERRORCHECK;
   }


   /*
    *     Get deterministic equivalents (and optionally export them)
    */
   if (0)
   {
     nErrorCode = LSgetInfo(pModel,LS_IINFO_NUM_SPARS,&numStocPars);
     ptr=deqFile;
     strcpy(ptr,stocFile);
     if (0)
     {
       ideModel = LSgetDeteqModel(pModel,LS_DETEQ_IMPLICIT,&nErrorCode);
       if (numStocPars)
       {
         ptr = strcat(ptr,"_IDE.mpi");
         LSwriteMPIFile(ideModel,ptr);
       }
       else
       {
         ptr = strcat(ptr,"_IDE.ltx");
         LSwriteLINDOFile(ideModel,ptr);
       }
     }
     APIERRORCHECK;

     if (1)
     {
       edeModel = LSgetDeteqModel(pModel,LS_DETEQ_EXPLICIT,&nErrorCode);
       if (numStocPars)
       {
         ptr = strcat(ptr,"_EDE.mpi");
         LSwriteMPIFile(edeModel,ptr);
       }
       else
       {
         ptr = strcat(ptr,"_EDE.ltx");
         LSwriteLINDOFile(edeModel,ptr);
       }     }
     APIERRORCHECK;
   }


   /*
    *     Set params and solve the SP
    */
   if (0)
   {
     LSsetModelStocIntParameter(pModel,LS_IPARAM_STOC_DETEQ_TYPE,LS_DETEQ_EXPLICIT);
     LSsetModelStocIntParameter(pModel,LS_IPARAM_STOC_TOPOPT,LS_METHOD_GOP);
     LSsetModelStocIntParameter(pModel,LS_IPARAM_STOC_TOPOPT,LS_METHOD_MULTIS);
   }
   nErrorCode = LSsolveSP(pModel,&nStatus);


   /*
    *     Access the final solution if optimal or feasible
    */
   if (nStatus == LS_STATUS_OPTIMAL ||
       nStatus == LS_STATUS_BASIC_OPTIMAL ||
       nStatus == LS_STATUS_LOCAL_OPTIMAL ||
       nStatus == LS_STATUS_FEASIBLE)
   {
     /* E[objective value] */
     nErrorCode = LSgetStocInfo(pModel,LS_DINFO_STOC_EVOBJ,0,&dObj);
     APIERRORCHECK;

     /* lower bound on E[value of perfect information] */
     nErrorCode = LSgetStocInfo(pModel,LS_DINFO_STOC_EVPI,0,&dEvpi);
     //APIERRORCHECK;

     if (numStocPars)
     {
       padOutcome = malloc(numStocPars*sizeof(double));
       if (padOutcome ==NULL)
       {
         nErrorCode = LSERR_OUT_OF_MEMORY;
         APIERRORCHECK;
       }
     }

     for (j=0; j<numScens; j++)
     {
       soluFile = NULL;
       if (1) nErrorCode = LSwriteScenarioSolutionFile(pModel,j,soluFile);
       if (0) nErrorCode = LSwriteScenarioMPIFile(pModel,j,NULL);
       if (numStocPars)
       {
         nErrorCode = LSgetStocParOutcomes(pModel,j,padOutcome,&dProb);
         APIERRORCHECK;
         printf("scenario %d has probability %g\n",j,dProb);
         for (i=0; i<numStocPars; i++)
         {
           printf("\t outcome[%d] = %10g\n",i,padOutcome[i]);
         }
       }
     }
   }
   else
   {
     char strbuf[255];
     LSgetErrorMessage(pEnv,nErrorCode,strbuf);
     printf ("\n Optimization failed. nStatus = %d ",nStatus);
     printf ("\n Error %d: %s\n",nErrorCode,strbuf);
   }

Terminate:
   nErrorCode = LSdeleteModel( &pModel);
   nErrorCode = LSdeleteEnv( &pEnv);
   free(numStageRows);

  /* Wait until user presses the Enter key */
   printf("Press <Enter> ...");
   getchar();


  return 0;
}
