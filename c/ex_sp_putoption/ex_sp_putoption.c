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

  File   : ex_sp_putoption.c

  Purpose: Build a SP model via an instruction list and solve it.

  Example:
  Stochastic Programming Version of an American Put Option as a
  six period model.

  The holder of the option has the right to sell a specified stock
  at any time(the American feature) between now and a specified
  expiration date at a specified strike price.
  The holder makes a profit in the period of exercise if the
  strike price exceeds the market price of the stock at the
  time of sale.  Wealth is invested at the risk free rate.

  Initial Price  = $100
  Strike price   =  $99
  Risk free rate = 0.04%


   MODEL:
   [OBJ] MAX= W5 ;

   [INITIAL]        P0 = 100 ;    !price at t=0;
   [R0000001] RV0 * P0 = P1  ;    !price at t=1;
   [R0000003] RV1 * P1 = P2  ;    !price at t=2;
   [R0000005] RV2 * P2 = P3  ;    !price at t=3;
   [R0000007] RV3 * P3 = P4  ;    !price at t=4;
   [R0000009] RV4 * P4 = P5  ;    !price at t=5;

   [R0000000]           + Y0 * ( 99 - P0) = W0  ;  !wealth at t=0;
   [R0000002] 1.04 * W0 + Y1 * ( 99 - P1) = W1  ;  !wealth at t=1;
   [R0000004] 1.04 * W1 + Y2 * ( 99 - P2) = W2  ;  !wealth at t=2;
   [R0000006] 1.04 * W2 + Y3 * ( 99 - P3) = W3  ;  !wealth at t=3;
   [R0000008] 1.04 * W3 + Y4 * ( 99 - P4) = W4  ;  !wealth at t=4;
   [R0000010] 1.04 * W4 + Y5 * ( 99 - P5) = W5  ;  !wealth at t=5;

   [R0000011] Y0 + Y1+ Y2+ Y3 + Y4 +  Y5 <= 1 ; ! sell only once;

   @FREE(Wt); t=0..5;
   @FREE(Pt); t=0..5;
   @BIN(Yt); t=0..5;

  Stochastic Parameters:
  RVt : random return in the end of period t, for t=0..4

  Decision Variables:
  Pt: Price of option in the beginning of period t, for t=0..5
  Wt: Wealth int the beginning of period t, for t=0..5
  Yt: 1 if sold in the beginning of period t, 0 otherwise, for t=0..5

  Objective: maximize the wealth at the beginning of period 5 (i.e.
             end of planning horizon).
*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

/* LINDO API header file */
#include "lindo.h"
#include "../common/commonutils.c"
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
    fprintf(stdout,"%s",line);
  } /*if*/
  fflush(stdout);
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


   /*
    *     Create a LINDO environment.
    */
   if (argc == 0) {
     printf("\nUsage: ex_sp_putoption\n\n");
     exit(1);
   }

   nErrorCode = LSloadDefaultLicenseString(MY_LICENSE_KEY);
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
    * Edit the MPI file to see how stochastic parameters {RVt}, t=0..4
    * marked with EP_PUSH_SVAR macro.
    */
   nErrorCode = LSreadMPIFile(pModel,"smpi/putoption.mpi");


   /* Load stage/time structure for rows,columns and stochastic params */
   { // begin time data
     int      errorcode   = LSERR_NO_ERROR;
     int      numStages   = 6;

     /* Stage indices of columns */
     int      colStages[]   =
     {
       5,  0,  1,  2,  3,  4,  5,  0,  0,  1,  1,  2,  2,  3,  3,  4,  4,  5,   -1
     };

     /* Stage indices of rows */
     int      rowStages[]   =
     {
       0,  1,  3,  5,  2,  4,  0,  2,  4,  1,  3,  5,  5,   -1
     };

     /* Stage indices of stochastic parameters */
     int      panSparStage[]   =
     {
       1,  2,  3,  4,  5,   -1
     };

     /* Default values of stochastic parameters (optional)*/
     double   padSparValue[]   =
     {
                0,           0,           0,           0,           0,   -1
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




   /* Load stochastic data */
   { // begin-block
     int      errorcode   = 0;
     int      iStage      = 1;
     int      nBlockEvents= 4;
     int      iModifyRule = LS_REPLACE;
     double   padProb[]   =
     {
        0.25,  0.25,  0.25,  0.25,  -1
     };
     int      pakStart[]  =
     {
           0,     1,     2,     3,     4, -1
     };
     int      *paiRows   = NULL;
     int      *paiCols   = NULL;
     int      paiStvs[]   =
     {
           0,     0,     0,     0,  -1
     };
     double  padVals[]   =
     {
       -0.08,  0.01,  0.07,  0.11,  -1
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
           0,     1,     2, -1
     };
     int      *paiRows   = NULL;
     int      *paiCols   = NULL;
     int      paiStvs[]   =
     {
           1,     1,  -1
     };
     double  padVals[]   =
     {
       -0.08,  0.01,  -1
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
           0,     1,     2, -1
     };
     int      *paiRows   = NULL;
     int      *paiCols   = NULL;
     int      paiStvs[]   =
     {
           2,     2,  -1
     };
     double  padVals[]   =
     {
        0.07,  0.11,  -1
     };
     errorcode=LSaddDiscreteBlocks(pModel,iStage,nBlockEvents,
           padProb,pakStart,paiRows,paiCols,paiStvs,padVals,iModifyRule);

     if (errorcode !=0) { fprintf(stdout,"\nError=%d\n",errorcode); exit(1);}
   } // end-block




   { // begin-block
     int      errorcode   = 0;
     int      iStage      = 4;
     int      nBlockEvents= 2;
     int      iModifyRule = LS_REPLACE;
     double   padProb[]   =
     {
         0.5,   0.5,  -1
     };
     int      pakStart[]  =
     {
           0,     1,     2, -1
     };
     int      *paiRows   = NULL;
     int      *paiCols   = NULL;
     int      paiStvs[]   =
     {
           3,     3,  -1
     };
     double  padVals[]   =
     {
        0.01,  0.11,  -1
     };
     errorcode=LSaddDiscreteBlocks(pModel,iStage,nBlockEvents,
           padProb,pakStart,paiRows,paiCols,paiStvs,padVals,iModifyRule);

     if (errorcode !=0) { fprintf(stdout,"\nError=%d\n",errorcode); exit(1);}
   } // end-block




   { // begin-block
     int      errorcode   = 0;
     int      iStage      = 5;
     int      nBlockEvents= 2;
     int      iModifyRule = LS_REPLACE;
     double   padProb[]   =
     {
         0.5,   0.5,  -1
     };
     int      pakStart[]  =
     {
           0,     1,     2, -1
     };
     int      *paiRows   = NULL;
     int      *paiCols   = NULL;
     int      paiStvs[]   =
     {
           4,     4,  -1
     };
     double  padVals[]   =
     {
       -0.08,  0.07,  -1
     };
     errorcode=LSaddDiscreteBlocks(pModel,iStage,nBlockEvents,
           padProb,pakStart,paiRows,paiCols,paiStvs,padVals,iModifyRule);

     if (errorcode !=0) { fprintf(stdout,"\nError=%d\n",errorcode); exit(1);}
   } // end-block


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


   if (1==0)
   {
     int      errorcode   = 0;
     int   panSampleSize[]   =
     {
                0,   6,  6, 6, 6, 6,   -1
     };
     /* Load sample sizes per stage */
     errorcode=LSloadSampleSizes(pModel,panSampleSize);
      if (errorcode !=0) { fprintf(stdout,"\nError=%d\n",errorcode); exit(1);}
   } // end event


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

     /* E[value of perfect information] */
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
       if (numStocPars && 0)
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
