
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

  File   : ex_sp_corr.c

  Purpose: Build a SP model reading from an SMPI file and induce
  a correlation structure before solving it.

  There are two sample models, GBD and BONDSTOK4 under ./smpi directory.
  Set the following macro to 0 or 1 to indicate the model to solve.

    1: solve gbd model under ./smpi/
    0: solve bonstok4 model under ./smpi/
*/
#define SOLVE_GBD  0


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
   int i,j=0,k=0,i1=0,i2=0;

   char MY_LICENSE_KEY[1024];
   char *mpsfile=NULL, *basfile = NULL, deqFile[255]="", *ptr=NULL;
   char *coreFile,*timeFile,*stocFile, *soluFile;
   char strbuf[1024];

#if SOLVE_GBD
   int CorrelType = LS_CORR_KENDALL;
   // Target correlation coefficients among stochastic parameters {0,1,3,4}.
   // Other stochastic parameters {2,5} are independent.
   //
   //       (0)     (1)           (3)       (4)
   //        1    0.5796        -0.953    0.5409    (0)
   //     0.5796     1          -0.4181   0.6431    (1)
   //    -0.953  -0.4181           1     -0.2616    (3)
   //     0.5409  0.6431        -0.2616       1     (4)
   //
   int nDim = 4;
   int TargetQCnonzeros = 10;
   int TargetQCvarndx1[] = {0, 0, 0, 0, 1, 1, 1,  3, 3, 4};
   int TargetQCvarndx2[] = {0, 1, 3, 4, 1, 3, 4,  3, 4, 4};
   double TargetQCcoef[] = {1, 0.5796, -0.953, 0.5409, //spar0
                            1, -0.4181, 0.6431,        //spar1
                            1, -0.2616,                //spar3
                            1};                        //spar4
#elif 0
   int nDim = 3;
   int TargetQCnonzeros = 6;
   int TargetQCvarndx1[] = {0, 0, 0, 1, 1, 2};
   int TargetQCvarndx2[] = {0, 1, 2, 1, 2, 2};
   double TargetQCcoef[] = {1, 0.2, 0.5, //spar0
                            1, 0.8,      //spar1
                            1};          //spar4
#elif 0
   int nDim = 3;
   int TargetQCnonzeros = 6;
   int TargetQCvarndx1[] = {0, 0, 0, 1, 1, 2};
   int TargetQCvarndx2[] = {0, 1, 2, 1, 2, 2};
   double TargetQCcoef[] = {1, 0.9, 0.5, //spar0
                            1, 0.8,      //spar1
                            1};          //spar4
#else
   int CorrelType = LS_CORR_PEARSON;
   int nDim = 6;
   int TargetQCnonzeros = 3;
   int TargetQCvarndx1[] = {0, 2, 4};
   int TargetQCvarndx2[] = {1, 3, 5};
   double TargetQCcoef[] = {0.9,
                            0.9,
                            0.9};          //spar4
#endif
   // Observed correlation coefficients among stochastic variables
   int ObservedQCnonzeros;
   int *ObservedQCvarndx1=NULL;
   int *ObservedQCvarndx2=NULL;
   double *ObservedQCcoef=NULL;


   double dObj,dEvpi, *padOutcome=NULL, dProb=0, *pX=NULL, *pCIX=NULL;

    /* Declare an instance of the LINDO environment object */
   pLSenv pEnv = NULL;

    /* Declare instances of the LINDO model object */
   pLSmodel pModel=NULL, edeModel=NULL, ideModel=NULL;

   pLSsample *paSample = NULL;


   /*
    *     Allocate misc strings
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
     printf("\nUsage: ex_sp_corr\n\n");
     exit(1);
   }

   nErrorCode = LSloadLicenseString("../../../license/lndapi140.lic",MY_LICENSE_KEY);
   APIERRORCHECK;

   pEnv = LScreateEnv ( &nErrorCode, MY_LICENSE_KEY);
   if ( nErrorCode == LSERR_NO_VALID_LICENSE) {
      printf( "Invalid License Key!\n");
      exit( 1);
   }
   APIERRORCHECK;
   APIVERSION;



   /*
    *     Create a model in the environment.
    */
   pModel = LScreateModel ( pEnv, &nErrorCode);
   APIERRORCHECK;

   LSsetModelIntParameter(pModel,LS_IPARAM_NLP_LINEARZ,1);
   //LSsetModelIntParameter(pModel,LS_IPARAM_STOC_PRINT_LEVEL,0);

   if (1==1)
   {
     /* Install a log function */
     nErrorCode = LSsetModelLogfunc(pModel, (printModelLOG_t) print_line_log, NULL);
   }
   else
   {
     /* Install a callback function */
     nErrorCode = LSsetCallback(pModel,(cbFunc_t) progress_log, NULL);
   }
   APIERRORCHECK;


   // Read the model
#if SOLVE_GBD
   nErrorCode = LSreadSMPIFile(pModel,"smpi/gbd.mpi",
                                      "smpi/gbd.time",
                                      "smpi/gbd_normal.stoch");
#else
   nErrorCode = LSreadSMPIFile(pModel,"smpi/bondstok4.mpi",
                                      "smpi/bondstok4.time",
                                      "smpi/bondstok4_cont.stoch");
#endif
   APIERRORCHECK;

   // Load a target correlation matrix
   if (1)
   {
      nErrorCode = LSloadCorrelationMatrix(pModel,nDim,CorrelType,
        TargetQCnonzeros,TargetQCvarndx2,TargetQCvarndx1,TargetQCcoef);
      APIERRORCHECK;
   }

   // Load a sample
   if (1)
   {
#if SOLVE_GBD
     int itmp[] = { 1, 16};
#else
     int itmp[] = { 1, 6, 6, 6, 6, 6, 6};
#endif
     // Try different seeds when repeating runs with sampling
     //LSsetModelIntParameter(pModel,LS_IPARAM_STOC_RG_SEED,1033);
     //LSsetModelStocIntParameter(pModel,LS_IPARAM_STOC_SAMP_CONT_ONLY,1);
     nErrorCode = LSloadSampleSizes(pModel,itmp);
     APIERRORCHECK;
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

     if (1)
     {
       paSample = malloc(numStocPars*sizeof(pLSsample));
       for (i=0; i<numStocPars; i++)
       {
         paSample[i] = LSgetStocParSample(pModel,i,-1,-1,&nErrorCode);
         APIERRORCHECK;
       }

       if (1)
       {
         printf("\nOriginal Sample\n");
         for (i=0; i<numStocPars; i++)
         {
           // sample and size
           nErrorCode = LSsampGetPointsPtr(paSample[i],&j,&pX);
           if (nErrorCode==LSERR_NO_ERROR)
           {
             printf("\n%2d,",i);
             for (k=0; k<j; k++) printf("%8.2f",pX[k]);
           }
         }

         printf("\nCorrelation Induced (CI) sample\n");
         for (i=0; i<numStocPars; i++)
         {
           nErrorCode = LSsampGetCIPointsPtr(paSample[i],&j,&pCIX);
           if (nErrorCode==LSERR_NO_ERROR)
           {
             printf("\n%2d,",i);
             for (k=0; k<j; k++) printf("%8.2f",pCIX[k]);
           }
         }
         fflush(stdout);
       }

       if (1)
       {
         printf("\nCorrelations of Original sample\n");
         ObservedQCvarndx1=malloc(sizeof(int)*numStocPars*numStocPars);
         ObservedQCvarndx2=malloc(sizeof(int)*numStocPars*numStocPars);
         ObservedQCcoef=malloc(sizeof(double)*numStocPars*numStocPars);

         nErrorCode = LSgetCorrelationMatrix(pModel,0,CorrelType,
                                             &ObservedQCnonzeros,
                                             ObservedQCvarndx1,
                                             ObservedQCvarndx2,
                                             ObservedQCcoef);
         APIERRORCHECK;

         for (i=0; i<ObservedQCnonzeros; i++)
         {
           printf("C[%d][%d] = %13.5f\n",
             ObservedQCvarndx1[i],ObservedQCvarndx2[i],ObservedQCcoef[i]);
         }
         fflush(stdout);

         printf("\nCorrelations of CI sample\n");
         nErrorCode = LSgetCorrelationMatrix(pModel,1,CorrelType,
                                             &ObservedQCnonzeros,
                                             ObservedQCvarndx1,
                                             ObservedQCvarndx2,
                                             ObservedQCcoef);
         APIERRORCHECK;

         for (i=0; i<ObservedQCnonzeros; i++)
         {
           printf("C[%d][%d] = %13.5f\n",
             ObservedQCvarndx1[i],ObservedQCvarndx2[i],ObservedQCcoef[i]);
         }
         fflush(stdout);
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
   free(paSample);
   free(ObservedQCvarndx1);
   free(ObservedQCvarndx2);
   free(ObservedQCcoef);

  /* Wait until user presses the Enter key */
   printf("Press <Enter> ...");
   getchar();

  return 0;
}
