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

  File   : ex_sp_newsboy.c

  Purpose: Build a SP model via an instruction list and solve it.
  There are 4 cases which can be toggled with an #ifdef block
  during model set up.

  Example:
  A two-period stochastic newsboy problem.

  ! Stochastic Newsvendor Model;
  DATA:
   C = 30;  ! Purchase cost/unit;
   P = 5;   ! Penalty shortage cost/unit unsatisfied demand;
   H = 10;  ! Holding cost/unit leftover;
   V = 60;  ! Revenue per unit sold;

  ! Random demand (D);
   D = 63;
  ! Random refund per return;
   R = 9;
  ENDDATA

  MAX = Z;
  ! Units bought, X, Buy at least 1 (serves as a dummy constraint for stage 1);
  [Row1] X >= 1;
  ! Inventory (I) and Lost Sales (L);
  [Row2]  I = X + L - D;
  ! Units sold S, and inventory left over, I;
  [Row3]  S = X - I;
  ! Y units returned to vendor for a possible refund, and E kept;
  [Row4]  Y + E = I;
  ! Profit, to be maximized;
  [Profit]  Z = V*S - C*X - H*I - P*L + Y*R - H*E;

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

/* Define error checking with argument */
#define APIERRORCHECK2(nErrorCode)  \
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
    printf("%s",line);
    fflush(stdout);
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


   /*
    *     Create a LINDO environment.
    */
   if (argc == 0) {
     printf("\nUsage: ex_sp_newsboy\n\n");
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


   /*
    *     Create a model in the environment.
    */
   pModel = LScreateModel ( pEnv, &nErrorCode);
   APIERRORCHECK;

   LSreadModelParameter(pModel,"lindo.par");

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
    * Set up SP model
    *
    * Edit the MPI file to see how stochastic parameters {Q}
    * marked with EP_PUSH_SVAR macro.
    */
#if 0 // Case 0 (undocumented)
   nErrorCode = LSreadMPIFile(pModel,"smpi/newsboy_nlp.mpi");
   #include "src/nlp_newsboy_time.c"
   #include "src/nlp_newsboy_stoc.c"
#elif 0 // Case 1 (see Chapter 8 in the manual)
   #include "src/newsboy_core.c"
   #include "src/newsboy_time.c"
   #include "src/newsboy_stoc.c"
#elif 0 // Case 2 (see Chapter 8 in the manual)
   #include "src/newsboy_core.c"
   #include "src/newsboy_time.c"
   #include "src/newsboy_stoc_normal.c"
#elif 1 // Case 2 (see Chapter 8 in the manual)
   #include "src/newsboy_matrix_core.c"
   #include "src/newsboy_matrix_time.c"
   #include "src/newsboy_matrix_stoc_normal.c"
#elif 0 // Case 3 (see Chapter 8 in the manual)
   #include "src/newsboy_core.c"
   #include "src/newsboy_time.c"
   #include "src/newsboy_scen.c"
#elif 0 // Case 4 (see Chapter 8 in the manual)
   #include "src/newsboy_core.c"
   #include "src/newsboy_agg_time.c"
   #include "src/newsboy_agg_stoc.c"
#endif


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
     nErrorCode = LSgetInfo(pModel,LS_IINFO_NUM_SPARS,&i1);
     if (i1)
     {
       nErrorCode =  LSwriteSMPIFile(pModel,"out/foo.mpi","out/foo.time","out/foo.stoc");
     }
     else
     {
       nErrorCode =  LSwriteSMPSFile(pModel,"out/foo_mat.core","out/foo_mat.time","out/foo_mat.stoc",LS_FORMATTED_MPS);
     }
     APIERRORCHECK;
   }


   /*
    *     Get deterministic equivalents (and optionally export them)
    */
   if (0)
   {
     nErrorCode = LSgetInfo(pModel,LS_IINFO_NUM_SPARS,&i1);
     ptr=deqFile;
     strcpy(ptr,stocFile);
     if (0)
     {
       ideModel = LSgetDeteqModel(pModel,LS_DETEQ_IMPLICIT,&nErrorCode);
       if (i1)
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
       if (i1)
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
     //LSsetModelIntParameter(pModel,LS_IPARAM_STOC_DETEQ_TYPE,LS_DETEQ_EXPLICIT);
     LSsetModelIntParameter(pModel,LS_IPARAM_STOC_TOPOPT,LS_METHOD_GOP);
     //LSsetModelIntParameter(pModel,LS_IPARAM_STOC_TOPOPT,LS_METHOD_MULTIS);
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
     nErrorCode = LSgetStocInfo(pModel,LS_IINFO_STOC_NUM_SCENARIOS, 0, &numScens);
     APIERRORCHECK;

     printf("\n\nOutcomes for spars by scenarios\n\n");
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
#if 1
   printf("Press <Enter> ...");
   getchar();
#endif
  return 0;
}
