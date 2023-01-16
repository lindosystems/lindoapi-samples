/*
###################################################################
#                       LINDO-API
#                    Sample Programs
#                  Copyright (c) 2001-2018
#
#         LINDO Systems, Inc.           312.988.7422
#         1415 North Dayton St.         info@lindo.com
#         Chicago, IL 60622             http://www.lindo.com
###################################################################

  File   : ex_ccp_kilosa.c

  Purpose: Setup and solve a CCP model with a user-defined function
  of random input.  In particular, we show how dependent
  stochastic parameters with arbitary relationships with a set of
  independent stochastic parameters can be used to model a stochastic
  program. This involes generating samples for the independent
  variables and computing the dependent variables explictly with
  LSaddUserDistr() interface while building the stochastic program.


  Example:
  A Kilosa farmer can grow maize and sorghum on his land, and needs to decide
  how many hectares  should be allocated to each. The yields are uncertain
  due to rainfall as well as white noise.

  Decision Variables:
  xm = acreage of maize in hectares
  xs = acreage of sorghum in hectares

  Independent stochastic Parameters;
    ksi_r ~ Normal(515.5,137.0): random rainfall during the growing season (mm)
    eps_m ~ Normal(  0.0, 10.0): white noise in the yield of maize
    eps_s ~ Normal(  0.0, 10.0): white noise in the yield of sorghum.

  Dependendent stochastic parameters;
    ym: random yield per hectare of maize (in 100 Kgs)
    ys: random yield per hectare of sorghum (in 100 Kgs)

  It is known that
   - 100 kgs of maize contains 2.8 . 105 Kcal and 6.4 kg of protein
   - 100 kgs of sorghum contains 2.8 . 105 Kcal and 8 kg of protein.;

  Objective: minimize total hectares  allocated for farming.

  MODEL:
    [OBJ] Min = xm + xs;
    [CALORIES] 2.8*ym*xm + 2.8*ys*xs > 44;
    [PROTEIN ] 6.4*ym*xm + 8.0*ys*xs > 89;

  A regression analysis suggest the following relationship
  between yields and independent random factors.

    ym = 0.020*ksi_r - 1.65 + eps_m;
    ys = 0.008*ksi_r + 5.92 + eps_s;

  According to this relationship, it is possible to
  have negative values ym and ym for a particular realization
  of (ksi_r, eps_m, eps_s). This would suggest negative yields
  which is not realistic. Thus we use a user-defined distribution
  function to sample realizations for ym and ys truncating
  any negative realizations to zero. This process is while setting
  up the problem.

  About alternatives formulations:
  1. A simple alternative would be to substitute ym and ys
  with the associated expressions involving (ksi_r, eps_m, eps_s) and
  formulate the problem with these stochastic parameters. Unfortunately,
  this would likely lead to negative ym and ys during which would
  invalidate the overall model.

  2. An alternative approach would be to fit a multivariate distribution
  for (ym,ys) directly such that nonnegative values for ym and ys
  are (almost) zero. Correlations between ym and ys can handled
  by inducing correlations as in sample application 'ex_sp_corr'.

  3. Another alternative would be to assume ym and ys to be independent
  in which case a conic formulation would be possible, but this
  may not be as realistic as the core cases.

  Ref:
  1) Schweigman, C.: 1985, `OR in development countries'.
     Khartoum University press,Khartoum.
  2) van der Vlerk, M. http://mally.eco.rug.nl/lnmb/cases.pdf.

*/
#ifdef _MSC_VER
#pragma warning( once : 4702 )
#pragma warning( once : 4996 )
#endif

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

/* LINDO API header file */
#include "lindo.h"

/* Parse command line options */
int LS_CALLTYPE LSgetCLOpt(pLSenv pEnv, int nArgc, char **pszArgv, char *pszOpt);
int LS_CALLTYPE LSgetCLOptArg(pLSenv pEnv, char **pszOptArg);
int LS_CALLTYPE LSgetCLOptInd(pLSenv pEnv, int *pnOptInd);

/* Define a macro to declare variables for
    error checking */
#define APIERRORSETUP  \
   int errorcode; \
   char cErrorMessage[LS_MAX_ERROR_MESSAGE_LENGTH] \

/* Define a macro to do our error checking */
#define APIERRORCHECK  \
   if (errorcode) \
   { \
      if ( pEnv) \
      { \
         LSgetErrorMessage( pEnv, errorcode, \
          cErrorMessage); \
         printf("Errorcode=%d:  %s\n", errorcode, \
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



int parseopt (pLSenv pEnv, int argc, char **argv, void *optMask)
{
  int tsum=0;
  char *optarg;
  int optind=1;
  int errflg = 0, nstg=0, errorcode=LSERR_NO_ERROR;
  char *ofile = NULL, *ptr=NULL;
  char seps[]   = ",", c, strbuf[255]="", *token=NULL;
  pLSmodel pModel = (pLSmodel) optMask;
  //double dargs[10];
  int k=0;


  while ((c = LSgetCLOpt(pEnv,argc, argv, "?v:r:n:")) != EOF)
  {
    LSgetCLOptArg(pEnv,&optarg);
    LSgetCLOptInd(pEnv,&optind);
    switch (c)
    {
    case 'n':
      k = atoi(optarg);
      LSsetModelStocIntParameter(pModel,LS_IPARAM_STOC_NSAMPLE_STAGE,k);
      break;
    case 'v':
      k = atoi(optarg);
      errorcode = LSsetModelStocIntParameter(pModel,LS_IPARAM_STOC_PRINT_LEVEL,k);
      break;
    case 'r':
      k = atoi(optarg);
      LSsetModelStocIntParameter(pModel,LS_IPARAM_STOC_RG_SEED,k);
      break;

    case '?':
      errflg=-1;
    }
  }


  if (errflg)
  {
    (void)fprintf(stderr,
      "\nusage: ex_ccp_kilosa  [-?| -r <seed> | -n <sample-size>]\n\n"
      "             -?              display options.  \n"
      "             -r=             rg seed.  \n"
      "             -n=             sample size.  \n"
      "             -v=             print level.  \n"
      );
    if (errflg>0)
      errorcode = LSERR_ERROR_IN_INPUT;
    else
      errorcode = -1; //display usage and terminate
  }
  for ( ; optind < argc; optind++)
    (void)printf("%s\n", argv[optind]);


  return errorcode;
}

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

/*
 * function to compute random yields ym and ys from random input
 * with known distributions.
 *
 */
int LS_CALLTYPE UserDistr(pLSsample pSample, int nFuncType, double *padInput, int nInput, double *pdOutput, void *userData)
{
  int errorcode = 0;
  static pLSsample pSamp = NULL;
  double ksi_r, eps_m, eps_s;
  int iStv = (*((int *) userData));

  if (nInput<2)
  {
    errorcode = LSERR_INTERNAL_ERROR;
    goto ErrReturn;
  }
  if (nFuncType != LS_USER)
  {
    errorcode = LSERR_INTERNAL_ERROR;
    goto ErrReturn;
  }

  if (iStv==0)
  {
    ksi_r = padInput[0];
    eps_m = padInput[1];
    *pdOutput = 0.020*ksi_r - 1.65 + eps_m;
    //yields cannot be negative, set them to zero
    if ((*pdOutput)<0) *pdOutput=0;
  }
  else if (iStv==1)
  {
    ksi_r = padInput[0];
    eps_s = padInput[1];
    *pdOutput = 0.008*ksi_r + 5.92 + eps_s;
    //yields cannot be negative, set them to zero
    if ((*pdOutput)<0) *pdOutput=0;
  }

ErrReturn:
  return errorcode;
}

/*
 *      Shared functions..
 *
 */
#include "../common/commonutils.c"


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
   pLSmodel pModel=NULL, edeModel=NULL, cdeModel=NULL;

   pLSsample pSample_KSI_R=NULL, pSample_EPS_M=NULL, pSample_EPS_S=NULL;
   pLSsample paSampleBuf[2];
   int userData_M;
   int userData_S;


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
     printf("\nUsage: ex_ccp_kilosa\n\n");
     exit(1);
   }

   errorcode = LSloadLicenseString("../../../license/lndapi140.lic",MY_LICENSE_KEY);
   APIERRORCHECK;

   pEnv = LScreateEnv ( &errorcode, MY_LICENSE_KEY);
   if ( errorcode == LSERR_NO_VALID_LICENSE) {
      printf( "Invalid License Key!\n");
      exit( 1);
   }
   APIERRORCHECK;

   /*
    *     Create a model in the environment.
    */
   pModel = LScreateModel ( pEnv, &errorcode);
   APIERRORCHECK;

   LSreadModelParameter(pModel,"lindo.par");

   errorcode = parseopt(pEnv,argc,argv,pModel);
   if (errorcode<0) { errorcode = LSERR_NO_ERROR; goto Terminate;}
   APIERRORCHECK;

   errorcode = LSsetModelIntParameter(pModel,LS_IPARAM_NLP_LINEARZ,1);
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

   /* Read core model in MPI format */
   errorcode = LSreadMPIFile(pModel,"smpi/kilosa-fun.core");
   APIERRORCHECK;

   /* Load stage/time structure for rows,columns and stochastic params */
   { // begin time data
     int      errorcode   = LSERR_NO_ERROR;
     int      numStages   = 1;

     /* Stage indices of columns */
     int      colStages[]   = { 0,  0,   -1 };

     /* Stage indices of rows */
     int      rowStages[]   = { 0,  0,   -1 };

     /* Stage indices of stochastic parameters */
     int      panSparStage[] = { 0,  0,  -1 };

     /* Default values of stochastic parameters (optional)*/
     double   padSparValue[] = { -91, -92,   -1 };

     /* Load stage data */
     errorcode=LSloadStageData(pModel,numStages,rowStages,colStages);
     APIERRORCHECK;

     errorcode=LSloadStocParData(pModel,panSparStage,padSparValue);
     APIERRORCHECK;

   } // end time data

   /*
    * Set up LSsample objects to express yields through the callback function.
    */
   // Rainfall affecting both ym and ys
   pSample_KSI_R = LSsampCreate(pEnv, LSDIST_TYPE_NORMAL, &errorcode); APIERRORCHECK;
   errorcode = LSsampSetDistrParam(pSample_KSI_R, 0, 515.5); APIERRORCHECK; // mu
   errorcode = LSsampSetDistrParam(pSample_KSI_R, 1, 137.0); APIERRORCHECK; // std

   // White-noise for ym
   pSample_EPS_M = LSsampCreate(pEnv, LSDIST_TYPE_NORMAL, &errorcode);APIERRORCHECK;
   errorcode = LSsampSetDistrParam(pSample_EPS_M, 0, 0.0); APIERRORCHECK; // mu
   errorcode = LSsampSetDistrParam(pSample_EPS_M, 1,10.0); APIERRORCHECK; // std

   // White-noise for ym
   pSample_EPS_S = LSsampCreate(pEnv, LSDIST_TYPE_NORMAL, &errorcode); APIERRORCHECK;
   errorcode = LSsampSetDistrParam(pSample_EPS_S, 0, 0.0); APIERRORCHECK; // mu
   errorcode = LSsampSetDistrParam(pSample_EPS_S, 1,10.0); APIERRORCHECK; // std


   // Set up ym and ys
   {// begin user-defined event
     int      errorcode = 0;
     int      iRow      = 0;
     int      jCol      = -8;
     int      iStv      = 0;
     int      iModifyRule = LS_REPLACE;

     // pass the samples set up above to the event
     paSampleBuf[0] = pSample_KSI_R;
     paSampleBuf[1] = pSample_EPS_M;
     userData_M = iStv;
     errorcode=LSaddUserDist(pModel,iRow,jCol, iStv,UserDistr,2,paSampleBuf,&userData_M,iModifyRule);
     APIERRORCHECK;
   } // end user-defined event


   {// begin user-defined event
     int      errorcode = 0;
     int      iRow      = 1;
     int      jCol      = -8;
     int      iStv      = 1;
     int      iModifyRule = LS_REPLACE;

     // pass the samples set up above to the event
     paSampleBuf[0] = pSample_KSI_R;
     paSampleBuf[1] = pSample_EPS_S;
     userData_S = iStv;
     errorcode=LSaddUserDist(pModel,iRow,jCol, iStv,UserDistr,2,paSampleBuf,&userData_S,iModifyRule);
     APIERRORCHECK;
   } // end user-defined event


   {// begin chance constraint
     int      errorcode = 0;
     int      nCons     = 1;
     double   dProb     = 0.9;
     int      iSense    = 71;
     double   dObjWeight = 0.0;
     int    paiRows[]   =
     {
       0,   -1
     };

     errorcode=LSaddChanceConstraint(pModel,iSense,nCons,paiRows,dProb,dObjWeight);
     APIERRORCHECK;
   } // end chance constraint


   {// begin chance constraint
     int      errorcode = 0;
     int      nCons     = 1;
     double   dProb     = 0.9;
     int      iSense    = 71;
     double   dObjWeight = 0.0;
     int    paiRows[]   =
     {
       1,   -1
     };

     errorcode=LSaddChanceConstraint(pModel,iSense,nCons,paiRows,dProb,dObjWeight);
     APIERRORCHECK;
   } // end chance constraint

   // optionally specify a correlation structure (default none).
   if (0)
   {

      int errorcode   = 0;
      int nDim = 4;
      int QCnonzeros = 10;
      int QCvarndx1[] = {0, 0, 0, 0, 1, 1, 1, 3, 3, 4, -1};
      int QCvarndx2[] = {0, 1, 3, 4, 1, 3, 4, 3, 4, 4, -1};
      double QCcoef[] = {1, 0.5796, -0.953, 0.5409, 1, -0.4181, 0.6431, 1, -0.2616, 1, -1};

      errorcode = LSgetInfo(pModel,LS_IINFO_NUM_SPARS,&numStocPars);
      APIERRORCHECK;
      errorcode = LSloadCorrelationMatrix(pModel,nDim,LS_CORR_PEARSON,QCnonzeros,QCvarndx1,QCvarndx2,QCcoef);
      APIERRORCHECK;
   }

   /* Load sample sizes per stage (default none) */
   if (1)
   {/* Sample sizes per stage */
     int   errorcode         = 0, itmp;
     int   panSampleSize[]   = { 30,   -1};
     errorcode=LSgetModelStocIntParameter(pModel,LS_IPARAM_STOC_NSAMPLE_STAGE,&itmp);
     if (errorcode==LSERR_NO_ERROR && itmp>0)
     {
       panSampleSize[0]=itmp;
     }
     errorcode=LSloadSampleSizes(pModel,panSampleSize);
     APIERRORCHECK;
   }



   /*
    *   Get and display SP statistics
    */
   errorcode = LSgetInfo(pModel,LS_IINFO_NUM_VARS,&numVars);
   APIERRORCHECK;
   errorcode = LSgetInfo(pModel,LS_IINFO_NUM_CONS,&numCons);
   APIERRORCHECK;
   errorcode = LSgetInfo(pModel,LS_IINFO_NUM_CONT,&numCont);
   APIERRORCHECK;
   LSgetInfo(pModel,LS_IINFO_NUM_SPARS,&numStocPars);
   errorcode = LSgetStocInfo(pModel,LS_IINFO_STOC_NUM_STAGES   , 0, &numStages);
   APIERRORCHECK;
   errorcode = LSgetStocInfo(pModel,LS_IINFO_STOC_NUM_NODES    , 0, &numNodes);
   APIERRORCHECK;
   errorcode = LSgetStocInfo(pModel,LS_IINFO_STOC_NUM_SCENARIOS, 0, &numScens);
   APIERRORCHECK;
   errorcode = LSgetStocInfo(pModel,LS_IINFO_STOC_NUM_ROWS_DETEQC , 0, &numDeqRows);
   APIERRORCHECK;
   errorcode = LSgetStocInfo(pModel,LS_IINFO_STOC_NUM_COLS_DETEQC , 0, &numDeqCols);
   APIERRORCHECK;
   errorcode = LSgetStocInfo(pModel,LS_IINFO_STOC_NUM_ROWS_CORE, 0, &numCoreRows);
   APIERRORCHECK;
   errorcode = LSgetStocInfo(pModel,LS_IINFO_STOC_NUM_COLS_CORE, 0, &numCoreCols);
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
     i1=0; LSgetInfo(pModel,LS_IINFO_NUM_STOCPAR_INSTR,&i1);
     if (i1)
     {
       errorcode =  LSwriteSMPIFile(pModel,"out/foo.mpi","out/foo.time","out/foo.stoc");
     }
     else
     {
       errorcode =  LSwriteSMPSFile(pModel,"out/foo.mps","out/foo.time","out/foo.stoc",LS_FORMATTED_MPS);
     }
     APIERRORCHECK;
   }


   /*
    *     Get deterministic equivalents (and optionally export them)
    */
   if (1)
   {
     i1=0; LSgetInfo(pModel,LS_IINFO_NUM_STOCPAR_INSTR,&i1);
     ptr=deqFile;
     strcpy(ptr,"out/foo");
     if (1)
     {
       cdeModel = LSgetDeteqModel(pModel,LS_DETEQ_CHANCE,&errorcode);
       if (i1)
       {
         ptr = strcat(ptr,"_deq.mpi");
         LSwriteMPIFile(cdeModel,ptr);
       }

       {
         ptr = strcat(ptr,"_deq.ltx");
         LSwriteLINDOFile(cdeModel,ptr);
       }
     }
     APIERRORCHECK;
   }


   /*
    *     Set params and solve the SP
    */
   if (0)
   {
     LSsetModelStocIntParameter(pModel,LS_IPARAM_STOC_TOPOPT,LS_METHOD_GOP);
     LSsetModelStocIntParameter(pModel,LS_IPARAM_STOC_TOPOPT,LS_METHOD_MULTIS);
   }
   errorcode = LSsolveSP(pModel,&nStatus);


   /*
    *     Access the final solution if optimal or feasible
    */
   if (nStatus == LS_STATUS_OPTIMAL ||
       nStatus == LS_STATUS_BASIC_OPTIMAL ||
       nStatus == LS_STATUS_LOCAL_OPTIMAL ||
       nStatus == LS_STATUS_FEASIBLE)
   {
     /* E[objective value] */
     errorcode = LSgetStocInfo(pModel,LS_DINFO_STOC_EVOBJ,0,&dObj);
     APIERRORCHECK;

     /* lower bound on E[value of perfect information] */
     errorcode = LSgetStocInfo(pModel,LS_DINFO_STOC_EVPI,0,&dEvpi);
     //APIERRORCHECK;

     for (j=0; j<numScens; j++)
     {
       soluFile = NULL;
       if (1) errorcode = LSwriteScenarioSolutionFile(pModel,j,soluFile);
       if (0) errorcode = LSwriteScenarioMPIFile(pModel,j,NULL);
     }

     errorcode = DisplayScenarioOutcomes(pModel);
     APIERRORCHECK;

     if (1) errorcode = LSwriteScenarioMPIFile(pModel,-2,NULL);
   }
   else
   {
     char strbuf[255];
     LSgetErrorMessage(pEnv,errorcode,strbuf);
     printf ("\n Optimization failed. nStatus = %d ",nStatus);
     printf ("\n Error %d: %s\n",errorcode,strbuf);
   }

Terminate:
   LSsampDelete(&pSample_KSI_R);
   LSsampDelete(&pSample_EPS_M);
   LSsampDelete(&pSample_EPS_S);
   errorcode = LSdeleteModel( &pModel);
   errorcode = LSdeleteEnv( &pEnv);
   free(numStageRows);
  /* Wait until user presses the Enter key */
  printf("Press <Enter> ...");
  getchar();

  return 0;
}
