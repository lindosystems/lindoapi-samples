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

@file   : ex_src.c

@purpose: Solve an LP/QCP/CONIC model using a C file exported
with LSwriteSrcFile

*/


#include <stdio.h>
#include <stdlib.h>
#include <math.h>
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
      printf("nErrorCode=%d:  %s\n", nErrorCode, \
      cErrorMessage); \
      } else {\
      printf( "Fatal Error\n"); \
      } \
      exit(1); \
   } \

#define APIVERSION \
{\
  char szVersion[255], szBuild[255];\
  LSgetVersionInfo(szVersion,szBuild);\
  printf("\nLINDO API Version %s built on %s\n",szVersion,szBuild);\
}\

/***************************************************************
*          Set up an output log function.
*/
static void LS_CALLTYPE print_line(pLSmodel model,
                                   char *line, void *notting)
{
  if (line)
  {
    printf("%s",line);
  } /*if*/
} /*print_line*/



/***************************************************************
*                    Main entry point
*/
int main()

{
  APIERRORSETUP;
  pLSenv pEnv = NULL;
  pLSmodel pModel;
  char MY_LICENSE_KEY[1024];
  int i, nStatus, nvars, ncons, ncont;
  double objval=0.0, *primal=NULL;

  /*
  * >>> Step 1 <<< Create a LINDO environment.
  */

  nErrorCode = LSloadLicenseString(
    "../../../license/lndapi150.lic",MY_LICENSE_KEY);
  if ( nErrorCode != LSERR_NO_ERROR)
  {
    printf( "Failed to load license key (error %d)\n",nErrorCode);
    exit( 1);
  }

  APIVERSION;
  pEnv = LScreateEnv ( &nErrorCode, MY_LICENSE_KEY);
  if ( nErrorCode == LSERR_NO_VALID_LICENSE)
  {
    printf( "Invalid License Key!\n");
    exit( 1);
  }
  APIERRORCHECK;

  /*
  * >>> Step 2 <<< Create a model in the environment.
  */
  pModel = LScreateModel(pEnv,&nErrorCode);
  APIERRORCHECK;

  nErrorCode = LSsetModelLogfunc(pModel,print_line,NULL);
  APIERRORCHECK;

#if 0
#include "qp1.c"
#elif (defined SEMI_LP_MPI)
#include "semi_lp_mpi.c"
#elif (defined SEMI_LP_MAT)
#include "semi_lp_mat.c"
#else
#include "uncap.c"
#endif

#if (0>2)
  nErrorCode = LSoptimize(pModel, LS_METHOD_FREE, NULL);
#elif (1>2)
  nErrorCode = LSoptimizeQP(pModel, NULL);
#else
  nErrorCode = LSsolveMIP(pModel, NULL);
#endif
  APIERRORCHECK;


  /*
  * >>> Step 6 <<< Retrieve the solution
  */

  /* Report the status of solution */
  nErrorCode = LSgetInfo(pModel, LS_IINFO_NUM_VARS,&nvars);
  APIERRORCHECK;
  nErrorCode = LSgetInfo(pModel, LS_IINFO_NUM_CONT,&ncont);
  APIERRORCHECK;
  primal = (double*)malloc(nvars*sizeof(double));
  nErrorCode = LSgetInfo(pModel, LS_IINFO_NUM_CONS,&ncons);
  APIERRORCHECK;

  if (nvars>ncont) {
    nErrorCode = LSgetInfo(pModel,LS_IINFO_MIP_STATUS,&nStatus);
    APIERRORCHECK;

    nErrorCode = LSgetInfo(pModel,LS_DINFO_MIP_OBJ,&objval);
    APIERRORCHECK;

    nErrorCode = LSgetMIPPrimalSolution(pModel,primal);
    APIERRORCHECK;
  } else {
    nErrorCode = LSgetInfo(pModel,LS_IINFO_MODEL_STATUS,&nStatus);
    APIERRORCHECK;

    nErrorCode = LSgetInfo(pModel,LS_DINFO_POBJ,&objval);
    APIERRORCHECK;

    nErrorCode = LSgetPrimalSolution(pModel,primal);
    APIERRORCHECK;
  }

  printf("\n\n\nSolution status = %d \n",nStatus);
  if (nStatus==LS_STATUS_OPTIMAL || nStatus==LS_STATUS_BASIC_OPTIMAL ||
    nStatus==LS_STATUS_FEASIBLE || nStatus==LS_STATUS_LOCAL_OPTIMAL)
  {
    printf("\n\nPrinting the solution ... \n\n");
    printf("f(x) = %20.15f \n",objval);
    for (i=0;i<nvars;i++)
      printf("  x[%d]  = %20.15f\n",i,primal[i]);
    printf("\n");
  }
  else if (nStatus == LS_STATUS_INFEASIBLE) {
    printf("\n\nNo feasible solution. \n\n");
  } else if (nStatus == LS_STATUS_UNBOUNDED) {
    printf("\n\nUnbounded solution. \n\n");
  }



  /*
  * >>> Step 7 <<< Delete the LINDO environment
  */
  LSdeleteEnv(&pEnv);
  if (primal) free(primal);

  printf("\n\nPress <Enter> ...");
  getchar();

}

