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

  File   : ex_multis.c
  Purpose: Read a nonconvex nonlinear model from an MPI file and
           optimize with the multistart solver
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

#define APIVERSION \
{\
    char szVersion[255], szBuild[255];\
    LSgetVersionInfo(szVersion,szBuild);\
    printf("\nLINDO API Version %s built on %s\n",szVersion,szBuild);\
}\

/* main entry point */
/****************************************************************
   Standard callback function to display local and intermediate
   solutions
 ****************************************************************/
int  LS_CALLTYPE print_log(pLSmodel model,int iLoc, void *cbData)
{
  static int siter=0,niter=0;
  static double pobj=0.0;
  static double bestbnd;
  static int status;

  if (iLoc == LSLOC_MSW)
  {
    LSgetProgressInfo(model,iLoc,LS_IINFO_CUR_STATUS,&status);
    LSgetProgressInfo(model,iLoc,LS_IINFO_CUR_ITER,&siter);
    LSgetProgressInfo(model,iLoc,LS_DINFO_CUR_OBJ,&pobj);
    printf("Iters=%6d \tObj=%11.5e \tStatus=%d\n",siter,pobj,status);
  }
  return 0;
} /*print_log*/

int main(int argc, char **argv)
{
   APIERRORSETUP;
   int m, n; /* number of constraints and vars */
   double dObj;
   int    status;
/* declare an instance of the LINDO environment object */
   pLSenv pEnv=NULL;
/* declare an instance of the LINDO model object */
   pLSmodel pModel;

  char MY_LICENSE_KEY[1024];

 /*****************************************************************
  * Step 1: Create a model in the environment.
  *****************************************************************/
  nErrorCode = LSloadDefaultLicenseString(MY_LICENSE_KEY);
  APIERRORCHECK;

  APIVERSION;

  pEnv = LScreateEnv ( &nErrorCode, MY_LICENSE_KEY);
  APIERRORCHECK;
  /****************************************************************
   * Step 2: Create a model in the environment.
   ****************************************************************/
   pModel = LScreateModel ( pEnv, &nErrorCode);
   APIERRORCHECK;
  /****************************************************************
   * Step 3: Read the model from an MPS file and get the model size
   ****************************************************************/
   nErrorCode = LSreadMPIFile(pModel,"../../data/peaks.mpi");

   if (nErrorCode != LSERR_NO_ERROR) {
     printf("\n Bad  MPI  format\n");
   } else {
     printf("Reading MPI format. \n\n");
   }
   APIERRORCHECK;

   nErrorCode = LSgetInfo(pModel,LS_IINFO_NUM_VARS,&n);
   APIERRORCHECK;
   nErrorCode = LSgetInfo(pModel,LS_IINFO_NUM_CONS,&m);
   APIERRORCHECK;
   /***************************************************************
    * Step 4: Optimize the model
    ***************************************************************/
   status = LS_STATUS_UNKNOWN;
   LSsetModelDouParameter(pModel,LS_DPARAM_CALLBACKFREQ,0.5);
   /* Install a callback function */
   LSsetCallback(pModel,(cbFunc_t) print_log, NULL);
   LSsetModelIntParameter(pModel,LS_IPARAM_NLP_SOLVER,LS_NMETHOD_MSW_GRG);
   /* optimize */
   printf("\tSearching for Best Local Solution\n\n");
   nErrorCode = LSoptimize( pModel, LS_METHOD_FREE, &status);
   /***************************************************************
    * Step 5: Access the final solution if optimal or feasible
    ***************************************************************/
   if (status == LS_STATUS_OPTIMAL ||
       status == LS_STATUS_LOCAL_OPTIMAL ||
       status == LS_STATUS_FEASIBLE )
   {
     double *primal = NULL, *dual = NULL;
     int    j, nCont;
     primal = (double *) malloc(n*sizeof(double));
     dual   = (double *) malloc(m*sizeof(double));

     nErrorCode = LSgetInfo(pModel, LS_IINFO_NUM_CONT, &nCont);
     APIERRORCHECK;

     if (nCont < n)
     {
       printf ("\n *** Integer Solution Report *** \n");
       nErrorCode = LSgetInfo(pModel, LS_DINFO_POBJ, &dObj);
         APIERRORCHECK;
       nErrorCode = LSgetMIPPrimalSolution( pModel,primal);
         APIERRORCHECK;
       nErrorCode = LSgetMIPDualSolution( pModel,dual);
         APIERRORCHECK;
     }
     else
     {
       printf ("\n ***  Solution Report *** \n");
       nErrorCode = LSgetInfo(pModel, LS_DINFO_POBJ, &dObj);
         APIERRORCHECK;
       nErrorCode = LSgetPrimalSolution( pModel,primal);
         APIERRORCHECK;
       nErrorCode = LSgetDualSolution( pModel,dual);
         APIERRORCHECK;
     }

     printf ("\n Objective = %f \n", dObj);
     printf ("\n Primal Solution\n");
     for (j = 0; j<n; j++)
       printf("\tprimal[%d] = %18.10e\n",j, primal[j]);

     printf ("\n Dual Solution\n");
     for (j = 0; j<m; j++)
         printf("\tdual[%d] = %18.10e\n",j, dual[j]);

     free(primal);
     free(dual);
   }

Terminate:
   /***************************************************************
    * Step 6: Terminate
    ***************************************************************/
   nErrorCode = LSdeleteModel( &pModel);
   nErrorCode = LSdeleteEnv( &pEnv);

  /* Wait until user presses the Enter key */
   printf("Press <Enter> ...");
   getchar();

}
