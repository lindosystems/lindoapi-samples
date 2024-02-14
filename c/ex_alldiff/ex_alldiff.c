/*
###################################################################
#                       LINDO-API
#                    Sample Programs
#                  Copyright (c) 2017
#
#         LINDO Systems, Inc.           312.988.7422
#         1415 North Dayton St.         info@lindo.com
#         Chicago, IL 60622             http://www.lindo.com
###################################################################

  File   : ex_alldiff.c

  Purpose: Read a model with ALLDIFF constraints from an MPI file
  and display ALLDIFF data and the solution
*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
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
         printf("Errorcode=%d:  %s\n", nErrorCode, \
          cErrorMessage); \
      } else {\
         printf( "Fatal Error\n"); \
      } \
      goto ErrReturn; \
   } \

#define APIVERSION \
{\
    char szVersion[255], szBuild[255];\
    LSgetVersionInfo(szVersion,szBuild);\
    printf("\nLINDO API Version %s built on %s\n",szVersion,szBuild);\
}\


int main(int argc, char **argv)
{
   APIERRORSETUP;

/* declare an instance of the LINDO environment object */
   pLSenv pEnv = NULL;
/* declare an instance of the LINDO model object */
   pLSmodel pModel = NULL;

   int nStatus=LS_STATUS_UNKNOWN;

   // ALLDIFF data
   int       nALLDIFF=0, nSumDim, i, k, panCount[3];
   int       *paiAlldiffDim=NULL;
   int       *paiAlldiffL=NULL;
   int       *paiAlldiffU=NULL;
   int       *paiAlldiffBeg=NULL;
   int       *paiAlldiffVar=NULL;

   // license key container
   char MY_LICENSE_KEY[1024];


   APIVERSION;
  /****************************************************************
   * Step 1: Create a LINDO environment.
   ****************************************************************/
   nErrorCode = LSloadLicenseString("../../../license/lndapi150.lic",MY_LICENSE_KEY);
   if ( nErrorCode != LSERR_NO_ERROR)
   {
      printf( "Failed to load license key (error %d)\n",nErrorCode);
      exit(1);
   }

   APIVERSION;
   pEnv = LScreateEnv ( &nErrorCode, MY_LICENSE_KEY);
   if (nErrorCode == LSERR_NO_VALID_LICENSE)
   {
      printf( "Invalid License Key!\n");
      exit(1);
   }
   APIERRORCHECK;

  /****************************************************************
   * Step 2: Create a model in the environment.
   ****************************************************************/
   pModel = LScreateModel ( pEnv, &nErrorCode);
   APIERRORCHECK;

  /****************************************************************
   * Step 3: Read the model from an MPI file
   ****************************************************************/
   nErrorCode = LSreadMPIFile(pModel,"ex_alldiff.mpi");
   APIERRORCHECK;

   nErrorCode = LSgetInfo(pModel,LS_IINFO_NUM_CONS,panCount+0);
   APIERRORCHECK;
   nErrorCode = LSgetInfo(pModel,LS_IINFO_NUM_VARS,panCount+1);
   APIERRORCHECK;
   nErrorCode = LSgetInfo(pModel,LS_IINFO_NUM_CONT,panCount+2);
   APIERRORCHECK;
   nErrorCode =  LSgetInfo(pModel,LS_IINFO_NUM_ALLDIFF,&nALLDIFF);
   APIERRORCHECK;
   fprintf(stdout,"\nNumber of Cons: \t%d\n", panCount[0]);
   fprintf(stdout,"Number of Vars: \t%d\n", panCount[1]);
   fprintf(stdout,"Number of ALLDIFF: \t%d\n\n", nALLDIFF);

   if (nALLDIFF>0) {
     paiAlldiffDim = malloc(sizeof(int)*nALLDIFF*4+1);
     paiAlldiffL=paiAlldiffDim+nALLDIFF;
     paiAlldiffU=paiAlldiffDim+nALLDIFF*2;
     paiAlldiffBeg=paiAlldiffDim+nALLDIFF*3;

     nErrorCode = LSgetALLDIFFData(pModel,&nSumDim,paiAlldiffDim,paiAlldiffL,paiAlldiffU,paiAlldiffBeg,NULL);
     APIERRORCHECK;
     if (nSumDim!=nALLDIFF) {
       nErrorCode = LSERR_INTERNAL_ERROR;
       APIERRORCHECK;
     }
     nSumDim=0;
     for (i=0; i<nALLDIFF; i++) {
       nSumDim += paiAlldiffDim[i];
     }
     paiAlldiffVar = malloc(sizeof(int)*nSumDim);
     nErrorCode = LSgetALLDIFFData(pModel,&nSumDim,paiAlldiffDim,paiAlldiffL,paiAlldiffU,paiAlldiffBeg,paiAlldiffVar);
     APIERRORCHECK;

     fprintf(stdout,"\n%7s %7s %7s %7s %s\n","ALLDIFF","Set","Lower","Upper"," Variable Indices..");
     fprintf(stdout,"%7s %7s %7s %7s\n","Set","Dim.","Bound","Bound");
     for (i=0; i<nALLDIFF; i++) {
       nSumDim = paiAlldiffDim[i];
       fprintf(stdout,"%7d %7d %7d %7d ",i,nSumDim,paiAlldiffL[i],paiAlldiffU[i]);
       for (k=paiAlldiffBeg[i]; k<paiAlldiffBeg[i]+nSumDim; k++) {
         fprintf(stdout,"%4d ",paiAlldiffVar[k]);
       }
       fprintf(stdout,"\n");
     }//for
   }//if

   nErrorCode = LSsolveGOP(pModel,&nStatus);
   if (nStatus==LS_STATUS_OPTIMAL || nStatus==LS_STATUS_BASIC_OPTIMAL ||
       nStatus==LS_STATUS_LOCAL_OPTIMAL || nStatus==LS_STATUS_FEASIBLE)
   {
     double objval, *primal=NULL;
     primal = malloc(sizeof(double)*panCount[1]) ;
     nErrorCode = LSgetMIPPrimalSolution(pModel, primal);
     APIERRORCHECK;
     nErrorCode = LSgetInfo(pModel, LS_DINFO_GOP_OBJ, &objval);
     if (nErrorCode == LSERR_NO_ERROR)
     {
		 printf("\nObj  = %15.7f %s\n",objval,"          Variable Values..");
		 for (i=0; i<nALLDIFF; i++) {
		   nSumDim = paiAlldiffDim[i];
		   fprintf(stdout,"%7d %7d %7d %7d ",i,nSumDim,paiAlldiffL[i],paiAlldiffU[i]);
		   for (k=paiAlldiffBeg[i]; k<paiAlldiffBeg[i]+nSumDim; k++) {
			 fprintf(stdout,"%4.1f ",primal[paiAlldiffVar[k]]);
		   }
		   fprintf(stdout,"\n");
		 }//for
     }
   } else {
     fprintf(stdout,"\nStatus: %d\n",nStatus);
     APIERRORCHECK;
   }
ErrReturn:

   if (paiAlldiffDim) free(paiAlldiffDim);
   if (paiAlldiffVar) free(paiAlldiffVar);
   /***************************************************************
    * Step 6: Terminate
    ***************************************************************/

   nErrorCode = LSdeleteModel( &pModel);
   nErrorCode = LSdeleteEnv( &pEnv);

  /* Wait until user presses the Enter key */
   printf("Press <Enter> ...");
   getchar();

}
