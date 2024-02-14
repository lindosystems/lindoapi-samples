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

  File   : ex_sdp2.c

  Purpose: Set up a SDP model and optimize.
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

static void LS_CALLTYPE print_line_log(pLSmodel pModel, char *line, void *userdata)
{
  if (line)
  {
    printf("%s",line);
  } /*if*/
} /*print_line*/

//
int main(int argc, char **argv)
{
   APIERRORSETUP;
   int m, n; /* number of constraints and vars */
   int nC=0, nB=0, nI=0; /* number of cont, bin. int vars*/
   double dObj;
   int counter = 0, status;

/* declare an instance of the LINDO environment object */
   pLSenv pEnv = NULL;
/* declare an instance of the LINDO model object */
   pLSmodel pModel, pModelR=NULL;

   char MY_LICENSE_KEY[1024];


  /****************************************************************
   * Step 1: Create a LINDO environment.
   ****************************************************************/
   nErrorCode = LSloadLicenseString("../../../license/lndapi150.lic",MY_LICENSE_KEY);
   APIERRORCHECK;
   APIVERSION;

   pEnv = LScreateEnv ( &nErrorCode, MY_LICENSE_KEY);
   if ( nErrorCode == LSERR_NO_VALID_LICENSE) {
      printf( "Invalid License Key!\n");
      exit( 1);
   }
   APIERRORCHECK;

  /****************************************************************
   * Step 2: Create a model in the environment.
   ****************************************************************/
   pModel = LScreateModel ( pEnv, &nErrorCode);
   APIERRORCHECK;

  /****************************************************************
   * Step 3: Read the model from a LINDO file and get the model size
     MODEL:
     MAX= X11 + 2 * X22 + 3 * X33 + 4 * X44;
          X11 +     X22                               = 10;
                    X22 + 5 * X33 + 6 * X44 + 4 * X43 = 20;
     END
     FREE Xij for all ij
   ****************************************************************/
#if 0
   nErrorCode = LSreadLINDOFile(pModel,"posd-ext.ltx");
   APIERRORCHECK;
   if (1)
   {
     char varType[] = "CCCCCCICI";
	   LSloadVarType(pModel,varType);
   }
#else
   nErrorCode = LSreadLINDOFile(pModel,"posd.ltx");
   APIERRORCHECK;
   if (1)
   {
     char varType[] = "CCCIII";
	   LSloadVarType(pModel,varType);
   }
#endif
   nErrorCode = LSgetInfo(pModel,LS_IINFO_NUM_VARS,&n);
   nErrorCode += LSgetInfo(pModel,LS_IINFO_NUM_CONS,&m);
   nErrorCode += LSgetInfo(pModel,LS_IINFO_NUM_CONT,&nC);
   APIERRORCHECK;
   /***************************************************************
    * Step 4: Load PSD constraint

        | X11             |
    X = | X21 X22         |  is PSD
        |         X33     |
        |         X43 X44 |

    ***************************************************************/
   {
    int nPOSDBlocks  = 2;
    int paiPOSDdim[] = {  2, 2 };
    int paiPOSDbeg[] = {  0, 3, 6};
    int paiPOSDrowndx[] = {  0, 1, 1, 0, 1, 1 };
    int paiPOSDcolndx[] = {  0, 0, 1, 0, 0, 1 };
    int paiPOSDvarndx[] = {  0, 1, 2, 3, 4, 5 };
    nErrorCode = LSloadPOSDData(pModel,
                               nPOSDBlocks,
                               paiPOSDdim,
                               paiPOSDbeg,
                               paiPOSDrowndx,
                               paiPOSDcolndx,
                               paiPOSDvarndx);
    APIERRORCHECK;
   }



   /***************************************************************
    * Step 5: Optimize the model
    ***************************************************************/
   nErrorCode = LSsetModelLogfunc(pModel, (printModelLOG_t) print_line_log, NULL);
   if (n - nC > 0)
   {
     nErrorCode = LSsolveMIP( pModel, &status);
   }
   else
   {
     nErrorCode = LSoptimize( pModel, LS_METHOD_FREE, &status);
   }
   APIERRORCHECK;
   LSwriteSolution(pModel,"posd.sol");


   /***************************************************************
    * Step 6: Access the final solution if optimal or feasible
    ***************************************************************/
   if (status == LS_STATUS_OPTIMAL ||
       status == LS_STATUS_BASIC_OPTIMAL ||
       status == LS_STATUS_LOCAL_OPTIMAL ||
       status == LS_STATUS_FEASIBLE)
   {
     double *primal = NULL, *dual = NULL;
     int    j;

     primal = (double *) malloc(n*sizeof(double));
     dual   = (double *) malloc(m*sizeof(double));

     if (n - nC > 0) {
       nErrorCode = LSgetInfo(pModel,LS_DINFO_MIP_OBJ,&dObj);
       APIERRORCHECK;
       nErrorCode = LSgetMIPDualSolution( pModel,dual);
       APIERRORCHECK;
       nErrorCode = LSgetMIPPrimalSolution( pModel,primal);
       APIERRORCHECK;
     } else {
       nErrorCode = LSgetPrimalSolution( pModel, primal) ;
       APIERRORCHECK;
       nErrorCode = LSgetDualSolution( pModel, dual) ;
       APIERRORCHECK;
       nErrorCode = LSgetInfo(pModel,LS_DINFO_POBJ,&dObj);
       APIERRORCHECK;
     }

     printf ("\n Objective at solution = %f \n", dObj);


     // un/comment the block below if you would like the primal and dual solutions
     // to be printed on the screen.
     if (1){
       char szname[255];
       printf ("\n Primal Solution\n");
       printf("\t%8s %18s\n","VARS", "Primal");
       for (j = 0; j<n; j++)
       {
         nErrorCode = LSgetVariableNamej(pModel,j,szname);
         printf("\t%8s %18.10e\n",szname, primal[j]);
       }

       printf ("\n Dual Solution\n");
       printf("\t%8s %18s\n","CONS", "Dual");
       for (j = 0; j<m; j++)
       {
         nErrorCode = LSgetConstraintNamei(pModel,j,szname);
         printf("\t%8s %18.10e\n",szname, dual[j]);
       }
     }
     free(primal);
     free(dual);
   }
   else
   {
     char strbuf[255];
     LSgetErrorMessage(pEnv,nErrorCode,strbuf);
     printf ("\n Optimization failed. Status = %d ",status);
     //printf ("\n Error %d: %s\n",nErrorCode,strbuf);
   }
Terminate:
   /***************************************************************
    * Step 7: Terminate
    ***************************************************************/
   nErrorCode = LSdeleteModel( &pModel);
   nErrorCode = LSdeleteEnv( &pEnv);


  /* Wait until user presses the Enter key */
   printf("Press <Enter> ...");
   getchar();

}
