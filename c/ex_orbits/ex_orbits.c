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

  File   : ex_orbits.c

  Purpose: Read a model from an MPS file and find the symmetry information.
*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
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

   char MY_LICENSE_KEY[1024];

   void *pSymInfo = NULL;

   int  n_vars, n_orbits, n_generators;

  /****************************************************************
   * Step 1: Create a LINDO environment.
   ****************************************************************/
   nErrorCode = LSloadDefaultLicenseString(MY_LICENSE_KEY);
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
   * Step 3: Read the model from an MPS file
   ****************************************************************/
   nErrorCode = LSreadMPSFile(pModel,"ex_orbits.mps",LS_UNFORMATTED_MPS);
   APIERRORCHECK;

   /****************************************************************
   * Step 4: Find symmetry information
   ****************************************************************/
   LSsetModelIntParameter(pModel, LS_IPARAM_FIND_SYMMETRY_LEVEL, 2);

   pSymInfo = LSfindSymmetry(pModel,&nErrorCode);
   APIERRORCHECK;

   /****************************************************************
   * Step 5: Get orbit information
   ****************************************************************/
   nErrorCode = LSgetInfo(pModel, LS_IINFO_NUM_VARS, &n_vars);
   nErrorCode = LSgetOrbitInfo(pSymInfo,&n_generators, &n_orbits, NULL, NULL);
   APIERRORCHECK;

   printf("Number of variables  = %d\n", n_vars);
   printf("Number of generators = %d\n", n_generators);
   printf("Number of orbits     = %d\n", n_orbits);

ErrReturn:

   /***************************************************************
    * Step 6: Terminate
    ***************************************************************/
   nErrorCode = LSdeleteSymmetry(&pSymInfo);
   nErrorCode = LSdeleteModel( &pModel);
   nErrorCode = LSdeleteEnv( &pEnv);

  /* Wait until user presses the Enter key */
   printf("Press <Enter> ...");
   getchar();

}
