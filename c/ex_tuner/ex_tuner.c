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

  File   : ex_tuner.c

  Purpose: Tuning parameters

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
#include "../common/commonutils.c"
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

#define APIVERSION \
{\
    char szVersion[255], szBuild[255];\
    LSgetVersionInfo(szVersion,szBuild);\
    printf("\nLINDO API Version %s built on %s\n",szVersion,szBuild); fflush(stdout);\
}\

#define INIT_DATA_PATH(dataPath) {\
  if (getenv("LINDOAPI_HOME")) {\
    sprintf(dataPath,"%s/samples/data",getenv("LINDOAPI_HOME"));\
  } else {\
    sprintf(dataPath,"../../samples/data");\
  }\
}

int main(int argc, char **argv)
{
   APIERRORSETUP;
   char MY_LICENSE_KEY[1024];
   int mCriterion,jInstance;
   char dataPath[255];

    /* Declare an instance of the LINDO environment object */
   pLSenv pEnv = NULL;

   APIVERSION;
   /*
    *     Create a LINDO environment.
    */
   errorcode = LSloadDefaultLicenseString(MY_LICENSE_KEY);
   APIERRORCHECK;

   pEnv = LScreateEnv ( &errorcode, MY_LICENSE_KEY);
   if ( errorcode == LSERR_NO_VALID_LICENSE) {
      printf( "Invalid License Key!\n");
      exit( 1);
   }
   APIERRORCHECK;

   /* Tuner instances */
   INIT_DATA_PATH(dataPath);
   errorcode = LSaddTunerInstance(pEnv, strcat(dataPath,"/bm23.mps.gz")); APIERRORCHECK;

   INIT_DATA_PATH(dataPath);
   errorcode = LSaddTunerInstance(pEnv, strcat(dataPath,"/p0033.mps.gz")); APIERRORCHECK;

   INIT_DATA_PATH(dataPath);
   errorcode = LSaddTunerInstance(pEnv, strcat(dataPath,"/p0201.mps.gz")); APIERRORCHECK;

   INIT_DATA_PATH(dataPath);
   errorcode = LSaddTunerInstance(pEnv, strcat(dataPath,"/p0282.mps.gz")); APIERRORCHECK;

   /* Tuner options */
   errorcode = LSaddTunerOption(pEnv,"max_parsets",6); APIERRORCHECK;
   errorcode = LSaddTunerOption(pEnv,"use_gop",0); APIERRORCHECK;
   errorcode = LSaddTunerOption(pEnv,"time_limit",10); APIERRORCHECK;
   errorcode = LSaddTunerOption(pEnv,"ntrials",2); APIERRORCHECK;
   errorcode = LSaddTunerOption(pEnv,"nthreads",1); APIERRORCHECK;
   errorcode = LSaddTunerOption(pEnv,"seed",1032); APIERRORCHECK;
   errorcode = LSaddTunerOption(pEnv,"criterion",1); APIERRORCHECK;

   /* Tuner dynamic parameters */
   errorcode = LSaddTunerZDynamic(pEnv,LS_IPARAM_LP_SCALE); APIERRORCHECK;
   errorcode = LSaddTunerZDynamic(pEnv,LS_IPARAM_MIP_PRELEVEL); APIERRORCHECK;
   errorcode = LSaddTunerZDynamic(pEnv,LS_IPARAM_MIP_BRANCHDIR); APIERRORCHECK;
   errorcode = LSaddTunerZDynamic(pEnv,LS_IPARAM_MIP_BRANCHRULE); APIERRORCHECK;
   errorcode = LSaddTunerZDynamic(pEnv,LS_IPARAM_MIP_FP_MODE); APIERRORCHECK;
   errorcode = LSaddTunerZDynamic(pEnv,LS_DPARAM_SOLVER_FEASTOL); APIERRORCHECK;

   /* Tuner static groups and parameters */
   errorcode = LSaddTunerZStatic(pEnv,1,LS_IPARAM_MIP_NODESELRULE,4); APIERRORCHECK;
   errorcode = LSaddTunerZStatic(pEnv,1,LS_DPARAM_MIP_RELINTTOL,0.0001); APIERRORCHECK;
   errorcode = LSaddTunerZStatic(pEnv,1,LS_DPARAM_SOLVER_OPTTOL,1e-006); APIERRORCHECK;
   errorcode = LSaddTunerZStatic(pEnv,2,LS_IPARAM_MIP_NODESELRULE,1); APIERRORCHECK;
   errorcode = LSaddTunerZStatic(pEnv,2,LS_DPARAM_MIP_RELINTTOL,0.001); APIERRORCHECK;
   errorcode = LSaddTunerZStatic(pEnv,2,LS_DPARAM_SOLVER_OPTTOL,1e-005); APIERRORCHECK;
   errorcode = LSaddTunerZStatic(pEnv,3,LS_IPARAM_MIP_NODESELRULE,3); APIERRORCHECK;
   errorcode = LSaddTunerZStatic(pEnv,3,LS_DPARAM_MIP_RELINTTOL,1e-005); APIERRORCHECK;
   errorcode = LSaddTunerZStatic(pEnv,3,LS_DPARAM_SOLVER_OPTTOL,0.0001); APIERRORCHECK;


   if (0>1) {
    errorcode = LSprintTuner(pEnv);
    APIERRORCHECK;
   }
   errorcode = LSrunTuner(pEnv);
   APIERRORCHECK;

   errorcode = LSdisplayTunerResults(pEnv);
   APIERRORCHECK;

   mCriterion = -1; //selected criterion
   jInstance  = -1; //avg instance
   errorcode = LSwriteTunerParameters(pEnv,"lindo_tuned.par",jInstance,mCriterion);
   APIERRORCHECK;

   errorcode = LSclearTuner(pEnv);
   APIERRORCHECK;


Terminate:
   LSdeleteEnv(&pEnv);

  return 0;
}
