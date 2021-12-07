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

File   : ex_dw.c

Purpose: This application uses a set of LINDO API routines to compute
a set-partitioning relaxation to the bin packing problem based on
Dantzig-Wolfe (DW) decomposition with column generation.

Suppose we have N objects with weights W[j], j=1,…,N and the objective
is to find the minimum number of bins, each with capacity C, required
to pack all N objects.

DW-decomposition can compute a very tight (lower) bound on the minimum
number of bins required.The problem data is represented by the
following */

/* number of objects to pack */
#define nObjects           25

/* capacity of each bin      */
#define dCapacity          150.0

/* item weights  */
double  dWeights[nObjects] = { 90,    43,    27,    67,    49,
                               48,    23,    72,    60,    41,
                               50,    52,    40,    97,    27,
                               41,    55,    33,    38,    65,
                               67,    32,    89,    80,    40 };

/* optimality tolerance      */
#define dOptTolerance      1.0e-7

/* local debug flag */
#define _DEBUG_          0

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


/* main entry point */

/****************************************************************
   Standard callback function to display local and intermediate
   solutions if needed.
 ****************************************************************/
int  LS_CALLTYPE print_log(pLSmodel model,int iLoc, void *cbData)
{
  static int iter=0;
  static double pfeas=0.0,pobj=0.0;
  static double bestbnd;
  static int status;

  if (iLoc == LSLOC_MIP)
  {
    LSgetCallbackInfo(model,iLoc,LS_IINFO_MIP_STATUS,&status);
    LSgetCallbackInfo(model,iLoc,LS_IINFO_MIP_SIM_ITER,&iter);
    LSgetCallbackInfo(model,iLoc,LS_DINFO_MIP_OBJ,&pobj);
    LSgetCallbackInfo(model,iLoc,LS_DINFO_MIP_BESTBOUND,&bestbnd);
    LSgetCallbackInfo(model,iLoc,LS_DINFO_PINFEAS,&pfeas);
    printf("\n%2d It=%4d Infeas= %8.3e Obj=%11.5e BestBnd=%11.5e Stat=%d",
      iLoc,iter,pfeas,pobj,bestbnd,status);
  }
  else if ( 0 &(iLoc == LSLOC_PRIMAL || iLoc ==LSLOC_DUAL || iLoc ==LSLOC_CONOPT))
  {
    LSgetCallbackInfo(model,iLoc,LS_IINFO_MIP_STATUS,&status);
    LSgetCallbackInfo(model,iLoc,LS_DINFO_PINFEAS,&pfeas);
    printf("\n%2d It=%4d Infeas= %8.3e Obj=%11.5e BestBnd=%11.5e Stat=%d",
      iLoc,iter,pfeas,pobj,bestbnd,status);
  }
  return 0;
} /*print_log*/


/****************************************************************

   Main starts here.

 ****************************************************************/
int main(int argc, char **argv)
{
   APIERRORSETUP;

   int i,j,k,n;
   double dKnapObj, dMasterObj;
   int nStatus;
   char MY_LICENSE_KEY[1024];

   /* declare an instance of the LINDO environment object */
   pLSenv pEnv=NULL;

   /* declare instance of the LINDO model object for master and
   knapsack problems */
   pLSmodel pMaster, pKnapsack;

   /* objective function sense */
   int    nObjDir = LS_MIN;

   /* E matrix for master */
   int    iE[nObjects];
   int    kE[nObjects+1], *itmp;
   double  E[nObjects];

   /* right hand side vector for master */
   double  e[nObjects];

   /* objective coeff for master*/
   double  c[nObjects];

   /* constraint senses for master */
   char csense[nObjects];

   /* dual solution for master */
   double dual[nObjects];

   /* primal solution for knapsack */
   double *primal=NULL;

   /* variable types for knapsack */
   char *vtype=NULL;



   /*
   Step 1: Create a LINDO environment.
   */
   nErrorCode = LSloadLicenseString("../../../license/lndapi130.lic",MY_LICENSE_KEY);
   if ( nErrorCode != LSERR_NO_ERROR)
   {
      printf( "Failed to load license key (error %d)\n",nErrorCode);
      exit( 1);
   }

   APIVERSION;
   pEnv = LScreateEnv ( &nErrorCode, MY_LICENSE_KEY);
   if ( nErrorCode == LSERR_NO_VALID_LICENSE) {
      printf( "Invalid License Key!\n");
      exit( 1);
   }
   APIERRORCHECK;


   /*
   Step 2.1: Create the master problem in the environment.
   */

   pMaster = LScreateModel ( pEnv, &nErrorCode);
   APIERRORCHECK;

   /* Assemble a set of feasible packings and construct the
   initial master problem. Each column of E corresponds to a
   bin and the nonzero entries in each marks the items in it.

     min  cx
          Ex = e
          x >= 0
   */

   /* Initially we choose to set E to the identity matrix, but
   several heuristics can be used alternatively.*/
   for (i=0; i<nObjects; i++)
   {
     iE[i] = i;
     kE[i] = i;
     E[i] = 1.0;
     e[i] = 1.0;
     c[i] = 1.0;
     csense[i] = 'E';
   }
   kE[i] = nObjects;

   /* Load the master problem using LSloadLPData */
   nErrorCode = LSloadLPData( pMaster, nObjects, nObjects, nObjDir, 0,
     c, e, csense, nObjects, kE, NULL, E, iE, NULL, NULL);
   APIERRORCHECK;


   /*
   Step 2.2: Create the knapsack problem for column generation.
   */
   pKnapsack = LScreateModel ( pEnv, &nErrorCode);
   APIERRORCHECK;

   /* reuse iE,kE vectors to load the knapsack problem */
   vtype = (char *) malloc(nObjects*sizeof(char));
   if (vtype == NULL) return LSERR_OUT_OF_MEMORY;

   for (i=0; i<nObjects; i++)
   {
     iE[i] = 0;
     kE[i] = i;
     vtype[i] ='B';
   }
   kE[nObjects] = nObjects;
   csense[0] = 'L';
   e[0] = dCapacity;
   nObjDir = LS_MAX;


   /* Load the knapsack problem using LSloadLPData */
   nErrorCode = LSloadLPData( pKnapsack, 1, nObjects, nObjDir, 0,
     c, e, csense, nObjects, kE, NULL, dWeights, iE, NULL, NULL);

   /* Set the variable types to binary */
   nErrorCode = LSloadVarType( pKnapsack, vtype);

   /* allocate memory for the solution of the knapsack problem */
   primal = (double *) malloc(nObjects*sizeof(double));
   if (primal == NULL) return LSERR_OUT_OF_MEMORY;


   /*
   Step 3. Start column generation to solve the LP relaxation of DW
   for binpacking problem.
   */

   {
     printf("\n\nBIN PACKING WITH DANTZIG-WOLFE DECOMPOSITION.\n\n");
     printf("Capacity of each bin      = %f\n", dCapacity);
     printf("Number of objects to pack = %d\n", nObjects);
     printf("\n\n");
     printf("%15s %15s %15s\n","Num cols ","Obj of DW ","Reduced cost ");
     printf("%15s %15s %15s\n","generated","relaxation","of new column");
     printf("%15s %15s %15s\n","---------","----------","-------------");
   }

   while (1)
   {
     /* Optimize the master problem */
     nErrorCode = LSoptimize( pMaster, LS_METHOD_FREE, &nStatus);
     APIERRORCHECK;

#if _DEBUG_
     LSwriteLINDOFile(pMaster, "master.ltx");
#endif

     nErrorCode = LSgetDualSolution( pMaster, dual) ;
     APIERRORCHECK;

     nErrorCode = LSgetInfo(pMaster,LS_DINFO_POBJ,&dMasterObj);

     /* the dual solution to the master becomes the objective coeff. of
     the knapsack (column generation) problem. */
     kE[1] = 1;
     nErrorCode = LSmodifyObjective(pKnapsack, nObjects, kE, dual);
     APIERRORCHECK;

     /* solve the knapsack (column generation) problem */
     nErrorCode = LSsolveMIP(pKnapsack, &nStatus);
     APIERRORCHECK;

#if _DEBUG_
     LSwriteLINDOFile(pKnapsack, "knapsack.ltx");
#endif

     /* get objective function value of the knapsack problem*/
     nErrorCode = LSgetInfo(pKnapsack,LS_DINFO_MIP_OBJ,&dKnapObj);
     APIERRORCHECK;

     /* break if LP relaxation is optimal (i.e. reduced cost of
     the generated column is not attractive to enter the LP basis*/
     if (dKnapObj <= 1 + dOptTolerance)
     {
       break;
     }

     nErrorCode = LSgetInfo(pMaster,LS_IINFO_NUM_VARS,&n);
     if (((n-nObjects)%5)==0)
     {
       printf("%15d %15.3f %15.3f\n",n,dMasterObj,dKnapObj-1);
     }

     /* get new column */
     nErrorCode = LSgetMIPPrimalSolution( pKnapsack, primal) ;
     APIERRORCHECK;

     /* construct the sparse vector to append to master */
     k = 0;
     for (i=0; i<nObjects; i++)
     {
       if (primal[i])
       {
         E[k] = primal[i];
         iE[k++] = i;
       }
     }
     kE[0] = 0;
     kE[1] = k;
     c[0] = 1.0;

     /* append to master problem as new column */
     nErrorCode = LSaddVariables(pMaster, 1, NULL, NULL,
       kE, NULL, E, iE, c, NULL, NULL);
     APIERRORCHECK;

   }


   nErrorCode = LSgetInfo(pMaster,LS_IINFO_NUM_VARS,&n);
   printf("%15d %15.3f %15.3f\n",n,dMasterObj,dKnapObj-1);
   printf("\nMinimum bins required >= %g\n",dMasterObj);

   if (0)
   {
     goto Terminate;
   }

   /*
   Step 4. Solve the final master problem as an integer model
   to obtain a near optimal solution to the bin packing model.
   */
   if (vtype)
     vtype = realloc(vtype, n*sizeof(char));
   else
     vtype = (char *) malloc(n*sizeof(char));

   if (primal)
     primal = realloc(primal,2*n*sizeof(double));
   else
     primal = (double *) malloc(2*n*sizeof(double));
   itmp = ((int*) &primal[n]);

   for (i=0; i<n; i++)
   {
     vtype[i] = 'B';
     itmp[i] = i;
   }

   nErrorCode = LSmodifyVariableType(pMaster, n, itmp, vtype);
   APIERRORCHECK;

   nErrorCode = LSsetCallback(pMaster,(cbFunc_t) print_log, NULL);
   APIERRORCHECK;

   // free resident solution memory
   LSfreeSolutionMemory(pMaster);

   printf("\n\nSolving the Dantzig-Wolfe relaxation as MIP...\n\n");
   nErrorCode = LSsolveMIP(pMaster, &nStatus);
   APIERRORCHECK;

   /*
   Step 5. Report bin contents
   */
   nErrorCode = LSgetInfo(pMaster,LS_IINFO_NUM_VARS,&n);
   if (nStatus == LS_STATUS_OPTIMAL)
   {
     int binsize;

     /* get feasible bin allocations */
     nErrorCode = LSgetMIPPrimalSolution( pMaster, primal) ;
     APIERRORCHECK;

     k = 0; /* optimal number of bins */
     for (j=0; j<n; j++)
     {
       if (primal[j] == 1)
       {
         printf("\nBin %d contains ", ++k);

         /* indices of nonzeros in E(:,j) correspond to indices of
         items in this bin */
         nErrorCode = LSgetLPVariableDataj(pMaster,j,NULL,NULL,
           NULL,NULL,&binsize,iE,NULL);

         /* print bin contents */
         for (i=0; i<binsize; i++) printf(", %d ",iE[i]);
       }
     }
   }
   else
   {
     printf("Error: optimal solution cannot be found.\n");
   }

Terminate:
   /*
   Step 6: Terminate
   */
   nErrorCode = LSdeleteModel( &pKnapsack);
   nErrorCode = LSdeleteModel( &pMaster);
   nErrorCode = LSdeleteEnv( &pEnv);

  /* Wait until user presses the Enter key */

   free(vtype);
   free(primal);

   printf("\n\nPress <Enter> ...");
   getchar();

   return nErrorCode;

}
