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

  File   : ex_la.c

  Purpose: Samples for Linear Algebra routines

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
    printf("\nLINDO API Version %s built on %s\n",szVersion,szBuild);\
}\

/**
 * @brief Print row vector
 * @param[in,out] NR dimension of vector
 * @param[in,out] v a double vector
 * @return void
 * @remark None
 */
void printRowVector(int NR, double *v) {
  int j;
  fprintf(stdout,"%6s ","");
  for (j=0; j<NR; j++) {
    fprintf(stdout,"%9g ",v[j]);
  }
  fprintf(stdout,"\n");
}

/**
 * @brief Print square matrix
 * @param[in,out] NR size of square matrix
 * @param[in,out] a a double matrix of NR*NR elements
 * @return void
 * @remark None
 */
void printSqMatrix(int NR, double *a)
{
  int i,j;
  fprintf(stdout,"%6s ","");
  for (j=0; j<NR; j++) {
    fprintf(stdout,"%9d ",j);
  }
  fprintf(stdout,"\n\n");
  for (i=0; i<NR; i++) {
    fprintf(stdout,"%6d ",i);
    for (j=0; j<NR; j++) {
      fprintf(stdout,"%9g ",a[i*NR+j]);
    }
    fprintf(stdout,"\n");
  }
  fprintf(stdout,"\n");
}

/**
 *
 *      Main
 *
 */
int main(int argc, char **argv)
{
   APIERRORSETUP;

   char MY_LICENSE_KEY[1024];

    /* Declare an instance of the LINDO environment object */
   pLSenv pEnv = NULL;
   int info=0, i,j;
   int NR = 4; // number of rows
   double A[] = {
     1.0,  2.0,  3.0,  4.0,
     0.0,  2.0,  3.0,  4.0,
     0.0,  0.0,  3.0,  4.0,
     0.0,  0.0,  0.0,  4.0
   };
   double magic4[] = { 16,     2,     3,    13,
                         5,    11,    10,     8,
                         9,     7,     6,    12,
                         4,    14,    15,     1 };
   double At[16];
   // eiggs
   double *W, *V;
   double *WR,*WI,*VRR,*VRI,*VRL,*VLI;
   // lu
   double *L,*U;
   // qr
   double *Q,*R;
   // svd
   double *S;

   /*
    *     Create a LINDO environment.
    */
   if (argc == 0) {
     printf("\nUsage: ex_la\n\n");
     exit(1);
   }

   errorcode = LSloadLicenseString("../../../license/lndapi130.lic",MY_LICENSE_KEY);
   APIERRORCHECK;

   pEnv = LScreateEnv ( &errorcode, MY_LICENSE_KEY);
   if ( errorcode == LSERR_NO_VALID_LICENSE) {
      printf( "Invalid License Key!\n");
      exit( 1);
   }
   APIERRORCHECK;

   //////////////////////////////////////////////////////////////////////////
   // Test LSgetEigs
   W = malloc(NR*sizeof(double));
   V = malloc(NR*NR*sizeof(double));

   errorcode = LSgetEigs(4,'U',A,W,V,&info);
   if (errorcode || info) {
    fprintf(stdout,"\nLSgetEigs info:%d\n",info);
    APIERRORCHECK(errorcode);
   }

   fprintf(stdout,"\n");
   fprintf(stdout,"All eigenvalues:\n");
   printRowVector(NR,W);

   fprintf(stdout,"\n");
   fprintf(stdout,"All eigenvectors:\n");
   printSqMatrix(NR,V);


   //////////////////////////////////////////////////////////////////////////
   // Test LSgetEigg
   WR = malloc(2*NR*sizeof(double));
   WI = WR + NR;
   VRR = malloc(4*NR*NR*sizeof(double));
   VRI = VRR + NR*NR;
   VRL = VRI + NR*NR;
   VLI = VRL + NR*NR;
   errorcode = LSgetEigg(NR,'B',A,WR,WI,VRR,VRI,VRL,VLI,&info);
   if (errorcode || info) {
     fprintf(stdout,"\nLSgetEigg info:%d\n",info);
     APIERRORCHECK(errorcode);
   }

   fprintf(stdout,"\n");
   fprintf(stdout,"Input matrix:\n");
   printSqMatrix(NR,A);

   fprintf(stdout,"\n");
   fprintf(stdout,"Real eigenvalues:\n");
   printRowVector(NR,WR);

   fprintf(stdout,"\n");
   fprintf(stdout,"Right eigenvectors (Real):\n");
   printSqMatrix(NR,VRR);
   fprintf(stdout,"\n");
   fprintf(stdout,"Right eigenvectors (Img.):\n");
   printSqMatrix(NR,VRI);

   fprintf(stdout,"\n");
   fprintf(stdout,"Imaginary eigenvalues:\n");
   printRowVector(NR,WI);

   fprintf(stdout,"\n");
   fprintf(stdout,"Left eigenvectors (Real):\n");
   printSqMatrix(NR,VRL);
   fprintf(stdout,"\n");
   fprintf(stdout,"Left eigenvectors (Img.):\n");
   printSqMatrix(NR,VLI);

   fprintf(stdout,"\ninfo: %d\n",info);


   //////////////////////////////////////////////////////////////////////////
   // Transpose
   memcpy(A,magic4,sizeof(double)*NR*NR);
   LSgetMatrixTranspose(NR,NR,A,At);

   fprintf(stdout,"\n");
   fprintf(stdout,"A = Magic square(4):\n");
   printSqMatrix(NR,A);

   fprintf(stdout,"\n");
   fprintf(stdout,"A':\n");
   printSqMatrix(NR,At);

   //////////////////////////////////////////////////////////////////////////
   // Cholesky decomposition
   fprintf(stdout,"\n");
   fprintf(stdout,"Cholesky decomposition\n");
   fprintf(stdout,"[L,L'] = chol((A+A')/2)\n");
   L = VRR;
   memset(L,0,2*NR*NR*sizeof(double));
   for (i=0; i<NR*NR; i++) { At[i]+=A[i]; At[i]/=2; }
   LSgetMatrixCholFactor(NR,'L',At,L,&info);
   for (i=0; i<NR; i++) {
     for (j=i+1; j<NR; j++) {
       L[i*NR+j] = 0;
     }
   }
   if (info>0) {
     for (i=info-1; i<NR; i++) {
       for (j=0; j<NR; j++) {
         L[i*NR+j] = 0;
       }
     }
   }

   fprintf(stdout,"\n");
   fprintf(stdout,"L:\n");
   printSqMatrix(NR,L);

   fprintf(stdout,"\ninfo: %d\n",info);


   //////////////////////////////////////////////////////////////////////////
   // LU decomposition
   fprintf(stdout,"\n");
   fprintf(stdout,"LU decomposition\n");
   fprintf(stdout,"[L,U] = lu(A)\n");
   L = VRR;
   U = L + NR*NR;
   memset(L,0,2*NR*NR*sizeof(double));
   LSgetMatrixLUFactor(NR,NR,A,NULL,L,U,&info);

   fprintf(stdout,"\n");
   fprintf(stdout,"L:\n");
   printSqMatrix(NR,L);

   fprintf(stdout,"\n");
   fprintf(stdout,"U:\n");
   printSqMatrix(NR,U);

   fprintf(stdout,"\ninfo: %d\n",info);

   //////////////////////////////////////////////////////////////////////////
   // QR decomposition
   fprintf(stdout,"\n");
   fprintf(stdout,"QR decomposition\n");
   fprintf(stdout,"[Q,R] = qr(A)\n");
   Q = VRR;
   R = Q + NR*NR;
   memset(Q,0,2*NR*NR*sizeof(double));
   LSgetMatrixQRFactor(NR,NR,A,Q,R,&info);

   fprintf(stdout,"\n");
   fprintf(stdout,"Q:\n");
   printSqMatrix(NR,Q);

   fprintf(stdout,"\n");
   fprintf(stdout,"R:\n");
   printSqMatrix(NR,R);

   fprintf(stdout,"\ninfo: %d\n",info);

   //////////////////////////////////////////////////////////////////////////
   // SVD decomposition
   fprintf(stdout,"\n");
   fprintf(stdout,"SVD decomposition\n");
   fprintf(stdout,"[U,S,V] = qr(A)\n");
   S = VRR;
   U = S + NR;
   V = V;
   memset(Q,0,2*NR*NR*sizeof(double));
   LSgetMatrixSVDFactor(NR,NR,A,U,S,V,&info);

   fprintf(stdout,"\n");
   fprintf(stdout,"U:\n");
   printSqMatrix(NR,U);

   fprintf(stdout,"\n");
   fprintf(stdout,"S: (singular values)\n");
   printRowVector(NR,S);

   fprintf(stdout,"\n");
   fprintf(stdout,"V:\n");
   printSqMatrix(NR,V);

   fprintf(stdout,"\ninfo: %d\n",info);

Terminate:
   free(W);
   free(V);
   free(WR);
   free(VRR);

  return 0;
}
