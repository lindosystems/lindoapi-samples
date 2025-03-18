/*/////////////////////////////////////////////////////////////////////*
 * Display scenario outcomes
 */
int DisplayScenarioOutcomes(pLSmodel pModel)
{
  int nErrorCode=LSERR_NO_ERROR, numStocPars, i,j, numScens;
  char strbuf[255];
  double *padOutcome=NULL, dProb;

  nErrorCode = LSgetInfo(pModel,LS_IINFO_NUM_SPARS,&numStocPars);
  if (nErrorCode != LSERR_NO_ERROR) return nErrorCode;
  nErrorCode = LSgetStocInfo(pModel,LS_IINFO_STOC_NUM_SCENARIOS, 0, &numScens);
  if (nErrorCode != LSERR_NO_ERROR) return nErrorCode;

  printf("\n\nOutcomes of stochastic params by scenarios.\n");

  if (numStocPars==0)
  {
    printf("No stochastic parameters found. Quitting\n");
    return LSERR_NO_ERROR;
  }

  printf("%10s","Scenario");
  for (i=0; i<numStocPars; i++)
  {
   sprintf(strbuf,"spar[%d]",i); printf("%10s",strbuf);
  }
  printf(" %13s\n","Prob()");


  padOutcome = malloc(numStocPars*sizeof(double));
  if (padOutcome ==NULL)
  {
    return LSERR_OUT_OF_MEMORY;
  }


  for (j=0; j<numScens; j++)
  {
    nErrorCode = LSgetStocParOutcomes(pModel,j,padOutcome,&dProb);
    if (nErrorCode != LSERR_NO_ERROR) break;

    printf("%10d",j);
    for (i=0; i<numStocPars; i++)
    {
     printf("%10g",padOutcome[i]);
    }
    printf(" %13g\n",dProb);
  }

  free(padOutcome);
  return nErrorCode;

}

/*/////////////////////////////////////////////////////////////////////*
 * Display sample correlation
 */
int DisplaySampleCorr(pLSmodel pModel, int numStocPars, int CorrelType)
{
   int nErrorCode=LSERR_NO_ERROR, i,j,k;
   int ObservedQCnonzeros;
   int *ObservedQCvarndx1=NULL;
   int *ObservedQCvarndx2=NULL;
   double *ObservedQCcoef=NULL;
   double *pX=NULL, *pCIX=NULL;
   pLSsample *paSample = NULL;

   printf("\n\nObserved correlation coefficients among stochastic variables\n");

   paSample = malloc(numStocPars*sizeof(pLSsample));
   for (i=0; i<numStocPars; i++)
   {
     paSample[i] = LSgetStocParSample(pModel,i,-1,-1,&nErrorCode);
   }

   if (1)
   {
     printf("\nOriginal Sample\n");
     for (i=0; i<numStocPars; i++)
     {
       // sample and size
       nErrorCode = LSsampGetPointsPtr(paSample[i],&j,&pX);
       if (nErrorCode==LSERR_NO_ERROR)
       {
         printf("\nDIM %2d: ",i);
         for (k=0; k<j; k++) printf("%8.2f",pX[k]);
       }
       else
       {
         break;
       }
     }

     printf("\n\nCorrelation Induced (CI) sample\n");
     for (i=0; i<numStocPars; i++)
     {
       nErrorCode = LSsampGetCIPointsPtr(paSample[i],&j,&pCIX);
       if (nErrorCode==LSERR_NO_ERROR)
       {
         printf("\nDIM %2d: ",i);
         for (k=0; k<j; k++) printf("%8.2f",pCIX[k]);
       }
       else
       {
         break;
       }
     }
     fflush(stdout);
   }

   if (1)
   {
     printf("\nCorrelations of Original sample\n");
     ObservedQCvarndx1=malloc(sizeof(int)*numStocPars*numStocPars);
     ObservedQCvarndx2=malloc(sizeof(int)*numStocPars*numStocPars);
     ObservedQCcoef=malloc(sizeof(double)*numStocPars*numStocPars);

     nErrorCode = LSgetCorrelationMatrix(pModel,0,CorrelType,
                                         &ObservedQCnonzeros,
                                         ObservedQCvarndx1,
                                         ObservedQCvarndx2,
                                         ObservedQCcoef);
     if (nErrorCode!=LSERR_NO_ERROR) goto ErrReturn;

     for (i=0; i<ObservedQCnonzeros; i++)
     {
       printf("C[%d][%d] = %13.5f\n",
         ObservedQCvarndx1[i],ObservedQCvarndx2[i],ObservedQCcoef[i]);
     }
     fflush(stdout);

     printf("\nCorrelations of CI sample\n");
     nErrorCode = LSgetCorrelationMatrix(pModel,1,CorrelType,
                                         &ObservedQCnonzeros,
                                         ObservedQCvarndx1,
                                         ObservedQCvarndx2,
                                         ObservedQCcoef);

     for (i=0; i<ObservedQCnonzeros; i++)
     {
       printf("C[%d][%d] = %13.5f\n",
         ObservedQCvarndx1[i],ObservedQCvarndx2[i],ObservedQCcoef[i]);
     }
     fflush(stdout);

   }

ErrReturn:

   free(paSample);
   free(ObservedQCvarndx1);
   free(ObservedQCvarndx2);
   free(ObservedQCcoef);

   return nErrorCode;
}

/*
 * @brief Swaps the contents of two 32 bit integers
 *
 *
 * @param pi  A pointer to the first integer
 * @param pj  A pointer to the second integer
 *
 * @return None
 *
 * @remark On return, the values of at addresses pi and pj will be
 *         interchanged.
 *
 */
static void ls_swap_i32 ( int *pi, int *pj )
{
  int k;

  k = *pi;
  *pi = *pj;
  *pj = k;

  return;
}


/*
 * @brief Generates a random permutation of length n.
 *
 *
 * @param pRG A reference to the random number generator.
 * @param n   An integer specifiying number of objects
 * @param p   An integer output vector specifiying permutation.
 *
 * @return None
 *
 *
 */
void ls_randperm (  pLSrandGen pRG, int n, int p[] )
{
  int i;
  int j;

  for ( i = 0; i < n; i++ )
  {
    p[i] = i ;
  }

  for ( i = 1; i <= n; i++ )
  {
    j = LSgetInt32RV(pRG,i, n);
    ls_swap_i32 ( &p[i-1], &p[j-1] );
  }

  return;
}

void invperm(int *perm, int *iperm, int len)
{
  int i;
  for (i=0; i<len; i++)
  {
    iperm[perm[i]]=i;
  }
}

/******************************************************************************
 *
 *
 *
 */
static void LS_CALLTYPE print_file_log(char     *fname,
                                    char     *line)
{
  FILE *out;

  if (line)
  {
    out=fopen(fname,"a");

    if (out)
    {
      fprintf(out,"%s",line);
      fclose(out);
    }
  } /*if*/

} /*print_file_log*/


int LSloadDefaultLicenseString(char* MY_LICENSE_KEY) {
  int nErrorCode=LSERR_NO_ERROR;
  char* str = NULL;
  char MY_LICENSE_FILE[1024];
  int major = LS_MAJOR_VER_NUMBER, minor=LS_MINOR_VER_NUMBER;

  // relative path to the loader app
  sprintf(MY_LICENSE_FILE, "../../../license/lndapi%d%d.lic", major, minor);
  nErrorCode = LSloadLicenseString(MY_LICENSE_FILE, MY_LICENSE_KEY);
  if (!nErrorCode) return nErrorCode;

  // try environment variable for license file
  str = getenv("LINDOAPI_LICENSE_FILE");
  if (str) {
    nErrorCode = LSloadLicenseString(str, MY_LICENSE_KEY);
    if (!nErrorCode) return nErrorCode;
  }
  else {
    printf("Warning: LINDOAPI_LICENSE_HOME not set\n");
  }

  // try LINDOAPI_HOME
  str = getenv("LINDOAPI_HOME");
  if (str) {
    sprintf(MY_LICENSE_FILE, "%s/license/lndapi%d%d.lic", str, major, minor);
    nErrorCode = LSloadLicenseString(MY_LICENSE_FILE, MY_LICENSE_KEY);
    if (!nErrorCode) return nErrorCode;
  }
  else {
    printf("Warning: LINDOAPI_HOME not set\n");
  }

  return nErrorCode;
}