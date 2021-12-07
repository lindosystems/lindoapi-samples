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

  File   : ex_dist_gen.c

  Purpose: Generate rvs from various distributions.

*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <assert.h>

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
    printf("\nLINDO API Version %s built on %s\n\n",szVersion,szBuild);\
}\

#ifdef _MSC_VER
/* ignore MSVC++ warnings that are annoying and hard to remove:
 4702 unreachable code
 (there is an unreachable assert(0) in case somehow it is reached)
 */
#pragma warning( disable : 4702 )
#endif

#define OPTERRCOLON (1)
#define OPTERRNF (2)
#define OPTERRARG (3)

char *optarg;
int optreset = 0;
int optind = 1;
int opterr = 1;
int optopt;

static int
optiserr(int argc, char * const *argv, int oint, const char *optstr,
         int optchr, int err)
{
    (void) argc;  /* not used */
    (void) optstr; /* not used */
    if(opterr)
    {
        fprintf(stderr, "Error in argument %d, char %d: ", oint, optchr+1);
        switch(err)
        {
        case OPTERRCOLON:
            fprintf(stderr, ": in flags\n");
            break;
        case OPTERRNF:
            fprintf(stderr, "option not found %c\n", argv[oint][optchr]);
            break;
        case OPTERRARG:
            fprintf(stderr, "no argument for option %c\n", argv[oint][optchr]);
            break;
        default:
            fprintf(stderr, "unknown\n");
            break;
        }
    }
    optopt = argv[oint][optchr];
    return('?');
}



int
getopt(int argc, char* const *argv, const char *optstr)
{
    static int optchr = 0;
    static int dash = 0; /* have already seen the - */

    char *cp;

    if (optreset)
        optreset = optchr = dash = 0;
    if(optind >= argc)
        return(EOF);
    if(!dash && (argv[optind][0] !=  '-'))
        return(EOF);
    if(!dash && (argv[optind][0] ==  '-') && !argv[optind][1])
    {
        /*
         * use to specify stdin. Need to let pgm process this and
         * the following args
         */
        return(EOF);
    }
    if((argv[optind][0] == '-') && (argv[optind][1] == '-'))
    {
        /* -- indicates end of args */
        optind++;
        return(EOF);
    }
    if(!dash)
    {
        assert((argv[optind][0] == '-') && argv[optind][1]);
        dash = 1;
        optchr = 1;
    }

    /* Check if the guy tries to do a -: kind of flag */
    assert(dash);
    if(argv[optind][optchr] == ':')
    {
        dash = 0;
        optind++;
        return(optiserr(argc, argv, optind-1, optstr, optchr, OPTERRCOLON));
    }
    cp = strchr(optstr, argv[optind][optchr]);
    if(!cp)
    {
        int errind = optind;
        int errchr = optchr;

        if(!argv[optind][optchr+1])
        {
            dash = 0;
            optind++;
        }
        else
            optchr++;
        return(optiserr(argc, argv, errind, optstr, errchr, OPTERRNF));
    }
    if(cp[1] == ':')
    {
        dash = 0;
        optind++;
        if(optind == argc)
            return(optiserr(argc, argv, optind-1, optstr, optchr, OPTERRARG));
        optarg = argv[optind++];
        return(*cp);
    }
    else
    {
        if(!argv[optind][optchr+1])
        {
            dash = 0;
            optind++;
        }
        else
            optchr++;
        return(*cp);
    }
    assert(0);
    return(0);
}

/* enable the callback function for distribution function
   computations (use -u to enable) */
int USER_DISTRIB	=0;

/* sample size (use -n 100 to increase it to 100) */
int SAMPLE_SIZE		=30;

/* use the discrete distribution specified with adPdfTable[]
  (use -d to enable) */
int DISCRETE		=0;

int parseopt (int argc, char **argv, int *optMask)
 {
      int tsum=0;
      extern char *optarg;
      extern int optind;
      int errflg = 0, nstg=0, errorcode=LSERR_NO_ERROR;
      char *ofile = NULL, *ptr=NULL;
      char seps[]   = ",", c, strbuf[255]="", *token=NULL;
      //double dargs[10];
      int k=0;

      *optMask = 0;

      while ((c = getopt(argc, argv, "?dun:")) != EOF)
           switch (c)
           {
           case 'n':
           		k = atoi(optarg);
           		if (k<=0)
					printf("invalid sample size %d\n",k);
				else
					SAMPLE_SIZE =k;
           		break;
           case 'd':
           		DISCRETE = 1;
           		break;
           case 'u':
           		USER_DISTRIB = 1;
           		break;

           case '?':
                errflg++;
           }

      if (errflg)
      {
           (void)fprintf(stderr,
                "\nusage: ex_dist_gen  [-?| -d | -u |-n <sample-size>]\n\n"
                "             -?              display options.  \n"
                "             -d              use sample discrete distribution.  \n"
                "             -u              use callback function for user distribution.  \n"
                "             -n=             sample size.  \n"
                );
           errorcode = LSERR_ERROR_IN_INPUT;
      }
      for ( ; optind < argc; optind++)
           (void)printf("%s\n", argv[optind]);


      return errorcode;
 }



/*/////////////////////////////////////////////////////////////////////*/

int LS_CALLTYPE UserDistr(pLSsample pSample, int nFuncType, double *dInput, int nInput, double *pdOutput, void *userData)
{
  int nErrorCode = 0;
  static pLSsample pSamp = NULL;

  if (pSamp == NULL)
  {
    pSamp = LSsampCreate(NULL, LSDIST_TYPE_NORMAL, &nErrorCode);
    nErrorCode = LSsampSetDistrParam(pSamp, 0,  2.0);
    nErrorCode = LSsampSetDistrParam(pSamp, 1, 10.0);
  }

  if (nFuncType == LS_CDFINV)
    nErrorCode = LSsampEvalDistr(pSamp,LS_CDFINV, *dInput, pdOutput);
  else if (nFuncType == LS_CDF)
    nErrorCode = LSsampEvalDistr(pSamp,LS_CDF, *dInput, pdOutput);
  else if (nFuncType == LS_PDF)
    nErrorCode = LSsampEvalDistr(pSamp,LS_PDF, *dInput, pdOutput);
  else if (nFuncType == LS_PDFDIFF)
    nErrorCode = LSsampEvalDistr(pSamp,LS_PDFDIFF, *dInput, pdOutput);

  return nErrorCode;
}

/*/////////////////////////////////////////////////////////////////////*
 *
 *      Main
 *
 */
int main(int argc, char **argv)
{
   APIERRORSETUP;

    /* Declare an instance of the LINDO environment object */
   pLSenv pEnv = NULL;

    /* Declare instances of the LINDO model object */
   pLSmodel pModel=NULL;

   pLSsample *paSample = NULL, pUserSample=NULL;


   pLSrandGen pRG = NULL;

   int itmp, i, j;
   int nDistType = LSDIST_TYPE_NORMAL;
   int nSamp, nDim;
   double dMean = 2.0, dXbar;
   double dSigma = 10.0, dStd, utmp, dtmp;
   double *pX=NULL;
   /* A PDF table for illustrating sampling from a discrete
   distribution */
#if 1
   double adPdfTable[10] = { 0.05, 0.05, 0.05, // .15
                             0.10, 0.15, 0.15, // .40
                             0.08, 0.07, 0.25, // .40
                             0.05 };           //1.00
#else
   double adPdfTable[10] = {0.02, 0.07, 0.09,
                            0.12, 0.20, 0.20,
                            0.18, 0.10, 0.01,
                            0.01 };
#endif
   int anCntTable[10][3];

   char MY_LICENSE_KEY[1024];


   APIVERSION;

   /*
    *     Parse options.
    */

   nErrorCode = parseopt(argc,argv,&i);
   if (nErrorCode)
     return (1);
   nSamp = SAMPLE_SIZE;
   nDim = 3;


   /*
    *     Create a LINDO environment.
    */
   nErrorCode = LSloadLicenseString("../../../license/lndapi130.lic",MY_LICENSE_KEY);

   pEnv = LScreateEnv ( &nErrorCode, MY_LICENSE_KEY);
   if ( nErrorCode == LSERR_NO_VALID_LICENSE) {
      printf( "Invalid License Key!\n");
      exit( 1);
   }
   APIERRORCHECK;


   pRG = LScreateRG(pEnv, LS_RANDGEN_LINDO2);

   LSsetRGSeed(pRG, 1031);

   paSample = calloc(nDim,sizeof(pLSsample));
   if (paSample == NULL)
   {
     nErrorCode = LSERR_OUT_OF_MEMORY;
     APIERRORCHECK;
   }

   for (j=0; j<nDim; j++)
   {
     if (!DISCRETE)
     {
       paSample[j] = LSsampCreate(pEnv, nDistType, &nErrorCode);
       APIERRORCHECK;

       nErrorCode = LSsampSetDistrParam(paSample[j], 0, dMean);
       APIERRORCHECK;

       nErrorCode = LSsampSetDistrParam(paSample[j], 1, dSigma);
       APIERRORCHECK;
     }
     else
     {
       for (i=0; i<10; i++) anCntTable[i][j] = 0;

       paSample[j] = LSsampCreate(pEnv, LSDIST_TYPE_DISCRETE, &nErrorCode);
       APIERRORCHECK;

       nErrorCode = LSsampLoadDiscretePdfTable(paSample[j], 10, adPdfTable,NULL);
       APIERRORCHECK;
     }

     nErrorCode = LSsampSetRG(paSample[j],pRG);
     APIERRORCHECK;
   }

   nErrorCode = LSsampGenerate(paSample[0], LS_LATINSQUARE, nSamp);
   APIERRORCHECK;
   nErrorCode = LSsampGenerate(paSample[1], LS_ANTITHETIC, nSamp);
   APIERRORCHECK;
   nErrorCode = LSsampGenerate(paSample[2], LS_MONTECARLO, nSamp);
   APIERRORCHECK;


   for (j=0; j<nDim; j++)
   {
     nErrorCode = LSsampGetPointsPtr(paSample[j],&itmp,&pX);
     fprintf(stdout,"\n%s %3s %13s %13s %13s\n"," ","idx","x","u=CDF(x)","x'=CDFINV(u)");
     for (i=0; i<nSamp; i++)
     {
       APIERRORCHECK;
       LSsampEvalDistr(paSample[j],LS_CDF,pX[i],&utmp);
       LSsampEvalDistr(paSample[j],LS_CDFINV,utmp,&dtmp);
       fprintf(stdout,"%d:%3d %13.7f %13.7f %13.7f\n",j,i,pX[i],utmp,dtmp);
       if (DISCRETE)
       {
         itmp = (int) pX[i];
         anCntTable[itmp][j]++;
       }
     }
   }

   if (DISCRETE)
   {
     dMean = 0.0;
     dSigma = 0;
     for (j=0; j<10; j++)
     {
       dMean += adPdfTable[j]*j;        //E[x]
       dSigma += pow(j,2)*adPdfTable[j];  //E[x^2]
     }
     dSigma -= dMean*dMean;
     dSigma = sqrt(dSigma);  //population stdev : sqrt(E[x^2] - E[x]^2)
   }


   fprintf(stdout,"\nSample info\n");
   for (j=0; j<nDim; j++)
   {
     nErrorCode = LSsampGetInfo(paSample[j],LS_DINFO_SAMP_MEAN,&dXbar);
     nErrorCode = LSsampGetInfo(paSample[j],LS_DINFO_SAMP_STD,&dStd);
     fprintf(stdout,"Sample :%d,  xbar=%13.7f (err=%%%7f), stdev=%13.7f (err=%%%7f)\n",
       j,dXbar,fabs(dMean-dXbar)*100/dMean,dStd,fabs(dStd-dSigma)*100/dSigma);
       //j,dXbar,dMean,dStd,dSigma);
   }

   if (DISCRETE)
   {
     fprintf(stdout,"\nObservation counts\n");
     for (i=0; i<10; i++)
     {
       fprintf(stdout,"%2d: ",i);
       for (j=0; j< nDim; j++)
       {
         fprintf(stdout,"%6d ",anCntTable[i][j]);
       }
       fprintf(stdout,"\n");
     }
   }


   /// test user cdfinv
   if (USER_DISTRIB)
   {
     fprintf(stdout,"\nTesting callback interface\n");
     pUserSample = LSsampCreate(pEnv, LSDIST_TYPE_USER, &nErrorCode);
     APIERRORCHECK;
     nErrorCode = LSsampSetRG(pUserSample,pRG);
     APIERRORCHECK;
     nErrorCode = LSsampSetUserDistr(pUserSample,UserDistr,NULL);
     APIERRORCHECK;
     nErrorCode = LSsampGenerate(pUserSample, LS_LATINSQUARE, nSamp);
     APIERRORCHECK;
     nErrorCode = LSsampGetPointsPtr(pUserSample,&itmp,&pX);
     fprintf(stdout,"\n%s %3s %13s %13s %13s\n"," ","idx","x","u=CDF(x)","x'=CDFINV(u)");
     for (i=0; i<nSamp; i++)
     {
       APIERRORCHECK;
       LSsampEvalDistr(pUserSample,LS_CDF,pX[i],&utmp);
       LSsampEvalDistr(pUserSample,LS_CDFINV,utmp,&dtmp);
       fprintf(stdout,"%d:%3d %13.7f %13.7f %13.7f\n",j,i,pX[i],utmp,dtmp);
     }
   }

Terminate:
   if (nErrorCode != LSERR_NO_ERROR)
   {
     char strbuf[255];
     LSgetErrorMessage(pEnv,nErrorCode,strbuf);
     printf ("\n Error %d: %s\n",nErrorCode,strbuf);
   }


   for (i=0; i<nDim; i++)
     nErrorCode = LSsampDelete( &paSample[i]);
   free(paSample);

   LSsampDelete(&pUserSample);

   LSdisposeRG( &pRG);

   nErrorCode = LSdeleteModel( &pModel);
   nErrorCode = LSdeleteEnv( &pEnv);

   /*
    *     Terminate
    */


  /* Wait until user presses the Enter key */
  printf("Press <Enter> ...");
  getchar();

  return 0;
}
