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

  File   : ex_datetime.c

  Purpose: Demonstrate the usage for Date-Time functions.

*/
#ifdef _MSC_VER
#pragma warning( once : 4702 )
#pragma warning( once : 4996 )
#endif

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <assert.h>
#define csv(a) printf("%d\n",(int)a)
/* LINDO API header file */
#include "lindo.h"
#define APIVERSION \
{\
    char szVersion[255], szBuild[255];\
    LSgetVersionInfo(szVersion,szBuild);\
    printf("\nLINDO API Version %s built on %s\n",szVersion,szBuild);\
}\

#define SEPS 1e-5

#define ASSERT3(ymdnow,hmsnow,ssec,ymd1,hms1){\
  ssec1=LSdateScalarSec(ymdnow,hmsnow);\
  LSdateScalarSecInverse(ssec1,&ymd1,&hms1);\
  printf("(%s%s%s)* ",ymdnow!=ymd1?"D":" ",hmsnow!=hms1?"T":" ",fabs(ssec-ssec1)>SEPS?"S":" ");\
  printf("%d,%12.4f: ssec:%20.4f, ymd:%08d, hms:%12.4f\n",ymdnow,hmsnow,ssec1,ymd1,hms1);\
  }
#define ASSERT1(a,b) if (a!=b) printf("Assert failed %d != %d\n",a,b);
#define ASSERT2(a,b) if (fabs(a-b)>SEPS) printf("Assert failed %.20g != %.20g\n",a,b);

/*/////////////////////////////////////////////////////////////////////*
 *
 *      Main
 *
 */
int main(int argc, char **argv) {
  int i,k;
  int ymd1, ymd2, ymdnow, jday1;
  double hms1=123456, hms2, dmaxerr=0, rnd, delta;
  double ssec1, shour1, sh1, dp, hmsnow;
  char stime[255], sdate[255];
  int nyr=1839, nmo=1, nda=15;
  int nhr=2, nmin=24; double dsc=22.06145;

  APIVERSION;

  LSdateSetBaseDate(1800,03,01);
  //LSdateSetBaseDate(1900,01,01);
  //LSdateSetBaseDate(0000,01,01);

  printf("\nDebug instance ymd,hms\n");
  ymd1=LSdateMakeDate(1800,01,01);
  hms1=LSdateMakeTime(12,34,56.781);
  ssec1=LSdateScalarSec(ymd1,hms1);
  printf("ssec(%8d,%12.4f): %32.4f\n",ymd1,hms1,ssec1);
  LSdateScalarSecInverse(ssec1,&ymd2,&hms2);
  printf("ssecinv(%32.4f): %08d %12.4f\n",ssec1,ymd2,hms2);

  printf("\nDebug instance ymd,hms\n");
  printf( "input  date: %d %d %d %d %d %g\n", nyr, nmo, nda, nhr, nmin, dsc);

  ymd1=LSdateMakeDate(nyr,nmo,nda);
  hms1=LSdateMakeTime(nhr,nmin,dsc);

  shour1=LSdateScalarHour(ymd1,hms1);
  printf("shour(%8d,%12.4f): %32.4f\n",ymd1,hms1,shour1);
  LSdateScalarHourInverse(shour1,&ymd2,&hms2);
  printf("shourinv(%32.4f): %08d %12.4f\n",shour1,ymd2,hms2);

  printf( "output date: %d %d %d %d %d %g\n",
     LSdateYear( ymd2),
     LSdateMonth( ymd2),
     LSdateDay( ymd2),
     LSdateHour( hms2),
     LSdateMinute( hms2),
     LSdateSecond( hms2)
  );

  ymd1=LSdateMakeDate(1900,01,01);
  hms1=LSdateMakeTime(12,34,56.781);

  ssec1=LSdateScalarSec(ymd1,hms1);
  printf("ssec(%8d,%12.4f): %32.4f\n",ymd1,hms1,ssec1);
  LSdateScalarSecInverse(ssec1,&ymd2,&hms2);
  printf("ssecinv(%32.4f): %08d %12.4f\n",ssec1,ymd2,hms2);

  shour1=LSdateScalarHour(ymd1,hms1);
  printf("shour(%8d,%12.4f): %32.4f\n",ymd1,hms1,shour1);
  LSdateScalarHourInverse(shour1,&ymd2,&hms2);
  printf("shourinv(%32.4f): %08d %12.4f\n",shour1,ymd2,hms2);

  printf(" shift +1 hrs\n");
  for (i=1; i<10; i++) {
    sh1 = shour1+i;
    LSdateScalarHourInverse(sh1,&ymd2,&hms2);
    printf("shourinv(%32.4f): %08d %12.4f\n",sh1,ymd2,hms2);
  }

  printf(" shift +6 hrs\n");
  for (i=1; i<10; i++) {
    sh1 = shour1+i*6;
    LSdateScalarHourInverse(sh1,&ymd2,&hms2);
    printf("shourinv(%32.4f): %08d %12.4f\n",sh1,ymd2,hms2);
  }

  printf(" accuracy in centiseconds \n");
  ssec1=LSdateScalarSec(20100101,123456.78);
  dp = 0.0;
  for (i=1; i<10; i++, dp+=0.01) {
    LSdateScalarSecInverse(ssec1+dp,&ymd2,&hms2);
    printf("ssecinv(%32.4f): %08d %12.4f\n",ssec1+dp,ymd2,hms2);
  }

  printf(" accuracy in milliseconds \n");
  ssec1=LSdateScalarSec(20100101,123456.78);
  dp = 0.0;
  for (i=1; i<10; i++, dp+=0.001) {
    LSdateScalarSecInverse(ssec1+dp,&ymd2,&hms2);
    printf("ssecinv(%32.4f): %08d %12.4f\n",ssec1+dp,ymd2,hms2);
  }

  printf(" accuracy in centihours \n");
  shour1=LSdateScalarHour(20100101,123456.78);
  dp = 0.0;
  for (i=1; i<10; i++, dp+=0.01) {
    LSdateScalarHourInverse(shour1+dp,&ymd2,&hms2);
    printf("shourinv(%32.4f): %08d %12.4f\n",shour1+dp,ymd2,hms2);
  }

  printf(" accuracy in 2.78e-6 hours \n");
  shour1=LSdateScalarHour(20100101,123456.78);
  dp = 0.0;
  for (i=1; i<10; i++, dp+=1.0/LSHOUR01/100.0) {
    LSdateScalarHourInverse(shour1+dp,&ymd2,&hms2);
    printf("shourinv(%32.8f): %08d %12.4f\n",shour1+dp,ymd2,hms2);
  }

  printf(" accuracy in 2.78e-7 \n");
  shour1=LSdateScalarHour(20100101,123456.78);
  dp = 0.0;
  for (i=1; i<10; i++, dp+=1.0/LSHOUR01/1000.0) {
    LSdateScalarHourInverse(shour1+dp,&ymd2,&hms2);
    printf("shourinv(%32.8f): %08d %12.4f\n",shour1+dp,ymd2,hms2);
  }

  LSdateNow(&ymdnow,&hmsnow);
  jday1=LSdateJulianDay(ymdnow);
  printf(" accuracy around now (jday:%d)\n",jday1);
  srand(1);
  for (i=1; i<50; i++) {
    LSdateNow(&ymdnow,&hmsnow);
    ssec1=LSdateScalarSec(ymdnow,hmsnow);
    LSdateScalarSecInverse(ssec1,&ymd1,&hms1);
    if (fabs(hms1-hmsnow)>0) {
      dmaxerr=fabs(hms1-hmsnow);
    }
    sprintf(stime,"%02d:%02d:%.6g",LSdateHour(hms1),LSdateMinute(hms1),LSdateSecond(hms1));
    sprintf(sdate,"%04d/%02d/%02d",LSdateYear(ymd1),LSdateMonth(ymd1),LSdateDay(ymd1));
    printf("%d,%12.4f: ssec:%20.4f, ymd:%08d, hms:%12.4f %s %s\n",ymdnow,hmsnow,ssec1,ymd1,hms1,sdate,stime);
    ASSERT1(ymd1,ymdnow);
    ASSERT2(hms1,hmsnow);
    for (k=0; k<100000+rand(); k++);
  }
  printf(" maxerr: %g\n",dmaxerr);


  printf("\nMillisecond accuracy\n");
  delta=0;
  ymdnow=20130110;
  hmsnow=025845.845;
  ssec1=LSdateScalarSec(ymdnow,hmsnow);
  LSdateScalarSecInverse(ssec1,&ymd1,&hms1);
  sprintf(stime,"%02d:%02d:%.6g (%g)",LSdateHour(hms1),LSdateMinute(hms1),LSdateSecond(hms1),delta);
  sprintf(sdate,"%04d/%02d/%02d",LSdateYear(ymd1),LSdateMonth(ymd1),LSdateDay(ymd1));
  printf("%d,%12.4f: ssec:%20.4f, ymd:%08d, hms:%12.4f %s %s\n",ymdnow,hmsnow,ssec1,ymd1,hms1,sdate,stime);

  delta=1/1000.0;
  ssec1+=delta;
  LSdateScalarSecInverse(ssec1,&ymd1,&hms1);
  ymdnow=ymd1;
  hmsnow=hms1;
  sprintf(stime,"%02d:%02d:%.6g (%g)",LSdateHour(hms1),LSdateMinute(hms1),LSdateSecond(hms1),delta);
  sprintf(sdate,"%04d/%02d/%02d",LSdateYear(ymd1),LSdateMonth(ymd1),LSdateDay(ymd1));
  printf("%d,%12.4f: ssec:%20.4f, ymd:%08d, hms:%12.4f %s %s\n",ymdnow,hmsnow,ssec1,ymd1,hms1,sdate,stime);

  ssec1=LSdateScalarSec(20130110,025845.849);
  printf("\nMove into the future from %.20g (3hr steps + random seconds)\n",ssec1);
  for (i=0; i<10; i++) {
    rnd = rand()/((double)RAND_MAX+1);
    delta=i*LSHOUR03+i*rnd*LSSEC05;
    LSdateScalarSecInverse(ssec1+delta,&ymd1,&hms1);
    sprintf(stime,"%02d:%02d:%.6g (%g)",LSdateHour(hms1),LSdateMinute(hms1),LSdateSecond(hms1),delta);
    sprintf(sdate,"%04d/%02d/%02d",LSdateYear(ymd1),LSdateMonth(ymd1),LSdateDay(ymd1));
    printf("%32.6f: %s %s\n",ssec1+delta,sdate,stime);
  }

  printf("\nMove into the past from %.20g (30min steps + random seconds)\n",ssec1);
  for (i=0; i<10; i++) {
    rnd = rand()/((double)RAND_MAX+1);
    delta = -(i*LSMIN30+i*rnd*LSSEC05);
    LSdateScalarSecInverse(ssec1+delta,&ymd1,&hms1);
    sprintf(stime,"%02d:%02d:%.10g (%g)",LSdateHour(hms1),LSdateMinute(hms1),LSdateSecond(hms1),delta);
    sprintf(sdate,"%04d/%02d/%02d",LSdateYear(ymd1),LSdateMonth(ymd1),LSdateDay(ymd1));
    printf("%32.6f: %s %s\n",ssec1+delta,sdate,stime);
  }

  printf("\nDebug instance ssec\n");
  delta = 0;
  ssec1 = 152263756743.101200;
  LSdateScalarSecInverse(ssec1,&ymd1,&hms1);
  sprintf(stime,"%02d:%02d:%.10g (%g)",LSdateHour(hms1),LSdateMinute(hms1),LSdateSecond(hms1),delta);
  sprintf(sdate,"%04d/%02d/%02d",LSdateYear(ymd1),LSdateMonth(ymd1),LSdateDay(ymd1));
  printf("%32.6f: %s %s\n",ssec1+delta,sdate,stime);

  {
    int nYr1,  nMon1,  nDay1,  nHr1,  nMin1; double dSec1, dSc1=0;
    int nYr2,  nMon2,  nDay2,  nHr2,  nMin2; double dSec2, dSc2=0;
    int nYr3,  nMon3,  nDay3,  nHr3,  nMin3; double dSec3, dSc3=0;
    int nDow;
    double dSecdiff, r;
    srand(1001);
    for (i=0; i<10; i++) {
      r = rand()/(RAND_MAX+1.0);
      nYr1 = (int) floor(4000*r)+1;
      r = rand()/(RAND_MAX+1.0);
      nMon1 = (int) floor(1+ 11.999*r);
      r = rand()/(RAND_MAX+1.0);
      nDay1 = (int) floor(1.5 + 27*r);
      r = rand()/(RAND_MAX+1.0);
      nHr1 = (int) floor(23.9999999 *r);
      r = rand()/(RAND_MAX+1.0);
      nMin1 = (int) floor(59.999999*r);
      r = rand()/(RAND_MAX+1.0);
      dSec1 = 59.999999*r;
      dSec1 = floor(dSec1*100000)/100000;

      r = rand()/(RAND_MAX+1.0);
      nYr2 = (int) floor(4000*r)+1;
      r = rand()/(RAND_MAX+1.0);
      nMon2 = (int) floor(1+ 11.999*r);
      r = rand()/(RAND_MAX+1.0);
      nDay2 = (int) floor(1.5 + 27*r);
      r = rand()/(RAND_MAX+1.0);
      nHr2 = (int) floor(23.9999999 *r);
      r = rand()/(RAND_MAX+1.0);
      nMin2 = (int) floor(59.999999*r);
      r = rand()/(RAND_MAX+1.0);
      dSec2 = 59.999999*r;
      dSec2 = floor(dSec2*100000)/100000;

      // diff b/w two timestamps s = 2 - 1
      LSdateDiffSecs(nYr1, nMon1, nDay1, nHr1, nMin1, dSec1,
        nYr2, nMon2, nDay2, nHr2, nMin2,dSec2, &dSecdiff);
      fprintf(stdout,"AA: [%4d-%02d-%02d-%02d:%02d:%10.7f] - [%4d-%02d-%02d-%02d:%02d:%10.7f] = %20.7f\n",
        nYr1, nMon1, nDay1, nHr1, nMin1, dSec1,nYr2, nMon2, nDay2, nHr2, nMin2,dSec2,dSecdiff);

      // 3 = 1 + s
      LSdateYmdhms(dSecdiff,nYr1, nMon1, nDay1, nHr1, nMin1,dSec1,&nYr3, &nMon3, &nDay3, &nHr3, &nMin3,&dSec3,&nDow);

      // rediff two date s2 = 3 - 1
      LSdateDiffSecs(nYr1, nMon1, nDay1, nHr1, nMin1, dSec1,
        nYr3, nMon3, nDay3, nHr3, nMin3,dSec3, &dSecdiff);
      fprintf(stdout,"BB: [%4d-%02d-%02d-%02d:%02d:%10.7f] - [%4d-%02d-%02d-%02d:%02d:%10.7f] = %20.7f",
        nYr1, nMon1, nDay1, nHr1, nMin1, dSec1,nYr3, nMon3, nDay3, nHr3, nMin3,dSec3,dSecdiff);
      if (nYr2!=nYr3 || nDay2!=nDay3 || nHr2!=nHr3 || nMin2!=nMin3 || fabs(dSec2-dSec3)>1e-5)
        fprintf(stdout,"***\n");
      else
        fprintf(stdout,"\n");

    }
  }
  /* Wait until user presses the Enter key */
  printf("Press <Enter> ...");
  getchar();
  return 0;
}

