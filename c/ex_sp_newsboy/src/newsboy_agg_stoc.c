

{ /* Load a single block */
  int      errorcode   = 0;
  int      iStage      = 1;
  int      nBlockEvents= 6; 
  int      pakStart[] =  { 0,      2,      4,      6,      8,     10,   12}; 
  int      paiStvs[]  =  { 0, 1,   0, 1,   0, 1,   0, 1,   0, 1,   0, 1};     
  double   padVals[]  =  { 90,9,  90,-15,  60,9,  60,-15,  30,9,  30,-15};

  double   padProb[]  =  { 0.12,  0.28,  0.15,  0.15,  0.27,  0.03 };

  errorcode=LSaddDiscreteBlocks(pModel,iStage,nBlockEvents,
        padProb,pakStart,NULL,NULL,paiStvs,padVals,LS_REPLACE);
  APIERRORCHECK2(errorcode);
} // end-block
