//GPFPB005 JOB 'GPFPB005',CLASS=A,MSGCLASS=X
//*
//STEP1   EXEC PGM=GPFPB005
//*CADPECA DD DUMMY
//CADPECA DD DSN=AD.CADPECA.DAT,DISP=SHR
//CADPATU DD DSN=AD.CADPATU.DAT,DISP=OLD,
//           SPACE=(TRK,(1,1),RLSE),UNIT=DISCO,
//           DCB=(LRECL=100,RECFM=FB,DSORG=PS)
//SYSOUT DD SYSOUT=*
//*
