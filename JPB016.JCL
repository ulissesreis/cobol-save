//GPFPB016 JOB GPFPB016,CLASS=A,MSGCLASS=X
//*
//STEP1   EXEC PGM=GPFPB016
//*CADPECA DD DUMMY
//CADPECA DD DSN=AD.CADPECA.DAT,DISP=SHR
//CADPREL DD SYSOUT=*
//*CADPREL DD DSN=AD.CADPREL.DAT,DISP=OLD,
//*           SPACE=(TRK,(1,1),RLSE),UNIT=DISCO,
//*           DCB=(LRECL=133,RECFM=FB,DSORG=PS)
//SYSOUT DD SYSOUT=*
//*
