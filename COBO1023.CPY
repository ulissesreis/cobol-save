      ******************************************************************
      * SISTEMA         - CURSO DE PROGRAMACAO COBOL II      MAINFRAME *
      ******************************************************************
      * BOOK DO ARQUIVO DE SAIDA   - CADFSUM         - LCREL 100 BYTES *
      ******************************************************************
      * NOME DO BOOK    - COBO1023 - CAD.FILIAIS FATURAMENTO SUMARIZADO*
      ******************************************************************
      * FILF-COD-FIL   - PIC 9(005)        - CODIGO DA FILIAL          *
      * FILF-QTD-FAT   - PIC 9(009)        - CODIGO REGIONAL           *
      * FILF-VLR-FAT   - PIC 9(016)V9(002) - VALOR UNITARIO DA PECA    *
      * FILLER         - PIC X(068)        - AREA LIVRE                *
      ******************************************************************
      *
       01          FILF-REG.
           03      FILF-COD-FIL    PIC     9(005).
           03      FILF-QTD-FAT    PIC     9(009).
           03      FILF-VLR-FAT    PIC     9(016)V9(002).
           03      FILLER          PIC     X(068).
      *
      ******************************************************************
      * FIM DO BOOK DO ARQUIVO DE SAIDA         SEQ. - INPUT - CADFSUM *
      ******************************************************************
