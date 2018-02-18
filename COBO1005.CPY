      ******************************************************************
      * SISTEMA         - CURSO DE PROGRAMACAO COBOL II      MAINFRAME *
      ******************************************************************
      * BOOK DO ARQUIVO DE SAIDA - CADPRES           - LCREL 100 BYTES *
      ******************************************************************
      * NOME DO BOOK    - COBO1005 - CADASTRO DE RESUMO                *
      ******************************************************************
      * PRES-COD-MENOR  - PIC 9(005)        - CODIGO DA PECA  -  MENOR *
      * PRES-VLR-MENOR  - PIC 9(013)V9(002) - VALOR UNITARIO  -  MENOR *
      * PRES-COD-MAIOR  - PIC 9(005)        - CODIGO DA PECA  -  MAIOR *
      * PRES-VLR-MAIOR  - PIC 9(013)V9(002) - VALOR UNITARIO  -  MAIOR *
      * PRES-VLR-MEDIA  - PIC 9(013)V9(002) - VALOR DO RESUMO -  MEDIA *
      * FILLER          - PIC X(045)        - AREA LIVRE               *
      ******************************************************************
      *
       01          REG-PRES.
           03      PRES-COD-MENOR  PIC     9(005).
           03      PRES-VLR-MENOR  PIC     9(013)V9(002).
           03      PRES-COD-MAIOR  PIC     9(005).
           03      PRES-VLR-MAIOR  PIC     9(013)V9(002).
           03      PRES-VLR-MEDIA  PIC     9(013)V9(002).
           03      FILLER          PIC     X(045).
      *
      ******************************************************************
      * FIM DO BOOK DO ARQUIVO DE SAIDA        SEQ. - OUTPUT - CADPRES *
      ******************************************************************