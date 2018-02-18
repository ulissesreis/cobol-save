      ******************************************************************
      * SISTEMA         - CURSO DE PROGRAMACAO COBOL II      MAINFRAME *
      ******************************************************************
      * BOOK DO ARQUIVO DE SAIDA - CADFOCO           - LCREL 100 BYTES *
      ******************************************************************
      * NOME DO BOOK    - COBO2006 - CADASTRO DE PECAS DESPREZADAS     *
      ******************************************************************
      * DESP-COD-PEC    - PIC 9(005)        - CODIGO DA PECA           *
      * DESP-NOME       - PIC X(030)        - NOME DA PECA             *
      * DESP-QTD-MIN    - PIC 9(005)        - QUANTIDADE MINIMA PECAS  *
      * DESP-QTD-MAX    - PIC 9(005)        - QUANTIDADE MAXIMA PECAS  *
      * DESP-DATA       - PIC 9(008)        - DATA COTACAO (AAAAMMDD)  *
      * DESP-FORNECE    - PIC 9(010)        - CODIGO DO FORNECEDOR     *
      * DESP-VLR-UNI    - PIC 9(013)V9(002) - VALOR UNITARIO PECA      *
      * FILLER          - PIC X(022)        - AREA LIVRE               *
      ******************************************************************
      *
       01          REG-DESP.
           03      DESP-COD-PEC    PIC     9(005).
           03      DESP-NOME       PIC     X(030).
           03      DESP-QTD-MIN    PIC     9(005).
           03      DESP-QTD-MAX    PIC     9(005).
           03      DESP-DATA       PIC     9(008).
           03      DESP-FORNECE    PIC     9(010).
           03      DESP-VLR-UNI    PIC     9(013)V9(002).
           03      FILLER          PIC     X(022).
      *
      ******************************************************************
      * FIM DO BOOK DO ARQUIVO DE SAIDA        SEQ. - OUTPUT - CADFOCO *
      ******************************************************************