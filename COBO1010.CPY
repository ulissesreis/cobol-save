      ******************************************************************
      * SISTEMA         - CURSO DE PROGRAMACAO COBOL II      MAINFRAME *
      ******************************************************************
      * BOOK DO ARQUIVO DE SAIDA - CADMATR           - LCREL 100 BYTES *
      ******************************************************************
      * NOME DO BOOK    - COBO1010 - CADASTRO DA MATRIZ                *
      ******************************************************************
      * MATR-COD-PEC    - PIC 9(005)        - CODIGO DA PECA           *
      * MATR-NOME       - PIC X(030)        - NOME DA PECA             *
      * MATR-QTD-ETQ    - PIC 9(005)        - QUANTIDADE PECAS ESTOQUE *
      * MATR-QTD-MIN    - PIC 9(005)        - QUANTIDADE MINIMA PECAS  *
      * MATR-QTD-MAX    - PIC 9(005)        - QUANTIDADE MAXIMA PECAS  *
      * MATR-FORNECEDOR - PIC 9(010)        - CODIGO DO FORNECEDOR     *
      * MATR-VLR-UNIT   - PIC 9(013)V9(002) - VALOR UNITARIO DA PECA   *
      * FILLER          - PIC X(025)        - AREA LIVRE               *
      ******************************************************************
      *
       01          REG-MATR.
           03      MATR-COD-PEC    PIC     9(005).
           03      MATR-NOME       PIC     X(030).
           03      MATR-QTD-ETQ    PIC     9(005).
           03      MATR-QTD-MIN    PIC     9(005).
           03      MATR-QTD-MAX    PIC     9(005).
           03      MATR-FORNECEDOR PIC     9(010).
           03      MATR-VLR-UNIT   PIC     9(013)V9(002).
           03      FILLER          PIC     X(025).
      *
      ******************************************************************
      * FIM DO BOOK DO ARQUIVO DE SAIDA        SEQ. - OUTPUT - CADMATR *
      ******************************************************************