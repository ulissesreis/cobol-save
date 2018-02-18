      ******************************************************************
      * SISTEMA         - CURSO DE PROGRAMACAO COBOL II      MAINFRAME *
      ******************************************************************
      * BOOK DO ARQUIVO DE SAIDA - CADFSEL           - LCREL 100 BYTES *
      ******************************************************************
      * NOME DO BOOK    - COBO1006 - CADASTRO DE COTACAO SELECIONADA   *
      ******************************************************************
      * PSEL-COD-PEC    - PIC 9(005)        - CODIGO DA PECA           *
      * PSEL-NOME       - PIC X(030)        - NOME DA PECA             *
      * PSEL-QTD-MIN    - PIC 9(005)        - QUANTIDADE MINIMA PECAS  *
      * PSEL-QTD-MAX    - PIC 9(005)        - QUANTIDADE MAXIMA PECAS  *
      * PSEL-DATA       - PIC 9(008)        - DATA COTACAO (AAAAMMDD)  *
      * PSEL-FORNECE    - PIC 9(010)        - CODIGO DO FORNECEDOR     *
      * PSEL-VLR-UNI    - PIC 9(013)V9(002) - VALOR UNITARIO PECA      *
      * FILLER          - PIC X(022)        - AREA LIVRE               *
      ******************************************************************
      *
       01          REG-PSEL.
           03      PSEL-COD-PEC    PIC     9(005).
           03      PSEL-NOME       PIC     X(030).
           03      PSEL-QTD-MIN    PIC     9(005).
           03      PSEL-QTD-MAX    PIC     9(005).
           03      PSEL-DATA       PIC     9(008).
           03      PSEL-FORNECE    PIC     9(010).
           03      PSEL-VLR-UNI    PIC     9(013)V9(002).
           03      FILLER          PIC     X(022).
      *
      ******************************************************************
      * FIM DO BOOK DO ARQUIVO DE SAIDA        SEQ. - OUTPUT - CADFSEL *
      ******************************************************************