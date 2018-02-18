      ******************************************************************
      * SISTEMA         - CURSO DE PROGRAMACAO COBOL II      MAINFRAME *
      ******************************************************************
      * BOOK DO ARQUIVO DE SAIDA - CADPSEL           - LCREL 100 BYTES *
      ******************************************************************
      * NOME DO BOOK    - COBO1002 - CADASTRO DE PECA SELECIONADA      *
      ******************************************************************
      * PSEL-COD-PEC    - PIC 9(005)        - CODIGO DA PECA           *
      * PSEL-NOME       - PIC X(030)        - NOME DA PECA             *
      * PSEL-QTD-ETQ    - PIC 9(005)        - QUANTIDADE PECAS ESTOQUE *
      * PSEL-VLR-UNIT   - PIC 9(013)V9(002) - VALOR UNITARIO DA PECA   *
      * FILLER          - PIC X(045)        - AREA LIVRE               *
      ******************************************************************
      *
       01          REG-PSEL.
           03      PSEL-COD-PEC    PIC     9(005).
           03      PSEL-NOME       PIC     X(030).
           03      PSEL-QTD-ETQ    PIC     9(005).
           03      PSEL-VLR-UNIT   PIC     9(013)V9(002).
           03      FILLER          PIC     X(045).
      *
      ******************************************************************
      * FIM DO BOOK DO ARQUIVO DE SAIDA        SEQ. - OUTPUT - CADPSEL *
      ******************************************************************