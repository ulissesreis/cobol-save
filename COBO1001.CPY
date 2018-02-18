      ******************************************************************
      * SISTEMA         - CURSO DE PROGRAMACAO COBOL II      MAINFRAME *
      ******************************************************************
      * BOOK DO ARQUIVO DE SAIDA - CADPATU           - LCREL 100 BYTES *
      ******************************************************************
      * NOME DO BOOK    - COBO1002 - CADASTRO DE PECA ATUALIZADO       *
      ******************************************************************
      * PATU-COD-PEC    - PIC 9(005)        - CODIGO DA PECA           *
      * PATU-NOME       - PIC X(030)        - NOME DA PECA             *
      * PATU-QTD-ETQ    - PIC 9(005)        - QUANTIDADE PECAS ESTOQUE *
      * PATU-VLR-UNIT   - PIC 9(013)V9(002) - VALOR UNITARIO DA PECA   *
      * FILLER          - PIC X(045)        - AREA LIVRE               *
      ******************************************************************
      *
       01          REG-PATU.
           03      PATU-COD-PEC    PIC     9(005).
           03      PATU-NOME       PIC     X(030).
           03      PATU-QTD-ETQ    PIC     9(005).
           03      PATU-VLR-UNIT   PIC     9(013)V9(002).
           03      FILLER          PIC     X(045).
      *
      ******************************************************************
      * FIM DO BOOK DO ARQUIVO DE SAIDA        SEQ. - OUTPUT - CADPATU *
      ******************************************************************