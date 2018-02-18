      ******************************************************************
      * SISTEMA         - CURSO DE PROGRAMACAO COBOL II      MAINFRAME *
      ******************************************************************
      * BOOK DO ARQUIVO DE SAIDA - CADPREL           - LCREL 050 BYTES *
      ******************************************************************
      * NOME DO BOOK    - COBO1009 - CADASTRO DE PECAS RESUMO          *
      ******************************************************************
      * RESP-COD-PEC    - PIC 9(005)        - CODIGO DA PECA           *
      * RESP-QTD-ETQ    - PIC 9(007)        - QUANTIDADE PECAS ESTOQUE *
      * FILLER          - PIC X(038)        - AREA LIVRE               *
      ******************************************************************
      *
       01          REG-RESP.
           03      RESP-COD-PEC    PIC     9(005).
           03      RESP-QTD-ETQ    PIC     9(007).
           03      FILLER          PIC     X(038).
      *
      ******************************************************************
      * FIM DO BOOK DO ARQUIVO DE SAIDA        SEQ. - OUTPUT - CADPREL *
      ******************************************************************