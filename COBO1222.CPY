      ******************************************************************
      * SISTEMA         - CURSO DE PROGRAMACAO COBOL II      MAINFRAME *
      ******************************************************************
      * BOOK DO ARQUIVO DE SAIDA   - CADMOVR         - LCREL 050 BYTES *
      ******************************************************************
      * NOME DO BOOK    - COBO1222 - CADASTRO DE PECAS MOV. RESUMO     *
      ******************************************************************
      * REG-PMOV        - PIC X(100)        - REG. TOTAL DO ARQUIVO    *
      ******************************************************************
      * MOVR-COD-PEC    - PIC 9(005)        - CODIGO DA PECA           *
      * MOVR-VLR-VEN    - PIC 9(015)V9(002) - VALOR ACUMULADO VENDA    *
      * MOVR-DAT-MOV    - PIC 9(008)        - DATA MOVIMENTO (AAAAMMDD)*
      * FILLER          - PIC X(020)        - AREA LIVRE               *
      ******************************************************************
      *
       01          REG-MOVR.
         03        MOVR-COD-PEC    PIC     9(005).
         03        MOVR-VLR-VEN    PIC     9(015)V9(002).
         03        MOVR-DAT-MOV    PIC     9(008).
         03        FILLER          PIC     X(020).
      *
      ******************************************************************
      * FIM DO BOOK DO ARQUIVO DE SAIDA        SEQ. - OUTPUT - CADMOVR *
      ******************************************************************