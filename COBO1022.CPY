      ******************************************************************
      * SISTEMA         - CURSO DE PROGRAMACAO COBOL II      MAINFRAME *
      ******************************************************************
      * BOOK DO ARQUIVO DE SAIDA   - CADSELF         - LCREL 100 BYTES *
      ******************************************************************
      * NOME DO BOOK    - COBO1022 - CAD.FILIAIS REGIONAIS SELECIONADAS*
      ******************************************************************
      * FILS-COD-FIL   - PIC 9(005)        - CODIGO DA FILIAL          *
      * FILS-COD-REG   - PIC X(004)        - CODIGO REGIONAL           *
      * FILS-DAT-MOV   - PIC 9(008)        - DATA MOVIMENTO (AAAAMMDD) *
      * FILS-VLR-FAT   - PIC 9(013)V9(002) - VALOR UNITARIO DA PECA    *
      * FILLER         - PIC X(068)        - AREA LIVRE                *
      ******************************************************************
      *
       01          FILS-REG.
           03      FILS-COD-FIL    PIC     9(005).
           03      FILS-COD-REG    PIC     X(004).
           03      FILS-DAT-MOV    PIC     9(008).
           03      FILS-VLR-FAT    PIC     9(013)V9(002).
           03      FILLER          PIC     X(068).
      *
      ******************************************************************
      * FIM DO BOOK DO ARQUIVO DE SAIDA         SEQ. - INPUT - CADSELF *
      ******************************************************************
