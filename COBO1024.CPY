      ******************************************************************
      * SISTEMA         - CURSO DE PROGRAMACAO COBOL II      MAINFRAME *
      ******************************************************************
      * BOOK DO ARQUIVO DE SAIDA           - CADBANF - LCREL 100 BYTES *
      ******************************************************************
      * NOME DO BOOK    - COBO1024 - CADASTRO BANCARIO COMPLETO FILIAL *
      ******************************************************************
      * BANF-COD-FIL   - PIC 9(005)        - CODIGO DA FILIAL          *
      * BANF-NOM-FIL   - PIC X(030)        - NOME DA FILIAL            *
      * BANF-COD-CLIE  - PIC 9(018)        - CODIGO UNICO DO CLIENTE   *
      * BANF-BANCO     - PIC 9(003)        - CODIGO DO BANCO           *
      * BANF-AGENCIA   - PIC 9(004)        - CODIGO DA AGENCIA         *
      * BANF-OPERACAO  - PIC 9(003)        - CODIGO DA OPERACAO        *
      *                                    - 003-CTA.CORRENTE          *
      *                                    - 006-CTA.CORRENTE JUDICIAL *
      * BANF-CONTA     - PIC 9(009)        - CODIGO DA CONTA/DIGITO    *
      *                                    - (1:8) - NUMERO DA CONTA   *
      *                                    - (9:1) - DIGITO DA CONTA   *
      * BANF-DAT-MOV   - PIC 9(008)        - DATA MOVIMENTO (AAAAMMDD) *
      * BANF-VLR-FAT   - PIC 9(013)V9(002) - VALOR FATURAMENTO         *
      * FILLER         - PIC X(005)        - AREA LIVRE                *
      ******************************************************************
      *
       01          BANF-REG.
           03      BANF-COD-FIL    PIC     9(005).
           03      BANF-NOM-FIL    PIC     X(030).
           03      BANF-COD-CLIE   PIC     9(018).
           03      BANF-BANCO      PIC     9(003).
           03      BANF-AGENCIA    PIC     9(004).
           03      BANF-OPERACAO   PIC     9(003).
           03      BANF-CONTA      PIC     9(009).
           03      BANF-DAT-MOV    PIC     9(008).
           03      BANF-VLR-FAT    PIC     9(013)V9(002).
           03      FILLER          PIC     X(005).
      *
      ******************************************************************
      * FIM DO BOOK DO ARQUIVO SAIDA           SEQ. - OUTPUT - CADBANF *
      ******************************************************************
