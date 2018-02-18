      ******************************************************************
      * SISTEMA         - CURSO DE PROGRAMACAO COBOL II      MAINFRAME *
      ******************************************************************
      * BOOK DO ARQUIVO DE SAIDA   - CADMFIN         - LCREL 100 BYTES *
      ******************************************************************
      * NOME DO BOOK    - COBO4001 - CADASTRO MOVIMENTO FINANCEIRO     *
      ******************************************************************
      * MFIN-AGENCIA    - PIC 9(004)        - CODIGO DA AGENCIA        *
      * MFIN-OPERACAO   - PIC 9(001)        - CODIGO DA OPERACAO       *
      *                 -                   - 1 - POUPANCA             *
      *                 -                   - 3 - CONTA CORRENTE       *
      * MFIN-CONTA      - PIC 9(009)        - 1 TAM 8 - CONTA CORRENTE *
      *                 -                   - 9 TAM 1 - DIGITO CONTA C.*
      * MFIN-COD-ENT    - PIC 9(005)        - CODIGO DA ENTIDADE       *
      * MFIN-TIP-ENT    - PIC 9(001)        - TIPO DA ENTIDADE         *
      * MFIN-NUM-GUIA   - PIC 9(012)        - NUMERO DA GUIA           *
      * MFIN-DAT-LANCTO - PIC 9(008)        - DT LANCAMENTO (AAAAMMDD) *
      * MFIN-VLR-LANCTO - PIC 9(015)        - VALOR DO LANCAMENTO      *
      * FILLER          - PIC X(045)        - AREA LIVRE               *
      ******************************************************************
      *
       01          REG-MFIN.
           03      MFIN-AGENCIA    PIC     9(004).
           03      MFIN-OPERACAO   PIC     9(001).
           03      MFIN-CONTA      PIC     9(009).
           03      MFIN-COD-ENT    PIC     9(005).
           03      MFIN-TIP-ENT    PIC     9(001).
           03      MFIN-NUM-GUIA   PIC     9(012).
           03      MFIN-DAT-LANCTO PIC     9(008).
           03      MFIN-VLR-LANCTO PIC     9(015).
           03      FILLER          PIC     X(045).
      *
      ******************************************************************
      * FIM DO BOOK DO ARQUIVO DE SAIDA         SEQ. - INPUT - CADMFIN *
      ******************************************************************