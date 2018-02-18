      ******************************************************************
      * SISTEMA         - CURSO DE PROGRAMACAO COBOL II      MAINFRAME *
      ******************************************************************
      * BOOK DO ARQUIVO DE ENTRADA - S2EQEX22        - LCREL 100 BYTES *
      ******************************************************************
      * NOME DO BOOK    - COBO2102 - SOLICITACOES EFETIVADAS           *
      ******************************************************************
      * S2EQ-COD-ENTIDAT     - PIC 9(007)   - CODIGO DA ENTIDADE       *
      * S2EQ-COD-CENTRO      - PIC 9(005)   - CODIGO DO CENTRO         *
      * S2EQ-ANO-PROPU       - PIC 9(004)   - ANO DA PROPOSTA          *
      * S2EQ-NUM-SOLICITU    - PIC 9(013)   - NUMERO DE SOLICITACAO    *
      * S2EQ-TIP-SOLICITA    - PIC X(001)   - TIPO DE SOLICITACAO      *
      * S2EQ-TXT-DESCRICAO   - PIC X(030)   - DESCRICAO DA SOLICITACAO *
      * FILLER               - PIC X(040)   - AREA LIVRE               *
      ******************************************************************
      *
       01          S2EQ-REG.
           03      S2EQ-COD-ENTIDAT     PIC     9(007).
           03      S2EQ-COD-CENTRO      PIC     9(005).
           03      S2EQ-ANO-PROPU       PIC     9(004).
           03      S2EQ-NUM-SOLICITU    PIC     9(013).
           03      S2EQ-TIP-SOLICITA    PIC     X(001).
           03      S2EQ-TXT-DESCRICAO   PIC     X(030).
           03      FILLER               PIC     X(040).
      *
      ******************************************************************
      * FIM DO BOOK DO ARQUIVO DE SAIDA       SEQ. - OUTPUT - S2EQEX22 *
      ******************************************************************
