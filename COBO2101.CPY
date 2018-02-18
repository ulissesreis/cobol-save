      ******************************************************************
      * SISTEMA         - CURSO DE PROGRAMACAO COBOL II      MAINFRAME *
      ******************************************************************
      * BOOK DO ARQUIVO DE SAIDA   - S1LQEX22        - LCREL 100 BYTES *
      ******************************************************************
      * NOME DO BOOK    - COBO2101 - SOLICITACOES NAO EFETIVADAS       *
      ******************************************************************
      * S1LQ-CD-ENTI     - PIC 9(007)       - CODIGO DA ENTIDADE       *
      * S1LQ-CD-CENT     - PIC 9(005)       - CODIGO DO CENTRO         *
      * S1LQ-AN-PROP     - PIC 9(004)       - ANO DA PROPOSTA          *
      * S1LQ-NR-SOLI     - PIC 9(013)       - NUMERO DE SOLICITACAO    *
      * S1LQ-TP-SOLI     - PIC X(001)       - TIPO DE SOLICITACAO      *
      * S1LQ-TX-DESC     - PIC X(030)       - DESCRICAO DA SOLICITACAO *
      * FILLER           - PIC X(040)       - AREA LIVRE               *
      ******************************************************************
      *
       01          S1LQ-REG.
           03      S1LQ-CD-ENTI    PIC     9(007).
           03      S1LQ-CD-CENT    PIC     9(005).
           03      S1LQ-AN-PROP    PIC     9(004).
           03      S1LQ-NR-SOLI    PIC     9(013).
           03      S1LQ-TP-SOLI    PIC     X(001).
           03      S1LQ-TX-DESC    PIC     X(030).
           03      FILLER          PIC     X(040).
      *
      ******************************************************************
      * FIM DO BOOK DO ARQUIVO DE SAIDA       SEQ. - OUTPUT - S1LQEX22 *
      ******************************************************************
