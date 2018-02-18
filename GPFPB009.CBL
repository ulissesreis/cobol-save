      *****************************************************************
       IDENTIFICATION              DIVISION.
      *****************************************************************
      *
       PROGRAM-ID.                 GPFPB009.
       AUTHOR.                     ULISSES SOUZA
       DATE-WRITTEN.               11/02/2014.
       SECURITY.
      *
      *****************************************************************
      * SISTEMA......: SIGPF - SISTEMA DE GESTAO DE PECAS/FILIAIS     *
      *****************************************************************
      * ANALISTA.....: ULISSES SOUZA                                  *
      * LINGUAGEM....: COBOL/BATCH                                    *
      * PROGRAMADOR..: ULISSES SOUZA                                  *
      * DATA.........: 11/02/2014                                     *
      *****************************************************************
      * OBJETIVO.....: A  PARTIR  DO  CADASTRO  DE  PECAS,  GERAR     *
      *                CADASTRO DE RESUMO DE PECAS                    *
      *****************************************************************
      *
      *****************************************************************
       ENVIRONMENT                 DIVISION.
      *****************************************************************
       CONFIGURATION               SECTION.
      *****************************************************************
       SPECIAL-NAMES.              DECIMAL-POINT   IS    COMMA.
      *****************************************************************
       INPUT-OUTPUT                SECTION.
      *****************************************************************
       FILE-CONTROL.
      *****************************************************************
      * INPUT..: CADPECA - CADASTRO DE PECAS            - LRECL = 100 *
      *****************************************************************
      *
           SELECT  CADPECA  ASSIGN TO  UT-S-CADPECA
                   FILE     STATUS IS  WS-FS-CADPECA.
      *
      *****************************************************************
      * OUTPUT.: CADRESP - RESUMO DE PECAS              - LRECL = 050 *
      *****************************************************************
      *
           SELECT  CADRESP  ASSIGN TO  UT-S-CADRESP
                   FILE     STATUS IS  WS-FS-CADRESP.
      *
      *****************************************************************
       DATA                        DIVISION.
      *****************************************************************
       FILE                        SECTION.
      *****************************************************************
      * INPUT..: CADPECA - CADASTRO DE PECAS            - LRECL = 100 *
      *****************************************************************
      *
       FD  CADPECA
           RECORDING  MODE      IS  F
           LABEL      RECORD    IS  STANDARD
           BLOCK      CONTAINS  0   RECORDS.
      *
       01      REG-CADPECA         PIC     X(100).
      *
      *****************************************************************
      * OUTPUT.: CADRESP - RESUMO DE PECAS              - LRECL = 050 *
      *****************************************************************
      *
       FD  CADRESP
           RECORDING  MODE      IS  F
           LABEL      RECORD    IS  STANDARD
           BLOCK      CONTAINS  0   RECORDS.
      *
       01      REG-CADRESP         PIC     X(050).
      *
      *****************************************************************
       WORKING-STORAGE             SECTION.
      *****************************************************************
      *
       01      WS-FS-CADPECA       PIC     9(002) VALUE ZEROS.
       01      WS-FS-CADRESP       PIC     9(002) VALUE ZEROS.
      *
       01      WS-LID-CADPECA      PIC     9(018) VALUE ZEROS.
       01      WS-GRV-CADRESP      PIC     9(018) VALUE ZEROS.
      *
       01      WS-EDICAO           PIC     Z.ZZZ.ZZ9.
      *
      ******************************************************************
      *        TRATAMENTO DE CHAVE DE QUEBRA                           *
      ******************************************************************
      *
       01      WS-CHV-ATU          PIC     X(005) VALUE ZEROS.
       01      FILLER              REDEFINES      WS-CHV-ATU.
         03    WS-COD-ATU          PIC     9(005).
      *
       01      WS-CHV-ANT          PIC     X(005) VALUE ZEROS.
       01      FILLER              REDEFINES      WS-CHV-ANT.
         03    WS-COD-ANT          PIC     9(005).
      *
      ******************************************************************
      *        TRATAMENTO DE ACUMULADORES                              *
      ******************************************************************
      *
       01      AC-QTD-ETQ          PIC     9(007) VALUE ZEROS.
      *
      *****************************************************************
      *        VARIAVEIS PARA TRATAMENTO DE ABEND                     *
      *****************************************************************
      *
       01      WS-ACESSO-ARQ       PIC     X(013) VALUE SPACES.
       01      WS-DDNAME-ARQ       PIC     X(008) VALUE SPACES.
       01      WS-FS-ARQ           PIC     9(002) VALUE ZEROS.
      *
       01      WS-PTO-ERRO         PIC     9(003) VALUE ZEROS.
      *
      *****************************************************************
      * INPUT..: CADPECA - CADASTRO DE PECAS            - LRECL = 100 *
      *****************************************************************
      *
           COPY    COBI1001.
      *
      *****************************************************************
      * OUTPUT.: CADRESP - CADASTRO DE PECAS ATUALIZADO - LRECL = 100 *
      *****************************************************************
      *
           COPY    COBO1009.
      *
      *****************************************************************
       LINKAGE                     SECTION.
      *****************************************************************
       PROCEDURE                   DIVISION.
      *****************************************************************
      *
           PERFORM 0100-00-PROCED-INICIAIS.

           PERFORM 1000-00-PROCED-PRINCIPAIS
             UNTIL WS-FS-CADPECA EQUAL 10.

           PERFORM 3000-00-PROCED-FINAIS.

           GOBACK.
      *
      *****************************************************************
       0100-00-PROCED-INICIAIS     SECTION.
      *****************************************************************
      *
           OPEN    INPUT   CADPECA
                   OUTPUT  CADRESP.

           MOVE   ' NA ABERTURA '  TO      WS-ACESSO-ARQ.

           MOVE    001             TO      WS-PTO-ERRO.

           PERFORM 0200-00-TESTA-FILE-STATUS.

           PERFORM 0500-00-LEITURA-CADPECA.

           IF      WS-FS-CADPECA   EQUAL   10
                   DISPLAY
                   '******************* GPFPB009 ******************'
                   DISPLAY
                   '*                                             *'
                   DISPLAY
                   '*          ARQUIVO CADPECA ESTA VAZIO         *'
                   DISPLAY
                   '*                                             *'
           ELSE
                   PERFORM         0150-00-MONTA-CHAVE
           END-IF.
      *
       0100-99-EXIT.
           EXIT.
      *
      *****************************************************************
       0150-00-MONTA-CHAVE         SECTION.
      *****************************************************************
      *
           MOVE    WS-CHV-ATU      TO      WS-CHV-ANT.

           MOVE    ZEROS           TO      AC-QTD-ETQ.
      *
       0150-99-EXIT.
           EXIT.
      *
      *****************************************************************
       0200-00-TESTA-FILE-STATUS   SECTION.
      *****************************************************************
      *
           PERFORM 0300-00-TESTA-FS-CADPECA.

           PERFORM 0400-00-TESTA-FS-CADRESP.
      *
       0200-99-EXIT.
           EXIT.
      *
      *****************************************************************
       0300-00-TESTA-FS-CADPECA    SECTION.
      *****************************************************************
      *
           IF      WS-FS-CADPECA NOT EQUAL 00 AND 10
                   MOVE 'CADPECA'  TO      WS-DDNAME-ARQ
                   MOVE  WS-FS-CADPECA
                                   TO      WS-FS-ARQ
                   PERFORM         0999-00-ABEND-ARQ
           END-IF.
      *
       0300-99-EXIT.
           EXIT.
      *
      *****************************************************************
       0400-00-TESTA-FS-CADRESP    SECTION.
      *****************************************************************
      *
           IF      WS-FS-CADRESP NOT EQUAL 00
                   MOVE 'CADRESP'  TO      WS-DDNAME-ARQ
                   MOVE  WS-FS-CADRESP
                                   TO      WS-FS-ARQ
                   PERFORM         0999-00-ABEND-ARQ
           END-IF.
      *
       0400-99-EXIT.
           EXIT.
      *
      *****************************************************************
       0500-00-LEITURA-CADPECA     SECTION.
      *****************************************************************
      *
           READ    CADPECA         INTO    REG-PECA.

           MOVE   ' NA LEITURA '   TO      WS-ACESSO-ARQ.

           MOVE    002             TO      WS-PTO-ERRO.

           PERFORM 0300-00-TESTA-FS-CADPECA.

           IF      WS-FS-CADPECA   EQUAL   00
                   ADD 001         TO      WS-LID-CADPECA
                   MOVE PECA-COD-PEC
                                   TO      WS-COD-ATU
           END-IF.
      *
       500-99-EXIT.
           EXIT.
      *
      *****************************************************************
       1000-00-PROCED-PRINCIPAIS   SECTION.
      *****************************************************************
      *
           IF      WS-CHV-ATU  NOT EQUAL   WS-CHV-ANT
                   PERFORM         1400-00-GRAVACAO-CADRESP
                   PERFORM         0150-00-MONTA-CHAVE
           END-IF.

           ADD     PECA-QTD-ETQ    TO      AC-QTD-ETQ.

           PERFORM 0500-00-LEITURA-CADPECA.
      *
       1000-99-EXIT.
           EXIT.
      *
      *****************************************************************
       3000-00-PROCED-FINAIS       SECTION.
      *****************************************************************
      *
           IF      WS-LID-CADPECA  GREATER ZEROS
                   PERFORM         1400-00-GRAVACAO-CADRESP
           END-IF.

           CLOSE   CADPECA
                   CADRESP.

           MOVE   'NO FECHAMENTO'  TO      WS-ACESSO-ARQ.

           MOVE    004             TO      WS-PTO-ERRO.

           PERFORM 0200-00-TESTA-FILE-STATUS.

           PERFORM 3100-00-MONTA-ESTATISTICA.
      *
       3000-99-EXIT.
           EXIT.
      *
      *****************************************************************
       1400-00-GRAVACAO-CADRESP    SECTION.
      *****************************************************************
      *
           MOVE    SPACES          TO      REG-RESP.

           MOVE    WS-COD-ANT      TO      RESP-COD-PEC.
           MOVE    AC-QTD-ETQ      TO      RESP-QTD-ETQ.

           WRITE   REG-CADRESP     FROM    REG-RESP.

           MOVE   ' NA GRAVACAO '  TO      WS-ACESSO-ARQ.

           MOVE    003             TO      WS-PTO-ERRO.

           PERFORM 0400-00-TESTA-FS-CADRESP.

           ADD     001             TO      WS-GRV-CADRESP.
      *
       1400-99-EXIT.
           EXIT.
      *
      *****************************************************************
       3100-00-MONTA-ESTATISTICA   SECTION.
      *****************************************************************
      *
           DISPLAY '******************* GPFPB009 ******************'.
           DISPLAY '*                                             *'.
           DISPLAY '*        ESTATISTICA DE PROCESSAMENTO         *'.
           DISPLAY '*                                             *'.
           DISPLAY '******************* GPFPB009 ******************'.
           DISPLAY '*                                             *'.
           MOVE    WS-LID-CADPECA  TO      WS-EDICAO.
           DISPLAY '* REGISTROS LIDOS.......- CADPECA.: ' WS-EDICAO
           ' *'.
           MOVE    WS-GRV-CADRESP  TO      WS-EDICAO.
           DISPLAY '* REGISTROS GRAVADOS....- CADRESP.: ' WS-EDICAO
           ' *'.
           DISPLAY '*                                             *'.
           DISPLAY '******************* GPFPB009 ******************'.
      *
       3100-99-EXIT.
           EXIT.
      *
      *****************************************************************
       0999-00-ABEND-ARQ           SECTION.
      *****************************************************************
      *
           MOVE    12              TO      RETURN-CODE.

           DISPLAY '******************* GPFPB009 ******************'.
           DISPLAY '*                                             *'.
           DISPLAY '*      TERMINO ANORMAL DE PROCESSAMENTO       *'.
           DISPLAY '*                                             *'.
           DISPLAY '******************* GPFPB009 ******************'.
           DISPLAY '*                                             *'.
           DISPLAY '* PROBLEMAS ' WS-ACESSO-ARQ ' DO ARQUIVO '
           WS-DDNAME-ARQ ' *'.
           DISPLAY '*                                             *'.
           DISPLAY '*             FILE STATUS....: ' WS-FS-ARQ
           '             *'.
           DISPLAY '*                                             *'.
           DISPLAY '*           PONTO COM ERRO...: ' WS-PTO-ERRO
           '            *'.
           DISPLAY '*                                             *'.
           DISPLAY '******************* GPFPB009 ******************'.
           DISPLAY '*     P R O G R A M A  C A N C E L A D O      *'.
           DISPLAY '******************* GPFPB009 ******************'.

      *    CALL    'IBM'.

           GOBACK.
      *
       0999-00-EXIT.
           EXIT.
      *
      *****************************************************************
      *                   FIM DO PROGRAMA - GPFPB009                  *
      *****************************************************************
