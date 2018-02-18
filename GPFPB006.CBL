      *****************************************************************
       IDENTIFICATION              DIVISION.
      *****************************************************************
      *
       PROGRAM-ID.                 GPFPB006.
       AUTHOR.                     ULISSES SOUZA
       DATE-WRITTEN.               16/06/2013.
       SECURITY.
      *
      *****************************************************************
      * SISTEMA......: SIGPF - SISTEMA DE GESTAO DE PECAS/FILIAIS     *
      *****************************************************************
      * ANALISTA.....: ULISSES SOUZA                                  *
      * LINGUAGEM....: COBOL/BATCH                                    *
      * PROGRAMADOR..: ULISSES SOUZA                                  *
      * DATA.........: 03/02/2014                                     *
      *****************************************************************
      * OBJETIVO.....: A  PARTIR  DO  CADASTRO  DE  PECAS,  GERAR     *
      *                CADASTRO DE PECAS RESUMIDO/PARAMETRIZADO       *
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
           SELECT  CADPECA  ASSIGN  TO  UT-S-CADPECA
                   FILE     STATUS  IS  WS-FS-CADPECA.
      *
      *****************************************************************
      * OUTPUT.: CADPRES - CADASTRO DE PECAS ATUALIZADO - LRECL = 100 *
      *****************************************************************
      *
           SELECT  CADPRES  ASSIGN  TO  UT-S-CADPRES
                   FILE     STATUS  IS  WS-FS-CADPRES.
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
      * OUTPUT.: CADPRES - CADASTRO DE PECAS ATUALIZADO - LRECL = 100 *
      *****************************************************************
      *
       FD  CADPRES
           RECORDING  MODE      IS  F
           LABEL      RECORD    IS  STANDARD
           BLOCK      CONTAINS  0   RECORDS.
      *
       01      REG-CADPRES         PIC     X(100).
      *
      *****************************************************************
       WORKING-STORAGE             SECTION.
      *****************************************************************
      *
       01      WS-FS-CADPECA       PIC     9(002) VALUE ZEROS.
       01      WS-FS-CADPRES       PIC     9(002) VALUE ZEROS.
      *
       01      WS-LID-CADPECA      PIC     9(018) VALUE ZEROS.
       01      WS-GRV-CADPRES      PIC     9(018) VALUE ZEROS.
      *
       01      WS-EDICAO           PIC     Z.ZZZ.ZZ9.
      *
      ******************************************************************
      *        VARIAVEIS PARA ARMAZENAMENTO DOS VALORES                *
      ******************************************************************
      *
       01       WS-VLR-TOTAL     PIC     9(016)V99 VALUE ZEROS.
      *
       01       WS-COD-MENOR     PIC     9(005)    VALUE ZEROS.
       01       WS-VLR-MENOR     PIC     9(013)V99 VALUE ZEROS.
      *
       01       WS-COD-MAIOR     PIC     9(005)    VALUE ZEROS.
       01       WS-VLR-MAIOR     PIC     9(013)V99 VALUE ZEROS.
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
      * OUTPUT.: CADPRES - CADASTRO DE PECAS ATUALIZADO - LRECL = 100 *
      *****************************************************************
      *
           COPY    COBO1005.
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
                   OUTPUT  CADPRES.

           MOVE   ' NA ABERTURA '  TO      WS-ACESSO-ARQ.

           MOVE    001             TO      WS-PTO-ERRO.

           PERFORM 0200-00-TESTA-FILE-STATUS.

           PERFORM 0500-00-LEITURA-CADPECA.

           IF      WS-FS-CADPECA   EQUAL   10
                   DISPLAY
                   '******************* GPFPB006 ******************'
                   DISPLAY
                   '*                                             *'
                   DISPLAY
                   '*          ARQUIVO CADPECA ESTA VAZIO         *'
                   DISPLAY
                   '*                                             *'
           ELSE
                   PERFORM         1100-00-TRATA-MENOR
                   PERFORM         1200-00-TRATA-MAIOR
           END-IF.
      *
       0100-99-EXIT.
           EXIT.
      *
      *****************************************************************
       0200-00-TESTA-FILE-STATUS   SECTION.
      *****************************************************************
      *
           PERFORM 0300-00-TESTA-FS-CADPECA.

           PERFORM 0400-00-TESTA-FS-CADPRES.
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
       0400-00-TESTA-FS-CADPRES    SECTION.
      *****************************************************************
      *
           IF      WS-FS-CADPRES NOT EQUAL 00
                   MOVE 'CADPRES'  TO      WS-DDNAME-ARQ
                   MOVE  WS-FS-CADPRES
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
           END-IF.
      *
       0500-99-EXIT.
           EXIT.
      *
      *****************************************************************
       1000-00-PROCED-PRINCIPAIS   SECTION.
      *****************************************************************
      *
           ADD    PECA-VLR-UNIT    TO      WS-VLR-TOTAL

           IF     PECA-VLR-UNIT    LESS    WS-VLR-MENOR
                  PERFORM          1100-00-TRATA-MENOR
           END-IF.

           IF     PECA-VLR-UNIT    GREATER WS-VLR-MAIOR
                  PERFORM          1200-00-TRATA-MAIOR
           END-IF.

           PERFORM 0500-00-LEITURA-CADPECA.
      *
       1000-99-EXIT.
           EXIT.
      *
      *****************************************************************
       1100-00-TRATA-MENOR         SECTION.
      *****************************************************************
      *
           MOVE    PECA-COD-PEC    TO      WS-COD-MENOR.
           MOVE    PECA-VLR-UNIT   TO      WS-VLR-MENOR.
      *
       1100-99-EXIT.
           EXIT.
      *
      *
      *****************************************************************
       1200-00-TRATA-MAIOR         SECTION.
      *****************************************************************
      *
           MOVE    PECA-COD-PEC    TO      WS-COD-MAIOR.
           MOVE    PECA-VLR-UNIT   TO      WS-VLR-MAIOR.
      *
       1200-99-EXIT.
           EXIT.
      *
      *****************************************************************
       3000-00-PROCED-FINAIS       SECTION.
      *****************************************************************
      *
           IF      WS-LID-CADPECA NOT EQUAL 00
                   PERFORM         1400-00-GRAVACAO-CADPRES
           END-IF.

           CLOSE   CADPECA
                   CADPRES.

           MOVE   'NO FECHAMENTO'  TO      WS-ACESSO-ARQ.

           MOVE    004             TO      WS-PTO-ERRO.

           PERFORM 0200-00-TESTA-FILE-STATUS.

           PERFORM 3100-00-MONTA-ESTATISTICA.
      *
       3000-99-EXIT.
           EXIT.
      *
      *****************************************************************
       1400-00-GRAVACAO-CADPRES    SECTION.
      *****************************************************************
      *
           MOVE    SPACES          TO      REG-PRES.

           MOVE    WS-COD-MENOR  TO      PRES-COD-MENOR.
           MOVE    WS-VLR-MENOR  TO      PRES-VLR-MENOR.
           MOVE    WS-COD-MAIOR  TO      PRES-COD-MAIOR.
           MOVE    WS-VLR-MAIOR  TO      PRES-VLR-MAIOR.

           COMPUTE PRES-VLR-MEDIA =
                   ( WS-VLR-TOTAL / WS-LID-CADPECA ).

           WRITE   REG-CADPRES     FROM    REG-PRES.

           MOVE   ' NA GRAVACAO '  TO      WS-ACESSO-ARQ.

           MOVE    003             TO      WS-PTO-ERRO.

           PERFORM 0400-00-TESTA-FS-CADPRES.

           ADD     001             TO      WS-GRV-CADPRES.
      *
       1400-99-EXIT.
           EXIT.
      *
      *****************************************************************
       3100-00-MONTA-ESTATISTICA   SECTION.
      *****************************************************************
      *
           DISPLAY '******************* GPFPB006 ******************'.
           DISPLAY '*                                             *'.
           DISPLAY '*        ESTATISTICA DE PROCESSAMENTO         *'.
           DISPLAY '*                                             *'.
           DISPLAY '******************* GPFPB006 ******************'.
           DISPLAY '*                                             *'.
           MOVE    WS-LID-CADPECA  TO      WS-EDICAO.
           DISPLAY '* REGISTROS LIDOS.......- CADPECA.: ' WS-EDICAO
           ' *'.
           MOVE    WS-GRV-CADPRES  TO      WS-EDICAO.
           DISPLAY '* REGISTROS GRAVADOS....- CADPRES.: ' WS-EDICAO
           ' *'.
           DISPLAY '*                                             *'.
           DISPLAY '******************* GPFPB006 ******************'.
      *
       3100-99-EXIT.
           EXIT.
      *
      *****************************************************************
       0999-00-ABEND-ARQ           SECTION.
      *****************************************************************
      *
           MOVE    12              TO      RETURN-CODE.

           DISPLAY '******************* GPFPB006 ******************'.
           DISPLAY '*                                             *'.
           DISPLAY '*      TERMINO ANORMAL DE PROCESSAMENTO       *'.
           DISPLAY '*                                             *'.
           DISPLAY '******************* GPFPB006 ******************'.
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
           DISPLAY '******************* GPFPB006 ******************'.
           DISPLAY '*     P R O G R A M A  C A N C E L A D O      *'.
           DISPLAY '******************* GPFPB006 ******************'.

      *    CALL    'IBM'.

           GOBACK.
      *
       0999-00-EXIT.
           EXIT.
      *
      *****************************************************************
      *                   FIM DO PROGRAMA - GPFPB006                  *
      *****************************************************************
