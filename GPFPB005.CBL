      *****************************************************************
       IDENTIFICATION              DIVISION.
      *****************************************************************
      *
       PROGRAM-ID.                 GPFPB005.
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
      *                CADASTRO DE PECAS ATUALIZADO.                  *
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
      * OUTPUT.: CADPATU - CADASTRO DE PECAS ATUALIZADO - LRECL = 100 *
      *****************************************************************
      *
           SELECT  CADPATU  ASSIGN  TO  UT-S-CADPATU
                   FILE     STATUS  IS  WS-FS-CADPATU.
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
      * OUTPUT.: CADPATU - CADASTRO DE PECAS ATUALIZADO - LRECL = 100 *
      *****************************************************************
      *
       FD  CADPATU
           RECORDING  MODE      IS  F
           LABEL      RECORD    IS  STANDARD
           BLOCK      CONTAINS  0   RECORDS.
      *
       01      REG-CADPATU         PIC     X(100).
      *
      *****************************************************************
       WORKING-STORAGE             SECTION.
      *****************************************************************
      *
       01      WS-FS-CADPECA       PIC     9(002) VALUE ZEROS.
       01      WS-FS-CADPATU       PIC     9(002) VALUE ZEROS.
      *
       01      WS-LID-CADPECA      PIC     9(018) VALUE ZEROS.
       01      WS-GRV-CADPATU      PIC     9(018) VALUE ZEROS.
       01      WS-DES-CADPECA      PIC     9(018) VALUE ZEROS.
      *
       01      WS-GRV-CAUM         PIC     9(018) VALUE ZEROS.
       01      WS-GRV-SAUM         PIC     9(018) VALUE ZEROS.
      *
       01      WS-EDICAO           PIC     Z.ZZZ.ZZ9.
      *
       01      WS-VLR-UNIT         PIC     9(013)V9(002) VALUE ZEROS.
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
      * OUTPUT.: CADPATU - CADASTRO DE PECAS ATUALIZADO - LRECL = 100 *
      *****************************************************************
      *
           COPY    COBO1001.
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
                   OUTPUT  CADPATU.

           MOVE   ' NA ABERTURA '  TO      WS-ACESSO-ARQ.

           MOVE    001             TO      WS-PTO-ERRO.

           PERFORM 0200-00-TESTA-FILE-STATUS.

           PERFORM 0500-00-LEITURA-CADPECA.

           IF      WS-FS-CADPECA   EQUAL   10
                   DISPLAY
                   '******************* GPFPB005 ******************'
                   DISPLAY
                   '*                                             *'
                   DISPLAY
                   '*          ARQUIVO CADPECA ESTA VAZIO         *'
                   DISPLAY
                   '*                                             *'
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

           PERFORM 0400-00-TESTA-FS-CADPATU.
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
       0400-00-TESTA-FS-CADPATU    SECTION.
      *****************************************************************
      *
           IF      WS-FS-CADPATU NOT EQUAL 00
                   MOVE 'CADPATU'  TO      WS-DDNAME-ARQ
                   MOVE  WS-FS-CADPATU
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
           IF      PECA-COD-PEC    LESS    8000
                   ADD 001         TO      WS-DES-CADPECA
           ELSE
            IF     PECA-COD-PEC    EQUAL   10000
                   COMPUTE WS-VLR-UNIT =
                       ( PECA-VLR-UNIT * 1,20 )
                   PERFORM         1100-00-GRAVACAO-CADPATU
                   ADD 001         TO      WS-GRV-CAUM
            ELSE
                   MOVE PECA-VLR-UNIT
                                   TO      WS-VLR-UNIT
                   PERFORM         1100-00-GRAVACAO-CADPATU
                   ADD 001         TO      WS-GRV-SAUM
            END-IF
           END-IF.

           PERFORM 0500-00-LEITURA-CADPECA.
      *
       1000-99-EXIT.
           EXIT.
      *
      *****************************************************************
       1100-00-GRAVACAO-CADPATU    SECTION.
      *****************************************************************
      *
           MOVE    SPACES          TO      REG-PATU.

           MOVE    PECA-COD-PEC    TO      PATU-COD-PEC.
           MOVE    PECA-NOME       TO      PATU-NOME.
           MOVE    PECA-QTD-ETQ    TO      PATU-QTD-ETQ.
           MOVE    WS-VLR-UNIT     TO      PATU-VLR-UNIT.

           WRITE   REG-CADPATU     FROM    REG-PATU.

           MOVE   ' NA GRAVACAO '  TO      WS-ACESSO-ARQ.

           MOVE    003             TO      WS-PTO-ERRO.

           PERFORM 0400-00-TESTA-FS-CADPATU.

           ADD     001             TO      WS-GRV-CADPATU.
      *
       1100-99-EXIT.
           EXIT.
      *
      *****************************************************************
       3000-00-PROCED-FINAIS       SECTION.
      *****************************************************************
      *
           CLOSE   CADPECA
                   CADPATU.

           MOVE   'NO FECHAMENTO'  TO      WS-ACESSO-ARQ.

           MOVE    004             TO      WS-PTO-ERRO.

           PERFORM 0200-00-TESTA-FILE-STATUS.

           PERFORM 3100-00-MONTA-ESTATISTICA.
      *
       3000-99-EXIT.
           EXIT.
      *
      *****************************************************************
       3100-00-MONTA-ESTATISTICA   SECTION.
      *****************************************************************
      *
           DISPLAY '******************* GPFPB005 ******************'.
           DISPLAY '*                                             *'.
           DISPLAY '*        ESTATISTICA DE PROCESSAMENTO         *'.
           DISPLAY '*                                             *'.
           DISPLAY '******************* GPFPB005 ******************'.
           DISPLAY '*                                             *'.
           MOVE    WS-LID-CADPECA  TO      WS-EDICAO.
           DISPLAY '* REGISTROS LIDOS.......- CADPECA.: ' WS-EDICAO
           ' *'.
           MOVE    WS-GRV-CAUM     TO      WS-EDICAO.
           DISPLAY '* REGISTROS GRAV. C/AUM.- CADPATU.: ' WS-EDICAO
           ' *'.
           MOVE    WS-GRV-SAUM     TO      WS-EDICAO.
           DISPLAY '* REGISTROS GRAV. S/AUM.- CADPATU.: ' WS-EDICAO
           ' *'.
           MOVE    WS-GRV-CADPATU     TO      WS-EDICAO.
           DISPLAY '* REGISTROS GRAVADOS....- CADPATU.: ' WS-EDICAO
           ' *'.
           MOVE    WS-DES-CADPECA     TO      WS-EDICAO.
           DISPLAY '* REGISTROS DESPREZADOS.- CADPECA.: ' WS-EDICAO
           ' *'.
           DISPLAY '*                                             *'.
           DISPLAY '******************* GPFPB005 ******************'.
      *
       3100-99-EXIT.
           EXIT.
      *
      *****************************************************************
       0999-00-ABEND-ARQ           SECTION.
      *****************************************************************
      *
           MOVE    12              TO      RETURN-CODE.

           DISPLAY '******************* GPFPB005 ******************'.
           DISPLAY '*                                             *'.
           DISPLAY '*      TERMINO ANORMAL DE PROCESSAMENTO       *'.
           DISPLAY '*                                             *'.
           DISPLAY '******************* GPFPB005 ******************'.
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
           DISPLAY '******************* GPFPB005 ******************'.
           DISPLAY '*     P R O G R A M A  C A N C E L A D O      *'.
           DISPLAY '******************* GPFPB005 ******************'.

      *    CALL    'IBM'.

           GOBACK.
      *
       0999-00-EXIT.
           EXIT.
      *
      *****************************************************************
      *                   FIM DO PROGRAMA - GPFPB005                  *
      *****************************************************************
