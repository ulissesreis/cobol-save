      *****************************************************************
       IDENTIFICATION              DIVISION.
      *****************************************************************
      *
       PROGRAM-ID.                 GPFPB011.
       AUTHOR.                     ULISSES SOUZA
       DATE-WRITTEN.               14/02/2014.
       SECURITY.
      *
      *****************************************************************
      * SISTEMA......: SIGPF - SISTEMA DE GESTAO DE PECAS/FILIAIS     *
      *****************************************************************
      * ANALISTA.....: ULISSES SOUZA                                  *
      * LINGUAGEM....: COBOL/BATCH                                    *
      * PROGRAMADOR..: ULISSES SOUZA                                  *
      * DATA.........: 14/02/2014                                     *
      *****************************************************************
      * OBJETIVO.....: REALIZAR BALANCE-LINE (MERGE) GERANDO O        *
      *                CADASTRO DE PECAS DA MATRIZ.                   *
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
      * INPUT..: CADFIL1 - CADASTRO DA FILIAL 1         - LRECL = 100 *
      *****************************************************************
      *
           SELECT  CADFIL1  ASSIGN  TO  UT-S-CADFIL1
                   FILE     STATUS  IS  WS-FS-CADFIL1.
      *
      *****************************************************************
      * INPUT..: CADFIL2 - CADASTRO DA FILIAL 2         - LRECL = 100 *
      *****************************************************************
      *
           SELECT  CADFIL2  ASSIGN  TO  UT-S-CADFIL2
                   FILE     STATUS  IS  WS-FS-CADFIL2.
      *
      *****************************************************************
      * OUTPUT.: CADMATR - CADASTRO MATRIZ              - LRECL = 100 *
      *****************************************************************
      *
           SELECT  CADMATR  ASSIGN  TO  UT-S-CADMATR
                   FILE     STATUS  IS  WS-FS-CADMATR.
      *
      *****************************************************************
       DATA                        DIVISION.
      *****************************************************************
       FILE                        SECTION.
      *****************************************************************
      * INPUT..: CADFIL1 - CADASTRO DA FILIAL 1         - LRECL = 100 *
      *****************************************************************
      *
       FD  CADFIL1
           RECORDING  MODE      IS  F
           LABEL      RECORD    IS  STANDARD
           BLOCK      CONTAINS  0   RECORDS.
      *
       01      REG-CADFIL1         PIC     X(100).
      *
      *****************************************************************
      * INPUT..: CADFIL1 - CADASTRO DA FILIAL 2         - LRECL = 100 *
      *****************************************************************
      *
       FD  CADFIL2
           RECORDING  MODE      IS  F
           LABEL      RECORD    IS  STANDARD
           BLOCK      CONTAINS  0   RECORDS.
      *
       01      REG-CADFIL2         PIC     X(100).
      *
      *****************************************************************
      * OUTPUT.: CADMATR - CADASTRO MATRIZ              - LRECL = 100 *
      *****************************************************************
      *
       FD  CADMATR
           RECORDING  MODE      IS  F
           LABEL      RECORD    IS  STANDARD
           BLOCK      CONTAINS  0   RECORDS.
      *
       01      REG-CADMATR         PIC     X(100).
      *
      *****************************************************************
       WORKING-STORAGE             SECTION.
      *****************************************************************
      *
       01      WS-FS-CADFIL1       PIC     9(002) VALUE ZEROS.
       01      WS-FS-CADFIL2       PIC     9(002) VALUE ZEROS.
       01      WS-FS-CADMATR       PIC     9(002) VALUE ZEROS.

      *
       01      WS-LID-CADFIL1      PIC     9(018) VALUE ZEROS.
       01      WS-LID-CADFIL2      PIC     9(018) VALUE ZEROS.
       01      WS-GRV-CADMATR      PIC     9(018) VALUE ZEROS.
      *
       01      WS-EDICAO           PIC     Z.ZZZ.ZZ9.
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
      * INPUT..: CADFIL1 - CADASTRO DA FILIAL 1         - LRECL = 100 *
      *****************************************************************
      *
           COPY    COBI1010.
      *
      *****************************************************************
      * INPUT..: CADFIL2 - CADASTRO DA FILIAL 2         - LRECL = 100 *
      *****************************************************************
      *
           COPY    COBI2010.
      *
      *****************************************************************
      * OUTPUT.: CADMATR - CADASTRO MATRIZ              - LRECL = 100 *
      *****************************************************************
      *
           COPY    COBO1010.
      *
      *****************************************************************
       LINKAGE                     SECTION.
      *****************************************************************
       PROCEDURE                   DIVISION.
      *****************************************************************
      *
           PERFORM 0100-00-PROCED-INICIAIS.

           PERFORM 1000-00-PROCED-PRINCIPAIS
             UNTIL WS-FS-CADFIL1 EQUAL 10 AND
                   WS-FS-CADFIL2 EQUAL 10
           PERFORM 3000-00-PROCED-FINAIS.

           GOBACK.
      *
      *****************************************************************
       0100-00-PROCED-INICIAIS     SECTION.
      *****************************************************************
      *
           OPEN    INPUT   CADFIL1
                           CADFIL2
                   OUTPUT  CADMATR.

           MOVE   ' NA ABERTURA '  TO      WS-ACESSO-ARQ.

           MOVE    001             TO      WS-PTO-ERRO.

           PERFORM 0200-00-TESTA-FILE-STATUS.

           PERFORM 0500-00-LEITURA-CADFIL1.
           PERFORM 0600-00-LEITURA-CADFIL2.

           IF      WS-FS-CADFIL1 EQUAL   10  AND
                   WS-FS-CADFIL2 EQUAL   10
                   DISPLAY
                   '******************* GPFPB011 ******************'
                   DISPLAY
                   '*                                             *'
                   DISPLAY
                   '*       NAO EXISTE MOVIMENTO DE FILIAIS       *'
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
           PERFORM 0300-00-TESTA-FS-CADFIL1.

           PERFORM 0350-00-TESTA-FS-CADFIL2.

           PERFORM 0400-00-TESTA-FS-CADMATR.
      *
       0200-99-EXIT.
           EXIT.
      *
      *****************************************************************
       0300-00-TESTA-FS-CADFIL1    SECTION.
      *****************************************************************
      *
           IF      WS-FS-CADFIL1 NOT EQUAL 00 AND 10
                   MOVE 'CADFIL1'  TO      WS-DDNAME-ARQ
                   MOVE  WS-FS-CADFIL1
                                   TO      WS-FS-ARQ
                   PERFORM         0999-00-ABEND-ARQ
           END-IF.
      *
       0300-99-EXIT.
           EXIT.
      *
      *****************************************************************
       0350-00-TESTA-FS-CADFIL2    SECTION.
      *****************************************************************
      *
           IF      WS-FS-CADFIL2 NOT EQUAL 00 AND 10
                   MOVE 'CADFIL2'  TO      WS-DDNAME-ARQ
                   MOVE  WS-FS-CADFIL2
                                   TO      WS-FS-ARQ
                   PERFORM         0999-00-ABEND-ARQ
           END-IF.
      *
       0350-99-EXIT.
           EXIT.
      *
      *****************************************************************
       0400-00-TESTA-FS-CADMATR    SECTION.
      *****************************************************************
      *
           IF      WS-FS-CADMATR NOT EQUAL 00
                   MOVE 'CADMATR'  TO      WS-DDNAME-ARQ
                   MOVE  WS-FS-CADMATR
                                   TO      WS-FS-ARQ
                   PERFORM         0999-00-ABEND-ARQ
           END-IF.
      *
       0400-99-EXIT.
           EXIT.
      *
      *****************************************************************
       0500-00-LEITURA-CADFIL1     SECTION.
      *****************************************************************
      *
           READ    CADFIL1         INTO    REG-FIL1.

           MOVE   ' NA LEITURA '   TO      WS-ACESSO-ARQ.

           MOVE    002             TO      WS-PTO-ERRO.

           PERFORM 0300-00-TESTA-FS-CADFIL1.

           IF      WS-FS-CADFIL1   EQUAL   00
                   ADD 001         TO      WS-LID-CADFIL1
           END-IF.

           IF      WS-FS-CADFIL1   EQUAL   10
                   MOVE HIGH-VALUES TO     REG-FIL1
           END-IF.
      *
       0500-99-EXIT.
           EXIT.
      *
      *****************************************************************
       0600-00-LEITURA-CADFIL2     SECTION.
      *****************************************************************
      *
           READ    CADFIL2         INTO    REG-FIL2.

           MOVE   ' NA LEITURA '   TO      WS-ACESSO-ARQ.

           MOVE    002             TO      WS-PTO-ERRO.

           PERFORM 0350-00-TESTA-FS-CADFIL2.

           IF      WS-FS-CADFIL2   EQUAL   00
                   ADD 001         TO      WS-LID-CADFIL2
           END-IF.

           IF      WS-FS-CADFIL2   EQUAL   10
                   MOVE HIGH-VALUES TO     REG-FIL2
           END-IF.
      *
       0500-99-EXIT.
           EXIT.
      *
      *****************************************************************
       1000-00-PROCED-PRINCIPAIS   SECTION.
      *****************************************************************
      *
            IF     FIL1-COD-PEC    LESS    FIL2-COD-PEC
                   PERFORM 1100-00-GRAVACAO-CADMATR-FIL1
                   PERFORM 0500-00-LEITURA-CADFIL1
            ELSE
             IF     FIL2-COD-PEC    LESS    FIL1-COD-PEC
                   PERFORM 1200-00-GRAVACAO-CADMATR-FIL2
                   PERFORM 0600-00-LEITURA-CADFIL2
             ELSE
                   PERFORM 1100-00-GRAVACAO-CADMATR-FIL1
                   PERFORM 1200-00-GRAVACAO-CADMATR-FIL2
                   PERFORM 0500-00-LEITURA-CADFIL1
                   PERFORM 0600-00-LEITURA-CADFIL2
            END-IF.
      *
      *
       1000-99-EXIT.
           EXIT.
      *
      *****************************************************************
       1100-00-GRAVACAO-CADMATR-FIL1 SECTION.
      *****************************************************************
      *
           MOVE    SPACES          TO      REG-MATR.
      *
           MOVE    FIL1-COD-PEC    TO      MATR-COD-PEC.
           MOVE    FIL1-NOME       TO      MATR-NOME.
           MOVE    FIL1-QTD-ETQ    TO      MATR-QTD-ETQ.
           MOVE    FIL1-QTD-MIN    TO      MATR-QTD-MIN.
           MOVE    FIL1-QTD-MAX    TO      MATR-QTD-MAX.
           MOVE    FIL1-FORNECEDOR TO      MATR-FORNECEDOR.
           MOVE    FIL1-VLR-UNIT   TO      MATR-VLR-UNIT.

           WRITE   REG-CADMATR     FROM    REG-MATR.

           MOVE   ' NA GRAVACAO '  TO      WS-ACESSO-ARQ.

           MOVE    003             TO      WS-PTO-ERRO.

           PERFORM 0400-00-TESTA-FS-CADMATR.

           ADD     001             TO      WS-GRV-CADMATR.
      *
       1100-99-EXIT.
           EXIT.
      *
      *****************************************************************
       1200-00-GRAVACAO-CADMATR-FIL2 SECTION.
      *****************************************************************
      *
           MOVE    SPACES          TO      REG-MATR.
      *
           MOVE    FIL2-COD-PEC    TO      MATR-COD-PEC.
           MOVE    FIL2-NOME       TO      MATR-NOME.
           MOVE    FIL2-QTD-ETQ    TO      MATR-QTD-ETQ.
           MOVE    FIL2-QTD-MIN    TO      MATR-QTD-MIN.
           MOVE    FIL2-QTD-MAX    TO      MATR-QTD-MAX.
           MOVE    FIL2-FORNECEDOR TO      MATR-FORNECEDOR.
           MOVE    FIL2-VLR-UNIT   TO      MATR-VLR-UNIT.

           WRITE   REG-CADMATR     FROM    REG-MATR.

           MOVE   ' NA GRAVACAO '  TO      WS-ACESSO-ARQ.

           MOVE    004             TO      WS-PTO-ERRO.

           PERFORM 0400-00-TESTA-FS-CADMATR.

           ADD     001             TO      WS-GRV-CADMATR.
      *
       1100-99-EXIT.
           EXIT.
      *
      *****************************************************************
       3000-00-PROCED-FINAIS       SECTION.
      *****************************************************************
      *
           CLOSE   CADFIL1
                   CADFIL2
                   CADMATR.

           MOVE   'NO FECHAMENTO'  TO      WS-ACESSO-ARQ.

           MOVE    005             TO      WS-PTO-ERRO.

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
           DISPLAY '******************* GPFPB011 ******************'.
           DISPLAY '*                                             *'.
           DISPLAY '*        ESTATISTICA DE PROCESSAMENTO         *'.
           DISPLAY '*                                             *'.
           DISPLAY '******************* GPFPB011 ******************'.
           DISPLAY '*                                             *'.
           MOVE    WS-LID-CADFIL1  TO      WS-EDICAO.
           DISPLAY '* REGISTROS LIDOS.......- CADFIL1.: ' WS-EDICAO
           ' *'.
           MOVE    WS-LID-CADFIL2  TO      WS-EDICAO.
           DISPLAY '* REGISTROS LIDOS.......- CADFIL2.: ' WS-EDICAO
           ' *'.
           MOVE    WS-GRV-CADMATR  TO      WS-EDICAO.
           DISPLAY '* REGISTROS GRAVADOS....- CADMATR.: ' WS-EDICAO
           ' *'.
           DISPLAY '*                                             *'.
           DISPLAY '******************* GPFPB011 ******************'.
      *
       3100-99-EXIT.
           EXIT.
      *
      *****************************************************************
       0999-00-ABEND-ARQ           SECTION.
      *****************************************************************
      *
           MOVE    12              TO      RETURN-CODE.

           DISPLAY '******************* GPFPB011 ******************'.
           DISPLAY '*                                             *'.
           DISPLAY '*      TERMINO ANORMAL DE PROCESSAMENTO       *'.
           DISPLAY '*                                             *'.
           DISPLAY '******************* GPFPB011 ******************'.
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
           DISPLAY '******************* GPFPB011 ******************'.
           DISPLAY '*     P R O G R A M A  C A N C E L A D O      *'.
           DISPLAY '******************* GPFPB011 ******************'.

      *    CALL    'IBM'.

           GOBACK.
      *
       0999-00-EXIT.
           EXIT.
      *
      *****************************************************************
      *                   FIM DO PROGRAMA - GPFPB011                  *
      *****************************************************************
