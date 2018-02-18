      *****************************************************************
       IDENTIFICATION              DIVISION.
      *****************************************************************
      *
       PROGRAM-ID.                 GPFPB007.
       AUTHOR.                     ULISSES SOUZA
       DATE-WRITTEN.               05/01/2014.
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
      * OBJETIVO.....: A  PARTIR  DO  CADASTRO  DE  COTAÇOES, GERAR O *
      *                CADASTRO DE COTAÇÕES SELECIONDAS E O DE        *
      *                COTAÇÕES DESPREZADAS                           *
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
      * INPUT..: CADCOTA - CADASTRO DE COTAÇOES         - LRECL = 150 *
      *****************************************************************
      *
           SELECT  CADCOTA  ASSIGN  TO  UT-S-CADCOTA
                   FILE     STATUS  IS  WS-FS-CADCOTA.
      *
      *****************************************************************
      * INPUT..: CADCOTS - CADASTRO DE COT. SELECIONADAS- LRECL = 100 *
      *****************************************************************
      *
           SELECT  CADCOTS  ASSIGN  TO  UT-S-CADCOTS
                   FILE     STATUS  IS  WS-FS-CADCOTS.
      *
      *****************************************************************
      * OUTPUT.: CADCOTD - CADASTRO DE COT. DESPREZADAS - LRECL = 100 *
      *****************************************************************
      *
           SELECT  CADCOTD  ASSIGN  TO  UT-S-CADCOTD
                   FILE     STATUS  IS  WS-FS-CADCOTD.
      *
      *****************************************************************
       DATA                        DIVISION.
      *****************************************************************
       FILE                        SECTION.
      *******************************''''''''**************************
      * INPUT..: CADCOTA - CADASTRO DE COTAÇOES         - LRECL = 150 *
      *****************************************************************
      *
       FD  CADCOTA
           RECORDING  MODE      IS  F
           LABEL      RECORD    IS  STANDARD
           BLOCK      CONTAINS  0   RECORDS.
      *
       01      REG-CADCOTA         PIC     X(150).
      *
      *****************************************************************
      * INPUT..: CADCOTS - CADASTRO DE COT. SELECIONADAS- LRECL = 100 *
      *****************************************************************
      *
       FD  CADCOTS
           RECORDING  MODE      IS  F
           LABEL      RECORD    IS  STANDARD
           BLOCK      CONTAINS  0   RECORDS.
      *
       01      REG-CADCOTS         PIC     X(100).
      *
      *****************************************************************
      * OUTPUT.: CADCOTD - CADASTRO DE COT. DESPREZADAS - LRECL = 100 *
      *****************************************************************
      *
       FD  CADCOTD
           RECORDING  MODE      IS  F
           LABEL      RECORD    IS  STANDARD
           BLOCK      CONTAINS  0   RECORDS.
      *
       01      REG-CADCOTD         PIC     X(100).
      *
      *****************************************************************
       WORKING-STORAGE             SECTION.
      *****************************************************************
      *
       01      WS-FS-CADCOTA       PIC     9(002) VALUE ZEROS.
       01      WS-FS-CADCOTS       PIC     9(002) VALUE ZEROS.
       01      WS-FS-CADCOTD       PIC     9(002) VALUE ZEROS.
      *
       01      WS-LID-CADCOTA      PIC     9(018) VALUE ZEROS.
       01      WS-GRV-CADCOTS      PIC     9(018) VALUE ZEROS.
       01      WS-DES-CADCOTD      PIC     9(018) VALUE ZEROS.
      *
       01      WS-EDICAO           PIC     Z.ZZZ.ZZ9.
      *
      ******************************************************************
      *        VARIAVEIS PARA ARMAZENAMENTO DOS VALORES                *
      ******************************************************************
      *
       01      WS-FORNECE          PIC     9(010)    VALUE ZEROS.
       01      WS-VLR-UNI          PIC     9(013)V99 VALUE ZEROS.
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
      * INPUT..: CADCOTA - CADASTRO DE COTAÇOES           LRECL = 150 *
      *****************************************************************
      *
           COPY    COBI1006.
      *
      *****************************************************************
      * OUTPUT.: CADCOTS - CADASTRO DE COT. SELECIONADAS  LRECL = 100 *
      *****************************************************************
      *
           COPY    COBO1006.
      *
      *****************************************************************
      * OUTPUT.: CADCOTD - CADASTRO DE COT. DESPREZADAS   LRECL = 100 *
      *****************************************************************
      *
           COPY    COBO2006.
      *
      *****************************************************************
       LINKAGE                     SECTION.
      *****************************************************************
       PROCEDURE                   DIVISION.
      *****************************************************************
      *
           PERFORM 0100-00-PROCED-INICIAIS.

           PERFORM 1000-00-PROCED-PRINCIPAIS
             UNTIL WS-FS-CADCOTA EQUAL 10.

           PERFORM 3000-00-PROCED-FINAIS.

           GOBACK.
      *
      *****************************************************************
       0100-00-PROCED-INICIAIS     SECTION.
      *****************************************************************
      *
           OPEN    INPUT   CADCOTA
                   OUTPUT  CADCOTS
                           CADCOTD.

           MOVE   ' NA ABERTURA '  TO      WS-ACESSO-ARQ.

           MOVE    001             TO      WS-PTO-ERRO.

           PERFORM 0200-00-TESTA-FILE-STATUS.

           PERFORM 0500-00-LEITURA-CADCOTA.

           IF      WS-FS-CADCOTA   EQUAL   10
                   DISPLAY
                   '******************* GPFPB007 ******************'
                   DISPLAY
                   '*                                             *'
                   DISPLAY
                   '*          ARQUIVO CADCOTA ESTA VAZIO         *'
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
           PERFORM 0300-00-TESTA-FS-CADCOTA.

           PERFORM 0400-00-TESTA-FS-CADCOTS.

           PERFORM 0410-00-TESTA-FS-CADCOTD.
      *
       0200-99-EXIT.
           EXIT.
      *
      *****************************************************************
       0300-00-TESTA-FS-CADCOTA    SECTION.
      *****************************************************************
      *
           IF      WS-FS-CADCOTA NOT EQUAL 00 AND 10
                   MOVE 'CADCOTA'  TO      WS-DDNAME-ARQ
                   MOVE  WS-FS-CADCOTA
                                   TO      WS-FS-ARQ
                   PERFORM         0999-00-ABEND-ARQ
           END-IF.
      *
       0300-99-EXIT.
           EXIT.
      *
      *****************************************************************
       0400-00-TESTA-FS-CADCOTS    SECTION.
      *****************************************************************
      *
           IF      WS-FS-CADCOTS NOT EQUAL 00
                   MOVE 'CADCOTS'  TO      WS-DDNAME-ARQ
                   MOVE  WS-FS-CADCOTS
                                   TO      WS-FS-ARQ
                   PERFORM         0999-00-ABEND-ARQ
           END-IF.
      *
       0400-99-EXIT.
           EXIT.
      *
      *****************************************************************
       0410-00-TESTA-FS-CADCOTD    SECTION.
      *****************************************************************
      *
           IF      WS-FS-CADCOTD NOT EQUAL 00
                   MOVE 'CADCOTD'  TO      WS-DDNAME-ARQ
                   MOVE  WS-FS-CADCOTD
                                   TO      WS-FS-ARQ
                   PERFORM         0999-00-ABEND-ARQ
           END-IF.
      *
       0400-99-EXIT.
           EXIT.
      *
      *****************************************************************
       0500-00-LEITURA-CADCOTA     SECTION.
      *****************************************************************
      *
           READ    CADCOTA         INTO    REG-COTA.

           MOVE   ' NA LEITURA '   TO      WS-ACESSO-ARQ.

           MOVE    002             TO      WS-PTO-ERRO.

           PERFORM 0300-00-TESTA-FS-CADCOTA.

           IF      WS-FS-CADCOTA   EQUAL   00
                   ADD 001         TO      WS-LID-CADCOTA
           END-IF.
      *
       0500-99-EXIT.
           EXIT.
      *
      *****************************************************************
       1000-00-PROCED-PRINCIPAIS   SECTION.
      *****************************************************************
      *
           IF      COTA-VLR-UNI-01 LESS    COTA-VLR-UNI-02 AND
                   COTA-VLR-UNI-01 LESS    COTA-VLR-UNI-03
                   PERFORM         1100-00-TRATA-COTA-01
           ELSE
            IF     COTA-VLR-UNI-02 LESS    COTA-VLR-UNI-03
                   PERFORM         1200-00-TRATA-COTA-02
            ELSE
                   PERFORM         1300-00-TRATA-COTA-03
            END-IF
           END-IF.

           PERFORM 0500-00-LEITURA-CADCOTA.
      *
       1000-99-EXIT.
           EXIT.
      *
      ******************************************************************
       1100-00-TRATA-COTA-01       SECTION.
      ******************************************************************
      *
           MOVE    COTA-FORNECE-01 TO      WS-FORNECE.
           MOVE    COTA-VLR-UNI-01 TO      WS-VLR-UNI.
           PERFORM 1400-00-GRAVACAO-CADCOTS.

           MOVE    COTA-FORNECE-02 TO      WS-FORNECE.
           MOVE    COTA-VLR-UNI-02 TO      WS-VLR-UNI.
           PERFORM 1500-00-GRAVACAO-CADCOTD.

           MOVE    COTA-FORNECE-03 TO      WS-FORNECE.
           MOVE    COTA-VLR-UNI-03 TO      WS-VLR-UNI.
           PERFORM 1500-00-GRAVACAO-CADCOTD.
      *
       1100-99-EXIT.
           EXIT.
      *
      ******************************************************************
       1200-00-TRATA-COTA-02       SECTION.
      ******************************************************************
      *
           MOVE    COTA-FORNECE-02 TO      WS-FORNECE.
           MOVE    COTA-VLR-UNI-02 TO      WS-VLR-UNI.
           PERFORM 1400-00-GRAVACAO-CADCOTS.

           MOVE    COTA-FORNECE-01 TO      WS-FORNECE.
           MOVE    COTA-VLR-UNI-01 TO      WS-VLR-UNI.
           PERFORM 1500-00-GRAVACAO-CADCOTD.

           MOVE    COTA-FORNECE-03 TO      WS-FORNECE.
           MOVE    COTA-VLR-UNI-03 TO      WS-VLR-UNI.
           PERFORM 1500-00-GRAVACAO-CADCOTD.
      *
       1200-99-EXIT.
           EXIT.
      *
      ******************************************************************
       1300-00-TRATA-COTA-03       SECTION.
      ******************************************************************
      *
           MOVE    COTA-FORNECE-03 TO      WS-FORNECE.
           MOVE    COTA-VLR-UNI-03 TO      WS-VLR-UNI.
           PERFORM 1400-00-GRAVACAO-CADCOTS.

           MOVE    COTA-FORNECE-01 TO      WS-FORNECE.
           MOVE    COTA-VLR-UNI-01 TO      WS-VLR-UNI.
           PERFORM 1500-00-GRAVACAO-CADCOTD.

           MOVE    COTA-FORNECE-02 TO      WS-FORNECE.
           MOVE    COTA-VLR-UNI-02 TO      WS-VLR-UNI.
           PERFORM 1500-00-GRAVACAO-CADCOTD.
      *
       1300-99-EXIT.
           EXIT.
      *
      *****************************************************************
       1400-00-GRAVACAO-CADCOTS    SECTION.
      *****************************************************************
      *
           MOVE    SPACES          TO      REG-PSEL.

           MOVE    COTA-COD-PEC    TO      PSEL-COD-PEC.
           MOVE    COTA-NOME       TO      PSEL-NOME.
           MOVE    COTA-QTD-MIN    TO      PSEL-QTD-MIN.
           MOVE    COTA-QTD-MAX    TO      PSEL-QTD-MAX.
           MOVE    COTA-DATA       TO      PSEL-DATA.
           MOVE    WS-FORNECE      TO      PSEL-FORNECE.
           MOVE    WS-VLR-UNI      TO      PSEL-VLR-UNI.

           WRITE   REG-CADCOTS     FROM    REG-PSEL.

           MOVE   ' NA GRAVACAO '  TO      WS-ACESSO-ARQ.

           MOVE    003             TO      WS-PTO-ERRO.

           PERFORM 0400-00-TESTA-FS-CADCOTS.

           ADD     001             TO      WS-GRV-CADCOTS.
      *
       1400-99-EXIT.
           EXIT.
      *
      *****************************************************************
       1500-00-GRAVACAO-CADCOTD    SECTION.
      *****************************************************************
      *
           MOVE    SPACES          TO      REG-DESP.

           MOVE    COTA-COD-PEC    TO      DESP-COD-PEC.
           MOVE    COTA-NOME       TO      DESP-NOME.
           MOVE    COTA-QTD-MIN    TO      DESP-QTD-MIN.
           MOVE    COTA-QTD-MAX    TO      DESP-QTD-MAX.
           MOVE    COTA-DATA       TO      DESP-DATA.
           MOVE    WS-FORNECE      TO      DESP-FORNECE.
           MOVE    WS-VLR-UNI      TO      DESP-VLR-UNI.

           WRITE   REG-CADCOTD     FROM    REG-DESP.

           MOVE   ' NA GRAVACAO '  TO      WS-ACESSO-ARQ.

           MOVE    004             TO      WS-PTO-ERRO.

           PERFORM 0410-00-TESTA-FS-CADCOTD.

           ADD     001             TO      WS-DES-CADCOTD.
      *
       1500-99-EXIT.
           EXIT.
      *
      *****************************************************************
       3000-00-PROCED-FINAIS       SECTION.
      *****************************************************************
      *
           CLOSE   CADCOTA
                   CADCOTS
                   CADCOTD.

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
           DISPLAY '******************* GPFPB007 ******************'.
           DISPLAY '*                                             *'.
           DISPLAY '*        ESTATISTICA DE PROCESSAMENTO         *'.
           DISPLAY '*                                             *'.
           DISPLAY '******************* GPFPB007 ******************'.
           DISPLAY '*                                             *'.
           MOVE    WS-LID-CADCOTA  TO      WS-EDICAO.
           DISPLAY '* REGISTROS LIDOS.......- CADCOTA.: ' WS-EDICAO
           ' *'.
           MOVE    WS-GRV-CADCOTS  TO      WS-EDICAO.
           DISPLAY '* REGISTROS GRAVADOS....- CADCOTS.: ' WS-EDICAO
           ' *'.
           MOVE    WS-DES-CADCOTD  TO      WS-EDICAO.
           DISPLAY '* REGISTROS GRAVADOS....- CADCOTD.: ' WS-EDICAO
           ' *'.
           DISPLAY '*                                             *'.
           DISPLAY '******************* GPFPB007 ******************'.
      *
       3100-99-EXIT.
           EXIT.
      *
      *****************************************************************
       0999-00-ABEND-ARQ           SECTION.
      *****************************************************************
      *
           MOVE    12              TO      RETURN-CODE.

           DISPLAY '******************* GPFPB007 ******************'.
           DISPLAY '*                                             *'.
           DISPLAY '*      TERMINO ANORMAL DE PROCESSAMENTO       *'.
           DISPLAY '*                                             *'.
           DISPLAY '******************* GPFPB007 ******************'.
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
           DISPLAY '******************* GPFPB007 ******************'.
           DISPLAY '*     P R O G R A M A  C A N C E L A D O      *'.
           DISPLAY '******************* GPFPB007 ******************'.

      *    CALL    'IBM'.

           GOBACK.
      *
       0999-00-EXIT.
           EXIT.
      *
      *****************************************************************
      *                   FIM DO PROGRAMA - GPFPB007                  *
      *****************************************************************
