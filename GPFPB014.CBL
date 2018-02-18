      *****************************************************************
       IDENTIFICATION              DIVISION.
      *****************************************************************
      *
       PROGRAM-ID.                 GPFPB014.
       AUTHOR.                     ULISSES SOUZA
       DATE-WRITTEN.               26/02/2014.
       SECURITY.
      *
      *****************************************************************
      * SISTEMA......: SIGPF - SISTEMA DE GESTAO DE PECAS/FILIAIS     *
      *****************************************************************
      * ANALISTA.....: ULISSES SOUZA                                  *
      * LINGUAGEM....: COBOL/BATCH                                    *
      * PROGRAMADOR..: ULISSES SOUZA                                  *
      * DATA.........: 26/02/2014                                     *
      *****************************************************************
      * OBJETIVO.....: A PARTIR DO CADASTRO DE MOVIMENTO, GERAR O     *
      *  CADASTRO DE MOVIMENTO ATUALIZADO E O CADASTRO DE RESUMO.     *
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
      * INPUT..: CADPMOV - CADASTRO DE FUNCIONARIOS     - LRECL = 200 *
      *****************************************************************
      *
           SELECT  CADPMOV  ASSIGN  TO  UT-S-CADPMOV
                   FILE     STATUS  IS  WS-FS-CADPMOV.
      *
      *****************************************************************
      * OUTPUT.: CADMOVA - CADASTRO DE FUNC SELECIONADOS- LRECL = 200 *
      *****************************************************************
      *
           SELECT  CADMOVA  ASSIGN  TO  UT-S-CADMOVA
                   FILE     STATUS  IS  WS-FS-CADMOVA.
      *
      *****************************************************************
      * OUTPUT.: CADMOVR - CADASTRO DE OCORRENCIAS      - LRECL = 050 *
      *****************************************************************
      *
           SELECT  CADMOVR  ASSIGN  TO  UT-S-CADMOVR
                   FILE     STATUS  IS  WS-FS-CADMOVR.
      *
      *****************************************************************
       DATA                        DIVISION.
      *****************************************************************
       FILE                        SECTION.
      *******************************''''''''**************************
      * OUTPUT.: CADPMOV - CADASTRO DE FUNCIONARIOS     - LRECL = 200 *
      *****************************************************************
      *
       FD  CADPMOV
           RECORDING  MODE      IS  F
           LABEL      RECORD    IS  STANDARD
           BLOCK      CONTAINS  0   RECORDS.
      *
       01      REG-CADPMOV         PIC     X(100).
      *
      *****************************************************************
      * OUTPUT.: CADMOVA - CADASTRO DE FUNC SELECIONADOS- LRECL = 200 *
      *****************************************************************
      *
       FD  CADMOVA
           RECORDING  MODE      IS  F
           LABEL      RECORD    IS  STANDARD
           BLOCK      CONTAINS  0   RECORDS.
      *
       01      REG-CADMOVA         PIC     X(100).
      *
      *****************************************************************
      * OUTPUT.: CADMOVR - CADASTRO DE OCORRENCIAS      - LRECL = 050 *
      *****************************************************************
      *
       FD  CADMOVR
           RECORDING  MODE      IS  F
           LABEL      RECORD    IS  STANDARD
           BLOCK      CONTAINS  0   RECORDS.
      *
       01      REG-CADMOVR         PIC     X(050).
      *
      *****************************************************************
       WORKING-STORAGE             SECTION.
      *****************************************************************
      *
       01      WS-FS-CADPMOV       PIC     9(002) VALUE ZEROS.
       01      WS-FS-CADMOVA       PIC     9(002) VALUE ZEROS.
       01      WS-FS-CADMOVR       PIC     9(002) VALUE ZEROS.
      *
       01      WS-LID-CADPMOV      PIC     9(018) VALUE ZEROS.
       01      WS-GRV-CADMOVA      PIC     9(018) VALUE ZEROS.
       01      WS-GRV-CADMOVR      PIC     9(018) VALUE ZEROS.
       01      WS-DET-CADPMOV      PIC     9(018) VALUE ZEROS.
      *
       01      WS-COBBB006         PIC     X(008) VALUE 'COBBB006'.
      *
       01      WS-ERRO             PIC     9(003) VALUE ZEROS.
      *
       01      WS-EDICAO           PIC     Z.ZZZ.ZZ9.
      *
       01      WS-TOTA-VND         PIC     9(015)V99 VALUE ZEROS.
       01      WS-TOTA-VENDA       PIC     9(015)V99 VALUE ZEROS.
       01      WS-COD-PEC          PIC     9(005) VALUE ZEROS.
      *
      *****************************************************************
      *        TRATAMENTO DE DATA/HORA/TIMESTAMP                      *
      *****************************************************************
      *
       01      WS-DAT-AMD          PIC     9(008) VALUE ZEROS.
       01      FILLER              REDEFINES      WS-DAT-AMD.
         03    WS-ANO-AMD          PIC     9(004).
         03    WS-MES-AMD          PIC     9(002).
         03    WS-DIA-AMD          PIC     9(002).
      *
       01      WS-DAT-DMA          PIC     9(008) VALUE ZEROS.
       01      FILLER              REDEFINES      WS-DAT-DMA.
         03    WS-DIA-DMA          PIC     9(002).
         03    WS-MES-DMA          PIC     9(002).
         03    WS-ANO-DMA          PIC     9(004).
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
       01      WS-SUB-ROTINA        PIC     X(008) VALUE SPACES.
      *
      *****************************************************************
      * INPUT..: COBB006 - CONSISTENCIA DE DATAS          LRECL = 150 *
      *****************************************************************
      *
           COPY    RUCWS006.
      *
      *****************************************************************
      * INPUT..: CADPMOV - CADASTRO DE COTAÇOES           LRECL = 150 *
      *****************************************************************
      *
           COPY    COBI1221.
      *
      *****************************************************************
      * OUTPUT.: CADMOVA - CADASTRO DE COT. SELECIONADAS  LRECL = 200 *
      *****************************************************************
      *
           COPY    COBO1221.
      *
      *****************************************************************
      * OUTPUT.: CADMOVR - CADASTRO DE COT. DESPREZADAS   LRECL = 100 *
      *****************************************************************
      *
           COPY    COBO1222.
      *
      *****************************************************************
       LINKAGE                     SECTION.
      *****************************************************************
      *
       01      LKG-PARM.
         03    LKG-TAM             PIC    S9(004) COMP.
         03    LKG-DATA            PIC     9(008).
         03    LKG-FILLER          REDEFINES LKG-DATA.
           05  LKG-ANO             PIC     9(004).
           05  LKG-MES             PIC     9(002).
           05  LKG-DIA             PIC     9(002).
      *
      *****************************************************************
       PROCEDURE   DIVISION        USING LKG-PARM.
      *****************************************************************
      *
           PERFORM 0100-00-PROCED-INICIAIS.

           PERFORM 1000-00-PROCED-PRINCIPAIS
             UNTIL WS-FS-CADPMOV EQUAL 10 OR
                   PMOV-TIP-REG  EQUAL 'T'.
           PERFORM 3000-00-PROCED-FINAIS.

           GOBACK.
      *
      *****************************************************************
       0100-00-PROCED-INICIAIS     SECTION.
      *****************************************************************
      *

           PERFORM 0150-00-CRITICA-PARM.
           PERFORM 1400-00-CRITICA-DATA.

           OPEN    INPUT   CADPMOV
                   OUTPUT  CADMOVA
                           CADMOVR.

           MOVE   ' NA ABERTURA '  TO      WS-ACESSO-ARQ.

           MOVE    001             TO      WS-PTO-ERRO.

           PERFORM 0200-00-TESTA-FILE-STATUS.

           PERFORM 0500-00-LEITURA-CADPMOV.


      *    TESTANDO SE VAZIO
           IF      WS-FS-CADPMOV   EQUAL   10
                   PERFORM 0996-00-ABEND-MOV-VAZIO
           END-IF.

      *    VERIFICANDO SE O ARQUIVO CONTEM HEADER
           IF      PMOV-TIP-REG    NOT EQUAL 'H'
                   PERFORM 0995-00-ABEND-MOV-S-HEADER
           END-IF.

           PERFORM 0500-00-LEITURA-CADPMOV.

           PERFORM 0200-00-TESTA-FILE-STATUS.

           IF      PMOV-TIP-REG    NOT EQUAL 'D'
                   PERFORM 0990-00-ABEND-ARQ-S-ORDEM
           END-IF.

           MOVE   PMOV-COD-PEC     TO      WS-COD-PEC.
      *
       0100-99-EXIT.
           EXIT.
      *
      *****************************************************************
       0150-00-CRITICA-PARM        SECTION.
      *****************************************************************
      *
           IF      LKG-DATA        NOT NUMERIC OR
                   LKG-DATA        EQUAL       ZEROS
                   PERFORM         0997-00-ABEND-PARM
           END-IF.
      *
       0200-99-EXIT.
           EXIT.
      *
      *****************************************************************
       0200-00-TESTA-FILE-STATUS   SECTION.
      *****************************************************************
      *
           PERFORM 0300-00-TESTA-FS-CADPMOV.

           PERFORM 0400-00-TESTA-FS-CADMOVA.

           PERFORM 0410-00-TESTA-FS-CADMOVR.
      *
       0200-99-EXIT.
           EXIT.
      *
      *****************************************************************
       0300-00-TESTA-FS-CADPMOV    SECTION.
      *****************************************************************
      *
           IF      WS-FS-CADPMOV NOT EQUAL 00 AND 10
                   MOVE 'CADPMOV'  TO      WS-DDNAME-ARQ
                   MOVE  WS-FS-CADPMOV
                                   TO      WS-FS-ARQ
                   PERFORM         0999-00-ABEND-ARQ
           END-IF.
      *
       0300-99-EXIT.
           EXIT.
      *
      *****************************************************************
       0400-00-TESTA-FS-CADMOVA    SECTION.
      *****************************************************************
      *
           IF      WS-FS-CADMOVA NOT EQUAL 00
                   MOVE 'CADMOVA'  TO      WS-DDNAME-ARQ
                   MOVE  WS-FS-CADMOVA
                                   TO      WS-FS-ARQ
                   PERFORM         0999-00-ABEND-ARQ
           END-IF.
      *
       0400-99-EXIT.
           EXIT.
      *
      *****************************************************************
       0410-00-TESTA-FS-CADMOVR    SECTION.
      *****************************************************************
      *
           IF      WS-FS-CADMOVR   NOT EQUAL 00
                   MOVE 'CADMOVR'  TO      WS-DDNAME-ARQ
                   MOVE  WS-FS-CADMOVR
                                   TO      WS-FS-ARQ
                   PERFORM         0999-00-ABEND-ARQ
           END-IF.
      *
       0400-99-EXIT.
           EXIT.
      *
      *****************************************************************
       0500-00-LEITURA-CADPMOV     SECTION.
      *****************************************************************
      *
           READ    CADPMOV         INTO    REG-PMOV.

           MOVE    ' NA LEITURA '  TO      WS-ACESSO-ARQ.

           MOVE    002             TO      WS-PTO-ERRO.

           PERFORM 0300-00-TESTA-FS-CADPMOV.

           IF      WS-FS-CADPMOV   EQUAL   00
                   ADD 001         TO      WS-LID-CADPMOV
           END-IF.
      *
       0500-99-EXIT.
           EXIT.
      *
      *****************************************************************
       1000-00-PROCED-PRINCIPAIS   SECTION.
      *****************************************************************
      *
      *    VERIFICANDO ARQUIVO DETALHE
           IF      PMOV-TIP-REG    NOT EQUAL 'D'
                   PERFORM 0994-00-ABEND-DET-INVALIDO
           END-IF.

           ADD     001             TO      WS-DET-CADPMOV.

           PERFORM 1400-00-GRAVACAO-CADMOVA.

      *    GERANDO QUEBRA QUANDO CODIGO FOR DIFERENTE
           IF      PMOV-COD-PEC    EQUAL WS-COD-PEC
                   ADD PMOV-VLR-VENDA
                                   TO      WS-TOTA-VENDA
           ELSE
                   PERFORM 1500-00-GRAVACAO-CADMOVR
                   MOVE PMOV-VLR-VENDA TO  WS-TOTA-VENDA
           END-IF.

           PERFORM         0500-00-LEITURA-CADPMOV.
      *
       1000-99-EXIT.
           EXIT.
      *
      *****************************************************************
       1400-00-CRITICA-DATA        SECTION.
      *****************************************************************
      *
           MOVE    'COBBB006'      TO      WS-SUB-ROTINA.

           MOVE    'C'             TO      WRD-CODOPE.

           MOVE    LKG-DATA        TO      WRD-DATA01.

           MOVE    00              TO      WRD-CODRET.

           CALL    WS-COBBB006     USING   WRD-GRUPO

           END-CALL.

           IF      WRD-CODRET  NOT EQUAL   00 AND 92 AND 93
                   MOVE    003     TO      WS-PTO-ERRO
                   MOVE 'COBBB006' TO      WS-SUB-ROTINA
                   PERFORM         0998-00-ABEND-SUB
           END-IF.
      *
       1400-99-EXIT.
           EXIT.
      *****************************************************************
       1400-00-GRAVACAO-CADMOVA    SECTION.
      *****************************************************************
      *
           MOVE    SPACES          TO      REG-MOVA.

           MOVE    PMOV-COD-PEC    TO      MOVA-COD-PEC.
           MOVE    PMOV-NOME       TO      MOVA-NOME.
           MOVE    PMOV-FORNECEDOR TO      MOVA-FORNECEDOR.
           MOVE    PMOV-VLR-VENDA  TO      MOVA-VLR-VENDA.
           ADD     PMOV-VLR-VENDA  TO      WS-TOTA-VND.

           WRITE   REG-CADMOVA     FROM    REG-MOVA.

           MOVE   ' NA GRAVACAO '  TO      WS-ACESSO-ARQ.

           MOVE    004             TO      WS-PTO-ERRO.

           PERFORM 0400-00-TESTA-FS-CADMOVA.

           ADD     001             TO      WS-GRV-CADMOVA.
      *
       1400-99-EXIT.
           EXIT.
      *
      *****************************************************************
       1500-00-GRAVACAO-CADMOVR    SECTION.
      *****************************************************************
      *
           MOVE    SPACES          TO      REG-MOVR.

           MOVE    WS-COD-PEC      TO      MOVR-COD-PEC.
           MOVE    WS-TOTA-VENDA   TO      MOVR-VLR-VEN.
           MOVE    LKG-DATA        TO      MOVR-DAT-MOV.

           WRITE   REG-CADMOVR     FROM    REG-MOVR.

           MOVE   ' NA GRAVACAO '  TO      WS-ACESSO-ARQ.

           MOVE    005             TO      WS-PTO-ERRO.

           PERFORM 0410-00-TESTA-FS-CADMOVR.
           ADD     001             TO      WS-GRV-CADMOVR.

      *    ATUALIZANDO O CODIGO DA PECA PARA PROXIMA QUEBRA
           MOVE    PMOV-COD-PEC    TO      WS-COD-PEC.
      *
       1500-99-EXIT.
           EXIT.
      *
      *****************************************************************
       3000-00-PROCED-FINAIS       SECTION.
      *********************************************'********************
      *
           IF      WS-DET-CADPMOV  NOT EQUAL ZEROS
                   PERFORM         1500-00-GRAVACAO-CADMOVR
           END-IF.

      *    VERIFICANDO SE O UTIMO REGISTRO E TRAILLER
           IF      PMOV-TIP-REG    NOT EQUAL   'T'
                   PERFORM         0993-00-ABEND-S-REG-TRAILLER
           END-IF.

      * VERRIFICAR A QTD DE DETALHE SOMADO COM O DO ARQ. TRAILLER
           IF      PMOV-QTD-REG    NOT EQUAL   WS-DET-CADPMOV
                   PERFORM         0992-00-ABEND-QTD-DET-INCOP
           END-IF.

      * VERIFICAR O TOTAL DA VENDA SOMADO COM O DO ARQ. TRAILLER
           IF      PMOV-TOT-VENDA  NOT EQUAL   WS-TOTA-VND
                   PERFORM         0991-00-ABEND-TOTAL-VENDAS
           END-IF.

      * VERIFICANDO SE EXISTE MAIS ALGUM APOS TRAILLER
           PERFORM 0500-00-LEITURA-CADPMOV.

           IF      WS-FS-CADPMOV   NOT EQUAL   10
                   PERFORM         0990-00-ABEND-ARQ-S-ORDEM
           END-IF.

           CLOSE   CADPMOV
                   CADMOVA
                   CADMOVR.

           MOVE   'NO FECHAMENTO'  TO      WS-ACESSO-ARQ.

           MOVE    006             TO      WS-PTO-ERRO.

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
           DISPLAY '******************* GPFPB014 ******************'.
           DISPLAY '*                                             *'.
           DISPLAY '*        ESTATISTICA DE PROCESSAMENTO         *'.
           DISPLAY '*                                             *'.
           DISPLAY '******************* GPFPB014 ******************'.
           DISPLAY '*                                             *'.
           MOVE    WS-LID-CADPMOV  TO      WS-EDICAO.
           DISPLAY '* REGISTROS LIDOS.......- CADPMOV.: ' WS-EDICAO
           ' *'.
           MOVE    WS-GRV-CADMOVA  TO      WS-EDICAO.
           DISPLAY '* REGISTROS GRAVADOS....- CADMOVA.: ' WS-EDICAO
           ' *'.
           MOVE    WS-GRV-CADMOVR  TO      WS-EDICAO.
           DISPLAY '* REGISTROS GRAVADOS....- CADMOVR.: ' WS-EDICAO
           ' *'.
           DISPLAY '*                                             *'.
           DISPLAY '******************* GPFPB014 ******************'.
      *
       3100-99-EXIT.
           EXIT.
      *
      *****************************************************************
       0990-00-ABEND-ARQ-S-ORDEM    SECTION.
      *****************************************************************
      *
           MOVE    12              TO      RETURN-CODE.

           DISPLAY
           '******************* GPFPB014 ******************'
           DISPLAY
           '*                                             *'
           DISPLAY
           '*   ARQUIVO FORA DE ORDEM / DESCLASSIFICADO   *'
           DISPLAY
           '*                                             *'
           DISPLAY
           '******************* GPFPB014 ******************'

      *    CALL    'IBM'.

           GOBACK.
      *
       0991-99-EXIT.
           EXIT.
      *
      *****************************************************************
       0991-00-ABEND-TOTAL-VENDAS   SECTION.
      *****************************************************************
      *
           MOVE    12              TO      RETURN-CODE.

           DISPLAY
           '******************* GPFPB014 ******************'
           DISPLAY
           '*                                             *'
           DISPLAY
           '*  VALOR DA VENDA ACUMULADA NAO COMPATIVEL    *'
           DISPLAY
           '*                                             *'
           MOVE    WS-TOTA-VENDA  TO      WS-EDICAO.
           DISPLAY
           '* VLR. DET. PROGRAMA..:             ' WS-EDICAO      ' *'
           MOVE    PMOV-TOT-VENDA  TO      WS-EDICAO.
           DISPLAY
           '* VLR. DET. TRAILLER..:             'WS-EDICAO       ' *'
           DISPLAY
           '*                                             *'

           PERFORM 3100-00-MONTA-ESTATISTICA.

      *    CALL    'IBM'.

           GOBACK.
      *
       0991-99-EXIT.
           EXIT.
      *
      *****************************************************************
       0992-00-ABEND-QTD-DET-INCOP  SECTION.
      *****************************************************************
      *
           MOVE    12              TO      RETURN-CODE.

           DISPLAY
           '******************* GPFPB014 ******************'
           DISPLAY
           '*                                             *'
           DISPLAY
           '*QUANTIDADE DE REGISTROS DETALHE N COMPATIVEL *'
           DISPLAY
           '*                                             *'
           MOVE    WS-DET-CADPMOV  TO      WS-EDICAO.
           DISPLAY
           '* QTD. DET. PROGRAMA..:            ' WS-EDICAO       '  *'
           MOVE    PMOV-QTD-REG    TO      WS-EDICAO.
           DISPLAY
           '* QTD. DET. TRAILLER..:            ' WS-EDICAO       '  *'
           DISPLAY
           '*                                             *'
           DISPLAY
           '******************* GPFPB014 ******************'

      *    CALL    'IBM'.

           GOBACK.
      *
       0992-99-EXIT.
           EXIT.
      *
      *****************************************************************
       0993-00-ABEND-S-REG-TRAILLER SECTION.
      *****************************************************************
      *
           MOVE    12              TO      RETURN-CODE.

           DISPLAY
           '******************* GPFPB014 ******************'
           DISPLAY
           '*                                             *'
           DISPLAY
           '*    ARQUIVO CADPMOV SEM REGISTRO TRAILLER    *'
           DISPLAY
           '*                                             *'

           PERFORM 3100-00-MONTA-ESTATISTICA.

      *    CALL    'IBM'.

           GOBACK.
      *
       0993-99-EXIT.
           EXIT.
      *
      *****************************************************************
       0994-00-ABEND-DET-INVALIDO  SECTION.
      *****************************************************************
      *
           MOVE    12              TO      RETURN-CODE.

           DISPLAY
           '******************* GPFPB014 ******************'
           DISPLAY
           '*                                             *'
           DISPLAY
           '*  ARQUIVO CADPMOV DETALHE COM TIPO INVALIDO  *'
           DISPLAY
           '*                                             *'

           PERFORM 3100-00-MONTA-ESTATISTICA.

      *    CALL    'IBM'.

           GOBACK.
      *
       0994-99-EXIT.
           EXIT.
      *
      *****************************************************************
       0995-00-ABEND-MOV-S-HEADER  SECTION.
      *****************************************************************
      *
           MOVE    12              TO      RETURN-CODE.

           DISPLAY
           '******************* GPFPB014 ******************'
           DISPLAY
           '*                                             *'
           DISPLAY
           '*     ARQUIVO CADPMOV SEM REGISTRO HEADER     *'
           DISPLAY
           '*                                             *'

           PERFORM 3100-00-MONTA-ESTATISTICA.

      *    CALL    'IBM'.

           GOBACK.
      *
       0995-99-EXIT.
           EXIT.
      *
      *****************************************************************
       0996-00-ABEND-MOV-VAZIO     SECTION.
      *****************************************************************
      *
           MOVE    12              TO      RETURN-CODE.

           DISPLAY
           '******************* GPFPB014 ******************'
           DISPLAY
           '*                                             *'
           DISPLAY
           '*          ARQUIVO CADPMOV ESTA VAZIO         *'
           DISPLAY
           '*                                             *'

           PERFORM 3100-00-MONTA-ESTATISTICA.

      *    CALL    'IBM'.

           GOBACK.
      *
       0996-99-EXIT.
           EXIT.
      *
      *****************************************************************
       0997-00-ABEND-PARM          SECTION.
      *****************************************************************
      *
           MOVE    12              TO      RETURN-CODE.

           DISPLAY
           '******************* GPFPB014 ******************'
           DISPLAY
           '*                                             *'
           DISPLAY
           '*    PARAMETRO PARM ESTA INVALIDO 'LKG-PARM'  *'
           DISPLAY
           '*                                             *'

           PERFORM 3100-00-MONTA-ESTATISTICA.

      *    CALL    'IBM'.

           GOBACK.
      *
       0997-99-EXIT.
           EXIT.
      *
      *****************************************************************
       0998-00-ABEND-SUB           SECTION.
      *****************************************************************
      *
           MOVE    12              TO      RETURN-CODE.

           DISPLAY '******************* GPFPB014 ******************'.
           DISPLAY '*                                             *'.
           DISPLAY '*      TERMINO ANORMAL DE PROCESSAMENTO       *'.
           DISPLAY '*                                             *'.
           DISPLAY '******************* GPFPB014 ******************'.
           DISPLAY '*                                             *'.
           DISPLAY '* PROBLEMAS NO ACESSO A SUBROTINA ' WS-SUB-ROTINA
           '    *'.
           DISPLAY '*                                             *'.
           DISPLAY '*             COD.RETORNO....: ' WRD-CODRET
           '             *'.
           DISPLAY '*                                             *'.
           DISPLAY '*           PONTO COM ERRO...: ' WS-PTO-ERRO
           '            *'.
           DISPLAY '*                                             *'.
           DISPLAY '******************* GPFPB014 ******************'.
           DISPLAY '*     P R O G R A M A  C A N C E L A D O      *'.
           DISPLAY '******************* GPFPB014 ******************'.

      *    CALL    'IBM'.

           GOBACK.
      *
       0998-99-EXIT.
           EXIT.
      *
      *****************************************************************
       0999-00-ABEND-ARQ           SECTION.
      *****************************************************************
      *
           MOVE    12              TO      RETURN-CODE.

           DISPLAY '******************* GPFPB014 ******************'.
           DISPLAY '*                                             *'.
           DISPLAY '*      TERMINO ANORMAL DE PROCESSAMENTO       *'.
           DISPLAY '*                                             *'.
           DISPLAY '******************* GPFPB014 ******************'.
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
           DISPLAY '******************* GPFPB014 ******************'.
           DISPLAY '*     P R O G R A M A  C A N C E L A D O      *'.
           DISPLAY '******************* GPFPB014 ******************'.

      *    CALL    'IBM'.

           GOBACK.
      *
       0999-99-EXIT.
           EXIT.
      *
      *****************************************************************
      *                   FIM DO PROGRAMA - GPFPB014                  *
      *****************************************************************
