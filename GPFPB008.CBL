      *****************************************************************
       IDENTIFICATION              DIVISION.
      *****************************************************************
      *
       PROGRAM-ID.                 GPFPB008.
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
      * OBJETIVO.....: CRITICAR O CADASTRO DE FUNCIONARIOS, GERANDO O *
      *                CADASTRO DE FUNCIONARIOS SELECIONADOS,         *
      *                DESPREZADOS E OCORRENCIAS.                     *
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
      * INPUT..: CADFUNC - CADASTRO DE FUNCIONARIOS     - LRECL = 200 *
      *****************************************************************
      *
           SELECT  CADFUNC  ASSIGN  TO  UT-S-CADFUNC
                   FILE     STATUS  IS  WS-FS-CADFUNC.
      *
      *****************************************************************
      * OUTPUT.: CADFSEL - CADASTRO DE FUNC SELECIONADOS- LRECL = 200 *
      *****************************************************************
      *
           SELECT  CADFSEL  ASSIGN  TO  UT-S-CADFSEL
                   FILE     STATUS  IS  WS-FS-CADFSEL.
      *
      *****************************************************************
      * OUTPUT.: CADFOCO - CADASTRO DE OCORRENCIAS      - LRECL = 050 *
      *****************************************************************
      *
           SELECT  CADFOCO  ASSIGN  TO  UT-S-CADFOCO
                   FILE     STATUS  IS  WS-FS-CADFOCO.
      *
      *****************************************************************
      * OUTPUT.: CADFDES - CADASTRO DE FUNC DESPREZADOS - LRECL = 200 *
      *****************************************************************
      *
           SELECT  CADFDES  ASSIGN  TO  UT-S-CADFDES
                   FILE     STATUS  IS  WS-FS-CADFDES.
      *
      *****************************************************************
       DATA                        DIVISION.
      *****************************************************************
       FILE                        SECTION.
      *******************************''''''''**************************
      * OUTPUT.: CADFUNC - CADASTRO DE FUNCIONARIOS     - LRECL = 200 *
      *****************************************************************
      *
       FD  CADFUNC
           RECORDING  MODE      IS  F
           LABEL      RECORD    IS  STANDARD
           BLOCK      CONTAINS  0   RECORDS.
      *
       01      REG-CADFUNC         PIC     X(200).
      *
      *****************************************************************
      * OUTPUT.: CADFSEL - CADASTRO DE FUNC SELECIONADOS- LRECL = 200 *
      *****************************************************************
      *
       FD  CADFSEL
           RECORDING  MODE      IS  F
           LABEL      RECORD    IS  STANDARD
           BLOCK      CONTAINS  0   RECORDS.
      *
       01      REG-CADFSEL         PIC     X(200).
      *
      *****************************************************************
      * OUTPUT.: CADFOCO - CADASTRO DE OCORRENCIAS      - LRECL = 050 *
      *****************************************************************
      *
       FD  CADFOCO
           RECORDING  MODE      IS  F
           LABEL      RECORD    IS  STANDARD
           BLOCK      CONTAINS  0   RECORDS.
      *
       01      REG-CADFOCO         PIC     X(050).
      *
      *****************************************************************
      * OUTPUT.: CADFDES - CADASTRO DE FUNC DESPREZADOS - LRECL = 200 *
      *****************************************************************
      *
       FD  CADFDES
           RECORDING  MODE      IS  F
           LABEL      RECORD    IS  STANDARD
           BLOCK      CONTAINS  0   RECORDS.
      *
       01      REG-CADFDES         PIC     X(200).

      *
      *****************************************************************
       WORKING-STORAGE             SECTION.
      *****************************************************************
      *
       01      WS-FS-CADFUNC       PIC     9(002) VALUE ZEROS.
       01      WS-FS-CADFSEL       PIC     9(002) VALUE ZEROS.
       01      WS-FS-CADFOCO       PIC     9(002) VALUE ZEROS.
       01      WS-FS-CADFDES       PIC     9(002) VALUE ZEROS.
      *
       01      WS-LID-CADFUNC      PIC     9(018) VALUE ZEROS.
       01      WS-GRV-CADFSEL      PIC     9(018) VALUE ZEROS.
       01      WS-GRV-CADFOCO      PIC     9(018) VALUE ZEROS.
       01      WS-GRV-CADFDES      PIC     9(018) VALUE ZEROS.
      *
       01      WS-COBBB006         PIC     X(008) VALUE 'COBBB006'.
      *
       01      WS-ERRO             PIC     9(003) VALUE ZEROS.
      *
       01      WS-EDICAO           PIC     Z.ZZZ.ZZ9.
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
      *        TABELA DE CARACTERES VALIDOS                           *
      *****************************************************************
      *
       01      WS-BYTE-NOME        PIC     X(001) VALUE SPACES.
         88    WS-BYTE-NOME-88     VALUE   ' ',
                                           'A',
                                           'B',
                                           'C',
                                           'D',
                                           'E',
                                           'F',
                                           'G',
                                           'H',
                                           'I',
                                           'J',
                                           'K',
                                           'L',
                                           'M',
                                           'N',
                                           'O',
                                           'P',
                                           'Q',
                                           'R',
                                           'S',
                                           'T',
                                           'U',
                                           'V',
                                           'W',
                                           'X',
                                           'Y',
                                           'Z'.
      *
      *****************************************************************
      *        TABELA INTERNA PARA CRITICA LOGICA DO NOME             *
      *****************************************************************
      *
       01      TAB-IND1            PIC     9(003) VALUE ZEROS.
      *
       01      TAB-INTERNA.
         03    TAB-OCCURS          OCCURS  30  TIMES.
           05  TAB-BYTE            PIC     X(001).
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
      * INPUT..: CADFUNC - CADASTRO DE COTAÇOES           LRECL = 150 *
      *****************************************************************
      *
           COPY    COBI1007.
      *
      *****************************************************************
      * OUTPUT.: CADFSEL - CADASTRO DE COT. SELECIONADAS  LRECL = 200 *
      *****************************************************************
      *
           COPY    COBO1007.
      *
      *****************************************************************
      * OUTPUT.: CADFOCO - CADASTRO DE COT. DESPREZADAS   LRECL = 100 *
      *****************************************************************
      *
           COPY    COBO2007.
      *
      *****************************************************************
      * OUTPUT.: CADFOCO - CADASTRO DE COT. DESPREZADAS   LRECL = 100 *
      *****************************************************************
      *
           COPY    COBO3007.
      *
      *****************************************************************
       LINKAGE                     SECTION.
      *****************************************************************
       PROCEDURE                   DIVISION.
      *****************************************************************
      *
           PERFORM 0100-00-PROCED-INICIAIS.

           PERFORM 1000-00-PROCED-PRINCIPAIS
             UNTIL WS-FS-CADFUNC EQUAL 10.

           PERFORM 3000-00-PROCED-FINAIS.

           GOBACK.
      *
      *****************************************************************
       0100-00-PROCED-INICIAIS     SECTION.
      *****************************************************************
      *
           OPEN    INPUT   CADFUNC
                   OUTPUT  CADFSEL
                           CADFOCO
                           CADFDES.

           MOVE   ' NA ABERTURA '  TO      WS-ACESSO-ARQ.

           MOVE    001             TO      WS-PTO-ERRO.

           PERFORM 0200-00-TESTA-FILE-STATUS.

           PERFORM 0500-00-LEITURA-CADFUNC.

           IF      WS-FS-CADFUNC   EQUAL   10
                   DISPLAY
                   '******************* GPFPB008 ******************'
                   DISPLAY
                   '*                                             *'
                   DISPLAY
                   '*          ARQUIVO CADFUNC ESTA VAZIO         *'
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
           PERFORM 0300-00-TESTA-FS-CADFUNC.

           PERFORM 0400-00-TESTA-FS-CADFSEL.

           PERFORM 0410-00-TESTA-FS-CADFOCO.

           PERFORM 0420-00-TESTA-FS-CADFDES.
      *
       0200-99-EXIT.
           EXIT.
      *
      *****************************************************************
       0300-00-TESTA-FS-CADFUNC    SECTION.
      *****************************************************************
      *
           IF      WS-FS-CADFUNC NOT EQUAL 00 AND 10
                   MOVE 'CADFUNC'  TO      WS-DDNAME-ARQ
                   MOVE  WS-FS-CADFUNC
                                   TO      WS-FS-ARQ
                   PERFORM         0999-00-ABEND-ARQ
           END-IF.
      *
       0300-99-EXIT.
           EXIT.
      *
      *****************************************************************
       0400-00-TESTA-FS-CADFSEL    SECTION.
      *****************************************************************
      *
           IF      WS-FS-CADFSEL NOT EQUAL 00
                   MOVE 'CADFSEL'  TO      WS-DDNAME-ARQ
                   MOVE  WS-FS-CADFSEL
                                   TO      WS-FS-ARQ
                   PERFORM         0999-00-ABEND-ARQ
           END-IF.
      *
       0400-99-EXIT.
           EXIT.
      *
      *****************************************************************
       0410-00-TESTA-FS-CADFOCO    SECTION.
      *****************************************************************
      *
           IF      WS-FS-CADFOCO   NOT EQUAL 00
                   MOVE 'CADFOCO'  TO      WS-DDNAME-ARQ
                   MOVE  WS-FS-CADFOCO
                                   TO      WS-FS-ARQ
                   PERFORM         0999-00-ABEND-ARQ
           END-IF.
      *
       0400-99-EXIT.
           EXIT.
      *
      *****************************************************************
       0420-00-TESTA-FS-CADFDES    SECTION.
      *****************************************************************
      *
           IF      WS-FS-CADFDES   NOT EQUAL 00
                   MOVE 'CADFDES'  TO      WS-DDNAME-ARQ
                   MOVE  WS-FS-CADFDES
                                   TO      WS-FS-ARQ
                   PERFORM         0999-00-ABEND-ARQ
           END-IF.
      *
       0420-99-EXIT.
           EXIT.
      *
      *****************************************************************
       0500-00-LEITURA-CADFUNC     SECTION.
      *****************************************************************
      *
           READ    CADFUNC         INTO    REG-FUNC.

           MOVE    ' NA LEITURA '  TO      WS-ACESSO-ARQ.

           MOVE    002             TO      WS-PTO-ERRO.

           PERFORM 0300-00-TESTA-FS-CADFUNC.

           IF      WS-FS-CADFUNC   EQUAL   00
                   ADD 001         TO      WS-LID-CADFUNC
           END-IF.
      *
       0500-99-EXIT.
           EXIT.
      *
      *****************************************************************
       1000-00-PROCED-PRINCIPAIS   SECTION.
      *****************************************************************
      *
           PERFORM 1200-00-CRITICA-DADOS.

           IF      WS-ERRO         EQUAL   ZEROS
                   PERFORM         1400-00-GRAVACAO-CADFSEL
           ELSE
                   PERFORM         1600-00-GRAVACAO-CADFDES
           END-IF.

           PERFORM 0500-00-LEITURA-CADFUNC.
      *
       1000-99-EXIT.
           EXIT.
      *
      ******************************************************************
       1200-00-CRITICA-DADOS       SECTION.
      ******************************************************************
      *
           MOVE    ZEROS           TO      WS-ERRO.

           IF      FUNC-NOME       EQUAL   SPACES
                   MOVE 051        TO      WS-ERRO
                   PERFORM         1500-00-GRAVACAO-CADFOCO
           ELSE
                   PERFORM         1300-00-CRITICA-FIS-NOME
           END-IF.

           IF      FUNC-DT-NAS     NOT     NUMERIC OR
                   FUNC-DT-NAS     EQUAL   ZEROS
                   MOVE 052        TO      WS-ERRO
                   PERFORM         1500-00-GRAVACAO-CADFOCO
                   MOVE ZEROS      TO      FUNC-DT-NAS
           ELSE
                   PERFORM         1400-00-CRITICA-DATA-NAS
           END-IF.

           IF      FUNC-DT-ADM     NOT     NUMERIC OR
                   FUNC-DT-ADM     EQUAL   ZEROS
                   MOVE 053        TO      WS-ERRO
                   PERFORM         1500-00-GRAVACAO-CADFOCO
                   MOVE ZEROS      TO      FUNC-DT-ADM
           ELSE
                   PERFORM         1500-00-CRITICA-DATA-ADM
           END-IF.
      *
       1200-99-EXIT.
           EXIT.
      *
      ******************************************************************
       1300-00-CRITICA-FIS-NOME    SECTION.
      ******************************************************************
      *
           MOVE    FUNC-NOME       TO      TAB-INTERNA.

           PERFORM VARYING TAB-IND1 FROM 01 BY 01
                   UNTIL   TAB-IND1 GREATER 30
                   OR      WS-ERRO  NOT EQUAL ZEROS

             MOVE  TAB-BYTE(TAB-IND1)
                                   TO      WS-BYTE-NOME

             IF    NOT     WS-BYTE-NOME-88
                   MOVE 051        TO      WS-ERRO
             END-IF

           END-PERFORM.

           IF      WS-ERRO     NOT EQUAL ZEROS
                   PERFORM         1500-00-GRAVACAO-CADFOCO
           END-IF.
      *
       1300-99-EXIT.
           EXIT.
      *
      *****************************************************************
       1400-00-CRITICA-DATA-NAS    SECTION.
      *****************************************************************
      *
           MOVE    'COBBB006'      TO      WS-SUB-ROTINA.

           MOVE    'C'             TO      WRD-CODOPE.

           MOVE    FUNC-DT-NAS     TO      WRD-DATA01.

           MOVE    00              TO      WRD-CODRET.

           CALL    WS-COBBB006     USING   WRD-GRUPO

           END-CALL.

           IF      WRD-CODRET  NOT EQUAL   00 AND 92 AND 93
                   MOVE    006     TO      WS-PTO-ERRO
                   MOVE 'COBBB006' TO      WS-SUB-ROTINA
                   PERFORM         0998-00-ABEND-SUB
           END-IF.

           IF      WRD-CODRET      EQUAL   92 OR 93
                   MOVE 052        TO      WS-ERRO
                   PERFORM         1500-00-GRAVACAO-CADFOCO
           END-IF.
      *
       1400-99-EXIT.
           EXIT.
      *
      *****************************************************************
       1500-00-CRITICA-DATA-ADM    SECTION.
      *****************************************************************
      *
           MOVE    'COBBB006'      TO      WS-SUB-ROTINA.

           MOVE    'C'             TO      WRD-CODOPE.

           MOVE    FUNC-DT-ADM     TO      WRD-DATA01.

           MOVE    00              TO      WRD-CODRET.

           CALL    WS-COBBB006     USING   WRD-GRUPO

           END-CALL.

           IF      WRD-CODRET  NOT EQUAL   00 AND 92 AND 93
                   MOVE    007     TO      WS-PTO-ERRO
                   MOVE 'COBBB006' TO      WS-SUB-ROTINA
                   PERFORM         0998-00-ABEND-SUB
           END-IF.

           IF      WRD-CODRET      EQUAL   92 OR 93
                   MOVE 053        TO      WS-ERRO
                   PERFORM         1500-00-GRAVACAO-CADFOCO
           END-IF.
      *
       1500-99-EXIT.
           EXIT.
      *
      *****************************************************************
       1400-00-GRAVACAO-CADFSEL    SECTION.
      *****************************************************************
      *
           MOVE    SPACES          TO      REG-FSEL.

           MOVE    FUNC-CODIGO     TO      FSEL-CODIGO
           MOVE    FUNC-NOME       TO      FSEL-NOME
           MOVE    FUNC-RG         TO      FSEL-RG
           MOVE    FUNC-CPF        TO      FSEL-CPF
           MOVE    FUNC-ENDERECO   TO      FSEL-ENDERECO
           MOVE    FUNC-CART-TRAB  TO      FSEL-CART-TRAB
           MOVE    FUNC-TELEFONE   TO      FSEL-TELEFONE

           MOVE    FUNC-DT-NAS     TO      WS-DAT-AMD.

           MOVE    WS-DIA-AMD      TO      WS-DIA-DMA.
           MOVE    WS-MES-AMD      TO      WS-MES-DMA.
           MOVE    WS-ANO-AMD      TO      WS-ANO-DMA.

           MOVE    WS-DAT-DMA      TO      FSEL-DT-NAS.

           MOVE    FUNC-DT-ADM     TO      WS-DAT-AMD.

           MOVE    WS-DIA-AMD      TO      WS-DIA-DMA.
           MOVE    WS-MES-AMD      TO      WS-MES-DMA.
           MOVE    WS-ANO-AMD      TO      WS-ANO-DMA.

           MOVE    WS-DAT-DMA      TO      FSEL-DT-ADM.

           WRITE   REG-CADFSEL     FROM    REG-FSEL.

           MOVE   ' NA GRAVACAO '  TO      WS-ACESSO-ARQ.

           MOVE    003             TO      WS-PTO-ERRO.

           PERFORM 0400-00-TESTA-FS-CADFSEL.

           ADD     001             TO      WS-GRV-CADFSEL.
      *
       1400-99-EXIT.
           EXIT.
      *
      *****************************************************************
       1500-00-GRAVACAO-CADFOCO    SECTION.
      *****************************************************************
      *
           MOVE    SPACES          TO      REG-FOCO.

           MOVE    FUNC-CODIGO     TO      FOCO-CODIGO.
           MOVE    WS-ERRO         TO      FOCO-ERRO.

           WRITE   REG-CADFOCO     FROM    REG-FOCO.

           MOVE   ' NA GRAVACAO '  TO      WS-ACESSO-ARQ.

           MOVE    004             TO      WS-PTO-ERRO.

           PERFORM 0410-00-TESTA-FS-CADFOCO.

           ADD     001             TO      WS-GRV-CADFOCO.
      *
       1500-99-EXIT.
           EXIT.
      *
      *****************************************************************
       1600-00-GRAVACAO-CADFDES    SECTION.
      *****************************************************************
      *
           MOVE    SPACES          TO      REG-FDES.

           MOVE    FUNC-CODIGO     TO      FDES-CODIGO
           MOVE    FUNC-NOME       TO      FDES-NOME
           MOVE    FUNC-RG         TO      FDES-RG
           MOVE    FUNC-CPF        TO      FDES-CPF
           MOVE    FUNC-ENDERECO   TO      FDES-ENDERECO
           MOVE    FUNC-CART-TRAB  TO      FDES-CART-TRAB
           MOVE    FUNC-TELEFONE   TO      FDES-TELEFONE

           MOVE    FUNC-DT-NAS     TO      WS-DAT-AMD.

           MOVE    WS-DIA-AMD      TO      WS-DIA-DMA.
           MOVE    WS-MES-AMD      TO      WS-MES-DMA.
           MOVE    WS-ANO-AMD      TO      WS-ANO-DMA.

           MOVE    WS-DAT-DMA      TO      FDES-DT-NAS.

           MOVE    FUNC-DT-ADM     TO      WS-DAT-AMD.

           MOVE    WS-DIA-AMD      TO      WS-DIA-DMA.
           MOVE    WS-MES-AMD      TO      WS-MES-DMA.
           MOVE    WS-ANO-AMD      TO      WS-ANO-DMA.

           MOVE    WS-DAT-DMA      TO      FDES-DT-ADM.

           WRITE   REG-CADFDES     FROM    REG-FDES.

           MOVE   ' NA GRAVACAO '  TO      WS-ACESSO-ARQ.

           MOVE    005             TO      WS-PTO-ERRO.

           PERFORM 0420-00-TESTA-FS-CADFDES.

           ADD     001             TO      WS-GRV-CADFDES.
      *
       1600-99-EXIT.
           EXIT.
      *
      *****************************************************************
       3000-00-PROCED-FINAIS       SECTION.
      *****************************************************************
      *
           CLOSE   CADFUNC
                   CADFSEL
                   CADFDES
                   CADFOCO.

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
           DISPLAY '******************* GPFPB008 ******************'.
           DISPLAY '*                                             *'.
           DISPLAY '*        ESTATISTICA DE PROCESSAMENTO         *'.
           DISPLAY '*                                             *'.
           DISPLAY '******************* GPFPB008 ******************'.
           DISPLAY '*                                             *'.
           MOVE    WS-LID-CADFUNC  TO      WS-EDICAO.
           DISPLAY '* REGISTROS LIDOS.......- CADFUNC.: ' WS-EDICAO
           ' *'.
           MOVE    WS-GRV-CADFSEL  TO      WS-EDICAO.
           DISPLAY '* REGISTROS GRAVADOS....- CADFSEL.: ' WS-EDICAO
           ' *'.
           MOVE    WS-GRV-CADFDES  TO      WS-EDICAO.
           DISPLAY '* REGISTROS GRAVADOS....- CADFDES.: ' WS-EDICAO
           ' *'.
           MOVE    WS-GRV-CADFOCO  TO      WS-EDICAO.
           DISPLAY '* REGISTROS GRAVADOS....- CADFOCO.: ' WS-EDICAO
           ' *'.
           DISPLAY '*                                             *'.
           DISPLAY '******************* GPFPB008 ******************'.
      *
       3100-99-EXIT.
           EXIT.
      *
      *****************************************************************
       0998-00-ABEND-SUB           SECTION.
      *****************************************************************
      *
           MOVE    12              TO      RETURN-CODE.

           DISPLAY '******************* GPFPB008 ******************'.
           DISPLAY '*                                             *'.
           DISPLAY '*      TERMINO ANORMAL DE PROCESSAMENTO       *'.
           DISPLAY '*                                             *'.
           DISPLAY '******************* GPFPB008 ******************'.
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
           DISPLAY '******************* GPFPB008 ******************'.
           DISPLAY '*     P R O G R A M A  C A N C E L A D O      *'.
           DISPLAY '******************* GPFPB008 ******************'.

      *    CALL    'IBM'.

           GOBACK.
      *
       0998-00-EXIT.
           EXIT.
      *
      *****************************************************************
       0999-00-ABEND-ARQ           SECTION.
      *****************************************************************
      *
           MOVE    12              TO      RETURN-CODE.

           DISPLAY '******************* GPFPB008 ******************'.
           DISPLAY '*                                             *'.
           DISPLAY '*      TERMINO ANORMAL DE PROCESSAMENTO       *'.
           DISPLAY '*                                             *'.
           DISPLAY '******************* GPFPB008 ******************'.
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
           DISPLAY '******************* GPFPB008 ******************'.
           DISPLAY '*     P R O G R A M A  C A N C E L A D O      *'.
           DISPLAY '******************* GPFPB008 ******************'.

      *    CALL    'IBM'.

           GOBACK.
      *
       0999-00-EXIT.
           EXIT.
      *
      *****************************************************************
      *                   FIM DO PROGRAMA - GPFPB008                  *
      *****************************************************************
