      *****************************************************************
       IDENTIFICATION              DIVISION.
      *****************************************************************
      *
       PROGRAM-ID.                 GPFPB013.
       AUTHOR.                     ULISSES SOUZA
       DATE-WRITTEN.               19/02/2014.
       SECURITY.
      *
      *****************************************************************
      * SISTEMA......: SIGPF - SISTEMA DE GESTAO DE PECAS/FILIAIS     *
      *****************************************************************
      * ANALISTA.....: ULISSES SOUZA                                  *
      * LINGUAGEM....: COBOL/BATCH                                    *
      * PROGRAMADOR..: ULISSES SOUZA                                  *
      * DATA.........: 19/02/2014                                     *
      *****************************************************************
      * OBJETIVO.....: REALIZAR BALANCE-LINE (MERGE) GERANDO   UM     *
      *                CADASTRO ATUALIZADO                            *
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
      * INPUT..: CADFUNC - CADASTRO DE FUNCIONARIOS     - LRECL = 200 *
      *****************************************************************
      *
           SELECT  CADFUNC  ASSIGN  TO  UT-S-CADFUNC
                   FILE     STATUS  IS  WS-FS-CADFUNC.
      *
      *****************************************************************
      * INPUT..: MOVFUNC - MOVIMENTO DE FUNCIONARIOS    - LRECL = 200 *
      *****************************************************************
      *
           SELECT  MOVFUNC  ASSIGN  TO  UT-S-MOVFUNC
                   FILE     STATUS  IS  WS-FS-MOVFUNC.
      *
      *****************************************************************
      * OUTPUT.: CADFATU - CADASTRO DE FUNC. ATUALIZADO - LRECL = 200 *
      *****************************************************************
      *
           SELECT  CADFATU  ASSIGN  TO  UT-S-CADFATU
                   FILE     STATUS  IS  WS-FS-CADFATU.
      *
      *****************************************************************
      * OUTPUT.: CADFLOG - LOG DO CADAST DE FUNCIONARIO - LRECL = 250 *
      *****************************************************************
      *
           SELECT  CADFLOG  ASSIGN  TO  UT-S-CADFLOG
                   FILE     STATUS  IS  WS-FS-CADFLOG.
      *
      *****************************************************************
       DATA                        DIVISION.
      *****************************************************************
       FILE                        SECTION.
      *****************************************************************
      * INPUT..: CADFUNC - CADASTRO DE FUNCIONARIOS     - LRECL = 200 *
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
      * INPUT..: MOVFUNC - MOVIMENTO DE FUNCIONARIOS    - LRECL = 200 *
      *****************************************************************
      *
       FD  MOVFUNC
           RECORDING  MODE      IS  F
           LABEL      RECORD    IS  STANDARD
           BLOCK      CONTAINS  0   RECORDS.
      *
       01      REG-MOVFUNC         PIC     X(200).
      *
      *****************************************************************
      * OUTPUT.: CADFATU - CADASTRO DE FUNC. ATUALIZADO - LRECL = 200 *
      *****************************************************************
      *
       FD  CADFATU
           RECORDING  MODE      IS  F
           LABEL      RECORD    IS  STANDARD
           BLOCK      CONTAINS  0   RECORDS.
      *
       01      REG-CADFATU         PIC     X(200).
      *
      *****************************************************************
      * OUTPUT.: CADFLOG - LOG DO CADAST DE FUNCIONARIO - LRECL = 250 *
      *****************************************************************
      *
       FD  CADFLOG
           RECORDING  MODE      IS  F
           LABEL      RECORD    IS  STANDARD
           BLOCK      CONTAINS  0   RECORDS.
      *
       01      REG-CADFLOG         PIC     X(250).
      *
      *****************************************************************
       WORKING-STORAGE             SECTION.
      *****************************************************************
      *
       01      WS-FS-CADFUNC       PIC     9(002) VALUE ZEROS.
       01      WS-FS-MOVFUNC       PIC     9(002) VALUE ZEROS.
       01      WS-FS-CADFATU       PIC     9(002) VALUE ZEROS.
       01      WS-FS-CADFLOG       PIC     9(002) VALUE ZEROS.

      *
       01      WS-LID-CADFUNC      PIC     9(018) VALUE ZEROS.
       01      WS-LID-MOVFUNC      PIC     9(018) VALUE ZEROS.
       01      WS-GRV-CADFATU      PIC     9(018) VALUE ZEROS.
       01      WS-GRV-CADFLOG      PIC     9(018) VALUE ZEROS.
       01      WS-EXC-CADFUNC      PIC     9(018) VALUE ZEROS.
       01      WS-ALT-CADFATU      PIC     9(018) VALUE ZEROS.
       01      WS-INC-CADFATU      PIC     9(018) VALUE ZEROS.
      *
       01      WS-EDICAO           PIC     Z.ZZZ.ZZ9.
      *
       01      WS-FLOG-STATUS      PIC     X(003) VALUE SPACES.
       01      WS-FLOG-ORIGEM      PIC     X(008) VALUE SPACES.
      *
      *****************************************************************
      *        VARIAVEIS PARA TRATAMENTO DE DATA NO CADFLOG           *
      *****************************************************************
      *

       01      WS-TIME             PIC     X(008)  VALUE ZEROS.
       01      FILLER              REDEFINES       WS-TIME.
        03     WS-TIME-ANO         PIC     9(004).
        03     WS-TIME-MES         PIC     9(002).
        03     WS-TIME-DIA         PIC     9(002).

      *
       01      WS-DATA             PIC     X(010)  VALUE '99/99/9999'.
       01      FILLER              REDEFINES       WS-DATA.
        03     WS-DIA              PIC     9(002).
        03     FILLER              PIC     X(001).
        03     WS-MES              PIC     9(002).
        03     FILLER              PIC     X(001).
        03     WS-ANO              PIC     9(004).
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
      * INPUT..: CADFUNC - CADASTRO DE FUNCIONARIOS     - LRECL = 200 *
      *****************************************************************
      *
           COPY    COBI1012.
      *
      *****************************************************************
      * INPUT..: MOVFUNC - MOVIMENTO DE FUNCIONARIOS    - LRECL = 200 *
      *****************************************************************
      *
           COPY    COBI2012.
      *
      *****************************************************************
      * OUTPUT.: CADFATU - CADASTRO DE FUNC. ATUALIZADO - LRECL = 200 *
      *****************************************************************
      *
           COPY    COBO1012.
      *
      *****************************************************************
      * OUTPUT.: CADFLOG - LOG DO CADAST DE FUNCIONARIO - LRECL = 250 *
      *****************************************************************
      *
           COPY    COBO2012.
      *
      *****************************************************************
       LINKAGE                     SECTION.
      *****************************************************************
       PROCEDURE                   DIVISION.
      *****************************************************************
      *
           PERFORM 0100-00-PROCED-INICIAIS.

           PERFORM 1000-00-PROCED-PRINCIPAIS
             UNTIL WS-FS-CADFUNC EQUAL 10 AND
                   WS-FS-MOVFUNC EQUAL 10
           PERFORM 3000-00-PROCED-FINAIS.

           GOBACK.
      *
      *****************************************************************
       0100-00-PROCED-INICIAIS     SECTION.
      *****************************************************************
      *
           OPEN    INPUT   CADFUNC
                           MOVFUNC
                   OUTPUT  CADFATU
                           CADFLOG.

           MOVE   ' NA ABERTURA '  TO      WS-ACESSO-ARQ.

           MOVE    001             TO      WS-PTO-ERRO.

           PERFORM 0200-00-TESTA-FILE-STATUS.

           PERFORM 0500-00-LEITURA-CADFUNC.
           PERFORM 0600-00-LEITURA-MOVFUNC.

           IF      WS-FS-CADFUNC EQUAL   10  AND
                   WS-FS-MOVFUNC EQUAL   10
                   DISPLAY
                   '******************* GPFPB013 ******************'
                   DISPLAY
                   '*                                             *'
                   DISPLAY
                   '*     NAO EXISTE MOVIMENTO DE FUNCIONARIO     *'
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

           PERFORM 0350-00-TESTA-FS-MOVFUNC.

           PERFORM 0400-00-TESTA-FS-CADFATU.

           PERFORM 0450-00-TESTA-FS-CADFLOG.
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
       0350-00-TESTA-FS-MOVFUNC    SECTION.
      *****************************************************************
      *
           IF      WS-FS-MOVFUNC NOT EQUAL 00 AND 10
                   MOVE 'MOVFUNC'  TO      WS-DDNAME-ARQ
                   MOVE  WS-FS-MOVFUNC
                                   TO      WS-FS-ARQ
                   PERFORM         0999-00-ABEND-ARQ
           END-IF.
      *
       0350-99-EXIT.
           EXIT.
      *
      *****************************************************************
       0400-00-TESTA-FS-CADFATU    SECTION.
      *****************************************************************
      *
           IF      WS-FS-CADFATU NOT EQUAL 00
                   MOVE 'CADFATU'  TO      WS-DDNAME-ARQ
                   MOVE  WS-FS-CADFATU
                                   TO      WS-FS-ARQ
                   PERFORM         0999-00-ABEND-ARQ
           END-IF.
      *
       0400-99-EXIT.
           EXIT.
      *
      *****************************************************************
       0450-00-TESTA-FS-CADFLOG    SECTION.
      *****************************************************************
      *
           IF      WS-FS-CADFLOG NOT EQUAL 00
                   MOVE 'CADFLOG'  TO      WS-DDNAME-ARQ
                   MOVE  WS-FS-CADFLOG
                                   TO      WS-FS-ARQ
                   PERFORM         0999-00-ABEND-ARQ
           END-IF.
      *
       0450-99-EXIT.
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

           IF      WS-FS-CADFUNC   EQUAL   10
                   MOVE HIGH-VALUES TO     REG-FUNC
           END-IF.
      *
       0500-99-EXIT.
           EXIT.
      *
      *****************************************************************
       0600-00-LEITURA-MOVFUNC     SECTION.
      *****************************************************************
      *
           READ    MOVFUNC         INTO    REG-MOVF.

           MOVE   ' NA LEITURA '   TO      WS-ACESSO-ARQ.

           MOVE    003             TO      WS-PTO-ERRO.

           PERFORM 0350-00-TESTA-FS-MOVFUNC.

           IF      WS-FS-MOVFUNC   EQUAL   00
                   ADD 001         TO      WS-LID-MOVFUNC
           END-IF.

           IF      WS-FS-MOVFUNC   EQUAL   10
                   MOVE HIGH-VALUES TO     REG-MOVF
           END-IF.
      *
       0600-99-EXIT.
           EXIT.
      *
      *****************************************************************
       0650-00-FUNC-ALT-EXC        SECTION.
      *****************************************************************
      *
           IF      MOVF-TP-SOL     NOT EQUAL   'I' AND 'A' AND 'E'
      *            SOLICITACAO INVALIDA
                   PERFORM 1100-00-GRAV-FATU-POR-CADFUNC
                   MOVE 'T01'      TO   WS-FLOG-STATUS
                   MOVE 'CADFUNC ' TO   WS-FLOG-ORIGEM
                   PERFORM 1250-00-GRAV-FLOG-POR-MOVFUNC
           END-IF.

           IF      MOVF-TP-SOL     EQUAL   'I'
      *            INCLUSAO NAO PERMITIDA
                   PERFORM 1100-00-GRAV-FATU-POR-CADFUNC
                   MOVE 'I02'      TO   WS-FLOG-STATUS
                   MOVE 'CADFUNC ' TO   WS-FLOG-ORIGEM
                   PERFORM 1250-00-GRAV-FLOG-POR-MOVFUNC
           END-IF.

           IF      MOVF-TP-SOL     EQUAL   'A'
      *            ALTERACAO EFETUADA
                   PERFORM 1150-00-GRAV-FATU-POR-MOVFUNC
                   ADD 001         TO      WS-ALT-CADFATU
                   MOVE 'A01'      TO   WS-FLOG-STATUS
                   MOVE 'CADFUNC ' TO   WS-FLOG-ORIGEM
                   PERFORM 1200-00-GRAV-FLOG-POR-CADFUNC
           END-IF.

           IF      MOVF-TP-SOL     EQUAL   'E'
      *            EXCLUSAO EFETUADA
                   ADD 001         TO      WS-EXC-CADFUNC
                   MOVE 'E01'      TO   WS-FLOG-STATUS
                   MOVE 'CADFUNC ' TO   WS-FLOG-ORIGEM
                   PERFORM 1200-00-GRAV-FLOG-POR-CADFUNC
           END-IF.

           PERFORM 0500-00-LEITURA-CADFUNC.
           PERFORM 0600-00-LEITURA-MOVFUNC.

      *
       0650-99-EXIT.
           EXIT.
      *
      *****************************************************************
       0700-00-FUNC-INC            SECTION.
      *****************************************************************
      *
           IF      MOVF-TP-SOL     NOT EQUAL   'I' AND 'A' AND 'E'
      *            SOLICITACAO INVALIDA
                   MOVE 'T01'      TO   WS-FLOG-STATUS
                   PERFORM 1250-00-GRAV-FLOG-POR-MOVFUNC
           END-IF.

           IF      MOVF-TP-SOL     EQUAL   'I'
      *            INCLUSAO EFETUADA
                   PERFORM 1150-00-GRAV-FATU-POR-MOVFUNC
                   ADD 001         TO      WS-INC-CADFATU
                   MOVE 'I01'      TO   WS-FLOG-STATUS
                   MOVE 'MOVFUNC ' TO   WS-FLOG-ORIGEM
                   PERFORM 1250-00-GRAV-FLOG-POR-MOVFUNC
           END-IF.

           IF      MOVF-TP-SOL     EQUAL   'A'
      *            ALTERACAO NAO PERMITIDA
                   MOVE 'A02'      TO   WS-FLOG-STATUS
                   MOVE 'MOVFUNC ' TO   WS-FLOG-ORIGEM
                   PERFORM 1250-00-GRAV-FLOG-POR-MOVFUNC
           END-IF.

           IF      MOVF-TP-SOL     EQUAL   'E'
      *            EXCLUSAO NAO PERMITIDA
                   MOVE 'E02'      TO   WS-FLOG-STATUS
                   MOVE 'MOVFUNC ' TO   WS-FLOG-ORIGEM
                   PERFORM 1250-00-GRAV-FLOG-POR-MOVFUNC
           END-IF.

           PERFORM 0600-00-LEITURA-MOVFUNC.
      *
       0700-99-EXIT.
           EXIT.
      *
      *****************************************************************
       0750-00-FUNC-SMOV           SECTION.
      *****************************************************************
      *
           PERFORM 1100-00-GRAV-FATU-POR-CADFUNC.

           MOVE    'X01'   TO   WS-FLOG-STATUS.
      *    FUNCIONARIO SEM MOVIMENTO
           MOVE    'CADFUNC '      TO   WS-FLOG-ORIGEM

           PERFORM 1200-00-GRAV-FLOG-POR-CADFUNC.

           PERFORM 0500-00-LEITURA-CADFUNC.
      *
       0750-99-EXIT.
           EXIT.
      *
      *****************************************************************
       1000-00-PROCED-PRINCIPAIS   SECTION.
      *****************************************************************
      *
            IF     FUNC-CODIGO     LESS    MOVF-CODIGO
                   PERFORM 0750-00-FUNC-SMOV
            ELSE
             IF     FUNC-CODIGO    GREATER MOVF-CODIGO
                   PERFORM 0700-00-FUNC-INC
             ELSE
                   PERFORM 0650-00-FUNC-ALT-EXC
             END-IF
            END-IF.
      *
      *
       1000-99-EXIT.
           EXIT.
      *
      *****************************************************************
       1100-00-GRAV-FATU-POR-CADFUNC SECTION.
      *****************************************************************
      *    SECTION DE GRAVACAO DO ARQUIVO CADFATU A PARTIR DE CADFUNC *
      *
           MOVE    FUNCTION CURRENT-DATE
                                   TO      WS-TIME.

           MOVE    WS-TIME-ANO     TO      WS-ANO.
           MOVE    WS-TIME-MES     TO      WS-MES.
           MOVE    WS-TIME-DIA     TO      WS-DIA.
      *
           MOVE    SPACES          TO      REG-FATU.
      *
           MOVE    FUNC-CODIGO     TO      FATU-CODIGO.
           MOVE    FUNC-NOME       TO      FATU-NOME.
           MOVE    FUNC-RG         TO      FATU-RG.
           MOVE    FUNC-CPF        TO      FATU-CPF.
           MOVE    FUNC-ENDERECO   TO      FATU-ENDERECO.
           MOVE    FUNC-CART-TRAB  TO      FATU-CART-TRAB.
           MOVE    FUNC-TELEFONE   TO      FATU-TELEFONE.
           MOVE    FUNC-DT-NAS     TO      FATU-DT-NAS.
           MOVE    FUNC-DT-ADM     TO      FATU-DT-ADM.
           MOVE    WS-DATA         TO      FATU-DT-MAN.

           WRITE   REG-CADFATU     FROM    REG-FATU.

           MOVE   ' NA GRAVACAO '  TO      WS-ACESSO-ARQ.

           MOVE    004             TO      WS-PTO-ERRO.

           PERFORM 0400-00-TESTA-FS-CADFATU.

           ADD     001             TO      WS-GRV-CADFATU.
      *
       1100-99-EXIT.
           EXIT.
      *
      *****************************************************************
       1150-00-GRAV-FATU-POR-MOVFUNC SECTION.
      *****************************************************************
      *                                                               *
      *    SECTION DE GRAVACAO DO ARQUIVO CADFATU A PARTIR DE MOVFUNC *
      *                                                               *
           MOVE    FUNCTION CURRENT-DATE
                                   TO      WS-TIME.

           MOVE    WS-TIME-ANO     TO      WS-ANO.
           MOVE    WS-TIME-MES     TO      WS-MES.
           MOVE    WS-TIME-DIA     TO      WS-DIA.
      *
           MOVE    SPACES          TO      REG-FATU.
      *
           MOVE    MOVF-CODIGO     TO      FATU-CODIGO.
           MOVE    MOVF-NOME       TO      FATU-NOME.
           MOVE    MOVF-RG         TO      FATU-RG.
           MOVE    MOVF-CPF        TO      FATU-CPF.
           MOVE    MOVF-ENDERECO   TO      FATU-ENDERECO.
           MOVE    MOVF-CART-TRAB  TO      FATU-CART-TRAB.
           MOVE    MOVF-TELEFONE   TO      FATU-TELEFONE.
           MOVE    MOVF-DT-NAS     TO      FATU-DT-NAS.
           MOVE    MOVF-DT-ADM     TO      FATU-DT-ADM.
           MOVE    WS-DATA         TO      FATU-DT-MAN.

           WRITE   REG-CADFATU     FROM    REG-FATU.

           MOVE   ' NA GRAVACAO '  TO      WS-ACESSO-ARQ.

           MOVE    005             TO      WS-PTO-ERRO.

           PERFORM 0400-00-TESTA-FS-CADFATU.

           ADD     001             TO      WS-GRV-CADFATU.
      *
       1100-99-EXIT.
           EXIT.
      *
      *****************************************************************
       1200-00-GRAV-FLOG-POR-CADFUNC SECTION.
      *****************************************************************
      *    SECTION DE GRAVACAO DO ARQUIVO CADFLOG A PARTIR DE CADFUNC *
      *
           MOVE    SPACES          TO      REG-FOCO.
      *
           MOVE    FUNC-CODIGO     TO      FLOG-CODIGO.
           MOVE    REG-CADFUNC     TO      FLOG-LOG.
           MOVE    WS-FLOG-STATUS  TO      FLOG-STATUS.
           MOVE    WS-FLOG-ORIGEM  TO      FLOG-ORIGEM.

           WRITE   REG-CADFLOG     FROM    REG-FOCO.

           MOVE   ' NA GRAVACAO '  TO      WS-ACESSO-ARQ.

           MOVE    006             TO      WS-PTO-ERRO.

           PERFORM 0450-00-TESTA-FS-CADFLOG.

           ADD     001             TO      WS-GRV-CADFLOG.
      *
       1100-99-EXIT.
           EXIT.
      *
      *****************************************************************
       1250-00-GRAV-FLOG-POR-MOVFUNC SECTION.
      *****************************************************************
      *
      *    SECTION DE GRAVACAO DO ARQUIVO CADFLOG A PARTIR DE MOVFUNC *
      *
           MOVE    SPACES          TO      REG-FOCO.
      *
           MOVE    FUNC-CODIGO     TO      FLOG-CODIGO.
           MOVE    REG-CADFUNC     TO      FLOG-LOG.
           MOVE    WS-FLOG-STATUS  TO      FLOG-STATUS.
           MOVE    WS-FLOG-ORIGEM  TO      FLOG-ORIGEM.

           WRITE   REG-CADFLOG     FROM    REG-FOCO.

           MOVE   ' NA GRAVACAO '  TO      WS-ACESSO-ARQ.

           MOVE    007             TO      WS-PTO-ERRO.

           PERFORM 0450-00-TESTA-FS-CADFLOG.

           ADD     001             TO      WS-GRV-CADFLOG.
      *
       1100-99-EXIT.
           EXIT.
      *
      *****************************************************************
       3000-00-PROCED-FINAIS       SECTION.
      *****************************************************************
      *
           CLOSE   CADFUNC
                   MOVFUNC
                   CADFATU
                   CADFLOG.

           MOVE   'NO FECHAMENTO'  TO      WS-ACESSO-ARQ.

           MOVE    008             TO      WS-PTO-ERRO.

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
           DISPLAY '******************* GPFPB013 ******************'.
           DISPLAY '*                                             *'.
           DISPLAY '*        ESTATISTICA DE PROCESSAMENTO         *'.
           DISPLAY '*                                             *'.
           DISPLAY '******************* GPFPB013 ******************'.
           DISPLAY '*                                             *'.
           MOVE    WS-LID-CADFUNC  TO      WS-EDICAO.
           DISPLAY '* REGISTROS LIDOS.......- CADFUNC.: ' WS-EDICAO
           ' *'.
           MOVE    WS-LID-MOVFUNC  TO      WS-EDICAO.
           DISPLAY '* REGISTROS LIDOS.......- MOVFUNC.: ' WS-EDICAO
           ' *'.
           MOVE    WS-GRV-CADFATU  TO      WS-EDICAO.
           DISPLAY '* REGISTROS GRAVADOS....- CADFATU.: ' WS-EDICAO
           ' *'.
           MOVE    WS-INC-CADFATU  TO      WS-EDICAO.
           DISPLAY '* REGISTROS INCLUIDOS...- CADFATU.: ' WS-EDICAO
           ' *'.
           MOVE    WS-ALT-CADFATU  TO      WS-EDICAO.
           DISPLAY '* REGISTROS ALTERADOS...- CADFATU.: ' WS-EDICAO
           ' *'.
           MOVE    WS-EXC-CADFUNC  TO      WS-EDICAO.
           DISPLAY '* REGISTROS EXCLUIDOS...- CADFUNC.: ' WS-EDICAO
           ' *'.
           MOVE    WS-GRV-CADFLOG  TO      WS-EDICAO.
           DISPLAY '* REGISTROS GRAVADOS....- CADFLOG.: ' WS-EDICAO
           ' *'.
           DISPLAY '*                                             *'.
           DISPLAY '******************* GPFPB013 ******************'.
      *
       3100-99-EXIT.
           EXIT.
      *
      *****************************************************************
       0999-00-ABEND-ARQ           SECTION.
      *****************************************************************
      *
           MOVE    12              TO      RETURN-CODE.

           DISPLAY '******************* GPFPB013 ******************'.
           DISPLAY '*                                             *'.
           DISPLAY '*      TERMINO ANORMAL DE PROCESSAMENTO       *'.
           DISPLAY '*                                             *'.
           DISPLAY '******************* GPFPB013 ******************'.
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
           DISPLAY '******************* GPFPB013 ******************'.
           DISPLAY '*     P R O G R A M A  C A N C E L A D O      *'.
           DISPLAY '******************* GPFPB013 ******************'.

      *    CALL    'IBM'.

           GOBACK.
      *
       0999-00-EXIT.
           EXIT.
      *
      *****************************************************************
      *                   FIM DO PROGRAMA - GPFPB013                  *
      *****************************************************************
