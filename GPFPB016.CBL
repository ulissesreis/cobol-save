      *****************************************************************
       IDENTIFICATION              DIVISION.
      *****************************************************************
      *
       PROGRAM-ID.                 GPFPB016.
       AUTHOR.                     ULISSES SOUZA
       DATE-WRITTEN.               27/02/2014.
       SECURITY.
      *
      *****************************************************************
      * SISTEMA......: SIGPF - SISTEMA DE GESTAO DE PECAS/FILIAIS     *
      *****************************************************************
      * ANALISTA.....: ULISSES SOUZA                                  *
      * LINGUAGEM....: COBOL/BATCH                                    *
      * PROGRAMADOR..: ULISSES SOUZA                                  *
      * DATA.........: 27/02/2014                                     *
      *****************************************************************
      * OBJETIVO.....: A  PARTIR  DO  CADASTRO  DE  PECAS,  GERAR     *
      *                CADASTRO DE PECAS ACUMULADO.                   *
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
      * OUTPUT.: CADPREL - CADASTRO DE PECAS ATUALIZADO - LRECL = 133 *
      *****************************************************************
      *
           SELECT  CADPREL  ASSIGN TO  UT-S-CADPREL
                   FILE     STATUS IS  WS-FS-CADPREL.
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
           RECORDING  MODE      IS F
           LABEL      RECORD    IS STANDARD
           BLOCK      CONTAINS  0  RECORDS.
      *
       01      REG-CADPECA         PIC     X(100).
      *
      *****************************************************************
      * OUTPUT.: CADPREL - CADASTRO DE PECAS ATUALIZADO - LRECL = 100 *
      *****************************************************************
      *
       FD  CADPREL
           RECORDING  MODE      IS F
           LABEL      RECORD    IS STANDARD
           BLOCK      CONTAINS  0  RECORDS.
      *
       01      REG-CADPREL         PIC     X(133).
      *
      *****************************************************************
       WORKING-STORAGE             SECTION.
      *****************************************************************
      *
       01  WS-FS-CADPECA           PIC     9(002) VALUE ZEROS.
       01  WS-FS-CADPREL           PIC     9(002) VALUE ZEROS.
      *
       01  WS-LID-CADPECA          PIC     9(018) VALUE ZEROS.
       01  WS-GRV-CADPREL          PIC     9(018) VALUE ZEROS.
       01  WS-COD-PEC              PIC     9(005) VALUE ZEROS.
      *
       01  WS-EDICAO               PIC     Z.ZZZ.ZZ9.
       01  WS-LINHAS               PIC     9(003) VALUE ZEROS.
       01  WS-PAGI                 PIC     9(003) VALUE ZEROS.
      *
       01  WS-TOT-ETQ              PIC     9(007) VALUE ZEROS.
       01  WS-TOT-MIN              PIC     9(007) VALUE ZEROS.
       01  WS-TOT-MAX              PIC     9(007) VALUE ZEROS.
       01  WS-TOT-VLR              PIC     9(016)V9(002) VALUE ZEROS.
      *
      *****************************************************************
      *        VARIAVEIS PARA TRATAMENTO DE ABEND                     *
      *****************************************************************
      *
       01  WS-ACESSO-ARQ           PIC     X(013) VALUE SPACES.
       01  WS-DDNAME-ARQ           PIC     X(008) VALUE SPACES.
       01  WS-FS-ARQ               PIC     9(002) VALUE ZEROS.
      *
       01  WS-PTO-ERRO             PIC     9(003) VALUE ZEROS.
      *
      *****************************************************************
      * INPUT..: CADPECA - CADASTRO DE PECAS            - LRECL = 100 *
      *****************************************************************
      *
           COPY    COBI1001.
      *
      *****************************************************************
      *        TABELA I TERNA PARA CRITICA LOGICA DO MES             *
      *****************************************************************
      *
       01      WS-TIME             PIC     X(014)  VALUE ZEROS.
       01      FILLER              REDEFINES       WS-TIME.
        03     WS-TIME-ANO         PIC     9(004).
        03     WS-TIME-MES         PIC     9(002).
        03     WS-TIME-DIA         PIC     9(002).
        03     WS-TIME-HORA        PIC     9(002).
        03     WS-TIME-MINUTO      PIC     9(002).
        03     WS-TIME-SEGUNDO     PIC     9(002).
      *
       01      WS-DATA             PIC     X(010)  VALUE '99/99/9999'.
       01      FILLER              REDEFINES       WS-DATA.
        03     WS-DIA              PIC     9(002).
        03     FILLER              PIC     X(001).
        03     WS-MES              PIC     9(002).
        03     FILLER              PIC     X(001).
        03     WS-ANO              PIC     9(004).

       01      WS-HORARIO          PIC     X(008)  VALUE '99:99:99'.
       01      FILLER              REDEFINES       WS-HORARIO.
        03     WS-HORA             PIC     9(002).
        03     FILLER              PIC     X(001).
        03     WS-MINUTO           PIC     9(002).
        03     FILLER              PIC     X(001).
        03     WS-SEGUNDO          PIC     9(002).
      *
      *****************************************************************
      *    LAY-OUT LINHA           1
      *****************************************************************
      *
       01      CAB1.
         03    FILLER              PIC     X(001) VALUE '1'.
         03    FILLER              PIC     X(010) VALUE '#MAINFRAME'.
         03    FILLER              PIC     X(030) VALUE SPACES.
         03    FILLER              PIC     X(053) VALUE
           'C U R S O  D E  P R O G R A M A C A O  C O B O L  II'.
         03    FILLER              PIC     X(019) VALUE SPACES.
         03    FILLER              PIC     X(009) VALUE 'DATA...: '.
         03    CAB1-DATA           PIC     X(010) VALUE '99/99/9999'.
      *
      *****************************************************************
      *    LAY-OUT LINHA           2
      *****************************************************************
      *
       01      CAB2.
         03    FILLER              PIC     X(058) VALUE SPACES.
         03    FILLER              PIC     X(017) VALUE
               'R E L A T O R I O'.
         03    FILLER              PIC     X(038) VALUE SPACES.
         03    FILLER              PIC     X(009) VALUE 'HORA...: '.
         03    CAB2-HORA           PIC     X(008) VALUE '99:99:99'.
      *
      *****************************************************************
      *    LAY-OUT LINHA           3
      *****************************************************************
      *
       01      CAB3.
         03    FILLER              PIC     X(001) VALUE SPACES.
         03    FILLER              PIC     X(015) VALUE
               '#OPERACAO BATCH'.
         03    FILLER              PIC     X(026) VALUE SPACES.
         03    FILLER              PIC     X(050) VALUE
           'C A D A S T R O  D E  P E C A S  A C U M U L A D O'.
         03    FILLER              PIC     X(021) VALUE SPACES.
         03    FILLER              PIC     X(009) VALUE 'PAGINA.: '.
         03    CAB3-PAGINA         PIC     ZZ9.
         03    FILLER              PIC     X(009) VALUE SPACES.
      *
      *****************************************************************
      *    LAY-OUT LINHA           4
      *****************************************************************
      *
       01      CAB4.
         03        FILLER          PIC     X(001) VALUE SPACES.
         03        FILLER          PIC     X(131) VALUE ALL '-'.
         03        FILLER          PIC     X(001) VALUE SPACES.
      *
      *****************************************************************
      *    LAY-OUT CABECALHO       5
      *****************************************************************
      *
       01      CAB5.
         03    FILLER              PIC     X(002) VALUE SPACES.
         03    FILLER              PIC     X(007) VALUE 'PECA.: '.
         03    DET-CODIGO          PIC     ZZ.ZZ9.
         03    FILLER              PIC     X(003) VALUE ' - '.
         03    DET-NOME            PIC     X(029) VALUE SPACES.

      *
      *****************************************************************
      *    LAY-OUT CABECALHO       6
      *****************************************************************
      *
       01      CAB6.
         03    FILLER              PIC     X(011) VALUE SPACES.
         03    FILLER              PIC     X(012) VALUE 'QTDE.ESTOQUE'.
         03    FILLER              PIC     X(009) VALUE SPACES.
         03    FILLER              PIC     X(012) VALUE 'QTDE. MINIMA'.
         03    FILLER              PIC     X(009) VALUE SPACES.
         03    FILLER              PIC     X(012) VALUE 'QTDE. MAXIMA'.
         03    FILLER              PIC     X(009) VALUE SPACES.
         03    FILLER              PIC     X(010) VALUE 'FORNECEDOR'.
         03    FILLER              PIC     X(014) VALUE SPACES.
         03    FILLER              PIC     X(014) VALUE
         'VALOR UNITARIO'.
      *
      *****************************************************************
      *    LAY-OUT DETALHE
      *****************************************************************
      *
       01      DETALHE.
         03    FILLER              PIC     X(014) VALUE SPACES.
         03    DET-ESTOQUE         PIC     ZZ.ZZ9.
         03    FILLER              PIC     X(015) VALUE SPACES.
         03    DET-MINIMA          PIC     ZZ.ZZ9.
         03    FILLER              PIC     X(015) VALUE SPACES.
         03    DET-MAXIMA          PIC     ZZ.ZZ9.
         03    FILLER              PIC     X(011) VALUE SPACES.
         03    DET-FORNECEDOR      PIC     9.999.999.999.
         03    FILLER              PIC     X(009) VALUE SPACES.
         03    DET-VALOR           PIC     Z.ZZZ.ZZZ.ZZZ.ZZ9,99.
      *
      *****************************************************************
      *    LAY-OUT ESPACOS
      *****************************************************************
      *
       01      ESPACOS.
         03    FILLER              PIC     X(133) VALUE SPACES.
      *
      *****************************************************************
      *    LAY-OUT RODAPE LINHA 01
      *****************************************************************
      *
       01      ROD1.
         03    FILLER              PIC     X(001) VALUE SPACES.
         03    FILLER              PIC     X(007) VALUE 'TOTAL.:'.
         03    FILLER              PIC     X(003) VALUE SPACES.
         03    ROD-ESTOQUE         PIC     Z.ZZZ.ZZ9.
         03    FILLER              PIC     X(012) VALUE SPACES.
         03    ROD-MINIMA          PIC     Z.ZZZ.ZZ9.
         03    FILLER              PIC     X(012) VALUE SPACES.
         03    ROD-MAXIMA          PIC     Z.ZZZ.ZZ9.
         03    FILLER              PIC     X(029) VALUE SPACES.
         03    ROD-VALOR           PIC     Z.ZZZ.ZZZ.ZZZ.ZZZ.ZZ9,99.
      *
      *****************************************************************
      *    LAY-OUT RODAPE LINHA 01
      *****************************************************************
      *
       01      ROD2.
         03    FILLER              PIC     X(001) VALUE SPACES.
         03    FILLER              PIC     X(026) VALUE
               'UNIDADE RESPONSAVEL: SIGPF'.
      *
      *****************************************************************
       LINKAGE                     SECTION.
      *****************************************************************
       PROCEDURE                   DIVISION.
      *****************************************************************
      *
           PERFORM 0100-00-PROCED-INICIAIS.

           PERFORM 1000-00-PROCED-PRINCIPAIS
             UNTIL WS-FS-CADPECA   EQUAL 10.

           PERFORM 3000-00-PROCED-FINAIS.

           GOBACK.
      *
      *****************************************************************
       0100-00-PROCED-INICIAIS     SECTION.
      *****************************************************************
      *
           OPEN    INPUT   CADPECA
                   OUTPUT  CADPREL.

           MOVE   ' NA ABERTURA '  TO      WS-ACESSO-ARQ.

           MOVE    001             TO      WS-PTO-ERRO.

           PERFORM 0200-00-TESTA-FILE-STATUS.

           PERFORM 0500-00-LEITURA-CADPECA.

           IF      WS-FS-CADPECA   EQUAL   10
                   DISPLAY
                   '******************* GPFPB001 ******************'
                   DISPLAY
                   '*                                             *'
                   DISPLAY
                   '*          ARQUIVO CADPECA ESTA VAZIO         *'
                   DISPLAY
                   '*                                             *'
           ELSE
                   MOVE PECA-COD-PEC   TO  WS-COD-PEC
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

           PERFORM 0400-00-TESTA-FS-CADPREL.
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
       0400-00-TESTA-FS-CADPREL    SECTION.
      *****************************************************************
      *
           IF      WS-FS-CADPREL   NOT     EQUAL 00
                   MOVE 'CADPREL'  TO      WS-DDNAME-ARQ
                   MOVE  WS-FS-CADPREL
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
           PERFORM 1300-00-TRATAMENTO-CABECALHO.

           PERFORM 1200-00-GRAVACAO-CADPREL.

           MOVE    PECA-COD-PEC    TO      WS-COD-PEC.

           PERFORM 0500-00-LEITURA-CADPECA.

      *
       1000-99-EXIT.
           EXIT.
      *
      *****************************************************************
       1100-00-GRAVACAO-CABECALHO    SECTION.
      *****************************************************************
      *
           MOVE    FUNCTION CURRENT-DATE
                                   TO      WS-TIME.

           MOVE    WS-TIME-ANO     TO      WS-ANO.
           MOVE    WS-TIME-MES     TO      WS-MES.
           MOVE    WS-TIME-DIA     TO      WS-DIA.
           MOVE    WS-TIME-HORA    TO      WS-HORA.
           MOVE    WS-TIME-MINUTO  TO      WS-MINUTO.
           MOVE    WS-TIME-SEGUNDO TO      WS-SEGUNDO.
      *
           MOVE    WS-HORARIO      TO      CAB2-HORA.
           MOVE    WS-DATA         TO      CAB1-DATA
           ADD     01              TO      WS-PAGI.
           MOVE    WS-PAGI         TO      CAB3-PAGINA.


           MOVE    PECA-COD-PEC    TO      DET-CODIGO.
           MOVE    PECA-NOME       TO      DET-NOME.

           WRITE   REG-CADPREL     FROM    CAB1.
           WRITE   REG-CADPREL     FROM    CAB2.
           WRITE   REG-CADPREL     FROM    CAB3.
           WRITE   REG-CADPREL     FROM    CAB4.
           WRITE   REG-CADPREL     FROM    ESPACOS.
           WRITE   REG-CADPREL     FROM    CAB5.
           WRITE   REG-CADPREL     FROM    ESPACOS.
           WRITE   REG-CADPREL     FROM    CAB6.

           MOVE   ' NO CABECALHO ' TO     WS-ACESSO-ARQ.

           MOVE    003             TO      WS-PTO-ERRO.

           MOVE    006             TO      WS-LINHAS.
      *
       1200-99-EXIT.
           EXIT.
      *
      *****************************************************************
       1200-00-GRAVACAO-CADPREL    SECTION.
      *****************************************************************
      *
           MOVE    PECA-QTD-ETQ    TO      DET-ESTOQUE.
           MOVE    PECA-QTD-MIN    TO      DET-MINIMA.
           MOVE    PECA-QTD-MAX    TO      DET-MAXIMA.
           MOVE    PECA-FORNECEDOR TO      DET-FORNECEDOR.
           MOVE    PECA-VLR-UNIT   TO      DET-VALOR.

           WRITE   REG-CADPREL     FROM    DETALHE.

           MOVE   ' NA GRAVACAO '  TO      WS-ACESSO-ARQ.

           MOVE    003             TO      WS-PTO-ERRO.

           PERFORM 0400-00-TESTA-FS-CADPREL.

           ADD     001             TO      WS-GRV-CADPREL.
           ADD     001             TO      WS-LINHAS.

           ADD     PECA-QTD-ETQ    TO      WS-TOT-ETQ.
           ADD     PECA-QTD-MIN    TO      WS-TOT-MIN.
           ADD     PECA-QTD-MAX    TO      WS-TOT-MAX.
           ADD     PECA-VLR-UNIT   TO      WS-TOT-VLR.
      *
       1200-99-EXIT.
           EXIT.
      *
      *****************************************************************
       1300-00-TRATAMENTO-CABECALHO SECTION.
      *****************************************************************
      *
           IF      WS-LID-CADPECA  EQUAL   001
                   PERFORM         1100-00-GRAVACAO-CABECALHO
           END-IF.

      *    MUDANCA DE PAGINA POR NUMERO DE LINHAS NO RELATORIO
           IF      WS-LINHAS       GREATER 54
                   PERFORM         1400-00-GRAVACAO-RODAPE
                   PERFORM         1100-00-GRAVACAO-CABECALHO
           END-IF.

      *    MUDANCA DE PAGINA POR QUEBRA DE CODIGO DE PRODUTO
           IF      WS-COD-PEC      NOT EQUAL   PECA-COD-PEC
                   PERFORM         1400-00-GRAVACAO-RODAPE
                   MOVE ZEROS      TO          WS-PAGI
                   PERFORM         1100-00-GRAVACAO-CABECALHO
           END-IF.
      *
       1300-99-EXIT.
           EXIT.
      *
      *****************************************************************
       1400-00-GRAVACAO-RODAPE     SECTION.
      *****************************************************************
      *
           MOVE    WS-TOT-ETQ      TO      ROD-ESTOQUE.
           MOVE    WS-TOT-MIN      TO      ROD-MINIMA.
           MOVE    WS-TOT-MAX      TO      ROD-MAXIMA.
           MOVE    WS-TOT-VLR      TO      ROD-VALOR.

           WRITE   REG-CADPREL     FROM    ESPACOS.
           WRITE   REG-CADPREL     FROM    ROD1.
           WRITE   REG-CADPREL     FROM    CAB4.
           WRITE   REG-CADPREL     FROM    ESPACOS.
           WRITE   REG-CADPREL     FROM    ROD2.

           MOVE   ' NA GRAVACAO '  TO      WS-ACESSO-ARQ.

           MOVE    003             TO      WS-PTO-ERRO.

           MOVE    ZEROS           TO      WS-TOT-ETQ
                                           WS-TOT-MIN
                                           WS-TOT-MAX
                                           WS-TOT-VLR.

           PERFORM 0400-00-TESTA-FS-CADPREL.
      *
       1400-99-EXIT.
           EXIT.
      *
      *****************************************************************
       3000-00-PROCED-FINAIS       SECTION.
      *****************************************************************
      *
           IF      WS-LID-CADPECA  GREATER 001
                   PERFORM         1400-00-GRAVACAO-RODAPE
           END-IF.

           CLOSE   CADPECA
                   CADPREL.

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
           DISPLAY '******************* GPFPB001 ******************'.
           DISPLAY '*                                             *'.
           DISPLAY '*        ESTATISTICA DE PROCESSAMENTO         *'.
           DISPLAY '*                                             *'.
           DISPLAY '******************* GPFPB001 ******************'.
           DISPLAY '*                                             *'.
           MOVE    WS-LID-CADPECA  TO      WS-EDICAO.
           DISPLAY '* REGISTROS LIDOS.......- CADPECA.: ' WS-EDICAO
           ' *'.
           MOVE    WS-GRV-CADPREL  TO      WS-EDICAO.
           DISPLAY '* REGISTROS GRV.DETALHE.- CADPREL.: ' WS-EDICAO
           ' *'.
           DISPLAY '*                                             *'.
           DISPLAY '******************* GPFPB001 ******************'.
      *
       3100-99-EXIT.
           EXIT.
      *
      *****************************************************************
       0999-00-ABEND-ARQ           SECTION.
      *****************************************************************
      *
           MOVE    12              TO      RETURN-CODE.

           DISPLAY '******************* GPFPB001 ******************'.
           DISPLAY '*                                             *'.
           DISPLAY '*      TERMINO ANORMAL DE PROCESSAMENTO       *'.
           DISPLAY '*                                             *'.
           DISPLAY '******************* GPFPB001 ******************'.
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
           DISPLAY '******************* GPFPB001 ******************'.
           DISPLAY '*     P R O G R A M A  C A N C E L A D O      *'.
           DISPLAY '******************* GPFPB001 ******************'.

      *    CALL    'IBM'.

           GOBACK.
      *
       0999-00-EXIT.
           EXIT.
      *
      *****************************************************************
      *                   FIM DO PROGRAMA - GPFPB001                  *
      *****************************************************************
