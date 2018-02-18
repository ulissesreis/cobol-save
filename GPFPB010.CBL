      *****************************************************************
       IDENTIFICATION              DIVISION.
      *****************************************************************
      *
       PROGRAM-ID.                 GPFPB010.
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
      *
       01  WS-EDICAO               PIC     Z.ZZZ.ZZ9.
       01  WS-LINHAS               PIC     9(003) VALUE 99.
       01  WS-PAGI                 PIC     9(003) VALUE ZEROS.
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
      *        TABELA INTERNA PARA CRITICA LOGICA DO MES             *
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

       01      TAB-MESES.
         03    FILLER              PIC     X(009) VALUE '  JANEIRO'.
         03    FILLER              PIC     X(009) VALUE 'FEVEREIRO'.
         03    FILLER              PIC     X(009) VALUE '    MARCO'.
         03    FILLER              PIC     X(009) VALUE '    ABRIL'.
         03    FILLER              PIC     X(009) VALUE '     MAIO'.
         03    FILLER              PIC     X(009) VALUE '    JUNHO'.
         03    FILLER              PIC     X(009) VALUE '    JULHO'.
         03    FILLER              PIC     X(009) VALUE '   AGOSTO'.
         03    FILLER              PIC     X(009) VALUE ' SETEMBRO'.
         03    FILLER              PIC     X(009) VALUE '  OUTUBRO'.
         03    FILLER              PIC     X(009) VALUE ' NOVEMBRO'.
         03    FILLER              PIC     X(009) VALUE ' DEZEMBRO'.
      *
       01      FILLER              REDEFINES   TAB-MESES.
         03    TAB-OCCURS          OCCURS  12  TIMES.
          05   TAB-MES             PIC     X(009).
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
         03    FILLER              PIC     X(001) VALUE SPACES.
         03    FILLER              PIC     X(015) VALUE
               '#OPERACAO BATCH'.
         03    FILLER              PIC     X(042) VALUE SPACES.
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
         03    FILLER              PIC     X(005) VALUE
               'REF: '.
         03    CAB3-MES            PIC     X(009) VALUE SPACES.
         03    FILLER              PIC     X(001) VALUE '/'.
         03    CAB3-ANO            PIC     9(002) VALUE ZEROS.
         03    FILLER              PIC     X(033) VALUE SPACES.
         03    FILLER              PIC     X(031) VALUE
              'C A D A S T R O  D E  P E C A S'.
         03    FILLER              PIC     X(031) VALUE SPACES.
         03    FILLER              PIC     X(009) VALUE 'PAGINA.: '.
         03    CAB3-PAGINA         PIC     ZZ9.
         03    FILLER              PIC     X(009) VALUE SPACES.
      *
      *****************************************************************
      *    LAY-OUT LINHA           4
      *****************************************************************
      *
       01      CAB4.
         03    FILLER              PIC     X(001) VALUE SPACES.
         03    FILLER              PIC     X(131) VALUE ALL '-'.
         03    FILLER              PIC     X(001) VALUE SPACES.
      *
      *****************************************************************
      *    LAY-OUT LINHA           6
      *****************************************************************
      *
       01      CAB6.
         03    FILLER              PIC     X(004) VALUE SPACES.
         03    FILLER              PIC     X(009) VALUE 'COD. PECA'.
         03    FILLER              PIC     X(010) VALUE SPACES.
         03    FILLER              PIC     X(012) VALUE 'NOME DA PECA'.
         03    FILLER              PIC     X(013) VALUE SPACES.
         03    FILLER              PIC     X(012) VALUE 'QTDE.ESTOQUE'.
         03    FILLER              PIC     X(003) VALUE SPACES.
         03    FILLER              PIC     X(012) VALUE 'QTDE. MINIMA'.
         03    FILLER              PIC     X(003) VALUE SPACES.
         03    FILLER              PIC     X(012) VALUE 'QTDE. MAXIMA'.
         03    FILLER              PIC     X(004) VALUE SPACES.
         03    FILLER              PIC     X(010) VALUE 'FORNECEDOR'.
         03    FILLER              PIC     X(008) VALUE SPACES.
         03    FILLER              PIC     X(014) VALUE
              'VALOR UNITARIO'.
         03    FILLER              PIC     X(007) VALUE SPACES.
      *
      *****************************************************************
      *    LAY-OUT DETALHE
      *****************************************************************
      *
       01      DET1.
         03    FILLER              PIC     X(005) VALUE SPACES.
         03    DET1-CODIGO         PIC     ZZ.ZZ9.
         03    FILLER              PIC     X(005) VALUE SPACES.
         03    DET1-NOME           PIC     X(029) VALUE SPACES.
         03    FILLER              PIC     X(006) VALUE SPACES.
         03    DET1-ESTOQUE        PIC     ZZ.ZZ9.
         03    FILLER              PIC     X(009) VALUE SPACES.
         03    DET1-MINIMA         PIC     ZZ.ZZ9.
         03    FILLER              PIC     X(009) VALUE SPACES.
         03    DET1-MAXIMA         PIC     ZZ.ZZ9.
         03    FILLER              PIC     X(006) VALUE SPACES.
         03    DET1-FORNECEDOR     PIC     9.999.999.999.
         03    FILLER              PIC     X(003) VALUE SPACES.
         03    DET1-VALOR          PIC     Z.ZZZ.ZZZ.ZZZ.ZZ9,99.
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
                   '******************* GPFPB010 ******************'
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
           IF      WS-FS-CADPREL NOT EQUAL 00
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
           IF      WS-LINHAS       GREATER 54
                   PERFORM         1100-00-GRAVACAO-CABECALHO
           END-IF.

           PERFORM 1200-00-GRAVACAO-DETALHE.

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

           MOVE    WS-TIME-DIA     TO      WS-DIA.
           MOVE    WS-TIME-MES     TO      WS-MES.
           MOVE    WS-TIME-ANO     TO      WS-ANO.

           MOVE    WS-DATA         TO      CAB1-DATA

           MOVE    CAB1            TO      REG-CADPREL.
           PERFORM 1200-00-GRAVACAO-CADPREL.

           MOVE    WS-TIME-HORA    TO      WS-HORA.
           MOVE    WS-TIME-MINUTO  TO      WS-MINUTO.
           MOVE    WS-TIME-SEGUNDO TO      WS-SEGUNDO.

           MOVE    WS-HORARIO      TO      CAB2-HORA.

           MOVE    CAB2            TO      REG-CADPREL.
           PERFORM 1200-00-GRAVACAO-CADPREL.

           MOVE    TAB-MES (WS-MES)
                                   TO      CAB3-MES.
           MOVE    WS-ANO          TO      CAB3-ANO.

           ADD     001             TO      WS-PAGI.
           MOVE    WS-PAGI         TO      CAB3-PAGINA.

           MOVE    CAB3            TO      REG-CADPREL.
           PERFORM 1200-00-GRAVACAO-CADPREL.

           MOVE    CAB4            TO      REG-CADPREL.
           PERFORM 1200-00-GRAVACAO-CADPREL.

           MOVE    SPACES          TO      REG-CADPREL.
           PERFORM 1200-00-GRAVACAO-CADPREL.

           MOVE    CAB6            TO      REG-CADPREL.
           PERFORM 1200-00-GRAVACAO-CADPREL.

           MOVE    006             TO      WS-LINHAS.
      *
       1200-99-EXIT.
           EXIT.
      *****************************************************************
       1200-00-GRAVACAO-DETALHE    SECTION.
      *****************************************************************
      *
           ADD     001             TO      WS-LINHAS.

           MOVE    PECA-COD-PEC    TO      DET1-CODIGO.
           MOVE    PECA-NOME       TO      DET1-NOME.
           MOVE    PECA-QTD-ETQ    TO      DET1-ESTOQUE.
           MOVE    PECA-QTD-MIN    TO      DET1-MINIMA.
           MOVE    PECA-QTD-MAX    TO      DET1-MAXIMA.
           MOVE    PECA-FORNECEDOR TO      DET1-FORNECEDOR.
           MOVE    PECA-VLR-UNIT   TO      DET1-VALOR.

           MOVE    DET1            TO      REG-CADPREL.

           MOVE   '  NA GRAVACAO ' TO      WS-ACESSO-ARQ.

           MOVE    003             TO      WS-PTO-ERRO.

           PERFORM 1200-00-GRAVACAO-CADPREL.

           ADD     001             TO      WS-GRV-CADPREL.
      *
       1200-99-EXIT.
           EXIT.
      *
      *****************************************************************
       1200-00-GRAVACAO-CADPREL    SECTION.
      *****************************************************************
      *
           WRITE   REG-CADPREL.

           MOVE   ' NA GRAVACAO '  TO      WS-ACESSO-ARQ.

           MOVE    003             TO      WS-PTO-ERRO.

           PERFORM 0400-00-TESTA-FS-CADPREL.
      *
       1200-99-EXIT.
           EXIT.
      *
      *****************************************************************
       3000-00-PROCED-FINAIS       SECTION.
      *****************************************************************
      *
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
           DISPLAY '******************* GPFPB010 ******************'.
           DISPLAY '*                                             *'.
           DISPLAY '*        ESTATISTICA DE PROCESSAMENTO         *'.
           DISPLAY '*                                             *'.
           DISPLAY '******************* GPFPB010 ******************'.
           DISPLAY '*                                             *'.
           MOVE    WS-LID-CADPECA  TO      WS-EDICAO.
           DISPLAY '* REGISTROS LIDOS.......- CADPECA.: ' WS-EDICAO
           ' *'.
           MOVE    WS-GRV-CADPREL  TO      WS-EDICAO.
           DISPLAY '* REGISTROS GRAVADOS....- CADPREL.: ' WS-EDICAO
           ' *'.
           DISPLAY '*                                             *'.
           DISPLAY '******************* GPFPB010 ******************'.
      *
       3100-99-EXIT.
           EXIT.
      *
      *****************************************************************
       0999-00-ABEND-ARQ           SECTION.
      *****************************************************************
      *
           MOVE    12              TO      RETURN-CODE.

           DISPLAY '******************* GPFPB010 ******************'.
           DISPLAY '*                                             *'.
           DISPLAY '*      TERMINO ANORMAL DE PROCESSAMENTO       *'.
           DISPLAY '*                                             *'.
           DISPLAY '******************* GPFPB010 ******************'.
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
           DISPLAY '******************* GPFPB010 ******************'.
           DISPLAY '*     P R O G R A M A  C A N C E L A D O      *'.
           DISPLAY '******************* GPFPB010 ******************'.

      *    CALL    'IBM'.

           GOBACK.
      *
       0999-00-EXIT.
           EXIT.
      *
      *****************************************************************
      *                   FIM DO PROGRAMA - GPFPB010                  *
      *****************************************************************
