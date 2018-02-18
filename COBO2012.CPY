      ******************************************************************
      * SISTEMA         - CURSO DE PROGRAMACAO COBOL II      MAINFRAME *
      ******************************************************************
      * BOOK DO ARQUIVO DE SAIDA - CADFLOG           - LCREL 250 BYTES *
      ******************************************************************
      * NOME DO BOOK    - COBO2012 - CADASTRO DE LOG (HISTORICO)       *
      ******************************************************************
      * FLOG-CODIGO     - PIC 9(005)        - CODIGO DO FUNCIONARIO    *
      * FLOG-STATUS     - PIC X(003)        - STATUS DO REGISTRO       *
      *                   S/MOVIMENTO 'X01' - FUNCIONARIO S/ MOVIMENTO *
      *                   TP INVALIDO 'T01' - SOLICITACAO INVALIDA     *
      *                   INCLUSAO    'I01' - INCLUSAO EFETUADA        *
      *                               'I02' - INCLUSAO NAO PERMITIDA   *
      *                   ALTERACAO   'A01' - ALTERACAO EFETUADA       *
      *                               'A02' - ALTERACAO NAO PERMITIDA  *
      *                   EXCLUSAO    'E01' - EXCLUSAO EFETUADA        *
      *                               'E02' - EXCLUSAO NAO PERMITIDA   *
      * FLOG-ORIGEM     - PIC X(003)        - ORIGEM DO REGISTRO       *
      * FLOG-LOG        - PIC X(200)        - REGISTRO LOG (HISTORICO) *
      * FILLER          - PIC X(034)        - AREA LIVRE               *
      ******************************************************************
      *
       01          REG-FOCO.
           03      FLOG-CODIGO     PIC     9(005).
           03      FLOG-STATUS     PIC     X(003).
           03      FLOG-ORIGEM     PIC     X(008).
           03      FLOG-LOG        PIC     X(200).
           03      FILLER          PIC     X(034).
      *
      ******************************************************************
      * FIM DO BOOK DO ARQUIVO DE SAIDA        SEQ. - OUTPUT - CADFLOG *
      ******************************************************************