      ******************************************************************
      * SISTEMA         - CURSO DE PROGRAMACAO COBOL II      MAINFRAME *
      ******************************************************************
      * BOOK DO ARQUIVO DE SAIDA - CADFDES           - LCREL 200 BYTES *
      ******************************************************************
      * NOME DO BOOK    - COBO3007 - CAD. DE FUNCIONARIOS DESPREZADOS  *
      ******************************************************************
      * FDES-CODIGO     - PIC 9(005)        - CODIGO DO FUNCIONARIO    *
      * FDES-NOME       - PIC X(030)        - NOME DO FUNCIONARIO      *
      * FDES-RG         - PIC 9(015)        - REGISTRO GERAL DO FUNC.  *
      * FDES-CPF        - PIC 9(011)        - CADASTRO PESSOA FISICA   *
      * FDES-ENDERECO   - PIC X(100)        - ENDERECO COMPLETO        *
      * FDES-CART-TRAB  - PIC 9(005)        - NUMERO CARTEIRA TRABALHO *
      * FDES-TELEFONE   - PIC 9(008)        - TELEFONE RESIDENCIAL     *
      * FDES-DT-NAS     - PIC 9(008)        - DT NASCIMENTO (DDMMAAAA) *
      * FDES-DT-ADM     - PIC 9(008)        - DT ADMISSAO   (DDMMAAAA) *
      * FILLER          - PIC X(026)        - AREA LIVRE               *
      ******************************************************************
      *
       01          REG-FDES.
           03      FDES-CODIGO     PIC     9(005).
           03      FDES-NOME       PIC     X(030).
           03      FDES-RG         PIC     9(015).
           03      FDES-CPF        PIC     9(011).
           03      FDES-ENDERECO   PIC     X(100).
           03      FDES-CART-TRAB  PIC     9(005).
           03      FDES-TELEFONE   PIC     9(008).
           03      FDES-DT-NAS     PIC     9(008).
           03      FDES-DT-ADM     PIC     9(008).
           03      FILLER          PIC     X(010).
      *
      ******************************************************************
      * FIM DO BOOK DO ARQUIVO DE SAIDA        SEQ. - OUTPUT - CADFDES *
      ******************************************************************