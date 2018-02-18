      ******************************************************************
      * SISTEMA         - CURSO DE PROGRAMACAO COBOL II      MAINFRAME *
      ******************************************************************
      * BOOK DO ARQUIVO DE SAIDA - CADRESP           - LCREL 200 BYTES *
      ******************************************************************
      * NOME DO BOOK    - COBO1007 - CAD. DE FUNCIONARIOS SELECIONADOS *
      ******************************************************************
      * FSEL-CODIGO     - PIC 9(005)        - CODIGO DO FUNCIONARIO    *
      * FSEL-NOME       - PIC X(030)        - NOME DO FUNCIONARIO      *
      * FSEL-RG         - PIC 9(015)        - REGISTRO GERAL DO FUNC.  *
      * FSEL-CPF        - PIC 9(011)        - CADASTRO PESSOA FISICA   *
      * FSEL-ENDERECO   - PIC X(100)        - ENDERECO COMPLETO        *
      * FSEL-CART-TRAB  - PIC 9(005)        - NUMERO CARTEIRA TRABALHO *
      * FSEL-TELEFONE   - PIC 9(008)        - TELEFONE RESIDENCIAL     *
      * FSEL-DT-NAS     - PIC 9(008)        - DT NASCIMENTO (DDMMAAAA) *
      * FSEL-DT-ADM     - PIC 9(008)        - DT ADMISSAO   (DDMMAAAA) *
      * FILLER          - PIC X(026)        - AREA LIVRE               *
      ******************************************************************
      *
       01          REG-FSEL.
           03      FSEL-CODIGO     PIC     9(005).
           03      FSEL-NOME       PIC     X(030).
           03      FSEL-RG         PIC     9(015).
           03      FSEL-CPF        PIC     9(011).
           03      FSEL-ENDERECO   PIC     X(100).
           03      FSEL-CART-TRAB  PIC     9(005).
           03      FSEL-TELEFONE   PIC     9(008).
           03      FSEL-DT-NAS     PIC     9(008).
           03      FSEL-DT-ADM     PIC     9(008).
           03      FILLER          PIC     X(010).
      *
      ******************************************************************
      * FIM DO BOOK DO ARQUIVO DE SAIDA        SEQ. - OUTPUT - CADRESP *
      ******************************************************************