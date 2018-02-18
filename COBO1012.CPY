      ******************************************************************
      * SISTEMA         - CURSO DE PROGRAMACAO COBOL II      MAINFRAME *
      ******************************************************************
      * BOOK DO ARQUIVO DE SAIDA - CADFATU         -   LCREL 200 BYTES *
      ******************************************************************
      * NOME DO BOOK    - COBI2012 - CADASTRO FUNCIONARIO ATUALIZADO   *
      ******************************************************************
      * FATU-CODIGO     - PIC 9(005)        - CODIGO DO FUNCIONARIO    *
      * FATU-NOME       - PIC X(030)        - NOME DO FUNCIONARIO      *
      * FATU-RG         - PIC 9(015)        - REGISTRO GERAL DO FUNC.  *
      * FATU-CPF        - PIC 9(011)        - CADASTRO PESSOA FISICA   *
      * FATU-ENDERECO   - PIC X(100)        - ENDERECO COMPLETO        *
      * FATU-CART-TRAB  - PIC 9(005)        - NUMERO CARTEIRA TRABALHO *
      * FATU-TELEFONE   - PIC 9(008)        - TELEFONE RESIDENCIAL     *
      * FATU-DT-NAS     - PIC 9(008)        - DT NASCIMENTO(AAAAMMDD)  *
      * FATU-DT-ADM     - PIC 9(008)        - DT ADMISSAO  (AAAAMMDD)  *
      * FATU-DT-MAN     - PIC X(010)        - DT MANUTENCAO(DD/MM/AAAA)*
      ******************************************************************
      *
       01          REG-FATU.
           03      FATU-CODIGO     PIC     9(005).
           03      FATU-NOME       PIC     X(030).
           03      FATU-RG         PIC     9(015).
           03      FATU-CPF        PIC     9(011).
           03      FATU-ENDERECO   PIC     X(100).
           03      FATU-CART-TRAB  PIC     9(005).
           03      FATU-TELEFONE   PIC     9(008).
           03      FATU-DT-NAS     PIC     9(008).
           03      FATU-DT-ADM     PIC     9(008).
           03      FATU-DT-MAN     PIC     X(010).
      *
      ******************************************************************
      * FIM DO BOOK DO ARQUIVO DE SAIDA        SEQ. - OUTPUT - CADFATU *
      ******************************************************************