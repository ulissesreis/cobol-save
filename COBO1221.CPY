      ******************************************************************
      * SISTEMA         - CURSO DE PROGRAMACAO COBOL II      MAINFRAME *
      ******************************************************************
      * BOOK DO ARQUIVO DE SAIDA   - CADMOVA         - LCREL 100 BYTES *
      ******************************************************************
      * NOME DO BOOK    - COBO1221 - CADASTRO DE PECAS MOV. ATUALIZADO *
      ******************************************************************
      * REG-PMOV        - PIC X(100)        - REG. TOTAL DO ARQUIVO    *
      ******************************************************************
      * REG-MOVA        - PIC X(100)        - NIVEL 01                 *
      * MOVA-COD-PEC    - PIC 9(005)        - CODIGO DA PECA           *
      * MOVA-NOME       - PIC X(030)        - NOME DA PECA             *
      * MOVA-FORNECEDOR - PIC 9(010)        - CODIGO FORNECEDOR        *
      * MOVA-VLR-VENDA  - PIC 9(013)V9(002) - VALOR VENDA              *
      * FILLER          - PIC X(040)        - AREA LIVRE               *
      ******************************************************************
      *
       01          REG-MOVA.
         03        MOVA-COD-PEC    PIC     9(005).
         03        MOVA-NOME       PIC     X(030).
         03        MOVA-FORNECEDOR PIC     9(010).
         03        MOVA-VLR-VENDA  PIC     9(013)V9(002).
         03        FILLER          PIC     X(040).
      *
      ******************************************************************
      * FIM DO BOOK DO ARQUIVO DE SAIDA        SEQ. - OUTPUT - CADMOVA *
      ******************************************************************