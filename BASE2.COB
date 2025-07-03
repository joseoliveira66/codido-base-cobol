       IDENTIFICATION              DIVISION.
       PROGRAM-ID. BASE2.
      *========================================
      *== AUTOR: JOSE OLIVEIRA    EMPRESA: CDB
      *== OBJETIVO: ESTRUTURA SISTEMA PRONTA
      *== DATA: XX/XX/20XX
      *== OBSERVACOES: INFORMACOES RELEVANTES
      *========================================
       ENVIRONMENT                DIVISION.
       CONFIGURATION              SECTION.

       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       DATA                           DIVISION.
       WORKING-STORAGE                SECTION.

       PROCEDURE                       DIVISION.
       0001-PRINCIPAL                  SECTION.
           PERFORM 0100-INICIALIZAR
           PERFORM 0200-PROCESSAR
           PERFORM 0300-FINALIZAR
           STOP RUN.

       0100-INICIALIZAR                SECTION.

       0200-PROCESSAR                   SECTION.

       0300-FINALIZAR                   SECTION.
                DISPLAY "FIM DO PROGRAMA".
