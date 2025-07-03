       IDENTIFICATION              DIVISION.
       PROGRAM-ID. ARQ007.
      *========================================
      *== AUTOR: JOSE OLIVEIRA    EMPRESA: CDB
      *== OBJETIVO: ESTATISTICA ARQ. LIDOS E ACHADOS CLIENTE.DAT
      *== DATA: 04/03/2025
      *== OBSERVACOES: INFORMACOES RELEVANTES
      *========================================
       ENVIRONMENT                DIVISION.
       CONFIGURATION              SECTION.
       INPUT-OUTPUT               SECTION.
       FILE-CONTROL.
         SELECT CLIENTES ASSIGN TO "C:\COBOL\DADOS\CLIENTES.DAT"
         FILE STATUS IS FS-CLIENTES.

       DATA                       DIVISION.
       FILE                       SECTION.
       FD CLIENTES.
       01 RG-CLIENTES.
           05 REG-ID           PIC 9(04).
           05 REG-NOME         PIC X(20).
           05 REG-TELEFONE     PIC X(11).

       WORKING-STORAGE            SECTION.
       77 WRK-ID              PIC 9(04)   VALUE ZEROS.

      *---------------ACUMULADORES
       77 WRK-ACUM-LIDOS       PIC 9(04)  VALUE ZEROS.
       77 WRK-ACUM-ACHADOS     PIC 9(04)  VALUE ZEROS.

       77 FS-CLIENTES   PIC 9(02) VALUE ZEROS.
       77 WRK-MSG-ERRO  PIC X(30) VALUE SPACES.

       PROCEDURE                  DIVISION.
       0000-PRINCIPAL             SECTION.
           PERFORM 0100-INICIALIZAR.
           PERFORM 0200-PROCESSAR.
           PERFORM 0300-FINALIZAR.
           STOP RUN.

       0100-INICIALIZAR           SECTION.
           OPEN INPUT CLIENTES.
           IF FS-CLIENTES EQUAL 35
               MOVE "O ARQUIVO NAO PODE SER ABERTO" TO WRK-MSG-ERRO
               PERFORM 9000-TRATA-ERRO
           END-IF.

           DISPLAY 'DIGITE O CODIGO'.
           ACCEPT WRK-ID.

       0200-PROCESSAR             SECTION.
           READ CLIENTES
               IF FS-CLIENTES EQUAL 0
               PERFORM UNTIL FS-CLIENTES NOT EQUAL 0
                  ADD 1 TO WRK-ACUM-LIDOS
                  IF REG-ID EQUAL WRK-ID
                    DISPLAY "ID........" REG-ID
                    DISPLAY "NOME......" REG-NOME
                    DISPLAY "TELEFONE.." REG-TELEFONE
                  ADD 1 TO WRK-ACUM-ACHADOS
                  END-IF

                    READ CLIENTES
               END-PERFORM
               ELSE
               DISPLAY "ARQUIVO VAZIO"
               END-IF.

       0300-FINALIZAR             SECTION.

       CLOSE CLIENTES.
       PERFORM 0310-ESTATISTICA.
       DISPLAY "FIM DE PROGRAMA".

       0310-ESTATISTICA           SECTION.
            DISPLAY '============================='
            DISPLAY 'QTDE REGISTROS LIDOS.....' WRK-ACUM-LIDOS.
            DISPLAY 'QTDE REGISTROS ACHADOS...' WRK-ACUM-ACHADOS.

       9000-TRATA-ERRO.
           DISPLAY WRK-MSG-ERRO.
           GOBACK.
