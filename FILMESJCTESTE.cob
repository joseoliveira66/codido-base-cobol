       IDENTIFICATION                             DIVISION.
       PROGRAM-ID. FILMESJC.
      *****************************************
      *OBJECT: PROGRAMA GESTAO CATALOGO DE FILMES
      *AUTHOR: JOSE CARLOS DE OLIVEIRA
      *CLIENT: CODIGO DE BASE / EDU360
      *****************************************
      * -----CONFIGURACOES DO AMBIENTE DE EXECUCAO--------
       ENVIRONMENT                                DIVISION.
       CONFIGURATION                              SECTION.

       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      * -----SECAO PARA DECLARACAO DE ARQUIVOS-----------
       INPUT-OUTPUT                               SECTION.
       FILE-CONTROL.
           SELECT FILMES ASSIGN TO 'DADOS\FILMES.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               FILE STATUS IS FILMES-STATUS
               RECORD KEY IS FILMES-CHAVE.

           SELECT RELATO ASSIGN TO 'DADOS\RELATO.TXT'
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS RELATO-STATUS.

      * -----SECAO PARA DECLARACAO DE DADOS----------------
       DATA                                       DIVISION.

      * -----DESCRICAO DOS ARQUIVOS------------------------
       FILE                                       SECTION.
       FD FILMES.
       01 FILMES-REG.
           05 FILMES-CHAVE.
               10 CODFILME          PIC 9(005) BLANK WHEN ZEROS.
           05 TITULO                PIC X(030).
           05 GENERO                PIC X(008).
           05 DURACAO               PIC 9(003).
           05 DISTRIBUIDORA         PIC X(015).
           05 NOTA                  PIC 9(002).

       FD RELATO.
       01 RELATO-REG.
           05 RELATO-DADOS          PIC X(79).

      * -----VARIAVEIS DE TRABALHO DO PROGRAMA-----------
       WORKING-STORAGE                            SECTION.
       77 WRK-OPCAO                 PIC X(1).
       77 WRK-MODULO                PIC X(25).
       77 WRK-TECLA                 PIC X(1).
       77 FILMES-STATUS             PIC 9(02).
       77 RELATO-STATUS             PIC 9(02).
       77 WRK-MSGERRO               PIC X(60).
       77 WRK-CONTALINHA            PIC 9(03)   VALUE 0.
       77 WRK-QTREGISTROS           PIC 9(05)   VALUE 0.
       77 WRK-SEQUENCIA             PIC X(100).
       77 WRK-CONTINUA-CADASTRO     PIC X(02)   VALUE 'S'.
       77 WRK-NUMPAGINA             PIC 9(03)   VALUE 1.
       77 WRK-TITULO                PIC X(30).
       77 WRK-GENERO                PIC X(08).
       77 WRK-DISTRIBUIDORA         PIC X(15).
       77 WRK-DURACAO               PIC 9(03).
       77 WRK-NOTA                  PIC 9(02).

      * -DEFINICAO DAS TELAS DE INTERACAO COM O USUARIO--
       SCREEN                                     SECTION.
       01 TELA.
           05 LIMPA-TELA.
              10 BLANK SCREEN.
              10 LINE 01 COLUMN 01 PIC X(20) ERASE EOL
                  BACKGROUND-COLOR 3.
              10 LINE 01 COLUMN 45 PIC X(40)
                  BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
                  FROM 'CONTROLE CATALOGO DE FILMES'.
              10 LINE 02 COLUMN 01 PIC X(25) ERASE EOL
                  BACKGROUND-COLOR 1 FROM WRK-MODULO.
              10 LINE 07 COLUMN 10 PIC X(50) ERASE EOL
                  BACKGROUND-COLOR 0 FROM WRK-SEQUENCIA.

       01 MENU.
           05 LINE 07 COLUMN 50 VALUE '1 - CADASTRO'.
           05 LINE 08 COLUMN 50 VALUE '2 - CONSULTA'.
           05 LINE 09 COLUMN 50 VALUE '3 - ALTERACAO'.
           05 LINE 10 COLUMN 50 VALUE '4 - EXCLUSAO'.
           05 LINE 11 COLUMN 50 VALUE '5 - RELACAOTELA'.
           05 LINE 12 COLUMN 50 VALUE '6 - RELACAODISCO'.
           05 LINE 13 COLUMN 50 VALUE 'X - SAIR'.
           05 LINE 14 COLUMN 50 VALUE 'OPCAO--->'.
           05 LINE 14 COLUMN 59 USING WRK-OPCAO.

       01 TELA-REGISTRO.
           05 CHAVE FOREGROUND-COLOR 2.
               10 LINE 10 COLUMN 10 VALUE 'CODIGO:'.
               10 COLUMN PLUS 2 PIC 9(005) USING CODFILME
                  BLANK WHEN ZEROS.
           05 SS-DADOS.
               10 LINE 11 COLUMN 10 VALUE 'TITULO:'.
               10 COLUMN PLUS 2 PIC X(030) USING TITULO.
               10 LINE 12 COLUMN 10 VALUE 'GENERO:'.
               10 COLUMN PLUS 2 PIC X(008) USING GENERO.
               10 LINE 13 COLUMN 10 VALUE 'DURACAO:'.
               10 COLUMN PLUS 2 PIC 9(003) USING DURACAO.
               10 LINE 14 COLUMN 10 VALUE 'DISTRIBUIDORA:'.
               10 COLUMN PLUS 2 PIC X(015) USING DISTRIBUIDORA.
               10 LINE 15 COLUMN 10 VALUE 'NOTA:'.
               10 COLUMN PLUS 2 PIC 9(002) USING NOTA.

       01 MOSTRA-ERRO.
           02 MSG-ERRO.
               10 LINE 20 COLUMN 01 ERASE EOL
                           BACKGROUND-COLOR 0.
               10 LINE 20 COLUMN 10 PIC X(60)
                           BACKGROUND-COLOR 0
                           FROM WRK-MSGERRO.
               10 COLUMN PLUS 2 PIC X(01)
                           BACKGROUND-COLOR 0
                           USING WRK-TECLA.

      * -----LOGICA PRINCIPAL DO PROGRAMA-----------------
       PROCEDURE                                  DIVISION.

      * -----SECAO PRINCIPAL DO PROGRAMA------------------
       0001-PRINCIPAL                             SECTION.
           PERFORM 1000-INICIAR THRU 1100-MONTATELA.
           PERFORM 2000-PROCESSAR UNTIL WRK-OPCAO EQUAL 'X' or 'x'.
           PERFORM 3000-FINALIZAR.
           STOP RUN.

      * -INICIALIZA O PROGRAMA, ABRINDO O ARQUIVO DE FILMES-
       1000-INICIAR.
           MOVE 'MENU' TO WRK-MODULO.
           OPEN I-O FILMES.
               IF FILMES-STATUS EQUAL 35 THEN
                   OPEN OUTPUT FILMES
                   CLOSE FILMES
                   OPEN I-O FILMES
               END-IF.

      * -----MONTA A TELA PRINCIPAL DO MENU------------
       1100-MONTATELA.
           MOVE 0 TO WRK-QTREGISTROS.
           DISPLAY TELA.
           ACCEPT MENU.

      * -----PROCESSA A OPCAO SELECIONADA PELO USUARIO----
       2000-PROCESSAR.
           MOVE SPACES TO TITULO GENERO DISTRIBUIDORA WRK-MSGERRO.
           MOVE ZEROS TO DURACAO NOTA.

           IF WRK-OPCAO = '1'
               PERFORM 5000-CADASTRO
           ELSE IF WRK-OPCAO = '2'
               PERFORM 6000-CONSULTA
           ELSE IF WRK-OPCAO = '3'
               PERFORM 7000-ALTERACAO
           ELSE IF WRK-OPCAO = '4'
               PERFORM 8000-EXCLUSAO
           ELSE IF WRK-OPCAO = '5'
               PERFORM 9000-RELACAOTELA
           ELSE IF WRK-OPCAO = '6'
               PERFORM 9100-RELACAODISCO
           ELSE IF WRK-OPCAO = 'X' OR WRK-OPCAO = 'x'
               CONTINUE
           ELSE
               MOVE 'ENTRE COM A OPCAO CORRETA - ENTER' TO WRK-MSGERRO
               PERFORM 9999-MOSTRAR-ERRO
           END-IF.

           PERFORM 1100-MONTATELA.

      * ---FINALIZA O PROGRAMA, FECHANDO O ARQUIVO DE FILMES--
       3000-FINALIZAR.
               CLOSE FILMES.

      * -----ROTINA PARA CADASTRAR UM NOVO FILME--------------
       5000-CADASTRO.
           MOVE 'MODULO - INCLUSAO' TO WRK-MODULO.
           MOVE 'DIGITE O CODIGO E TECLE ENTER E TAB PARA NAVEGAR'
                 TO WRK-SEQUENCIA.
           MOVE 'S' TO WRK-CONTINUA-CADASTRO.
           PERFORM UNTIL WRK-CONTINUA-CADASTRO NOT = 'S' AND
                         WRK-CONTINUA-CADASTRO NOT = 's'
             MOVE SPACES TO TITULO GENERO DISTRIBUIDORA WRK-MSGERRO
             MOVE ZEROS TO DURACAO NOTA CODFILME
             DISPLAY TELA
             ACCEPT TELA-REGISTRO
               READ FILMES
                INVALID KEY
                    DISPLAY TELA
                    DISPLAY TELA-REGISTRO
                    ACCEPT SS-DADOS
                    DISPLAY 'DESEJA GRAVAR? (S/N)' AT LINE 20 COLUMN 10
                    ACCEPT WRK-TECLA AT LINE 20 COLUMN 32
                        IF WRK-TECLA = 'S' OR WRK-TECLA = 's'
                            WRITE FILMES-REG
                                INVALID KEY
                                   MOVE 'ERRO AO GRAVAR. ENTER'
                                         TO WRK-MSGERRO
                                NOT INVALID KEY
                                   MOVE 'CADASTRADO OK. ENTER'
                                         TO WRK-MSGERRO
                            END-WRITE
                        ELSE
                    MOVE 'FILME NAO GRAVADO. ENTER' TO WRK-MSGERRO
                    END-IF
                    PERFORM 9999-MOSTRAR-ERRO
                NOT INVALID KEY
                    MOVE 'FILME JA EXISTE. ENTER' TO WRK-MSGERRO
                    PERFORM 9999-MOSTRAR-ERRO
               END-READ
           DISPLAY 'CADASTRAR OUTRO FILME?(S/N)' AT LINE 22 COLUMN 10
           ACCEPT WRK-CONTINUA-CADASTRO AT LINE 22 COLUMN 40
           END-PERFORM.
           PERFORM 1100-MONTATELA.

      * -----ROTINA PARA CONSULTAR UM FILME----------------------
       6000-CONSULTA.
           MOVE 'MODULO - CONSULTA' TO WRK-MODULO.
           DISPLAY TELA.
           DISPLAY TELA-REGISTRO.
           ACCEPT CHAVE.
               READ FILMES
                   INVALID KEY
                    MOVE 'FILME NAO ENCONTRADO.ENTER SAIR'
                           TO WRK-MSGERRO
                    NOT INVALID KEY
                    MOVE 'FILME ENCONTRADO.ENTER SAIR' TO WRK-MSGERRO
                    DISPLAY SS-DADOS
               END-READ.
           PERFORM 9999-MOSTRAR-ERRO.

      * -----ROTINA PARA ALTERAR OS DADOS DE UM FILME----------
       7000-ALTERACAO.
           MOVE 'MODULO - ALTERAR'        TO WRK-MODULO.
           MOVE SPACES                    TO WRK-MSGERRO.
           DISPLAY TELA.
           DISPLAY 'INFORME O CODIGO DO FILME: ' LINE 05 COL 10.
           ACCEPT CHAVE LINE 05 COL 32.

           READ FILMES
               INVALID KEY
               MOVE 'FILME NAO ENCONTRADO' TO WRK-MSGERRO
               PERFORM 9999-MOSTRAR-ERRO
                   NOT INVALID KEY
                   MOVE 'FILME ENCONTRADO. ENTER PARA OS NOVOS DADOS'
                   TO WRK-MSGERRO
                   PERFORM 9999-MOSTRAR-ERRO

                   MOVE TITULO        TO WRK-TITULO
                   MOVE GENERO        TO WRK-GENERO
                   MOVE DURACAO       TO WRK-DURACAO
                   MOVE DISTRIBUIDORA TO WRK-DISTRIBUIDORA
                   MOVE NOTA          TO WRK-NOTA

                   DISPLAY 'TITULO......: '        LINE 7  COL 10
                   DISPLAY WRK-TITULO              LINE 7  COL 30
                   ACCEPT WRK-TITULO               LINE 7  COL 30

                   DISPLAY 'GENERO......: '        LINE 8  COL 10
                   DISPLAY WRK-GENERO              LINE 8  COL 30
                   ACCEPT WRK-GENERO               LINE 8  COL 30

                   DISPLAY 'DURACAO.....: '        LINE 9  COL 10
                   DISPLAY WRK-DURACAO             LINE 9  COL 30
                   ACCEPT WRK-DURACAO              LINE 9  COL 30

                   DISPLAY 'DISTRIBUIDORA: '       LINE 10 COL 10
                   DISPLAY WRK-DISTRIBUIDORA       LINE 10 COL 30
                   ACCEPT WRK-DISTRIBUIDORA        LINE 10 COL 30

                   DISPLAY 'NOTA........: '        LINE 11 COL 10
                   DISPLAY WRK-NOTA                LINE 11 COL 30
                   ACCEPT WRK-NOTA                 LINE 11 COL 30

                   DISPLAY 'DESEJA CONFIRMAR A ALTERACAO? (S/N): '
                            LINE 16 COL 10
                   ACCEPT WRK-TECLA LINE 16 COL 50

                   IF WRK-TECLA = 'S' OR WRK-TECLA = 's'
                       MOVE WRK-TITULO        TO TITULO
                       MOVE WRK-GENERO        TO GENERO
                       MOVE WRK-DURACAO       TO DURACAO
                       MOVE WRK-DISTRIBUIDORA TO DISTRIBUIDORA
                       MOVE WRK-NOTA          TO NOTA
                       REWRITE FILMES-REG
                           INVALID KEY
                           MOVE 'REGISTRO NAO ALTERADO. ENTER'
                           TO WRK-MSGERRO
                           NOT INVALID KEY
                           MOVE 'REGISTRO ALTERADO. ENTER'
                                TO WRK-MSGERRO
                       END-REWRITE
                       PERFORM 9999-MOSTRAR-ERRO
                   ELSE
                       MOVE 'REGISTRO NAO ALTERADO. ENTER'
                             TO WRK-MSGERRO
                       PERFORM 9999-MOSTRAR-ERRO
                   END-IF
           END-READ.

      * -----ROTINA PARA EXCLUIR UM FILME------------------
       8000-EXCLUSAO.
           MOVE 'MODULO - EXCLUSAO' TO WRK-MODULO.
           MOVE SPACES TO WRK-MSGERRO.
           DISPLAY TELA.
           DISPLAY 'INFORME O CODIGO DO FILME PARA EXCLUSAO: '
                   LINE 05 COL 10.
           ACCEPT CHAVE LINE 05 COL 52.
           READ FILMES
               INVALID KEY
                   MOVE 'FILME NAO CADASTRADO. ENTER' TO WRK-MSGERRO
                   PERFORM 9999-MOSTRAR-ERRO
               NOT INVALID KEY
                   DISPLAY TELA
                   DISPLAY SS-DADOS
                   DISPLAY 'DESEJA EXCLUIR? (S/N)' AT LINE 20 COLUMN 10
                   ACCEPT WRK-TECLA LINE 20 COL 35
                   IF WRK-TECLA = 'S' OR WRK-TECLA = 's'
                       DELETE FILMES
                           INVALID KEY
                               MOVE 'ERRO AO EXCLUIR FILME. ENTER'
                                     TO WRK-MSGERRO
                           NOT INVALID KEY
                               MOVE 'FILME EXCLUIDO. ENTER'
                                     TO WRK-MSGERRO
                       END-DELETE
                       PERFORM 9999-MOSTRAR-ERRO
                   ELSE
                       MOVE 'FILME NAO EXCLUIDO. ENTER' TO WRK-MSGERRO
                       PERFORM 9999-MOSTRAR-ERRO
                   END-IF
           END-READ.

      * -----ROTINA PARA GERAR RELATORIO EM TELA----------------
       9000-RELACAOTELA.
           MOVE 'MODULO - RELATORIO TELA' TO WRK-MODULO.
           MOVE ZEROS TO
           CODFILME WRK-CONTALINHA WRK-QTREGISTROS WRK-NUMPAGINA.
           MOVE 1 TO WRK-NUMPAGINA.
           MOVE 3 TO WRK-CONTALINHA.

           DISPLAY TELA.
           DISPLAY 'RELATORIO DE FILMES - PAGINA:' AT LINE 02 COLUMN 80
                    ERASE EOL BACKGROUND-COLOR 1.
           DISPLAY WRK-NUMPAGINA AT LINE 02 COLUMN 110
                    ERASE EOL BACKGROUND-COLOR 1.

           START FILMES KEY >= CODFILME
               INVALID KEY
               MOVE 'NENHUM REGISTRO ENCONTRADO' TO WRK-MSGERRO
               PERFORM 9999-MOSTRAR-ERRO
               NOT INVALID KEY
               READ FILMES NEXT
                   PERFORM UNTIL FILMES-STATUS = 10
                       ADD 1 TO WRK-QTREGISTROS
                       MOVE SPACES TO WRK-SEQUENCIA
                        STRING
                            'COD: '          DELIMITED BY SIZE
                            CODFILME         DELIMITED BY SIZE
                            ' - '            DELIMITED BY SIZE
                            TITULO           DELIMITED BY SIZE
                            ' / '            DELIMITED BY SIZE
                            GENERO           DELIMITED BY SIZE
                            ' / '            DELIMITED BY SIZE
                            DURACAO          DELIMITED BY SIZE
                            ' min / '        DELIMITED BY SIZE
                            DISTRIBUIDORA    DELIMITED BY SIZE
                            ' / Nota: '      DELIMITED BY SIZE
                            NOTA             DELIMITED BY SIZE
                        INTO WRK-SEQUENCIA
                    DISPLAY WRK-SEQUENCIA LINE WRK-CONTALINHA COLUMN 01
                    ADD 1 TO WRK-CONTALINHA
                    IF WRK-CONTALINHA > 20
                        MOVE 'PROXIMA PAGINA TECLE ENTER' TO WRK-MSGERRO
                        PERFORM 9999-MOSTRAR-ERRO
                        ADD 1 TO WRK-NUMPAGINA
                        MOVE 3 TO WRK-CONTALINHA
                        DISPLAY TELA
                        DISPLAY 'RELATORIO DE FILMES - PAGINA:'
                                 AT LINE 02 COLUMN 80 ERASE EOL
                                 BACKGROUND-COLOR 1
                        DISPLAY WRK-NUMPAGINA AT LINE 02 COLUMN 110
                                 ERASE EOL BACKGROUND-COLOR 1
                    END-IF
                        READ FILMES NEXT
                   END-PERFORM
           END-START.
           MOVE SPACES TO WRK-SEQUENCIA.
           STRING
           'TOTAL DE REGISTROS: '      DELIMITED BY SIZE
           WRK-QTREGISTROS             DELIMITED BY SIZE
           INTO WRK-SEQUENCIA.
           DISPLAY WRK-SEQUENCIA LINE WRK-CONTALINHA COLUMN 01.
           ADD 1 TO WRK-CONTALINHA.
           MOVE 'FIM DO RELATORIO.ENTER SAIR' TO WRK-MSGERRO.
           PERFORM 9999-MOSTRAR-ERRO.

      * -----ROTINA PARA GERAR RELATORIO EM DISCO----------------
       9100-RELACAODISCO.
           MOVE 'MODULO - RELACAO EM DISCO' TO WRK-MODULO.
           DISPLAY TELA.
           MOVE ZEROS TO CODFILME WRK-QTREGISTROS.

           START FILMES KEY >= CODFILME
           INVALID KEY
           MOVE 'NENHUM REGISTRO ENCONTRADO' TO WRK-MSGERRO
           PERFORM 9999-MOSTRAR-ERRO
               NOT INVALID KEY
               OPEN OUTPUT RELATO
               PERFORM UNTIL FILMES-STATUS = 10
                ADD 1 TO WRK-QTREGISTROS
                MOVE SPACES TO RELATO-DADOS
                STRING
                    'COD: ' DELIMITED BY SIZE
                    CODFILME DELIMITED BY SIZE
                    ' - ' DELIMITED BY SIZE
                    TITULO DELIMITED BY SIZE
                    ' / ' DELIMITED BY SIZE
                    GENERO DELIMITED BY SIZE
                    ' / ' DELIMITED BY SIZE
                    DURACAO DELIMITED BY SIZE
                    ' min / ' DELIMITED BY SIZE
                    DISTRIBUIDORA DELIMITED BY SIZE
                    ' / Nota: ' DELIMITED BY SIZE
                    NOTA DELIMITED BY SIZE
                INTO RELATO-DADOS
                WRITE RELATO-REG
                READ FILMES NEXT
            END-PERFORM
            MOVE SPACES TO RELATO-DADOS
            STRING
                'TOTAL DE REGISTROS: ' DELIMITED BY SIZE
                WRK-QTREGISTROS        DELIMITED BY SIZE
            INTO RELATO-DADOS
            WRITE RELATO-REG
            CLOSE RELATO
            MOVE 'REGISTROS GRAVADOS:     ' TO WRK-MSGERRO
            MOVE WRK-QTREGISTROS TO WRK-MSGERRO(25:5)
            PERFORM 9999-MOSTRAR-ERRO
           END-START.

      *-----ROTINA PARA MOSTRAR OS ERROS---------------------
       9999-MOSTRAR-ERRO.
           DISPLAY MOSTRA-ERRO.
           ACCEPT WRK-TECLA LINE 24 COLUMN 30.
           MOVE SPACES TO WRK-MSGERRO WRK-TECLA WRK-SEQUENCIA.
