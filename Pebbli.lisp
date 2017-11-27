;Instituto Politécnico de Portalegre
;Escola Superior de Tecnologia e Gestão de Portalegre
;Disciplina: Inteligência Artificial
;Ano Lectivo: 2015-2016
;2ºAno - 2ºSemestre
;Projeto - Jogo Pebbli LISP
;Nome: Diogo Martins    - a14700
;Nome: Frederico Balcão - a15307
;**************************************************************************************************************************

;ler mapa(s) pelo ficheiro
(defun lePebbli (file)
  (with-open-file (stream file)
    (read stream))
)

(defun defPebbli (l)
  (setf *Pebbli-str* l)
)

(defPebbli '(
          "    +"
          "*  @#"
          "     "
          "    #"
          "     "
            )
)

;**************************************************************************************************************************
;Criar Nivel - função para colocar numa lista um conjunto de strings que irão ser lidas do ficheiro onde está o mapa (representação de simbolos)
(defun PebbliLevel2List (l)
  (cond ((null l) nil)
        (t (cons (string2List (first l)) (PebbliLevel2List (rest l))))))

(defun string2List (s)
  (string2List-aux s 0 (length s)))

(defun string2List-aux (s i tamanho)
  (cond ((= i tamanho) nil)
        (t (cons (char s i) (string2List-aux s (1+ i) tamanho)))))

;**************************************************************************************************************************
;transformar uma lista de listas numa matriz, necessario funções auxiliares para linhas e colunas
(defun listList2Matrix (ll)
  (ll2m-linha ll (make-array (list (length ll) (length (first ll)))) 0))

(defun ll2m-linha (ll m l)
  (cond ((null ll) m)
        (t (ll2m-linha (rest ll) (ll2m-coluna (first ll) m l 0) (1+ l)))))

(defun ll2m-coluna (lista m l c)
  (cond ((null lista) m)
        (t (progn (setf (aref m l c) (first lista))
             (ll2m-coluna (rest lista) m l (1+ c))))))

;imprime tabuleiro do pebbli (função auxiliar), recebe uma matriz, uma linha e uma coluna
;imprime tabuleiro do pebbli, Print-Pebbli recebe essa mesma matriz da função auxiliar
(defun print-Pebbli (m)
  (print-Pebbli-aux m (array-dimension m 0) (array-dimension m 1)))

(defun print-Pebbli-aux (m l c)
  (do ((i 0 (1+ i)))
      ((= i l))
    (do
        ((j 0 (1+ j)))
        ((= j c))
      (format t "~A" (aref m i j))
     )
    (format t "~%")
   )
)

;**************************************************************************************************************************
;Estrutura do jogo
(defstruct Pebbli
  (tabuleiro)       ; Tabuleiro
  (pecaObjectivo)   ; caixas objetivos (vermelha) (*) 
  (pecaEstrela)     ; caixa estrela (@)
  (pecasFixas)      ; caixas fixas (branca) (+)
  (pecasMoveis)     ; caixas moveis (branca) (#)
  (pecasAtrito)     ; caixas atrito (-)
) 

;; Cria estrutura do jogo vazia ;;

(defun cria-Pebbli ()
       (make-Pebbli :tabuleiro nil
                    :pecaObjectivo '()
                    :pecaEstrela '()
                    :pecasMoveis '()
                    :pecasFixas '()
                    :pecasAtrito '()))


;**************************************************************************************************************************
;get do tabuleiro (k representa o numero do tabuleiro, uma lista por exemplo...)
(defun getTabuleiro (l)
  (Pebbli-tabuleiro l))

;gets das listas das posições todas
(defun getpecaObjectivo (l)
  (Pebbli-pecaObjectivo l))

(defun getpecaEstrela (l)
  (Pebbli-pecaEstrela l))

(defun getpecasMoveis (l)
  (Pebbli-pecasMoveis l))

(defun getpecasFixas (l)
  (Pebbli-pecasFixas l))

(defun getpecasAtrito (l)
  (Pebbli-pecasAtrito l))

;**************************************************************************************************************************
;carregar tabuleiro do ficheiro PebbliL1.txt
(defun make-tabuleiro (s)
  (setf (Pebbli-tabuleiro s) (listList2Matrix (PebbliLevel2List (lePebbli "C:\\Users\\Fred\\Desktop\\Projeto Jogo IA\\mapas\\pebbli.txt")))))

;**************************************************************************************************************************
;constroi coordenadas pebbli do ficheiro 
(defun make-coordenadas (s)
  (do ((i 0 (1+ i)))
      ((= i (array-dimension (Pebbli-tabuleiro s) 0)) s)
  (do ((j 0 (1+ j)))
      ((= j (array-dimension (getTabuleiro s) 1)))
   
    (cond ((equal (aref (getTabuleiro s) i j) #\*)
           (coordenadaspecaObjectivo s i j))

          ((equal (aref (getTabuleiro s) i j) #\@)
           (coordenadaspecaEstrela s i j))
          
          ((equal (aref (getTabuleiro s) i j) #\#)
           (coordenadaspecasMoveis s i j))

          ((equal (aref (getTabuleiro s) i j) #\+)
           (coordenadaspecasFixas s i j))

          ((equal (aref (getTabuleiro s) i j) #\-)
           (coordenadaspecasAtrito s i j))))))

;**************************************************************************************************************************
;Inicia o Pebbli
(defun init-Pebbli (s)
  (progn (make-tabuleiro s)
	 (make-coordenadas s)))

;**************************************************************************************************************************
;Coordenadas das Caixas, da Caixa Estrela e da Caixa Principal
(defun coordenadaspecaObjectivo (s x y)
  (cond ((null (getpecaObjectivo s))
         (setf (Pebbli-pecaObjectivo s) (list (list x y))))
        (t (setf (Pebbli-pecaObjectivo s) (cons (list x y) (Pebbli-pecaObjectivo s))))))

(defun coordenadaspecaEstrela (s x y)
  (cond ((null (getpecaEstrela s))
         (setf (Pebbli-pecaEstrela s) (list (cons x y))))
        (t (setf (Pebbli-pecaEstrela s) (cons (cons x y) (Pebbli-pecaEstrela s)))))) ;; ALTERAR, NÃO PRECISAMOS DE FLAG NOS OBJECTIVOS

(defun coordenadaspecasMoveis (s x y)
  (cond ((null (getpecasMoveis s))
         (setf (Pebbli-pecasMoveis s) (list (list x y))))
        (t (setf (Pebbli-pecasMoveis s) (cons (list x y) (Pebbli-pecasMoveis s))))))

(defun coordenadaspecasFixas (s x y)
  (cond ((null (getpecasFixas s))
         (setf (Pebbli-pecasFixas s) (list (list x y))))
        (t (setf (Pebbli-pecasFixas s) (cons (list x y) (Pebbli-pecasFixas s))))))

(defun coordenadaspecasAtrito (s x y)
  (cond ((null (getpecasAtrito s))
         (setf (Pebbli-pecasAtrito s) (list (list x y))))
        (t (setf (Pebbli-pecasAtrito s) (cons (list x y) (Pebbli-pecasAtrito s))))))


;**************************************************************************************************************************
;verifica se uma determinada posição está livre, recebendo no l uma das getposições implementadas
(defun posicaoLivre (s l)
  (cond ((equal (aref (getTabuleiro s) (first l) (second l)) #\Space) T)
        (T nil)))

(defun comObstaculo (s l)
  (cond ((equal (aref (getTabuleiro s) (first l) (second l)) #\#) T)
        ((equal (aref (getTabuleiro s) (first l) (second l)) #\+) T)
        (T nil)))

(defun ComAtrito (s l)
  (cond ((equal (aref (getTabuleiro s) (first l) (second l)) #\-) T)
        (T nil)))

(defun encontraPecaObjectivo (s l)
  (cond ((equal (aref (getTabuleiro s) (first l) (second l)) #\*) T)
        (T nil)))

(defun encontraPecaEstrele (s l)
  (cond ((equal (aref (getTabuleiro s) (first l) (second l)) #\@) T)
	(T nil)))


;verifica se é objetivo, recebendo no l uma das getposicoes
(defun estaPosicaoEstrela (l posicao)
  (cond ((null l) nil)
	((equal (first l) (cons (first posicao) (second posicao))) T)
	(T (estaPosicaoEstrela (rest l) posicao))))

; Coloca o third a 1 para saber que podemos mover
(defun podeMover (l)
(progn (setf (third l) 1)
T))

;Faz a verificação se o third de facto pode mover
(defun podeMoverAux (l)
  (cond ((= 1 (third l)) T)
	(T nil)))

;**************************************************************************************************************************
; FUNÇÔES QUE MOVEM PARA CIMA
;**************************************************************************************************************************
;função auxiliar para inserir em cima
(defun verificaParaCima (s)
  (pecaObjectivoCima s (first (getpecaObjectivo s)) (first (getpecaObjectivo s))))


(defun insereParaCima (s p pa) 
 (progn (setCaixaObjectivo s (first p) (second p))
	(deleteCaixaObjectivo s (first pa) (second pa))
	(setf (first (first (getpecaObjectivo s))) (first p))
	(setf (second (first (getpecaObjectivo s))) (second p))
	s
	))

(defun pecaObjectivoCima (s posicao posicaoAux)
  (cond ((< (- (first posicao) 1) 0) s)
	((equal (aref (getTabuleiro s) (- (first posicao) 1) (second posicao)) #\#) 
	 (insereParaCima s posicao posicaoaux))
  ((equal (aref (getTabuleiro s) (- (first posicao) 1) (second posicao)) #\+) 
   (insereParaCima s posicao posicaoaux))
  ((equal (aref (getTabuleiro s) (- (first posicao) 1) (second posicao)) #\-) 
   (insereParaCima s posicao posicaoaux))
	(t (setf (first posicao) (- (first posicao) 1)) (pecaObjectivoCima s posicao posicaoAux))))

(defun moveParaCima (s)
  (verificaParaCima s))

       

;**************************************************************************************************************************
; FUNÇÔES QUE MOVEM PARA BAIXO
;**************************************************************************************************************************
;função auxiliar para inserir em baixo
(defun verificaParaBaixo (s)
  (pecaObjectivoBaixo s (first (getpecaObjectivo s)) (first (getpecaObjectivo s))))

(defun insereParaBaixo (s p pa)
 (progn (setCaixaObjectivo s (first p) (second p))
	(deleteCaixaObjectivo s (first pa) (second pa))
	(setf (first (first (getpecaObjectivo s))) (first p))
	(setf (second (first (getpecaObjectivo s))) (second p))
	s
	))

(defun pecaObjectivoBaixo (s posicao posicaoAux)
  (cond ((>= (+ (first posicao) 1) (first (array-dimensions (Pebbli-tabuleiro s)))) s)
	((equal (aref (getTabuleiro s) (+ (first posicao) 1) (second posicao)) #\#)
	 (insereParaBaixo s posicao posicaoaux))
	((equal (aref (getTabuleiro s) (+ (first posicao) 1) (second posicao)) #\+)
   (insereParaBaixo s posicao posicaoaux))
  ((equal (aref (getTabuleiro s) (+ (first posicao) 1) (second posicao)) #\-)
   (insereParaBaixo s posicao posicaoaux))
  (t (setf (first posicao) (+ (first posicao) 1)) (pecaObjectivoBaixo s posicao posicaoAux))))
        

(defun moveParaBaixo (s)
  (verificaParaBaixo s))

;**************************************************************************************************************************
; FUNÇÔES QUE MOVEM PARA ESQUERDA
;**************************************************************************************************************************
;função auxiliar para inserir para esquerda
(defun verificaParaEsquerda (s)
  (pecaObjectivoEsquerda s (first (getpecaObjectivo s)) (first (getpecaObjectivo s))))


(defun insereParaEsquerda (s p pa)
 (progn  (setCaixaObjectivo s (first p) (second p))
	 (deleteCaixaObjectivo s (first pa) (second pa))
	 (setf (first (first (getpecaObjectivo s))) (first p))
	 (setf (second (first (getpecaObjectivo s))) (second p))
	 s
	 ))

(defun pecaObjectivoEsquerda (s posicao posicaoAux)
  (cond ((< (- (second posicao) 1) 0) s)
	((equal (aref (getTabuleiro s) (first posicao) (- (second posicao) 1)) #\#)
	 (insereParaEsquerda s posicao posicaoaux))
  ((equal (aref (getTabuleiro s) (first posicao) (- (second posicao) 1)) #\+)
   (insereParaEsquerda s posicao posicaoaux))
  ((equal (aref (getTabuleiro s) (first posicao) (- (second posicao) 1)) #\-)
   (insereParaEsquerda s posicao posicaoaux))
	(t (setf (second posicao) (- (second posicao) 1)) (pecaObjectivoEsquerda s posicao posicaoAux))))

(defun moveParaEsquerda (s)
  (verificaParaEsquerda s))

;**************************************************************************************************************************
; FUNÇÔES QUE MOVEM PARA DIREITA
;**************************************************************************************************************************
;função auxiliar para inserir para direita
(defun verificaParaDireita (s)
  (pecaObjectivoDireita s (first (getpecaObjectivo s)) (first (getpecaObjectivo s))))

(defun insereParaDireita (s p pa)
 (progn (setCaixaObjectivo s (first p) (second p))
	(deleteCaixaObjectivo s (first pa) (second pa))
	(setf (first (first (getpecaObjectivo s))) (first p))
	(setf (second (first (getpecaObjectivo s))) (second p))
	s
	))

(defun pecaObjectivoDireita (s posicao posicaoAux)
  (cond ((>= (+ (second posicao) 1) (first (array-dimensions (Pebbli-tabuleiro s)))) s)
	((equal (aref (getTabuleiro s) (first posicao) (+ (second posicao) 1)) #\#)
	 (insereParaBaixo s posicao posicaoaux))
  ((equal (aref (getTabuleiro s) (first posicao) (+ (second posicao) 1)) #\+)
   (insereParaBaixo s posicao posicaoaux))
  ((equal (aref (getTabuleiro s) (first posicao) (+ (second posicao) 1)) #\-)
   (insereParaBaixo s posicao posicaoaux))
	(t (setf (second posicao) (+ (second posicao) 1)) (pecaObjectivoDireita s posicao posicaoAux))))

(defun moveParaDireita (s)
  (verificaParaDireita s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun setCaixaObjectivo (s x y)
  (setf (aref (Pebbli-tabuleiro s) x y) #\*))

(defun deleteCaixaObjectivo (s x y)
  (setf (aref (Pebbli-tabuleiro s) x y) #\Space))

;(defun setCaixaBranca (s x y)
;  (setf (aref (Pebbli-tabuleiro s) x y) #\#))



;**************************************************************************************************************************
;Faz uma copia da estrutura Pebbli
(defun copiaPebbli (s)
  (make-Pebbli :tabuleiro (copiaMatrizTabuleiro (getTabuleiro s))
	       :pecaObjectivo (copiaCaixas (getpecaObjectivo s))
	       :pecaEstrela (copiaLista (getpecaEstrela s))
	       :pecasFixas (copiaCaixas (getpecasFixas s))
	       :pecasMoveis (copiaCaixas (getpecasMoveis s))
	       :pecasAtrito (copiaCaixas (getpecasAtrito s)))
)

;**************************************************************************************************************************
;Faz uma copia de uma matriz, ou seja, faz uma copia do tabuleiro que é uma matriz
(defun copiaMatrizTabuleiro (tab)
  (let ((tab2 (make-array (list (array-dimension tab 0) (array-dimension tab 1)))))
    (do ((i 0 (1+ i)))
	((>= i (array-dimension tab 0)) tab2)
	(do ((j 0 (1+ j)))
	    ((>= j (array-dimension tab 1)) t)
	    (setf (aref tab2 i j) (aref tab i j))))))

;**************************************************************************************************************************
;Faz uma cópia de uma lista (função auxiliar)
(defun copiaLista (l)
  (cond ((null l) nil)
	((cons (first l) (copiaLista(rest l))))))

;Faz uma cópia de uma lista de listas
(defun copiaCaixas (l)
  (cond ((null l) nil)
	((listp (first l))
	 (cons (copiaLista (first l)) (copiaCaixas (rest l))))))

;**************************************************************************************************************************
(defun geraSucessores (s) ;; por exemplo, se um movimento para a esquerda não tiver efeito, este sucessor não aparecerá no output, devido ao removeIguais!!!
  (removeIguais s (list
       (list (moveParaDireita (copiaPebbli s)) 'Objectivo_Direita)
		   (list (moveParaBaixo (copiaPebbli s)) 'Objectivo_Baixo)
		   (list (moveParaCima (copiaPebbli s)) 'Objectivo_Cima)
		   (list (moveParaEsquerda (copiaPebbli s)) 'Objectivo_Esquerda)
       )))
			
;**************************************************************************************************************************
;(defun removeIguais (pai l)
 ; (cond ((null l) nil)
	;((equal (getpecaObjectivo (first (first l))) (getpecaObjectivo pai)) (removeIguais pai (rest l)))
	;(T (cons (first l) (removeIguais pai (rest l))))))

(defun removeIguais (pai l)
  (cond ((null l) nil)
  ((equal (getpecaObjectivo (first (first l))) (getpecaObjectivo pai)) (removeIguais pai (rest l))
  (equal (getpecasMoveis (first (first l))) (getpecasMoveis pai)) (removeIguais pai (rest l)))
  (T (cons (first l) (removeIguais pai (rest l))))))

;**************************************************************************************************************************
;Função que indica que uma caixa objetivo está presente numa caixa estrela       
(defun Resultado (s)
  (ResultadoAux (getpecaEstrela s) (getpecaObjectivo s)))

(defun ResultadoAux (l1 l2) ;; lista objectivos e uma pecaVermelha
  (cond ((null l2) T) 
  ((estaPosicaoEstrela l1 (first l2))
   (ResultadoAux l1 (rest l2)))
  (T nil)))

;**************************************************************************************************************************
;IMPLEMENTAÇÃO DOS ALGORITMOS PROCURA
(defstruct no
  (Pebbli)
  (pai)
  (nivel)
  (Movimento))

;**************************************************************************************************************************
;Cria a lista da solução
(defun construct-solution-path (sno)
  (cond ((null sno) nil)
	(t (cons (no-Movimento sno) (construct-solution-path (no-pai sno))))))

;************************************************************************************************************************** 
(defun dfs (sno limit)
  (let ((stack (list sno)))
    (do ((sno-actual (pop stack) (pop stack)))
	((or (null sno-actual) (Resultado (no-Pebbli sno-actual))) sno-actual)
	(format t "~A (~A)~%" (no-nivel sno-actual) (length stack))
	(if (< (no-nivel sno-actual) limit)
	    (setf stack (insertSucc sno-actual stack))))))

(defun insertSucc (sno stack)
  (insertListSucc (geraSucessores (no-Pebbli sno)) sno stack))

(defun insertListSucc (succList sno stack)
  (cond ((null succList) stack)
	(t (insertListSucc (rest succList) sno
			   (push (make-no :Pebbli (first (first succList))
					  :pai sno
					  :nivel (1+ (no-nivel sno))
					  :Movimento (second (first succList)))
				 stack)))))

;**************************************************************************************************************************
(defun solve-bfs (s)
  (reverse (construct-solution-path (bfs (make-no :Pebbli s
						  :pai nil
						  :nivel 0
						  :Movimento 'start)))))

(defun bfs(node)
  (let ((queue (list node)))
    (do ((node-actual (pop queue) (pop queue)))
	((Resultado (no-Pebbli node-actual)) node-actual)
	(cond ((null node-actual) node-actual))
	(format t "~A (~A)~%" (no-nivel node-actual) (length queue))
	(setf queue (addSucc (geraSucessores (no-Pebbli node-actual)) node-actual queue)))))

;**************************************************************************************************************************
;Retorna a lista de nós sucessores
(defun addSucc(listaSucc node queue)
  (cond ((null listaSucc) queue)
	(t (addSucc (rest listaSucc) node ;node = no pai que vem de cima
		    (append queue (list (make-no :Pebbli (first (first listaSucc))
						 :pai node
						 :nivel (1+ (no-nivel node))
						 :Movimento (second (first listaSucc)))))))))

;**************************************************************************************************************************
;ALGORITMOS DE PROFUNDIDADE ITERATIVO
(defun solve-deepening (s)
  (reverse (construct-solution-path (deepening s 5))))
 
(defun deepening (s limit)
  (let ((no (dfs (make-no :Pebbli s
			  :pai nil
			  :nivel 0
			  :Movimento 'start)
		 limit)))
    
    (cond ((null no) (deepening s (1+ limit)))
	  (t no))))

;**************************************************************************************************************************
;Resolução do jogo 
(defun resolve(s l)
  (cond ((null l) (print-Pebbli (getTabuleito s)))
	((equal 'Objectivo_Cima (first l)) (moveParaCima s) (resolve s (rest l)))
	((equal 'Objectivo_Baixo (first l)) (moveParaBaixo s) (resolve s (rest l)))
	((equal 'Objectivo_Esquerda (first l)) (moveParaEsquerda s) (resolve s (rest l)))
	((equal 'Objectivo_Direita (first l)) (moveParaDireita s) (resolve s (rest l)))
(T (RESOLVE s (REST l)))))

;**************************************************************************************************************************
;Execução do Jogo
(defun Jogo ()
  (let ((Pebbli (init-Pebbli (cria-Pebbli))))
  ;(solve-deepening Pebbli)))
  (solve-bfs Pebbli)))


;carregar o ficheiro lido .txt, chama a função ler mapa, lista de strings, e iguala à variavel tabuleiro da estrutura Pebbli (SETF Pebbli-Tabuleiro x) 
;cria matriz das listas, transforma uma lista de listas numa matriz
;cria lista de listas por linha, recebe uma lista, uma matriz já criada e uma linha
;cria lista de listas por coluna, recebe uma lista, uma matriz já criada, uma linha e uma coluna

;constroi coordenadas das caixas, objetivos...
;define completamente a estrutura com o make-tabuleiro x e make-coordenadas x
;carrega as coordenadas das caixas do tabuleiro da estrutura Pebbli, recebe uma variavel que representa a caixa, uma posição x, e uma posição y
;carrega as coordenadas dos objetivos do tabuleiro da estrutura Pebbli, recebe uma variavel que representa o objetivo, uma posição x, e uma posição y

;Verificações
;verifica se a posição da caixa está no objetivo pretendido, recebe uma lista de objetivos e uma lista de posições,
;   ou seja, se um objetivo de uma lista de objetivos for encontrado/seja igual (EQUAL) a uma posição de uma lista de posições, retorna True (a caixa está no objetivo)

;??
;(verifica se pode andar), ou seja, verifica se o terceiro elemento da lista está a 1, se sim retorna T, senão retorna NIL
;define que pode andar

;verifica posição livre para puder avançar, recebe o tabuleiro e uma posição (get-posição) da lista de posições, ou seja, se encontrar string # está ocupada, senão retorna NIL (livre)

;devolve as coordenadas da caixa que se pretende, recebe no l uma das get posições 

;(load "C:\\Users\\Fred\\Desktop\\Projeto Jogo IA\\Pebbli.lisp")

;(setf tab (Cria-Pebbli))

;(init-Pebbli tab)