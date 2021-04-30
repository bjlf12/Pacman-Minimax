#lang racket

(require 2htdp/universe)       
(require 2htdp/image)

(define MAX-DEPTH 9) ;; Máxima profundidad al usar el algoritmo de minimax con poda alfa-beta

;; Caracteristicas de la ventana
(define SCREEN-W 1080) ;Valor del largo de la resolución de la pantalla de la interfaz.
(define SCREEN-H 720) ;Valor del ancho de la resolución de la pantalla de la interfaz.
(define SCORE-HEIGHT    30) ;Valor del ancho del texto del texto en la interfaz.
(define SCORE-TEXT-SIZE 20) ;Valor del tamaño del texto del texto en la interfaz.
(define CELL-SIZE 25) ;; Valor del Tamaño de una celda del laberinto de juego en la interfaz.

(define TOCK 0.28)  ;; Valor con la duración de cada tick del juego.

;;Valores de las celdas de la matriz del juego
(define E "empty") ;; Representa una celda vacía en el laberinto.
(define D "dot")   ;; Representa un punto para comer en el laberinto.
(define W "wall")  ;; Representa una pared por la que no se puede pasar.
(define P "pill")  ;; Representa una pastilla usada por PacMan para poder comer fantasmas.

;; Ancho de la tabla en interfaz, se calcula segun en largo de la matriz que representa
(define (board-width board)  (* CELL-SIZE (vector-length (vector-ref board 0))))
;; Largo de la tabla en interfaz, se calcula
(define (board-height board) (* CELL-SIZE (vector-length board)))

;Configuraciones del juego
(define INIT-MAZE 1) ; Número de laberinto inicial

(define FINAL-MAZE 3) ; Número de laberinto final

(define INIT-LIVES-COUNT 2) ; Cantidad inicial de vidas que tiene el PacMan

(define ADD-LIVES-PER-MAZE 3) ; Cantidad de vidas que se le agrega al PacMan cada vez que este pase al siguiente nivel.

(define INIT-SCORE  0) ; Valor inicial que tendra el mancador del juego

(define INIT-TIME 0) ; Valor inicial que tendra el tiempo del juego

(define DEFAULT-SPAWN-TIME 3) ; Valor con la cantidad de tick que debe esperar un fantasma para reaparecer cuando este es comido.

(define PILL-STATE-START-TICKS 15) ;; Valor con la cantidad de tick en la que durara el efecto de comer fantasmas por parte de pacman

(define ghost-slowness 2) ; Valor utilizado para disminuir la velocidad de los fantasmas. Entre más alto el número más se acerca a la velocidad de pacman estaba en 6

(define ghosts-fear -100) ; Valor que representa el miedo que tendra PacMan a los fantasmas, utilizado en la funcion eval.


#|
  Función para obtener el laberinto correspondiente a un nivel.
  @param maze: número del nivel a obtener.
  @return matriz con los valores del laberinto.
|#
(define (get-current-board maze)
  (cond [(= 1 maze) FIRST-BOARD]
        [(= 2 maze) SECOND-BOARD]
        [else THIRD-BOARD]))


#|
  Función para obtener el PacMan correspondiente a un nivel.
  @param maze: número del nivel a obtener.
  @return estructura de tipo pacman correspondiente al nivel
|#
(define (get-current-pacman maze)
  (cond [(= 1 maze) FIRST-PACMAN]
        [(= 2 maze) SECOND-PACMAN]
        [else THIRD-PACMAN]))


#|
  Función para obtener el primer fantasma correspondiente a un nivel.
  @param maze: número del nivel a obtener.
  @return estructura de tipo fantasma correspondiente al nivel
|#
(define (get-current-ghost1 maze)
  (cond [(= 1 maze) FIRST-GHOST1]
        [(= 2 maze) SECOND-GHOST1]
        [else THIRD-GHOST1]))


#|
  Función para obtener el segundo fantasma correspondiente a un nivel.
  @param maze: número del nivel a obtener.
  @return estructura de tipo fantasma correspondiente al nivel
|#
(define (get-current-ghost2 maze)
  (cond [(= 1 maze) FIRST-GHOST2]
        [(= 2 maze) SECOND-GHOST2]
        [else THIRD-GHOST2]))


#|
  Función para obtener la cantidad de dots o puntos del laberinto correspondiente a un nivel.
  @param maze: número del nivel a obtener.
  @return entero con la candida de dots presentes en un nivel.
|#
(define (get-current-board-dots maze)
  (cond [(= 1 maze) FIRST-DOTS-QUANTITY]
        [(= 2 maze) SECOND-DOTS-QUANTITY]
        [else THIRD-DOTS-QUANTITY]))

#|------------------------------------------------------------------------------------------------------------------------------------------------|#

;; Parte del programa que se encarga de manejar las posiciones de los agentes en el tablero de juego.

;; Estructura con atributos x, y, con los cuales representamos las posiciones de los agentes en el juego.
(define-struct position (x y) #:transparent)

;; Función que comprueba si dos posiciones son la misma.
;; @param pos1 primera posición del tablero.
;; @param pos2 segunda posición del tablero.
;; @return #t si las posiciones son equivalentes, #f en caso contrario.
(define (equal-position? pos1 pos2)
  (if (and (equal? (position-x pos1) (position-x pos2)) (equal? (position-y pos1) (position-y pos2))) #t #f))

;; Se brinda la posición inmediatamente siguiente de una dada. Ya sea en una posición arriba, abajo, izquierda o derecha.
(define (upward    p) (make-position       (position-x p)  (sub1 (position-y p))))
(define (downward  p) (make-position       (position-x p)  (add1 (position-y p))))
(define (leftward  p) (make-position (sub1 (position-x p))       (position-y p)))
(define (rightward p) (make-position (add1 (position-x p))       (position-y p)))

#|------------------------------------------------------------------------------------------------------------------------------------------------|#

#|
  Estructura utilizada para representar los atributos de PacMan en el programa. Contiene:
    @param position estructura utilizada para representar la posición sobre el tablero de pacman, es decir
    el número de fila y columna.
    @param dir Consiste en un carácter que permite distinguir la dirección de los movimientos realizados por PacMan,  @param los valores son "U" "D" "L" "R", que son iniciales de los movimientos arriba, abajo, izquierda y derecha.
|#
(define-struct pacman (position dir) #:transparent) 


#|
  Función utilizada para que si una lista de posibles movimientos (lista de posiciones) se encuentra vaciá, retorne una con la posición actual.
    @param pos posición actual (x, y) de donde se calcula los posibles movimientos.
    @param moves lista con los posibles movimientos.
  @return lista de posiciones.
|#
(define (dont-move pos moves)
  (if (empty? moves) (list pos) moves)) ;Si la lista esta vaciá retorna la posición actual.


#|
  Función utilizada para evaluar el estado del tablero en un momento dado para el PacMan.
    @param board un estado del tablero del juego (laberinto).
    @param pacman-pos posición (x, y) de pacman sobre el tablero.
    @param ghost1-pos posición (x, y) del primer fantasma sobre el tablero.
    @param scared1 entero con el valor restante del afecto de píldora sobre el primer fantasma.
    @param ghost2-pos posición (x, y) del segundo fantasma sobre el tablero.
    @param scared2 entero con el valor restante del afecto de píldora sobre el segundo fantasma.
    @param maze número de nivel en que se encuentra el juego.
    @param dots cantidad actual de dots o puntos en el tablero.
  @return entero con el valor actual del tablero.
|#
(define (eval board pacman-pos ghost1-pos scared1 ghost2-pos scared2 maze dots)
  ; Función auxiliar recibe como parametro "sum" para calcular el valor del tablero.
  (define (eval-aux board pacman-pos ghost1-pos scared1 ghost2-pos scared2 maze dots sum)
    (for ([i (in-range 0 (vector-length board))]) ; Se recorre en tablero de juego (matriz) celda por celda
      ; Se le suma a "sum" el valor del campo de la celda segun su contenido y se multiplica por el valor de la distancia de manhattan
      (for ([j (in-range 0 (vector-length (vector-ref board i)))])
        (set! sum (+ sum (* (manhattan-value maze pacman-pos (make-position j i)) (get-value maze (board-ref board j i) dots))))))
    ; Se agrega a "sum" el miedo del fantasma, que si se encuentra en estado de píldora o asustado es:
    ; la multiplicación de la distancia de manhattan por el valor de miedo de los fantasmas 1000 veces,
    ; sino se encuentra en estado de píldora o asustado es la distancia de manhattan por el miedo de PacMan a los fantasmas.
    ; Esto para el primer y segundo fantasma respectivamente.
    (set! sum (+ sum (* (manhattan-value maze pacman-pos ghost1-pos) (if (zero? scared1) ghosts-fear (* -1500 ghosts-fear)))))
    (set! sum (+ sum (* (manhattan-value maze pacman-pos ghost2-pos) (if (zero? scared2) ghosts-fear (* -1500 ghosts-fear)))))
    sum)
  (eval-aux board pacman-pos ghost1-pos scared1 ghost2-pos scared2 maze dots 0))


(define MAXINF 9999999999) ; Valor utilizado para representar el infinito.
(define MININF -9999999999) ; Valor utilizado para representar el infinito negativo.

(define INIT-DEPTH 0); Valor utilizado para represetar la profundida inicial del MiniMax

#|
  Función utilizada para sumar el valor de las celdas que son recorridas en el minimax.
    @param current-sum entero con el valor actual de la ruta.
    @param pos posición actual del pacman en el tablero.
    @param board tablero actual del juego.
    @param maze número de laberinto actual.
    @param ghost1-pos posición (x, y) del primer fantasma sobre el tablero.
    @param scared1 entero con el valor restante del afecto de píldora sobre el primer fantasma.
    @param ghost2-pos posición (x, y) del segundo fantasma sobre el tablero.
    @param scared2 entero con el valor restante del afecto de píldora sobre el segundo fantasma.
    @param dots cantidad de dots actual en el tablero.
  @return Entero con la suma de current-sum + el valor de la celda.
|#
(define (sum-cell current-sum pos board maze dots ghost1-pos scared1 ghost2-pos scared2)
  (define cell (board-ref board (position-x pos) (position-y pos)))
  (define eaten1 (if (and scared1 (equal-position? pos ghost1-pos)) 9999999 0))
  (define eaten2 (if (and scared2 (equal-position? pos ghost2-pos)) 9999999 0))
  (cond
    [(string=? cell "empty") (+ current-sum -50 eaten1 eaten2)]
    [(string=? cell "wall") (+ current-sum 0 eaten1 eaten2)]
    [(string=? cell "dot") (+ current-sum (* (- 1 (/ dots (get-current-board-dots maze))) 999999) eaten1 eaten2)]
    [(string=? cell "pill") (+ current-sum 180 eaten1 eaten2)]))

#|
  Función minimax por la que se guía Pac-Man para realizar sus movimientos.
    @param current-game-state estructura de dónde se obtendrán todos los datos para definir el siguiente movimiento.
  @return Siguiente movimiento de Pac-Man.
|#
(define (minimax current-game-state)

  #|
    Función auxiliar del minimax, se utiliza para calcular los movimientos del pacman.
    @param board tablero actual del tablero.
    @param board un estado del tablero del juego (laberinto).
    @param pacman-pos posición (x, y) de pacman sobre el tablero.
    @param ghost1-pos posición (x, y) del primer fantasma sobre el tablero.
    @param scared1 entero con el valor restante del afecto de píldora sobre el primer fantasma.
    @param ghost2-pos posición (x, y) del segundo fantasma sobre el tablero.
    @param scared2 entero con el valor restante del afecto de píldora sobre el segundo fantasma.
    @param maze número de nivel en que se encuentra el juego.
    @param dots cantidad actual de dots o puntos en el tablero.
    @param current-depth profundidad actual, se utiliza para detenerse cuando sea igual a MAX-DEPTH.
    @param path-sum suma actual de la ruta elegida por el pacman.
    @param alpha valor mínimo calculado en el árbol del minimax.
    @param beta valor máximo calculado en el árbol del minimax.
    @return un entero con el valor evaluado del estado del tablero.
  |#
  (define (max-aux board pacman-pos ghost1-pos scared1 ghost2-pos scared2 maze dots current-depth path-sum alpha beta)
    ; Se valida si el movimiento es valido y si no se llego a la profundidad maxima.
    (cond [(and (> MAX-DEPTH current-depth) (not (goal? pacman-pos ghost1-pos scared1 ghost2-pos scared2)))
        (let ([best-val MININF])
                  ; Para todos los posibles movimientos del PacMan se calcula cual es el mejor para los siguientes movimientos, tomando en cuenta a los adversarios.
                 (for ([i (get-pacman-moves board pacman-pos ghost1-pos scared1 ghost2-pos scared2)])
                   (let ([val (min-aux (new-board-empty-at (position-x i) (position-y i) board) i ghost1-pos 
                    (if (string=? "pill" (board-ref board (position-x i) (position-y i))) PILL-STATE-START-TICKS 
                      (if (not (zero? scared1)) (sub1 scared1) scared1)) ghost2-pos 
                    (if (string=? "pill" (board-ref board (position-x i) (position-y i))) PILL-STATE-START-TICKS 
                      (if (not (zero? scared2)) (sub1 scared2) scared2)) maze (if (string=? "dot" (board-ref board (position-x i) (position-y i))) (sub1 dots) dots) 
                    (+ current-depth 1) (sum-cell path-sum i board maze dots ghost1-pos (not (zero? scared1)) ghost2-pos (not (zero? scared2))) #t alpha beta)])
                   (set! best-val (if (< val best-val) best-val val))) ; si el resultado el mayor se elige.
                     #:break (>= best-val beta) ; si el valor es menor o igual a beta se termina de evaluar movimientos
                     (set! alpha (if (< alpha best-val) best-val alpha))) ; sino se actualiza el valor de alpha 
                 best-val)]
          ; si el movimiento no es valido o ya no hay más profundidad se evalua el estado actual del tablero
        [else (+ path-sum (eval board pacman-pos ghost1-pos scared1 ghost2-pos scared1 maze dots))]))
  
  #|
    Función auxiliar del minimax, se utiliza para calcular los movimientos de los adversarios, los fantasmas.
    @param board tablero actual del tablero.
    @param board un estado del tablero del juego (laberinto).
    @param pacman-pos posición (x, y) de pacman sobre el tablero.
    @param ghost1-pos posición (x, y) del primer fantasma sobre el tablero.
    @param scared1 entero con el valor restante del afecto de píldora sobre el primer fantasma.
    @param ghost2-pos posición (x, y) del segundo fantasma sobre el tablero.
    @param scared2 entero con el valor restante del afecto de píldora sobre el segundo fantasma.
    @param maze número de nivel en que se encuentra el juego.
    @param dots cantidad actual de dots o puntos en el tablero.
    @param current-depth profundidad actual, se utiliza para detenerse cuando sea igual a MAX-DEPTH.
    @param path-sum suma actual de la ruta elegida por el pacman.
    @param conditional es una condisión para saber si es el turno de fantasma 1 o 2.
    @param alpha valor mínimo calculado en el árbol del minimax.
    @param beta valor máximo calculado en el árbol del minimax.
    @return una entero con el valor evaluado del estado del tablero.
  |#
  (define (min-aux board pacman-pos ghost1-pos scared1 ghost2-pos scared2 maze dots current-depth path-sum conditional alpha beta)
    ; Se valida si el movimiento es valido y si no se llego a la profundidad maxima.
    (cond [(and (> MAX-DEPTH current-depth) (not (goal? pacman-pos ghost1-pos scared1 ghost2-pos scared2)))
        (cond [conditional ; Fantasma 1
               (let ([best-val MAXINF])
                ;Para todos los posibles momimientos del fantasma 1, se evalua cual es la mejor, por lo tanto la peor para pacman.
                 (for ([i (get-ghost-moves board ghost1-pos pacman-pos (not (zero? scared1)))])
                   (let ([val (min-aux board pacman-pos i (if (not (zero? scared1)) (sub1 scared1) scared1) ghost2-pos 
                    (if (not (zero? scared2)) (sub1 scared2) scared2) maze dots (+ current-depth 1) path-sum #f alpha beta)])
                     (set! best-val (if (> val best-val) best-val val))) ; si el resultado el menor se elige.
                     #:break (<= best-val alpha) ; si el valor es mayor o igual a alpha se termina de evaluar movimientos
                     (set! beta (if (> beta best-val) best-val beta))) ; sino se actualiza el valor de beta
                 best-val)]
              [else  ; Fantasma 2
               (let ([best-val MAXINF]) 
                ;Para todos los posibles momimientos del fantasma 2, se evalua cual es la mejor, por lo tanto la peor para pacman.
                 (for ([i (get-ghost-moves board ghost2-pos pacman-pos (not (zero? scared2)))])
                   (let ([val (max-aux board pacman-pos ghost1-pos (if (not (zero? scared1)) (sub1 scared1) scared1) i 
                   (if (not (zero? scared2)) (sub1 scared2) scared2) maze dots (+ current-depth 1) path-sum alpha beta)]) ; se calcula el resultado del movimiento i.
                     (set! best-val (if (> val best-val) best-val val))) ; si el resultado el menor se elige.
                     #:break (<= best-val alpha) ; si el valor es mayor o igual a alpha se termina de evaluar movimientos
                     (set! beta (if (> beta best-val) best-val beta))); sino se actualiza el valor de beta
                 best-val)])]
    ; si el movimiento no es valido o ya no hay más profundidad se evalua el estado actual del tablero
        [else (+ path-sum (eval board pacman-pos ghost1-pos scared1 ghost2-pos scared2 maze dots))]))

  #|
    Función auxiliar del minimax, recibe los parametros necesarios en el minimax. Se evalua todas las posibles posiciones.
    @param board tablero actual del tablero.
    @param board un estado del tablero del juego (laberinto).
    @param pacman-pos posición (x, y) de pacman sobre el tablero.
    @param ghost1-pos posición (x, y) del primer fantasma sobre el tablero.
    @param scared1 entero con el valor restante del afecto de píldora sobre el primer fantasma.
    @param ghost2-pos posición (x, y) del segundo fantasma sobre el tablero.
    @param scared2 entero con el valor restante del afecto de píldora sobre el segundo fantasma.
    @param maze número de nivel en que se encuentra el juego.
    @param dots cantidad actual de dots o puntos en el tablero.
    @param current-depth profundidad actual, se utiliza para detenerse cuando sea igual a MAX-DEPTH.
    @param path-sum suma actual de la ruta elegida por el pacman.
    @return una posición (x, y).
  |#
  (define (minimax-aux board pacman-pos ghost1-pos scared1 ghost2-pos scared2 maze dots current-depth path-sum)
    (let ([best-move null]) (let ([best-val MININF])
      ; Para todos los posibles movimientos del PacMan se calcula cual es el mejor para los siguientes movimientos, tomando en cuenta a los adversarios.
      (for ([i (get-pacman-moves board pacman-pos ghost1-pos scared1 ghost2-pos scared2)])
          (let ([val (min-aux (new-board-empty-at (position-x i) (position-y i) board) i ghost1-pos 
          (if (string=? "pill" (board-ref board (position-x i) (position-y i))) PILL-STATE-START-TICKS 
          (if (not (zero? scared1)) (sub1 scared1) scared1)) ghost2-pos 
          (if (string=? "pill" (board-ref board (position-x i) (position-y i))) PILL-STATE-START-TICKS 
          (if (not (zero? scared2)) (sub1 scared2) scared2)) maze 
          (if (string=? "dot" (board-ref board (position-x i) (position-y i))) (sub1 dots) dots) (+ current-depth 1) 
          (sum-cell path-sum i board maze dots ghost1-pos (not (zero? scared1)) ghost2-pos (not (zero? scared2))) 
          #t best-val MAXINF)]) ; En caso de que el valor resultado sea mayor se elige este movimiento.
          (cond [(< best-val val) (set! best-move i) (set! best-val val)])))
      best-move)))
  ; Se llama a la función auxiliar, se obtienen los parametros del game-state.
  (minimax-aux (game-state-board current-game-state)
               (pacman-position (game-state-pacman current-game-state))
               (ghost-position (game-state-ghost1 current-game-state))
               (ghost-scared (game-state-ghost1 current-game-state))
               (ghost-position (game-state-ghost2 current-game-state))
               (ghost-scared (game-state-ghost2 current-game-state))
               (game-state-maze current-game-state)
               (game-state-dots current-game-state)
               INIT-DEPTH
               0
               ))


#|
  Función utilizada para obtener todos los posibles movimientos de PacMan en el tablero dada la posición de PacMan.
  @param board tablero actual del tablero.
  @param pos posición (x, y) actual del PacMan
  @param ghost1-pos posición (x, y) del primer fantasma sobre el tablero.
  @param scared1 entero con el valor restante del afecto de píldora sobre el primer fantasma.
  @param ghost2-pos posición (x, y) del segundo fantasma sobre el tablero.
  @param scared2 entero con el valor restante del afecto de píldora sobre el segundo fantasma.
  @return Lista de posiciones.
|#
(define (get-pacman-moves board pos ghost1-pos scared1 ghost2-pos scared2)
  (dont-move pos ; se verifica que la lista de movimientos no este vacia.
    ; se genera un filtro de las posiciones validas.
    (filter (λ (npos) (and (is-valid? board npos) (no-ghost? npos ghost1-pos scared1 ghost2-pos scared2)))
          ; se genera una lista con todos los posibles movimientos, arriba, abajo, izquierda y derecha
          (list (upward pos) (downward pos) (leftward pos) (rightward pos)))))

#|
  Función para conocer si se encuentra algun fantasma en una posición y si estos se encuentran en estado de pildora o miedo.
  @param pacman-pos posición (x, y) actual del PacMan
  @param ghost1-pos posición (x, y) del primer fantasma sobre el tablero.
  @param scared1 entero con el valor restante del afecto de píldora sobre el primer fantasma.
  @param ghost2-pos posición (x, y) del segundo fantasma sobre el tablero.
  @param scared2 entero con el valor restante del afecto de píldora sobre el segundo fantasma.
   @return verdadero si es una posición libre, falso en caso contrario.
|#
(define (no-ghost? pacman-pos ghost1-pos scared1 ghost2-pos scared2)
  ; se verifica que no haya un fantasma en la posición y si este no esta afectado por pildora
  (cond [(and (equal-position? pacman-pos ghost1-pos) (zero? scared1)) #f]
        [(and (equal-position? pacman-pos ghost2-pos) (zero? scared2)) #f]
        [else #t]))

#|
  Función para cambiar la dirección en la que se mueve el PacMan en el juego.
  @param pacman-pos posición actual del PacMan
  @param new-pacman-pos nueva posición del PacMan
  @param dir Direccion en la que se mueve actualmente el PacMan
  @return el caracter con la nueva dirección
|#
(define (change-dir pacman-pos new-pacman-pos dir)
  (cond [(not (string=? dir "C")) "C"] ; No se mueve
        [else (if (= (position-x pacman-pos) (position-x new-pacman-pos))
                  (if (< (position-y pacman-pos) (position-y new-pacman-pos)) "D" "U") ; se mueve arriba o abajo
                  (if (< (position-x pacman-pos) (position-x new-pacman-pos)) "R" "L"))])) ; se mueve derecha o izquierda

#|------------------------------------------------------------------------------------------------------------------------------------------------|#

;; Parte del programa que representa a los fantasmas en el juego.

;; Estructura encarada de almacenar la información de los fantasmas a lo largo de la ejecución de la aplicación. Poseen una posición, una dirección (representada
;; con un "char" para cada una de las posibles), un valor numérico (scared) que indica la cantidad de ticks en los que va a estar asustado el ghost y la cantidad
;; de ticks por los que estará inactivo inicialmente el ghost (spawn).
(define-struct ghost (position dir scared spawn) #:transparent)

;; Función para realizar el cálculo de la distancia de manhattan, este valor se utiliza en las diversas eurísticas para darle un valor a las diversas posiciones 
;; de los agentes.
;; @param pos1 posición del primer agente en el tablero.
;; @param pos2 posición del segundo agente o celda en el tablero.
;; @return valor de la distancia de manhattan según las posiciones dadas.
(define (manhattan-distance pos1 pos2)
  (+ (abs (- (position-x pos1) (position-x pos2))) (abs (- (position-y pos1) (position-y pos2)))))

;; Máximo valor de la distancia de manhattan dado cierto laberinto.
;; @param maze valor númerico que indica a cúal laberinto se hace referencia.
;; @return valor de la distancia de manhattan según las esquinas más alejadas.
(define (max-manhattan-distance maze)
  (manhattan-distance (make-position 0 0)
                      (make-position (- (vector-length (get-current-board maze)) 1) (- (vector-length (vector-ref (get-current-board maze) 0)) 1))))

;; Función para obtener un valor para una celda. Se utiliza la distancia de manhattan para darle más valor a las celdas cercanas y menos valor a las lejanas.
;; @param maze valor númerico que indica a cúal laberinto se hace referencia.
;; @param pos1 posición del primer agente en el tablero.
;; @param pos2 posición del segundo agente o celda en el tablero.
;; @return valor que es mayor entre más cercanos son las posiciones dadas y menor al alejarse.
(define (manhattan-value maze pos1 pos2)
  (local [(define value (manhattan-distance pos1 pos2))]
    (if (= value 0) (* 300 (- (max-manhattan-distance maze) value))
    (if (> value (/ (max-manhattan-distance maze) 3.5)) (/ (- (max-manhattan-distance maze) value) 1.5)
        (- (max-manhattan-distance maze) value)))))

;; Función para obtener los posibles movimientos de los fantasmas.
;; @param board tablero de juego actual en el que se evaluarán los movimientos.
;; @param ghost-pos posición inicial del fantasma.
;; @param pacman-pos posición actual de pacman.
;; @param is-scared booleano que indica si el fantasma se encuentra asustado de pacman.
;; @return lista de posibles movimientos que puede tomar el fantasma.
(define (get-ghost-moves board ghost-pos pacman-pos is-scared)
  (dont-move ghost-pos (filter (λ (npos) (and (is-valid? board npos) (no-pacman? npos pacman-pos (not is-scared))))
          (list (upward ghost-pos) (downward ghost-pos) (leftward ghost-pos) (rightward ghost-pos)))))

;; Se comprueba que en los movimientos del fantasma no se pueda mover hacía alguno si el fantasma está asustado y en ese lugar se encuentra el pacman.
;; @param ghost-pos nueva posición del fantasma.
;; @param pacman-pos posición actual de pacman.
;; @param not-scared booleano que indica si el fantasma no se encuentra asustado de pacman.
;; @return #t si es posible moverse a la posición, #f en caso contrario.
(define (no-pacman? ghost-pos pacman-pos not-scared)
  (cond [not-scared #t]
        [(equal-position? pacman-pos ghost-pos) #f]
        [else #t]))

;; Función para comprobar si alguno de los dos fantasmas se ha comido a pacman, tomando en cuenta si están asustado o no.
;; @param pacman-pos posición actual de pacman.
;; @param ghost1-pos posición actual del fantasma 1.
;; @param scared1 valor numérico que indica la cantidad de ticks que debe estar asustado el fantasma 1.
;; @param ghost2-pos posición actual del fantasma 2.
;; @param scared2 valor numérico que indica la cantidad de ticks que debe estar asustado el fantasma 2.
;; @return #t si los fantasmas se comieron a pacman, #f en caso contrario.
(define (goal? pacman-pos ghost1-pos scared1 ghost2-pos scared2)
  (cond [(and (zero? scared1) (equal-position? pacman-pos ghost1-pos)) #t]
        [(and (zero? scared2) (equal-position? pacman-pos ghost2-pos)) #t]
        [else #f]))

;; Método para hacer que los ghosts no se den la vuelta al momento de seguir a pacman. No pueden elegir una dirección si es contraria a la que ya están
;; siguiendo.
;; @param oldpos posición antigua del fantasma.
;; @param newpos nueva posición del fantasmas.
;; @param dir dirección antigua a la que apuntaba el fantasma.
;; @return #t en caso de que la posición no haga que el fantasma se regrese, #f en caso contrario.
(define (ghost-move-filter oldpos newpos dir)
  (cond [(and (string=? dir "D") (= (sub1 (position-y oldpos)) (position-y newpos))) #f]
        [(and (string=? dir "U") (= (add1 (position-y oldpos)) (position-y newpos))) #f]
        [(and (string=? dir "R") (= (sub1 (position-x oldpos)) (position-x newpos))) #f]
        [(and (string=? dir "L") (= (add1 (position-x oldpos)) (position-x newpos))) #f]
        [else #t]))

;; Función para obtener la dirección a la que está viendo el fantasma según la posición antigua y la actual.
;; @param oldpos posición antigua del fantasma.
;; @param newpos nueva posición del fantasmas.
;; @return nueva dirección del fantasma.
(define (get-ghost-dir oldpos newpos)
  (cond [(= (sub1 (position-y oldpos)) (position-y newpos)) "U"]
        [(= (add1 (position-y oldpos)) (position-y newpos)) "D"]
        [(= (sub1 (position-x oldpos)) (position-x newpos)) "L"]
        [else "R"]))

;; Función que calcula el movimiento que debe seguir un fantasma, se debe indicar con un booleano si se trata del primero o segundo fantasma.
;; @param current-game-state estado actual del juego de donde se obtiene la información.
;; @param is-ghost1 booleano que indica si se trata del primer fantasma.
;; @return nueva posición del fantasma.
(define (move-ghost current-game-state is-ghost1)
  (define (move-ghost-aux pacman-pos ghost-moves best-move best-value scared)
    (cond [(null? ghost-moves) best-move]
          [((if scared < >) (manhattan-distance (car ghost-moves) pacman-pos) best-value)
           (move-ghost-aux pacman-pos (cdr ghost-moves) (car ghost-moves) (manhattan-distance (car ghost-moves) pacman-pos) scared)]
          [else (move-ghost-aux pacman-pos (cdr ghost-moves) best-move best-value scared)]))
  (local [(define current-ghost (if is-ghost1 (game-state-ghost1 current-game-state) (game-state-ghost2 current-game-state)))
          (define pacman-pos (pacman-position (game-state-pacman current-game-state)))
          (define ghost-moves
            (dont-move (ghost-position current-ghost)
                       (if (zero? (ghost-scared current-ghost))
                           (filter (lambda (newpos) (ghost-move-filter (ghost-position current-ghost) newpos (ghost-dir current-ghost)))
                                   (get-ghost-moves (game-state-board current-game-state)
                                                    (ghost-position current-ghost)
                                                    pacman-pos (not (zero? (ghost-scared current-ghost)))))
                           (get-ghost-moves (game-state-board current-game-state)
                                            (ghost-position current-ghost)
                                            pacman-pos (not (zero? (ghost-scared current-ghost)))))))]
  (move-ghost-aux pacman-pos
                  (cdr ghost-moves)
                  (car ghost-moves)
                  (manhattan-distance (car ghost-moves) pacman-pos)
                  (zero? (ghost-scared current-ghost)))))

;; Función para comprobar si un fantasma fue comido por pacman.
;; @param pacman información del pacman en el juego.
;; @param ghost-pos posición actual del fantasma.
;; @param not-scared booleano que indica si el fantasma no está asustado.
;; @return #t en caso de que pacman se haga comido al fantasma, #f en caso contrario.
(define (got-eaten? pacman ghost-pos not-scared)
  (cond [not-scared #f]
        [else (equal-position? (pacman-position pacman) ghost-pos)]))

#|------------------------------------------------------------------------------------------------------------------------------------------------|#

;; Segmento del programa que se encarga de manejar el tablero de juego.

;; Definición del primer tablero/nivel a pasar.
;; A continuación se encuentran las características iniciales de pacman y los fantasmas en el primer nivel.
(define FIRST-PACMAN (make-pacman (make-position 6 6) "C"))
(define FIRST-GHOST1 (make-ghost (make-position 1 12) "R" 0 DEFAULT-SPAWN-TIME))
(define FIRST-GHOST2 (make-ghost (make-position 11 12) "L" 0 DEFAULT-SPAWN-TIME))

;; Se define el tablero que corresponde al primer nivel, junto con la cantidad de "dots" que posee.
(define FIRST-DOTS-QUANTITY 85)
(define FIRST-BOARD
  (vector (vector W W W W W W W W W W W W W)
          (vector W D D D D D D D D D D D W)
          (vector W D W D W W W W W D W D W)
          (vector W D W D W D D D W D W D W)
          (vector W D W D D D W D D D W D W)
          (vector W D W W D W W W D W W D W)
          (vector W D D D D D E D D D D D W)
          (vector W D W W D E W E D W W D W)
          (vector W D W D D D E D D D W D W)
          (vector W D W D W D W D W D W D W)
          (vector W D W D D D W D D D W D W)
          (vector W D W D W D W D W D W D W)
          (vector W D D D W D D D W D D D W)
          (vector W W W W W W W W W W W W W)))

;; Definición del segundo tablero/nivel a superar.
;; Características iniciales de pacman y los fantasmas en el segundo nivel.
(define SECOND-PACMAN (make-pacman (make-position 9 4) "C"))
(define SECOND-GHOST1 (make-ghost (make-position 8 10) "R" 0 DEFAULT-SPAWN-TIME))
(define SECOND-GHOST2 (make-ghost (make-position 10 10) "L" 0 DEFAULT-SPAWN-TIME))

;; Se define el tablero que corresponde al segundo nivel, junto con la cantidad de "dots" que posee.
(define SECOND-DOTS-QUANTITY 186)
(define SECOND-BOARD
  (vector 
    (vector W W W W W W W W W W W W W W W W W W W)
    (vector W P D D D D D D D W D D D D D D D P W)
    (vector W D W W D W W W D W D W W W D W W D W)
    (vector W D W W D W W W D W D W W W D W W D W)
    (vector W D D D D D D D D E D D D D D D D D W)
    (vector W D W W D W D W W W W W D W D W W D W)
    (vector W D D D D W D D D W D D D W D D D D W)
    (vector W D W W D W W W D W D W W W D W W D W)
    (vector W D W W D W D D D D D D D W D W W D W)
    (vector W D W W D W D W D W D W D W D W W D W)
    (vector W D D D D D D D E E E D D D D D D D W)
    (vector W D W W D W D W W E W W D W D W W D W)
    (vector W D W W D W D D D D D D D W D W W D W)
    (vector W D W W D W W W D W D W W W D W W D W)
    (vector W D D D D W D D D W D D D W D D D D W)
    (vector W D W W D W D W W W W W D W D W W D W)
    (vector W D D D D D D D D P D D D D D D D D W)
    (vector W D W W D W W W D W D W W W D W W D W)
    (vector W D W W D W W W D W D W W W D W W D W)
    (vector W P D D D D D D D W D D D D D D D P W)
    (vector W W W W W W W W W W W W W W W W W W W)))

;; Definición del tercer tablero/nivel a completar.
;; Características iniciales de pacman y los fantasmas en el tercer nivel.
(define THIRD-PACMAN (make-pacman (make-position 10 19) "C"))
(define THIRD-GHOST1 (make-ghost (make-position 8 5) "R" 0 DEFAULT-SPAWN-TIME))
(define THIRD-GHOST2 (make-ghost (make-position 12 5) "L" 0 DEFAULT-SPAWN-TIME))

;; Se define el tablero que corresponde al tercer nivel, junto con la cantidad de "dots" que posee.
(define THIRD-DOTS-QUANTITY 248)
(define THIRD-BOARD
  (vector
    (vector W W W W W W W W W W W W W W W W W W W W W)
    (vector W P D D D W D D D D D D D D D W D D D P W)
    (vector W D W W D W D W W W D W W W D W D W W D W)
    (vector W D W D D D D D D D D D D D D D D D W D W)
    (vector W D W D W W D W D W W W D W D W W D W D W)
    (vector W D W D D D D W E E E E E W D D D D W D W)
    (vector W D D D W W D W W W D W W W D W W D D D W)
    (vector W D W D D D D D D D D D D D D D D D W D W)
    (vector W D W D W W D W W D W W W W D W W D W D W)
    (vector W D W D D D D D D D D D D D D D D D W D W)
    (vector W D W W D W P W W D W D W W P W D W W D W)
    (vector W D D D D W D D D D D D D D D W D D D D W)
    (vector W D W W D W D W W W D W W W D W D W W D W)
    (vector W D W D D D D D D D D D D D D D D D W D W)
    (vector W D W D W W D W D W W W D W D W W D W D W)
    (vector W D W D D D D W D D D D D W D D D D W D W)
    (vector W D D D W W D W W W D W W W D W W D D D W)
    (vector W D W D D D D D D D D D D D D D D D W D W)
    (vector W D W D W W D W W W D W W W D W W D W D W)
    (vector W D W D D D D D D D E D D D D D D D W D W)
    (vector W D W W D W D W W W D W W W D W D W W D W)
    (vector W P D D D W D D D D D D D D D W D D D P W)
    (vector W W W W W W W W W W W W W W W W W W W W W)))

;; Función para obtener el valor que posee un tablero en cierta posición.
;; @param board laberinto del que se obtendrá una celda. 
;; @param x coordenada x del laberinto.
;; @param y coordenada y del laberinto.
;; @return valor presente en board en la posición x, y.
(define (board-ref board x y)
  (vector-ref (vector-ref board y) x))

;; Se obtiene el valor de una celda del tablero.
;; @param board laberinto del que se obtendrá una celda. 
;; @param maze número de laberinto actualmente.
;; @param cell celda del que se obtendrá un valor.
;; @param dots cantidad de dots actualmente en el laberinto.
;; @return valor de la celda correspondiente.
(define (get-value maze cell dots)
  (cond
    [(string=? cell "empty") 0]
    [(string=? cell "wall") 0]
    ;; Los dots van variando su peso dependiendo de su cantidad actual en el tablero.
    [(string=? cell "dot") (* (- 1 (/ dots (get-current-board-dots maze))) 100)]
    [(string=? cell "pill") 180]))

;; Función para comprobar si una posición en el tablero es válida (no existe una pared en ese lugar).
;; @param board laberinto en el que se comprobará si una posición es válida.
;; @param pos posición en el laberinto que se comprobará.
;; @return #t si la posición es válida, #f en caso contrario.
(define (is-valid? board pos)
  (not (is-wall? (board-ref board (position-x pos) (position-y pos)))))

;; Se comprueba si un valor corresponde a una pared.
;; @param value celda que se comprobará si es una pared
;; @return #t si la celda es una pared, #f en caso contrario.
(define (is-wall? value)
  (string=? value "wall"))

;; Función para recorrer la matriz de juego, realizar una operación con sus elementos y retoran una matriz que puede ser
;; mostrada en la interfaz gráfica de la aplicación.
;; @param function método que se le aplicará al laberinto.
;; @param board-image imagen del laberinto que cambiará dependiendo de los valores en el laberinto de juego.
;; @param board laberinto que se recorrerá.
;; @return imágen del board pasado por parámetro dependiendo de la función dada. 
(define (traverse-board function board-image board)
  (for ([i (in-range 0 (vector-length board))])
      (for ([j (in-range 0 (vector-length (vector-ref board i)))])
              (set! board-image (function (board-ref board j i) j i board-image))))
    board-image)

;; Método para contar la cantidad de "dots" que existen en un tablero en cierto momento dado.
;; @param board laberinto del que se contarán los "dots".
;; @return cantidad de "dots" en el laberinto.
(define (count-dots board)
  (define (count-dots-aux board sum)
    (for ([i (in-range 0 (vector-length board))])
      (for ([j (in-range 0 (vector-length (vector-ref board i)))])
        (set! sum (+ sum (if (string=? (board-ref board j i) D) 1 0)))))
    sum)
  (count-dots-aux board 0))

;; Función para realizar una operación "map" con un tablero y función dados.
;; @param function método con el que se realizará un "map".
;; @param board laberinto que se verá afectado por la función recibida.
(define (map-board function board)
  (build-vector (vector-length board)
                (lambda (y)
                  (build-vector (vector-length (vector-ref board y))
                                (lambda (x)
                                  (function x y (board-ref board x y)))))))

;; Función para obtener el mismo tablero recibido por parámetro pero vacío en cierta posición también dada.
;; @param empty-x valor x donde se colocará una celda vacía en el laberinto.
;; @param empty-y valor y donde se colocará una celda vacía en el laberinto.
;; @param board laberinto al que se le cambiará el valor de una celda a vacío.
;; @return mismo laberinto recibido pero con una celda vacía en una posición dada.
(define (new-board-empty-at empty-x empty-y board)
  (map-board (lambda (x y cell)
               (if (and (= x empty-x) (= y empty-y)) E cell))
             board))

#|------------------------------------------------------------------------------------------------------------------------------------------------|#

;; Segmento del programa que se encarga de manejar la interfaz gráfica de la aplicación.

;; Circulo amarillo con un triángulo negro de lado que representa la imagen de pacman en el juego. 
(define PACMAN-IMAGE (overlay/align "right" "middle" 
                          (rotate -45 (right-triangle 14 14 "solid" "black")) 
                          (circle 10 "solid" "yellow")))

(define R-PACMAN-IMAGE PACMAN-IMAGE)    ;; Pacman apuntando hacía la derecha.
(define U-PACMAN-IMAGE (rotate 90 R-PACMAN-IMAGE))    ;; Apuntando hacía arriba.
(define L-PACMAN-IMAGE (rotate 180 R-PACMAN-IMAGE))   ;; Apuntando hacía la izquierda.
(define D-PACMAN-IMAGE (rotate 270 R-PACMAN-IMAGE))   ;; Apuntando hacía abajo.
(define C-PACMAN-IMAGE (circle 10 "solid" "yellow"))  ;; Imagen de pacman con la boca cerrada.

;; Imágenes del primer fantasma (Rojo).
(define R-GHOST1-IMAGE (scale 0.5 (bitmap "images/RedRight.png")))  ;; Apuntando hacía la derecha.
(define U-GHOST1-IMAGE (scale 1.8 (bitmap "images/RedUp.png")))     ;; Apuntando hacía arriba.
(define L-GHOST1-IMAGE (scale 0.5 (bitmap "images/RedLeft.png")))   ;; Apuntando hacía la izquierda.
(define D-GHOST1-IMAGE (scale 0.5 (bitmap "images/RedDown.png")))   ;; Apuntando hacía abajo.

;; Imágenes del segundo fantasma (Naranja).
(define R-GHOST2-IMAGE (scale 0.5 (bitmap "images/OrangeRight.png"))) ;; Apuntando hacía la derecha.
(define U-GHOST2-IMAGE (scale 1.8 (bitmap "images/OrangeUp.png")))    ;; Apuntando hacía arriba.
(define L-GHOST2-IMAGE (scale 0.5 (bitmap "images/OrangeLeft.png")))  ;; Apuntando hacía la izquierda.
(define D-GHOST2-IMAGE (scale 0.5 (bitmap "images/OrangeDown.png")))  ;; Apuntando hacía abajo.

;; Imágenes que se muestran cuándo los fantasmas están asustados. 
(define SCARED-GHOST-IMAGE1 (scale 0.5 (bitmap "images/GhostFear1.png")))
(define SCARED-GHOST-IMAGE2 (scale 0.5 (bitmap "images/GhostFear2.png")))

;; Figuras que representan a los diversos elementos en el tablero de juego.
(define EMPTY-IMAGE  (rectangle CELL-SIZE CELL-SIZE "solid" "black")) ; Celda vacía
(define DOT-IMAGE  (overlay (circle 3  "solid" "white") EMPTY-IMAGE)) ; Una "dot" encima de la celda vacía.
(define WALL-IMAGE (rectangle CELL-SIZE CELL-SIZE "solid" "royalblue"))  ; Una pared en el laberinto
(define PILL-IMAGE  (overlay (circle 6  "solid" "lightyellow") EMPTY-IMAGE)) ; Una pildora encima de la celda vacía

;; Función que retorna un tablero imágen a partir del tablero de juego.
;; @param board tablero de juego en el momento actual.
;; @return escena vacía del mismo tamaño que el tablero recibido.
(define (BACKGROUND-IMAGE board)
  (empty-scene (board-width board)
               (+ (board-height board) (* 2 SCORE-HEIGHT))
               "black"))

;; Método que recupera una imágen para una celda.
;; @param cell celda del tablero.
;; @return imagen que corresponde al valor de la celda recibida.
(define (cell-image cell)
  (cond [(string=? cell "empty") EMPTY-IMAGE] 
        [(string=? cell "dot")   DOT-IMAGE]
        [(string=? cell "wall")  WALL-IMAGE]
        [(string=? cell "pill") PILL-IMAGE]))

;; Función que coloca una imagen en el tablero de la interfaz gráfica.
;; @param cell-img imágen que corresponde a la celda a colocar.
;; @param x posición x donde debe ser colocada.
;; @param y valor y donde debe ser colocada.
;; @param board-image escena del tablero donde será colocada.
;; @return imágen del tablero con la imágen de la celda colocada.
(define (place-cell-image cell-img x y board-image)
  (place-image cell-img
               (+ (* x CELL-SIZE) (/ CELL-SIZE 2))
               (+ (* y CELL-SIZE) (/ CELL-SIZE 2))
               board-image))

;; Función para renderizar en pantalla el tablero de juego.
;; @param board tablero que será mostrado en la escena.
;; @return escena con el tablero colocado.
(define (render-board board)
  (traverse-board (lambda (cell-value x y board-image)
                    (place-cell-image (cell-image cell-value) x y board-image))
                  (BACKGROUND-IMAGE board) board))

;; Función para mostrar en pantalla el tiempo (en ticks) que se lleva jugando.
;; @param time tiempo (contador de ticks) que será mostrado en la ventana.
;; @param board tablero de juego del que se tomarán las medidas para colocar correctamente el tiempo.
;; @param image escena donde se colocará el tiempo.
;; @return escena con el tiempo colocado.
(define (render-time time board image)
  (local [(define time-text
            (text (string-append "Tiempo: " (number->string time)) SCORE-TEXT-SIZE "royalblue"))]
    (place-image time-text
                 (/ (board-width board) 2)
                 (+ 5 (board-height board) SCORE-HEIGHT)
                 image)))

;; Método para mostrar a ambos fantasmas en la ventana de juego.
;; @param ghost1 información del primer fantasma que será colocado.
;; @param ghost2 información del segundo fantasma que será colocado.
;; @param image escena donde se colocarán los fantasmas.
;; @param time cantidad de ticks que han pasados, es utilizado para definir cuál de las dos imágenes de fantasma asustado
;;        se utilizará.
;; @return escena con los fantasmas colocados.
(define (render-ghosts ghost1 ghost2 image time)
  (define current-ghost1-image (cond [(not (zero? (ghost-scared ghost1))) (if (zero? (modulo time 2)) SCARED-GHOST-IMAGE1 SCARED-GHOST-IMAGE2)]
                                     [(string=? "U" (ghost-dir ghost1)) U-GHOST1-IMAGE]
                                     [(string=? "D" (ghost-dir ghost1)) D-GHOST1-IMAGE]
                                     [(string=? "L" (ghost-dir ghost1)) L-GHOST1-IMAGE]
                                     [else R-GHOST1-IMAGE]))
  (define current-ghost2-image (cond [(not (zero? (ghost-scared ghost2))) (if (zero? (modulo time 2)) SCARED-GHOST-IMAGE2 SCARED-GHOST-IMAGE1)]
                                     [(string=? "U" (ghost-dir ghost2)) U-GHOST2-IMAGE]
                                     [(string=? "D" (ghost-dir ghost2)) D-GHOST2-IMAGE]
                                     [(string=? "L" (ghost-dir ghost2)) L-GHOST2-IMAGE]
                                     [else R-GHOST2-IMAGE]))
  (place-cell-image current-ghost2-image (position-x (ghost-position ghost2)) (position-y (ghost-position ghost2)) 
    (place-cell-image current-ghost1-image (position-x (ghost-position ghost1)) (position-y (ghost-position ghost1)) image)))

;; Función para mostrar a pacman en el juego, dependiendo de hacía donde está mirando y su posición.
;; @param pacman datos del pacman a colocar
;; @param image escena donde se colocará el pacman.
;; @return escena con el pacman colocado.
(define (render-pacman pacman image)
  (local [(define PACMAN
            (cond [(string=? "U" (pacman-dir pacman)) U-PACMAN-IMAGE]
                  [(string=? "D" (pacman-dir pacman)) D-PACMAN-IMAGE]
                  [(string=? "L" (pacman-dir pacman)) L-PACMAN-IMAGE]
                  [(string=? "R" (pacman-dir pacman)) R-PACMAN-IMAGE]
                  [else C-PACMAN-IMAGE]))]
    (place-cell-image PACMAN (position-x (pacman-position pacman)) (position-y (pacman-position pacman)) image)))

;; Función para mostrar en pantalla la puntuación que lleva pacman en el juego actual.
;; @param score puntuación que será mostrada en la ventana.
;; @param board tablero de juego del que se tomarán las medidas para colocar correctamente la puntuación.
;; @param image escena donde se colocará la puntuación.
;; @return escena con la puntuación colocada.
(define (render-score score board image)
  (local [(define score-text
            (text (string-append "Marcador: " (number->string score)) SCORE-TEXT-SIZE "royalblue"))]
    (place-image score-text
                 (/ (board-width board) 2)
                 (+ (board-height board) (/ SCORE-HEIGHT 2))
                 image)))

;; Método por el cual se muestran las vidas actuales de pacman.
;; @param lives cantidad de vidas de pacman actualmente.
;; @param board tablero de juego del que se tomarán las medidas para colocar correctamente las vidas.
;; @param image escena donde se colocará la cantidad de vidas.
;; @return escena mostrando las vidas.
(define (render-lives lives board image)
  (local [(define score-text
            (text (string-append "Vidas: " (number->string lives)) SCORE-TEXT-SIZE "lime"))]
    (place-image score-text
                 35
                 (+ 5 (board-height board) SCORE-HEIGHT)
                 image)))

;; Función para indicar en la interfaz gráfica por cual laberinto vamos.
;; @param maze valor numérico que indica por cuál laberinto estamos jugando.
;; @param board tablero de juego del que se tomarán las medidas para colocar correctamente las vidas.
;; @param image escena donde se colocará el número de laberinto actual.
;; @return escena con el número de laberinto colocado.
(define (render-maze maze board image)
  (local [(define score-text
            (text (string-append "Nivel: " (number->string maze)) SCORE-TEXT-SIZE "lime"))]
    (place-image score-text
                 35
                 (+ (board-height board) (/ SCORE-HEIGHT 2))
                 image)))

;; Función que crea la interfaz gráfica al llamar a las demás funciones.
;; @param current-game-state estructura que almacena toda la información del juego y que se seguirá para mostrar la interfaz.
;; @return escena con toda la información colocada.
(define (render current-game-state)
  (overlay/align "middle"
                 "center"
                 (above/align "middle"
                              (text "MINIMAX CYBERPACMAN 2077" 20 "purple")
                              (render-time (ceiling (* 0.25 (game-state-time current-game-state)))
                                           (game-state-board current-game-state)
                                           (render-ghosts (game-state-ghost1 current-game-state)
                                                          (game-state-ghost2 current-game-state)
                                                         (render-pacman (game-state-pacman current-game-state)
                                                                    (render-score (game-state-score current-game-state)
                                                                                  (game-state-board current-game-state)
                                                                                  (render-lives (game-state-lives current-game-state) (game-state-board current-game-state)
                                                                                                (render-maze (game-state-maze current-game-state) (game-state-board current-game-state)
                                                                                                             (game-state-board-image current-game-state)))))
                                                         (game-state-time current-game-state))))
                 (empty-scene SCREEN-W SCREEN-H "black")))

;; Pantalla que se mostrará una vez pacman ha finalizado el juego o ha perdido todas sus vidas.
;; @param current-game-state estructura que almacena toda la información del juego y que se seguirá para mostrar la interfaz.
;; @return escena con el texto que corresponde colocado.
(define (last-scene current-game-state)
  ;; Se comprueba si pacman ha sido comido por algún fantasma.
  (cond [(goal? (pacman-position (game-state-pacman current-game-state))
                (ghost-position (game-state-ghost1 current-game-state))
                (ghost-scared (game-state-ghost1 current-game-state))
                (ghost-position (game-state-ghost2 current-game-state))
                (ghost-scared (game-state-ghost2 current-game-state)))
          ;; Texto que se muestra si ha perdido.
          (overlay/align "middle" "middle" 
                        (above (text "JUEGO FINALIZADO, SE COMIERON A PACMAN." 20 "crimson")
                               (text "¡Has fallado en el CyberPacman 2077!" 20 "crimson")
                               (text "Tiempo Total:" 20 "royalblue")
                               (text (number->string (ceiling (* TOCK (game-state-time current-game-state)))) 20 "royalblue")
                               (text "Puntuación Total:" 20 "gold")
                               (text (number->string (game-state-score current-game-state)) 20 "gold")
                               (text "Puntuación Final:" 20 "royalblue")
                               (text (number->string (final-score (ceiling (* TOCK (game-state-time current-game-state))) (game-state-score current-game-state))) 20 "royalblue")
                               (text "Nivel Alcanzado:" 20 "gold")
                               (text (number->string (game-state-maze current-game-state)) 20 "gold"))
                        (empty-scene SCREEN-W SCREEN-H "black"))]    
        ;; Texto para cuando gane pacman.   
        [else (overlay/align "middle" "middle" 
                             (above (text "PACMAN TERMINÓ TODOS LOS NIVELES." 20 "white")
                                    (text "¡Has superado el CyberPacman 2077!" 20 "white")
                                    (text "Tiempo Total:" 20 "royalblue")
                                    (text (number->string (ceiling (* TOCK (game-state-time current-game-state)))) 20 "royalblue")
                                    (text "Puntuación Total:" 20 "gold")
                                    (text (number->string (game-state-score current-game-state)) 20 "gold")
                                    (text "Puntuación Final:" 20 "royalblue")
                                    (text (number->string (final-score (ceiling (* TOCK (game-state-time current-game-state))) (game-state-score current-game-state))) 20 "royalblue")
                                    (text "Vidas Finales:" 20 "gold")
                                    (text (number->string (- (game-state-lives current-game-state) 3)) 20 "gold"))
                             (empty-scene SCREEN-W SCREEN-H "black"))]))

#|------------------------------------------------------------------------------------------------------------------------------------------------|#

#|
  Función utilizada para quitar una vida vida al pacman durante el juego, se encarga de actualizar los valores 
  correspondientes en el game-state.
    @param current-game-state: estado del juego actual sin modificar.
  @return un game-state con los valores actualizados.
|#
(define (lost-live current-game-state)
  ; Se crea un nuevo game state con los valrores actualizados
  (make-game-state (get-current-pacman (game-state-maze current-game-state)) (game-state-board current-game-state) (game-state-board-image current-game-state)
                   (if (< (game-state-score current-game-state) 100) 0 (- (game-state-score current-game-state) 100)) ; se resta 100 al marcador, si este es menor de 100 se deja en 0
                   (game-state-time current-game-state) (get-current-ghost1 (game-state-maze current-game-state)) (get-current-ghost2 (game-state-maze current-game-state)) (sub1 (game-state-lives current-game-state)) (game-state-maze current-game-state) (game-state-dots current-game-state))); se resta una vida


#|
  Función utilizada para pasar al siguiente nivel, para actualizar el laberinto, las vidas y el score, asi como cargar los demas 
  componenetes del game-state.
    @param current-game-state: estado actual del juego sin modificar.
  @return un estado de juego modificado.
|#
(define (next-maze current-game-state)
  (define current-maze (add1 (game-state-maze current-game-state)))
  ; se obtiene pacman, board, fantasmas, dots y la imagen del fondo del board
  (make-game-state (get-current-pacman current-maze) (get-current-board current-maze) (render-board (get-current-board current-maze))
                   (+ (game-state-score current-game-state) 300) ; se aumenta el marcador a + 300
                   (game-state-time current-game-state) (get-current-ghost1 current-maze) (get-current-ghost2 current-maze) (+ (game-state-lives current-game-state) ADD-LIVES-PER-MAZE) ; se agregan las vidas
                   current-maze (get-current-board-dots current-maze)))


#|
  Función utilizada en un tick para actualizar los valores de pacman.
    @param current-game-state: estado actual del juego sin modificar pacman.
  @return pacman en su siguiente movimiento.
|#
(define (tick-pacman current-game-state)
  (define new-pacman-pos (minimax current-game-state))
  (make-pacman new-pacman-pos (change-dir (pacman-position (game-state-pacman current-game-state))
                            new-pacman-pos (pacman-dir (game-state-pacman current-game-state)))))


#|
  Función utilizada para manejar el estado de pildora en el que pacman puede comer fantasmas.
    @param current-pill-state:
    @param pacman: estructura de pacman con posición (x, y)
    @param board: tablero con el estado actual del juego.
  @return entero con la cantidad de tick de comer fantasma.
|#
(define (tick-pill-state current-pill-state pacman board)
  (local [(define x (position-x (pacman-position pacman)))
          (define y (position-y (pacman-position pacman)))]
  (if (string=? (board-ref board x y) "pill") PILL-STATE-START-TICKS ; Sucede solo si el pacman esta sobre una pildora
      (if (zero? current-pill-state) 0 (sub1 current-pill-state)))))


#|
  Función utilizada para actualizar al fantasma 1 por cada tick del juego, actualiza su posición y el estado de miedo.
    @param current-game-state: estado actual del juego.
  @return el fantasma actualizado con nueva posición, dirección y estado de miedo.
|#
(define (tick-ghost1 current-game-state new-pacman)
  (define new-ghost1-pos (move-ghost current-game-state #t)) ; se actualiza la posición del pacman
  ; Se realiza una espera para volver a posicionar el fatasma en el tablero cuando muere
  (if (not (zero? (ghost-spawn (game-state-ghost1 current-game-state)))) 
    (make-ghost (ghost-position (game-state-ghost1 current-game-state)) (ghost-dir (game-state-ghost1 current-game-state)) 
      (tick-pill-state (ghost-scared (game-state-ghost1 current-game-state)) new-pacman (game-state-board current-game-state))
      (sub1 (ghost-spawn (game-state-ghost1 current-game-state))))
    ; Se actualiza el estado del fantasma si lo comen
  (if (got-eaten? (game-state-pacman current-game-state) (ghost-position (game-state-ghost1 current-game-state)) 
    (zero? (ghost-scared (game-state-ghost1 current-game-state)))) (get-current-ghost1 (game-state-maze current-game-state))
      (if (and (not (zero? (ghost-scared (game-state-ghost1 current-game-state)))) (zero? (modulo (game-state-time current-game-state) ghost-slowness))) 
        (make-ghost (ghost-position (game-state-ghost1 current-game-state)) (ghost-dir (game-state-ghost1 current-game-state)) 
          (tick-pill-state (ghost-scared (game-state-ghost1 current-game-state)) new-pacman (game-state-board current-game-state)) 0)
          (make-ghost new-ghost1-pos (get-ghost-dir (ghost-position (game-state-ghost1 current-game-state)) new-ghost1-pos) 
            (tick-pill-state (ghost-scared (game-state-ghost1 current-game-state)) new-pacman (game-state-board current-game-state)) 0)))))


#|
  Función utilizada para actualizar al fantasma 2 por cada tick del juego, actualiza su posición y el estado de miedo.
    @param current-game-state: estado actual del juego.
  @return el fantasma actualizado con nueva posición, dirección y estado de miedo.
|#
(define (tick-ghost2 current-game-state new-pacman) ; se actualiza la posición del pacman
  (define new-ghost2-pos (move-ghost current-game-state #f)) ; Se realiza una espera para volver a posicionar el fatasma en el tablero cuando muere
  ; Se realiza una espera para volver a posicionar el fatasma en el tablero cuando muere
  (if (not (zero? (ghost-spawn (game-state-ghost2 current-game-state)))) (make-ghost (ghost-position (game-state-ghost2 current-game-state)) 
    (ghost-dir (game-state-ghost2 current-game-state)) (tick-pill-state (ghost-scared (game-state-ghost2 current-game-state)) new-pacman 
      (game-state-board current-game-state)) (sub1 (ghost-spawn (game-state-ghost2 current-game-state))))
    ; Se actualiza el estado del fantasma si lo comen
  (if (got-eaten? (game-state-pacman current-game-state) (ghost-position (game-state-ghost2 current-game-state)) 
    (zero? (ghost-scared (game-state-ghost2 current-game-state)))) (get-current-ghost2 (game-state-maze current-game-state))
      (if (and (not (zero? (ghost-scared (game-state-ghost2 current-game-state)))) (zero? (modulo (game-state-time current-game-state) ghost-slowness))) 
        (make-ghost (ghost-position (game-state-ghost2 current-game-state)) (ghost-dir (game-state-ghost2 current-game-state)) 
          (tick-pill-state (ghost-scared (game-state-ghost2 current-game-state)) new-pacman (game-state-board current-game-state)) 0)
          (make-ghost new-ghost2-pos (get-ghost-dir (ghost-position (game-state-ghost2 current-game-state)) new-ghost2-pos) 
            (tick-pill-state (ghost-scared (game-state-ghost2 current-game-state)) new-pacman (game-state-board current-game-state)) 0)))))


#|
  Función utilizada para actualizar el marcador del juego, segun los moviemientos realizados de pacman
    @param current-game-state: estado actual del juego.
  @return el marcador actualizado
|#
(define (tick-score current-game-state)
  ; Se obtienen los atributos necesarios del game-state 
  (local [(define new-pm (game-state-pacman current-game-state))
          (define last-board (game-state-board current-game-state))
          (define ghost1 (game-state-ghost1 current-game-state))
          (define ghost2 (game-state-ghost2 current-game-state))
          (define cell (board-ref last-board (position-x (pacman-position new-pm)) (position-y (pacman-position new-pm))))
          ; Se suma 100 al score si pacman come un fantasma.
          (define score (+ (game-state-score current-game-state) (if (or (got-eaten? new-pm (ghost-position ghost1) (zero? (ghost-scared (game-state-ghost1 current-game-state)))) (got-eaten? new-pm (ghost-position ghost2) (zero? (ghost-scared (game-state-ghost2 current-game-state))))) 100 0)))]
    (cond [(string=? "dot" cell)
           (+ 5 score)] ;en caso que pacman se encuentre sobre un punto se suma 5
          [(string=? "pill" cell)
           (+ 10 score)] ;en caso que pacman se encuentre sobre una pildora se suma 10
          [(string=? "empty" cell) (if (zero? score) score (- score 1))] ; por espacio vacio se resta uno
          [else
           score]))); se retorna el score


#|
  Función utilizada para calcular la cantidad actual de dots en el tablero, se resta uno si pacman esta sobre un punto o dot en el tablero.
    @param current-dots: cantidad actual de puntos
    @param pacman: estructura de pacman con posición (x, y)
    @param board: tablero con el estado actual del juego.
  @return entero de la cantidad de puntos
|#
(define (tick-dots current-dots pacman board)
  (define cell (board-ref board (position-x (pacman-position pacman)) (position-y (pacman-position pacman))))
  (if (string=? "dot" cell) (sub1 current-dots) current-dots)) ;se resta uno si pacman esta sobre un punto o dot en el tablero.


#|
  Función utilizada para actualizar el tablero de juego, se cambian las celdas segun las pildoras o puntos por los que pase pacman
    @param pacman: estructura de pacman con posición (x, y)
    @param board: tablero con el estado actual del juego.
  @return un nuevo tablero con las celdas actualizadas
|#
(define (tick-board board pacman)
  (if (string=? (board-ref board (position-x (pacman-position pacman)) (position-y (pacman-position pacman))) "empty") board
      (new-board-empty-at (position-x (pacman-position pacman)) (position-y (pacman-position pacman)) board)))


#|
  Función utilizada para actualizar la interfaz del tablero de juego, en caso de que pacman pase por una pildora o punto
  se actualiza a una casilla vacía.
    @param pacman: estructura de pacman con posición (x, y)
    @param board: tablero con el estado actual del juego.
    @param board-image: escena usada para mostrar en la interfaz grafíca.
  @return actualiza la imagen que se muestra en la interfaz
|#
(define (tick-image-board board board-image pacman)
  (local [(define x (position-x (pacman-position pacman)))
          (define y (position-y (pacman-position pacman)))]
    (if (or (string=? (board-ref board x y) "dot")
            (string=? (board-ref board x y) "pill"))
        (place-cell-image EMPTY-IMAGE x y board-image)
        board-image)))


#|
  Función que se encarga de aumentar el marcador de tiempo del juego durante un tick.
    @param time: se aumenta en uno con cada tick que pasa.
  Return: tiempo aumentado.
|#
(define (tick-time time)
  (add1 time))

#|
  Función que se encarga de actualizar el estado de todos los elementos del juego, mediante ticks (ciclos) actualiza los valores
  de los distintos atributos y agentes que se encuentran en el juego.
    @param current-game-state: estado actual del juego.
  @return una actualización con de los movimientos y cambios en el juego.
|#
(define (tick current-game-state)
  (local [(define new-pacman (tick-pacman current-game-state))
          (define new-board (tick-board (game-state-board current-game-state) (game-state-pacman current-game-state)))]
  (cond [(zero? (game-state-dots current-game-state)) (next-maze current-game-state)]
        [else (cond [(goal? (pacman-position (game-state-pacman current-game-state))
                  (ghost-position (game-state-ghost1 current-game-state))
                (ghost-scared (game-state-ghost1 current-game-state))
                (ghost-position (game-state-ghost2 current-game-state))
                (ghost-scared (game-state-ghost2 current-game-state))) (lost-live current-game-state)]
                    [else (make-game-state new-pacman new-board
                            (tick-image-board (game-state-board current-game-state) (game-state-board-image current-game-state) (game-state-pacman current-game-state))
                            (tick-score current-game-state) (tick-time (game-state-time current-game-state))
                            (tick-ghost1 current-game-state new-pacman) (tick-ghost2 current-game-state new-pacman) (game-state-lives current-game-state) (game-state-maze current-game-state)
                            (tick-dots (game-state-dots current-game-state) new-pacman new-board))])])))

#|------------------------------------------------------------------------------------------------------------------------------------------------|#

#|
  Estructura utilizada para almacenar todos los datos necesarios para el juego. Contiene:
    @param pacman: una estructura de pacman
    @param board: una matriz con caracteres representando el valor de las celdas pared, pildora, vacío o puntos
    @param board-image: el fondo presente en la interfaz.
    @param score: marcador de la partida, cantidad de puntos.
    @param time: tiempo de la partida.
    @param ghost1: una estructura de fantasma 1
    @param ghost2: una estructura de fantasma 2
    @param lives: cantidad de vidas restantes del PacMan
    @param maze: número de tablero actual del juego.
    @param dots: cantidad de puntos en el tablero.
|#
(define-struct game-state (pacman board board-image score time ghost1 ghost2 lives maze dots))


#|
  Función utilizada para revisar el estado del juego, si ya se finalizado ya sea ganando el pacman o porque no tenga vidas 
    @param current-game-state: estructura con los atributos del juego, se obtienen el pacman, fantasmas y vidas.
  Rertorna: verdadero si ya se termino, falso otro caso.
|#
(define (game-over? current-game-state)
  (or (= (add1 FINAL-MAZE) (game-state-maze current-game-state)) ; se verifíca si es el último nivel
      (and (zero? (game-state-lives current-game-state)) ; se comprueba si aun hay vidas
           (goal? (pacman-position (game-state-pacman current-game-state)); y si no se comen al pacman
                (ghost-position (game-state-ghost1 current-game-state))
                (ghost-scared (game-state-ghost1 current-game-state))
                (ghost-position (game-state-ghost2 current-game-state))
                (ghost-scared (game-state-ghost2 current-game-state))))))


#|
  Función utilizada para calcular el marcador final del juego, al total de puntos se le resta el tiempo tardado.
    @param total-time: tiempo total en que se realizo la partida.
    @param total-score: marcador obtenido a lo largo de la partida.
  @return entero con los resultados
|#
(define (final-score total-time total-score)
  (if (< total-score total-time) 0 ; el marcador no va a ser negativo nunca.
      (- total-score  total-time)));


#|
  Función principal del programa, se crea el Game-State inicial y se inicializa la interfaz grafica
|#
(define (main)
  ; Se define el game-state inicial con sus atributos, correspondientes al nivel que se encuentra definido en INIT-MAZE.
  (local [(define INIT-GAME-STATE (make-game-state (get-current-pacman INIT-MAZE) (get-current-board INIT-MAZE)
                                   (render-board (get-current-board INIT-MAZE))
                                   INIT-SCORE
                                   INIT-TIME
                                   (get-current-ghost1 INIT-MAZE)
                                   (get-current-ghost2 INIT-MAZE)
                                   INIT-LIVES-COUNT
                                   INIT-MAZE
                                   (get-current-board-dots INIT-MAZE)))]
    (big-bang INIT-GAME-STATE ; funcion de la biblioteca grafica, se usa para la ejecución de la escena de la interfaz constantemente
              (on-tick tick TOCK) ; funcion de tiempo utilizada para realizar todos los calculos del programa
              (to-draw render) ; se encarga de renderizar el tablero de juego, asi como los datos asociados al juego: vidas, tiempo, marcador y nivel.
              (stop-when game-over? last-scene) ; Condición de parada del juego, en caso de cumplirse se llama a la pantalla final.
      )))

#|
(define (timerun)
  (define startt (current-milliseconds)) (main) (- (current-milliseconds) startt))

(timerun)
|#

(main)