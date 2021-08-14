# Inteligencia Artificial - Primer Proyecto: PacMan - Minimax con Poda α – β

Primer proyecto del curso de Inteligencia Artificial (código ic-6200) en la carrera de Ingeniería en Computación del Tecnológico de Costa Rica.

![cyberpacmac](https://user-images.githubusercontent.com/56206208/129430058-611a95a5-4a82-455e-898e-16a929198079.png)

## Objetivo

Desarrollar una aplicación que utilice Inteligencia Artificial para jugar "Pac-Man" de forma automatizada, utilizando el algoritmo Minimax con Poda α–β. El sistema deberá ser desarrollado utilizando el lenguaje de programación funcional Racket. Con este proyecto se plantea implementar y evaluar heurísticas para mejorar el rendimiento y eficacia de los agentes.

Para probar el desempeño de agentes en el juego de Pac-Man, las reglas del juego original van a ser simplificadas de la siguiente forma:
* El juego se va a realizar con máximo dos fantasmas.
* Todos los fantasmas van a estar controlados por la misma heurística.
* Comer un punto en el laberinto suma 10 puntos.
* Avanzar un cuadro resta un punto.
* Ganar un nivel suma 300 puntos.
* Perder una vida resta 100 puntos.
* Deben existir varias configuraciones de laberinto (al menos tres, una por nivel).
* El juego termina cuando un fantasma se come a Pac-Man y no hay vidas extra o cuando Pac-Man jugó los tres niveles.
* Inicialmente Pac-Mac contará con tres vidas, cuando pasa de nivel gana tres vidas adicionales.

## Requerimientos

* El sistema debe implementar funcionalidad multi-agente en un ambiente gráfico agradable utilizando alguna de las bibliotecas para manejo de interfaz gráfica disponibles en Racket.

* Los fantasmas deben seguir Pac-Man indefinidamente y actuar de forma independiente moviéndose en la dirección que reduce la distancia a Pac-Man. Sugerencia, pueden utilizar la distancia de Manhatan para implementar esta heurística o cualquier otra de su elección.

* En el juego, Pac-Man enfrenta múltiples adversarios o fantasmas, en este caso el árbol de juego de minimax deberá incluir una capa de min por cada fantasma. Por ejemplo, si se juega con dos fantasmas, el árbol de minimax deberá incluir dos capas de min después de cada capa de max. 

* La profundidad del árbol de minimax debe ser variable de forma que se pueda evaluar el juego en varias profundidades. Un nivel de profundidad involucra que todos los agentes muevan una vez, en este caso, para tres agentes (Pac-Man y dos fantasmas) cada jugada representará bajar tres niveles en el árbol de juego. 

* Documentación de pruebas de rendimiento variando la profundidad del árbol explorado. Debe incluir una tabla y gráficas comparativas que muestren el rendimiento (eficiencia y eficacia) de la función Eval en tiempo promedio de duración por jugada variando de acuerdo a la profundidad explorada (al menos se deben evaluar 3 profundidades en las que el tiempo de respuesta sea aceptable). Para realizar estadísticas confiables debe al menos evaluar más de 25 jugadas para obtener un promedio de duración.

* El sistema debe implementar correctamente la funcionalidad del juego, el algoritmo Minimax con Poda α–β, debe contar con una interfaz gráfica agradable y ser eficaz y eficiente. 

## Funcionamiento del programa

Para poner en ejecución al programa es necesario instalar Racket en el sistema (en este ejemplo con Ubuntu):

```console
sudo apt-get install -y racket
```

y ejecutar el siguiente comando: 

```console
racket pacman.rkt
```

Seguidamente, la aplicación se mostrará de la siguiente manera:

![cyberpacman](https://user-images.githubusercontent.com/56206208/129430053-3ea8d9b7-3d87-4433-8dba-7c657b4c9391.gif)

## Estado

El programa funciona correctamente, todas las funcionalidades fueron correctamente implementadas.

## Realizado por:

* Brandon Ledezma Fernández

* Walter Morales Vásquez
