# Analisis Amortizado

El análisis amortizado de las operaciones de una estructura de datos permite 
obtener cotas superiores más ajustadas a los tiempos de ejecución de una 
secuencia de operaciones sobre la estructura. Esto es, si por ejemplo en un 
algoritmo iterativo tomamos como barómetro las operaciones sobre la estructura 
de datos, entonces podemos obtener cotas mejores que multiplicar el peor caso 
por la cantidad de iteraciones. De esta manera se permite alguna ejecución 
costosa de una operación pero que beneficie a las ejecuciones posteriores de 
la mismo o de otras operaciones en la misma estructura.

Vamos primero a desarrollar cómo se aplica la técnica de análisis amortizado, y 
luego introduciremos dos estructuras de datos heaps que la utilizam: los Skew 
Heaps y los Heaps de Fibonacci.

- [Data Structures in Typescript #22 - Fibonacci Heap Introduction](https://www.youtube.com/watch?v=E_AgyAI8lsc)

Les recomendamos para estudiar el tema de Análisis Amortizado que se guíen por 
las transparencias, y vean:

1) Comenzar leyendo el capítulo 17 del libro de Cormen, sobre técnicas de 
análisis amortizado y ejemplos sencillos.

2) Para estudiar la estructura de datos de Skew Heaps, lean el capítulo 11 del 
libro de Weiss. Pueden usar alguna página de visualización de esta estructura de 
datos para comprender mejor su funcionamiento. Un ejemplo es

- [Skew Heap Visualization](https://www.cs.usfca.edu/~galles/visualization/SkewHeap.html)

3) Para los Heaps de Fibonacci complementen el capítulo anterior con el capítulo 
19 del libro de Cormen. También pueden usar alguna página de visualización para 
comprender mejor su funcionamiento, como por ejemplo

- [Fibonacci Heap Visualization](https://www.cs.usfca.edu/~galles/visualization/FibonacciHeap.html)

También tienen varios videos para ver sobre esta estructura de datos.

4) Realicen los ejercicios del práctico.

Como siempre cualquier duda tienen el foro para consultar. 
