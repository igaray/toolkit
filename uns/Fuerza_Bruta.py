import Punto

"""
    Esta funcion chequea la distancia entre todos los pares de puntos y retorna
    el par de puntos más cercanos, junto con la distancia entre ellos.
    Las coordenadas de los puntos se encuentran en el rango [1, 10^9].
    La cantidad de elementos en la lista es n, siendo la lista pasada como parámetro.
    La lista debe tener al menos 2 elementos, de lo contrario seria inncesario aplicar
    el algortimo.
"""

def fuerza_bruta(lista):
	if len(lista)>2:

		punto_inicial = lista[0]
		punto_final   = lista[1]
		distancia_minima = Punto.distancia(punto_inicial,punto_final)

		for p1 in lista:
			for p2 in lista:
				distancia = Punto.distancia(p1,p2)
				if (distancia!=0 and distancia<distancia_minima):
					punto_inicial = p1
					punto_final = p2
					distancia_minima = distancia

	return punto_inicial,punto_final,distancia_minima
