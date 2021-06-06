import time
import math
import copy

class Punto:
    def __init__(self, x, y):
        self.x = x
        self.y = y

    def __repr__(self):
        return f"({self.x},{self.y})"


def distancia(p1, p2):
    return math.sqrt((p1.x - p2.x) * (p1.x - p2.x) + (p1.y - p2.y) * (p1.y - p2.y))


def fuerza_bruta(lista):
    """
    Esta funcion chequea la distancia entre todos los pares de puntos y retorna
    el par de puntos más cercanos, junto con la distancia entre ellos.
    Las coordenadas de los puntos se encuentran en el rango [1, 10^9].
    La cantidad de elementos en la lista es n, siendo la lista pasada como parámetro.
    La lista debe tener al menos 2 elementos, de lo contrario seria inncesario aplicar
    el algortimo.
    """
    if len(lista)>2:
        punto_inicial = lista[0]
        punto_final   = lista[1]
        distancia_minima = distancia(punto_inicial,punto_final)
        for p1 in lista:
            for p2 in lista:
                distancia = distancia(p1,p2)
                if (distancia!=0 and distancia<distancia_minima):
                    punto_inicial = p1
                    punto_final = p2
                    distancia_minima = distancia
    return punto_inicial,punto_final,distancia_minima


def par_mas_cercano_fb(P):
    n = len(P)
    puntos = None
    distancia_minima = float('inf')
    for i in range(n):
        for j in range(i + 1, n):
            d = distancia(P[i], P[j])
            if d < distancia_minima:
                distancia_minima = d
                puntos = (P[i], P[j])
    return distancia_minima


def par_mas_cercano_en_franja(franja, n, d):
    distancia_minima = d
    for i in range(n):
        j = i + 1
        while j < n and (franja[j].y - franja[i].y) < distancia_minima:
            distancia_minima = distancia(franja[i], franja[j])
            j += 1
    return distancia_minima


def particionar(P, n):
    medio = n // 2
    puntoMedio = P[medio]
    Pl = P[:medio]
    Pr = P[medio:]
    return (medio, puntoMedio, Pl, Pr)


def combinar(n, Pl, Pr, dl, dr, puntoMedio, Q):
    d = min(dl, dr)
    franjaP = []
    franjaQ = []
    P = Pl + Pr
    for i in range(n):
        if abs(P[i].x - puntoMedio.x) < d:
            franjaP.append(P[i])
        if abs(Q[i].x - puntoMedio.x) < d:
            franjaQ.append(Q[i])
    franjaP.sort(key = lambda point: point.y)
    min_a = min(d, par_mas_cercano_en_franja(franjaP, len(franjaP), d))
    min_b = min(d, par_mas_cercano_en_franja(franjaQ, len(franjaQ), d))
    return min(min_a, min_b)


def par_mas_cercano_dyc_aux(P, Q, n):
    if n <= 3:
        return par_mas_cercano_fb(P)
    medio, puntoMedio, Pl, Pr = particionar(P, n)
    dl = par_mas_cercano_dyc_aux(Pl, Q, medio)
    dr = par_mas_cercano_dyc_aux(Pr, Q, n - medio)
    resultado = combinar(n, Pl, Pr, dl, dr, puntoMedio, Q)
    return resultado


def par_mas_cercano_dyc_1(P):
    n = len(P)
    P.sort(key = lambda punto: punto.x)
    Q = copy.deepcopy(P)
    return par_mas_cercano_dyc_aux(P, Q, n)


def par_mas_cercano_dyc_2(P):
    n = len(P)
    P.sort(key = lambda punto: punto.x)
    Q = copy.deepcopy(P)
    Q.sort(key = lambda punto: punto.y)   
    return par_mas_cercano_dyc_aux(P, Q, n)


def generar_datos(n):
    """
    Esta funcion genera una lista de puntos aleatorios.
    Las coordenadas de los puntos se encuentran en el rango [1, 10^9].
    La cantidad de elementos en la lista es una potencia de n, siendo n el valor pasado por parametro.
    """
    import random
    return [ Punto(random.randint(0, 10**9), random.randint(1, 10**9)) for _ in range(2**n) ]


def benchmark():
    import timeit

    base = "from __main__ import generar_datos, par_mas_cercano_fb; "
    for n in range(2, 14):
        t = timeit.timeit(stmt="par_mas_cercano_fb(d)", setup=base + f"d = generar_datos({n})", number=1)
        print("par_mas_cercano_fb,{},{:.6f}".format(n,t))

    base = "from __main__ import generar_datos, par_mas_cercano_dyc_1; "
    for n in range(2, 21):
        t = timeit.timeit(stmt="par_mas_cercano_dyc_1(d)", setup=base + f"d = generar_datos({n})", number=1)
        print("par_mas_cercano_dyc_1,{},{:.6f}".format(n,t))

    base = "from __main__ import generar_datos, par_mas_cercano_dyc_2; "
    for n in range(2, 21):
        t = timeit.timeit(stmt="par_mas_cercano_dyc_2(d)", setup=base + f"d = generar_datos({n})", number=1)
        print("par_mas_cercano_dyc_2,{},{:.6f}".format(n,t))



def main():
    # P = generar_datos(3)
    P = [Punto(100,200), Punto(300,400), Punto(500,600), Punto(1,1), Punto(2,2)]
    d = par_mas_cercano_dyc_2(P)
    print(f"Distancia: {d}")


if __name__ =="__main__":
    # main()
    benchmark()