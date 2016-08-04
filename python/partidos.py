#!/usr/bin/python
# Requires Python 3
# Uso:
# $ chmod +x partidos.py
# $ ./partidos.py
# Union Masoquista por la Castidad
# $
import random

prefix = [
    "Partido Nudista",
    "Frente Popular",
    "Coalicion Alcoholica",
    "Union Golfa",
    "Partido Yonqui",
    "Alianza Ninja",
    "Alianza Ninfomana",
    "Frente Tuitero",
    "Pardito Discelixo",
    "Confederacion Satanica",
    "Frente Pacifista",
    "Movimiento Ciborg",
    "Union Masoquista",
    "Union Choni",
    "Partido Ultrarreligioso",
    "Alianza Alienigena",
    "Movimiento Friki",
    "Frente Hipster",
    "Union Viciosa",
    "Coalicion Buenorra",
    "Coalicion Hippy",
    "Movimiento Aguafiestas",
    "Frente Histerico",
    "Movimiento Vegano",
    "Partido Suicida",
    "Partido Hermafrodita"
]
suffix = [
    "Por los Recortes",
    "Corrupto/a",
    "Contra la Depilacion",
    "del Espacio Exterior",
    "Anti Reggaeton",
    "Erotico-Festivo/a",
    "de Invernalia",
    "de Hogwarts",
    "por la Cerveza Gratis",
    "por el Topless",
    "por la Austeridad",
    "del Inframundo",
    "Exhibicionista",
    "Perroflauta",
    "Anti Siestas",
    "del Vaticano",
    "Control el Alcohol",
    "por la Castidad",
    "de Mordor",
    "Juerguista",
    "de Chernobil",
    "del Mal",
    "Democrata",
    "Imaginario/a",
    "Dialogante",
    "del Futuro"
]
i = random.randint(0, len(prefix) - 1)
j = random.randint(0, len(suffix) - 1)
print(prefix[i] + " " + suffix[j])