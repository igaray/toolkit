use std::cmp::Ordering;
use std::fmt;

// Definimos un struct Interval para referirnos comodamente a los campos.
// s representa "start", el inicio del intervalo, y f la finalizacion.
#[derive(Clone, Copy)]
struct Interval {
    s: i32,
    f: i32,
}

fn main() {
    let candidates = vec![
        Interval { s: 1, f: 10 },
        Interval { s: 2, f: 5 },
        Interval { s: 4, f: 6 },
        Interval { s: 5, f: 8 },
        Interval { s: 7, f: 9 },
        Interval { s: 8, f: 10 },
    ];
    println!("candidates: {:?}", candidates);
    let solution = activity_selection(candidates);
    println!("solution:   {:?}", solution);
    // Imprime [(2,5), (5,8), (8,10)]
}

// La funcion toma por parametro un vector mutable de intervalos representando al conjunto de candidatos.

fn activity_selection(mut candidates: Vec<Interval>) -> Vec<Interval> {         // n: cardinality of set of candidates
    // Ordenamos las actividades por su finalizacion.
    candidates.sort_by(|a, b| {                                                 // sort based on timsort, O(n log n)
        if (*a).f < (*b).f {
            Ordering::Less
        } else if (*a).f == (*b).f {
            Ordering::Equal
        } else {
            Ordering::Greater
        }
    });
    // Creamos un vector de intervalor para representar el conjunto solucion y lo inicializamos con el primer intervalo.
    // first() retorna el primer elemento de un vector, envuelto en una estructura de tipo Result que puede tener uno de dos valores: Some(valor), representando el valor deseado, y None, representando que no habia valor alguno en el vector.
    // Este metodo tiene orden constante porque solo debe acceder el primer valor del vector.
    // unwrap() es un metodo del envoltorio Result que lo desenvuelve y retorna directamente el valor si esta presente, y emite un panico (similar a una excepcion) si esta suposicion no es valida.
    // Por ende esta implementacion asume que el conjunto de candidatos tiene por lo menos un elemento.
    let mut solution: Vec<Interval> = Vec::new();                               // O(1)
    solution.push(*candidates.first().unwrap());                                // O(1)
    // Iteramos sobre los candidatos.
    // iter() construye un iterador sobre el vector, el cual mantiene la informacion necesaria para recorrerlo.
    // skip(1) construye otro iterador en base al anterior, salteandose el primer elemento.
    // Nos salteamos uno porque ya lo incluimos en el conjunto solucion.
    // Esto es equivalente a comenzar la iteracion a partir del indice 2, como lo hace la implementacion iterativa del algoritmo en el capitulo 16.1 del Cormen.
    // Todos estos metodos son de orden O(1).
    for candidate in candidates.iter().skip(1) {                                // skip(), iter(): O(1); for: O(n)
        // last() retorna el ultimo elemento en el vector solucion envuelto en un Result, de manera similar a como lo hace first().
        // En este caso, el unwrap() que accede de manera directa al valor es valido porque sabemos que el conjunto solucion tiene al menos un elemento, por su inicializacion.
        // Si el intervalo candidato es disjunto del ultimo, i.e. su comienzo es mayor o igual a la finalizacion del ultimo candidato elegido, entonces se incluye en el conjunto solucion.
        if disjoint(candidate, solution.last().unwrap()) {                      // unwrap(), last(), & disjoint(): O(1)
            solution.push(*candidate);                                          // O(1)
        }
    }
    return solution;
}

fn disjoint(x: &Interval, y: &Interval) -> bool {
    x.s >= y.f
}

impl fmt::Debug for Interval {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({}, {})", self.s, self.f)
    }
}
