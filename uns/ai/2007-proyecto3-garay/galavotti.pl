% estadoInicial(L): Hecho que representa el estado inicial del mundo, donde L 
% es una lista de instancias de relaciones que se verifican en el.

% ESTADO INICIAL I :  Requerimiento funcional dado en el enunciado del proyecto.
  estadoInicial([libre(a), 
                 libre(b), 
                 libre(c), 
                 enMesa(a), 
                 enMesa(b), 
                 enMesa(c)]).

p :- 
    plan([sobre(a, b), sobre(b, c)], P), 
    write('Plan encontrado : '), 
    write(P).


% ESTADO INICIAL II
% estadoInicial([libre(a), 
%                libre(b), 
%                sobre(a, c), 
%                enMesa(b), 
%                enMesa(c)]).

p0 :- 
    plan([sobre(a, b)], P0), 
    write('Plan encontrado : '), 
    write(P0).
    
p1 :- 
    plan([sobre(b, a)], P1), 
    write('Plan encontrado : '), 
    write(P1).

% ejemplo del enunciado del proyecto
p2 :- 
    plan([sobre(b, c), sobre(a, b)], P2), 
    write('Plan encontrado : '), 
    write(P2). 

p3 :- 
    plan([libre(c)], P), 
    write('Plan encontrado : '), 
    write(P).

% ESTADO INICIAL III 
% estadoInicial([libre(a), 
%                libre(o), 
%                libre(h), 
%                enMesa(l), 
%                enMesa(o), 
%                enMesa(h), 
%                sobre(a, l), 
%                sobre(d, e), 
%                libre(d), 
%                libre(f), 
%                enMesa(e), 
%                enMesa(f)]).

p5 :- 
    plan([sobre(h, o), sobre(a, h)], P), 
    write('Plan encontrado : '), 
    write(P).

p6 :- 
    plan([sobre(a, h),sobre(h, o)], P), 
    write('Plan encontrado : '), 
    write(P).

p7 :- plan([sobre(a, h),sobre(h, o), sobre(d, f), sobre(f, e), sobre(e, a)], P), 
    write('Plan encontrado : '), 
    write(P).

% Representacion de Acciones

% ACCIONES POSIBLES:
% apilar(A,B):
 % PRE:  [libre(A), libre(B), enMesa(A)]
 % ADD:  [sobre(A,B)]
 % DEL:  [libre(B), enMesa(A)]

% desapilar(A,B):
  % PRE:[sobre(A,B), libre(A)] 
  % ADD:[enMesa(A), libre(B)]
  % DEL:[sobre(A,B)]

% acc/4: acc(NombreAccion(Parametros), ListaPrecondiciones, AddList, DeleteList).

acc(apilar(A,B), [libre(A), libre(B), enMesa(A)], [sobre(A,B)], [libre(B), enMesa(A)]).
acc(desapilar(A,B), [sobre(A,B), libre(A)], [enMesa(A), libre(B)], [sobre(A,B)]).

% Implementacion del planificador

% plan(+Metas, -Plan) : dada la lista de metas a conseguir devuelve el plan 
% representado a traves de una lista ordenada de acciones, que consigue estas 
% metas a partir del estado inicial.

plan(Metas, Plan) :- 
    write('Metas dadas : '),
    write(Metas),
    nl,
    write('BUSCANDO PLAN...'),
    nl,
    estadoInicial(EstadoInicial), 
    alcanzar_todas(Metas, EstadoInicial, Plan, EstLgoDplan).

% alcanzar_todas(+Metas, +EstadoActual, -Plan, -ELgoPlan):Partiendo del estado 
% EstadoActual, trata de encontrar un plan que lleve a cabo las metas dadas 
% donde Plan contendra la lista de acciones a llevar a cabo y ELgoPlan sera el 
% estado resultante de ejecutar dichas acciones.

vale(M,E) :- member(M, E).

valen_todas([], _EstadoActual).
valen_todas([M|R], EstadoActual) :-
    vale(M, EstadoActual), 
    valen_todas(R, EstadoActual).
    
alcanzar_todas([], E, [], E).
alcanzar_todas(Metas, EstadoActual, [], EstadoActual) :-
    valen_todas(Metas, EstadoActual).
 
alcanzar_todas(Metas, EstadoActual, Plan, ELgo1) :-
    write('Estado actual : '),
    write(EstadoActual),
    nl,
    seleccionar(Metas, M, RestoMs),
    write('Meta seleccionada :'),
    write(M),
    nl,
    nl,
    write('Metas restantes :'), 
    write(RestoMs),
    nl,
    nl,
    alcanzar(M, EstadoActual, PlanM, ELgoPlanM), 
    alcanzar_todas(RestoMs, ELgoPlanM, PlanRestoMs, ELgoPlan),
    append(PlanM, PlanRestoMs, PlanR),
    
    % Chequeo para ver si alguna accion deshizo meta que se necesita.
    alcanzar_todas(Metas, ELgoPlan, Plans, ELgo1),
    append(PlanR, Plans, Plan).
    
% alcanzar(+Meta, +EstActual, -Plan, -ELgoPlanMeta): alcanza la meta dada, 
% Meta, partiendo del estado actual. Retorna el plan que consigue la meta y el 
% estado resultante de ejecutarlo desde el estado actual dado.

% caso base: la meta ya se satisface en el estado actual.

alcanzar(M, EAct, [], EAct) :- 
    member(M, EAct). 

% caso en que la meta no se satisface en el estado actual; luego debemos 
% encontrar que accion logra la meta, satisfacer las precondiciones de dicha 
% accion y luego ejecutar la accion obteniendo asi el estado resultante de esta
% ejecucion.

alcanzar(M, EAct, PlanM, ELgoPlanM):-
    % obtengo accion que logra M, ie, tiene a M en su add_list.
    alcanza(Acc, EAct, M),
    precondiciones(Acc, Pre),
    write('Accion que alcanza la meta seleccionada : '),
    write(Acc),
    nl,
    write('Precondiciones necesarias para ella : '),
    write(Pre),
    nl, 
    % obtengo las precondiciones necesarias para ejecutar M.
    % satisfago las precondiciones necesarias para ejecutar Acc.
    alcanzar_todas(Pre, EAct, PlanPre, ELgoPlanPre), 
    % ejecuto la accion Acc que logra la meta M desde el estado resultante de 
    % lograr las precondiciones para Acc., obteniendo el nuevo estado resultado 
    % de ejecutar al accion.
    ejecutar(Acc, ELgoPlanPre, ELgoPlanM),
    write('Estado luego de ejecutar accion :'),
    write(ELgoPlanM),
    nl,
    nl,
    % agrego al plan la accion que alcanza M junto al plan que logra las 
    % precondiciones para Acc.
    append(PlanPre, [Acc], PlanM).                              
    
% PREDICADOS AUXILIARES  

% seleccionar(M, Metas, RestoMs): Dada una lista de metas retorna una de ellas y 
% la lista resultante de sacar esta.    

% retorna la primera meta de la lista, y el resto de las metas.
seleccionar([M|RestoMs], M, RestoMs).   

% alcanza(-Accion, +EstadoActual, +Meta): retorna la accion que satiface Meta 
% tras su ejecucion,ie,que contiene a Meta en su add_list

% ACCIONES :

% apilar(A,B):
 % ADD:  [sobre(A,B)]

% desapilar(A,B):
  % ADD:[enMesa(A), libre(B)]
    
% necesito instanciar A con el bloque q este sobre B para lograr libre luego de 
% desapilar
alcanza(desapilar(A, B), Estado, libre(B)) :- 
    member(sobre(A, B), Estado ). 
  
% necesito instanciar B para obtener sobre que bloq esta A asi puedo 
% desapilarlo luego. 
alcanza( desapilar(A, B), Estado, enMesa(A)) :- 
    member(sobre(A, B), Estado).
  
alcanza(apilar(A, B), _Estado, sobre(A, B)).

% precondiciones(+Acc, -Pre): Dada una accion obtiene y retorna la lista de 
% precondiciones necesarias para ejecutar la accion.

precondiciones(Acc, Pre) :-
    acc(Acc, Pre, _Add, _Del).

% ejecutar(+Acc, +EstadoActual, -EstLgoAcc)
    
ejecutar(Acc, EstadoActual, EstLgoAcc) :-
    acc(Acc, _Pre, Add, Del),
    eliminar(Del, EstadoActual, NvoEstado),
    agregar(Add, NvoEstado, EstLgoAcc).
  
% eliminar(+ListaDel, +EstadoActual, -NvoEstado):para cada elemento de la lista 
% del lo saca del estado actual.

eliminar([], Est, Est). 
eliminar([Rel|ListaDel], Est, NuevoEstado) :-
    eliminar1(Rel, Est, NvoEstado1),
    eliminar(ListaDel, NvoEstado1, NuevoEstado).

eliminar1(Rel, [], []).
eliminar1(Rel, [Rel|Rs], Estado) :-
    eliminar1(Rel, Rs, Estado).
eliminar1(Rel, [R|Rs], [R|Estado]) :-
    eliminar1(Rel, Rs, Estado).
        
% agregar(+Add, +EstadoActual, -NvoEstado): toma cada elemento de add y lo
% agrega al estado actual obteniendo un nuevo estado.

agregar(Add, EAct, NvoEst) :-
    append(Add, EAct, NvoEst).

