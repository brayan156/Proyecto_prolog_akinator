
:-[database,lenguaje].

leer(Persona):-%lectura de los datos escritos en la consola y convertidos en una lista de palabras
nl,            %Persona: es la lista con los atributos de la persona que se esta construyendo
readln(X),
atomics_to_string(X, " ", S),
string_lower(S, L),
atomics_to_string(Datos," ", L),
lectura(Persona, Datos).

leer(Persona,Sus,Atri):-%lectura de los datos escritos en la consola y convertidos en una lista de palabras para las preguntas booleanas
nl,                      %Sus: sustantivo o caracteristica del personaje al que se refiere la pregunta
readln(X),               %Atri: valor del atributo de la pregunta
atomics_to_string(X, " ", S),
string_lower(S, L),
atomics_to_string(Datos," ", L),
lectura(Persona, Datos,Sus,Atri).




afirmar([A|_]):-afirmaciones(List),%detecta una respuesta afirmativa por parte del usuario
miembro(A,List).
afirmar([_|Datos]):-afirmar(Datos).


limpiar_oraciones([],[]).%recibe una lista de palabras en la primera entrada y en la segunda una lista limpia, sin signos
limpiar_oraciones([D|Datos],Newdatos):-limpieza(L),miembro(D,L),limpiar_oraciones(Datos,Newdatos).
limpiar_oraciones([D|Datos],[D|Newdatos]):-limpiar_oraciones(Datos,Newdatos).


% conjuto de reglas que realiza la separacion, de datos de palabras en
% adjetivos, verbos y sustantivos y manda a construir la lista de los
% atributos que de la persona que se esta buscando.
lectura(Persona,Datos,Sustantivo,Atri):-
limpiar_oraciones(Datos,Datos_revisar),%Entradas: Persona: es la lista con los atributos de la persona que se esta construyendo
oracion(Datos_revisar,[]),
afirmar(Datos_revisar),                 %Sustantivo: sustantivo o caracteristica del personaje al que se refiere la pregunta
sacar_verbos(Datos,V),                   %Atri: valor del atributo de la pregunta
sacar_sustantivos(Datos,Sus),
sacar_adjetivos(Datos,A),
agregar(Sustantivo,Sus,Ls),
agregar(Atri,A,La),
verificar(Persona,Ls,V,La).

lectura(Persona,Datos,_,_):-lectura(Persona,Datos).

lectura(Persona,['']):-
 respuesta(vacio,Ln),
 obtener_dato_random(Ln,Na),
 write(Na),nl,
 preguntar(Persona).

lectura(_,[abortar]):-
 write("juego terminado").


lectura(Persona,Datos):-
limpiar_oraciones(Datos,Datos_revisar),
oracion(Datos_revisar,[]),
sacar_verbos(Datos,V),
sacar_sustantivos(Datos,Sus),
sacar_adjetivos(Datos_revisar,A),
verificar(Persona,Sus,V,A).

lectura(Persona,_):-
 respuesta(mala_estructura,Ln),
 obtener_dato_random(Ln,Na),
 write(Na),nl,
 preguntar(Persona).




sacar_adjetivos([],[]).%crea una lista de adjetivos encontrados que coincidan con la base de datos, de una lista de palabras
sacar_adjetivos([Ad|Datos],[Adj|Rest]):-%primera entrada: lista de palabras
adjetivos(Adj,La),                       %segunda entrada: lista de adjetivos
miembro(Ad,La),
sacar_adjetivos(Datos,Rest).
sacar_adjetivos([_|Datos],Adjetivos):-sacar_adjetivos(Datos,Adjetivos).

sacar_verbos([],[]).%crea una lista de verbos encontrados que coincidan con la base de datos, de una lista de palabras
sacar_verbos([Verb|Datos],[V|Rest]):-%primera entrada: lista de palabras
referencia_verbos(V,La),             %segunda entrada: lista de verbos
miembro(Verb,La),
sacar_verbos(Datos,Rest).
sacar_verbos([_|Datos],Verbos):-sacar_verbos(Datos,Verbos).


sacar_sustantivos([],[]).%crea una lista de sustantivos encontrados que coincidan con la base de datos, de una lista de palabras
sacar_sustantivos([Sust|Datos],[S|Rest]):-%primera entrada: lista de palabras
referencia_sinonimos(S,La),                 %segunda entrada: lista de verbos
miembro(Sust,La),
sacar_sustantivos(Datos,Rest).
sacar_sustantivos([_|Datos],Sustantivos):-sacar_sustantivos(Datos,Sustantivos).



miembro(X, [X|_]).%funcion que indica si el elemento X de la primera entrada pertenece a la lista de la segunda entrada
miembro(X, [_|Y]) :- miembro(X, Y).

longitud([], 0).%determina la longitud de la lista de la primera entrada y la indica en la segunda entrada
longitud([_|L1], R2) :- longitud(L1, R), R2 is R+1.

obtener_pos(0,[A|_],A).%obtiene un elemento en cierta poscición de una lista
obtener_pos(Num,[_|B],A):-obtener_pos(N,B,A),Num is N+1.%Entradas: primera entrada: posicion en la lista %segunda entrada: lista de datos
                                         %tercera entrada: valor del elemento en la posicioón mencionada
agregar(X, L, [X|L]).%agrega el elemento de la primera entrada a la lista de la segunda entrada y devuelve la lista con el elemento en la tercera entrada

inversa(L1, L) :- inversa(L1, [], L).%invierte una lista entradas: Primera entrada: Lista a invertir
inversa([], L, L).                    %segunda entrada:lista invertida
inversa([X|L1], L2, L3) :- inversa(L1, [X|L2], L3).


cambiar_dato(0,A,[_|D],[A|C]):-cambiar_dato_aux(D,C).%cambia un dato en x posición por otro dado
cambiar_dato(N,A ,[B|C],[B|D]):-cambiar_dato(N1,A,C,D),N is N1+1.%primera entrada posición del elemento
cambiar_dato_aux([],[]).                                           %segunda entrada: valor del nuevo elemento
cambiar_dato_aux([B|C],[B|D]):-cambiar_dato_aux(C,D).              %tercera entrada: lista donde se desea modificar el elemento
                                                                   %cuarta entrada: lista con el dato modificado

lista_x_ele_repetidos(X,B,E):-lista_x_ele_repetidos(X,B,[],E).%crea una lista de x cantidad de un solo elemento
lista_x_ele_repetidos(0,_,D,D).                               %Entradas, X: es el largo de la lista %E: Es la lista creada de x cantidad de elementos B
lista_x_ele_repetidos(N,B,D,E):-lista_x_ele_repetidos(N2,B,[B|D],E),N is N2+1. %B: es el elemento del que se va a crear la lista

posiciones_dato(A,L,Ln):-longitud(L,N),inversa(L,L3),posiciones_dato_aux(A,L3,Ln,N).%obtiene las posiciones en las que se encuentra un dato en una lista
posiciones_dato_aux(_,[],[],0).                                                  %primera entrada: dato a buscar
posiciones_dato_aux(A,[A|L],[N1|Ln],N):-posiciones_dato_aux(A,L,Ln,N1),N is N1+1.%segunda entrada:lista donde se desea buscar el dato
posiciones_dato_aux(A,[_|L],Ln,N):-posiciones_dato_aux(A,L,Ln,N1),N is N1+1. %tercera entrada: lista con el número de las pocisiones en las que se encuentra el dato.

eliminar_dato_d(D,[D|F],F).%elimina un dato de una lista Primera entrada:dato
eliminar_dato_d(D,[A|B],[A|C]):-eliminar_dato_d(D,B,C).%segunda entrada: Lista  %tercera entrada: lista sin dato de la entrada 1


preguntar_booleano(Sus,Dato):-adj_preguntas(Sus,Ln),%obtiene una pregunta booleana de un sustantivo en especifico
    longitud(Ln,N),                                 %Sus: sustantivo al que se le desea sacar la pregunta booleana
    random(0,N,Nr),                                 %Dato: es string de la pregunta a realizar
    obtener_pos(Nr,Ln,Dato).

obtener_verb(Verb,C):-referencia_verbos(C,B),miembro(Verb,B).%obtiene La referecencia al verbo Verb: Verbo ,C:referencia al verbo Verb

obtener_sin(Sin,C):-referencia_sinonimos(C,B),miembro(Sin,B).%obtiene La referecencia al sustantivo Sin: Sustantivo ,C:referencia al sustantivo Sin

obtener_ad(Adj,C):-adjetivos(C,B),miembro(Adj,B).%obtiene La referecencia al adjetivo Adj: adjetivo ,C:referencia al adjetivo Adj

verificar(Persona,_,_,[]):-preguntar(Persona).%manda a verificar las similitudes entre la lista de verbos y sustantivos con los adjetivos para
verificar(_,[],[],_):-respuesta(mal,Ln),%ir armando la persona pensada por el usuario
    obtener_dato_random(Ln,Na), %Entradas: Persona: lista de atributos a construir para encontrar la persona en la que piensa el usuario
    write(Na),nl,   %la segunda entrada es la lista de sustantivos a los que se refieren los adjetivos
    jugar.            %la tercera entrada es la lista de verbos que se refieren a sustantivos
verificar(Persona,[],Verbos,Adjetivos):-verificar_verb(Persona,Verbos,Adjetivos,Nup,NewAdj),verificar(Nup,[],[],NewAdj).%Lista de adjetivos a comparar y encontrar  similitud
verificar(Persona,Sinonimos,Verbos,Adjetivos):-verificar_sin(Persona,Sinonimos,Adjetivos,Nup,NewAdj),verificar(Nup,[],Verbos,NewAdj).%con los personajes de la base de datos

verificar_verb(Persona,[],Adjetivos,Persona,Adjetivos).%manda a verificar la lista de verbos convirtiendolos en sustantivos encontrados en la lectura de
                                                       %la oración con los adjetivos encontrados
verificar_verb(Persona,[Verb|Rest],Adjetivos,Newp,Adrest):-%entradas: Persona: lista con las caracteristas del personaje que se esta construyendo
    obtener_verb(Verb,Cverb),                               %la segunda entrada es la lista de verbos que se refieren a sustantivos
    referencia_atributos(Cverb,Sinonimos), %Lista de adjetivos a comparar y encontrar similitud con los personajes de la base de datos
    verificar_sin(Persona,Sinonimos,Adjetivos,P,Nadjetivos),%Newp: lista de caracteristicas del personaje construido una vez determinado los nuevos adjetivos
    verificar_verb(P,Rest,Nadjetivos,Newp,Adrest). %ultima entrada es la lista de adjetivos restantes a las que no se le encontro
verificar_verb(Persona,[_|Verbs],Adjetivos,Newp,Adrest):-verificar_verb(Persona,Verbs,Adjetivos,Newp,Adrest).

verificar_sin(Persona,[],Adjetivos,Persona,Adjetivos).%manda a verificar la lista de sustantivos encontrados en la lectura de la oración con los adjetivos encontrados
verificar_sin(Persona,[Sin|Rest],Adjetivos,A,Adrest):-obtener_sin(Sin,Csin),%entradas: Persona: lista con las caracteristas del personaje que se esta construyendo
    comprobar(Persona,Rest,Csin,Adjetivos,Ad,P), %la segunda entrada es la lista de sustantivos a los que se refieren los adjetivos
    eliminar_dato_d(Ad,Adjetivos,Nadjetivos),%Lista de adjetivos a comparar y encontrar similitud con los personajes de la base de datos
    verificar_sin(P,Rest,Nadjetivos,A,Adrest).%Newp: lista de caracteristicas del personaje construido una vez determinado los nuevos adjetivos
verificar_sin(Persona,[_|Sins],Adjetivos,Newp,Adrest):-verificar_sin(Persona,Sins,Adjetivos,Newp,Adrest).%ultima entrada es la lista de adjetivos restantes a las que no se le encontro
                                                                                                      %un sinonimo y una igualdad en la base de datos con ello

comprobar(Persona,[Sin|Rest],Sustantivo,[Adj|_],Adj,Newp):-obtener_sin(Sin,Csin),%conjunto de reglas que manda a buscar y comparar todos los adjetivos
    obtener_ad(Adj,Ad),                                                          %encontrados con los personajes establecidos para construir al personaje pensado
    atributo_posicion(Sustantivo,Pos),                                      %entradas: Persona: lista con las caracteristas del personaje que se esta construyendo
    obtener_pos(Pos,Persona,Ad),                                             %la segunda entrada es la lista de sustantivos restantes
    comprobar_aux(Persona,Rest,Csin,[Adj|_],Adj,Newp).                       %Sustantivo: sustantivo a evaluar con la lista de adjetivo
comprobar(Persona,_,Sustantivo,[Adj|_],Adj,Newp):-                    %Lista de adjetivos a comparar y encontrar similitud con los personajes de la base de datos
    obtener_ad(Adj,Ad),                                                       % Adj:adjetivo a comparar actualmente,
    atributo_posicion(Sustantivo,Pos),                                       %Newp: lista de caracteristicas del personaje construido una vez determinado los nuevos adjetivos
    obtener_pos(Pos,Persona,""),
    cambiar_dato(Pos,Ad,Persona,Newp),
    comprobacion_listas(Newp).
comprobar(Persona,Rest,Sustantivo,[_|Adjetivos],Ad,Newp):-comprobar(Persona,Rest,Sustantivo,Adjetivos,Ad,Newp).

comprobar_aux(Persona,[],Sustantivo,[Adj|_],Adj,Newp):-%conjunto de reglas que funcionan por si se encuentra un mismo
obtener_ad(Adj,Ad),                                    %adjetivo para diferentes caracteristicas de un personaje
    atributo_posicion(Sustantivo,Pos),                 %comprueba cada una hasta llegar al final de la lista de caracteristicas
    obtener_pos(Pos,Persona,""),                       %entradas: Persona: lista con las caracteristas del personaje que se esta construyendo
    cambiar_dato(Pos,Ad,Persona,Newp),                 %la segunda entrada es la lista de sustantivos restantes
    comprobacion_listas(Newp).                         %Sustantivo: sustantivo a evaluar con la lista de adjetivos
comprobar_aux(Persona,[],_,[Adj|_],Adj,Persona).       %Lista de adjetivos a comparar y encontrar similitud con los personajes de la base de datos
comprobar_aux(Persona,[Sin|Rest],Sustantivo,[Adj|_],Adj,Newp):-% Adjetivo:adjetivo a comparar actualmente,
    obtener_sin(Sin,Csin),                             %Newp: lista de caracteristicas del personaje construido una vez determinado los nuevos adjetivos
    obtener_ad(Adj,Ad),
    atributo_posicion(Sustantivo,Pos),
    obtener_pos(Pos,Persona,Ad),
    cambiar_dato(Pos,Ad,Persona,Newp),
    comprobacion_listas(Newp),
    comprobar_aux(Persona,Rest,Csin,[Adj|_],Adj,Newp).
comprobar_aux(Persona,_,Sustantivo,[Adj|_],Adj,Newp):-
    obtener_ad(Adj,Ad),
    atributo_posicion(Sustantivo,Pos),
    obtener_pos(Pos,Persona,""),
    cambiar_dato(Pos,Ad,Persona,Newp),
    comprobacion_listas(Newp).
comprobar_aux(Persona,Rest,Sustantivo,[_|Adjetivos],Ad,Newp):-comprobar_aux(Persona,Rest,Sustantivo,Adjetivos,Ad,Newp).

comprobacion_listas(P):-personaje(A,P),%comprueba si el personaje construido concuerda completamente con alguno
    respuesta(bien_before,Ln),  %de los personajes de la base de datos y si es asi indica cual es y
    obtener_dato_random(Ln,Na),  %y vuelve a empezar
    write(Na),write(A),               %entradas: P: lista con las caracteristas del personaje que se esta construyendo
    respuesta(bien_after,Ln2),
    obtener_dato_random(Ln2,Na2),
    write(Na2),nl,nl,jugar.
comprobacion_listas(C):-personaje(_,N),comprobacion_individual(C,N).%manda a comprobar que exista algun personaje parecido al construido
                                                                    %entradas: C:  lista con las caracteristas del personaje que se esta construyendo
comprobacion_individual([D|C],[D|L]):-comprobacion_individual(C,L).%reglas para determinar si los atributos
comprobacion_individual([""|C],[_|L]):-comprobacion_individual(C,L).%de un personaje peretenecen a algun personaje
comprobacion_individual([],[]).                                     %establecido en la base de datos

obtener_dato_random(Ln,Dato):-%saca un dato random de una lista
longitud(Ln,N),               %Entradas: Ln:lista a sacar dato
    random(0,N,Nr),            %Dato: donde se guarda el dato de la lista
    obtener_pos(Nr,Ln,Dato).

preguntar(Persona):-%regla para empezar a preguntar al usuario para ir descubriendo al personaje
    random(0,2,1),    %pero de manera booleana
    %este random determina la razón de que la pregunta sea booleana
    posiciones_dato("",Persona,Ln),%entradas: Persona:La lista con los datos de la
    obtener_dato_random(Ln,Na),    %persona que se va descubriendo
    atributo_posicion(Aq,Na),
    preguntas_booleanas(Aq,Lq),
    obtener_dato_random(Lq,Q),
    write(Q),
    preguntar_booleano(Aq,Atri),
    write(Atri),
    write("?"),
    leer(Persona,Aq,Atri).

preguntar(Persona):-%realiza una pregunta normal al usuario para ir descubriendo al personaje
    posiciones_dato("",Persona,Ln),
    obtener_dato_random(Ln,Na),    %entradas: Persona:La lista con los datos de la
    atributo_posicion(Aq,Na),      %persona que se va descubriendo
    pregunta(Aq,Lq),
    obtener_dato_random(Lq,Q),
    write(Q),
    leer(Persona).

jugar:- % función a llamar para empezar el juego
    lista_x_ele_repetidos(5,"",L),%el primer numero de esta regla es la cantidad
    preguntar(L).                 %de atributos que tienen los personajes


