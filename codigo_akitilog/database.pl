personaje("Nery Brenes",['1985',limon,masculino,'34',deportista]).
personaje("Andrea Vargas",['1996',sanjose,femenino,'24',deportista]).
personaje("Andrey Amador",['1986',alajuela,masculino,'33',deportista]).
personaje("Keylor Navas",['1986',sanjose,masculino,'33',deportista]).
personaje("Claudia Poll",['1972',nicaragua,femenino,'47',deportista]).
personaje("Yokasta Valle",['1992',nicaragua,femenino,'27',boxeadora]).
personaje("Hanna Gabriels",['1983',alajuela,femenino,'37',boxeadora]).
personaje("Joel Campbell",['1992',sanjose,masculino,'27',deportista]).
personaje("Bryan Ruiz",['1985',sanjose,masculino,'34',deportista]).
personaje("Alvaro Saborio",['1982',alajuela,masculino,'38',deportista]).
personaje("Randall Brenes",['1983',cartago,masculino,'36',deportista]).
personaje("Daniel Salas",['1977',sanjose,masculino,'43',politico]).
personaje("Mauricio Hoffman",['1983',sanjose,masculino,'36',presentador]).
personaje("Franklin Chang",['1950',sanjose,masculino,'70',cientifico]).
personaje("Debi Nova",['1980',sanjose,femenino,'39',cantante]).
personaje("Hernan Jimenez",['1980',sanjose,masculino,'40',comediante]).
personaje("Maribel Guardia",['1959',sanjose,femenino,'61',presentador]).
personaje("Nancy Dobles",['1977',sanjose,femenino,'42',presentador]).
personaje("Ronald Matarrita",['1994',alajuela,masculino,'25',deportista]).
personaje("Oscar Arias",['1940',sanjose,masculino,'79',politico]).
personaje("Carlos Alvarado",['1980',sanjose,masculino,'40',politico]).

adjetivos(femenino,[mujer,femenino,hembra]).
adjetivos(masculino,[hombre,varon,masculino,macho]).
adjetivos(deportista,[corredora,atleta,corredor,jugador,portero,futbol,delantero,deportista,futbolista,ciclista,nadadora,nadador]).
adjetivos(boxeadora,[boxea,boxeador,boxeadora,lucha,pelea,ring]).
adjetivos(presentador,[television,actor,actriz,presentador,presentadora]).
adjetivos(cientifico,[fisico,f�sico,f�sica,astronauta,investigador,cientifica,fisica,biologa,cientifico]).
adjetivos(politico,[presidente,pol�tica,pol�tico,expresidente,ministro,ministra,diputado,diputada,politico]).
adjetivos(cantante,[cantante,solista,cantautor,canta]).
adjetivos(comediante,[comediante,standup,cuentachistes,reir,comedia]).
adjetivos(nicaragua,[matagalpa,managua,nicaragua,nicaraguense]).
adjetivos(limon,[limonense,limon,lim�n]).
adjetivos(cartago,[cartagines,cartago]).
adjetivos(alajuela,[alajuelense,sanramon,alajuela]).
adjetivos(sanjose,[josefino,puriscal,perezzeledon,sanjose]).

adjetivos('1985',['1985',1985]).
adjetivos('1996',['1996']).
adjetivos('1986',['1986']).
adjetivos('1972',['1972']).
adjetivos('1992',['1992']).
adjetivos('1983',['1983']).
adjetivos('1985',['1985']).
adjetivos('1982',['1982']).
adjetivos('1983',['1983']).
adjetivos('1950',['1950']).
adjetivos('1959',['1959']).
adjetivos('1977',['1977']).
adjetivos('1994',['1994']).
adjetivos('1940',['1940']).
adjetivos('1980',['1980']).



adjetivos('34',['34']).
adjetivos('24',['24']).
adjetivos('33',['33']).
adjetivos('47',['47']).
adjetivos('27',['27']).
adjetivos('37',['37']).
adjetivos('34',['34']).
adjetivos('38',['38']).
adjetivos('36',['36']).
adjetivos('43',['43']).
adjetivos('70',['70']).
adjetivos('39',['39']).
adjetivos('61',['61']).
adjetivos('42',['42']).
adjetivos('25',['25']).
adjetivos('79',['79']).
adjetivos('40',['40']).

referencia_sinonimos(fecha, [fecha,a�o]).
referencia_sinonimos(lugar, [lugar,provincia,canton,estado,distrito]).
referencia_sinonimos(genero, [genero,identidad,sexo]).
referencia_sinonimos(edad, [a�os,edad]).
referencia_sinonimos(profesion, [trabajo,profesion,ocupacion,empleo]).

adj_preguntas(fecha,[1985,1996,1986,1972,1992,1983,1985,1982,1980,1940,1977,1959]).
adj_preguntas(lugar,[sanjose,alajuela,cartago,limon,nicaragua]).
adj_preguntas(genero,[masculino,femenino]).
adj_preguntas(edad,[34,24,33,47,27,37,38,36,43,36,70,39,40,61,42,25,79,40]).
adj_preguntas(profesion,[comediante,politico,deportista,cientifico,cantante]).

atributo_posicion(fecha,0).
atributo_posicion(lugar,1).
atributo_posicion(genero,2).
atributo_posicion(edad,3).
atributo_posicion(profesion,4).

referencia_atributos(tiene,[edad]).
referencia_atributos(es,[profesion,lugar,genero,fecha]).
referencia_atributos(nacio,[fecha,lugar]).
referencia_atributos(dar,[edad,profesion]).


referencia_verbos(dar,[dar,dado]).
referencia_verbos(tiene,[tiene]).
referencia_verbos(es,[es,fue,ejerce,hacer,hace,era,eran,gobierna,gobernar,juega]).
referencia_verbos(nacio,[nacio,nace,naci�]).

afirmaciones([si,s�,efectivo,correcto,efectivamente,claro]).

preguntas_booleanas(fecha,["�tu personaje naci� en  ","la fecha de nacimiento es "]).
preguntas_booleanas(lugar,["tu personaje naci� en  ","tu personaje es originario de "]).
preguntas_booleanas(genero,["tu personaje es  "]).
preguntas_booleanas(edad,["tu personaje tiene  ","la edad de tu personaje es"]).
preguntas_booleanas(profesion,["tu personaje es  "]).

pregunta(fecha,["en que a�o naci� tu personaje?", "dime en que a�o nacio tu personaje"]).
pregunta(lugar,["en que provincia nacio?"]).
pregunta(genero,["Cual es el genero","es masculino o femenino?"]).
pregunta(edad, ["cuantos a�os tiene?", "cuantas vueltas al sol le ha dado?"]).
pregunta(profesion,["a que se dedica tu personaje?", "cual es el trabajo de tu personaje?"]).

limpieza([',','.',':',';','?','�','!','�']).

respuesta(mal,["la persona no se encuentra en la base de datos,piense en otra","un agujero negro se lo trago, piensa en otra"]).
respuesta(bien_before,["tu personaje es ","muy sencillo, es "]).
respuesta(bien_after,[", no ha sido ningun problema", ", m�s dificil la proxima vez",""]).
respuesta(vacio,["responde algo, no seas timido,pero bien escrito","no te ense�aron a escribir?"]).
respuesta(mala_estructura,["escribe correctamente","mala forma de escribir, trata con otras palabras o una buena sintaxis"]).






