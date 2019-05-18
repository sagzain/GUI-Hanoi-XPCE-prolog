%-------------------------------------------------------------------------------
%
%	    		 TORRES DE HANOI
%  		Autores: Iván Illán y Samuel González
%		     Programación Declarativa
%	Escuela Superior de Informatica, Universidad de Castilla La-Mancha
%
%-------------------------------------------------------------------------------

:- encoding(utf8).

%-------------------------------------------------------------------------------
%			Se carga las librerias XPCE
%-------------------------------------------------------------------------------

:- use_module(library(pce)).


%-------------------------------------------------------------------------------
%	Seleccionamos la carpeta que contiene los recursos que van a 
%			ser utilizados por el programa.
%-------------------------------------------------------------------------------

:- pce_image_directory('./resources/').


%-------------------------------------------------------------------------------
%	Marcamos como recursos a utilizar todas aquellas Imagenes contenidas
%	en la carpeta %seleccionada anteriormenta que van a ser dibujadas en
% 	nuestra aplicacion.
%-------------------------------------------------------------------------------

resource(portada, portada, image('portada.jpg')).
resource(titulo, titulo, image('titulo.jpg')).
resource(start, start, image('start.jpg')).
resource(back, back, image('back.jpg')).
resource(comenzar, comenzar, image('comenzar.jpg')).
resource(informacion, informacion, image('informacion.jpg')).
resource(salir, salir, image('salir.jpg')).
resource(manual, manual, image('manual.jpg')).
resource(instrucciones, instrucciones, image('instrucciones.jpg')).
resource(algoritmo, algoritmo, image('algoritmo.jpg')).
resource(ecuacion, ecuacion, image('ecuacion.jpg')).

%Imagenes correspondientes con el algoritmo de las torres de Hanoi
resource(torres, torres, image('torres.jpg')).
resource(blue, blue, image('blue.jpg')).
resource(cyan, cyan, image('cyan.jpg')).
resource(green, green, image('green.jpg')).
resource(yellow, yellow, image('yellow.jpg')).
resource(red, red, image('red.jpg')).


%-------------------------------------------------------------------------------
%	Predicados dinamicos que vamos a utilizar durante la ejecucion del
%	algoritmo
%-------------------------------------------------------------------------------

%Conteo del numero de discos actuales en la varilla (util para decidir a que
%altura vamos a poner el disco.
:- dynamic discos_en_varilla/2.

%Cantidad de discos que va a seleccionar el usuario para ejecutar el algoritmo
:- dynamic cantidad_discos/1.

%Conteo de numero de movimientos efectuados
:- dynamic movimientos/1.


%-------------------------------------------------------------------------------
%	Dada una imagen de la carpeta de recursos, crea dicha imagen y 
%	la coloca en la posicion que se le haya indicado en la ventana 
%	que se le haya indicado. 
%-------------------------------------------------------------------------------

imagen(Ventana, Figura, Imagen, Posicion) :- 
	new(Figura, figure),
	new(Bitmap, bitmap(resource(Imagen), @on)),
	send(Bitmap, name, 1),
	send(Figura, display, Bitmap),
	send(Figura, status, 1),
	send(Ventana, display, Figura, Posicion).		 


%-------------------------------------------------------------------------------
%    Elimina aquellas imagenes que ya no son necesarias, correspondientes con 
%    cada una de las pantallas por las que vamos a pasar.
%-------------------------------------------------------------------------------

%Liberamos las imagenes de la pantalla principal para poder pasar al resto 
%de pantallas.
liberar_principal :- 
		free(@titulo),
		free(@portada),
		free(@comenzar),
		free(@informacion), 
		free(@instrucciones),
		free(@salir).

%Liberamos las imagenes utilizadas en la ejecucion del algoritmo para regresar
%al menu principal.		
liberar_comenzar :- 
		free(@mov),
		free(@final),
		free(@titulo), 
		free(@numDiscos),
		free(@torres), 
		free(@start), 
		free(@back),
		free(@disco1),
		free(@disco2),
		free(@disco3),
		free(@disco4),
		free(@disco5).	
		
		
%Liberamos las imagenes utilizadas en la pantalla de informacion sobre el
%problema para poder regresar al menu principal.
liberar_informacion :- 
		free(@back),
		free(@manual).
	
%Liberamos las imagenes utilizadas en la pantalla de instrucciones del algoritmo
%para poder regresar al menu principal
liberar_instrucciones :- 
		free(@back),
		free(@algoritmo),
		free(@ecuacion).
		

%-------------------------------------------------------------------------------
% Hecho que nos van a ser utiles para conocer las coordenadas de los elementos
%-------------------------------------------------------------------------------

id_varilla(izquierda).
id_varilla(centro).
id_varilla(derecha).

id_disco(@disco1, 0, blue).
id_disco(@disco2, 1, cyan).
id_disco(@disco3, 2, green).
id_disco(@disco4, 3, yellow).
id_disco(@disco5, 4, red).

%Posicion del disco representa:
%posicion_disco(Nº del disco, Id de la Varilla, X)
%donde X es la posicion en la cual tiene que ser colocado el disco

%Para la primera varilla
posicion_disco(0,izquierda,223).
posicion_disco(1,izquierda,213).
posicion_disco(2,izquierda,203).
posicion_disco(3,izquierda,192).
posicion_disco(4,izquierda,180).

%Para la segunda varilla
posicion_disco(0,centro,377).
posicion_disco(1,centro,367).
posicion_disco(2,centro,357).
posicion_disco(3,centro,346).
posicion_disco(4,centro,334).

%Para la tercera varilla
posicion_disco(0,derecha,536).
posicion_disco(1,derecha,522).
posicion_disco(2,derecha,512).
posicion_disco(3,derecha,502).
posicion_disco(4,derecha,487).


%-------------------------------------------------------------------------------
%  		     Creacion de la ventana de la aplicacion
%-------------------------------------------------------------------------------

mostrar_GUI :- 
%Comenzamos con la creacion de la ventana principal en la cual van a ocurrir los
% eventos.
	new(Menu, window('Interfaz grafica: Torres de Hanoi', size(800,600))),
	
%Hacemos uso de la clausula mostrar_principal para crear las imagenes que tiene
%la ventana principal.
	mostrar_principal(Menu).	

%-------------------------------------------------------------------------------
%		Generacion de la pantalla principal de la aplicacion
%-------------------------------------------------------------------------------
mostrar_principal(Menu) :-

	liberar_comenzar,
	liberar_informacion,
	liberar_instrucciones,
%Haciendo uso del predicado imagen definido anteriormente, cargamos en las
%posiciones indicadas, las imagenes indicadas en la ventana Menu.
	imagen(Menu, @titulo, titulo, point(150, 0)),	
	imagen(Menu, @portada, portada, point(150,150)),
	imagen(Menu, @comenzar, comenzar, point(325, 370)),
	imagen(Menu, @informacion, informacion, point(315,435)),
	imagen(Menu, @instrucciones, instrucciones, point(310,490)),
	imagen(Menu, @salir, salir, point(650,500)),

%Establecemos para aquellos menus pulsables, el reconocimiento de la entrada
%del click del raton, así como la accion a realizar si dicho click es 
%reconocido en el elemento y los parámetros a pasar.
	send(@comenzar, recogniser, click_gesture(left, '', single, message(
	@prolog, mostrar_ejecucion, Menu))),
	send(@informacion, recogniser, click_gesture(left, '', single, message(
	@prolog, mostrar_informacion, Menu))),
	send(@instrucciones, recogniser, click_gesture(left, '', single, 
	message(@prolog, mostrar_instrucciones, Menu))),
	send(@salir, recogniser, click_gesture(left, '', single, message(Menu, 
	destroy))),
	
%Indicamos que cuando vayamos a pasar por un menu seleccionable, el cursor 
%cambie e indique que es posible ser pulsado.
	send(@comenzar, cursor, hand2),
	send(@informacion, cursor, hand2),
	send(@instrucciones, cursor, hand2),
	send(@salir, cursor, hand2),

%Centramos la ventana en la pantalla.
	send(Menu, open_centered).


%-------------------------------------------------------------------------------
%		Generacion de la pantalla de ejecucion del algoritmo
%-------------------------------------------------------------------------------

mostrar_ejecucion(Menu) :-
	liberar_principal,
%Volvemos a crear y utilizar el titulo de la ventana principal (el cual al pasar
%de pantalla habria sido liberado de memoria.
	imagen(Menu, @titulo, titulo, point(150, 0)),



%Creacion de un elemento de texto el cual va a contener el numero de discos que
%el usuario va a poder seleccionar asi como el rango del panel de texto, el cual
%para poder ejecutar el algoritmo va a poder ir desde 2 discos como minimo 
%hasta 6 como maximo.
	send(Menu, display, new(@numDiscos, int_item('Nº de discos ', 2)), 
	point(260,215)),

	send(@numDiscos, range(low :=2, high := 5)),
	

%Creacion y posicionamiento de las imagenes que va a contener para la ejecucion
%del algoritmo, asi como los botones para efectuar el calculo y para regresar
%a la pantalla principal.
	imagen(Menu, @back, back, point(55,510)),	%Imagen de retroceso
	imagen(Menu, @start, start, point(450,210)),	%Imagen de inicio
	imagen(Menu, @torres, torres, point(160, 336)), %Varillas del algoritmo
	
%Mostramos un texto con la cantidad de movimientos que se han efectuado
	send(Menu, display, text('Nº de movimientos: '), point(300,270)),
	send(Menu, display, new(@mov, text(0)), point(450, 270)),

%De nuevo establecemos el reconocimiento del click del raton para quellos menus
%que puedan ser pulsados y ejecuten alguna accion. 
	send(@start, recogniser, click_gesture(left, '', single, message(@prolog,
	ejecutar, Menu, @numDiscos?selection))),
	send(@back, recogniser, click_gesture(left, '', single, message(@prolog,
	mostrar_principal, Menu))),

%Cambiar el cursor al pasar por encima de las imagenes mostrar que es pulsable.
	send(@back, cursor, hand2),	
	send(@start, cursor, hand2).


%-------------------------------------------------------------------------------
%	Generacion de la pantalla de informacion de las torres de hanoi
%-------------------------------------------------------------------------------

mostrar_informacion(Menu) :- 
	liberar_principal,
%Creacion del panel de informacion que va a contener la explicacion de en que 
%consiste el problema de las torres de hanoi, asi como un boton que permita
%regresar hacia la pantalla principal para seleccionar otra opcion.
	imagen(Menu, @manual, manual, point(70,0)),
	imagen(Menu, @back, back, point(55,520)),

%Reconocimiento del click del raton y accion a efectuar.
	send(@back, recogniser, click_gesture(left, '', single, message(@prolog,
	mostrar_principal, Menu))),

%Cambiar el cursor al pasar por encima de las imagenes mostrar que es pulsable.
	send(@back, cursor, hand2).


%-------------------------------------------------------------------------------
%	Generacion de la pantalla con la explicacion sobre el algoritmo
%-------------------------------------------------------------------------------

mostrar_instrucciones(Menu) :-	
%Ya que unicamente podemo venir desde la pantalla principal vamos a liberar 
%todos las imagenes que se estaban utilizando para asi poder crear las nuevas.
	liberar_principal,

%Creamos y mostramos en la posicion indicada la imagen que explica en que se
%basa la ejecucion del algoritmo para obtener una solucion de las torres de 
%hanoi y la imagen para retroceder.
	imagen(Menu, @algoritmo, algoritmo, point(150,50)),	
	imagen(Menu, @back, back, point(55,520)),
	imagen(Menu, @ecuacion, ecuacion, point(200 ,470)),

%Establecemos la accion de volver al menu principal al pulsar la imagen de 
%retroceso
	send(@back, recogniser, click_gesture(left, '', single, message(@prolog, 
	mostrar_principal, Menu))),

%Cambio del icono del puntero para mostrar que es posible ser pulsado.
	send(@back, cursor, hand2).


%-------------------------------------------------------------------------------
%				COMIENZO DEL PROGRAMA
%-------------------------------------------------------------------------------

%Con este hecho comenzamos con la ejecucion de la interfaz grafica del programa
:- mostrar_GUI.


%-------------------------------------------------------------------------------
%Usa todos aquellos predicados necesarios para comenzar con la ejecucion del 
%algoritmo.
%-------------------------------------------------------------------------------
ejecutar(Menu, N) :-
	reiniciar(Menu),
	crear_discos(Menu,N), 
	hanoi(Menu, N),
	final(Menu).

%-------------------------------------------------------------------------------
%	Elimina de la interfaz grafica todas aquellas imagenes de los discos
%	y reinicia el contador de movimientos y elimina el texto de finalizado.
%	Asi mismo reinicia los predicados dinamicos para no contener los
%	valores de una ejecucion anterior.
%-------------------------------------------------------------------------------
reiniciar(Menu) :-
	free(@final),
	free(@mov),
	send(Menu, display, new(@mov, text(0)), point(450, 270)),
	free(@disco1),
	free(@disco2),
	free(@disco3),
	free(@disco4),
	free(@disco5),

	retractall(discos_en_varilla(_,_)),
	retractall(movimientos(_)),
	
	assert(movimientos(0)),
	
	assert(discos_en_varilla(centro,0)),
	assert(discos_en_varilla(derecha,0)).


%-------------------------------------------------------------------------------
%	Predicados para la creacion de las imagenes de los discos segun 
%	el numero de discos introducidos por el usuario a traves de la
% 	interfaz grafica.
%-------------------------------------------------------------------------------

%Crear discos para la seleccion de 2 discos
crear_discos(Menu, 2) :-
%Se asignan la cantidad de discos que se han creado y la varilla
%correspondiente (al inicio del algoritmo, los discos estan en la varilla 
%izquierda) al predicado dinamico discos_en_varilla.
	assert(discos_en_varilla(izquierda,2)),

%Creamos las imagenes correspondientes a los 2 discos
	imagen(Menu, @disco2, cyan, point(213,446)),
	imagen(Menu, @disco1, blue, point(223,426)),
	send(timer(1.5), delay).

%Crear discos para la seleccion de 3 discos
crear_discos(Menu, 3) :-
%Igual que en el resto de predicados crear_discos
	assert(discos_en_varilla(izquierda,3)),

%Creamos las imagenes correspondientes a los 3 discos
	imagen(Menu, @disco3, green, point(203,446)),
	imagen(Menu, @disco2, cyan, point(213,426)),
	imagen(Menu, @disco1, blue, point(223,406)),
	send(timer(1.5), delay).

%Crear discos para la seleccion de 4 discos
crear_discos(Menu, 4) :-
%Igual que en el resto de predicados crear_discos
	assert(discos_en_varilla(izquierda,4)),

%Creamos las imagenes correspondientes a los 4 discos
	imagen(Menu, @disco4, yellow, point(192, 446)),
	imagen(Menu, @disco3, green, point(203,426)),
	imagen(Menu, @disco2, cyan, point(213,406)),
	imagen(Menu, @disco1, blue, point(223,386)),
	send(timer(1.5), delay).

%Crear discos para la seleccion de 5 discos
crear_discos(Menu, 5) :-
%Igual que en el resto de predicados crear_discos
	assert(discos_en_varilla(izquierda,5)),

%Creamos las imagenes correspondientes a los 5 discos
	imagen(Menu, @disco5, red, point(180, 446)),
	imagen(Menu, @disco4, yellow, point(192, 426)),
	imagen(Menu, @disco3, green, point(203,406)),
	imagen(Menu, @disco2, cyan, point(213,386)),
	imagen(Menu, @disco1, blue, point(223,366)),
	send(timer(1.5), delay).


%-------------------------------------------------------------------------------
% Predicado que se encarga de avisar al usuario de que el algoritmo ha acabado
%-------------------------------------------------------------------------------

final(Menu) :- 
	send(Menu, display, new(@final, 
	text('El algoritmo ha finalizado. ¡Puedes ejecutarlo de nuevo!')), 
	point(250,535)).


%-------------------------------------------------------------------------------
%		4ALGORITMO DEL PROBLEMA DE LAS TORRES DE HANOI
%-------------------------------------------------------------------------------

hanoi(Menu, N) :- mover(Menu, N, izquierda, centro, derecha).

mover(Menu, 0, _, _, _) :- !.
mover(Menu, N, A, B, C) :- M is N-1,
		    mover(Menu, M, A, C, B),
		    send(timer(1.6), delay),
		    mover_disco(Menu, M, A, B),
		    mover(Menu, M, C, B, A).


%-------------------------------------------------------------------------------
%	Predicado encargado de mover las imagenes de los discos y de 
%	actualizar los predicados dinamicos.
%-------------------------------------------------------------------------------

mover_disco(Menu, DiscID, Origen, Destino):-
%Accedemos a los hechos de id_disco y posicion_disco para poder utilizar los 
%valores correspondientes al disco actual.
	id_disco(Disco, DiscID, Color),
	posicion_disco(DiscID, Destino, PosicionX),

%Obtenemos del predicado dinamico la cantidad de discos que hay en la varilla 
%de origen y se modifica la cantidad, restandole uno.
	discos_en_varilla(Origen, DiscosOrigen),
	NuevaCantidadOrigen is DiscosOrigen-1,
	retract(discos_en_varilla(Origen,_)),
	assert(discos_en_varilla(Origen,NuevaCantidadOrigen)),

%Se libera la imagen de la pantalla para la creacion de la nueva imagen del
%disco en la nueva posicion
	free(Disco),

%Liberamos el texto que contiene la cantidad de movimientos para poner el nuevo
	free(@mov),

%Obtenemos del predicado dinamico la cantidad de discos que hay en la varilla 
%de destino y se modifica la cantidad, sumandole uno.
	discos_en_varilla(Destino, DiscosDestino),
	NuevaCantidadDestino is DiscosDestino+1,
	retract(discos_en_varilla(Destino,_)),
	assert(discos_en_varilla(Destino,NuevaCantidadDestino)),
	
%Hacemos lo mismo con el predicado dinamico de movimientos, obtenemos el valor,
%lo incrementamos, y lo actualizamos.
	movimientos(Movimientos),
	NuevoMovimientos is Movimientos+1,
	retractall(movimientos(_)),
	assert(movimientos(NuevoMovimientos)),

%Creacion de la nueva imagen cogiendo la posicion correspondiente al disco y 
%a la varilla a la que se va a mover (para el eje X) y se calcula su posicion
%en el eje Y segun la cantidad de discos que tenga una varilla (19 es el tamaño 
%de alto de cada disco).	
	imagen(Menu, Disco, Color, point(PosicionX,446-(19*(NuevaCantidadDestino-1)))),

%Mostramos en pantalla la nueva cantidad de movimientos 
	send(Menu, display, new(@mov, text(NuevoMovimientos)), point(450, 270)).

