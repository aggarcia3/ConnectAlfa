// Agente player.asl en proyecto ConnectAlfa.mas2j

/* Creencias y reglas iniciales */

/* Objetivos iniciales */

/* Planes */

// Jugar aleatoriamente
+turno(Yo)[source(percept)] : .my_name(Yo) <-
	.random(R);
	put(R * 7);
	-turno(Yo)[source(percept)].
