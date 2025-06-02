# Compilador: Mini Reto

Este es un proyecto donde se implementa un compilador sencillo en Python usando PLY (Lex-Yacc). 

Actualmente incluye:

- Analizador léxico completo

- Analizador sintáctico básico

- Soporte para estructuras fundamentales del lenguaje

- Analizador semántico básico

- Generación de código intermedio

- Reservación simple de recursos

- Ejecucion de código (máquina virtual)

- Recursión básica

## Requisitos

- Python (3.13 o mayor)
- PLY

## Guía de instalación para primera vez

Abre el proyecto con todos los archivos en el mismo directorio.

Abre la terminal y escribe:

```python -m venv venv```

Esto crea un entorno virtual en Python para la instalación de librearías de manera aislada.

Ahora "entramos" al entorno virtual:

```venv\Scripts\activate```

Después instalamos las librerías necesarias:

```pip install -r requirements.txt```

Ahora sí puedes ir a ```main.py``` y hacer pruebas.

## Características Implementadas

### Léxico

Identificadores: [a-zA-Z_][a-zA-Z0-9_]*

Constantes:

- Enteras: [0-9]+

- Flotantes: [0-9]+\.[0-9]+

- Cadenas: "[^"\n]*"

Operadores: +, -, *, /, =, !=, >, <

Delimitadores: ;, :, ,, (), {}, []

### Sintaxis Básica

\<Programa> -> program id ; \<Variables> \<Funciones> main \<Cuerpo> end

\<Variables> -> var \<ListaVars>

\<Funciones> -> void id(params) [ \<VarsLocales> \<Cuerpo> ] ;

\<Cuerpo> -> { \<Sentencias> }

### Semántica

- Validación de variables declaradas

- Type Checking

- Generación de cuádruplos (normales y en base a memoria)

- Flujos de control lineales y no lineales

### Generación de código intermedio

- Cuádruplos para expresiones

- Cuádruplos para estatutos lineales

- Cuádruplos para estatutos no lineales

- Cuádruplos para funciones

### Memoria

- Reservación simple de recursos

### Máquina virtual

Este proyecto cuenta con una máquina virtual que puede correr programas para el lenguaje BabyDuck.