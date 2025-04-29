# Compilador: Mini Reto

Este es un proyecto donde se implementa un compilador sencillo en Python usando PLY (Lex-Yacc). 

Actualmente incluye:

- Analizador léxico completo

- Analizador sintáctico básico

- Soporte para estructuras fundamentales del lenguaje

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

## Requisitos

- Python 3.13
- PLY (`pip install ply`)