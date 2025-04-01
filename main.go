package main

import (
	"container/list"
	"fmt"
)

type Stack[T any] struct {
	items []T
}

// Metodo para hacer push a la pila. Recibe un parametro de tipo T y lo agrega a la pila
func (s *Stack[T]) Push(item T) {
	s.items = append(s.items, item) // agregamos el elemento al final de la pila
}

// Metodo para hacer pop de la pila. Regresa el elemento que se saco de la pila y un booleano
func (s *Stack[T]) Pop() (T, bool) {
	if s.IsStackEmpty() { // si la pila esta vacia
		var zero T         // regresa 0 para tipos numericos, false para bool, nil para punteros, etc
		return zero, false // regresamos 0 y false para indicar que no se pudo hacer pop
	}

	item := s.items[len(s.items)-1]    // rescatamos el ultimo elemento para regresarlo
	s.items = s.items[:len(s.items)-1] // ahora eliminamos el ultimo elemento de la pila
	return item, true                  // regresamos el elemento rescatado y true para indicar que se pudo hacer pop
}

// Metodo para ver el ultimo elemento de la pila
func (s *Stack[T]) Peek() T {
	if s.IsStackEmpty() {
		var zero T
		return zero
	}

	return s.items[len(s.items)-1]
}

// Metodo para ver si la pila esta vacia
func (s *Stack[T]) IsStackEmpty() bool {
	return len(s.items) == 0
}

type Queue[T any] struct {
	items *list.List // usamos un list.List para almacenar los elementos de la cola
}

// Metodo para crear una nueva cola
func NewQueue[T any]() *Queue[T] {
	return &Queue[T]{items: list.New()} // regresamos un apuntador a una nueva cola
}

// Metodo para agregar un elemento a la cola
func (q *Queue[T]) Add(item T) {
	q.items.PushBack(item) // agregamos el elemento al final de la cola
}

// Metodo para ver si la cola esta vacia
func (q *Queue[T]) IsQueueEmpty() bool {
	return q.items.Len() == 0
}

// Metodo para sacar un elemento de la cola
func (q *Queue[T]) Remove() (T, bool) {
	if q.IsQueueEmpty() { // si la cola esta vacia
		var zero T
		return zero, false
	}

	item := q.items.Front().Value.(T)
	q.items.Remove(q.items.Front())
	return item, true
}

// Metodo para ver el primer elemento de la cola
func (q *Queue[T]) Peek() T {
	if q.IsQueueEmpty() {
		var zero T
		return zero
	}

	return q.items.Front().Value.(T)
}

func main() {
	// Stack
	fmt.Println("\nPruebas de Stack")
	s := Stack[int]{}

	// Test 1: Agregar un elemento a la pila y ver el ultimo elemento
	s.Push(13)
	fmt.Println("Test 1 resultado: ", s.Peek()) // 13

	// Test 2: Agregar dos elementos a la pila y ver el ultimo elemento
	s.Push(24)
	s.Push(35)
	fmt.Println("Test 2 resultado: ", s.Peek()) // 35

	// Test 3: Hacer pop de la pila y ver el ultimo elemento
	s.Pop()
	fmt.Println("Test 3 resultado: ", s.Peek()) // 24

	// Test 4: Hacer pop dos veces de la pila y ver el ultimo elemento
	s.Pop()
	s.Pop()
	fmt.Println("Test 4 resultado: ", s.Peek()) // 0

	// Test 5: Hacer pop de una pila vacia y ver el reultado
	sr1, sr2 := s.Pop()
	fmt.Println("Test 5 resultado: ", sr1, sr2) // 0 false

	// Test 6: Verificar si la pila está vacía
	s.Push(49)
	fmt.Println("Test 6.1 resultado: ", s.IsStackEmpty()) // false
	s.Pop()
	fmt.Println("Test 6.2 resultado: ", s.IsStackEmpty()) // true

	// Queue
	fmt.Println("\nPruebas de Queue")
	q := NewQueue[int]()

	// Test 1: Agregar un elemento a la cola y ver el primer elemento
	q.Add(1)
	fmt.Println("Test 1 resultado: ", q.Peek()) // 1

	// Test 2: Agregar dos elementos a la cola y ver el primer elemento
	q.Add(3)
	q.Add(5)
	fmt.Println("Test 2 resultado: ", q.Peek()) // 1

	// Test 3: Eliminar primer elemento de la cola y ver el nuevo primer elemento
	q.Remove()
	fmt.Println("Test 3 resultado: ", q.Peek()) // 3

	// Test 4: Eliminar dos elementos de la cola y ver el nuevo primer elemento
	q.Remove()
	q.Remove()
	fmt.Println("Test 4 resultado: ", q.Peek()) // 0

	// Test 5: Eliminar un elemento de una cola vacia y ver el resultado
	qr1, qr2 := q.Remove()
	fmt.Println("Test 5 resultado: ", qr1, qr2) // 0 false

	// Test 6: Verificar si la cola está vacía
	q.Add(48)
	fmt.Println("Test 6.1 resultado: ", q.IsQueueEmpty()) // false
	q.Remove()
	fmt.Println("Test 6.2 resultado: ", q.IsQueueEmpty()) // true

	// Map
	fmt.Println("\nPruebas de Map")
	m := make(map[string]int)

	// Test 1: Agregar un elemento al mapa y ver el mapa
	m["a"] = 1
	fmt.Println("Test 1 resultado: ", m) // map[a:1]

	// Test 2: Agregar dos elementos al mapa y ver el mapa
	m["b"] = 2
	m["c"] = 3
	fmt.Println("Test 2 resultado: ", m) // map[a:1 b:2 c:3]

	// Test 3: Eliminar un elemento del mapa y ver el mapa
	delete(m, "b")
	fmt.Println("Test 3 resultado: ", m) // map[a:1 c:3]

	// Test 4: Ver un elemento del mapa
	fmt.Println("Test 4 resultado: ", m["a"]) // 1

	// Test 5: Ver un elemento que no existe en el mapa
	fmt.Println("Test 5 resultado: ", m["b"]) // 0

	// Test 6: Cambiar el valor de un elemento del mapa y ver el mapa
	m["a"] = -1
	fmt.Println("Test 6 resultado: ", m) // map[a:-1 c:3]

	// Test 7: Eliminar un elemento que no existe en el mapa
	delete(m, "b")
	fmt.Println("Test 7 resultado: ", m) // map[a:-1 c:3]

	// Test 8: Verificar si un elemento existe en el mapa
	_, exists := m["a"]
	fmt.Println("Test 8.1 resultado: ", exists) // true
	_, exists = m["b"]
	fmt.Println("Test 8.2 resultado: ", exists) // false
}
