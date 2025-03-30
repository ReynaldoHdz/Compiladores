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
	items *list.List
}

// Metodo para crear una nueva cola
func NewQueue[T any]() *Queue[T] {
	return &Queue[T]{items: list.New()}
}

// Metodo para agregar un elemento a la cola
func (q *Queue[T]) Add(item T) {
	q.items.PushBack(item)
}

// Metodo para ver si la cola esta vacia
func (q *Queue[T]) IsQueueEmpty() bool {
	return q.items.Len() == 0
}

// Metodo para sacar un elemento de la cola
func (q *Queue[T]) Remove() (T, bool) {
	if q.IsQueueEmpty() {
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
	s := Stack[int]{}
	s.Push(13)                               // 13
	fmt.Println("Ultimo numero: ", s.Peek()) // 13
	s.Push(24)                               // 13, 24
	s.Push(35)                               // 13, 24, 35
	fmt.Println("Ultimo numero: ", s.Peek()) // 35
	s.Pop()                                  // 13, 24
	fmt.Println("Ultimo numero: ", s.Peek()) // 24
	fmt.Println(s.Pop())
	fmt.Println("Ultimo numero: ", s.Peek()) // 13
	s.Pop()
	fmt.Println("Ultimo numero: ", s.Peek()) // 0
	fmt.Println(s.Pop())                     // 0

	// Queue
	q := NewQueue[int]()
	q.Add(1)                                 // 1
	q.Add(3)                                 // 1, 3
	fmt.Println("Primer numero: ", q.Peek()) // 1
	q.Add(5)                                 // 1, 3, 5
	fmt.Println("Primer numero: ", q.Peek()) // 1
	fmt.Println(q.Remove())                  // 1	// 3, 5
	fmt.Println("Primer numero: ", q.Peek()) // 3

	// Map
	m := make(map[string]int)
	m["a"] = 1
	m["b"] = 2
	m["c"] = 3
	fmt.Println(m) // map[a:1 b:2 c:3]
	delete(m, "b")
	fmt.Println(m)      // map[a:1 c:3]
	fmt.Println(m["a"]) // 1
	fmt.Println(m["b"]) // 0
	fmt.Println(m["c"]) // 3
	m["d"] = 4
	fmt.Println(m) // map[a:1 c:3 d:4]
}
