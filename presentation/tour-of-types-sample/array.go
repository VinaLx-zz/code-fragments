package main

import (
	"fmt"
)

func array() {
	dynamic_array := make([]int, 0)
	for i := 0; i < 100; i++ {
		// the array can grow at runtime
		append(dynamic_array, i)
	}
	for i := 0; i < len(dynamic_array); i++ {
		fmt.Println(dynamic_array[i])
	}
}
