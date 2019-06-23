package main

import (
	"encoding/json"
	"fmt"
	"log"
	"strings"
)

type Person struct {
	Name  string   `json:"fullname"`
	Age   int      `json:"age"`
	Phone []string `json:"person,omitempty"`
}

func main() {
	var jsonStr = `{ "fullname": "John Doe", "age": 21, "phone": ["512-555-1234", "512-555-5555"] }`
	decodeJsonMap(jsonStr)
	decodeJsonObj(jsonStr)

	jane := Person{
		Name:  "Jane Doe",
		Age:   40,
		Phone: []string{"512-555-9999", "512-555-5555"},
	}

	if j, err := json.Marshal(jane); err != nil {
		log.Printf("Unable to marshal struct to json: %v", err)

	} else {
		fmt.Printf("JSON String: %s\n", string(j))
	}
}

func decodeJsonMap(jsonStr string) {
	var pMap map[string]interface{}
	decoder := json.NewDecoder(strings.NewReader(jsonStr))
	if err := decoder.Decode(&pMap); err != nil {
		log.Printf("Unable to unmarshal json: %v", err)
	} else {
		fmt.Printf("Name: %v\n", pMap["fullname"])
	}
}

func decodeJsonObj(jsonStr string) {
	person := Person{}
	decoder := json.NewDecoder(strings.NewReader(jsonStr))
	if err := decoder.Decode(&person); err != nil {
		log.Printf("Unable to unmarshal json: %v", err)
	} else {
		fmt.Printf("%#v\n", person)
	}
}
