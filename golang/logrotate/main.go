package main

import (
	"fmt"
	"log"
	"os"
	"path/filepath"
	"time"
)

type Logger struct {
	logger *log.Logger
	path   string
}

func (l *Logger) init() {
	hostname, err := os.Hostname()
	if err != nil {
		hostname = "default"
	}
	logPath := filepath.FromSlash("/tmp/log")
	l.path = filepath.FromSlash(fmt.Sprintf("%v/%v.log", logPath, hostname))
	l.check()
}

func (l *Logger) check() {
	if l.logger == nil {
		file, _ := os.OpenFile(l.path, os.O_APPEND|os.O_CREATE|os.O_WRONLY, 0600)
		l.logger = log.New(file, "", 0)
	} else {
		if _, err := os.Stat(l.path); os.IsNotExist(err) {
			file, _ := os.OpenFile(l.path, os.O_APPEND|os.O_CREATE|os.O_WRONLY, 0600)
			if file, ok := l.logger.Writer().(*os.File); ok {
				file.Close()
			}
			l.logger = log.New(file, "", 0)
		}
	}
}

func (l *Logger) RecordActivity(activity *string) {
	l.check()
	l.logger.Println(fmt.Sprint(time.Now().UTC().Format("2006-01-02T15:04:05.999Z") + " [DEBUG] " + *activity))
	if file, ok := l.logger.Writer().(*os.File); ok {
		_ = file.Sync()
	}
}

func main() {
	fmt.Println("hello world")

	var log Logger
	log.init()

	go func(msg string) {
		for {
			log.RecordActivity(&msg)
			time.Sleep(5000 * time.Millisecond)

		}
	}("going")
	time.Sleep(100000000 * time.Millisecond)

}
