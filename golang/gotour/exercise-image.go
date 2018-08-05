package main

import (
	"golang.org/x/tour/pic"
	"image"
	"image/color"
	"math/rand"
	"time"
)

type Image struct {
	w, h int
	fn   func(int, int) int
}

func (_ Image) ColorModel() color.Model {
	return color.RGBAModel
}

func (i Image) Bounds() image.Rectangle {
	return image.Rect(0, 0, i.w, i.h)
}

func (i Image) At(x, y int) color.Color {
	v := uint8(i.fn(x, y))
	return color.RGBA{v, v, 255, 255}
}

func main() {
	rand.Seed(time.Now().UTC().UnixNano())
	m := []Image{
		Image{256, 256, func(x, y int) int { return x ^ y }},
		Image{256, 256, func(x, y int) int { return x * y }},
		Image{256, 256, func(x, y int) int { return (x + y) / 2 }},
	}

	pic.ShowImage(m[rand.Intn(len(m))])
}
