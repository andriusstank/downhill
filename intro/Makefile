all: part1.html part2.html part3.html

part1.html: part1.md
	pandoc --standalone --mathjax --css style.css -f markdown -t html part1.md -o part1.html
part2.html: part2.md
	pandoc --standalone --mathjax --css style.css -f markdown -t html part2.md -o part2.html
part3.html: part3.md
	pandoc --standalone --mathjax --css style.css -f markdown -t html part3.md -o part3.html

.PHONY: all
