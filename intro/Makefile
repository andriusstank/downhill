all: html/part1.html html/part2.html html/part3.html html/style.css

html/part1.html: part1.md
	pandoc --standalone --mathjax --css style.css -f markdown -t html part1.md -o html/part1.html
html/part2.html: part2.md
	pandoc --standalone --mathjax --css style.css -f markdown -t html part2.md -o html/part2.html
html/part3.html: part3.md
	pandoc --standalone --mathjax --css style.css -f markdown -t html part3.md -o html/part3.html
html/style.css: style.css
	cp style.css html/

.PHONY: all
