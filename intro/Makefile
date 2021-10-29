all: html/part1.html html/part2.html html/part3.html html/style.css html/inline_fwd.dot.svg html/inline_back.dot.svg

html/part1.html: part1.md
	pandoc --standalone --mathjax --css style.css -f markdown -t html part1.md -o html/part1.html
html/part2.html: part2.md
	pandoc --standalone --mathjax --css style.css -f markdown -t html part2.md -o html/part2.html
html/part3.html: part3.md
	pandoc --standalone --mathjax --css style.css -f markdown -t html part3.md -o html/part3.html
html/style.css: style.css
	cp style.css html/

html/inline_fwd.dot.svg: inline_fwd.dot
	dot inline_fwd.dot -Tsvg -ohtml/inline_fwd.dot.svg
html/inline_back.dot.svg: inline_back.dot
	dot inline_back.dot -Tsvg -ohtml/inline_back.dot.svg

.PHONY: all
