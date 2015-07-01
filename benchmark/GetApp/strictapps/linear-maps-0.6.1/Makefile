
Exercises.html: Exercises.pandoc graph.png type.png
	pandoc -t s5 -S -C header.html $< >$@

Intro.html: Intro.pandoc
	pandoc -t s5 -S -C header.html $< >$@

graph.png type.png: %.png: %.dot
	dot -Tpng -Granksep=0.3 -o $@ $<


