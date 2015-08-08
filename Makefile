all: play_icfp2015

play_icfp2015: hextris.lisp package.lisp compile.lisp
	sbcl --noinform --disable-debugger --load compile.lisp
