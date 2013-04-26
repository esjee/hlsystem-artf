Lindenmayer system in Haskell using SDL and OpenGL.
---
This is my first attempt at creating something in haskell that was not straight
out of a book, so please be nice. Pull requests or comments on what should be
improved are appreciated.

Controls
---
Keys | Action
--- | ---
wasd | move cam
op   | dec/inc generation
kl   | dec/inc angle
vb   | dec/inc lineLen (poor man's zoom in/out)
r    | reset
y    | sierpinski
t    | cantor
f    | koch
e    | dragon

ToDo
---
- Last line isn't drawn with LineStrips (all except koch)
- Koch is bugged, not sure why
- Controls suck
- More fractals!

License
---
MIT license, see LICENSE for details

![screenshot](scrot.png)
