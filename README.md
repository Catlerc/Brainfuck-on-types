Well, it's a BrainFuck interpreter, written on scala 3 (3.1.3).
It runs in compile time without macrosses. How? It's a magic of types!
Interestingly, since the IDE compute out types when you edit code,
it turns out that the interpreter is started every time you change something in files. RIP CPU.