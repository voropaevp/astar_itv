Martian Rover Challenge
=

- Rover controlled moved by receiving commands `forward`, `clockwise turn`, `counter clockwise turn`
- Rover teleported to the opposite edge when crosses the edge of the field
- A* pathfinding optimised by the number of commands required to reach the destination
- Field can contain rocks that cannot pass though


Legend
-

Field
- `o` - rover's trace
- `>`, `<`,`^`,`⍌` - rover facing each direction
- `X` - rock
- `.` - plain

Commands
- `cr` - clockwise turn
- `ccr` - counterclockwise turn
- `fw` - move forward

Examples
-
Via double teleport
```
IN           OUT          Commands
.X........   .Xo.......    cr
X^.......X   Xoo......X    fw
.X........   .X........    ccr
.....X....   .....X....    fw
.....X....   .....X....    fw
.XXX.....X   .XXX.....X    ccr
.X.X.X..X.   .X.X.X..X.    fw
.XXX......   .XXX......    fw
..........   ..........    fw
..........   ooo......<   
```

No teleport
```
IN           OUT          Commands
.X........   .Xo.......    cr
X^.......X   Xoo......X    fw
.X........   .X........    fw
.....X....   .....X....    fw
.....X....   .....X....    fw
.XXX.....X   .XXX.....X    fw
.X.X.X..X.   .X.X.X..X.    cr
.XXX......   .XXX......    fw
..........   ..........    fw
..........   ooo......<    fw
                           fw
                           fw
```
