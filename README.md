# Simple programming language interpreter

**Description**

Programming language interpreter created in Haskell. Project made in the Programming Languages subject of the Computer Science course - UFFS

**Generating the Parser**

The *Parser.y* source code contains the definitions accepted by *Happy* for generating the lexical and syntactic analyzer.

To generate, just run the following command:

```
happy parser.y
```

Whenever you add/change any information in the *Parser.y* file, you need to run the above command again.

If the system displays an error message informing you that the *Happy* command does not exist, you must install it on your system. To do this, just run the command:

```
stack install happy
```

**Running the Interpreter**

To run/test the developed interpreter, we will use the ```runghc``` program, as seen below.

```
stack runghc Interpreter.hs
```

When executing this command, the program waits for the user's input, who can type the expression to be interpreted. As an example, let's type the expression ```2 + 4``` and press *ENTER*. For the result to be displayed, we still need to use the key combinations *CTRL + Z* (*CTRL + D* in Linux). These commands in sequence are shown below.

```
2 + 4 <enter>
<CTRL + Z> <enter>
```

After this procedure, the result of the interpreter processing will be displayed on the screen.

```
Just (Number 6)
```

More details on the functions and adjustments needed for the final work can be found in comments in the *Parser.y* and *Interpreter.hs* files.