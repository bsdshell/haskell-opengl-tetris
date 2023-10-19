### PlotGeometry is test Haskell OpenGL file.
### Download Haskell OpenGL-3.0.3.0.tar
### Untar it and put it under *PlotGeometry/src*
* This is hug and complex package
* It dependents on many packages such as *OpenGL*, *OpenGLRaw* and *ObjectName*
* OpenGLRaw etc
* Build it on macOS, there is some small issue one a debug file etc, I hae to comment out the function to ignore the build issue. It is a hack but it works.

### What are you going to do if you got 'You can not use the hidden package' or some function you can not use because it is a hidden package.
* One simple 'solution' is to download the package, unzip it and dump it under your src directory then you can expose it in the module.

### Why there are not many good tutorials in Google for Haskell OpenGL?

* First, _OpenGL_ is one of the most complicated APL out there to understand.
  - What is the *Matrix*? It is not from the movie *The Matrix* or it has nothing to do with the guy called Neo(Keanu Reeves) who has super power.
  - What is the rotation matrix that rotates around *z-axis* in *xy-plane*
  - What is *Vertex Buffer*, *Fragment Shader* and *Vertex Shader* etc ?
* Second, _C/C++_ is one of the hardest programming language to learn. What are C99, C11, C14, C17, C20, Clang etc?
* Third, Haskell is one of the hardest programming language to understand.
* Haskell is *bad* for **Graphic** or **Game** development because it is *immutable*.
  - What is *Stack* or *Cabal*
  - What is *Category Theory*, *Monad*, *Functor* and *Monoid* etc.
* If you combine all those things into one, then you realize there is few people is willing to spend time on learning it or trying it out.
  - Add more here
  - Add more here

### Add zoom in, zoom out
* Change the angle(in degree) in perspective function
    perspective 30 x/y znear zfar
    -- 30 => 40
```
* Keypress 'I' Zoom in
* Keypress 'O' Zoom out
