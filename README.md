Clojurescript-Lua
-----------------

Welcome, ClojureScript/Lua is a lisp language targeting Lua. It is using the ClojureScript compiler, and provides a different backend and ecosystem for the ClojureScript language.

The current version of ClojureScript/Lua is 0.1.0-ALPHA. It is in alpha stage, hence the name, and you should expect that a lot of things are not working yet !

ClojureScript/Lua should run on any posix system that has bash, lua and java installed.

If you find any bug, don't hesitate to submit issues on github, especially if Cljs/Lua doesn't work at all on your computer, and you find a fix :)

Here is hello world in Cljs/Lua :

~~~clojure
(println "Hello, world !")
~~~

Distinctive traits of ClojureScript are :

- Lisp language (s-expression syntax, code is data, etc)
- Functional (mostly)
- Functional data structures with literal syntax 

~~~clojure
(def my-map {:john "doe" :jack "daniels"})
(println (my-map :john)) ;; prints "doe"
~~~

### Getting started

You need to have leiningen installed. This is the only prerequisite (with of course java & all). After that, you just need to grab yourself a copy of the repo, either via cloning it or downloading an archive.

### Running the Cljs/Lua repl

To run the repl, you need to have Lua 5.1 installed, as well as a few dependencies:

- lua json library
- lua bit ops library

The two are quite standard lua libs that should be available in your distribution's repositories.

To run the REPL, issue the following command

~~~sh
./cljslua repl
~~~

On the first run, Cljs/Lua will install some components that it needs.

#### REPL options

By default, the repl shows the output of compiled Cljs commands. You can switch that off by calling the function

~~~clojure
(switch-verbose)
~~~

You can also switch off execution by calling

~~~clojure
(switch-exec)
~~~

### Cljs/Lua compiler

Cljs/Lua has a **very** basic compiler that works the following way

~~~sh
./cljslua compile <in-file> <out-file>
~~~

1. You give it a in-file and an out-file.
2. It will compile *everything* to the out-file. That means, the content of the core library, of the compiled file, and of any dependencies.
3. It will search for required namespaces in subdirectories of the directory containing the in-file. File layout doesn't matter for the moment.

This is very basic, but yet functionnal. The compiler will be redesigned soon, but some thoughts need to be given to the general design of it first.

### Running the lein repl

If you want to see how the compiler compiles out snippets of clojurescript :

~~~sh
$ lein repl
~~~

~~~clojure
REPL started; server listening on localhost port 31236
cljs.lua.compiler=> (lua (defn add [a b] (+ a b)))
"cljs.user.add = (function (a,b)
return (a + b)
end)"
nil
~~~
