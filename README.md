Clojurescript-Lua
-----------------

Welcome, this is the repository for the version of clojurescript which will, soon, be running on Lua :)

It is also a proof of concept of the fact that you can now use the clojurescript analyzer as a library !

You can already test out things by following the getting started guide

### Getting started

You need to have leiningen installed. This is the only prerequisite (with of course java & all)

~~~sh
$ git clone https://github.com/raph-amiard/clojurescript-lua.git
$ cd clojurescript-lua
$ script/init.sh
~~~

### Running the Cljs/Lua repl

Cljs/Lua has no full fledged compiler at the moment, just a REPL, because that's all i need for testing. Since i'm quite close to having the full thing running, the compiler should follow quite shortly !

To run the repl, you need to have Lua 5.1 installed, as well as a few dependencies:

- lua json library
- lua bit ops library

The two are quite standard lua libs that should be available in your distribution's repositories.

The repl will spawn a lua subprocess from java, and interact with it via json chunks on named pipes. That means that *for the moment the repl will only run on posix systems*. You won't know if the lua side is running properly by running the repl because errors are not yet properly redirected, so to make sure everything is working properly, you can run the following command :

~~~sh
lua cljs/exec_server.lua
~~~

If there are no errors, and the process is just hanging there, it is that everything is fine. You can then run the repl :

~~~sh
script/repl
~~~

I will make the process more robust in time !

#### REPL options

By default, the repl shows the output of compiled Cljs commands. You can switch that off by calling the function

~~~clojure
(switch-verbose)
~~~

You can also switch off execution by calling

~~~clojure
(switch-exec
~~~

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
