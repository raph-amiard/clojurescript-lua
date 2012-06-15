Clojurescript-Lua
-----------------

Welcome, this is the repository for the version of clojurescript which will, soon, be running on Lua :)

It is also a proof of concept of the fact that you can now use the clojurescript analyzer as a library !

You can already test out things by following the getting started guide

### Getting started

You need to have leiningen installed. This is the only prerequisite (with of course java & all)

~~~
$ git clone https://github.com/raph-amiard/clojurescript-lua.git
$ cd clojurescript-lua
$ script/init.sh
~~~

If you want to see how the compiler compiles out snippets of clojurescript :

~~~
$ lein repl
REPL started; server listening on localhost port 31236
cljs.lua.compiler=> (lua (defn add [a b] (+ a b)))
cljs.user.add = (function (a,b)
return (a + b)
end)
nil
~~~

And that's about all you can do for the moment :)

Stay tuned !