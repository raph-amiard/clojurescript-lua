mkdir lib

echo "Getting Clojure ..."
wget http://repo1.maven.org/maven2/org/clojure/clojure/1.4.0/clojure-1.4.0.zip 2> /dev/null >&1
unzip -qu clojure-1.4.0.zip
mv clojure-1.4.0/clojure-1.4.0.jar lib/clojure.jar
rm clojure-1.4.0 -rf
echo "Getting data.json ..."
wget http://repo1.maven.org/maven2/org/clojure/data.json/0.1.3/data.json-0.1.3.jar 2> /dev/null >&1
mv data.json-0.1.3.jar lib

echo "Getting ClojureScript ..."
wget http://search.maven.org/remotecontent?filepath=org/clojure/clojurescript/0.0-1450/clojurescript-0.0-1450.jar -O libclojurescript.jar 2> /dev/null >&1

echo "Success !"
