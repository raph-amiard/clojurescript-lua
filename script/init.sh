echo "Getting clojurescript ..."
git clone https://github.com/clojure/clojurescript.git clojurescript 2> /dev/null >&1

echo "Compiling clojurescript into a jar .."
cp script/clojurescript_project_file.clj clojurescript/project.clj
cd clojurescript
lein jar 2> /dev/null > /dev/null

echo "Cleaning up .."
mv libclojurescript.jar ../lib/
cd .. && rm clojurescript -rf

echo "Success !"
