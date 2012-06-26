echo "Getting clojurescript ..."
git clone https://github.com/raph-amiard/clojurescript.git 2> /dev/null >&1

echo "Compiling clojurescript into a jar .."
cp script/clojurescript_project_file.clj clojurescript/project.clj
cd clojurescript
git checkout lua-workbranch
lein jar 2> /dev/null > /dev/null

echo "Cleaning up .."
mv libclojurescript.jar ../
cd .. && rm clojurescript -rf

echo "Success !"
