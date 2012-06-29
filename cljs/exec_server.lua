require("json")
require("io")
require("cljs.builtins")

actions = {
   exec = function (body)
      resp = {}
      func = loadstring(body)
      if func then
	 result = func()
	 if result then 
	    resp.body = tostring(result)
	 end
	 resp.status = "OK"
      else
	 resp.status = "ERROR"
      end
      return resp
   end
}

function exec_server()
   io.write("exec server\n")
   io.flush()
   while true do
      request = json.decode(io.read())
      io.write(json.encode(actions[request.action](request.body)) .. "\n")
      io.flush()
   end
end

exec_server()